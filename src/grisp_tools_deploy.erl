-module(grisp_tools_deploy).

-include_lib("kernel/include/file.hrl").

% API
-export([run/1]).

-import(grisp_tools_util, [event/2]).
-import(grisp_tools_util, [shell/2]).
-import(grisp_tools_util, [exec/3]).
-import(grisp_tools_util, [mv/2]).
-import(grisp_tools_util, [rm/1]).
-import(grisp_tools_util, [with_file/3]).

%--- API -----------------------------------------------------------------------

run(State) ->
    grisp_tools_util:weave(State, [
        fun grisp_tools_step:config/1,
        {deploy, [
            {validate, [
                fun grisp_tools_step:apps/1,
                fun grisp_tools_step:version/1
            ]},
            fun grisp_tools_step:collect/1,
            fun package/1,
            fun release/1,
            fun distribute/1
        ]}
    ]).

%--- Tasks ---------------------------------------------------------------------

package(State0 = #{custom_build := true, build := #{hash := #{value := Hash}}}) ->
    event(State0, [{type, {custom_build, Hash}}]);
package(State0 = #{build := #{hash := #{value := Hash}}}) ->
    State1 = event(State0, [{type, {package, Hash}}]),
    grisp_tools_util:weave(State1, [
        fun meta/1,
        fun init/1,
        fun download/1,
        fun extract/1
    ]).

release(State0 = #{paths := #{install := InstallPath}}) ->
    Release = maps:get(release, State0, #{}),
    release(State0, maps:merge(Release, #{erts => InstallPath})).

distribute(State0 = #{distribute := Dists}) ->
    grisp_tools_util:iterate(State0, Dists, dist, fun(S0) ->
        grisp_tools_util:pipe(S0, [
            fun(S) -> run_script([dist, scripts], pre_script, S) end,
            fun dist_prepare/1,
            fun dist_release/1,
            fun dist_files/1,
            fun dist_write_manifest/1,
            fun dist_finish/1,
            fun(S) -> run_script([dist, scripts], post_script, S) end
        ], [fun dist_abort/1])
    end).

%--- Internal ------------------------------------------------------------------

% Package downloads

meta(State0) ->
    Meta = grisp_tools_util:package_cache_meta(State0),
    State1 = event(State0, [package, {meta, Meta}]),
    mapz:deep_put([package, meta], Meta, State1).

init(State0) ->
    File = grisp_tools_util:package_cache_file(State0),
    Tmp = grisp_tools_util:package_cache_temp(State0),
    grisp_tools_util:ensure_dir(Tmp),
    State1 = event(State0, [{deleting_tmp_file, Tmp}]),
    rm(Tmp),
    mapz:deep_merge([State1, #{package => #{file => File, tmp => Tmp}}]).

download(State0 = #{package_source := cache}) ->
    event(State0, ['_skip']);
download(State0 = #{package := #{meta := Meta}}) ->
    Client = http_init(),
    URI = grisp_tools_util:cdn_path(otp, State0),
    Headers = [{"If-None-Match", ETag} || #{etag := ETag} <- [Meta]],
    Options = [{stream, {self, once}}, {sync, false}],
    State1 = event(State0, [{uri, URI}]),
    ReqID = http_get(URI, Headers, Options, Client),
    State2 = download_loop(ReqID, State1),
    case State2 of
        #{package := #{state := downloaded, tmp := Tmp, file := File, meta := NewMeta}} ->
            mv(Tmp, File),
            grisp_tools_util:package_cache_meta(State0, NewMeta);
        #{package := #{tmp := Tmp}} ->
            rm(Tmp)
    end,
    State2.

extract(State0 = #{package := #{state := State, file := File}})
        when (State == downloaded) or (State == not_modified) ->
    #{paths := #{install := InstallPath}} = State0,
    case filelib:is_dir(InstallPath) of
        true -> event(State0, ['_skip']);
        false -> extract_really(File, InstallPath, State0)
    end;
extract(State0) ->
    event(State0, ['_skip']).

extract_really(File, InstallPath, State0) ->
    State1 = event(State0, [{start, File}]),
    case erl_tar:extract(File, [compressed, {cwd, InstallPath}]) of
        ok              -> event(State1, [done]);
        {error, Reason} -> event(State1, [{error, Reason}])
    end.

dist_hash_prepare(State0) ->
    State0#{dist_hash => crypto:hash_init(sha)}.

dist_hash_update(State0 = #{dist_hash := Ctx0}, FileId, FileContent)
  when Ctx0 =/= undefined ->
    Ctx1 = crypto:hash_update(Ctx0, ["<", FileId, ":", FileContent, ">"]),
    State0#{dist_hash => Ctx1};
dist_hash_update(State0 = #{dist_hash := undefined}, _FileId, _FileContent) ->
    State0.

dist_hash_finalize(State0 = #{dist_hash := Ctx0})
  when Ctx0 =/= undefined ->
    {State0#{dist_hash => undefined}, crypto:hash_final(Ctx0)};
dist_hash_finalize(State0) ->
    {State0, undefined}.

dist_prepare(State0 = #{dist := #{type := copy, destination := Dest}}) ->
    case file:read_file_info(Dest) of
        {ok, #file_info{type = directory, access = Access}}
          when Access =:= write; Access =:= read_write ->
            dist_hash_prepare(State0);
        {ok, #file_info{type = directory}} ->
            event(State0, [{error, dir_not_writable, Dest}]);
        {ok, #file_info{}} ->
            event(State0, [{error, dir_access, Dest}]);
        {error, enoent} ->
            event(State0, [{error, dir_missing, Dest}])
    end;
dist_prepare(State0 = #{dist := #{type := archive, destination := Dest,
                                  force := Force, compressed := Compressed}}) ->
    case {Force, file:read_file_info(Dest)} of
        {_, {error, enoent}} -> ok;
        {false, {ok, #file_info{}}} ->
            event(State0, [{error, file_exists, Dest}]);
        {true, {ok, #file_info{type = regular}}} ->
            case file:delete(Dest) of
                ok -> ok;
                {error, _Reason} ->
                    event(State0, [{error, file_access, Dest}])
            end;
        {true, {ok, #file_info{}}} ->
            event(State0, [{error, not_a_file, Dest}])
    end,
    TarOpts = case Compressed of
        true -> [write, compressed];
        false -> [write]
    end,
    grisp_tools_util:ensure_dir(Dest),
    case erl_tar:open(Dest, TarOpts) of
        {ok, TarDesc} -> dist_hash_prepare(State0#{tar_desc => TarDesc});
        {error, Reason} -> event(State0, [archive, {error, Reason}])
    end.

dist_files(State0 = #{dist := #{destination := Dest}, release := Release}) ->
    #{paths := #{install := InstallPath}} = State0,
    State1 = event(State0, [files, {init, Dest}]),
    ERTSPath = filelib:wildcard(binary_to_list(filename:join(InstallPath, "erts-*"))),
    "erts-" ++ ERTSVsn = filename:basename(ERTSPath),
    #{name := RelName, version := RelVsn} = Release,
    Context = #{
        release_name    => RelName,
        release_version => RelVsn,
        erts_vsn        => ERTSVsn
    },
    maps:fold(
        fun(_Name, File, S) ->
            copy_file(S, File, Context)
        end,
        State1,
        mapz:deep_get([deploy, overlay, files], State0)
    ).

as_binary(Data) when is_atom(Data) -> atom_to_binary(Data);
as_binary(Data) -> iolist_to_binary(Data).

as_list(Data) when is_list(Data) -> Data;
as_list(Data) when is_binary(Data) -> binary_to_list(Data).

copy_file(State0 = #{dist := #{destination := Dest}}, File, Context) ->
    copy_file(State0, Dest, File, undefined, Context).

copy_file(State0, Dest, File, FileInfo, Context) ->
    Content = as_binary(grisp_tools_util:read_file(File, Context)),
    write_file(State0, Dest, File, FileInfo, Content).

write_file(State0 = #{dist := #{destination := Dest}}, File, Content) ->
    write_file(State0, Dest, File, undefined, Content).

write_file(State0 = #{dist := #{type := archive}, tar_desc := TarDesc}, _Dest,
           #{target := Target}, FileInfo, Content) ->
    State1 = event(State0, [files, {copy, Target}]),
    TarOpts = case FileInfo of
        undefined -> [];
        Rec when is_record(Rec, file_info) ->
            % TAR files lose the files mode
            [{K, V} || {K, V} <- [
                {atime, Rec#file_info.atime},
                {mtime, Rec#file_info.mtime},
                {ctime, Rec#file_info.ctime},
                {uid, Rec#file_info.uid},
                {gid, Rec#file_info.gid}
             ], V =/= undefined]
    end,
    ContentBin = iolist_to_binary(Content),
    case erl_tar:add(TarDesc, ContentBin, as_list(Target), TarOpts) of
        {error, Reason} -> error(Reason);
        ok -> dist_hash_update(State1, Target, Content)
    end;
write_file(State0 = #{dist := #{type := copy, force := Force}}, Dest,
           File = #{target := Target}, FileInfo, Content) ->
    Path = filename:join(Dest, Target),
    State1 = event(State0, [files, {copy, Target}]),
    State2 = dist_hash_update(State1, Target, Content),
    force_execute(Path, Force, fun(F) ->
        grisp_tools_util:ensure_dir(F),
        grisp_tools_util:write_file(Dest, File, Content, FileInfo)
    end, State2).

force_execute(File, Force, Fun, State0) ->
    State1 = case {filelib:is_file(File), Force} of
        {true, false} ->
            event(State0, [files, {error, file_exists, File}]);
        _ ->
            State0
    end,
    Fun(File),
    State1.

dist_create_dir_callback(State0 = #{dist := #{type := archive}},
                         _DestRoot, _RelPath, _FileInfo) ->
    % TAR files do not contains empty directories...
    State0;
dist_create_dir_callback(State0 = #{dist := #{type := copy}},
                         DestRoot, RelPath, FileInfo) ->
    DestPath = filename:join(DestRoot, RelPath),
    grisp_tools_util:filelib_ensure_path(filename:join(DestPath, "dummy.file")),
    case file:write_file_info(DestPath, FileInfo, [{time, posix}]) of
        ok -> State0;
        {error, Reason} ->
            erlang:error({write_file_info_error, Reason, DestPath})
    end.

dist_copy_callback(State0, SrcPath, DestRoot, RelPath, FileInfo) ->
    FileSpec = #{source => SrcPath, target => RelPath, name => RelPath},
    copy_file(State0, DestRoot, FileSpec, FileInfo, #{}).

dist_release(State0 = #{release := #{name := RelName, dir := Source},
                        dist := #{type := archive}}) ->
    Target = atom_to_list(RelName),
    State1 = event(State0, [release, {archive, Source, Target}]),
    Opts = #{copy_fun => fun dist_copy_callback/5,
             create_dir_fun => fun dist_create_dir_callback/4},
    grisp_tools_util:copy_directory(State1, Source, Target, Opts);
dist_release(State0 = #{release := #{name := RelName, dir := Source},
                        dist := #{type := copy, destination := Dest}}) ->
    Target = filename:join(Dest, RelName),
    State1 = event(State0, [release, {copy, Source, Target}]),
    Opts = #{copy_fun => fun dist_copy_callback/5,
             create_dir_fun => fun dist_create_dir_callback/4},
    grisp_tools_util:copy_directory(State1, Source, Target, Opts).

parse_grisp_package_file(Line) ->
    case binary:split(Line, <<" ">>, [global]) of
        [Path, Hash] -> {Path, Hash};
        Parts ->
            % The path contains a space
            Hash = lists:last(Parts),
            Path = binary:part(Line, {0, byte_size(Line) - byte_size(Hash) - 1}),
            {Path, Hash}
    end.

read_grisp_package_files(ErtsPath) ->
    case file:read_file(filename:join(ErtsPath, "GRISP_PACKAGE_FILES")) of
        {error, enoent} -> undefined;
        {ok, Data} ->
            Lines = binary:split(Data, <<"\n">>, [global, trim]),
             [parse_grisp_package_file(Line) || Line <- Lines]
    end.

read_grisp_toolchain_revision(ErtsPath) ->
    case file:read_file(filename:join(ErtsPath, "GRISP_TOOLCHAIN_REVISION")) of
        {error, enoent} -> undefined;
        {ok, Data} ->
            re:replace(Data, <<"(^\\s+|\\s+$)">>, <<>>,
                       [global, {return, binary}])
    end.

validate_manifest(Data, Expected) ->
    TempFilePath = string:chomp(os:cmd("mktemp")),
    try
        file:write_file(TempFilePath, Data),
        case file:consult(TempFilePath) of
            {ok, Expected} -> ok;
            {ok, _Other} ->
                {internal_manifest_error, inconsistent, Data};
            {error, Reason} ->
                {internal_manifest_error, Reason, Data}
        end
    after
        os:cmd("rm -f " ++ TempFilePath)
    end.

dist_write_manifest(State0 = #{platform := Platform,
                         otp_version := {_, _, _, OtpVer},
                         release := Release}) ->
    #{name := RelName, version := RelVsn, erts := ErtsPath} = Release,
    Profiles = maps:get(profiles, Release, []),
    {State1, HashBin} = dist_hash_finalize(State0),
    Hash = iolist_to_binary([io_lib:format("~2.16.0b", [Byte])
                             || <<Byte>> <= HashBin]),
    ToolchainRev = read_grisp_toolchain_revision(ErtsPath),
    PackageFiles = read_grisp_package_files(ErtsPath),
    ManifestTerms = [
        {platform, as_binary(Platform)},
        {id, Hash},
        {relname, as_binary(RelName)},
        {relvsn, as_binary(RelVsn)},
        {profiles, Profiles},
        {package, [
            {toolchain, [
                {revision, as_binary(ToolchainRev)}
            ]},
            {rtems, [
                % FIXME: whe finally support rtems 6, we need a way
                % to figure the version out.
                {version, <<"5">>}
            ]},
            {otp, [
                {version, as_binary(OtpVer)}
            ]},
            {custom, PackageFiles}
        ]}
    ],
    Manifest = grisp_tools_util:format_term(ManifestTerms),
    validate_manifest(Manifest, ManifestTerms),
    write_file(State1, #{target => <<"MANIFEST">>}, Manifest).

dist_finish(State0 = #{tar_desc := TarDesc,
                       dist := #{type := archive, destination := Dest}}) ->
    erl_tar:close(TarDesc),
    event(State0, [archive, {closed, Dest}]),
    maps:remove(tar_desc, State0);
dist_finish(State0 = #{dist := #{type := copy}}) ->
    State0.

dist_abort(State0 = #{tar_desc := TarDesc,
                      dist := #{type := archive, destination := Dest}}) ->
    erl_tar:close(TarDesc),
    file:delete(Dest),
    maps:remove(tar_desc, State0);
dist_abort(State0) ->
    State0.

% Helpers

download_loop(ReqID, State0 = #{package := #{tmp := Tmp}}) ->
    with_file(Tmp, [raw, append, binary], fun(Handle) ->
        download_loop({ReqID, undefined}, Handle, State0, 0)
    end).

download_loop({ReqID, RequestPid}, Handle, State0, Bytes) ->
    receive
    % Progress:
        {http, {ReqID, stream_start, Headers, Pid}} ->
            ContentLength = proplists:get_value("content-length", Headers),
            Size = case ContentLength of
                undefined -> undefined;
                Length    -> list_to_integer(Length)
            end,
            State1 = event(State0, [{start, Size}]),
            ok = httpc:stream_next(Pid),
            download_loop({ReqID, Pid}, Handle, State1, Bytes);
        {http, {ReqID, stream, BinBodyPart}} ->
            NewBytes = Bytes + byte_size(BinBodyPart),
            State1 = event(State0, [{progress, NewBytes}]),
            ok = httpc:stream_next(RequestPid),
            ok = file:write(Handle, BinBodyPart),
            download_loop({ReqID, RequestPid}, Handle, State1, NewBytes);
    % Result:
        {http, {ReqID, stream_end, Headers}} ->
            NewETag = case lists:keyfind("etag", 1, Headers) of
                {"etag", ServerETag} -> ServerETag;
                false                -> undefined
            end,
            State1 = event(State0, [{complete, NewETag}]),
            mapz:deep_merge([State1, #{package => #{
                state => downloaded,
                meta => #{etag => NewETag}}
            }]);
        {http, {ReqID, {{_HTTPVer, 304, "Not Modified"}, _Headers, _Body}}} ->
            State1 = event(State0, ['_skip']),
            mapz:deep_merge([State1, #{package => #{state => not_modified}}]);
        {http, {ReqID, {{_HTTPVer, 404, "Not Found"}, _Headers, _Body}}} ->
            #{
                otp_version := {_, _, _, OTPVersion},
                build := #{hash := #{value := Hash}}
            } = State0,
            error({package, {not_found, OTPVersion, Hash}});
        {http, Other} ->
            State1 = event(State0, [{error, Other}]),
            mapz:deep_merge([State1, #{package => #{state => error}}])
    after
        120000 ->
            State1 = event(State0, [{error, timeout}]),
            mapz:deep_merge([State1, #{package => #{state => error}}])
    end.

http_init() ->
    {ok, InetsPid} = inets:start(httpc, [{profile, grisp_tools}], stand_alone),
    InetsPid.

http_get(URI, Headers, Options, InetsPid) ->
    Request = {URI, Headers},
    HTTPOptions = [{connect_timeout, 5000}],
    {ok, ID} = httpc:request(get, Request, HTTPOptions, Options, InetsPid),
    ID.

run_script(StatePath, Name, State0) ->
    case mapz:deep_get(StatePath ++ [Name], State0, undefined) of
        undefined -> State0;
        Script ->
            State1 = event(State0, [Name, {run, Script}]),
            {{ok, Output}, State2} = shell(State1, Script),
            event(State2, [Name, {result, Output}])
    end.

release(State0, Release) ->
    State1 = event(State0, [{start, Release}]),
    {Spec, State2} = exec(release, State1, [Release]),
    Merged = maps:merge(Release, Spec),
    State3 = event(State2, [{done, Merged}]),
    mapz:deep_merge(State3, #{release => Merged}).
