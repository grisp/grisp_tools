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

package(#{custom_build := true, build := #{hash := #{value := Hash}}} = State0) ->
    event(State0, [{type, {custom_build, Hash}}]);
package(#{build := #{hash := #{value := Hash}}} = State0) ->
    State1 = event(State0, [{type, {package, Hash}}]),
    grisp_tools_util:weave(State1, [
        fun meta/1,
        fun init/1,
        fun download/1,
        fun extract/1
    ]).

release(#{paths := #{install := InstallPath}} = State0) ->
    Release = maps:get(release, State0, #{}),
    release(State0, maps:merge(Release, #{erts => InstallPath})).

distribute(State0 = #{distribute := Dists}) ->
    grisp_tools_util:iterate(State0, Dists, dist, fun(S0) ->
        grisp_tools_util:pipe(S0, [
            fun(S) -> run_script([dist, scripts], pre_script, S) end,
            fun dist_prepare/1,
            fun dist_release/1,
            fun dist_files/1,
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

download(#{package_source := cache} = State0) ->
    event(State0, ['_skip']);
download(#{package := #{meta := Meta}} = State0) ->
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

extract(#{package := #{state := State, file := File}} = State0)
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

dist_prepare(State0 = #{dist := #{type := copy, destination := Dest}}) ->
    case file:read_file_info(Dest) of
        {ok, #file_info{type = directory, access = Access}}
          when Access =:= write; Access =:= read_write ->
            State0;
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
        {ok, TarDesc} -> State0#{tar_desc => TarDesc};
        {error, Reason} -> event(State0, [archive, {error, Reason}])
    end.

dist_files(#{dist := #{destination := Dest}, release := Release} = State0) ->
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
            write_file(S, File, Context)
        end,
        State1,
        mapz:deep_get([deploy, overlay, files], State0)
    ).

write_file(#{dist := #{type := archive}, tar_desc := TarDesc} = State0,
           #{target := Target} = File, Context) ->
    Content = iolist_to_binary(grisp_tools_util:read_file(File, Context)),
    State1 = event(State0, [files, {copy, File}]),
    case erl_tar:add(TarDesc, Content, binary_to_list(Target), []) of
        {error, Reason} -> error(Reason);
        ok -> State1
    end;
write_file(#{dist := #{type := copy, destination := Dest, force := Force}} = State0,
           #{target := Target} = File, Context) ->
    Path = filename:join(Dest, Target),
    State1 = event(State0, [files, {copy, File}]),
    force_execute(Path, Force, fun(F) ->
        grisp_tools_util:ensure_dir(F),
        grisp_tools_util:write_file(Dest, File, Context)
    end, State1).

force_execute(File, Force, Fun, State0) ->
    State1 = case {filelib:is_file(File), Force} of
        {true, false} ->
            event(State0, [files, {error, file_exists, File}]);
        _ ->
            State0
    end,
    Fun(File),
    State1.

dist_release(#{tar_desc := TarDesc, release := Release,
               dist := #{type := archive}} = State0) ->
    #{name := RelName, dir := Source} = Release,
    Target = atom_to_list(RelName),
    State1 = event(State0, [release, {archive, Source, Target}]),
    case erl_tar:add(TarDesc, Source, Target, [dereference]) of
        {error, Reason} -> error(Reason);
        ok -> State1
    end;
dist_release(#{release := Release, dist := #{type := copy} = Dist} = State0) ->
    #{name := RelName, dir := Source} = Release,
    #{destination := Dest, force := Force} = Dist,
    Target = filename:join(Dest, RelName),
    State1 = event(State0, [release, {copy, Source, Target}]),
    CopyExe = case Force of
        true  -> "cp -Rf";
        false -> "cp -R"
    end,
    Command = string:join([CopyExe, qoute(Source ++ "/"), qoute(Target)], " "),
    {Output, State2} = shell(State1, Command),
    event(State2, [release, {copy, {result, Output}}]).

qoute(String) -> "\"" ++ String ++ "\"".

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

download_loop(ReqID, #{package := #{tmp := Tmp}} = State0) ->
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
