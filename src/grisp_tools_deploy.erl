-module(grisp_tools_deploy).

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
        {validate, [
            fun grisp_tools_step:apps/1,
            fun grisp_tools_step:version/1
        ]},
        fun grisp_tools_step:collect/1,
        fun calculate_hash/1,
        fun download/1,
        fun make_release/1,
        fun copy/1,
        fun finalize/1
    ]).

%--- Tasks ---------------------------------------------------------------------

calculate_hash(#{build := #{overlay := Overlay}} = S0) ->
    % FIXME: Move to function in _steps module and use in build task
    {Hash, _HashIndex} = grisp_tools_util:overlay_hash(Overlay),
    S0#{hash => Hash}.

download(#{custom_build := true, hash := Hash} = State0) ->
    event(State0, {otp_type, Hash, custom_build});
download(#{hash := Hash} = State0) ->
    State1 = event(State0, {otp_type, Hash, package}),
    grisp_tools_util:pipe(State1, [
        fun package_load_etag/1,
        fun package_init_tmp/1,
        fun package_download/1,
        fun package_save_etag/1,
        fun package_extract/1
    ]).

make_release(#{paths := #{install := InstallPath}} = State0) ->
    Release = maps:get(release, State0, #{}),
    release(State0, maps:merge(Release, #{erts => InstallPath})).

copy(State0) ->
    State1 = event(State0, {deployment, init}),
    grisp_tools_util:pipe(State1, [
        fun(S) -> run_script(pre_script, S) end,
        fun copy_files/1,
        fun copy_release/1,
        fun(S) -> run_script(post_script, S) end
    ]).

finalize(State0) ->
    event(State0, {deployment, done}).

%--- Internal ------------------------------------------------------------------

% Package downloads

package_load_etag(State0) ->
    {ETagFile, ETag} = grisp_tools_util:package_cache_etag(State0),
    State1 = event(State0, {package, {current_etag, ETag}}),
    mapz:deep_put([package, etag], #{file => ETagFile, value => ETag}, State1).

package_init_tmp(State0) ->
    File = grisp_tools_util:package_cache_file(State0),
    Tmp = grisp_tools_util:package_cache_temp(State0),
    grisp_tools_util:ensure_dir(Tmp),
    State1 = event(State0, {package, {deleting_tmp_file, Tmp}}),
    rm(Tmp),
    mapz:deep_merge([State1, #{package => #{file => File, tmp => Tmp}}]).

package_download(#{package := #{etag := #{value := ETag}}} = State0) ->
    Client = http_init(),
    URI = grisp_tools_util:cdn_path(otp, State0),
    Headers = [{"If-None-Match", ETag} || ETag =/= undefined],
    Options = [{stream, {self, once}}, {sync, false}],
    State1 = event(State0, {package, {download_init, URI, ETag}}),
    ReqID = http_get(URI, Headers, Options, Client),
    State2 = download_loop(ReqID, State1),
    case State2 of
        #{package := #{state := downloaded, tmp := Tmp, file := File}} ->
            mv(Tmp, File);
        #{package := #{tmp := Tmp}} ->
            rm(Tmp)
    end,
    State2.

package_save_etag(#{package := #{etag := #{value := undefined}}} = State0) ->
    State0;
package_save_etag(#{package := #{state := downloaded} = Package} = State0) ->
    #{etag := #{file := ETagFile, value := ETag}} = Package,
    State1 = event(State0, {new_etag, ETag}),
    ok = file:write_file(ETagFile, io_lib:format("~p.~n", [{etag, ETag}])),
    State1;
package_save_etag(State0) ->
    State0.

package_extract(#{package := #{state := downloaded, file := File}} = State0) ->
    #{paths := #{install := InstallPath}} = State0,
    State1 = event(State0, {package, {extract, {start, File}}}),
    case erl_tar:extract(File, [compressed, {cwd, InstallPath}]) of
        ok              -> event(State1, {package, {extract, done}});
        {error, Reason} -> event(State1, {package, {extract, {error, Reason}}})
    end;
package_extract(State0) ->
    State0.

% copy_files(State, RelName, RelVsn, Board, ERTSVsn, Dest, Force, Opts) ->
copy_files(#{copy := #{destination := Dest, force := Force}, release := Release} = State0) ->
    #{paths := #{install := InstallPath}} = State0,
    State1 = event(State0, {deployment, {files, {init, Dest}}}),
    ERTSPath = filelib:wildcard(binary_to_list(filename:join(InstallPath, "erts-*"))),
    "erts-" ++ ERTSVsn = filename:basename(ERTSPath),
    #{name := RelName, version := RelVsn} = Release,
    Context = #{
        release_name    => RelName,
        release_version => RelVsn,
        erts_vsn        => ERTSVsn
    },
    maps:fold(
        fun(_Name, #{target := Target, source := Source}, S) ->
            write_file(Dest, Target, Source, Force, Context, S)
        end,
        State1,
        mapz:deep_get([deploy, overlay, files], State0)
    ).

write_file(Dest, Target, Source, Force, Context, State0) ->
    Path = filename:join(Dest, Target),
    State1 = event(State0, {deployment, files, {copy, Source, Path}}),
    Content = load_file(Source, Context),
    force_execute(Path, Force, fun(F) ->
        grisp_tools_util:ensure_dir(F),
        ok = file:write_file(F, Content)
    end, State1).

load_file({template, Source}, Context) ->
    grisp_tools_template:render(Source, Context);
load_file(Source, _Context) ->
    {ok, Binary} = file:read_file(Source),
    Binary.

force_execute(File, Force, Fun, State0) ->
    State1 = case {filelib:is_file(File), Force} of
        {true, false} ->
            event(State0, {deployment, files, {copy_error, {exists, File}}});
        _ ->
            State0
    end,
    Fun(File),
    State1.

copy_release(#{release := Release, copy := Copy} = State0) ->
    #{name := RelName, dir := Source} = Release,
    #{destination := Dest, force := Force} = Copy,
    Target = filename:join(Dest, RelName),
    State1 = event(State0, {deployment, release, {copy, Source, Target}}),
    CopyExe = case Force of
        true  -> "cp -Rf";
        false -> "cp -R"
    end,
    Command = string:join([CopyExe, qoute(Source ++ "/"), qoute(Target)], " "),
    {Output, State2} = shell(State1, Command),
    event(State2, {deployment, release, {copy, {result, Output}}}).

qoute(String) -> "\"" ++ String ++ "\"".

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
            State1 = event(State0, {package, {download_start, Size}}),
            ok = httpc:stream_next(Pid),
            download_loop({ReqID, Pid}, Handle, State1, Bytes);
        {http, {ReqID, stream, BinBodyPart}} ->
            NewBytes = Bytes + byte_size(BinBodyPart),
            State1 = event(State0, {package, {download_progress, NewBytes}}),
            ok = httpc:stream_next(RequestPid),
            ok = file:write(Handle, BinBodyPart),
            download_loop({ReqID, RequestPid}, Handle, State1, NewBytes);
    % Result:
        {http, {ReqID, stream_end, Headers}} ->
            NewETag = case lists:keyfind("etag", 1, Headers) of
                {"etag", ServerETag} -> ServerETag;
                false                -> undefined
            end,
            State1 = event(State0, {package, {download_end, NewETag, Headers}}),
            State2 = event(State1, {package, {download_complete, NewETag}}),
            mapz:deep_merge([State2, #{package => #{
                state => downloaded,
                etag => #{value => NewETag}}
            }]);
        {http, {ReqID, {{_HTTPVer, 304, "Not Modified"}, _Headers, _Body}}} ->
            State1 = event(State0, {package, download_cached}),
            mapz:deep_merge([State1, #{package => #{state => not_modified}}]);
        {http, {ReqID, {{_HTTPVer, 404, "Not Found"}, _Headers, _Body}}} ->
            #{otp_version := OTPVersion, hash := Hash} = State0,
            error({package, {not_found, OTPVersion, Hash}});
        {http, Other} ->
            State1 = event(State0, {package, {http_error, Other}}),
            mapz:deep_merge([State1, #{package => #{state => error}}])
    after
        120000 ->
            State1 = event(State0, {package, {http_error, timeout}}),
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

run_script(Name, State0) ->
    case mapz:deep_get([scripts, Name], State0, undefined) of
        undefined -> State0;
        Script ->
            State1 = event(State0, {deployment, script, Name, {run, Script}}),
            {{ok, Output}, State2} = shell(State1, Script),
            event(State2, {deployment, script, Name, {result, Output}})
    end.

release(State0, Release) ->
    State1 = event(State0, {release, {start, Release}}),
    {Spec, State2} = exec(release, State1, [Release]),
    case maps:merge(Release, Spec) of
        #{name := Name, dir := Dir, version := Version} = Merged
          when length(Name) > 0, length(Dir) > 0, length(Version) > 0 ->
            State3 = event(State2, {release, {done, Merged}}),
            mapz:deep_merge(State3, #{release => Merged});
        Merged ->
            error({invalid_release, Merged})
    end.
