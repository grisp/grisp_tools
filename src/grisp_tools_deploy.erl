-module(grisp_tools_deploy).

% API
-export([run/1]).

-import(grisp_tools_util, [
    mv/2,
    rm/1,
    with_file/3
]).

%--- API -----------------------------------------------------------------------

run(State) ->
    execute(State, [
        fun check_args/1,
        fun check_otp_version/1,
        fun calculate_hash/1,
        fun init_otp/1,
        fun make_release/1,
        fun copy/1,
        fun finalize/1
    ]).

%--- Tasks ---------------------------------------------------------------------

check_args(#{release := Release} = State0) ->
    case Release of
        #{name := N, version := V} when N =/= undefined, V =/= undefined ->
            State0;
        _Else ->
            error({release_unspecified, Release})
    end;
check_args(State0) ->
    State0.

check_otp_version(#{otp_version := OTPVersion} = State) ->
    [Major|_] = string:split(OTPVersion, "."),
    case {Major, erlang:system_info(otp_release)} of
        {Target, Target}  -> State;
        {Target, Current} -> error({otp_version_mismatch, Target, Current})
    end.

calculate_hash(#{apps := Apps, board := Board} = State0) ->
    {Hash, _HashIndex} = grisp_tools_util:source_hash(Apps, Board),
    State1 = State0#{hash => Hash},
    State1#{install_root => grisp_tools_util:otp_path(State1, install_root)}.

init_otp(#{custom_build := true, hash := Hash} = State0) ->
    event(State0, {otp_type, Hash, custom_build});
init_otp(#{hash := Hash} = State0) ->
    State1 = event(State0, {otp_type, Hash, package}),
    execute(State1, [
        fun package_load_etag/1,
        fun package_init_tmp/1,
        fun package_download/1,
        fun package_save_etag/1,
        fun package_extract/1
    ]).

make_release(#{install_root := InstallRoot} = State0) ->
    Release = maps:get(release, State0, #{}),
    State1 = event(State0, {deployment, {release, {start, Release}}}),
    release(State1, maps:merge(Release, #{otp_root => InstallRoot})).

copy(State0) ->
    execute(State0, [
        fun(S) -> run_script(pre_script, S) end,
        fun copy_files/1,
        fun copy_release/1,
        fun(S) -> run_script(post_script, S) end
    ]).

finalize(State0) ->
    event(State0, {deployment, done}).

%--- Internal ------------------------------------------------------------------

execute(State, Actions) ->
    lists:foldl(fun(Action, S) -> Action(S) end, State, Actions).

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
    ReqID = http_get({URI, Headers}, Options, Client),
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
    #{install_root := InstallRoot} = State0,
    State1 = event(State0, {package, {extract, {start, File}}}),
    case erl_tar:extract(File, [compressed, {cwd, InstallRoot}]) of
        ok              -> event(State1, {package, {extract, done}});
        {error, Reason} -> event(State1, {package, {extract, {error, Reason}}})
    end;
package_extract(State0) ->
    State0.

% copy_files(State, RelName, RelVsn, Board, ERTSVsn, Dest, Force, Opts) ->
copy_files(#{copy := #{destination := Dest, force := Force}, release := Release, install_root := Root} = State0) ->
    State1 = event(State0, {deployment, {files, {init, Dest}}}),
    ERTSPath = filelib:wildcard(filename:join(Root, "erts-*")),
    "erts-" ++ ERTSVsn = filename:basename(ERTSPath),
    Tree = find_replacement_files(State1, "files"),
    #{name := RelName, version := RelVsn} = Release,
    Context = #{
        release_name    => RelName,
        release_version => RelVsn,
        erts_vsn        => ERTSVsn
    },
    maps:fold(
        fun(Target, Source, S) ->
            write_file(Dest, Target, Source, Force, Context, S)
        end,
        State1,
        Tree
    ).

find_replacement_files(#{apps := Apps, project_root := Root, board := Board}, SubDir) ->
    Sorted = case lists:keytake(grisp, 1, Apps) of
        {value, Grisp, Rest} -> [Grisp|Rest];
        false                -> Apps
    end,
    Dirs = [Dir || {_App, Dir} <- Sorted] ++ [Root],
    lists:foldl(fun(Files, Acc) -> maps:merge(Acc, Files) end, #{}, [grisp_files(Dir, Board, SubDir) || Dir <- Dirs]).

grisp_files(Dir, Board, Subdir) ->
    Path = filename:join([Dir, "grisp", Board, Subdir]),
    resolve_files(find_files(Path), Path).

find_files(Dir) ->
    [F || F <- filelib:wildcard(Dir ++ "/**"), filelib:is_regular(F)].

resolve_files(Files, Root) -> resolve_files(Files, Root, #{}).

resolve_files([File|Files], Root, Resolved) ->
    Relative = prefix(File, Root ++ "/"),
    Name = filename:rootname(Relative, ".mustache"),
    resolve_files(Files, Root, maps:put(
        Name,
        resolve_file(Root, Relative, Name, maps:find(Name, Resolved)),
        Resolved
    ));
resolve_files([], _Root, Resolved) ->
    Resolved.

prefix(String, Prefix) ->
    case lists:split(length(Prefix), String) of
        {Prefix, Rest} -> Rest;
        _              -> String
    end.

resolve_file(Root, Source, Source, error) ->
    filename:join(Root, Source);
resolve_file(Root, Source, _Target, _) ->
    {template, filename:join(Root, Source)}.

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
    Command = case Force of
        true  -> "cp -Rf";
        false -> "cp -R"
    end,
    {Output, State2} = shell(State1, string:join([Command, qoute(Source ++ "/"), qoute(Target)], " ")),
    event(State2, {deployment, release, {copy, {result, Output}}}).

qoute(String) -> "\"" ++ String ++ "\"".

% Helpers

download_loop(ReqID, #{package := #{tmp := Tmp}} = State0) ->
    with_file(Tmp, [raw, append, binary], fun(Handle) ->
        download_loop({ReqID, undefined}, Handle, State0, 0)
    end).

download_loop({ReqID, RequestPid}, Handle, State0, Size) ->
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
            download_loop({ReqID, Pid}, Handle, State1, Size);
        {http, {ReqID, stream, BinBodyPart}} ->
            NewSize = Size + byte_size(BinBodyPart),
            State1 = event(State0, {package, {download_progress, NewSize}}),
            ok = httpc:stream_next(RequestPid),
            ok = file:write(Handle, BinBodyPart),
            download_loop({ReqID, RequestPid}, Handle, State1, NewSize);
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

http_get(Request, Options, InetsPid) ->
    HTTPOptions = [{connect_timeout, 5000}],
    {ok, ID} = httpc:request(get, Request, HTTPOptions, Options, InetsPid),
    ID.

run_script(Name, State0) ->
    case mapz:deep_get([scripts, Name], State0, undefined) of
        undefined -> State0;
        Script ->
            State1 = event(State0, {deployment, script, Name, {run, Script}}),
            {Output, State2} = shell(State1, Script),
            event(State2, {deployment, script, Name, {result, Output}})
    end.

event(State0, Event) ->
    {_Result, State1} = exec(event, State0, [Event]),
    State1.

release(State0, Release) ->
    {Dir, State1} = exec(release, State0, [Release]),
    mapz:deep_put([release, dir], Dir, State1).

shell(State, Script)    -> exec(shell, State, [Script]).

exec(Handler, #{handlers := Handlers} = State, Args) ->
    {Result, NewHandlers} = grisp_tools_handler:run(Handler, Args, Handlers),
    {Result, State#{handlers => NewHandlers}}.
