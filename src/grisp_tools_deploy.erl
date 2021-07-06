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
        {deploy, [
            {validate, [
                fun grisp_tools_step:apps/1,
                fun grisp_tools_step:version/1
            ]},
            fun grisp_tools_step:collect/1,
            fun package/1,
            fun release/1,
            fun copy/1
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

copy(State0) ->
    grisp_tools_util:pipe(State0, [
        fun(S) -> run_script(pre_script, S) end,
        fun copy_files/1,
        fun copy_release/1,
        fun(S) -> run_script(post_script, S) end
    ]).

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

extract(#{package := #{state := downloaded, file := File}} = State0) ->
    #{paths := #{install := InstallPath}} = State0,
    State1 = event(State0, [{start, File}]),
    case erl_tar:extract(File, [compressed, {cwd, InstallPath}]) of
        ok              -> event(State1, [done]);
        {error, Reason} -> event(State1, [{error, Reason}])
    end;
extract(State0) ->
    event(State0, ['_skip']).

copy_files(#{copy := #{destination := Dest, force := Force}, release := Release} = State0) ->
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
            write_file(Dest, File, Force, Context, S)
        end,
        State1,
        mapz:deep_get([deploy, overlay, files], State0)
    ).

write_file(Dest, #{target := Target, source := Source} = File, Force, Context, State0) ->
    Path = filename:join(Dest, Target),
    State1 = event(State0, [files, {copy, File}]),
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
            event(State0, [files, {error, {exists, File}}]);
        _ ->
            State0
    end,
    Fun(File),
    State1.

copy_release(#{release := Release, copy := Copy} = State0) ->
    #{name := RelName, dir := Source} = Release,
    #{destination := Dest, force := Force} = Copy,
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
            #{otp_version := OTPVersion, build := #{hash := #{value := Hash}}} = State0,
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

run_script(Name, State0) ->
    case mapz:deep_get([scripts, Name], State0, undefined) of
        undefined -> State0;
        Script ->
            State1 = event(State0, [Name, {run, Script}]),
            {{ok, Output}, State2} = shell(State1, Script),
            event(State2, [Name, {result, Output}])
    end.

release(State0, Release) ->
    State1 = event(State0, [{start, Release}]),
    {Spec, State2} = exec(release, State1, [Release]),
    case maps:merge(Release, Spec) of
        #{name := Name, dir := Dir, version := Version} = Merged
          when length(Name) > 0, length(Dir) > 0, length(Version) > 0 ->
            State3 = event(State2, [{done, Merged}]),
            mapz:deep_merge(State3, #{release => Merged});
        Merged ->
            error({invalid_release, Merged})
    end.
