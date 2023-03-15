-module(grisp_tools_report).

% API
-export([run/1]).

-import(grisp_tools_util, [event/2]).
-import(grisp_tools_util, [shell/2]).
-import(grisp_tools_util, [shell/3]).
-import(grisp_tools_util, [ensure_dir/1]).
-import(grisp_tools_util, [write_file/3]).

%--- API -----------------------------------------------------------------------

run(State) ->
    grisp_tools_util:weave(State, [
        fun grisp_tools_step:config/1,
        {report, [
            fun clean/1,
            {validate, [
                fun grisp_tools_step:apps/1,
                fun grisp_tools_step:version/1
            ]},
            fun grisp_tools_step:collect/1,
            fun write_report/1,
            fun tar/1
        ]}
    ]).

%--- Tasks ---------------------------------------------------------------------

clean(#{flags := #{tar := true}} = S0) -> S0;
clean(#{report_dir := ReportDir} = S0) ->
    Cmd = lists:append(["rm -r ", ReportDir]),
    {_, S1} = shell(S0, Cmd, [return_on_error]),
    Cmd2 = lists:append(["rm ", ReportDir, ".tar.gz"]),
    {_, S2} = shell(S1, Cmd2, [return_on_error]),
    S2.

write_report(#{flags := #{tar := true}} = S0) -> event(S0, [skip]);
write_report(#{report_dir := ReportDir} = S0) ->
    S1 = grisp_tools_util:pipe(S0, [
        fun hash_index/1,
        fun build_overlay/1,
        fun project_settings/1
    ]),
    event(S1, [{new_report, ReportDir}]).

tar(#{flags := #{tar := false}} = S0) -> S0;
tar(#{report_dir := ReportDir, flags := #{tar := true}} = S0) ->
    case filelib:is_dir(ReportDir) of
        false ->
            event(S0, [{error, no_report}]);
        true ->
            TarFile = ReportDir ++ ".tar.gz",
            Cmd = lists:append(["tar -czvf ", TarFile, " -C ", ReportDir, " ."]),
            {{ok, _Res}, S1} = shell(S0, Cmd),
            event(S1, [TarFile])
    end.

%--- Internal ------------------------------------------------------------------

project_settings(S0) ->
    S1 = copy_project_file("rebar.config", S0),
    copy_project_file("rebar.lock", S1).

hash_index(#{
    report_dir := ReportDir,
    build := #{
        hash := Hash
}} = S) ->
    HashInfo = filename:join(ReportDir, "hash.txt"),
    ensure_dir(HashInfo),
    file:write_file(HashInfo, io_lib:format("~p~n", [Hash])),
    event(S, [write, HashInfo]).

build_overlay(#{
    project_root := Project_root,
    report_dir := ReportDir,
    build := #{
        overlay := BuildOverlay
    }} = S)  ->
    BuildInfo = filename:join(ReportDir, "build_overlay.txt"),
    ensure_dir(BuildInfo),
    ProjectForlder = list_to_binary(filename:basename(
                                                filename:dirname(Project_root))),
    BuildOverlay2 = strip_absolute_paths(BuildOverlay, ProjectForlder),
    file:write_file(BuildInfo, io_lib:format("~p~n", [BuildOverlay2])),
    event(S, [write, BuildInfo]).

copy_project_file(Filename, #{project_root := Root, report_dir := ReportDir} = S0) ->
    Src = filename:join(Root, Filename),
    Dst = filename:join(ReportDir, Filename),
    Copy = #{source => Src, target => Dst},
    try
            write_file(Root, Copy, #{}),
            event(S0, [files, {copy, Dst}])
        catch error:_ ->
            event(S0, [files, {missing, Src}])
    end.

strip_absolute_paths({_Term, Binary}, Seam) when is_binary(Binary) ->
    {_Term, strip_absolute_paths(Binary, Seam)};
strip_absolute_paths(Term, Seam) when is_binary(Term) ->
    case binary:split(Term, Seam) of
        [_RootPath, InnerPath] -> iolist_to_binary([Seam, InnerPath]);
        [Skipped] -> Skipped
    end;
strip_absolute_paths(Term, Seam) when is_map(Term) ->
    maps:from_list([ {K, strip_absolute_paths(V, Seam)} || {K,V}
                                                        <- maps:to_list(Term)]);
strip_absolute_paths(Term, _) ->
    Term.
