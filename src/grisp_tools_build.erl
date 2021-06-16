-module(grisp_tools_build).

% API
-export([run/1]).

%--- Macros --------------------------------------------------------------------

-define(RE_VERSION, "
    (?<VERSION>


    (?<MAJOR>\\d+)
    (?:
        \\.(?<MINOR>\\d+)
        (?:
            \\.(?<PATCH>\\d+)
            (?:
                \\.(?<EXTRA1>\\d+)
                (?:
                    \\.(?<EXTRA2>\\d+)
                )?
            )?
        )?
    )?

    (?:-(?<PRE>[^\\+\\s]*))?

    (?:\\+(?<BUILD>.*))?

    )
").

%--- API -----------------------------------------------------------------------

run(Configuration) ->
    State = mapz:deep_merge(Configuration, #{
        event_stack => [],
        build => #{
            overlay => #{
                files => #{},
                patches => #{},
                drivers => #{},
                nifs => #{}
            },
            config => #{}
        }
    }),

    weave(State, [
        {fun build/1, [
            {validate, [
                fun apps/1,
                fun version/1
            ]},
            fun collect/1,
            fun download/1,
            {prepare, [
                fun clean/1,
                fun patch/1,
                fun files/1,
                fun drivers/1,
                fun nifs/1
            ]},
            {compile, [
                fun configure/1,
                fun boot/1,
                fun install/1,
                fun post/1
            ]},
            fun tar/1
        ]}
    ]).

    % pipe(State, [build], [
    %     fun validate/1,
    %     fun config/1,
    %     fun collect_files/1,
    %     fun otp_download/1,
    %     fun otp_clean/1,
    %     fun otp_patch/1,
    %     fun otp_build/1,
    %     % fun otp_install/1,
    %     fun debug_state/1
    % ]).

weave(S0, []) ->
    S0;
weave(S0, [{Step, Inner}|Steps]) ->
    weave(with_event(S0, [name(Step)], fun(S1) ->
        S2 = case Step of
            Step when is_atom(Step) -> S1;
            Step when is_function(Step) -> Step(S1)
        end,
        weave(S2, Inner)
    end), Steps);
weave(S0, [debug|Steps]) ->
    io:format("~p~n", [S0]),
    weave(S0, Steps);
weave(_S0, [abort|_Steps]) ->
    event(_S0, [abort]);
weave(S0, [Step|Steps]) ->
    weave(with_event(S0, [name(Step)], Step), Steps).

%--- Internal Steps ------------------------------------------------------------

build(S0) ->
    ToolchainRoot = mapz:deep_get([paths, toolchain], S0),
    PATH = os:getenv("PATH"),
    Env = #{
        "GRISP_TC_ROOT" => ToolchainRoot,
        "PATH" => filename:join(ToolchainRoot, "rtems/5/bin") ++ ":" ++ PATH
    },
    mapz:deep_merge(S0, #{shell => #{env => Env}}).

collect(#{platform := Platform} = State0) ->
    Platforms = [default, Platform],
    lists:foldl(fun collect_platform_files/2, State0, Platforms).

download(#{otp_version := {_,_,_,Ver}} = State0) ->
    BuildPath = mapz:deep_get([paths, build], State0),
    % FIXME: Check actual state of checkout instead of just checking for folder
    % presence!
    case filelib:is_dir(filename:join(BuildPath, ".git")) of
        false ->
            URL = grisp_tools_util:env(otp_git_url),
            Branch = ["OTP-", Ver],
            {{ok, Output}, State1} = shell(State0,
                "git clone "
                "-b " ++ Branch ++ " "
                "--single-branch " ++
                URL ++ " " ++BuildPath
            ),
            event(mapz:deep_put([build, download], true, State1), [{output, Output}]);
        true ->
            event(mapz:deep_put([build, download], false, State0), ['_skip'])
    end.

clean(#{build := #{flags := #{clean := true}}} = State0) ->
    State1 = event(State0, ['_run']),
    BuildPath = mapz:deep_get([paths, build], State1),
    grisp_tools_util:pipe(State1, [
        fun(S) -> shell_ok(S, "git reset --hard", [{cd, BuildPath}]) end,
        fun(S) -> shell_ok(S, "git clean -fXd", [{cd, BuildPath}]) end,
        fun(S) -> shell_ok(S, "git clean -fxd", [{cd, BuildPath}]) end
    ]);
clean(State0) ->
    event(State0, ['_skip']).

patch(#{build := #{overlay := #{patches := Patches}}} = State0) ->
    Sorted = lists:sort(maps:to_list(Patches)),

    Drivers = maps:to_list(mapz:deep_get([build, overlay, drivers], State0)),
    NIFs = maps:to_list(mapz:deep_get([build, overlay, nifs], State0)),

    DrvPatchLineCount = 10 + length(Drivers),
    NifPatchLineCount = 9 + length(NIFs),

    Context = #{
        erts_emulator_makefile_in => #{
            driver_lines => DrvPatchLineCount,
            nif_lines => NifPatchLineCount,
            total_lines => DrvPatchLineCount + NifPatchLineCount,
            drivers => [#{name => filename:basename(N, ".c")} || {N, _P} <- Drivers],
            nifs => [#{name => filename:basename(N, ".c")} || {N, _P} <- NIFs]
        }
    },
    lists:foldl(fun apply_patch/2, mapz:deep_put([build, patch_context], Context, State0), Sorted).

files(State0) -> copy_files(State0, files, "").

drivers(State0) -> copy_files(State0, drivers, "").

nifs(State0) -> copy_files(State0, nifs, "").

copy_files(State0, Type, SubDir) ->
    #{
        sorted_apps := Apps,
        build := #{overlay := #{Type := FileMap}},
        paths := #{build := Root}
    } = State0,
    case maps:size(FileMap) of
        0 ->
            event(State0, ['_skip']);
        _Size ->
            Files = sort_files(Apps, FileMap),
            Target = filename:join(Root, SubDir),
            lists:foldl(fun(F, S) -> copy_file(Target, F, S) end, State0, Files)
    end.

configure(#{build := Build} = State0) ->
    #{
        download := WasDownloaded,
        flags := #{clean := DidClean, configure := ShouldConfigure}
    } = Build,
    % We must configure if we just downloaded or cleaned the repository
    case WasDownloaded orelse DidClean orelse ShouldConfigure of
        true ->
            Opts = [{cd, mapz:deep_get([paths, build], State0)}],
            XCompConf = mapz:deep_get([build, config, xcomp_conf], State0),
            Command = [
                "./otp_build configure ",
                " --prefix=/",

                % Disable apps not built for target
                " --without-debugger",
                " --without-dialyzer",
                " --without-erl_interface",
                " --without-et",
                " --without-hipe",
                " --without-javac",
                " --without-jinterface",
                " --without-megaco",
                " --without-observer",
                " --without-odbc",
                " --without-os_mon",
                " --without-typer",
                " --without-wx",
                " --xcomp-conf=", XCompConf
            ],
            build_step(Command, Opts, State0);
        false ->
            event(State0, ['_skip'])
    end.

boot(State0) ->
    Opts = [{cd, mapz:deep_get([paths, build], State0)}],
    build_step("./otp_build boot -a", Opts, State0).

install(S0) ->
    BuildPath = mapz:deep_get([paths, build], S0),
    InstallPath = mapz:deep_get([paths, install], S0),

    grisp_tools_util:ensure_dir(filename:join(InstallPath, ".")),
    S1 = grisp_tools_util:pipe(S0, [
        fun(S) -> shell_ok(S, ["rm -rf ", filename:join(InstallPath, "*")], [{cd, InstallPath}]) end,
        fun(S) -> shell_ok(S, ["make install DESTDIR=", $", InstallPath, $"], [{cd, BuildPath}]) end
    ]),

    [ERTS] = filelib:wildcard(to_list(filename:join(InstallPath, "lib/erlang/erts-*"))),
    Ver = lists:last(string:split(ERTS, "-", trailing)),

    S2 = mapz:deep_merge(S1, #{shell => #{env => #{
        "OTP_ROOT" => to_list(InstallPath),
        "ERTS_VERSION" => Ver
    }}}),

    run_hooks(S2, post_install, [{cd, InstallPath}]).

post(#{build := #{overlay := Overlay}} = S0) ->
    % info("Computing file hashes"),
    {Hash, HashIndex} = grisp_tools_util:overlay_hash(Overlay),


    % info("Writing hashes to file. Hash: ~p", [Hash]),
    PackageListing = filename:join(mapz:deep_get([paths, install], S0), "GRISP_PACKAGE_FILES"),
    ok = file:write_file(PackageListing, HashIndex),

    % info("Copying revision string into install dir"),
    ToolchainRoot = mapz:deep_get([paths, toolchain], S0),
    InstallPath = mapz:deep_get([paths, install], S0),
    RevSource = filename:join([ToolchainRoot, "rtems/5", "GRISP_TOOLCHAIN_REVISION"]),
    RevDestination = filename:join(InstallPath, "GRISP_TOOLCHAIN_REVISION"),
    case file:copy(RevSource, RevDestination) of
        {ok, _} -> ok;
        _ -> error({missing_toolchain_revision, RevSource})
    end,
    mapz:deep_put([build, hash], Hash, S0).

tar(#{build := #{flags := #{tar := true}}} = S0) ->
    #{
        build := #{hash := Hash},
        otp_version := {_, _, _, Version},
        paths := #{package := PackagePath, install := InstallPath}
    } = S0,
    CacheFile = grisp_tools_util:package_name(#{otp_version => Version, hash => Hash}),
    Filename = filename:join(PackagePath, CacheFile),
    grisp_tools_util:ensure_dir(Filename),
    shell_ok(S0, ["tar -zcf ", Filename, " ."], [{cd, InstallPath}]),
    event(S0, [{file, relative(Filename)}]);
tar(S0) ->
    event(S0, ['_skip']).

build_step(Command, Opts, State0) ->
    {{ok, Output}, State1} = shell(State0, Command, Opts),
    event(State1, [{output, Output}]).

%--- Internal ------------------------------------------------------------------

apps(#{apps := Apps} = State0) ->
    Graph = digraph:new([acyclic]),
    lists:map(fun({A, C}) ->
        Deps = maps:get(deps, C, []),
        digraph:add_vertex(Graph, A),
        lists:foreach(fun(D) ->
            digraph:add_vertex(Graph, D),
            % Since we want dependencies first, add an edge from the dependency
            % to the app
            digraph:add_edge(Graph, D, A)
        end, Deps)
    end, Apps),

    State1 = validate_apps(State0, Apps, Graph),

    DependsOnGrisp = digraph_utils:reachable([grisp], Graph),
    GrispRelevant = digraph_utils:subgraph(Graph, DependsOnGrisp),
    Sorted = digraph_utils:topsort(GrispRelevant),
    digraph:delete(Graph),
    State1#{apps => maps:from_list(Apps), sorted_apps => Sorted}.

version(#{otp_version_requirement := SVersion} = S0) ->
    {{ok, Output}, S1} = shell(S0,
        "git ls-remote --tags --refs https://github.com/erlang/otp"
    ),
    Versions = parse_versions(Output),
    ReOpts = [extended, global, notempty, {capture, all_names, binary}],
    {match, [RawVersion]} = re:run(SVersion, ?RE_VERSION, ReOpts),
    {Version, Pre, Build, Full} = parse_version(RawVersion),
    Indexed = lists:zip(lists:seq(1, length(Version)), Version),
    case find_version({Indexed, Pre, Build, Full}, Versions) of
        {error, not_found} ->
            error({otp_version_not_found, SVersion});
        {ok, {_Version, _Pre, _Build, FoundFull} = Found} ->
            S2 = event(S1, [{selected, FoundFull, SVersion}]),
            Root = maps:get(project_root, S2),
            Platform = maps:get(platform, S2),
            S3 = mapz:deep_merge(S2, #{
                otp_version => Found,
                otp_version_list => [<<"common">>] ++ version_list(Found, 1, []),
                paths => #{
                    % FIXME: Code smell
                    build => grisp_tools_util:otp_build_path(Root, Platform, Found),
                    install => grisp_tools_util:otp_install_path(Root, Platform, Found),
                    package => grisp_tools_util:otp_package_path(Root, Platform, Found)
                }
            }),
            maps:without([erlang_versions], S3)
    end.

validate_apps(State, Apps, Graph) ->
    lists:foldl(fun({A, #{dir := Dir}}, S) ->
        case {A, digraph:get_path(Graph, grisp, A)} of
            {A, false} when A =/= grisp ->
                case filelib:is_dir(filename:join(Dir, "grisp")) of
                    true -> event(S, [{grisp_dir_without_dep, A}]);
                    _ -> S
                end;
            _Else ->
                S
        end
    end, State, Apps).

parse_versions(Output) ->
    {match, RawVersions} = re:run(
        Output,
        "
            [^\\s]*\t # Git hash and tab character

            refs/tags/OTP- # OTP Git tag prefix

            " ?RE_VERSION "

            .*\n
        ",
        [extended, global, notempty, {capture, all_names, binary}]
    ),
    [parse_version(V) || V <- RawVersions].

parse_version([Build, Extra1, Extra2, Major, Minor, Patch, Pre, Full]) ->
    {[I || I <- [
        parse_version_integer(Major),
        parse_version_integer(Minor),
        parse_version_integer(Patch),
        parse_version_integer(Extra1),
        parse_version_integer(Extra2)
    ], I =/= <<>>], Pre, Build, Full}.

parse_version_integer(<<>>) -> <<>>;
parse_version_integer(Version) -> binary_to_integer(Version).

find_version({[], _, _, _}, []) ->
    {error, not_found};
find_version({[], _, _, _}, Versions) ->
    {ok, hd(lists:reverse(lists:sort(Versions)))};
find_version({[{N, V}|Version], Pre, Build, Full}, Versions) ->
    find_version(
        {Version, Pre, Build, Full},
        lists:filter(fun
            ({Ver, P, B, _F}) when P =:= Pre, B =:= Build ->
                length(Ver) >= N andalso lists:nth(N, Ver) == V;
            (_) ->
                false
        end, Versions)
    ).

version_list({Version, Pre, Build, _Full}, N, [Last|_] = List) when
    N > length(Version)
->
    Extra = case {Pre, Build} of
        {<<>>, <<>>} -> [];
        {Pre, <<>>} -> [iolist_to_binary([Last, "-", Pre])];
        {<<>>, Build} -> [iolist_to_binary([Last, "+", Build])];
        {Pre, Build} -> [iolist_to_binary([Last, "-", Pre, "+", Build])]
    end,
    lists:reverse(Extra ++ List);
version_list({Version, _Pre, _Build, _Full} = Ver, N, List) ->
    Components = [integer_to_list(V) || V <- lists:sublist(Version, N)],
    Intermediate = iolist_to_binary(string:join(Components, ".")),
    version_list(Ver, N + 1, [Intermediate|List]).

collect_platform_files(Platform, #{sorted_apps := Apps} = State) ->
    lists:foldl(fun(App, S) ->
        collect_app_files(App, Platform, S)
    end, State, Apps).

collect_app_files(App, Platform, #{apps := Apps, otp_version_list := Versions} = State0) ->
    Dir = mapz:deep_get([App, dir], Apps),
    {Overlay, Config} = grisp_tools_util:build_overlay(App, Dir, Platform, Versions),
    State1 = mapz:deep_merge(State0, #{build => #{overlay => Overlay}}),
    mapz:deep_update_with([build, config], fun(C) ->
        grisp_tools_util:merge_build_config(C, Config)
    end, State1).

apply_patch({Name, #{source := File} = Patch}, State0) ->
    Dir = mapz:deep_get([paths, build], State0),
    Context = mapz:deep_get([build, patch_context], State0),
    Rendered = grisp_tools_template:render(File, Context),
    ok = file:write_file(filename:join(Dir, Name), Rendered),
    State4 = case shell(State0, "git apply " ++ Name ++ " --ignore-whitespace --reverse --check",
            [{cd, Dir}, return_on_error]) of
        {{ok, _Output}, State1} ->
            event(State1, [{skip, Patch}]);
        {{error, {1, _}}, State1} ->
            State2 = event(State1, [{apply, Patch}]),
            {{ok, _}, State3} = shell(State2, "git apply --ignore-whitespace " ++ Name, [{cd, Dir}]),
            State3
    end,
    {{ok, _}, State5} = shell(State4, "rm " ++ Name, [{cd, Dir}]),
    State5.

sort_files(Apps, Files) ->
    Indices = maps:from_list(lists:zip(Apps, lists:seq(1, length(Apps)))),
    SortFiles = fun
        (#{app := A, name := N1}, #{app := A, name := N2}) ->
            N1 =< N2;
        (#{app := A1}, #{app := A2}) ->
            maps:get(A1, Indices) =< maps:get(A2, Indices)
    end,
    lists:sort(SortFiles, [F || {_, F} <- maps:to_list(Files)]).

copy_file(Root, #{dest := Dest, source := Path} = File, State0) ->
    State1 = event(State0, [{copy, File}]),
    file:copy(Path, filename:join(Root, Dest)),
    State1.

run_hooks(S0, Type, Opts) ->
    Hooks = mapz:deep_get([build, overlay, hooks, Type], S0, []),
    case maps:size(Hooks) of
        0 ->
            S0;
        _Else ->
            S1 = event(S0, [hook, Type]),
            lists:foldl(fun({_Name, #{source := Source} = Hook}, S2) ->
                S3 = event(S2, [hook, Type, {run, Hook}]),
                shell_ok(S3, Source, Opts)
            end, S1, lists:sort(maps:to_list(Hooks)))
    end.

shell_ok(State0, Hook, Opts) ->
    {{ok, _Output}, State1} = shell(State0, Hook, Opts),
    State1.

shell(State, Hook) -> shell(State, Hook, []).

shell(#{shell := #{env := Env}} = State, Hook, Opts) ->
    exec(shell, State, [Hook, [{env, maps:to_list(Env)}] ++ Opts]).

event(#{event_stack := Stack} = State0, Events) when is_list(Events) ->
    {_Result, State1} = exec(event, State0, [Stack ++ Events]),
    State1;
event(State0, Event) -> % TODO: Remove this clause
    {_Result, State1} = exec(event, State0, [Event]),
    State1.

exec(Handler, #{handlers := Handlers} = State, Args) ->
    {Result, NewHandlers} = grisp_tools_handler:run(Handler, Args, Handlers),
    {Result, State#{handlers => NewHandlers}}.

with_event(#{event_stack := Current} = State0, New, Fun) ->
    Stack = Current ++ New,
    grisp_tools_util:pipe(State0, [
        fun(S) -> event(S#{event_stack => Stack}, []) end,
        fun(S) -> Fun(S#{event_stack => Stack}) end,
        % fun(S) -> event(S, ['_end']) end,
        fun(S) -> S#{event_stack => Current} end
    ]).

    % State1 = event(State0#{event_stack => Stack}, ['_begin']),
    % State2 = Fun(State1),
    % State3 = event(State2, ['_end']),
    % State3#{event_stack => Current}.

name(Atom) when is_atom(Atom) ->
    Atom;
name(Fun) when is_function(Fun) ->
    {name, AName} = erlang:fun_info(Fun, name),
    Name = atom_to_list(AName),
    list_to_atom(string:slice(Name, 5, length(Name) - 8)).

to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(List) when is_list(List) -> List.

relative(Path) ->
    {ok, CWD} = file:get_cwd(),
    string:trim(string:prefix(Path, CWD), leading, "/").
