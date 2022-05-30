-module(grisp_tools_step).

% API
-export([config/1]).
-export([version/1]).
-export([apps/1]).
-export([collect/1]).
-export([toolchain/1]).

-import(grisp_tools_util, [event/2]).
-import(grisp_tools_util, [shell/3]).

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

config(S0) ->
    Env = #{
        "GRISP" => "yes",
        "GRISP_PLATFORM" => atom_to_list(maps:get(platform, S0, grisp2))
    },
    mapz:deep_merge(S0, #{shell => #{env => Env}}).

version(#{otp_version_requirement := SVersion} = S0) ->
    {Versions, S1} = available_versions(S0),
    {Type, RawVersion} = parse_version_requirement(SVersion),
    {Version, Pre, Build, Full} = parse_version(RawVersion),
    Indexed = lists:zip(lists:seq(1, length(Version)), Version),
    case find_version({Indexed, Pre, Build, Full}, Type, Versions) of
        {error, not_found} ->
            error({otp_version_not_found, SVersion});
        {ok, {[Major|_Version], _Pre, _Build, FoundFull} = Found} ->
            % TODO: Use the exact OTP_VERSION file here instead?
            % https://erlang.org/doc/system_principles/versions.html
            S2 = case {Major, list_to_integer(erlang:system_info(otp_release))} of
                {Target, Target}  -> S1;
                {Target, Current} -> event(S1, [{mismatch, Target, Current}])
            end,
            S3 = event(S2, [{selected, FoundFull, SVersion}]),
            mapz:deep_merge(S3, #{
                otp_version => Found,
                otp_version_list => [<<"common">>] ++ version_list(Found, 1, [])
            })
    end.

available_versions(#{custom_build := true} = S0) ->
    Cmd = "git ls-remote --tags --refs https://github.com/erlang/otp",
    case shell(S0, Cmd, [return_on_error]) of
        {{ok, Output}, S1} ->
            {parse_versions(Output), S1};
        {{error, { _, "fatal: unable to access 'https://github.com/erlang/otp/': "
                        "Could not resolve host: github.com\n"}}, S1} ->
            S1 = event(S0, [{error, Cmd}]),
            VersionNames = list_local_checkouts(S0),
            Versions = [begin
                    {match, [Vsn]} = re:run(V, ?RE_VERSION, [extended, global, notempty, {capture, all_names, binary}]),
                    parse_version(Vsn)
                end || V <- lists:usort(VersionNames)],
            {Versions, S1};
        _ ->
            throw(rebar_abort)
    end;
available_versions(#{platform := Platform} = S0) ->
    PackageVersions =
        try grisp_tools_package:list_online(#{type => otp, platform => Platform})
        catch
            error:_Error ->
                event(S0, "Could not list packages, using cache ..."),
                grisp_tools_package:list_local(otp)
        end,
    Versions = [begin
        {match, [Vsn]} = re:run(V, ?RE_VERSION, [extended, global, notempty, {capture, all_names, binary}]),
        parse_version(Vsn)
    end || V <- lists:usort([V1 || #{version := V1} <- PackageVersions])],
    {Versions, S0}.

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

collect(#{project_root := Root, platform := Platform,
          otp_version := Version, custom_build := CustomBuild} = S0) ->
    Platforms = [default, Platform],
    S1 = lists:foldl(fun collect_platform_files/2, S0, Platforms),
    {Hash, HashIndex} = grisp_tools_util:build_hash(S1),
    S2 = event(S1, [{hash, Hash, HashIndex}]),
    S3 = mapz:deep_merge(S2, #{
        paths => grisp_tools_util:paths(Root, Platform, Version, Hash, CustomBuild)
    }),
    mapz:deep_put([build, hash], #{value => Hash, index => HashIndex}, S3).

toolchain(S0) ->
    ToolchainRoot = mapz:deep_get([paths, toolchain], S0),
    [error({toolchain_root_invalid, ToolchainRoot}) || not filelib:is_dir(ToolchainRoot)],
    Files = [
        ["GRISP_TOOLCHAIN_REVISION"],
        ["GRISP_TOOLCHAIN_PLATFORM"],
        ["grisp_buildinfo.hrl"],
        ["arm-rtems5"],
        ["arm-rtems5", "atsamv"],
        ["arm-rtems5", "imx7"],
        ["bin","arm-rtems5-gcc"]
    ],
    [ check_toolchain_file([ToolchainRoot|File]) || File <- Files],
    S0.

%--- Internal ------------------------------------------------------------------

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

parse_version_requirement("=" ++ SVersion) ->
    {strict, parse_version_name(SVersion)};
parse_version_requirement(SVersion) ->
    {fuzzy, parse_version_name(SVersion)}.

parse_version_name(SVersion) ->
    ReOpts = [extended, global, notempty, {capture, all_names, binary}],
    {match, [RawVersion]} = re:run(SVersion, ?RE_VERSION, ReOpts),
    RawVersion.

find_version(Requested, strict, Availables) ->
    find_version_strict(Requested, Availables);
find_version(Requested, fuzzy, Availables) ->
    find_version_fuzzy(Requested, Availables).

find_version_strict({_VN_list, Pre, Build, Full}, Versions) ->
    case lists:filter(fun
            ({_Ver, P, B, VerFullBin}) 
              when P =:= Pre, B =:= Build, VerFullBin =:= Full ->
                true;
            (_) ->
                false end, Versions) of
        [V|_] -> {ok, V};
        [] -> {error, not_found}
    end.

find_version_fuzzy({[], _, _, _}, []) ->
    {error, not_found};
find_version_fuzzy({[], _, _, _}, Versions) ->
    {ok, hd(lists:reverse(lists:sort(Versions)))};
find_version_fuzzy({[{N, V}|Version], Pre, Build, Full}, Versions) ->
    find_version_fuzzy(
        {Version, Pre, Build, Full},
        lists:filter(fun
            ({Ver, P, B, _F}) when P =:= Pre, B =:= Build ->
                length(Ver) >= N andalso lists:nth(N, Ver) == V;
            (_) ->
                false
        end, Versions)
    ).

version_list({Version, Pre, Build, _Full}, N, [Last|_] = List) 
  when N > length(Version) ->
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

collect_platform_files(Platform, #{sorted_apps := Apps} = State) ->
    lists:foldl(fun(App, S) ->
        collect_app_files(App, Platform, S)
    end, State, Apps).

collect_app_files(App, Platform, #{apps := Apps, otp_version_list := Versions} = S0) ->
    Dir = mapz:deep_get([App, dir], Apps),
    {BuildOverlay, Config} = grisp_tools_util:build_overlay(App, Dir, Platform, Versions),
    S1 = mapz:deep_merge(S0, #{build => #{overlay => BuildOverlay}}),
    DeployOverlay = grisp_tools_util:deploy_overlay(App, Dir, Platform, Versions),
    S2 = mapz:deep_merge(S1, #{deploy => #{overlay => DeployOverlay}}),
    mapz:deep_update_with([build, config], fun(C) ->
        grisp_tools_util:merge_build_config(C, Config)
    end, Config, S2).

check_toolchain_file(PathElements) ->
    Path = filename:join(PathElements),
    [error({toolchain_root_invalid, Path}) || not filelib:is_file(Path)].

list_local_checkouts(#{project_root := Root, platform := Platform, custom_build := true}) ->
    Dir = grisp_tools_util:otp_checkout_dir(Root, Platform),
    {ok, Versions} = file:list_dir(Dir),
    [ list_to_binary(V) || V <- Versions].
