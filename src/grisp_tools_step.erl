-module(grisp_tools_step).

% API
-export([config/1]).
-export([version/1]).
-export([apps/1]).
-export([collect/1]).

-import(grisp_tools_util, [event/2]).
-import(grisp_tools_util, [shell/2]).

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
    ReOpts = [extended, global, notempty, {capture, all_names, binary}],
    {match, [RawVersion]} = re:run(SVersion, ?RE_VERSION, ReOpts),
    {Version, Pre, Build, Full} = parse_version(RawVersion),
    Indexed = lists:zip(lists:seq(1, length(Version)), Version),
    case find_version({Indexed, Pre, Build, Full}, Versions) of
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
            Root = maps:get(project_root, S3),
            Platform = maps:get(platform, S3),
            mapz:deep_merge(S3, #{
                otp_version => Found,
                otp_version_list => [<<"common">>] ++ version_list(Found, 1, []),
                paths => grisp_tools_util:paths(Root, Platform, Found)
            })
    end.

available_versions(#{custom_build := true} = S0) ->
    {{ok, Output}, S1} = shell(S0,
        "git ls-remote --tags --refs https://github.com/erlang/otp"
    ),
    {parse_versions(Output), S1};
available_versions(#{platform := Platform} = S0) ->
    PackageVersions = grisp_tools_package:list(#{type => otp, platform => Platform}),
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

collect(#{platform := Platform} = S0) ->
    Platforms = [default, Platform],
    S1 = lists:foldl(fun collect_platform_files/2, S0, Platforms),
    {Hash, HashIndex} = grisp_tools_util:build_hash(S1),
    S2 = event(S1, [{hash, Hash, HashIndex}]),
    mapz:deep_put([build, hash], #{value => Hash, index => HashIndex}, S2).

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
