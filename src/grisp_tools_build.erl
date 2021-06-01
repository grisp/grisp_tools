-module(grisp_tools_build).

% API
-export([run/1]).

%--- Macros --------------------------------------------------------------------

-define(version_re, "
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
    State = maps:merge(Configuration, #{
        result => #{
            files => [],
            patches => [],
            overlays => [],
            drivers => [],
            nifs => [],
            config => #{}
        }
    }),
    grisp_tools_util:pipe(State, [
        fun sort_apps/1,
        fun get_erlang_versions/1,
        fun find_erlang_version/1
    ]).

%--- Internal ------------------------------------------------------------------

sort_apps(#{apps := Apps} = State0) ->
    Graph = digraph:new([acyclic]),
    lists:map(fun({A, C}) ->
        Deps = maps:get(deps, C, []),
        digraph:add_vertex(Graph, A),
        lists:foreach(fun(D) ->
            digraph:add_vertex(Graph, D),
            % Since we want dependecies first, add an edge from the dependency
            % to the app
            digraph:add_edge(Graph, D, A)
        end, Deps)
    end, Apps),

    State1 = validate_apps(State0, Apps, Graph),

    DependsOnGrisp = digraph_utils:reachable([grisp], Graph),
    GrispRelevant = digraph_utils:subgraph(Graph, DependsOnGrisp),
    Sorted = digraph_utils:topsort(GrispRelevant),
    digraph:delete(Graph),
    State1#{sorted_apps => Sorted}.

validate_apps(State, Apps, Graph) ->
    lists:foldl(fun({A, #{dir := Dir}}, S) ->
        case {A, digraph:get_path(Graph, grisp, A)} of
            {A, false} when A =/= grisp ->
                case filelib:is_dir(filename:join(Dir, "grisp")) of
                    true -> event(S, {grisp_dir_without_dep, A});
                    _ -> S
                end;
            _Else ->
                S
        end
    end, State, Apps).

get_erlang_versions(State0) ->
    {Output, State1} = shell(State0,
        "git ls-remote --tags --refs https://github.com/erlang/otp"
    ),
    State1#{erlang_versions => parse_versions(Output)}.

parse_versions(Output) ->
    {match, RawVersions} = re:run(
        Output,
        "
            [^\\s]*\t # Git hash and tab character

            refs/tags/OTP- # OTP Git tag prefix

            " ?version_re "

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

find_erlang_version(#{otp_version := SVersion, erlang_versions := Versions} = State0) ->
    {match, [RawVersion]} = re:run(SVersion, ?version_re, [extended, global, notempty, {capture, all_names, binary}]),
    {Version, Pre, Build, Full} = parse_version(RawVersion),
    case find_version1({lists:zip(lists:seq(1, length(Version)), Version), Pre, Build, Full}, Versions) of
        {error, not_found} ->
            event(State0, {otp_version_not_found, SVersion});
        {ok, {_Version, _Pre, _Build, Found}} ->
            State1 = event(State0, {selected_otp_version, Found, SVersion}),
            State1#{otp_verson => Found}
    end.

find_version1({[], _, _, _}, []) ->
    {error, not_found};
find_version1({[], _, _, _}, Versions) ->
    {ok, hd(lists:reverse(lists:sort(Versions)))};
find_version1({[{N, V}|Version], Pre, Build, Full}, Versions) ->
    find_version1(
        {Version, Pre, Build, Full},
        lists:filter(fun
            ({Ver, P, B, _F}) when P =:= Pre, B =:= Build ->
                length(Ver) >= N andalso lists:nth(N, Ver) == V;
            (_) ->
                false
        end, Versions)
    ).

event(State0, Event) ->
    {_Result, State1} = exec(event, State0, [Event]),
    State1.

shell(State, Script) -> exec(shell, State, [Script]).

exec(Handler, #{handlers := Handlers} = State, Args) ->
    {Result, NewHandlers} = grisp_tools_handler:run(Handler, Args, Handlers),
    {Result, State#{handlers => NewHandlers}}.
