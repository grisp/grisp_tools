-module(grisp_tools_rust).

-export([config/2, defaults/0, check_cargo/0, prepare/1]).

-define(INPUT_SCHEMA, 4).

-type input() :: #{app := atom(), load_app := atom(), cargo := cargo_opts:t()}.
-type settings() :: #{app := atom(), directory := file:filename_all(),
                      target_dir := binary() | undefined, nm := file:filename_all(),
                      bundler := file:filename_all(), inputs := [input()],
                      sdk := grisp_tools_rust_sdk:t(),
                      workspace_manifest => file:filename_all() | undefined}.
-export_type([input/0]).

%% Resolve the same platform/common hierarchy for every package manager.
config(Apps, Platform) ->
    Config = lists:foldl(fun(P, Acc0) ->
        lists:foldl(fun({App, #{dir := Dir}}, Acc) ->
            Overlay = grisp_tools_util:build_config(App, Dir, P, [<<"common">>]),
            grisp_tools_util:merge_build_config(Acc, Overlay)
        end, Acc0, Apps)
    end, #{}, [default, Platform]),
    Rust = maps:get(rust, Config, #{}),
    case maps:get(supported, Rust, false) of
        true -> Rust;
        false -> error({rust_not_supported, Platform})
    end.

defaults() ->
    #{library_types => [<<"cdylib">>, <<"staticlib">>],
      release => true}.

%% Cargo is supplied by the frontend only when Rust inputs need preparing.
-spec check_cargo() -> ok.
check_cargo() ->
    Required = [
        {cargo_opts, [{new, 1}, {path, 1}, {path, 2}, {release, 1}, {release, 2}, {target, 1},
                      {target_dir, 1}, {target_dir, 2}, {env, 1}, {rustflags, 1}, {features, 1},
                      {default_features, 1}, {library_types, 1}]},
        {cargo_cmd, [{run_executable, 3}]},
        {cargo_util, [{ensure_binary, 1}]}
    ],
    Missing = lists:flatmap(fun({Module, Functions}) ->
        _ = code:ensure_loaded(Module),
        [{Module, Function, Arity} || {Function, Arity} <- Functions,
            not erlang:function_exported(Module, Function, Arity)]
    end, Required),
    case Missing of
        [] -> ok;
        _ -> error({rust_cargo_unsupported, Missing})
    end.

%% The frontend supplies resolved inputs and output paths. The ordinary
%% grisp_tools event handler remains stateful; native archives are returned in state.
-spec prepare(#{rust := settings(), handlers := map(), event_stack => [term()]}) -> map().
prepare(#{rust := #{inputs := []}} = State) -> finish(State, []);
prepare(#{rust := #{app := App, directory := Directory, target_dir := TargetDir,
                   bundler := Bundler, nm := Nm, inputs := [#{cargo := Base} | _] = Inputs}} = State) ->
    ok = check_cargo(),
    validate(Inputs, Base),
    Indexed = lists:zip(lists:seq(1, length(Inputs)), Inputs),
    Rust = maps:get(rust, State),
    SDK = maps:get(sdk, Rust),
    ok = grisp_tools_rust_sdk:configure_environment(SDK, cargo_opts:env(Base)),
    Cargo = maps:get(cargo, SDK),
    Spec = maps:filter(fun(_Key, Value) -> Value =/= undefined end, #{
        schema => 1, out_dir => cargo_util:ensure_binary(filename:absname(Directory)),
        cargo => Cargo, release => cargo_opts:release(Base), target => cargo_opts:target(Base),
        target_dir => TargetDir,
        workspace_manifest => case maps:get(workspace_manifest, Rust, undefined) of
            undefined -> undefined;
            Manifest -> cargo_util:ensure_binary(filename:absname(Manifest))
        end,
        inputs => [bundle_input(N, I) || {N, I} <- Indexed]}),
    SpecPath = filename:absname(filename:join(Directory, "bundle.json")),
    write_changed(SpecPath, [jsx:encode(json_object(Spec), [space, {indent, 2}]), "\n"]),
    State1 = grisp_tools_util:event(State, [{build, App}]),
    {Output, State2} = case cached_result(Directory, Base, SDK, Spec) of
        none -> run_bundler(State1, Base, Bundler, SpecPath);
        Cached -> {Cached, State1}
    end,
    [Result] = [R || #{<<"reason">> := <<"rustler-bundle">>} = R <- Output],
    #{<<"schema">> := 1, <<"entries">> := Entries, <<"archive">> := Archive} = Result,
    case {Entries, Archive} of
        {[], null} -> finish(State2, []);
        _ ->
            Native = grisp_tools_native:inspect(Archive, Nm),
            Expected = lists:sort([maps:get(<<"library">>, E) || E <- Entries]),
            Actual = case Native of undefined -> []; _ -> maps:get(symbols, Native) end,
            case Actual == Expected of
                true -> ok;
                false -> error({rust_bundle_entries_mismatch, Expected, Actual})
            end,
            Owners = maps:from_list([{integer_to_binary(N), maps:get(load_app, I)} || {N, I} <- Indexed]),
            LoadApps = maps:from_list([{maps:get(<<"library">>, E), maps:get(maps:get(<<"input">>, E), Owners)}
                                      || E <- Entries]),
            InputsHash = input_identity(Directory, Base, Inputs, SDK, Spec,
                maps:get(<<"workspace_inputs">>, Result)),
            Cache = #{result => Result, archive_hash => file_hash(Archive)},
            write_changed(cache_path(Directory), [jsx:encode(json_object(Cache), [space, {indent, 2}]), "\n"]),
            finish(State2, [Native#{app => App, load_app => App, load_apps => LoadApps,
                                    inputs_hash => InputsHash,
                                    name => maps:get(<<"library">>, Result)}])
    end.

cache_path(Directory) -> filename:join(filename:dirname(Directory), "bundle-result.json").

cached_result(Directory, Base, #{identity := Identity}, Spec) ->
    try
        {ok, Previous} = file:read_file(filename:join(filename:dirname(Directory), "build-inputs.json")),
        #{<<"schema">> := ?INPUT_SCHEMA} = Data = jsx:decode(Previous, [return_maps]),
        Roots = maps:get(<<"roots">>, Data),
        Current = Data#{<<"files">> := source_index(Roots), <<"sdk">> := Identity,
                        <<"spec">> := json_term(Spec), <<"rustflags">> := json_term(cargo_opts:rustflags(Base)),
                        <<"configured_environment">> := configured_environment_hash(Base),
                        <<"environment">> := environment_hash(Base)},
        true = Data =:= Current,
        {ok, Cache} = file:read_file(cache_path(Directory)),
        #{<<"result">> := #{<<"archive">> := Archive} = Result, <<"archive_hash">> := Hash} =
            jsx:decode(Cache, [return_maps]),
        Hash = file_hash(Archive),
        [Result]
    catch error:_ -> none end.

json_term(Data) -> jsx:decode(jsx:encode(json_object(Data)), [return_maps]).

environment_hash(Base) ->
    Inherited = [begin
        {Name, [$= | Value]} = lists:splitwith(fun(C) -> C =/= $= end, Entry),
        {Name, Value}
    end || Entry <- os:getenv()],
    Effective = lists:ukeysort(1, cargo_opts:env(Base) ++ Inherited),
    Inputs = [{Name, Value} || {Name, Value} <- Effective, Value =/= false,
        not lists:member(Name, ["_", "PWD", "OLDPWD", "SHLVL"])],
    binary:encode_hex(crypto:hash(sha256, term_to_binary(Inputs)), lowercase).

configured_environment_hash(Base) ->
    %% Ambient environment changes must recheck the Rust build, but need not
    %% invalidate installed OTP when the resulting archive is identical.
    Env = lists:keydelete("PATH", 1, cargo_opts:env(Base)),
    binary:encode_hex(crypto:hash(sha256, term_to_binary(Env)), lowercase).

source_index(Roots) ->
    Files = lists:usort(lists:flatmap(fun source_files/1, Roots)),
    maps:from_list([{cargo_util:ensure_binary(File), file_hash(File)} || File <- Files]).

%% Include SDK and local source inputs in the established native package hash.
input_identity(Directory, Base, Inputs, SDK, Spec, WorkspaceInputs) ->
    Roots = lists:usort(input_roots(Directory, Base, Inputs, SDK) ++ config_roots(Directory) ++
                        [unicode:characters_to_list(P) || P <- WorkspaceInputs]),
    Data = #{schema => ?INPUT_SCHEMA, sdk => maps:get(identity, SDK), rustflags => cargo_opts:rustflags(Base),
             spec => Spec, roots => [cargo_util:ensure_binary(R) || R <- Roots],
             files => source_index(Roots), environment => environment_hash(Base),
             configured_environment => configured_environment_hash(Base)},
    Encoded = jsx:encode(json_object(Data), [space, {indent, 2}]),
    write_changed(filename:join(filename:dirname(Directory), "build-inputs.json"), [Encoded, "\n"]),
    PackageInputs = jsx:encode(json_object(maps:remove(environment, Data))),
    binary:encode_hex(crypto:hash(sha256, PackageInputs), lowercase).

%% Atom insertion order differs between provider VMs. Sort JSON object keys so
%% identical native inputs retain their identity across build and deploy.
json_object(Map) when is_map(Map), map_size(Map) > 0 ->
    lists:sort([{cargo_util:ensure_binary(K), json_object(V)} || {K, V} <- maps:to_list(Map)]);
json_object(List) when is_list(List) -> [json_object(V) || V <- List];
json_object(Value) -> Value.

input_roots(Directory, Base, Inputs, #{metadata_cargo := Executable, cargo := [_ | Flags], root := Root}) ->
    Args = Flags ++ ["metadata", "--offline", "--locked", "--format-version=1",
                     "--filter-platform", cargo_opts:target(Base),
                     "--manifest-path", filename:join(Directory, "Cargo.toml")],
    [#{<<"packages">> := Packages}] = cargo_cmd:run_executable(cargo_opts:path(Base, Directory), Executable, Args),
    Own = [cargo_opts:path(C) || #{cargo := C} <- Inputs],
    Own ++ [filename:dirname(unicode:characters_to_list(Manifest))
        || #{<<"source">> := null, <<"manifest_path">> := Manifest} <- Packages,
           not lists:prefix(Root ++ "/", unicode:characters_to_list(Manifest))].

config_roots(Directory) ->
    Configs = [filename:join([Directory, ".cargo", Name]) || Name <- ["config", "config.toml"]],
    case filename:dirname(Directory) of
        Directory -> Configs;
        Parent -> Configs ++ config_roots(Parent)
    end.

source_files(Directory) ->
    case filelib:is_regular(Directory) of
        true -> [Directory];
        false -> source_directory(Directory)
    end.

source_directory(Directory) ->
    case file:list_dir(Directory) of
        {ok, Names} -> lists:flatmap(fun(Name) ->
            Path = filename:join(Directory, Name),
            case grisp_tools_util:is_build_output(Directory, Name) of
                true -> [];
                false -> case filelib:is_dir(Path) of
                    true -> source_files(Path);
                    false -> [Path]
                end
            end
        end, lists:sort(Names));
        {error, enoent} -> [];
        {error, Reason} -> error({rust_input_unreadable, Directory, Reason})
    end.

file_hash(Path) ->
    {ok, Data} = file:read_file(Path),
    binary:encode_hex(crypto:hash(sha256, Data), lowercase).

run_bundler(State, Cargo, Bundler, SpecPath) ->
    try cargo_cmd:run_executable(Cargo, Bundler, ["build", "--spec", SpecPath]) of
        Output -> {Output, diagnostics(State, Output)}
    catch
        error:{cargo_error, {Code, Output}} ->
            _ = diagnostics(State, Output),
            error({rust_bundler_failed, Bundler, Code, Output})
    end.

diagnostics(State, Output) ->
    case [Message || #{<<"reason">> := <<"compiler-message">>} = Message <- Output] of
        [] -> State;
        Messages -> grisp_tools_util:event(State, [{diagnostics, Messages}])
    end.

bundle_input(Number, #{cargo := Cargo}) ->
    #{id => integer_to_binary(Number),
      manifest_path => cargo_util:ensure_binary(filename:absname(
          filename:join(cargo_opts:path(Cargo), "Cargo.toml"))),
      features => cargo_opts:features(Cargo), default_features => cargo_opts:default_features(Cargo),
      library_types => cargo_opts:library_types(Cargo)}.

%% One bundle has one compiler invocation context. Package checks belong to Rustler.
validate(Inputs, Base) ->
    lists:foreach(fun(#{cargo := Cargo}) ->
        lists:foreach(fun(Setting) ->
            case apply(cargo_opts, Setting, [Cargo]) == apply(cargo_opts, Setting, [Base]) of
                true -> ok;
                false -> error({rust_settings_mismatch, Setting})
            end
        end, [target, toolchain, env, rustflags])
    end, Inputs).

finish(State, Nifs) ->
    State1 = lists:foldl(fun(Nif, S) -> grisp_tools_util:event(S, [{archive, Nif}]) end, State, Nifs),
    mapz:deep_put([build, native_nifs], Nifs, State1).

write_changed(Path, Contents) ->
    Data = iolist_to_binary(Contents),
    case file:read_file(Path) of
        {ok, Data} -> ok;
        _ ->
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, Data)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

input_cache_test() ->
    Root = filename:join("/tmp", "rust-input-cache-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Directory = filename:join(Root, "_grisp/board/rust/29.1.2/bundle"),
    Source = filename:join(Root, "native/src/lib.rs"),
    Archive = filename:join(Root, "_grisp/board/rust/29.1.2/target/bundle.a"),
    Config = filename:join(Root, ".cargo/config.toml"),
    Variable = "GRISP_SDK_CACHE_TEST_INPUT",
    Previous = os:getenv(Variable),
    try
        ok = filelib:ensure_dir(Source), ok = file:write_file(Source, <<"source">>),
        ok = filelib:ensure_dir(Archive), ok = file:write_file(Archive, <<"archive">>),
        ok = filelib:ensure_dir(filename:join(Directory, "Cargo.toml")),
        Metadata = filename:join(Root, "metadata"),
        ok = file:write_file(Metadata, <<"#!/bin/sh\nprintf '%s\\n' '{\"packages\":[]}'\n">>),
        ok = file:change_mode(Metadata, 8#755),
        Cargo = cargo_opts:new(#{path => filename:join(Root, "native"), target => <<"target">>}),
        SDK = #{identity => <<"SDK-A">>, metadata_cargo => Metadata,
                cargo => [Metadata], root => filename:join(Root, "sdk")},
        Spec = #{schema => 1, out_dir => list_to_binary(Directory)},
        WorkspaceInputs = [list_to_binary(filename:join(Root, "Cargo.lock"))],
        InputHash = input_identity(Directory, Cargo, [#{cargo => Cargo}], SDK, Spec, WorkspaceInputs),
        Result = #{reason => <<"rustler-bundle">>, archive => list_to_binary(Archive)},
        ok = file:write_file(cache_path(Directory), jsx:encode(#{result => Result, archive_hash => file_hash(Archive)})),
        ?assertEqual([json_term(Result)], cached_result(Directory, Cargo, SDK, Spec)),
        Lock = filename:join(Root, "Cargo.lock"),
        ok = file:write_file(Lock, <<"workspace lock">>),
        ?assertEqual(none, cached_result(Directory, Cargo, SDK, Spec)),
        ok = file:delete(Lock),
        ?assertEqual([json_term(Result)], cached_result(Directory, Cargo, SDK, Spec)),
        Extra = filename:join(Root, "native/include.txt"),
        ok = file:write_file(Extra, <<"new build input">>),
        ?assertEqual(none, cached_result(Directory, Cargo, SDK, Spec)),
        ok = file:delete(Extra),
        Manifest = filename:join(Root, "native/Cargo.toml"),
        ok = file:write_file(Manifest, <<"[package]\nname='probe'\n">>),
        Nested = filename:join(Root, "native/src/target/mod.rs"),
        ok = filelib:ensure_dir(Nested), ok = file:write_file(Nested, <<"source A">>),
        Before = source_index([filename:join(Root, "native")]),
        ok = file:write_file(Nested, <<"source B">>),
        ?assertNotEqual(Before, source_index([filename:join(Root, "native")])),
        Generated = filename:join(Root, "native/target/debug/generated"),
        CacheTag = filename:join(Root, "native/target/CACHEDIR.TAG"),
        ok = filelib:ensure_dir(CacheTag),
        ok = file:write_file(CacheTag, <<"Signature: 8a477f597d28d172789f06886806bc55\n">>),
        AfterSource = source_index([filename:join(Root, "native")]),
        ok = filelib:ensure_dir(Generated), ok = file:write_file(Generated, <<"output">>),
        ?assertEqual(AfterSource, source_index([filename:join(Root, "native")])),
        ok = file:delete(Nested), ok = file:delete(Manifest),
        ok = file:delete(Generated),
        ok = file:delete(CacheTag),
        ok = filelib:ensure_dir(Config), ok = file:write_file(Config, <<"[profile.release]\nopt-level=1\n">>),
        ?assertEqual(none, cached_result(Directory, Cargo, SDK, Spec)),
        ok = file:delete(Config),
        ?assertEqual([json_term(Result)], cached_result(Directory, Cargo, SDK, Spec)),
        true = os:putenv(Variable, "changed"),
        ?assertEqual(none, cached_result(Directory, Cargo, SDK, Spec)),
        ?assertEqual(InputHash, input_identity(Directory, Cargo, [#{cargo => Cargo}], SDK, Spec, WorkspaceInputs)),
        case Previous of false -> os:unsetenv(Variable); _ -> os:putenv(Variable, Previous) end,
        ?assertEqual(none, cached_result(Directory, Cargo, SDK#{identity := <<"SDK-B">>}, Spec)),
        ok = file:write_file(Archive, <<"modified archive">>),
        ?assertEqual(none, cached_result(Directory, Cargo, SDK, Spec))
    after
        case Previous of false -> os:unsetenv(Variable); _ -> os:putenv(Variable, Previous) end,
        file:del_dir_r(Root)
    end.

bundle_rejections_test() ->
    Base = cargo_opts:new(#{path => "."}),
    lists:foreach(fun({Setting, Value}) ->
        Other = cargo_opts:new(#{path => ".", Setting => Value}),
        ?assertError({rust_settings_mismatch, Setting}, validate([#{cargo => Other}], Base))
    end, [{target, <<"other-target">>}, {toolchain, <<"other-toolchain">>},
          {env, [{"CUSTOM", "value"}]}, {rustflags, [<<"-Cpanic=abort">>]}]).

diagnostics_event_test() ->
    Message = #{<<"reason">> => <<"compiler-message">>,
                <<"message">> => #{<<"rendered">> => <<"marker">>}},
    State = #{event_stack => [rust], handlers => grisp_tools:handlers_init(#{
        event => {fun(Event, Events) -> {ok, [Event | Events]} end, []}})},
    ?assertEqual(State, diagnostics(State, [#{<<"reason">> => <<"build-finished">>}])),
    Updated = diagnostics(State, [Message, #{<<"reason">> => <<"compiler-artifact">>}]),
    ?assertEqual(#{event => [[rust, {diagnostics, [Message]}]]},
                 grisp_tools:handlers_finalize(Updated)).

bundler_failure_diagnostics_test() ->
    case os:type() of
        {win32, nt} -> ok;
        _ ->
            Path = filename:join("/tmp", "bundler-failure-" ++ integer_to_list(
                erlang:unique_integer([positive]))),
            Message = #{<<"reason">> => <<"compiler-message">>,
                        <<"message">> => #{<<"rendered">> => <<"failure marker">>}},
            Script = ["#!/bin/sh\nprintf '%s\\n' '", jsx:encode(Message), "'\nexit 23\n"],
            ok = file:write_file(Path, Script),
            ok = file:change_mode(Path, 8#755),
            State = #{event_stack => [rust], handlers => grisp_tools:handlers_init(#{
                event => {fun(Event, Events) -> self() ! Event, {ok, Events} end, []}})},
            try
                ?assertError({rust_bundler_failed, Path, 23, [Message]},
                    run_bundler(State, cargo_opts:new(#{path => "/tmp"}), Path, "unused")),
                receive
                    [rust, {diagnostics, [Message]}] -> ok
                after 1000 -> error(missing_diagnostic_event)
                end
            after file:delete(Path) end
    end.
-endif.
