-module(grisp_tools_rust_sdk).

-export([resolve/4, configure_environment/2]).
-export_type([t/0]).

-type t() :: #{cargo := [binary()], env := [{string(), string() | false}],
               bundler := file:filename_all(), nm := file:filename_all(),
               target_dir := file:filename_all(), root := file:filename_all(),
               metadata_cargo := file:filename_all(), identity := binary(),
               environment_file := file:filename_all() | undefined, rustler := [binary()]}.

%% The platform supplies ABI policy. The selected SDK supplies tools and sources.
resolve(Toolchain, Project0, Directory0, Platform) ->
    ok = grisp_tools_rust:check_cargo(),
    Project = filename:absname(unicode:characters_to_list(Project0)),
    Directory = filename:absname(unicode:characters_to_list(Directory0)),
    try do_resolve(Toolchain, Project, Directory, Platform)
    catch error:{badkey, Key} -> error({rust_sdk_invalid, {missing_field, Key}}) end.

do_resolve(Toolchain, Project, Directory, Platform) ->
    Root = root(Toolchain),
    Core = metadata(Toolchain, Root, "share/rtems/rust-support.json"),
    Nif = metadata(Toolchain, Root, "share/rust-nif/support.json"),
    Target = cargo_util:ensure_binary(proplists:get_value(target, maps:get(cargo_opts, Platform))),
    validate(Core, Nif, Target, maps:get(sdk, Platform)),
    Config = sdk_path(Root, maps:get(<<"cargo_config">>, Nif)),
    NmName = unicode:characters_to_list(maps:get(nm, Platform)),
    CTarget = unicode:characters_to_list(maps:get(c_target, maps:get(sdk, Platform))),
    Programs = ["cargo", "rustc", "rustler-bundler", NmName,
                CTarget ++ "-gcc", CTarget ++ "-g++", CTarget ++ "-ar"],
    lists:foreach(fun(Name) ->
        Path = filename:join([Root, "bin", Name]),
        case program_exists(Toolchain, Path) of
            true -> ok;
            false -> error({rust_sdk_missing_file, Path})
        end
    end, Programs),
    Std = filename:join([Root, "lib/rustlib", binary_to_list(Target), "lib"]),
    Libraries = ["core", "alloc", "std"] ++ case maps:get(panic, maps:get(sdk, Platform)) of
        <<"unwind">> -> ["panic_unwind"];
        _ -> []
    end,
    lists:foreach(fun(Name) ->
        Pattern = filename:join(Std, "lib" ++ Name ++ "-*.rlib"),
        case library_exists(Toolchain, Std, Name, Pattern) of
            true -> ok;
            false -> error({rust_sdk_missing_file, Pattern})
        end
    end, Libraries),
    case exists(Toolchain, Config) of
        true -> ok;
        false -> error({rust_sdk_missing_file, Config})
    end,
    Home = filename:join(Directory, "cargo-home"),
    TargetDir = filename:join(Directory, "target"),
    Env = environment(Toolchain, Root, Home, TargetDir) ++ native_environment(Root, Target, CTarget, Platform),
    EnvFile = case Toolchain of
        {docker, _} -> filename:join(Directory, "docker-env.list");
        _ -> undefined
    end,
    configure_environment(#{environment_file => EnvFile}, Env),
    Bundler = executable(Toolchain, Project, Directory, Root, "rustler-bundler"),
    Nm = executable(Toolchain, Project, Directory, Root, NmName),
    MetadataCargo = executable(Toolchain, Project, Directory, Root, "cargo"),
    %% Build scripts can invoke Cargo themselves, without inheriting CLI flags.
    %% Give those invocations the same SDK patches through the private Cargo home.
    CargoConfig = absolute_paths(read(Toolchain, Config), filename:dirname(filename:dirname(Config))),
    HomeConfig = filename:join(Home, "config.toml"),
    ok = filelib:ensure_dir(HomeConfig),
    case file:read_file(HomeConfig) of
        {ok, CargoConfig} -> ok;
        _ -> ok = file:write_file(HomeConfig, CargoConfig)
    end,
    Revision = read(Toolchain, filename:join(Root, "GRISP_TOOLCHAIN_REVISION")),
    Identity = crypto:hash(sha256, [jsx:encode(Core), jsx:encode(Nif), Revision]),
    EncodedIdentity = binary:encode_hex(Identity, lowercase),
    reset_target(Project, Directory, TargetDir, EncodedIdentity),
    #{cargo => [cargo_util:ensure_binary(filename:join([Root, "bin", "cargo"])),
                <<"--config">>, cargo_util:ensure_binary(Config)],
      env => Env, bundler => Bundler, nm => Nm, target_dir => TargetDir,
      root => Root, metadata_cargo => MetadataCargo,
      environment_file => EnvFile,
      identity => EncodedIdentity,
      rustler => maps:get(<<"rustler">>, Nif)}.

%% Only variable names are persisted; values remain in the invocation environment.
configure_environment(#{environment_file := Path}, Env) when Path =/= undefined ->
    Names = lists:usort([Name || {Name, _} <- Env] ++
        ["CARGO_BUILD_JOBS", "CARGO_NET_OFFLINE", "HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY"]),
    Data = iolist_to_binary([[Name, "\n"] || Name <- Names]),
    ok = filelib:ensure_dir(Path),
    case file:read_file(Path) of
        {ok, Data} -> ok;
        _ -> ok = file:write_file(Path, Data)
    end;
configure_environment(_, _) -> ok.

native_environment(Root, Target, CTarget, Platform) ->
    Key = binary_to_list(binary:replace(Target, <<"-">>, <<"_">>, [global])),
    Compiler = filename:join([Root, "bin", CTarget ++ "-gcc"]),
    Include = case maps:find(bsp, Platform) of
        {ok, BSP} -> ["-I" ++ filename:join([Root, CTarget, BSP, "lib/include"])];
        error -> []
    end,
    Flags = maps:get(cflags, Platform, []) ++ Include,
    CFlags = lists:flatten(lists:join(" ", [quote_flag(F) || F <- Flags])),
    [{"CC_" ++ Key, Compiler},
     {"CXX_" ++ Key, filename:join([Root, "bin", CTarget ++ "-g++"])},
     {"AR_" ++ Key, filename:join([Root, "bin", CTarget ++ "-ar"])},
     {"CFLAGS_" ++ Key, CFlags}, {"CXXFLAGS_" ++ Key, CFlags},
     {"CC_SHELL_ESCAPED_FLAGS", "1"},
     {"CARGO_TARGET_" ++ string:uppercase(Key) ++ "_LINKER", Compiler}].

quote_flag(Flag) ->
    case lists:any(fun(C) -> lists:member(C, " \t'\"\\") end, Flag) of
        true -> grisp_tools_util:shell_quote(Flag);
        false -> Flag
    end.

%% Cargo cannot detect every change to std rebuilt at the same Rust revision.
%% Invalidate only this project's target cache when the SDK identity changes.
reset_target(Project, Directory, TargetDir, Identity) ->
    case lists:prefix(filename:join(Project, "_grisp") ++ "/", Directory) of
        true -> ok;
        false -> error({rust_sdk_invalid, {workspace, Directory}})
    end,
    Marker = filename:join(Directory, "sdk-identity"),
    case file:read_file(Marker) of
        {ok, Identity} -> ok;
        _ ->
            case file:read_link(TargetDir) of
                {ok, _} -> error({rust_sdk_invalid, {symlink, TargetDir}});
                _ -> case file:del_dir_r(TargetDir) of
                    ok -> ok;
                    {error, enoent} -> ok;
                    {error, Reason} -> error({rust_sdk_invalid, {cache, TargetDir, Reason}})
                end
            end,
            ok = file:write_file(Marker, Identity)
    end.

%% SDK-generated path entries are ordinary quoted relative paths. Rebase only
%% those entries when deriving the Cargo-home configuration; keep SDK files intact.
absolute_paths(Config, Root) ->
    case re:run(Config, <<"path\\s*=\\s*\"([^\"]*)\"">>, [{capture, [0, 1], index}]) of
        nomatch -> Config;
        {match, [{Start, Length}, {PStart, PLength}]} ->
            Relative = binary:part(Config, PStart, PLength),
            Prefix = binary:part(Config, 0, Start),
            Rest = binary:part(Config, Start + Length, byte_size(Config) - Start - Length),
            Path = cargo_util:ensure_binary(sdk_path(Root, Relative)),
            iolist_to_binary([Prefix, "path = ", jsx:encode(Path), absolute_paths(Rest, Root)])
    end.

root({directory, Root}) -> filename:absname(unicode:characters_to_list(Root));
root({docker, Image}) ->
    %% The port merges stderr into stdout for diagnostics. Keep pull progress out of the SDK path.
    Root = string:trim(binary_to_list(command("docker", ["run", "--rm", "--quiet", Image,
        "sh", "-c", "printf '%s' \"$GRISP_TOOLCHAIN\""]))),
    case filename:pathtype(Root) of
        absolute -> Root;
        _ -> error({rust_sdk_invalid, missing_docker_sdk_root})
    end;
root(_) -> error(rust_sdk_not_selected).

metadata(Toolchain, Root, Relative) ->
    Path = filename:join(Root, Relative),
    try jsx:decode(read(Toolchain, Path), [return_maps]) of
        #{<<"schema">> := 1} = Data -> Data;
        _ -> error({rust_sdk_invalid, Path})
    catch
        error:{rust_sdk_read_failed, _, _} -> error({rust_sdk_missing_support, Path});
        error:badarg -> error({rust_sdk_invalid, Path})
    end.

validate(#{<<"identity">> := Identity, <<"targets">> := Targets},
         #{<<"identity">> := Identity, <<"rustler">> := [_ | _]}, Target, Expected) ->
    case maps:find(Target, Targets) of
        error -> error({rust_sdk_target_missing, Target});
        {ok, ABI} ->
            maps:foreach(fun(Key, Value) ->
                Actual = maps:get(atom_to_binary(Key), ABI, undefined),
                case Actual =:= Value of
                    true -> ok;
                    false -> error({rust_sdk_abi_mismatch, Key, Value, Actual})
                end
            end, maps:remove(std_variant, Expected)),
            Variant = maps:get(std_variant, Expected),
            case lists:member(Variant, maps:get(<<"std_variants">>, ABI, [])) of
                true -> ok;
                false -> error({rust_sdk_abi_mismatch, std_variant, Variant, ABI})
            end
    end;
validate(_, _, _, _) -> error({rust_sdk_invalid, mismatched_capabilities}).

sdk_path(Root, Relative0) ->
    Relative = unicode:characters_to_list(Relative0),
    case filename:pathtype(Relative) =:= relative andalso
         not lists:member("..", filename:split(Relative)) of
        true -> filename:join(Root, Relative);
        false -> error({rust_sdk_invalid, {path, Relative}})
    end.

environment(Toolchain, Root, Home, TargetDir) ->
    Clear = [{Name, false} || Name <- ["RUSTUP_TOOLCHAIN", "RUSTC_BOOTSTRAP", "RUSTFLAGS",
        "RUSTC_WRAPPER", "RUSTC_WORKSPACE_WRAPPER", "CARGO_ENCODED_RUSTFLAGS",
        "CARGO_BUILD_TARGET", "CARGO_UNSTABLE_BUILD_STD", "CARGO_UNSTABLE_JSON_TARGET_SPEC",
        "RUSTC_STD_RTEMS_NEWLIB_NO_TLS_ERRNO", "GRISP_RUST_BUILD_MANIFEST"]],
    Path = case Toolchain of
        {directory, _} -> [{"PATH", filename:join(Root, "bin") ++ ":" ++ os:getenv("PATH", "")}];
        {docker, _} -> []
    end,
    [{"RUSTC", filename:join([Root, "bin", "rustc"])},
     {"CARGO_HOME", Home}, {"CARGO_TARGET_DIR", TargetDir}] ++ Path ++ Clear.

executable({directory, _}, _Project, _Directory, Root, Name) -> filename:join([Root, "bin", Name]);
executable({docker, Image}, Project, Directory, Root, Name) ->
    Path = filename:join([Directory, "bin", Name]),
    Quote = fun grisp_tools_util:shell_quote/1,
    EnvFile = filename:join(Directory, "docker-env.list"),
    Script = ["#!/bin/sh\nset -- ", Quote(Image), " ", Quote(filename:join([Root, "bin", Name])), " \"$@\"\n",
        "while IFS= read -r name; do set -- --env \"$name\" \"$@\"; done < ", Quote(EnvFile), "\n",
        "exec docker run --rm --user \"$(id -u):$(id -g)\"",
        " --volume ", Quote([Project, ":", Project]), " --workdir \"$PWD\" \"$@\"\n"],
    ok = filelib:ensure_dir(Path),
    Data = iolist_to_binary(Script),
    case file:read_file(Path) of
        {ok, Data} -> ok;
        _ -> ok = file:write_file(Path, Data)
    end,
    ok = file:change_mode(Path, 8#755),
    Path.

exists({directory, _}, Path) -> filelib:is_regular(Path);
exists({docker, Image}, Path) ->
    try command("docker", ["run", "--rm", Image, "test", "-f", Path]) of
        _ -> true
    catch error:{rust_sdk_command_failed, _, _} -> false end.

program_exists({directory, _}, Path) -> os:find_executable(Path) =/= false;
program_exists({docker, Image}, Path) ->
    try command("docker", ["run", "--rm", Image, "test", "-x", Path]) of
        _ -> true
    catch error:{rust_sdk_command_failed, _, _} -> false end.

library_exists({directory, _}, _Std, _Name, Pattern) ->
    lists:any(fun filelib:is_regular/1, filelib:wildcard(Pattern));
library_exists({docker, Image}, Std, Name, _Pattern) ->
    Script = "for p in \"$1\"/lib\"$2\"-*.rlib; do test -f \"$p\" && exit 0; done; exit 1",
    try command("docker", ["run", "--rm", Image, "sh", "-c", Script, "sh", Std, Name]) of
        _ -> true
    catch error:{rust_sdk_command_failed, _, _} -> false end.

read({directory, _}, Path) ->
    case file:read_file(Path) of
        {ok, Data} -> Data;
        {error, Reason} -> error({rust_sdk_read_failed, Path, Reason})
    end;
read({docker, Image}, Path) ->
    try command("docker", ["run", "--rm", Image, "cat", Path])
    catch error:{rust_sdk_command_failed, Code, _} -> error({rust_sdk_read_failed, Path, Code}) end.

command(Name, Args) ->
    case os:find_executable(Name) of
        false -> error({rust_sdk_command_failed, Name, not_found});
        Executable ->
            Port = open_port({spawn_executable, Executable},
                [binary, use_stdio, stderr_to_stdout, exit_status, {args, Args}]),
            output(Port, [])
    end.

output(Port, Acc) ->
    receive
        {Port, {data, Data}} -> output(Port, [Data | Acc]);
        {Port, {exit_status, 0}} -> iolist_to_binary(lists:reverse(Acc));
        {Port, {exit_status, Code}} -> error({rust_sdk_command_failed, Code,
                                            iolist_to_binary(lists:reverse(Acc))})
    end.
