-module(grisp_tools_rust_tests).
-include_lib("eunit/include/eunit.hrl").

platform_overlay_test() ->
    Root = filename:join("/tmp", "grisp-platform-" ++ integer_to_list(
        erlang:unique_integer([positive]))),
    MakeConfig = fun(Platform, Target) ->
        File = filename:join([Root, "grisp", atom_to_list(Platform), "common", "grisp.config"]),
        ok = filelib:ensure_dir(File),
        ok = file:write_file(File, io_lib:format("~p.~n", [
            #{build => #{rust => #{supported => true,
                cargo_opts => [{target, Target}], nm => "target-nm"}}}]))
    end,
    try
        MakeConfig(platform_a, "first-target"),
        MakeConfig(platform_b, "second-target"),
        %% OTP-version folders must not affect platform/common compiler defaults.
        VersionConfig = filename:join([Root, "grisp", "platform_a", "29.1.2", "grisp.config"]),
        ok = filelib:ensure_dir(VersionConfig),
        ok = file:write_file(VersionConfig, <<"#{build => #{rust => #{supported => false}}}.\n">>),
        Apps = [{platform_owner, #{dir => Root}}],
        #{supported := true, cargo_opts := [{target, "first-target"}]} = grisp_tools_rust:config(Apps, platform_a),
        #{cargo_opts := [{target, "second-target"}]} = grisp_tools_rust:config(Apps, platform_b),
        ?assertError({rust_not_supported, unknown_platform}, grisp_tools_rust:config(Apps, unknown_platform))
    after
        file:del_dir_r(Root)
    end.

workspace_manifest_protocol_test() ->
    Root = filename:join("/tmp", "rust workspace " ++ integer_to_list(
        erlang:unique_integer([positive]))),
    Directory = filename:join(Root, "bundle"),
    Bundler = filename:join(Root, "bundler"),
    try
        ok = filelib:ensure_dir(Bundler),
        ok = file:write_file(Bundler,
            <<"#!/usr/bin/env python3\nimport json, pathlib, sys\n",
              "spec=json.loads(pathlib.Path(sys.argv[-1]).read_text())\n",
              "assert isinstance(spec['workspace_manifest'], str)\n",
              "assert pathlib.Path(spec['workspace_manifest']).is_absolute()\n",
              "print(json.dumps({'reason':'rustler-bundle','schema':1,'entries':[],'archive':None}))\n">>),
        ok = file:change_mode(Bundler, 8#755),
        Cargo = cargo_opts:new(#{path => Root}),
        State = #{rust => #{app => owner, directory => Directory,
            workspace_manifest => filename:join(Root, "Cargo.toml"),
            sdk => #{cargo => [<<"cargo">>], identity => <<"test">>},
            target_dir => undefined, nm => "unused", bundler => Bundler,
            inputs => [#{app => owner, load_app => owner, cargo => Cargo}]},
            event_stack => [rust], handlers => grisp_tools:handlers_init(#{
                event => {fun(_, S) -> {ok, S} end, []}})},
        Prepared = grisp_tools_rust:prepare(State),
        ?assertEqual([], mapz:deep_get([build, native_nifs], Prepared))
    after file:del_dir_r(Root) end.
