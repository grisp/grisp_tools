-module(grisp_tools_native_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

source_input_directories_test() ->
    Root = filename:join("/tmp", "grisp-source-inputs-" ++
                         integer_to_list(erlang:unique_integer([positive]))),
    Write = fun(Path, Data) ->
        ok = filelib:ensure_dir(Path), ok = file:write_file(Path, Data)
    end,
    Read = fun() ->
        {Overlay, _} = grisp_tools_util:build_overlay(owner, Root, grisp2, false,
            {[29, 1, 2], undefined, undefined, <<"29.1.2">>}),
        grisp_tools_util:build_hash(#{build => #{overlay => Overlay}})
    end,
    try
        Write(filename:join(Root, "grisp/grisp2/common/grisp.config"),
            <<"#{build => #{inputs => [\"native\"]}}.\n">>),
        CSource = filename:join(Root, "native/target/driver.c"),
        Write(CSource, <<"int value = 1;">>),
        CFirst = Read(),
        Write(CSource, <<"int value = 2;">>),
        ?assertNotEqual(CFirst, Read()),
        Write(filename:join(Root, "native/Cargo.toml"), <<"[package]\nname='probe'\n">>),
        WithCargo = Read(),
        Write(CSource, <<"source beside a Cargo manifest">>),
        ?assertNotEqual(WithCargo, Read()),
        Write(filename:join(Root, "native/target/CACHEDIR.TAG"),
            <<"Signature: 8a477f597d28d172789f06886806bc55\n">>),
        RustSource = filename:join(Root, "native/src/target/mod.rs"),
        Write(RustSource, <<"pub fn value() -> u8 {1}">>),
        First = Read(),
        Write(CSource, <<"generated Cargo output">>),
        ?assertEqual(First, Read()),
        Write(RustSource, <<"pub fn value() -> u8 {2}">>),
        ?assertNotEqual(First, Read())
    after file:del_dir_r(Root) end.

table_arguments_test() ->
    Root = filename:join("/tmp", "grisp-native-table-" ++
                         integer_to_list(erlang:unique_integer([positive]))),
    Source = filename:join(Root, "input.a"),
    Makefile = filename:join(Root, "erts/emulator/Makefile.in"),
    ok = filelib:ensure_dir(Makefile),
    ok = file:write_file(Source, <<"archive">>),
    ok = file:write_file(Makefile,
        <<"STATIC_DRIVERS=@STATIC_DRIVERS@\n",
          "$(TTF_DIR)/driver_tab.c: Makefile.in utils/make_driver_tab\n">>),
    Nif = #{app => portable_lib, name => <<"bundle">>, source => Source,
            symbols => [<<"first">>, <<"second">>], hash => <<"same-archive">>},
    Other = Nif#{app := other_lib, name := <<"other">>, symbols := [<<"third">>]},
    State = #{build => #{native_nifs => [Nif, Other]}, paths => #{build => Root},
              otp_version => {[29], undefined, undefined, <<"29">>},
              event_stack => [], handlers => #{event => {fun(_, S) -> {ok, S} end, []}}},
    try
        grisp_tools_native:prepare(State),
        Probe = ["colon:=:\nempty:=\nspace:=$(empty) $(empty)\nERL_TOP:=", Root,
                 "\nSTATIC_NIFS:=/existing/c_nif.a\ninclude ", Root, "/grisp-native.mk\n",
                 "STATIC_NIF_LIBS = $(foreach n,$(STATIC_NIFS),$(word 1, $(subst $(colon),$(space),$(n))))\n",
                 "STATIC_NIF_SYMBOLS = $(STATIC_NIFS)\ninspect:\n",
                 "\t$(file >", Root, "/libraries,$(STATIC_NIF_LIBS))\n",
                 "\t$(file >", Root, "/symbols,$(STATIC_NIF_SYMBOLS))\n"],
        ProbeFile = filename:join(Root, "probe.mk"),
        ok = file:write_file(ProbeFile, Probe),
        Port = open_port({spawn_executable, os:find_executable("make")},
                         [{args, ["-s", "-f", ProbeFile, "inspect"]},
                          binary, use_stdio, stderr_to_stdout, exit_status]),
        0 = make_result(Port),
        Archive = list_to_binary(filename:join([Root, "grisp-native", "portable_lib", "bundle.a"])),
        OtherArchive = list_to_binary(filename:join([Root, "grisp-native", "other_lib", "other.a"])),
        {ok, Libraries} = file:read_file(filename:join(Root, "libraries")),
        {ok, Symbols} = file:read_file(filename:join(Root, "symbols")),
        ?assertEqual([<<"/existing/c_nif.a">>, Archive, OtherArchive], words(Libraries)),
        ?assertEqual([<<"/existing/c_nif.a">>, <<Archive/binary, ":first">>,
                      <<Archive/binary, ":second">>, <<OtherArchive/binary, ":third">>], words(Symbols))
    after
        file:del_dir_r(Root)
    end.

make_result(Port) ->
    receive
        {Port, {data, _}} -> make_result(Port);
        {Port, {exit_status, Code}} -> Code
    after 10000 -> error(make_timeout)
    end.

words(Binary) -> binary:split(string:trim(Binary), <<" ">>, [global, trim_all]).

integration_hash_test() ->
    Nif = #{app => owner, name => <<"bundle">>, hash => <<"unchanged">>,
            symbols => [<<"first">>]},
    First = grisp_tools_native:hash_index(#{build => #{native_nifs => [Nif]}}),
    Second = grisp_tools_native:hash_index(#{build => #{native_nifs => [
        Nif#{symbols := [<<"first">>, <<"second">>]}]}}),
    ?assertNotEqual(First, Second),
    WithInputs = grisp_tools_native:hash_index(#{build => #{native_nifs => [Nif#{inputs_hash => <<"SDK-A">>}]}}),
    ChangedInputs = grisp_tools_native:hash_index(#{build => #{native_nifs => [Nif#{inputs_hash => <<"SDK-B">>}]}}),
    ?assertNotEqual(WithInputs, ChangedInputs).

native_integration_test() ->
    Root = filename:join(
        "/tmp", "grisp-native-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Makefile = filename:join(Root, "erts/emulator/Makefile.in"),
    Source = filename:join(Root, "input.a"),
    ok = filelib:ensure_dir(Makefile),
    ok = file:write_file(Source, <<"archive">>),
    Original =
        <<"comma:=,\nERL_TOP:=.\nSTATIC_DRIVERS=@STATIC_DRIVERS@\n",
            "$(TTF_DIR)/driver_tab.c: Makefile.in utils/make_driver_tab\n",
            "ifneq ($(STATIC_NIFS),no)\nSTATIC_NIF_LIBS = $(foreach n,$(STATIC_NIFS),$(word 1, $(subst $(colon),$(space),$(n))))\n",
            "DEPLIBS += $(STATIC_NIF_LIBS)\nendif\n",
            "link:\n\t$(CC) $(STATIC_NIF_LIBS) $(LIBS)\n">>,
    ok = file:write_file(Makefile, Original),
    Nif = #{
        app => portable_lib,
        name => <<"different_name">>,
        source => Source,
        symbols => [<<"another_entry">>],
        hash => <<"first-hash">>
    },
    State = #{
        apps => #{portable_lib => #{native_nifs => [Nif]}},
        sorted_apps => [grisp],
        event_stack => [build, prepare, copy, nif_libraries],
        handlers => #{event => {fun(Event, Events) -> {ok, [Event | Events]} end, []}},
        build => #{},
        paths => #{build => Root, install => Root},
        otp_version => {[29, 1, 2], undefined, undefined, <<"29.1.2">>}
    },
    S1 = grisp_tools_native:collect(State),
    ?assertEqual(<<"first-hash">>, proplists:get_value(
        <<"native/portable_lib/different_name">>, grisp_tools_native:hash_index(S1))),
    ?assert(is_binary(proplists:get_value(<<"native/integration">>,
                                        grisp_tools_native:hash_index(S1)))),
    S2 = grisp_tools_native:prepare(S1),
    {ok, Prepared} = file:read_file(Makefile),
    ?assertNotEqual(nomatch, binary:match(Prepared, <<"-include $(ERL_TOP)/grisp-native.mk">>)),
    ?assertNotEqual(nomatch, binary:match(Prepared, <<"--whole-archive">>)),
    ?assertNotEqual(nomatch, binary:match(Prepared, <<"DEPLIBS += $(STATIC_NIF_LIBS)">>)),
    ?assertNotEqual(nomatch, binary:match(Prepared, <<"\t$(CC) $(GRISP_STATIC_NIF_LINK_LIBS)">>)),
    Fragment = filename:join(Root, "grisp-native.mk"),
    {ok, Content} = file:read_file(Fragment),
    ?assertNotEqual(nomatch, binary:match(Content, <<"different_name.a:another_entry">>)),
    {ok, Info} = file:read_file_info(Fragment),
    OldTime = {{2000, 1, 1}, {0, 0, 0}},
    ok = file:write_file_info(Fragment, Info#file_info{mtime = OldTime}),
    _ = grisp_tools_native:prepare(S2),
    {ok, Prepared} = file:read_file(Makefile),
    %% A generated block must remain at its canonical insertion point.
    Anchor = <<"STATIC_DRIVERS=@STATIC_DRIVERS@">>,
    {HeaderStart, _} = binary:match(Prepared, <<"# GRiSP generated static NIFs">>),
    {AnchorStart, _} = binary:match(Prepared, Anchor),
    Header = binary:part(Prepared, HeaderStart, AnchorStart - HeaderStart),
    WithoutHeader = binary:replace(Prepared, Header, <<>>),
    ok = file:write_file(Makefile, <<Header/binary, WithoutHeader/binary>>),
    ?assertError(native_nifs_unsupported_makefile, grisp_tools_native:prepare(S2)),
    ok = file:write_file(Makefile, Prepared),
    {ok, After} = file:read_file_info(Fragment),
    ?assertEqual(OldTime, After#file_info.mtime),
    %% The existing package listing covers C inputs and language-neutral archives.
    CSource = filename:join(Root, "example.c"),
    CTarget = <<"erts/emulator/nifs/common/example.c">>,
    ok = file:write_file(CSource, <<"int example = 1;">>),
    Overlay = #{nifs => #{CTarget => #{source => CSource, target => CTarget}}},
    WithC = mapz:deep_put([build, overlay], Overlay, S2),
    {_, Index} = grisp_tools_util:build_hash(WithC),
    Checked = mapz:deep_put([build, hash, index], Index, WithC),
    ?assertException(error, {build_inputs_stale, _}, grisp_tools_util:check_build(Checked)),
    ok = file:write_file(filename:join(Root, "GRISP_PACKAGE_FILES"),
                         grisp_tools_util:build_hash_format(Index)),
    ?assertEqual(Checked, grisp_tools_util:check_build(Checked)),
    %% The same archive in a package built before the adapter is stale too.
    OldIndex = lists:keydelete(<<"native/integration">>, 1, Index),
    ok = file:write_file(filename:join(Root, "GRISP_PACKAGE_FILES"),
                         grisp_tools_util:build_hash_format(OldIndex)),
    ?assertException(error, {build_inputs_stale, _}, grisp_tools_util:check_build(Checked)),
    ok = file:write_file(filename:join(Root, "GRISP_PACKAGE_FILES"),
                         grisp_tools_util:build_hash_format(Index)),
    ok = file:write_file(CSource, <<"int example = 2;">>),
    {_, ChangedC} = grisp_tools_util:build_hash(WithC),
    ?assertException(error, {build_inputs_stale, _}, grisp_tools_util:check_build(
        mapz:deep_put([build, hash, index], ChangedC, WithC))),
    ok = file:write_file(CSource, <<"int example = 1;">>),
    lists:foreach(fun(NativeNifs) ->
        Changed = mapz:deep_put([build, native_nifs], NativeNifs, WithC),
        {_, ChangedIndex} = grisp_tools_util:build_hash(Changed),
        ?assertException(error, {build_inputs_stale, _}, grisp_tools_util:check_build(
            mapz:deep_put([build, hash, index], ChangedIndex, Changed)))
    end, [[Nif#{hash := <<"second-hash">>}], [Nif#{symbols := [<<"new_entry">>]}], []]),
    {_, Events} = maps:get(event, maps:get(handlers, S2)),
    ?assertMatch([[build, prepare, copy, nif_libraries,
                  #{app := portable_lib, name := "grisp-native/portable_lib/different_name.a"}]],
                 Events),
    Removed = mapz:deep_put([build, native_nifs], [], S2),
    _ = grisp_tools_native:prepare(Removed),
    {ok, Empty} = file:read_file(Fragment),
    ?assertEqual(nomatch, binary:match(Empty, <<"STATIC_NIFS +=">>)),
    ok = file:delete(Fragment),
    _ = grisp_tools_native:prepare(Removed),
    ?assertEqual({ok, Empty}, file:read_file(Fragment)),
    ok = file:del_dir_r(Root).

release_layouts_test() ->
    Root = filename:join("/tmp", "grisp-shared-release-" ++
                         integer_to_list(erlang:unique_integer([positive]))),
    RebarHost = filename:join([Root, "lib", "rebar_owner-1", "priv", "crates", "example", "example.so"]),
    MixPriv = filename:join([Root, "lib", "mix_owner-1", "priv"]),
    MixHost = filename:join([MixPriv, "native", "example.so"]),
    Unrelated = filename:join([MixPriv, "native", "unrelated.so"]),
    Source = filename:join(Root, "source-priv"),
    SourceHost = filename:join([Source, "native", "example.so"]),
    Nifs = [#{host_artifacts => [
        #{app => rebar_owner, path => "priv/crates/example"},
        #{app => mix_owner, path => <<"priv/native/example.so">>}]}],
    lists:foreach(fun(Path) ->
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, <<"host library">>)
    end, [RebarHost, MixHost, Unrelated, SourceHost]),
    try
        ok = grisp_tools_native:strip_release(Root, Nifs),
        ?assertNot(filelib:is_file(RebarHost)),
        ?assertNot(filelib:is_file(MixHost)),
        ?assert(filelib:is_file(Unrelated)),
        ?assert(filelib:is_file(SourceHost)),
        ok = grisp_tools_native:strip_release(Root, Nifs),
        ok = file:del_dir_r(MixPriv),
        ok = file:make_symlink(Source, MixPriv),
        ?assertException(error, {native_release_symlink, _},
                         grisp_tools_native:strip_release(Root, Nifs)),
        ?assert(filelib:is_file(SourceHost)),
        ok = file:delete(MixPriv),
        lists:foreach(fun(Path) ->
            ?assertError({native_release_invalid_path, Path},
                grisp_tools_native:strip_release(Root, [#{host_artifacts => [#{app => mix_owner, path => Path}]}]))
        end, ["../source-priv", "/tmp/source-priv", "priv/../source-priv", "priv/native"])
    after
        case file:read_link(MixPriv) of
            {ok, _} -> file:delete(MixPriv);
            _ -> ok
        end,
        file:del_dir_r(Root)
    end.
