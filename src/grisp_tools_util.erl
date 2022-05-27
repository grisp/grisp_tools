-module(grisp_tools_util).

% API
-export([weave/2]).
-export([event/2]).
-export([shell/2]).
-export([shell/3]).
-export([exec/3]).
-export([env/1]).
-export([cdn_path/2]).
-export([paths/5]).
-export([otp_checkout_dir/2]).
-export([package_name/1]).
-export([package_cache_temp/1]).
-export([package_cache_file/1]).
-export([package_cache_meta/1]).
-export([package_cache_meta/2]).
-export([ensure_dir/1]).
-export([mv/2]).
-export([rm/1]).
-export([build_overlay/4]).
-export([deploy_overlay/4]).
-export([build_hash/1]).
-export([build_hash_format/1]).
-export([merge_build_config/2]).
-export([source_hash/2]).
-export([write_file/3]).
-export([with_file/3]).
-export([pipe/2]).
-export([find_files/2]).
-export([find_files/3]).
-export([otp_package_cache/0]).

%--- Macros --------------------------------------------------------------------

-define(HASH_BLOCK_SIZE, 4194304). % 4MB

%--- API -----------------------------------------------------------------------

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

with_event(#{event_stack := Current} = State0, New, Fun) ->
    Stack = Current ++ New,
    grisp_tools_util:pipe(State0, [
        fun(S) -> event(S#{event_stack => Stack}, []) end,
        fun(S) -> Fun(S#{event_stack => Stack}) end,
        % fun(S) -> event(S, ['_end']) end,
        fun(S) -> S#{event_stack => Current} end
    ]);
with_event(State0, New, Fun) ->
    with_event(State0#{event_stack => []}, New, Fun).

event(#{event_stack := Stack} = State0, Events) when is_list(Events) ->
    {_Result, State1} = exec(event, State0, [Stack ++ Events]),
    State1;
event(State0, Event) -> % TODO: Remove this clause
    {_Result, State1} = exec(event, State0, [Event]),
    State1.

shell(State, Command) -> shell(State, Command, []).

shell(State, Command, Opts) ->
    Env = mapz:deep_get([shell, env], State, #{}),
    exec(shell, State, [Command, [{env, maps:to_list(Env)}] ++ Opts]).

exec(Handler, #{handlers := Handlers} = State, Args) ->
    {Result, NewHandlers} = grisp_tools_handler:run(Handler, Args, Handlers),
    {Result, State#{handlers => NewHandlers}}.

env(Key) ->
    {ok, Value} = application:get_env(grisp_tools, Key),
    Value.

cdn_path(otp, #{platform := Platform} = State) ->
    File = package_name(State),
    Path = lists:join($/, ["platforms", atom_to_binary(Platform), "otp", File]),
    uri_string:resolve(Path, [env(cdn), "/"]).

paths(Root, Platform, {_Components, _Pre, _Build, Ver}, _Hash, _CustomBuild = true) ->
    Dir = filename:join(otp_checkout_dir(Root, Platform), Ver),
    sub_paths([Dir]);
paths( _, Platform, {_Components, _Pre, _Build, Ver}, Hash, _CustomBuild = false) ->
    Dir = filename:join([cache(), Platform, "otp", Ver]),    
    sub_paths([Dir, Hash]).

otp_checkout_dir(Root, Platform) ->
    filename:join([Root, "_grisp", Platform, "otp"]).

package_name(#{otp_version := {_, _, _, OTPVersion}, build := #{hash := #{value := Hash}}}) ->
    iolist_to_binary(["grisp_otp_build_", OTPVersion, "_", Hash, ".tar.gz"]).

package_cache_temp(State) ->
    <<(package_cache_file(State))/binary, ".temp">>.

package_cache_file(#{paths := #{package_cache := Cache}} = State) ->
    filename:join(Cache, package_name(State)).

package_cache_meta(State) ->
    PackageFile = package_cache_file(State),
    MetaFile = iolist_to_binary([PackageFile, ".meta"]),
    case {filelib:is_regular(PackageFile), filelib:is_regular(MetaFile)} of
        {true, true}  ->
            {ok, Meta} = file:consult(MetaFile),
            mapz:deep_merge(Meta);
        _Else ->
            #{}
    end.

package_cache_meta(State, Meta) ->
    PackageFile = package_cache_file(State),
    MetaFile = iolist_to_binary([PackageFile, ".meta"]),
    ok = file:write_file(MetaFile, io_lib:format("~p.~n", [Meta])).

ensure_dir(File) ->
    case filelib:ensure_dir(File) of
        ok    -> ok;
        Error -> error({create_dir_failed, filename:dirname(File), Error})
    end.

mv(From, To) ->
    rm(To),
    case file:rename(From, To) of
        ok              -> ok;
        Error           -> error({rename_file_failed, From, To, Error})
    end.

rm(File) ->
    case file:delete(File) of
        ok              -> ok;
        {error, enoent} -> ok;
        Error           -> error({delete_file_failed, File, Error})
    end.

build_overlay(App, AppDir, Platform, Versions) ->
    collect_overlay(AppDir, Platform, Versions, {#{}, #{}}, fun(Dir, {Files, Config}) ->
        BuildDir = filename:join(Dir, build),
        NewFiles = mapz:deep_merge(Files, pipe(Files, [
            fun(S) -> collect_build_files(App, BuildDir, S) end,
            fun(S) -> collect_build_patches(App, BuildDir, S) end,
            fun(S) -> collect_build_drivers(App, BuildDir, S) end,
            fun(S) -> collect_build_nifs(App, BuildDir, S) end,
            fun(S) -> collect_build_hooks(App, BuildDir, S) end
        ])),
        {NewFiles, collect_build_config(App, Dir, Config)}
    end).

deploy_overlay(App, AppDir, Platform, Versions) ->
    collect_overlay(AppDir, Platform, Versions, #{}, fun(Dir, Files) ->
        BuildDir = filename:join(Dir, deploy),
        mapz:deep_merge(Files, pipe(Files, [
            fun(S) -> collect_build_files(App, BuildDir, S) end
        ]))
    end).

collect_overlay(Dir, Platform, Versions, Init, CollectFun) ->
    PlatformDir = filename:join([Dir, grisp, Platform]),
    case filelib:is_dir(PlatformDir) of
        true ->
            lists:foldl(fun(Version, Acc) ->
                collect_version_files(PlatformDir, Version, Acc, CollectFun)
            end, Init, Versions);
        false ->
            Init
    end.

merge_build_config(C1, C2) ->
    mapz:deep_merge(C1, C2).

build_hash(#{build := #{overlay := #{hooks := Hooks} = Overlay}}) ->
    AllHooks = maps:from_list([{N, I} || {_T, F} <- maps:to_list(Hooks), {N, I} <- maps:to_list(F)]),
    HashIndex = lists:sort(maps:fold(fun
        (_Type, Files, Acc) ->
            Acc ++ maps:fold(fun(_Name, Info, L) ->
                {File, Origin} = case Info of
                    #{target := Target, source := {template, Source}} ->
                        {Target, Source};
                    #{target := Target, source := Source} ->
                        {Target, Source};
                    #{name := Name, source := {template, Source}} ->
                        {Name, Source};
                    #{name := Name, source := Source} ->
                        {Name, Source}
                end,
                {ok, Hash} = hash_file(Origin, sha256),
                [{File, format_hash(sha256, Hash)}|L]
            end, [], Files)
    end, [], Overlay#{hooks => AllHooks})),
    TopHash = format_hash(sha256, crypto:hash(sha256, build_hash_format(HashIndex))),
    {TopHash, HashIndex}.

build_hash_format(Index) ->
    [io_lib:format("~s ~s~n", [File, Hash]) || {File, Hash} <- Index].

source_files(Apps, Board) ->
    lists:foldl(fun({_App, #{dir := Dir}}, {Sys, Drivers, NIFs}) ->
        {AppSys, AppDrivers, AppNIFs} = collect_c_sources(Dir, Board),
        {maps:merge(Sys, AppSys), maps:merge(Drivers, AppDrivers), maps:merge(NIFs, AppNIFs)}
    end, {#{}, #{}, #{}}, Apps).

source_hash(Apps, Board) ->
    {DriverFiles, SystemFiles, NIFFiles} = source_files(Apps, Board),
    Targets = maps:merge(DriverFiles, SystemFiles),
    Targets2 = maps:merge(Targets, NIFFiles),
    hash_files(Targets2).

write_file(Root, #{target := Target} = File, Context) ->
    Content = read_file(File, Context),
    Destination = filename:join(Root, Target),
    ensure_dir(Destination),
    ok = file:write_file(Destination, Content).

read_file(#{source := {template, Source}}, Context) ->
    grisp_tools_template:render(Source, Context);
read_file(#{source := Source}, _Context) ->
    {ok, Binary} = file:read_file(Source),
    Binary.

with_file(File, Opts, Fun) ->
    Handle = case file:open(File, Opts) of
        {ok, H} -> H;
        Error   -> error({open_file_failed, File, Error})
    end,
    try
        Fun(Handle)
    after
        file:close(Handle)
    end.

pipe(State, Actions) ->
    lists:foldl(fun(Action, S) -> Action(S) end, State, Actions).

otp_package_cache() ->
    filename:join([cache(), "packages", "otp"]).

find_files(Dir, Regex) ->
    find_files(Dir, Regex, false).

find_files(Dir, Regex, Recursive) ->
    filelib:fold_files(Dir, Regex, Recursive, fun(F, Acc) -> [F | Acc] end, []).

%--- Internal ------------------------------------------------------------------

sub_paths(Dir) ->
    #{
        build => filename:join(Dir ++ ["build"]),
        install => filename:join(Dir ++ ["install"]),
        package => filename:join(Dir ++ ["package"]),
        package_cache => otp_package_cache()
    }.

cache() -> filename:basedir(user_cache, "grisp").

collect_c_sources(Dir, Board) ->
    Source = filename:join([Dir, "grisp", Board]),
    case filelib:is_dir(Source) of
        true  -> {collect_sys(Source), collect_drivers(Source), collect_nifs(Source)};
        false -> {#{}, #{}, #{}}
    end.

collect_sys(Source) ->
    maps:merge(
        collect_files({Source, "sys/*.h"}, "erts/emulator/sys/unix"),
        collect_files({Source, "sys/*.c"}, "erts/emulator/sys/unix")
    ).

collect_drivers(Source) ->
    maps:merge(
        collect_files(
            {Source, "drivers/*.h"},
            "erts/emulator/drivers/unix"
        ),
        collect_files(
            {Source, "drivers/*.c"},
            "erts/emulator/drivers/unix"
        )
    ).

collect_nifs(Source) ->
    maps:merge(
      collect_files(
        {Source, "nifs/*.h"},
        "erts/emulator/nifs/common"
       ),
      collect_files(
        {Source, "nifs/*.c"},
        "erts/emulator/nifs/common"
       )
     ).

collect_files({SourceRoot, Pattern}, Target) ->
    Files = filelib:wildcard(filename:join(SourceRoot, Pattern)),
    lists:foldl(fun(File, Collected) ->
        TargetFile = filename:join([Target, filename:basename(File)]),
        Collected#{TargetFile => File}
    end, #{}, Files).

hash_files(Targets) ->
    Sorted = lists:keysort(1, maps:to_list(Targets)),
    FileHashes = lists:map(fun({Target, Source}) ->
        {ok, Hash} = hash_file(Source, sha256),
        {Target, Hash}
    end, Sorted),

    HashIndex = lists:map(fun({Target, Hash}) ->
        io_lib:format("~s ~s~n", [Target, format_hash(sha256, Hash)]) end,
    FileHashes),

    TopHash = format_hash(sha256, crypto:hash(sha256, HashIndex)),
    {TopHash, HashIndex}.

hash_file(File, Algorithm) ->
    Context = crypto:hash_init(Algorithm),
    with_file(File, [binary, raw, read], fun(Handle) ->
        hash_file_read(Handle, Context)
    end).

hash_file_read(Handle, Context) ->
    case file:read(Handle, ?HASH_BLOCK_SIZE) of
        {ok, Bin} -> hash_file_read(Handle, crypto:hash_update(Context, Bin));
        eof       -> {ok, crypto:hash_final(Context)}
    end.

format_hash(sha256, <<Int:256/big-unsigned-integer>>) -> format_hash(Int);
format_hash(md5, <<Int:128/big-unsigned-integer>>)    -> format_hash(Int).

format_hash(Int) when is_integer(Int) ->
    list_to_binary(io_lib:format("~.16b", [Int])).

collect_version_files(PlatformDir, Version, Acc, CollectFun) ->
    RootDir = filename:join(PlatformDir, Version),
    case filelib:is_dir(RootDir) of
        true -> CollectFun(RootDir, Acc);
        false -> Acc
    end.

collect_build_files(App, Dir, State) ->
    State#{files => collect_file_tree(App, filename:join(Dir, "files"))}.

collect_build_patches(App, Dir, State) ->
    State#{patches => collect_file_list(App, filename:join(Dir, "patches"))}.

collect_build_drivers(App, Dir, State) ->
    State#{drivers => collect_file_list(App, filename:join(Dir, "drivers"), "erts/emulator/drivers/unix")}.

collect_build_nifs(App, Dir, State) ->
    State#{nifs => collect_file_list(App, filename:join(Dir, "nifs"), "erts/emulator/nifs/common")}.

collect_build_hooks(App, Root, State0) ->
    Dir = filename:join(Root, "hooks"),
    lists:foldl(fun(Hook, State1) ->
        Prefix = hook_prefix(Hook),
        Info = file_info(Hook, App, filename:join(Dir, Hook)),
        mapz:deep_put([hooks, Prefix, Hook], Info, State1)
    end, State0, filelib:wildcard("*", binary_to_list(Dir))).

hook_prefix("post-install" ++ _) -> post_install;
hook_prefix(Hook) -> error({unknown_hook_prefix, Hook}).

collect_build_config(_App, Dir, Acc) ->
    case file:consult(filename:join(Dir, "grisp.config")) of
        {error, enoent} ->
            Acc;
        {error, Reason} ->
            error({error_reading_grisp_conf, file:format_error(Reason)});
        {ok, List} ->
            maps:get(build, mapz:deep_merge(List))
    end.

collect_file_list(App, Dir) -> collect_file_list(App, Dir, "").

collect_file_list(App, Dir, Root) ->
    InsertFile = fun(Match, A) ->
        File = list_to_binary(Match),
        {Source, Target} = check_template(
            filename:join(Dir, File),
            string:trim(filename:join(Root, File), both, "/")
        ),
        A#{Target => file_info(Target, App, Source, Target)}
    end,
    lists:foldl(InsertFile, #{}, filelib:wildcard("*", binary_to_list(Dir))).

collect_file_tree(App, Root) ->
    filelib:fold_files(Root, ".*", true, fun(File, T) ->
        Relative = string:trim(string:prefix(File, Root), leading, "/"),
        {Source, Target} = check_template(File, Relative),
        T#{Target => file_info(Relative, App, Source, Target)}
    end, #{}).

name(Atom) when is_atom(Atom) ->
    Atom;
name(Fun) when is_function(Fun) ->
    {name, AName} = erlang:fun_info(Fun, name),
    name(atom_to_list(AName));
name("-fun." ++ Fun) ->
    name(string:trim(Fun, trailing, "/1-"));
name(String) when is_list(String) ->
    list_to_atom(String).

check_template(File, Target) ->
    case filename:extension(File) of
        <<".mustache">> ->
            {{template, File}, filename:rootname(Target, <<".mustache">>)};
        _Else ->
            {File, Target}
    end.

file_info(Name, App, Source) ->
    #{
        name => path_to_binary(Name),
        app => App,
        source => path_to_binary(Source)
    }.

file_info(Name, App, Source, Target) ->
    maps:put(target, path_to_binary(Target), file_info(Name, App, Source)).

path_to_binary({template, Path}) when is_list(Path) -> {template, iolist_to_binary(Path)};
path_to_binary(Path) when is_list(Path) -> iolist_to_binary(Path);
path_to_binary(Path) -> Path.
