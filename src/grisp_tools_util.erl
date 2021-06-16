-module(grisp_tools_util).

% API
-export([env/1]).
-export([cdn_path/2]).
-export([otp_build_path/3]).
-export([otp_install_path/3]).
-export([otp_package_path/3]).
-export([otp_path/2]).
-export([package_name/1]).
-export([package_cache_temp/1]).
-export([package_cache_file/1]).
-export([package_cache_etag/1]).
-export([ensure_dir/1]).
-export([mv/2]).
-export([rm/1]).
-export([build_overlay/4]).
-export([overlay_hash/1]).
-export([merge_build_config/2]).
-export([source_hash/2]).
-export([with_file/3]).
-export([pipe/2]).

%--- Macros --------------------------------------------------------------------

-define(HASH_BLOCK_SIZE, 4194304). % 4MB

%--- API -----------------------------------------------------------------------

% FIXME: Change to grisp_tools when migrated
env(Key) ->
    {ok, Value} = application:get_env(grisp_tools, Key),
    Value.

cdn_path(otp, #{board := Board} = State) ->
    File = grisp_tools_util:package_name(State),
    string:join([env(cdn), "platforms", Board, "otp", File], "/").

otp_build_path(Root, Platform, Version) ->
    filename:join(otp_dir(Root, Platform, Version), "build").

otp_install_path(Root, Platform, Version) ->
    filename:join(otp_dir(Root, Platform, Version), "install").

otp_package_path(Root, Platform, Version) ->
    filename:join(otp_dir(Root, Platform, Version), "package").

otp_dir(Root, Platform, {_Components, _Pre, _Build, Ver}) ->
    filename:join([Root, "_grisp", Platform, "otp", Ver]).

% TODO: Deprecate
otp_path(#{custom_build := true, project_root := Root, otp_version := OTPVersion}, build_root) ->
    filename:join([Root, "_grisp", "otp", OTPVersion, "build"]);
otp_path(#{custom_build := true, project_root := Root, otp_version := OTPVersion}, install_root) ->
    filename:join([Root, "_grisp", "otp", OTPVersion, "install"]);
otp_path(#{board := Board} = State, install_root) ->
    filename:join([package_dir(), Board, package_name(State)]).

package_name(#{otp_version := OTPVersion, hash := Hash}) ->
    ["grisp_otp_build_", OTPVersion, "_", Hash, ".tar.gz"].

package_cache_temp(State) ->
    package_cache_file(State) ++ ".temp".

package_cache_file(State) ->
    filename:join(otp_path(State, install_root), package_name(State)).

package_cache_etag(State) ->
    InstallRoot = otp_path(State, install_root),
    Tarball = grisp_tools_util:package_name(State),
    PackageFile = filename:join(InstallRoot, Tarball),
    ETagFile = filename:join(InstallRoot, "ETag"),
    ETag = case {filelib:is_regular(PackageFile), filelib:is_regular(ETagFile)} of
        {true, true}  ->
            {ok, Properties} = file:consult(ETagFile),
            proplists:get_value(etag, Properties);
        _Else ->
            undefined
    end,
    {ETagFile, ETag}.

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
    PlatformDir = filename:join([AppDir, grisp, Platform]),
    Init = {#{}, #{}},
    case filelib:is_dir(PlatformDir) of
        true ->
            lists:foldl(fun(Version, Acc) ->
                collect_version_files(App, PlatformDir, Version, Acc)
            end, Init, Versions);
        false ->
            Init
    end.

merge_build_config(C1, C2) ->
    mapz:deep_merge(C1, C2).

overlay_hash(#{hooks := Hooks} = Overlay) ->
    AllHooks = maps:from_list([{N, I} || {_T, F} <- maps:to_list(Hooks), {N, I} <- maps:to_list(F)]),
    HashIndex = lists:sort(maps:fold(fun
        (_Type, Files, Acc) ->
            Acc ++ maps:fold(fun(Name, Info, L) ->
                {File, Origin} = case Info of
                    #{dest := Dest, source := Source} -> {Dest, Source};
                    #{source := Source} -> {Name, Source}
                end,
                {ok, Hash} = hash_file(Origin, sha256),
                [io_lib:format("~s ~s~n", [File, format_hash(sha256, Hash)])|L]
            end, [], Files)
    end, [], Overlay#{hooks => AllHooks})),
    TopHash = format_hash(sha256, crypto:hash(sha256, HashIndex)),
    {TopHash, HashIndex}.

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

%--- Internal ------------------------------------------------------------------

cache_dir() -> filename:basedir(user_cache, "grisp").

package_dir() -> filename:join([cache_dir(), "packages", "otp"]).

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

format_hash(Int) when is_integer(Int) -> io_lib:format("~.16b", [Int]).

collect_version_files(App, PlatformDir, Version, {Files, Config} = Acc) ->
    RootDir = filename:join(PlatformDir, Version),
    BuildDir = filename:join(RootDir, build),
    case filelib:is_dir(RootDir) of
        true ->
            NewFiles = mapz:deep_merge(Files, pipe(Files, [
                fun(S) -> collect_build_files(App, BuildDir, S) end,
                fun(S) -> collect_build_patches(App, BuildDir, S) end,
                fun(S) -> collect_build_drivers(App, BuildDir, S) end,
                fun(S) -> collect_build_nifs(App, BuildDir, S) end,
                fun(S) -> collect_build_hooks(App, BuildDir, S) end
            ])),
            {NewFiles, collect_build_config(App, RootDir, Config)};
        false ->
            Acc
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
        Info = #{
            name => Hook,
            app => App,
            source => filename:join(Dir, Hook)
        },
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

collect_file_list(App, Dir, Target) ->
    InsertFile = fun(File, A) ->
        Info = #{
            name => File,
            app => App,
            source => filename:join(Dir, File),
            dest => string:trim(filename:join(Target, File), both, "/")
        },
        A#{File => Info}
    end,
    lists:foldl(InsertFile, #{}, filelib:wildcard("*", binary_to_list(Dir))).

collect_file_tree(App, Root) ->
    filelib:fold_files(Root, ".*", true, fun(File, T) ->
        Relative = string:trim(string:prefix(File, Root), leading, "/"),
        Info = #{
            name => Relative,
            app => App,
            source => File,
            dest => Relative
        },
        T#{Relative => Info}
    end, #{}).
