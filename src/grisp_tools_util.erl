-module(grisp_tools_util).

-include_lib("kernel/include/file.hrl").

% API
-export([weave/2]).
-export([weave/3]).
-export([event/2]).
-export([event_with_result/2]).
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
-export([copy_directory/3, copy_directory/4]).
-export([copy_file/3, copy_file/4]).
-export([write_file/3, write_file/4]).
-export([read_file/2]).
-export([with_file/3]).
-export([pipe/2]).
-export([pipe/3]).
-export([iterate/4]).
-export([find_files/2]).
-export([otp_package_cache/1]).
-export([make_relative/1]).
-export([make_relative/2]).
-export([maybe_relative/2]).
-export([maybe_relative/3]).
-export([filelib_ensure_path/1]).
-export([format_term/1]).


%--- Macros --------------------------------------------------------------------

-define(HASH_BLOCK_SIZE, 4194304). % 4MB

%--- API -----------------------------------------------------------------------

weave(S0, []) ->
    S0;
weave(S0, [{Step, Inner}|Steps]) ->
    weave(with_event(S0, [name(Step)], fun(S1) ->
        S2 = case Step of
            Step when is_atom(Step) -> S1;
            Step when is_function(Step) -> wrap_call(S1, Step)
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

weave(State, Actions, CleanupActions) ->
    cleanup_call(State, fun(S) -> weave(S, Actions) end, CleanupActions).

with_event(#{event_stack := Current} = State0, New, Fun) ->
    Stack = Current ++ New,
    grisp_tools_util:pipe(State0, [
        fun(S) -> event(S#{event_stack => Stack}, []) end,
        fun(S) -> wrap_call(S#{event_stack => Stack}, Fun) end,
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

event_with_result(#{event_stack := Stack} = State0, Events) when is_list(Events) ->
    exec(event, State0, [Stack ++ Events]);
event_with_result(State0, Event) ->
    exec(event, State0, [Event]).

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
    sub_paths([Dir], Platform);
paths( _, Platform, {_Components, _Pre, _Build, Ver}, Hash, _CustomBuild = false) ->
    Dir = filename:join([cache(), Platform, "otp", Ver]),
    sub_paths([Dir, Hash], Platform).

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

build_hash(#{build := #{overlay := Overlay}}) ->
    Hooks = maps:get(hooks, Overlay, #{}),
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

copy_directory(State0, Src, Dest) ->
    recursive_copy(State0, Src, Dest, [], #{}).

copy_directory(State0, Src, Dest, Opts) ->
    recursive_copy(State0, Src, Dest, [], Opts).

copy_file(Root, File, Context) ->
    Content = read_file(File, Context),
    write_file(Root, File, Content).

copy_file(Root, File, Context, FileInfo) ->
    Content = read_file(File, Context),
    write_file(Root, File, Content, FileInfo).

write_file(Root, File, Content) ->
    write_file(Root, File, Content, undefined).

write_file(Root, #{target := Target}, Content, FileInfo) ->
    Destination = filename:join(Root, Target),
    ensure_dir(Destination),
    ok = file:write_file(Destination, Content),
    case FileInfo of
        undefined -> ok;
        Rec when is_record(Rec, file_info) ->
            case file:write_file_info(Destination, FileInfo, [{time, posix}]) of
                ok -> ok;
                {error, Reason} ->
                    erlang:error({write_file_info_error, Reason, Destination})
            end
    end.

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
    lists:foldl(fun(Action, S) -> wrap_call(S, Action) end, State, Actions).

pipe(State, Actions, CleanupActions) ->
    cleanup_call(State, fun(S) -> pipe(S, Actions) end, CleanupActions).

iterate(State, Iterations, StateKey, Fun) ->
    lists:foldl(fun
        ({IterName, IterState}, S) when is_atom(IterName), is_map(IterState) ->
            with_event(S#{StateKey => IterState}, [IterName], Fun);
        (IterState, S) ->
            wrap_call(S#{StateKey => IterState}, Fun)
    end, State, Iterations).

otp_package_cache(Platform) ->
    filename:join([cache(), "packages", Platform, "otp"]).

find_files(Dir, Regex) ->
    find_files(Dir, Regex, false).

find_files(Dir, Regex, Recursive) ->
    filelib:fold_files(Dir, Regex, Recursive, fun(F, Acc) -> [F | Acc] end, []).

make_relative(Path) ->
    case file:get_cwd() of
        {error, Reason} -> error(Reason);
        {ok, Dir} -> make_relative(Dir, Path)
    end.

make_relative(BasePath, Path) ->
    maybe_relative(BasePath, Path, infinity).

maybe_relative(Path, MaxDoubleDots) ->
    case file:get_cwd() of
        {error, Reason} -> error(Reason);
        {ok, Dir} -> maybe_relative(Dir, Path, MaxDoubleDots)
    end.

maybe_relative(BasePath, Path, MaxDoubleDots) ->
    AbsBase = filename:absname(iolist_to_binary(BasePath)),
    AbsPath = filename:absname(iolist_to_binary(Path)),
    BaseParts = filename:split(AbsBase),
    PathParts = filename:split(AbsPath),
    {_Common, BaseRem, PathRem} = common_prefix(BaseParts, PathParts),
    case MaxDoubleDots =:= infinity orelse length(BaseRem) =< MaxDoubleDots of
        false -> AbsPath;
        true ->
            RelParts = lists:duplicate(length(BaseRem), "..") ++ PathRem,
            filename:join(RelParts)
    end.

% For OTP < 25
filelib_ensure_path(Path) ->
    DirPath = filename:dirname(Path),
    case filelib:ensure_dir(filename:join(DirPath, "dummy.file")) of
        ok -> ok;
        {error, enoent} ->
            filelib_ensure_path(DirPath),
            file:make_dir(DirPath);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%% @doc Formats a term into a UTF8 encoded binary that can be later read by
%% file:consult/1. If the binaries are strings, they are supposed to be encoded
%% in UTF8.
-spec format_term(term()) -> binary().
format_term(Term) ->
    IoData = format_term(<<"    ">>, 0, [], Term),
    Bin = unicode:characters_to_binary(IoData, utf8),
    <<"%% coding: utf-8\n", Bin/binary>>.


%--- Internal ------------------------------------------------------------------

common_prefix([H | T1], [H | T2]) ->
    {Common, R1, R2} = common_prefix(T1, T2),
    {[H | Common], R1, R2};
common_prefix(L1, L2) ->
    {[], L1, L2}.

wrap_call(State = #{'_wrap_exceptions' := true}, Fun) ->
    try Fun(State)
    catch
        throw:{'_wrapped_exception', _, _, _, _}=Reason ->
            throw(Reason);
        Class:Reason:Stack ->
            throw({'_wrapped_exception', State, Class, Reason, Stack})
    end;
wrap_call(State, Fun) ->
    Fun(State).

safe_pipe(State, Actions) ->
    lists:foldl(fun(Action, S) ->
        try Action(S) catch _:_ -> S end
    end, State, Actions).

cleanup_call(State = #{'_wrap_exceptions' := true}, Fun, CleanupActions) ->
    try Fun(State)
    catch throw:{'_wrapped_exception', LastState, Class, Reason, Stack} ->
        LastState2 = safe_pipe(LastState, CleanupActions),
        throw({'_wrapped_exception', LastState2, Class, Reason, Stack})
    end;
cleanup_call(State, Fun, CleanupActions) ->
    try Fun(State#{'_wrap_exceptions' => true}) of
        NewState -> maps:remove('_wrap_exceptions', NewState)
    catch throw:{'_wrapped_exception', LastState, Class, Reason, Stack} ->
        safe_pipe(LastState, CleanupActions),
        erlang:raise(Class, Reason, Stack)
    end.

sub_paths(Dir, Platform) ->
    #{
        build => filename:join(Dir ++ ["build"]),
        install => filename:join(Dir ++ ["install"]),
        package => filename:join(Dir ++ ["package"]),
        package_cache => otp_package_cache(Platform)
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

format_hash(sha256, <<Int:256/big-unsigned-integer>>) -> format_hash(Int).

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

resolve_symlink(FilePath) ->
    case file:read_link_info(FilePath, [{time, posix}]) of
        {ok, #file_info{type = symlink}} ->
            case file:read_link(FilePath) of
                {ok, Target} -> resolve_symlink(Target);
                {error, Reason} ->
                    erlang:error({read_link_error, Reason, FilePath})
            end;
        {error, Reason} ->
            erlang:error({read_link_info_error, Reason, FilePath});
        _ ->
            FilePath
    end.

recursive_copy_file(State0, SrcPath, DestRoot, RelPath, FileInfo,
                    #{copy_fun := CopyFun})
  when is_function(CopyFun, 5) ->
    CopyFun(State0, SrcPath, DestRoot, RelPath, FileInfo);
recursive_copy_file(State0, SrcPath, DestRoot, RelPath, FileInfo, _Opts) ->
    FileSpec = #{source => SrcPath, target => RelPath, name => RelPath},
    copy_file(DestRoot, FileSpec, #{}, FileInfo),
    State0.

recursive_create_dir(State0, DestRoot, RelPath, FileInfo,
                    #{create_dir_fun := CreateDirFun})
  when is_function(CreateDirFun, 4) ->
    CreateDirFun(State0, DestRoot, RelPath, FileInfo);
recursive_create_dir(State0, DestRoot, RelPath, FileInfo, _Opts) ->
    DestPath = filename:join(DestRoot, RelPath),
    filelib_ensure_path(filename:join(DestPath, "dummy.file")),
    case FileInfo of
        Rec when is_record(Rec, file_info) ->
            case file:write_file_info(DestPath, FileInfo, [{time, posix}]) of
                ok -> State0;
                {error, Reason} ->
                    erlang:error({write_file_info_error, Reason, DestPath})
            end
    end.

recursive_copy(State0, Src, Dest, RevRelNames, Opts) ->
    {SrcPath, RelPath} = case RevRelNames of
        [] -> {Src, ""};
        _ ->
            R = filename:join(lists:reverse(RevRelNames)),
            {filename:join(Src, R), R}
    end,
    case file:read_file_info(SrcPath, [{time, posix}]) of
        {ok, #file_info{type = directory} = FileInfo} ->
            State1 = recursive_create_dir(State0, Dest, RelPath, FileInfo, Opts),
            case file:list_dir(SrcPath) of
                {error, Reason} ->
                    erlang:error({list_dir_error, Reason, SrcPath});
                {ok, Names} ->
                    lists:foldl(fun(Name, State) ->
                        RevRelNames2 = [Name | RevRelNames],
                        recursive_copy(State, Src, Dest, RevRelNames2, Opts)
                    end, State1, Names)
            end;
        {ok, #file_info{type = regular} = FileInfo} ->
            case file:read_link_info(SrcPath, [{time, posix}]) of
                {ok, #file_info{type = regular}} ->
                    recursive_copy_file(State0, SrcPath, Dest, RelPath,
                                        FileInfo, Opts);
                {ok, #file_info{type = symlink}} ->
                    SrcPath2 = resolve_symlink(SrcPath),
                    recursive_copy_file(State0, SrcPath2, Dest, RelPath,
                                        FileInfo, Opts);
                {ok, #file_info{type = Other}} ->
                    erlang:error({invalid_file_type, Other, SrcPath});
                {error, Reason} ->
                    erlang:error({read_link_info_error, Reason, SrcPath})
            end;
        {ok, #file_info{type = Other}} ->
            erlang:error({invalid_file_type, Other, SrcPath});
        {error, Reason} ->
            erlang:error({read_file_info_error, Reason, SrcPath})
    end.

format_term_value(Val)
  when is_integer(Val); is_float(Val) ->
    io_lib:format("~w", [Val]);
format_term_value(Val)
  when is_atom(Val) ->
    io_lib:write_atom(Val);
format_term_value([]) ->
    "[]";
format_term_value(Val)
  when is_list(Val) ->
    io_lib:write_string(Val);
format_term_value(Val)
  when is_binary(Val) ->
    try unicode:characters_to_list(Val, utf8) of
        Unicode ->
            try io_lib:write_latin1_string(Unicode) of
                Str -> io_lib:format("<<~ts>>", [Str])
            catch
                _:_ ->
                    Str = io_lib:write_string(Unicode),
                    io_lib:format("<<~ts/utf8>>", [Str])
            end
    catch
        _:_ ->
            io_lib:format("~w", [Val])
    end.

format_term(_Prefix, _Level, Acc, []) ->
    lists:reverse(Acc);
format_term(Prefix, Level, Acc, [{Key, Val} | Rest])
  when is_atom(Key) orelse is_binary(Key),
       is_atom(Val) orelse is_binary(Val)
       orelse is_integer(Val) orelse is_float(Val) ->
    Indent = binary:copy(Prefix, Level),
    Ending = case {Level, Rest} of
        {0, _} -> <<".">>;
        {_, []} -> <<>>;
        {_, _} -> <<",">>
    end,
    KeyStr = format_term_value(Key),
    ValStr = format_term_value(Val),
    ResStr = io_lib:format("~s{~ts, ~ts}~s~n",
                           [Indent, KeyStr, ValStr, Ending]),
    format_term(Prefix, Level, [ResStr | Acc], Rest);
format_term(Prefix, Level, Acc, [Val | Rest])
  when is_atom(Val) orelse is_binary(Val)
       orelse is_integer(Val) orelse is_float(Val) ->
    Indent = binary:copy(Prefix, Level),
    Ending = case {Level, Rest} of
        {0, _} -> <<".">>;
        {_, []} -> <<>>;
        {_, _} -> <<",">>
    end,
    ValStr = format_term_value(Val),
    ResStr = io_lib:format("~s~ts~s~n", [Indent, ValStr, Ending]),
    format_term(Prefix, Level, [ResStr | Acc], Rest);
format_term(Prefix, Level, Acc, [{Key, Val} | Rest])
  when is_atom(Key) orelse is_binary(Key), is_list(Val) ->
    Indent = binary:copy(Prefix, Level),
    Ending = case {Level, Rest} of
        {0, _} -> <<".">>;
        {_, []} -> <<>>;
        {_, _} -> <<",">>
    end,
    KeyStr = format_term_value(Key),
    ResStr = case io_lib:printable_unicode_list(Val) of
        true ->
            ValStr = format_term_value(Val),
            io_lib:format("~s{~ts, ~ts}~s~n",
                          [Indent, KeyStr, ValStr, Ending]);
        false ->
            SubStr = format_term(Prefix, Level + 1, [], Val),
            io_lib:format("~s{~ts, [~n~ts~s]}~s~n",
                          [Indent, KeyStr, SubStr, Indent, Ending])
    end,
    format_term(Prefix, Level, [ResStr | Acc], Rest).
