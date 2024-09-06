-module(grisp_tools_firmware).

-include("grisp_tools.hrl").
-include_lib("kernel/include/file.hrl").

% API
-export([run/1]).

-import(grisp_tools_util, [event/2]).
-import(grisp_tools_util, [shell/2]).
-import(grisp_tools_util, [shell/3]).


%--- API -----------------------------------------------------------------------

run(State) ->
    grisp_tools_util:weave(State, [
        fun grisp_tools_step:config/1,
        {firmware, [
            fun prepare/1,
            fun build_firmware/1
        ]}
    ]).

%--- Tasks ---------------------------------------------------------------------

prepare(State) ->
    Force = maps:get(force, State, false),
    State2 = State#{force => Force},
    grisp_tools_util:weave(State2, [
        fun validate_platform/1,
        fun validate_temp_dir/1,
        fun validate_bundle/1,
        fun validate_system/1,
        fun validate_image/1,
        fun validate_boot/1,
        fun validate_toolchain/1
    ]).

validate_platform(State) ->
    case maps:find(platform, State) of
        {ok, grisp2} -> State;
        {ok, Platform} ->
            event(State, [{error, unsupported_platform, Platform}]);
        _ ->
            event(State, [{error, missing_parameter, platform}])
    end.

validate_temp_dir(State) ->
    case maps:find(temp_dir, State) of
        {ok, TempDir} when TempDir =/= undefined ->
            case filelib:is_dir(TempDir) of
                true -> State;
                false -> event(State, [{error, directory_not_found, TempDir}])
            end;
        _ ->
            {{ok, Output}, State2} = shell(State, "mktemp -d"),
            TempDir = string:trim(Output),
            case filelib:is_dir(TempDir) of
                true ->
                    State2#{temp_dir => TempDir,
                            cleanup_temp_dir => true};
                false ->
                    event(State2, [{error, directory_not_found, TempDir}])
            end
    end.

validate_bundle(State) ->
    case maps:find(bundle, State) of
        {ok, BundlePath} when BundlePath =/= undefined ->
            case filelib:is_file(BundlePath) of
                true -> State;
                false ->
                    event(State, [{error, bundle_not_found, BundlePath}])
            end;
        _ ->
            event(State, [{error, missing_parameter, bundle}])
    end.

validate_system(State) ->
    case maps:find(system, State) of
        error -> State#{system => undefined};
        {ok, undefined} -> State;
        {ok, #{target := TargetPath} = Opts} ->
            Compress = maps:get(compress, Opts, true),
            State2 = prepare_output_file(State, TargetPath),
            State2#{system => Opts#{compress => Compress}};
        {ok, Opts} ->
            event(State, [{error, invalid_system_options, Opts}])
    end.

validate_image(State) ->
    case maps:find(image, State) of
        error -> State#{image => undefined};
        {ok, undefined} -> State;
        {ok, #{target := TargetPath} = Opts} ->
            Compress = maps:get(compress, Opts, true),
            Truncate = maps:get(truncate, Opts, true),
            State2 = prepare_output_file(State, TargetPath),
            Opts2 = Opts#{compress => Compress, truncate => Truncate},
            State2#{image => Opts2};
        {ok, Opts} ->
            event(State, [{error, invalid_image_options, Opts}])
    end.

validate_boot(State) ->
    case maps:find(boot, State) of
        error -> State#{boot => undefined};
        {ok, undefined} -> State;
        {ok, #{target := TargetPath} = Opts} ->
            Compress = maps:get(compress, Opts, true),
            State2 = prepare_output_file(State, TargetPath),
            State2#{boot => Opts#{compress => Compress}};
        {ok, Opts} ->
            event(State, [{error, invalid_boot_options, Opts}])
    end.

validate_toolchain(State = #{image := ImageSpec, boot := BootSpec})
  when ImageSpec =/= undefined; BootSpec =/= undefined ->
    Toolchain = maps:get(toolchain, State, undefined),
    {SelectedBootloader, State2} = select_bootloader(State, Toolchain),
    case State2#{bootloader => SelectedBootloader} of
        #{bootloader := undefined, image := ImageSpec, boot := BootSpec}
          when ImageSpec =/= undefined; BootSpec =/= undefined ->
            event(State, [{error, toolchain_required}]);
        #{bootloader := undefined} = State3 ->
            State3;
        #{bootloader := BootloaderPath} = State3 ->
            event(State3, [{bootloader, filename:basename(BootloaderPath)}])
    end;
validate_toolchain(State) ->
    State#{bootloader => undefined}.

build_firmware(State) ->
    grisp_tools_util:weave(State, [
        fun create_image/1
    ]
    ++ if_key_defined(State, bootloader, [fun copy_bootloader/1], [])
    ++ [
        fun create_partitions/1,
        fun format_system/1,
        fun deploy_bundle/1
    ]
    ++ if_key_defined(State, system, [fun extract_system/1], [])
    ++ if_key_defined(State, image, [fun extract_image/1], [])
    ++ if_key_defined(State, boot, [fun extract_boot/1], [])
    ++ [
        fun close_image/1,
        fun cleanup/1
    ], [
        fun close_image/1,
        fun cleanup/1
    ]).

create_image(State = #{temp_dir := TempDir}) ->
    Opts = edifa_opts(State, #{temp_dir => TempDir}),
    ImageFile = filename:join(TempDir, "emmc.img"),
    case edifa:create(ImageFile, ?GRISP2_IMAGE_SIZE, Opts) of
        {ok, Pid, State2} ->
            State2#{edifa_pid => Pid};
        {error, Reason, State2} ->
            event(State2, [{error, Reason}])
    end.

copy_bootloader(State = #{edifa_pid := Pid, bootloader := BootFile}) ->
    Opts = edifa_opts(State, #{count => ?GRISP2_RESERVED_SIZE}),
    case edifa:write(Pid, BootFile, Opts) of
        {ok, State2} -> State2;
        {error, Reason, State2} ->
            event(State2, [{error, Reason}])
    end.

create_partitions(State = #{edifa_pid := Pid}) ->
    Opts = edifa_opts(State),
    case edifa:partition(Pid, mbr, ?GRISP2_PARTITIONS, Opts) of
        {ok, [_, _] = Partitions, State2} ->
            State2#{partitions => Partitions};
        {error, Reason, State2} ->
            event(State2, [{error, Reason}])
    end.

format_system(State = #{partitions := [PartId1, PartId2]}) ->
    format_system(format_system(State, PartId1, "SYS"), PartId2, "SYS").

deploy_bundle(State = #{partitions := [PartId1, PartId2]}) ->
    deploy_bundle(deploy_bundle(State, PartId1), PartId2).

extract_system(State = #{edifa_pid := Pid, partitions := [PartId, _],
                         system := #{target := Path,
                                     compress := Compress}}) ->
    Opts = edifa_opts(State, #{
        compressed => Compress,
        timeout => 10000
    }),
    case edifa:extract(Pid, PartId, PartId, Path, Opts) of
        {error, Reason, State2} ->
            event(State2, [{error, Reason}]);
        {ok, State2} ->
            event(State2, [{extracted, Path}])
    end.


extract_image(State = #{edifa_pid := Pid, partitions := [PartId1, PartId2],
                        image := #{target := Path,
                                   truncate := Truncated,
                                   compress := Compress}}) ->
    Opts = edifa_opts(State, #{
        compressed => Compress,
        timeout => 20000
    }),
    PartId = case Truncated of
        true -> PartId1;
        false -> PartId2
    end,
    case edifa:extract(Pid, reserved, PartId, Path, Opts) of
        {error, Reason, State2} ->
            event(State2, [{error, Reason}]);
        {ok, State2} ->
            event(State2, [{extracted, Path}])
    end.

extract_boot(State0 = #{edifa_pid := Pid,
                       boot := #{target := Path,
                                 compress := Compress}}) ->
    State = event(State0, [start_extracting_boot]),
    Opts = edifa_opts(State, #{compressed => Compress}),
    case edifa:extract(Pid, reserved, reserved, Path, Opts) of
        {error, Reason, State2} ->
            event(State2, [{error, Reason}]);
        {ok, State2} ->
            event(State2, [{extracted, Path}])
    end.

close_image(State = #{edifa_pid := Pid}) ->
    case edifa:close(Pid, edifa_opts(State)) of
        {ok, State2} -> maps:remove(edifa_pid, State2);
        {error, Reason, State2} ->
            event(State2, [{error, Reason}])
    end.

cleanup(State) ->
    cleanup_temp_dir(cleanup_image(State)).

cleanup_image(State = #{edifa_pid := Pid}) ->
    case edifa:close(Pid, edifa_opts(State)) of
        {ok, State2} -> maps:remove(edifa_pid, State2);
        {error, _Reason, State2} -> maps:remove(edifa_pid, State2)
    end;
cleanup_image(State) ->
    State.

cleanup_temp_dir(State = #{temp_dir := TempDir, cleanup_temp_dir := true}) ->
    {_, State2} = shell(State, ["rm -rf '", TempDir, "'"]),
    State2#{cleanup_temp_dir => false};
cleanup_temp_dir(State) ->
    State.


%--- Internal ------------------------------------------------------------------

if_key_defined(State, Key, Result, Default) ->
    case maps:find(Key, State) of
        {ok, Value} when Value =/= undefined -> Result;
        _ -> Default
    end.

edifa_opts(State) ->
    edifa_opts(State, #{}).

edifa_opts(State, Opts) ->
    Opts#{
        log_handler => fun edifa_log_hanler/2,
        log_state => State
    }.

edifa_log_hanler(Event, State) ->
    event(State, [Event]).

prepare_output_file(State, Filepath) ->
    Force = maps:get(force, State, false),
    case file:read_file_info(Filepath) of
        {ok, #file_info{}} when Force =:= false ->
            event(State, [{error, file_exists, Filepath}]);
        {ok, #file_info{type = regular}} when Force =:= true->
            case file:delete(Filepath) of
                ok -> State;
                {error, _Reason} ->
                    event(State, [{error, file_access, Filepath}])
            end;
        {ok, #file_info{type = regular}} ->
            event(State, [{error, not_a_file, Filepath}]);
        {error, enoent} ->
            State
    end.

select_bootloader(State, undefined) ->
    {undefined, State};
select_bootloader(State, {directory, RootDir}) ->
    case filelib:is_dir(RootDir) of
        false ->
            event(State, [{error, toolchain_not_found, {directory, RootDir}}]);
        true ->
            BaseDir = filename:join(RootDir, "barebox"),
            Filenames = ?GRISP2_BOOTLOADER_FILENAMES,
            select_bootloader(State, BaseDir, Filenames, fun(P, S) ->
                case filelib:is_file(P) of
                    true -> {ok, P, S};
                    false -> {error, S}
                end
            end)
    end;
select_bootloader(State = #{temp_dir := TempDir}, {docker, ImageName}) ->
    case docker_check_image(State, ImageName) of
        {error, State2} ->
            event(State2, [{error, toolchain_not_found, {docker, ImageName}}]);
        {ok, State2} ->
            BaseDir = filename:join(?DOCKER_TOOLCHAIN_ROOT, "barebox"),
            Filenames = ?GRISP2_BOOTLOADER_FILENAMES,
            select_bootloader(State2, BaseDir, Filenames, fun(P, S) ->
                case docker_export(S, ImageName, P, TempDir) of
                    {error, _S2} = Error -> Error;
                    {ok, S2} ->
                        {ok, filename:join(TempDir, filename:basename(P)), S2}
                end
            end)
    end.

select_bootloader(State, _BaseDir, [], _CheckFun) ->
    {undefined, State};
select_bootloader(State, BaseDir, [Filename | Rest], CheckFun) ->
    Path = filename:join(BaseDir, Filename),
    case CheckFun(Path, State) of
        {ok, Result, State2} -> {Result, State2};
        {error, State2} ->
            select_bootloader(State2, BaseDir, Rest, CheckFun)
    end.

docker_check_image(State, ImageName) ->
    Command = ["docker manifest inspect '", ImageName, "'"],
    case shell(State, Command, [return_on_error]) of
        {{ok, _}, State2} -> {ok, State2};
        {{error, _}, State2} -> {error, State2}
    end.

docker_export(State, ImageName, InPath, OutDir) ->
    ok = grisp_tools_util:ensure_dir(OutDir),
    Command = ["docker run --rm --volume ", OutDir,  ":", OutDir,
               " ", ImageName, " sh -c \"cd ", OutDir,
               " && cp -rf '", InPath, "' .\""],
    case shell(State, Command, [return_on_error]) of
        {{ok, _}, State2} -> {ok, State2};
        {{error, _}, State2} -> {error, State2}
    end.

format_system(State = #{edifa_pid := Pid}, PartId, Label) ->
    Opts = edifa_opts(State, #{
        label => Label,
        type => ?GRISP2_FAT_TYPE,
        cluster_size => ?GRISP2_FAT_CLUSTER_SIZE
    }),
    case edifa:format(Pid, PartId, fat, Opts) of
        {ok, State2} -> State2;
        {error, Reason, State2} ->
            event(State2, [{error, Reason}])
    end.

deploy_bundle(State = #{edifa_pid := Pid, bundle := BundleFile}, PartId) ->
    Opts = edifa_opts(State),
    case edifa:mount(Pid, PartId, Opts) of
        {error, Reason, State2} ->
            event(State2, [{error, Reason}]);
        {ok, MountPoint, State2} ->
            ExpandCmd = ["tar -C ", MountPoint, " -xzf ", BundleFile],
            {{ok, _}, State3} = shell(State2, ExpandCmd),
            Opts2 = edifa_opts(State2),
            case edifa:unmount(Pid, PartId, Opts2) of
                {error, Reason, State2} ->
                    event(State2, [{error, Reason}]);
                {ok, State3} ->
                    State3
            end
    end.
