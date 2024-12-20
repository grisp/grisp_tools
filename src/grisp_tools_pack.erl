-module(grisp_tools_pack).

-include("grisp_tools.hrl").
-include_lib("kernel/include/file.hrl").

% API
-export([run/1]).

-import(grisp_tools_util, [event/2]).


%--- API -----------------------------------------------------------------------

run(State) ->
    grisp_tools_util:weave(State, [
        fun grisp_tools_step:config/1,
        {pack, [
            fun prepare/1,
            fun package/1,
            fun cleanup/1
        ]}
    ]).

%--- Tasks ---------------------------------------------------------------------

prepare(State) ->
    Force = maps:get(force, State, false),
    State2 = State#{force => Force},
    grisp_tools_util:weave(State2, [
        fun validate_temp_dir/1,
        fun validate_system/1,
        fun validate_bootloader/1,
        fun validate_package/1
    ]).

validate_temp_dir(State) ->
    case maps:find(temp_dir, State) of
        {ok, TempDir} when TempDir =/= undefined ->
            case filelib:is_dir(TempDir) of
                true -> State;
                false -> event(State, [{error, directory_not_found, TempDir}])
            end;
        _ ->
            {Output, State2} = shell(State, "mktemp -d", []),
            TempDir = string:trim(Output),
            case filelib:is_dir(TempDir) of
                true ->
                    State2#{temp_dir => TempDir,
                            cleanup_temp_dir => true};
                false ->
                    event(State2, [{error, directory_not_found, TempDir}])
            end
    end.

validate_system(State) ->
    case maps:find(system, State) of
        {ok, SysPath} when SysPath =/= undefined ->
            case filelib:is_file(SysPath) of
                true -> State;
                false ->
                    event(State, [{error, system_not_found, SysPath}])
            end;
        _ ->
            event(State, [{error, missing_parameter, system}])
    end.

validate_bootloader(State = #{bootloader := BootPath})
  when BootPath =/= undefined ->
    case filelib:is_file(BootPath) of
        true -> State;
        false ->
            event(State, [{error, bootloader_not_found, BootPath}])
    end;
validate_bootloader(State) ->
     State#{bootloader => undefined}.

validate_package(State) ->
    case maps:find(package, State) of
        {ok, PackageFile} when PackageFile =/= undefined ->
            prepare_output_file(State, PackageFile);
        _ ->
            event(State, [{error, missing_parameter, package}])
    end.

package(State) ->
    grisp_tools_util:weave(State, [
        fun expand_bootloader/1,
        fun expand_system/1,
        fun create_image/1,
        fun create_partitions/1,
        fun copy_firmware/1,
        fun extract_manifest/1,
        fun close_image/1,
        fun build_package/1,
        fun cleanup/1
    ], [
        fun close_image/1,
        fun cleanup/1
    ]).

expand_bootloader(State = #{bootloader := BootPath})
 when BootPath =/= undefined ->
    {ExpPath, State2} = maybe_expand(State, BootPath),
    State2#{bootloader => ExpPath};
expand_bootloader(State) ->
    State.

expand_system(State = #{system := SysPath})
 when SysPath =/= undefined ->
    {ExpPath, State2} = maybe_expand(State, SysPath),
    State2#{system => ExpPath};
expand_system(State) ->
    State.

create_image(State = #{temp_dir := TempDir}) ->
    Opts = edifa_opts(State, #{temp_dir => TempDir}),
    ImageFile = filename:join(TempDir, "emmc.img"),
    case edifa:create(ImageFile, ?GRISP2_IMAGE_SIZE, Opts) of
        {ok, Pid, State2} ->
            State2#{edifa_pid => Pid};
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

copy_firmware(State = #{edifa_pid := Pid, system := ExpPath}) ->
    Opts = edifa_opts(State, #{
        count => ?GRISP2_SYSTEM_SIZE,
        seek => ?GRISP2_RESERVED_SIZE
    }),
    case edifa:write(Pid, ExpPath, Opts) of
        {ok, State2} -> State2;
        {error, Reason, State2} ->
            event(State2, [{error, Reason}])
    end.

extract_manifest(State = #{edifa_pid := Pid, partitions := [PartId | _]}) ->
    Opts = edifa_opts(State),
    case edifa:mount(Pid, PartId, Opts) of
        {error, Reason, State2} ->
            event(State2, [{error, Reason}]);
        {ok, MountPoint, State2} ->
            ManifestPath = filename:join(MountPoint, "MANIFEST"),
            Manifest = case file:consult(ManifestPath) of
                {error, _Reason} -> undefined;
                {ok, Term} -> Term
            end,
            Opts2 = edifa_opts(State2),
            case edifa:unmount(Pid, PartId, Opts2) of
                {error, Reason, State2} ->
                    event(State2, [{error, Reason}]);
                {ok, State3} ->
                    event(State3#{manifest => Manifest},
                          [{manifest, Manifest}])
            end
    end.

close_image(State = #{edifa_pid := Pid}) ->
    case edifa:close(Pid, edifa_opts(State)) of
        {ok, State2} -> maps:remove(edifa_pid, State2);
        {error, Reason, State2} ->
            event(State2, [{error, Reason}])
    end.

build_package(State = #{package := PackageFile, manifest := Manifest}) ->
    PackagerOpts1 = maps:with([name, version, block_size,
                               key_file, system, bootloader], State),
    PackagerOpts2 = PackagerOpts1#{
        tarball => true,
        mbr => ?GRISP2_PARTITIONS,
        manifest => Manifest
    },
    case grisp_update_packager:package(PackageFile, PackagerOpts2) of
        ok -> event(State, [{done, PackageFile}]);
        {error, Reason} -> event(State, [{error, Reason}])
    end.

cleanup(State) ->
    cleanup_temp_dir(cleanup_image(State)).

cleanup_temp_dir(State = #{temp_dir := TempDir, cleanup_temp_dir := true}) ->
    {_, State2} = shell(State, "rm -rf '~s'", [TempDir]),
    State2#{cleanup_temp_dir => false};
cleanup_temp_dir(State) ->
    State.

cleanup_image(State = #{edifa_pid := Pid}) ->
    case edifa:close(Pid, edifa_opts(State)) of
        {ok, State2} -> maps:remove(edifa_pid, State2);
        {error, _Reason, State2} -> maps:remove(edifa_pid, State2)
    end;
cleanup_image(State) ->
    State.


%--- Internal ------------------------------------------------------------------

edifa_opts(State) ->
    edifa_opts(State, #{}).

edifa_opts(State, Opts) ->
    Opts#{
        log_handler => fun edifa_log_hanler/2,
        log_state => State
    }.

edifa_log_hanler(Event, State) ->
    event(State, [Event]).

shell(State, Fmt, Args) ->
    Cmd = binary_to_list(iolist_to_binary(io_lib:format(Fmt, Args))),
    {{ok, Output}, State2} = grisp_tools_util:shell(State, Cmd),
    {Output, State2}.

is_compressed(Path) ->
    Ext = <<".gz">>,
    case binary:matches(Path, Ext) of
        [{Pos, _Length}] when Pos + byte_size(Ext) =:= byte_size(Path) ->
            NoExt = binary:part(Path, 0, byte_size(Path) - byte_size(Ext)),
            {true, filename:basename(NoExt)};
        _ ->
            {false, filename:basename(Path)}
    end.

maybe_expand(State = #{temp_dir := TempDir}, Path) ->
    case is_compressed(Path) of
        {false, _} -> {Path, State};
        {true, ExpName} ->
            CompName = filename:basename(Path),
            State2 = event(State, [{expanding, CompName}]),
            TempCompPath = filename:join([TempDir, CompName]),
            TempExpPath = filename:join([TempDir, ExpName]),
            {_, State2} = shell(State, "cp '~s' '~s'", [Path, TempDir]),
            {_, State3} = shell(State2, "gunzip '~s'", [TempCompPath]),
            {TempExpPath, State3}
    end.

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
