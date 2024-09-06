-ifndef(GRISP_TOOLS_HRL).
-define(GRISP_TOOLS_HRL, true).

-define(MiB, * 1024 * 1024).
-define(DOCKER_TOOLCHAIN_ROOT, "/grisp2-rtems-toolchain/rtems/5").
-define(GRISP2_BOOTLOADER_FILENAMES, [
    "barebox-phytec-phycore-imx6ull-emmc-512mb.img",
    "barebox-phytec-phycore-imx6ul-emmc-512mb.img"
]).
-define(GRISP2_RESERVED_SIZE, (4?MiB)).
-define(GRISP2_SYSTEM_SIZE, (256?MiB)).
-define(GRISP2_IMAGE_SIZE, (?GRISP2_RESERVED_SIZE + 2 * ?GRISP2_SYSTEM_SIZE)).
-define(GRISP2_PARTITIONS, [
        #{role => system, type => fat32, size => ?GRISP2_SYSTEM_SIZE, start => ?GRISP2_RESERVED_SIZE},
        #{role => system, type => fat32, size => ?GRISP2_SYSTEM_SIZE}
]).
-define(GRISP2_FAT_TYPE, 32).
-define(GRISP2_FAT_CLUSTER_SIZE, 4).


-endif. % GRISP_TOOLS_HRL