----------------------------------------------------------------------------------------------------
-- Sdcard_FAT32
--    Version V1.0 (2021/09/24)
--    (c)2021 J. CHESLET - student at Bordeaux INP / ENSEIRB-MATMECA - Internship at IMS Laboratory
--    This module manages the FAT32 file system of a SD card.
--
-- @copyright
-- SPDX-FileCopyrightText: © 2021 Jeremy Cheslet <jeremy.cheslet@u-bordeaux.fr>
-- SPDX-License-Identifier: GPL-3.0-or-later
----------------------------------------------------------------------------------------------------
-- Working version.
-- TODO list :
--    Handle written file of more than 2GB of data
--    Separate the check and search of file descriptor:
--              - search in one FSM
--              - open in read mode in another FSM
--              - open in write mode in a third FSM
--    Add formating
--    Add erase file
--    Add ls function
--    Optimisation improvements
--              - Multiple read/write for FAT copy  (easy to do & high benefit when ejecting)
--              - Multiple read for FAT check       (not that easy to do with how it is setup :/ & medium benefit when inserting the card)
--              - Multiple read when checking a directory when trying to open/create a file (not that easy to do with how it is setup :/ & low(?) benefit when opening a file/directory)
--
--    Add hard reset input ? (it would just force to set longreset and reset module_fsm) (The user could mutiplex SD_CD and put it to '1' to make a hard reset!!)
--    Remove some Boot Record parameters and don't stop reading FSI sector as these parameters aren't used.
--
--
-- known bugs :
--    - Opening(going in) a directory. It works in simulation and on board ONLY when a fres bitstream is running.
--      Once the card has been ejected, if we insert it again and try to open a directory, everyting seems to go
--      as intended but file are written in the root directory.
--    - Files cannot be created if there is no more space available in the directory. Extending a directory for 1 cluster, i.e. give more file emplacements in the directory,
--      is not yet implemented.
--
--
--
--
-- Unknow behavior: - File with more than 512 fragments (this should be ok)
--                  - What happen when we reach 512 fragments in the free fragment table (this should be ok)
--                  - Does file manager handle well file that have 512 fragments (writing) ?
--                  - Directory creation on board
--                  -
--
--
-------------------------------------------------------------------------------------------------------
-- How to use This module :
-------------------------------------------------------------------------------------------------------
-- 
-- Limited to SD card without much fragmentation. 
-- Recommended to use freshly formated SD card, with 64 or 128 sectors per clusters.
--
-- This module use 1 BRAM + 2*MAX_FILES BRAM with the default configuration. If the FIFO depth increase,
-- the number of BRAM used per file will increase too.
--
-- To read data, first open a file in read mode. If the file is open succesfully, data can be read from the file
-- manager FIFO when it contains some. On his own, the file manager will read the file to feed the file manager
-- FIFO as soon as it cans.
-- Closing a file in read mode takes no operation, so it should be done in 2 clk cycle. But it also could be longer
-- if the file manager is currently reading sectors from the SD card because these operations cannot be canceled.
--
--
-- To write data, first open a file in write mode. To avoid fragmentation, it is recommended to provide a value
-- to reserv_clust when opening the file. A value which imply a fragment of data close (just above) to the file
-- size is great but might not be possible if the partition is already fragmented. If the file is created successfuly,
-- data can be written to the file. This data will be written in the file manager FIFO, which will be written onto the
-- SD card when it contains enough data (defined by generics...).
-- When the file is closed in write mode, it will take some time because it needs to : empty his buffers, write the 
-- chain of used clusters in the FAT and update file information (file size and file first cluster).
--
-- If "data" is being written on a file at the same moment that the user close it (by itself or by eject), the "data"
-- will be written on the file.
--
--
-- Creating a directory doesn't open it, so to create a file in a new directory, the user needs to successfuly create
-- the directory. Then, it has to be open manually. If no problem encounter, the file can finaly be created.
--
--
-- When eject card is demanded, all files data inputs are cut off for the user perpective, as if the user had closed 
-- a file manually. For the module perspective, all remaining opened file are requested to close simultaneously and
-- the module let written file closed one by one until every files are closed. Then, it recopy the FAT 1 into FAT 2
-- to ensure the integrity of the partition. This step might take some time (from seconds to couple of minutes)
-- depending of the card quality.
--
-- in the following diagrams, all signals that are not shown are tied to 0.
--
-- Open a file in read mode:
---------------------------------------------
--                   ____      ___
--   clk           _/    \____/           ...
-- 
--   filename     --< fname # >-------    ...
--
--   file_id      --<   ID    >-------    ...
--
--   read_write   _____________________   ...
--
--   read_write   --< block #  >-------   ...
--
--   read_write   --< block #  >-------   ...
--                    __________
--   open_file     __/          \_______  ...
--                               _______       ___  module ready for new operation
--   busy          _____________/         ...     \__________
--                                                  If file_opened( ID ) = '1' when busy = '0' then the file was successfuly opened.
--
-- Open a file in write mode:
---------------------------------------------
--                   ____      ___
--   clk           _/    \____/             ...
-- 
--   filename     --<   fname #   >-------  ...
--
--   date_time    --< date_time # >-------  ...
--
--   file_id      --<     ID      >-------  ...
--                    __________
--   read_write    __/          \_______    ...
--                    __________
--   open_file     __/          \_______    ...
--
--   reserv_clust --< nb_cluster # >------- ...
--                               _________       ___  module ready for new operation
--   busy          _____________/           ...     \__________
--                                                  If file_opened( ID ) = '1' when busy = '0' then the file was successfuly created.
--
--                                                 
-- Close a file:
--------------------------------------------------
--                   ____      ___
--   clk           _/    \____/           ...
-- 
--   file_id      --<  ID  >-------       ...
--                    __________
--   close_file    __/          \_______  ...
--                               _______       ___  module ready for new operation
--   busy          _____________/         ...     \__________
--
--
-- Close a file and writing data at the same time:
--------------------------------------------------
--                    ____      ____      ____      ____      ____ 
--   clk            _/    \____/    \____/    \____/    \____/    \     ...
-- 
--   file_id       ------------------------<  ID  >-----------------    ...
--                                           __________
--   close_file    _________________________/          \____________    ...
--                    ___________________________________________
--   enable_in     __/                                           \__    ...
--                    _________________________________
--   enable_out    __/                                 \____________    ...
-- 
--   data_in    ... ><  DATA A  ><  DATA B  ><  DATA C  ><  DATA D  ><  ...
--                                                      _______       ___  module ready for new operation
--   busy         _____________________________________/         ...     \__________
--
-- In this exemple, DATA A and B will be written on the SD card.
-- DATA C will be written if the file buffer isn't full just after writing DATA B.
-- To avoid any confusion, close the file at least 1 clk cycle after writing the last data.
--
--
-- Open directory:
---------------------------------------------
--                   ____      ___
--   clk           _/    \____/           ...
-- 
--   filename     --< dirname # >-------  ...
--
--   read_write   _____________________   ...
--                    __________
--   open_dir      __/          \_______  ...
--                               _______       ___  module ready for new operation
--   busy          _____________/         ...     \__________
--                                                  If fat_error = '1' during the opening process, then the directory hasn't been found.
--
-- Create a directory:
---------------------------------------------
--                   ____      ___
--   clk           _/    \____/             ...
-- 
--   filename     --< dirname # >-------    ...
--                    __________
--   read_write    __/          \_______    ...
--                    __________
--   open_dir      __/          \_______    ...
--                               _________       ___  module ready for new operation
--   busy          _____________/           ...     \__________
--                                                  If fat_error = '1' during the opening process, then the directory hasn't been created.
--
--                                                 
-- Close a directory:
--------------------------------------------------
--                   ____      ____      ____     
--   clk           _/    \____/    \____/    \___ ...
--                    __________
--   close_dir     __/          \________________ ...
--                               _________    module ready for new operation
--   busy          _____________/         \_______ 
--
--
----------------------------------------------------------------------------------
-- History
----------------------------------------------------------------------------------
-- V1.0 (2021/09/24)
--    - initial release
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


package data_bus_pkg is
    -- type data_bus is array (natural range <>) of std_logic_vector;

    function log2 (value : integer) return integer;

    type fragment is record 
        cluster : std_logic_vector(27 downto 0);  -- first cluster of the fragment
        size    : unsigned(21 downto 0);          -- nb of consecutive cluster
    end record;
end package;

package body data_bus_pkg is
    function log2 (value : integer) return integer is
        variable nb_bits : integer := 0;
        variable temp    : integer;
    begin
        temp := value;
        while temp /= 0 loop
            temp    := temp / 2;
            nb_bits := nb_bits + 1;
        end loop;

        return nb_bits;
    end function;
end data_bus_pkg;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.data_bus_pkg.all;


-- TODO : - For formating, add a signal to reset some process (e.g. FAT ?)
--        - Add support for a potential root directory starting in another cluster than 2 (does this really happen?)
--        - Implement free_memory to inform the number of free cluster remaining on the SD card
--        - Implement different word size and LITTLE_ENDIAN generic


entity Sdcard_FAT32 is
    Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
            DIRNAME             : string  := "simulation only";   -- for simulation purpose
            VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
            HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
            -- PARAMETER_BUFFERING : boolean := True;                -- if true, the SDcard module keeps its own copy of input parameter SD_block
                                                                  --     setting this generic to false may save slice FFs, but SD_block
                                                                  --     input MUST NOT change as long as read_block, write_block or busy are set.
            LITTLE_ENDIAN       : boolean := True;                -- when multiple bytes per word, tells which byte is stored at lower address
                                                                  --     little endian (True) mean least significant byte first

            MAX_FILES           : integer :=    1;               -- nb of files that can be opened at the same time
            WORD_SIZE_PW2       : integer :=    0;               -- ??
            FIFO_DEPTH          : integer := 4096;               -- maximum size of internal fifo for file reading (4096 means 1 block RAM, i.e. the minimum)
            NB_MULTIPLE_OP      : integer range 4 to 256 := 4;   -- Number of sector read or written with multiple set when processing file operations. This has to be a power of 2, the max is equal or lower than the number of sectors per cluster AND a power of 2 equal(or below) FIFO_DEPTH / 512.
                                                                 -- If the number of sectors per cluster is unknown, use 4 to fit every cards.
            FORMAT_CLUSTER_SIZE : integer range 1 to 8 := 7;     -- power of 2 => size of the cluster (default: 7 => 7 * 512 bytes, i.e. 64KB per clusters)
            TREE_STRUCTURE_MAX  : integer :=    3);              -- maximum sub folder from root directory

    Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
           reset        : in    STD_LOGIC;                                          -- reset, active high

           -- User interface
           filename     : in    STD_LOGIC_VECTOR (87 downto 0);                     -- file name : 8 + '.' + 3
           date_time    : in    STD_LOGIC_VECTOR (39 downto 0) := (others => '0');  -- Creation time, can be ignored
           file_id      : in    STD_LOGIC_VECTOR (log2(MAX_FILES)-1 downto 0);      -- ID (or integer? or...?) operation done on file_ID
           read_write   : in    STD_LOGIC;                                          -- read = '0' ; write = '1'
           open_file    : in    STD_LOGIC;                                          -- try to open the file_name on the stream ID
           file_opened  : out   STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);            -- high if the file has been opened
           file_size    : out   STD_LOGIC_VECTOR (31 downto 0);                     -- return the file size (in bytes) if the file is open for reading
           reserv_clust : in    STD_LOGIC_VECTOR (21 downto 0);                     -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case
           close_file   : in    STD_LOGIC;                                          -- close the file which is opened at ID
           open_dir     : in    STD_LOGIC;                                          -- go into the directory specified in file_name if reading, create it if writing
           close_dir    : in    STD_LOGIC;                                          -- go back to the parent directory (same as cd ../)
        --    erase_file   : in    STD_LOGIC;
           format       : in    STD_LOGIC := '0';                                   -- format the card
           eject_card   : in    STD_LOGIC;                                          -- eject the card : close remaining open files and write/copy FATs, then the card can be removed
           busy         : out   STD_LOGIC;                                          -- the file system is busy

           free_memory  : out   STD_LOGIC;                                          -- Number of free cluster available on the card -- indicate the remaining free memory

           debug        : out   STD_LOGIC_VECTOR ( 7 downto 0);                     -- debug/error data
           fat_error    : out   STD_LOGIC;                                          -- high when an error on the FAT occurs

        --    data_in      : in    data_bus(0 to MAX_FILES-1)(WORD_SIZE-1 downto 0);
        --    data_out     : out   data_bus(0 to MAX_FILES-1)(WORD_SIZE-1 downto 0);
           data_in      : in    STD_LOGIC_VECTOR (MAX_FILES*8*(2**WORD_SIZE_PW2)-1 downto 0);
           data_out     : out   STD_LOGIC_VECTOR (MAX_FILES*8*(2**WORD_SIZE_PW2)-1 downto 0);
           enable_in    : in    STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);            -- tell the module to write data_in / read next data
           enable_out   : out   STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);            -- '1' when data_in can be write / data_out can be read
           
           -- Debug
           debug_addr   : in    STD_LOGIC_VECTOR (15 downto 0);                     -- address of the register to look out
           debug_data   : out   STD_LOGIC_VECTOR (31 downto 0);                     -- the data to look out
           
           SD_DAT       : inout STD_LOGIC_VECTOR ( 3 downto 0);                      -- DATA line for SDcard access
           SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
           SD_WP        : in    STD_LOGIC := '0';                                   -- Write Protect (Not used)
           SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
           SD_CMD       : inout STD_LOGIC);                                         -- Command line
end Sdcard_FAT32;

architecture Behavioral of Sdcard_FAT32 is
    -------------------------------------------------------------------------
    --
    --                      Debug signals
    --
    -------------------------------------------------------------------------
    constant NOTHING_TO_SAY : std_logic_vector(7 downto 0) := "00000000";

    -- INFORMATION CODES
    constant SDCARD_EJECTING : std_logic_vector(7 downto 0) := "00100001"; 
    constant SDCARD_EJECTED  : std_logic_vector(7 downto 0) := "11100111";  -- SDcard can be safely removed

    -- WARNING CODES
    constant TOO_MANY_FRAG : std_logic_vector(7 downto 0) := "01000001";   -- file too much fragmented, some data will be skipped
    -- card ejected whereas data was still being written / have to be written
    -- not much space available
    -- no space avaible

    -- ERROR CODES
    constant NO_FILE_FOUND           : std_logic_vector(7 downto 0) := "10000001";   -- no file
    constant FILE_ALREADY_EXIST      : std_logic_vector(7 downto 0) := "10000010";   -- file already exist
    constant NO_DIRECTORY_FOUND      : std_logic_vector(7 downto 0) := "10001001";   -- directory doesn't exist in the current directory
    constant DIRECTORY_ALREADY_EXIST : std_logic_vector(7 downto 0) := "10001010";   -- directory already exist
    constant TREE_STRUCTURE_FULL     : std_logic_vector(7 downto 0) := "10001011";   -- cannot open another directory because the maximum is already reached
    constant TREE_STRUCTURE_ROOT     : std_logic_vector(7 downto 0) := "10001100";   -- cannot close the current directory since its root
    -- stream already used, cannot open this file on this stream
    -- Boot record missing parameters ?
    -- Boot record issues ? wrong values
    -- difference between fat (put it as a warning but need to redo some code to ignore other FAT and work with FAT 1, and then recopy it to others FAT at the end)
    -- writing is not possible, no space available


    signal SD_state       : std_logic_vector(31 downto 0);  -- current SDcard action (idle, writing,...)
    signal module_state   : std_logic_vector(31 downto 0);  -- current module state  (idle, boot_record,...)
    signal BR_state       : std_logic_vector(31 downto 0);  -- current state of the Boot Record fsm
    signal BR_state_err   : std_logic_vector(31 downto 0);  -- state of Boot Record fsm that raise an issue
    signal FAT_state      : std_logic_vector(31 downto 0);  -- current state of FAT_fsm
    signal frag_state     : std_logic_vector(31 downto 0);  -- current state of frag_fsm
    signal check_fd_state : std_logic_vector(31 downto 0);  -- current state of check_fd_fsm
    signal info_state     : std_logic_vector(31 downto 0);  -- current state of info_fsm
    signal SD_manag_state : std_logic_vector(31 downto 0);  -- current state of SDm_fsm
    signal SD_op_state    : std_logic_vector(31 downto 0);  -- current state of SD_op_fsm

    -------------------------------------------------------------------------
    --
    --                   Signals used to manage SDcard
    --
    -------------------------------------------------------------------------
    constant SD_WORD_SIZE_PW2     : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
    constant BUFFER_NUM_PW2      : integer range 1 to 8 := 3;      -- codes the number of 512B data buffers for transfers, actual value will be 2^BUFFER_NUM_PW2
    constant SD_write_buffer     : std_logic_vector(2 downto 0) := "000"; -- might be not used.
    constant SD_read_buffer      : std_logic_vector(2 downto 0) := "010"; -- might be not used.
    constant SD_FAT_buffer       : std_logic_vector(2 downto 0) := "100"; -- might be not used.
    constant SD_dir_buffer       : std_logic_vector(2 downto 0) := "110"; -- might be not used.

    -------------------------------------------------------------------------
    -- Signals used to control the SDcard
    -------------------------------------------------------------------------
    signal SD_block_num          : std_logic_vector(31 downto 0);                -- number of the SDblock on which we perform the next o
    signal SD_op_buff            : std_logic_vector(BUFFER_NUM_PW2-1 downto 0);  -- the buffer on which we perform the operation
    signal SD_read_blk           : std_logic;                                    -- SDcard read order
    signal SD_write_blk          : std_logic;                                    -- SDcard write order
    signal SD_erase_blk          : std_logic;                                    -- SDcard write order
    signal SD_multiple           : std_logic;                                    -- multiple block operation
    -- signal SD_prep_blk           : std_logic;
    signal SD_busy               : std_logic;                                    -- SDcard module is busy
    signal SD_err_code           : std_logic_vector( 3 downto 0);                -- error output
    signal SD_nb_blocks          : std_logic_vector(31 downto 0);                -- The number of blocks available on the SDcard

    signal SD_buffer   : std_logic_vector(BUFFER_NUM_PW2-1 downto 0);       -- buffer for local access
    signal SD_address  : std_logic_vector(8-SD_WORD_SIZE_PW2  downto 0);       -- data address in the buffer
    signal SD_write    : std_logic;                                         -- data write to buffer
    signal SD_data_in  : std_logic_vector(8*(2**SD_WORD_SIZE_PW2)-1 downto 0); -- data to be written to buffer
    signal SD_data_out : std_logic_vector(8*(2**SD_WORD_SIZE_PW2)-1 downto 0); -- data read from buffer

    signal synced_CD             : std_logic;                      -- synchronized version of SD_CD
    signal synced_WP             : std_logic;                      -- synchronized version of SD_WP
    signal SDcard_reset          : std_logic;                      -- reset sent to the SDcard manager


    -------------------------------------------------------------------------
    -- Signals used to control the module globally
    -------------------------------------------------------------------------
    type t_module_fsm is (reseting,         -- reset the whole module
                          no_SD_card,       -- waiting an SDcard in SDcard slot
                          initializing,     -- SDcard is initializing
                          boot_record,      -- read Boot Record
                          fat,              -- read and check fats, and fill free clusters fifo
                          idle,             -- SDcard is ready for a new operation
                          open_fd,          -- open a file or a directory (read or write)
                          close_fd,         -- close a file (read or write)
                          erase_fd,         -- erase a file
                          close_directory,  -- close a directory
                          close_files,      -- Begin ejection by closing all files
                          recopy_FAT,       -- recopy the used FAT into other FATs
                          ejected,          -- SDcard is rejected
                          formating,        -- format the card to FAT32 format
                          format_problem);  -- SDcard format is not recognised / Boot Record issue or because of some differences between FATs

    signal module_fsm : t_module_fsm;
    signal longreset  : std_logic;    -- signal used to reset the module

    signal formating_done : std_logic; -- temp signal
    signal erase_file_ok : std_logic; -- temp signal

    signal busy_i : std_logic;  -- internal busy state of the module

    signal user_admin_op : std_logic;    -- user requiere an operation on the SDcard (eject or format)
    signal user_file_op  : std_logic;    -- user requiere an operation on a file
    signal user_op_bsy   : std_logic;    -- the module is processing a user operation, thus file (except the one focused by the user) cannot do operation until its done

    -- These are inputs buffer
    signal open_file_buff    : std_logic;
    signal close_file_buff   : std_logic;
    -- signal erase_file_in_reg   : std_logic; -- not implemented
    signal open_dir_buff     : std_logic;
    signal close_dir_buff    : std_logic;
    signal file_id_in_reg      : std_logic_vector(log2(MAX_FILES)-1 downto 0);
    signal read_write_in_reg   : std_logic;
    signal filename_in_reg     : std_logic_vector(87 downto 0);
    signal date_time_in_reg    : std_logic_vector(39 downto 0);
    signal reserv_clust_in_reg : std_logic_vector(21 downto 0);
    signal eject_in_reg        : std_logic;
    signal reset_in_reg        : std_logic;
    signal format_in_reg       : std_logic;

    signal not_a_file_op : std_logic;   -- indicate if the module enable file operation

    signal start_eject     : std_logic; -- impulse to start eject
    signal ejecting_SDcard : std_logic; -- set when SDcard is ejecting
    signal eject_busy      : std_logic;

    signal load_file_to_close  : std_logic;                                 -- set when file opened state are loaded in the shift register
    signal file_to_close_shift : std_logic_vector(MAX_FILES-1 downto 0);
    signal file_to_close_id    : integer range 0 to MAX_FILES-1;
    signal file_to_close_found : std_logic;
    signal all_file_closed     : boolean;

    -------------------------------------------------------------------------
    --
    --                      Signals used for FAT32
    --
    -------------------------------------------------------------------------
    -------------------------------------------------------------------------
    -- Signals used to read and analyse the Boot Record
    -------------------------------------------------------------------------
    type t_BR_fsm is (off,                      -- idle state, not started yet
                      read_BR,                  -- command SDcard to read the boot record sector, i.e. the first sector of the SDcard
                      analyse_BR,               -- Check and get Boot Record infos/constants/specs
                      get_jump_code,            -- jump code + NOP => FAT32 identifier
                      get_OEM_name,             -- OEM_name, should be useless (except for formating?)
                      get_bytes_per_sector,     -- bytes per sector (has to be 0x0200)
                      get_sectors_per_cluster,  -- sectors per cluster (power of 2) 
                      get_reserved_sectors,     -- number of sectors before the first FAT
                      get_nb_FAT,               -- number of FAT, should be 2
                      get_media_descriptor,     -- media descriptor, should be 0xF8
                      get_sectors_per_track,    -- sectors per track, useless ?
                      get_nb_heads,             -- number of heads, useless ?
                      get_nb_hidden_sectors,    -- number of hidden sector in the partition (should be 0 or 0x200)
                      get_nb_sectors_partition, -- number of sectors in the partition
                      get_nb_sectors_per_FAT,   -- number of sectors per FAT
                      get_active_FAT,           -- number of active FAT
                      get_root_dir_cluster,     -- cluster number of the start of the Root directory (should be 2)
                      get_FSI_sector,           -- useless if we do not check it
                      get_backup_boot_sector,   -- nothing ? -> useless ?
                      get_logical_drive_nb,     -- useless ?
                      get_extended_signature,   -- just check ? If 0x28, don't check volume name and FAT_name
                      get_serial_nb,            -- useless ?
                      get_volume_name,          -- might be useless (just for formating?)
                      get_FAT_name,             -- we should find "FAT32   "
                      get_boot_signature,       -- 0xAA55
                      read_FSI,                 -- command SDcard to read the File System Information sector (opt.)
                      analyse_FSI,              -- check and get number of free cluster and "first" free clusters
                      get_nb_free_cluster,      -- last known number of free data clusters on the volume, or 0xFFFFFFFF if unknown.
                      get_recent_cluster,       -- number of the most recently known to be allocated data cluster. Should be set to 0xFFFFFFFF after formating, and means to start at cluster 0x02
                      final_check,              -- check nb_free_cluster and recent_cluster and see if their are usable
                      BR_issue,                 -- error found in the Boot Record
                      BR_end);                  -- end state of the Boot Record
    signal BR_fsm        : t_BR_fsm;
    signal BR_error      : std_logic;                                   -- high if an unknow value occurs
    signal BR_bytes_cnt  : integer range 0 to 10;                       -- cnt bytes to fill values/parameters from SDcard module data                  
    signal BR_block_num  : std_logic_vector(31 downto 0);               -- block number to read for Boot Record info (sector 0 and sector FSI)
    signal BR_loc_buffer : std_logic_vector(BUFFER_NUM_PW2-1 downto 0); -- local SD module buffer : 010 for Boot Record, 011 for FSI
    signal BR_address    : unsigned(8-SD_WORD_SIZE_PW2 downto 0);          -- address in the above buffer, to fetch one of the 512 bytes

    constant identifier          : std_logic_vector(23 downto 0) := x"9058EB"; -- 0x0000 (3 bytes): Jump Code + NOP
    constant bytes_per_sector_th : std_logic_vector(15 downto 0) := x"0200";   -- 0x000B (2 bytes): Bytes Per Sector
    constant FAT_name_th         : std_logic_vector(63 downto 0) := x"2020203233544146"; -- 0x0052 (8 bytes): FAT Name (should be "FAT32   ")
    constant boot_signature_th   : std_logic_vector(15 downto 0) := x"AA55";   -- 0x01FE (2 bytes): Boot Signature = 0xAA55

    signal jump_code_NOP                : std_logic_vector(23 downto 0);   -- 0x0000 ( 3 bytes): Jump Code + NOP
    signal OEM_name                     : std_logic_vector(63 downto 0);   -- 0x0003 ( 8 bytes): OEM Name, should be useless ?
    signal bytes_per_sector             : std_logic_vector(15 downto 0);   -- 0x000B ( 2 bytes): Bytes Per Sector
    signal sectors_per_cluster          : std_logic_vector( 7 downto 0);   -- 0x000D ( 1 byte ): Sectors Per Cluster (power of 2)
    signal reserved_sectors             : std_logic_vector(15 downto 0);   -- 0x000E ( 2 bytes): Count of reserved logical sectors, the number of sectors before the first FAT
    signal nb_FAT                       : std_logic_vector( 7 downto 0);   -- 0x0010 ( 1 byte ): Number of copies of FAT
    signal media_descriptor             : std_logic_vector( 7 downto 0);   -- 0x0015 ( 1 byte ): Media descriptor, should be 0xF8 ?
    signal sectors_per_track            : std_logic_vector(15 downto 0);   -- 0x0018 ( 2 bytes): Physical sectors per tracks, might be useless ?
    signal nb_heads                     : std_logic_vector(15 downto 0);   -- 0x001A ( 2 bytes): Number of Heads for disks, should be useless ?
    signal nb_hidden_sectors            : std_logic_vector(31 downto 0);   -- 0x001C ( 4 bytes): Count of hidden sectors preceding the partition that contains this FAT volume, should be 0, and should be useless
    signal nb_sectors_in_partition      : std_logic_vector(31 downto 0);   -- 0x0020 ( 4 bytes): Total logical sectors, not enough byte to encode this ??
    signal nb_sectors_per_FAT           : std_logic_vector(31 downto 0);   -- 0x0024 ( 4 bytes): Number of sectors per FAT
    signal active_FAT                   : std_logic_vector(15 downto 0);   -- 0x0028 ( 2 bytes): Number of active FAT, might be useless ?
    signal root_directory_cluster       : std_logic_vector(31 downto 0);   -- 0x002C ( 4 bytes): Cluster number of the start of the Root directory, should be 0x02
    signal sector_nb_file_system_info   : std_logic_vector(15 downto 0);   -- 0x0030 ( 2 bytes): Sector numer of the File System Information sector, might be useless if we doesn't check it
    signal sector_nb_backup_boot_sector : std_logic_vector(15 downto 0);   -- 0x0032 ( 2 bytes): Sector number of the backup boot sector, should be useless and it shouldn't exist ?
    signal logical_drive_nb             : std_logic_vector( 7 downto 0);   -- 0x0040 ( 1 byte ): Logical drive number of partition, should be useless ?
    signal extended_signature           : std_logic_vector( 7 downto 0);   -- 0x0042 ( 1 byte ): Extended signature, should be 0x29. 0x28 means no volume name and no FAT name
    signal serial_nb_of_partition       : std_logic_vector(31 downto 0);   -- 0x0043 ( 4 bytes): Serial number of the partion, should be useless ?
    signal volume_name_of_partition     : std_logic_vector(87 downto 0);   -- 0x0047 (11 bytes): Volume name of the partition
    signal FAT_name                     : std_logic_vector(63 downto 0);   -- 0x0052 ( 8 bytes): FAT Name (should be "FAT32   ")
    signal boot_signature               : std_logic_vector(15 downto 0);   -- 0x01FE ( 2 bytes): Boot Signature = 0xAA55

    -- Check signatures ?
    signal BR_nb_free_cluster           : std_logic_vector(31 downto 0);   -- 0x01E8 ( 4 bytes): number of free cluster, if unknown: 0xFFFFFFFF
    signal recent_cluster               : std_logic_vector(31 downto 0);   -- 0x01EC ( 4 bytes): indicates the cluster number where we should start looking for free cluster 
    -- Check trailing signature ?

    signal total_size_of_FATs           : unsigned(31 downto 0);        -- store the number of sectors use by all the FATs
    signal nb_FAT_cpy                   : unsigned( 7 downto 0);        -- copy of the number of fat. Used to use addition instead of multiplication (and a DSP)
    signal nb_total_cluster             : unsigned(31 downto 0);        -- number of cluster available on the FAT, according to the Boot Record
    signal sector_per_cluster_cpy       : unsigned( 7 downto 0);        -- copy of the boot record parameter, which will be used to shift FAT_nb_cluster_in_FAT to the right
    signal data_area_begin_sector       : unsigned(31 downto 0);        -- this signal store the sector of the first data region cluster, thus the Root directory in the "very usual" case where root_directory_cluster=2
    signal root_sector_buffer           : unsigned(31 downto 0);        -- temporary buffer used to calculate the number of sectors (offset) where the root directory begin, after the data area begin (used to save a DSP)
    signal root_directory_sector        : unsigned(31 downto 0);        -- store the root directory first sector
    --signal nb_free_cluster              : integer range 0 to 2**28-1;  -- total number of free cluster in the SDcard. If 0, no write is possible


    -------------------------------------------------------------------------
    -- Signals used to read and analyse the FAT
    -------------------------------------------------------------------------
    constant MAX_NB_CLUSTER_PW2         : integer := 28;        -- numb
    CONSTANT MAX_NB_CONSECUTIVE_CLUSTER_PW2 : integer := 28;        --
    constant FREE_FRAG_SIZE             : integer := MAX_NB_CLUSTER_PW2+MAX_NB_CONSECUTIVE_CLUSTER_PW2;     -- info stored to store clusters (28 MSB: nb of consecutive ; 28 LSB: address of the first cluster of the chain)


    type t_FAT_fsm is (off,                 -- startup state, waiting to read and check FATs
                       read_fat_1,          -- read a sector from the first FAT
                       read_fat_1_wait,     -- wait the end of reading
                       read_fat_k,          -- read the "same" sector from another FAT (the second usually)
                       read_fat_k_wait,     -- wait the end of reading
                       start_check,         -- start the comparison, and meanwhile, start another read instruction
                       waiting_check,       -- wait the end of check, or read a new fat if possible
                       read_whole_FAT,      -- in case of just 1 FAT, we read the whole FAT to find free cluster segments
                       FAT_check_done,      -- state to reset free fragment ptr
                       idle,                -- idle state, waiting for an interaction with the FAT
                       frag_requested,      -- give a fragment to a file manager and subtract the fragment size to the current fragment
                       update_frag,         -- write back the fragment, and increment read ptr if fragment as been completly eaten (also serve as buffer state to let time to the file manager to get the acknoledge)
                       
                    --    paste_FAT_1,         -- copy FAT 1 and paste it on other FAT at the end of operations, when eject is call
                       FAT_issue,           -- go in this state if their is differences between FATs
                       FAT_end);            -- FAT manipulation is over, no change can happen

    signal FAT_fsm               : t_FAT_fsm;
    signal FAT_block_num         : unsigned(31 downto 0);                       -- sector focus in the FAT (should be superior to reserved_sectors and inferior to reserved_sector + nb_sectors_per_FAT (for 1 FAT)
    signal FAT_block_num_1       : unsigned(31 downto 0);                       -- sector focus in the FAT 1
    signal FAT_block_num_k       : unsigned(31 downto 0);                       -- sector focus in the FAT k
    signal FAT_op_buff           : std_logic_vector(BUFFER_NUM_PW2-1 downto 0); -- SDcard buffer to use to read/write with the FAT. It should be "100" or "101" (might also take read or directory buffers during the boot init.)
    signal FAT_loc_buffer        : std_logic_vector(BUFFER_NUM_PW2-1 downto 0); -- buffer to use in SDcard module. It should be "100" or "101" (might also take read or directory buffers during the boot init.)
    signal FAT_address           : unsigned(8-SD_WORD_SIZE_PW2 downto 0);          -- address in the above buffer
    signal FAT_multiple          : std_logic;                                   -- enable multiple read for consecutive sectors, during initialisation.
    
    -------------------------------------------------------------------------
    -- Signals used to check FATs
    -------------------------------------------------------------------------
    signal FAT_check_last_addr   : std_logic;                                           -- Use to check the last byte of the buffer
    signal FAT_nb_fat_check      : unsigned(7 downto 0) := x"01";                       -- count the number of FAT that has been checked and validated
    signal FAT_check_sector_done : std_logic;                                           -- high when a sector has been checked, this means the buffer can be flushed
    signal FAT_all_sectors_read  : std_logic;                                           -- high when module reads every sectors of FATs, and enable next state of FAT_fsm
    signal FAT_difference        : std_logic;                                           -- high when a difference is read between FAT 1 and FAT K (K=[2;nb_FAT])
    signal FAT_read_buffer       : std_logic;                                           -- start the pipeline for checking differences in FATs' sectors
    signal FAT_1_buffer          : std_logic_vector(8*(2**SD_WORD_SIZE_PW2)-1 downto 0);   -- Temporary buffer to compare FATs


    -- This FSM is used to recopy the first FAT of the FS in the 2nd FAT
    -- TODO : - Add a signal which says if something as been written on SDcard (i.e. if the signals SD_writre_blk has been once), so 
    --          it can skip FAT recopy and save ejecting time
    type t_recopy_FAT_fsm is (off,         -- waiting the module to eject the card
                              read_FAT,    -- read a sector from the FAT used by this module (ie the first FAT)
                              wait_read,   -- wait the sector to be read
                              write_FAT,   -- write on the SDcard the 'just read' sector
                              wait_write,  -- wait the write operation to end
                              recopy_end); -- notify the end of the recopy
    signal recopy_FAT_fsm : t_recopy_FAT_fsm;

    signal no_written_op : boolean;     -- this signal watch out for write operation on SD card to know if the FAT has to be recopied when ejecting the SDcard
    -------------------------------------------------------------------------
    -- Signals used to get free clusters (chains) in FAT
    -------------------------------------------------------------------------
    -- TODO : - Rename "free clusters chains" in "fragments" or something more relevant
    -- Store cluster
    constant NB_BRAM_FREE_FRAGMENT_TABLE : integer range 1 to 32 := 1;  -- TODO : put this constant as generic
    constant FREE_FRAGMENT_TABLE_SIZE    : integer := 512 * NB_BRAM_FREE_FRAGMENT_TABLE;    -- size of the memory which store free fragment
    type t_free_cluster_table is array (0 to 511) of std_logic_vector(FREE_FRAG_SIZE-1 downto 0);   -- this table will give free clusters for writing purpose, sorted with the most consecutive free clusters 
                                                                                                        -- to the lowest chain available (that can be stored in the array).
                                                                                                        -- If size of two free clusters chains is the same, the first to be selected will be the one with the lowest id.
                                                                                                        -- A chain of free clusters must be emptyed to "output" a new chain.
    signal free_frag_table     : t_free_cluster_table;
    signal free_frag_read_ptr  : integer range 0 to FREE_FRAGMENT_TABLE_SIZE-1;     -- pointer to elements to read in the free fragment table
    signal free_frag_write_ptr : integer range 0 to FREE_FRAGMENT_TABLE_SIZE-1;     -- pointer to elements to write in the free fragment table
    signal free_frag_write_en  : std_logic;                                         -- set to write a value in the table

    signal free_frag_table_size : integer range 0 to FREE_FRAGMENT_TABLE_SIZE;      -- current size used in the table
    signal free_frag_table_full : boolean;                                          -- set when the free fragment table is full of element (and so flush the sortest fragment when inserting a new one)
    signal free_frag_write_dly  : std_logic;                                        -- used to delay writing and write ptr update for 1 clk cycle when the tanle is full (i.e. avoid 'out of range')

    signal free_frag_read       : std_logic_vector(FREE_FRAG_SIZE-1 downto 0);      -- output register of the fragments table, at the read pointer
    signal free_frag_to_write   : std_logic_vector(FREE_FRAG_SIZE-1 downto 0);      -- register to write in the fragments table, at the write pointer
                                                                                    -- when scouting for free fragment, this register takes the shortest fragment
                                                                                    -- between the fragment read in the table and the new fragment to store

    signal store_new_fragment     : std_logic;                                      -- set when we need to store a new fragment
    signal fragment_ordering      : std_logic;                                      -- set when the new segment has to be inserted and when the table is ordering
    signal new_free_frag          : std_logic_vector(FREE_FRAG_SIZE-1 downto 0);    -- buffer for the new free fragment to insert
    signal new_free_frag_inserted : std_logic;                                      -- set when the new fragment has been inserted, to stop the ordering



    signal FAT_free_clusters_start    : std_logic_vector(27 downto 0);                    -- ID of the starting clusters of a chain
    signal FAT_free_clusters_cnt      : unsigned(27 downto 0);                            -- cnt consecutive free clusters following the FAT_free_clusters_starts
    signal FAT_nb_free_clusters       : integer range 0 to 2**28-1;                       -- number of free cluster available (to compare with the value read in the Boot Record)
    signal FAT_chain_to_store         : std_logic_vector(FREE_FRAG_SIZE-1 downto 0);  -- nb of consecutive clusters + cluster's ID to store in the ordered table
    signal FAT_new_chain_to_store     : std_logic;                                        -- high when we need to store a chain, starting the insertion of the chain
    signal FAT_store_last_chain       : std_logic;                                        -- use to not store multiple time the last possible chain of the FAT, if it exist

    -- Get clusters
    signal FAT_cluster           : std_logic_vector(31 downto 0);     -- this signal store the value of the cluster that we construct from byte
    signal FAT_bytes_cnt         : integer range 0 to 3;              -- sub counter to know when a new cluster is available
    signal FAT_cluster_cnt       : unsigned(27 downto 0);             -- cnt the number of the cluster studied
    signal FAT_cluster_cnt_dly   : std_logic;                         -- 1 clk cycle delay at the beginning to have cluster 0 as the first cluster (and so cluster 2 as the "root cluster")
    signal FAT_new_cluster       : std_logic;                         -- high when a new cluster is avaible for a test


    -------------------------------------------------------------------------
    -- Signals used to get free fragments from free clusers found precedently
    -------------------------------------------------------------------------
    signal current_frag_cluster : unsigned(27 downto 0);    -- current starting cluster of the fragment read by free_frag_read_ptr (i.e. the current top fragment)
    signal current_frag_size    : unsigned(27 downto 0);    -- current size of the fragment read by free_frag_read_ptr             (i.e. the current top fragment)
    signal request_frag         : std_logic;                -- set when a fragment has been requested
    signal request_frag_size    : unsigned(21 downto 0);    -- 31(2GB) - 9(512 Bytes per sector) = 22 : 2^22 sectors maximum for a file
    signal frag_out             : fragment;


    -------------------------------------------------------------------------
    --
    --                      Write fragment in FAT (cluster chain)
    --
    -------------------------------------------------------------------------
    -- These signals are used to update used clusters in the FAT when closing a file which has written data onto the SDcard.
    -- It gets a fragment of used cluster from the file manager and update the cluster chain. The process is over when the file
    -- manager gives the null fragment ("000....0"), then the FSM understands that the chain of used clusters is complete and then exit.
    type t_frag_fsm is (off,            -- idle
                        get_fragment,   -- get the next fragment from the file manager
                        is_over,        -- check if the fragment is null. If it's the case, then every used clusters have been updated and so exit the fsm after writing back the sector
                        read_sector,    -- read the sector in the FAT where the last cluster of the current fragment is located
                        wait_read,      -- wait the read op to end
                        write_fragment, -- update the used cluster chain
                        write_sector,   -- write back the sector to the SDcard
                        wait_write);    -- wait the end of the writing
    signal frag_fsm : t_frag_fsm;
    signal frag_start_cluster : std_logic_vector(27 downto 0);  -- starting cluster of the fragment
    signal frag_size          : unsigned(21 downto 0);          -- size of the fragment (decreased each time a cluster is being updated)
    signal frag_block_num     : std_logic_vector(31 downto 0);  -- sector to operate
    signal frag_buff_addr     : std_logic_vector( 8 downto 0);  -- address of the cluster in the sector(/buffer)
    signal frag_bytes_cnt     : integer range 0 to 3;           -- count bytes written in the buffer
    signal frag_cluster_nb    : unsigned(27 downto 0);          -- current cluster to update
    signal frag_next_cluster  : std_logic_vector(31 downto 0);  -- next cluster in the cluster chain, to attribute at cluster_nb
    signal frag_next_cluster_B: std_logic_vector( 7 downto 0);  -- byte to write (LSB of the next cluster)

    signal frag_out_of_sector : boolean;    -- check if the (next) cluster to update is out of the sector. If it the case, then the current sector is written back, 
                                            -- the next (actually previous) sector is read, and then the clusters is updated 
    signal frag_is_the_first  : boolean;    -- true when we're dealing with the first fragment to write on the SDcard.
                                            -- It serves frag_cluster_nb 'initial' value (in each new sector), so we know if we have to do -1 to the cluster
                                            -- value in case it is this fragment which has written the end of chain (0FFFFFFF). (messy explaination :x)
    signal frag_is_first_dly  : std_logic;  -- delay of one fragment

    -------------------------------------------------------------------------
    --
    --       Signals used to check files existence and fetch clusters
    --
    -------------------------------------------------------------------------
    -- This FSM searched the file which is ask to be open or created.
    --   It starts by searching if a file with the given filename exist in the current directory.
    --   It starts by looking at the first sector of the directory, and check every file descriptor.
    --     For each file descriptor, it compares filename. If filenames correspond, it check if this file is
    --     a directory or an archive. If they don't correspond, it fetchs the next file, fetch the next sector
    --     if necessary, the next cluster if necessary, until the directory ends.
    --   If the user intends to read the file, if it exists (and if it is an archive), we now collect
    --     the starting cluster and the size of the file. Then we list every clusters that point to the file.
    --   If the user inends to write in the file, if it doesn't exist, then we load the sector where the 
    --     file descriptor can be writed (it is determined during the existence check) and write the name and
    --     type of the new file.
    type t_check_fd_fsm is (off,                  -- no file is getting checked
                            load_directory,       -- load the (first) sector of the current directory
                            wait_directory,       -- wait the sector to be read
                            start_check,          -- buffer state to enable pipeline 
                            check_filename,       -- test the filename to know if it could be this file
                            -- get_attribute,      -- get the type of file (directory or archive)
                            check_attribute,      -- 
                            next_file,            -- increment the file pointer
                            next_sector,          -- increment the sector in the cluster
                            read_dir_fat,         -- find the next cluster to read by reading the fat
                            wait_dir_fat,         -- wait the sector to be read
                            get_next_cluster,     -- read the sector (of the fat)
                            next_cluster,         -- analyse the sector value and start to calculate the sector value if not end of chain
                            calc_next_cluster,    -- wait the next sector to read with the new cluster
                            end_of_dir,           -- no more cluster to read, i.e. the file has not been found
                            -- read
                            get_start_cluster,    -- save first cluster
                            get_size,             -- save size of the file
                            is_file_empty,        -- buffer state to check if the file is empty or not. If it is, check is over
                            read_fat,             -- read the sector in the fat which has the next cluster
                            wait_fat,             -- wait sector to be fully read
                            list_clusters,        -- put file's clusters in fifo until end (or max) is reached
                            list_clust_dly,       -- dly to get the cluster value and analyse it at the next state
                            is_list_done,         -- test the value of the cluster read to continue or to stop process
                            -- write
                            read_dir_sector,      -- read the sector where the file can be written
                            wait_dir_sector,      -- load the sector where file could be written
                            append_directory,     -- directory is full so we add 1 cluster to the directory chain
                            create_dir,           -- Create or append a directory 
                            create_file,          -- write the file descriptor, i.e. the filename
                            write_sector_back,    -- write back the sector in the directory
                            wait_write_end,       -- wait the end of writing operation to have the module ready for next op.
                            -- directory
                            open_directory,       -- wait the calculation of the first sector of the directory 'just' opened
                            -- end
                            check_over);          -- over 
    signal check_fd_fsm : t_check_fd_fsm;
    signal free_fd_found   : std_logic;                     -- when set, stop looking for a free file descriptor that the new file could use
    signal free_fd_sector  : std_logic_vector(31 downto 0); -- sector where the free fd has been found
    signal free_fd_address : std_logic_vector( 3 downto 0); -- address of the free fd in the sector (*32 bytes)
    signal FD_data         : std_logic_vector( 7 downto 0); -- buffer for the data to write in SD module buffers
    signal FD_data_buffer  : std_logic_vector(79 downto 0); -- shift register to put data in SD module buffer (for file's entry)

    signal FD_bytes_cnt       : integer range 0 to 31;         -- counter for bytes read in a file descriptor
    signal FD_loc_buffer      : std_logic_vector( 2 downto 0); -- SDcard module local buffer to address
    signal FD_intern_addr     : unsigned( 4 downto 0);         -- used to address bytes in the file descriptor
    signal FD_fd_addr         : unsigned( 3 downto 0);         -- used to address file descriptor in the sector
    signal FD_buff_addr       : std_logic_vector( 8 downto 0); -- concatenation of the two above signals, to address bytes in the SDcard buffer
    signal FD_block_num       : unsigned(31 downto 0);         -- sector to point for SDcard operation
    signal FD_sector_in_clust : unsigned( 7 downto 0);         -- used to address a sector in the cluster
    signal FD_current_cluster : std_logic_vector(27 downto 0); -- number of the current cluster
    signal FD_next_cluster    : std_logic_vector(27 downto 0); -- number of the next cluster to read, the next part of the directory

    signal FD_name_diff   : boolean;                       -- true if names are different
    signal FD_name        : std_logic_vector(87 downto 0); -- filename
    signal FD_name_buffer : std_logic_vector(87 downto 0); -- filename buffer
    alias FD_attribute    : std_logic_vector( 7 downto 0) is FD_name_buffer(87 downto 80);
    alias FD_start_clust  : std_logic_vector(31 downto 0) is FD_name_buffer(63 downto 32);  -- we use 32 bits instead of 28 bits here because it doesn't really waste bits since the
                                                                                            -- 88-bit signal already exist, but it may save LUT by reducing (affectation) complexity
    alias FD_size         : std_logic_vector(31 downto 0) is FD_name_buffer(31 downto  0);
    signal FD_word_buffer : std_logic_vector( 7 downto 0); -- buffer to compare file descriptor byte per byte
    signal FD_rw_mode     : std_logic;                     -- open mode : '0' => read, '1' => write
    signal FD_file_type   : std_logic;                     -- File to open : '0' => archive, '1' => directory
    signal reserv_clust_i : std_logic_vector(21 downto 0); -- buffer for reserv_clust (module input port) to pass it to file manager
    signal date_time_buff : std_logic_vector(39 downto 0);      -- buffer to store date_time into file descriptor
    
    signal diff_sector    : std_logic;  -- tell check_fd_fsm if the next cluster is in the same fat sector or not
    
    signal FD_op_success  : std_logic;  -- tell to the design if the open/create operation is a success

    signal FD_fragment           : fragment;
    signal FD_nb_frag            : integer range 0 to 512;   -- cnt the number of fragment sent. Used to not send more than the maximum storable
    signal FD_cant_list_all_frag : std_logic;
    signal FD_last_fragment      : std_logic;   -- set when the new fragment to transfer
    signal FD_new_fragment       : std_logic;   -- set when a new fragment can be transfered
    signal FD_completed_fragment : fragment;


    -----------------------------------------------------------------
    --  Create or append a directory
    -----------------------------------------------------------------
    type t_create_dir_fsm is (off,              -- off
                              get_1_cluster,    -- reserve a free cluster
                              calcul_sector,    -- calculate the sector from the cluster nb
                              clear_buffer,     -- clear a whole SDcard raw access buffer
                              write_sector,     -- write the cleared buffer to the new directory"s sectors
                              wait_write,       -- wait write
                              read_FAT,         -- read FAT sector where the cluster status is stored
                              wait_FAT_read,    -- wait tthe read operation
                              update_cluster,   -- update the cluster chain (if append && in the same sector, do it here too)
                              write_FAT,        -- write back FAT
                              wait_FAT_write,   -- wait write (APPEND: go to read_FAT again where the last cluster of the directory to append is)
                              done);            -- operation over
    signal create_dir_fsm     : t_create_dir_fsm;
    signal c_dir_bytes_cnt    : integer range 0 to 511;
    signal c_dir_multiple     : std_logic;
    signal c_dir_multiple_cnt : integer range 0 to 512;
    signal c_dir_cluster_nb   : std_logic_vector(27 downto 0);
    signal c_dir_sector       : std_logic_vector(31 downto 0);
    signal c_dir_buff_addr    : unsigned(8 downto 0);
    signal c_dir_data_in      : std_logic_vector(7 downto 0);
    signal c_dir_start_calc   : std_logic;                      -- start calculation of the sector from the cluster value
    signal c_dir_calculating  : std_logic;                      -- use to make c_dir_start_calc an impulse
    -- For append directory:
    -- signal c_dir_last_cluster_nb : std_logic_vector(27 downto 0);

    -------------------------------------------------------------------------
    --
    --                      Update file information
    --
    -------------------------------------------------------------------------
    constant MSB_CLUSTER_OFFSET : std_logic_vector(4 downto 0) := '1' & x"4";    -- offset of where the starting cluster 2 MSB are stored
    constant LSB_CLUSTER_OFFSET : std_logic_vector(4 downto 0) := '1' & x"A";    -- offset of where the starting cluster 2 LSB are stored
    constant FILE_SIZE_OFFSET   : std_logic_vector(4 downto 0) := '1' & x"C";    -- offset of where the size of a file is stored
    type t_info_fsm is (off,            -- waiting for a file to be closed
                        read_sector,    -- read the sector where the file descriptor is
                        wait_read,      -- wait the read op to end
                        copy,           -- update information directly in the buffer
                        write_sector,   -- write the updated sector
                        wait_write,     -- wait the write op to end
                        over);          -- update file information is over
    signal info_fsm : t_info_fsm;
    signal info_sector_num    : std_logic_vector(31 downto 0); -- save the sector where the file descriptor is stored
    signal info_addr          : std_logic_vector( 3 downto 0); -- save the position of the current file descriptor in the sector
    signal info_true_addr     : std_logic_vector( 8 downto 0); -- complete address which address (in) the sector
    signal info_bytes_cnt     : integer range 0 to 7;          -- cnt how many bytes have been copied
    signal info_buffer        : std_logic_vector(31 downto 0); -- shift register which shift either the starting cluster or the size
    signal info_buffer_byte   : std_logic_vector( 7 downto 0); -- LSB of the above shift register, used as data input for the SDcard buffer


    -------------------------------------------------------------------------
    --
    --              Signals used to manage the directory buffer
    --
    -------------------------------------------------------------------------
    constant DIR_op_buff            : std_logic_vector(2 downto 0) := "110";    -- buffer to work on when using directories
    type t_tree_structure is array (0 to TREE_STRUCTURE_MAX) of std_logic_vector(31 downto 0);    -- (first) sector of the directory 
    signal tree_structure  : t_tree_structure;
    signal tree_depth      : integer range 0 to TREE_STRUCTURE_MAX;   -- how many directories are opened
    signal tree_depth_full : boolean;                                 -- true when directory tree depth is at the maximum (tree_depth=TREE_STRUCTURE_MAX), i.e. no sub folder can be opened

    type t_tree_structure_cluster is array (0 to TREE_STRUCTURE_MAX) of std_logic_vector(27 downto 0);    -- (first) cluster of the directory 
    signal tree_structure_cluster : t_tree_structure_cluster;

    signal open_dir_busy    : std_logic;                     -- busy signal to tell that the module is currently calculating the first sector of the newly opened directory
    signal calc_dir_sector  : std_logic;                     -- start the calcul of the first sector of the newly opened directory

    -------------------------------------------------------------------------
    -- calculate sector given by a cluster
    -------------------------------------------------------------------------
    -- Signals used to calculate to sector with a given cluster and the sector focused in this cluster
    signal calc_sector     : std_logic;                     -- start signal to calculate
    signal rem_shifts      : unsigned( 6 downto 0);         -- copy of sector per clusters during init, to know how many shift to do
    signal buffer_to_shift : unsigned(31 downto 0);         -- shift register
    signal sector_in_clust : unsigned( 7 downto 0);         -- sector focused in the cluster
    signal block_num       : unsigned(31 downto 0);         -- final sector address
    signal block_num_rdy   : std_logic;                     -- set when block num is ready
    signal calc_sector_bsy : std_logic;                     -- set if calculating

    signal mux_out_cluster : std_logic_vector(27 downto 0); -- cluster to use to determine the block num
    signal prev_cluster    : std_logic_vector(27 downto 0); -- previous cluster used to calculate the block num

    -------------------------------------------------------------------------
    --
    --                      Signals used to manage files
    --
    -------------------------------------------------------------------------
    component Sdcard_FAT32_file is
        Generic(CLK_FREQ_HZ   : integer := 100000000; -- the frequency of the clock expressed in Hz
                VIVADO_SYNTH  : boolean := True;      -- select the correct part of code to use to get a proper RAM implementation in vivado
                NB_MULTIPLE_OP: integer :=    8;      -- Number of sectors to operate at each operation
                WORD_SIZE_PW2 : integer :=    0;      -- power of 2 of the size or word to use
                FIFO_DEPTH    : integer := 4096);     -- maximum size of internal fifo for file reading

        Port (clk                  : in  STD_LOGIC;                         -- Main clock, frequency given by generic CLK_FREQ_HZ
              reset                : in  STD_LOGIC;                         -- reset, active high

              -- input interface
              filename             : in  STD_LOGIC_VECTOR (87 downto 0);    -- file name : 8 + '.' + 3
              date_time            : in  STD_LOGIC_VECTOR (39 downto 0) := (others => '0'); -- Creation time, can be ignored
              read_write_in        : in  STD_LOGIC;                         -- read = '0' ; write = '1'
              read_write_out       : out STD_LOGIC;                         -- read = '0' ; write = '1'
              reserve_clusters_in  : in  STD_LOGIC_VECTOR (21 downto 0);    -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case
              reserve_clusters_out : out STD_LOGIC_VECTOR (21 downto 0);    -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case

              opened               : out STD_LOGIC;                         -- high if the file is opened
              size_in              : in  STD_LOGIC_VECTOR (31 downto 0);    -- size of the file to read in bytes 
              size                 : out STD_LOGIC_VECTOR (31 downto 0);    -- return the file size (in bytes) if the file is open for reading (migh be useless)

              -- control signals
              open_file            : in  STD_LOGIC;                         -- try to open the file_name on the stream ID
              close_file           : in  STD_LOGIC;                         -- close the file
              format               : in  STD_LOGIC;                         -- format the card

              -- data interface
              data_in              : in  STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0);
              data_out             : out STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0);
              enable_in            : in  STD_LOGIC;                         -- tell the module to write data_in / read next data
              enable_out           : out STD_LOGIC;                         -- '1' when data_in can be write / data_out can be read

              -- constant from SD card
              sectors_per_cluster  : in  STD_LOGIC_VECTOR ( 7 downto 0);    -- nb of sector per cluster (constant from boot record)
              data_area_offset     : in  STD_LOGIC_VECTOR (31 downto 0);    -- offset for the begining of the data area, after boot record and fats (constant deducted/calculated from boot record)

              -- these access are control or info from file data fifo
              fifo_read            : in  STD_LOGIC;                          -- used by the SD manager to read data from FIFO
              fifo_write           : in  STD_LOGIC;                          -- used by the SD manager to write data on FIFO
              fifo_empty           : out STD_LOGIC;                          -- inform the module about the data fifo's filling
              fifo_full            : out STD_LOGIC;                          -- inform the module about the data fifo's filling

              -- sector/cluster management
              sector_num           : out STD_LOGIC_VECTOR (31 downto 0);    -- sector to read/write
              request_op           : out STD_LOGIC;                         -- request a read or write operation
              request_clusters     : out STD_LOGIC;                         -- request a fragment of free clusters for writing

              -- fragment transfert
              fragment_in          : in  fragment;                          -- fragment receive from free fragment manager
              last_fragment        : in  STD_LOGIC;                         -- annonce the last fragment (when collecting file's fragment for reading)
              write_clusters_chain : out STD_LOGIC;                         -- set when the clusters chain is ready to be written (into FAT)
              fragment_out         : out fragment;                          -- fragment to write in the FAT

              -- file information (writing)
              update_file_info     : out STD_LOGIC;                         -- set to request the update of file information, which will result in closing the file
              info_sector_num      : in  STD_LOGIC_VECTOR (31 downto 0);    -- file info sector number (=> file descritor sector)
              info_addr_offset_in  : in  STD_LOGIC_VECTOR ( 3 downto 0);    -- address offset of the file descriptor in the sector
              info_addr_offset_out : out STD_LOGIC_VECTOR ( 3 downto 0);    -- address offset of the file descriptor in the sector
              starting_cluster     : out STD_LOGIC_VECTOR (27 downto 0);    -- starting cluster of the file. Used to save it in file information

              -- ackknoledge
              op_ack               : in  STD_LOGIC;                         -- top module inform that the operation is accepted/can start
                                                                            -- also use at the opening of a file to transfert useful data/parameter through the signal 'sector_num'

              -- debug
              file_debug           : out STD_LOGIC_VECTOR ( 7 downto 0);    -- debug/error data
              file_error           : out STD_LOGIC;                         -- high when an error on the FAT occurs

              -- Debug
              debug_addr           : in  STD_LOGIC_VECTOR (15 downto 0);    -- address of the register to look out
              debug_data           : out STD_LOGIC_VECTOR (31 downto 0));   -- the data to look out
    end component;

    type t_file_interface_in is record
        -- control signals
        open_file                : std_logic;
        close_file               : std_logic;

        -- data interface
        data_in                  : std_logic_vector(8*(2**WORD_SIZE_PW2)-1 downto 0);
        enable_in                : std_logic;

        -- these access are contr
        fifo_read                : std_logic;
        fifo_write               : std_logic;

        -- fragment transfert
        fragment_in              : fragment;
        last_fragment            : std_logic;

        -- file information
        info_sector_num          : std_logic_vector(31 downto 0);
        info_addr_offset_in      : std_logic_vector( 3 downto 0);

        -- ackknoledge
        op_ack                   : std_logic;
    end record;
    type t_file_interface_out is record
        read_write_out           : std_logic;
        reserve_clusters_out     : std_logic_vector(21 downto 0);
 
        opened                   : std_logic;
        size                     : std_logic_vector(31 downto 0);

        -- data interface
        data_out                 : std_logic_vector(8*(2**WORD_SIZE_PW2)-1 downto 0);
        enable_out               : std_logic;

        -- these access are contr
        fifo_empty               : std_logic;
        fifo_full                : std_logic;
                    
        -- sector/cluster managem
        sector_num               : std_logic_vector(31 downto 0);
        request_op               : std_logic;
        request_clusters         : std_logic;
                    
        -- fragment transfert
        write_clusters_chain     : std_logic;
        fragment_out             : fragment;

        -- file information 
        update_file_info         : std_logic;
        info_addr_offset_out     : std_logic_vector( 3 downto 0);
        starting_cluster         : std_logic_vector(27 downto 0);

        -- debug
        file_debug               : std_logic_vector(7 downto 0);
        file_error               : std_logic;
    end record;
    type t_file_in  is array (0 to MAX_FILES-1) of t_file_interface_in;
    type t_file_out is array (0 to MAX_FILES-1) of t_file_interface_out;
    signal file_in  : t_file_in;
    signal file_out : t_file_out;

    signal file_opened_i     : std_logic_vector(MAX_FILES-1 downto 0);           -- set when the file is opened
    signal rw_mode_i         : std_logic_vector(MAX_FILES-1 downto 0);           -- set when the file is in write mode
    signal file_size_i       : std_logic_vector(31 downto 0);                    -- current file size

    signal file_debug_addr   : std_logic_vector(15 downto 0);
    signal file_debug_data   : std_logic_vector(31 downto 0);

    signal file_last_fragment       : std_logic;
    signal file_data_in             : std_logic_vector(8*(2**WORD_SIZE_PW2)-1 downto 0);
    signal file_op_ack              : std_logic;
    signal file_info_sector_num     : std_logic_vector(31 downto 0);
    signal file_info_addr_offset_in : std_logic_vector( 3 downto 0);
    signal file_fragment_in         : fragment;
    signal file_fifo_read           : std_logic;
    signal file_fifo_write          : std_logic;

  -- function fetch_op(file_out: t_file_out) return std_logic_vector is
    -- begin 
    --     for i in 0 to MAX_FILES-1 loop
    --         if file_out(i).request_op = '1' then
    --             return '1' & file_out(i).read_write_out & std_logic_vector(to_unsigned(i, log2(MAX_FILES)));
    --         end if;
    --     end loop;
    --     return "000" & (log2(MAX_FILES)-1 downto 0 => '0'); -- std_logic_vector(to_unsigned(0, log2(MAX_FILES)));
    -- end function;

    -------------------------------------------------------------------------
    --
    --                      SD manager signals (short: SDm)
    --
    -------------------------------------------------------------------------
    -- FSM which operate sector transfert with the SD card. Works in parallele with SD_op_FSM
    type t_SD_manager_fsm is (idle,             -- wait for user op or file op
                              give_free_frag,   -- fetch a free cluster fragment for a file manager
                              wait_next_op,     -- wait the free cluster fragment to be receive by the file manager, which will then request write op
                              ack_file_op,      -- send ack to the file which will have his operation done
                              wait_buffer,      -- wait the next buffer to be free (reading) / to be full (writing)
                              read_sector,      -- read a sector and put it in the just selected (free) buffer
                              wait_read,        -- wait read op to end
                              write_sector,     -- write the sector which is in the just selected (full) buffer
                              wait_write,       -- wait write op to end
                              end_op);          -- the multiple op is done, wait for SD_op_fsm to go off (basically wait the last sector read to be full transfered to the file manager)
    signal SDm_fsm : t_SD_manager_fsm;

    signal SDm_free_frag  : std_logic;                      -- set when a file manager need free fragment to write stuff
    signal SDm_file_op    : std_logic;                      -- set when file(s) need an operation
    signal SDm_write_op   : std_logic;                      -- set for a write operation, unset for a read operation

    signal SDm_file_op_shift      : std_logic_vector(MAX_FILES-1 downto 0); -- contain the file resquested operation and it is use to find and select a file operation
    -- signal SDm_request_frag_shift : std_logic_vector(MAX_FILES-1 downto 0);

    signal file_id_user : integer range 0 to MAX_FILES-1;  -- file ID manager with which the user has request an operation
    signal file_id_op   : integer range 0 to MAX_FILES-1;  -- file ID of the file which has been grant an operation
    signal file_id_all  : integer range 0 to MAX_FILES-1;  -- result of a mux of the abose file id

    signal SDm_sector_num    : std_logic_vector(31 downto 0);  -- sector to operate
    -- signal SDm_op_buff       : std_logic_vector(2 downto 0);   -- buffer to operate
    signal SDm_buff_addr     : unsigned(8 downto 0);           -- addr the buffer
    signal SDm_last_byte_dly : std_logic;                      -- set when transfering the last byte of the newly read sector. It save one 9-bit counter
    signal SDm_bytes_cnt     : integer range 0 to 511;         -- cnt bytes to transfert (512 per sectors)
    signal SDm_data_read     : std_logic_vector(8*(2**WORD_SIZE_PW2)-1 downto 0);  -- buffer for data to transfert from sector read to file's fifo
    signal SDm_data_write    : std_logic_vector(8*(2**WORD_SIZE_PW2)-1 downto 0);  -- buffer for data to transfert from file's fifo to sector to write

    -- FSM which transfert sector from file manager to SDcard raw access buffers (reverse for writing). Works in parallele with SD_manager_fsm
    type t_SD_op_fsm is (off,              -- not using SDcard
                         wait_buffer,      -- wait a buffer to be free (writing)/ to be full (reading) 
                         transfert_read,   -- transfert data read to the file manager
                         read_dly,         -- tempo state to let time to reset counter and increment buffer ptr
                         transfert_write); -- transfert data to write to the SDcard buffer

    signal SD_op_fsm  : t_SD_op_fsm;


    -- Signals for multiple operation
    signal mul_op_cnt            : integer range 0 to NB_MULTIPLE_OP-1;     -- cnt the number of sector read or write since multiple has been set
    -- signal mul_previous_sector   : std_logic_vector(31 downto 0);           -- save the just operated sector to comapre it with the futur one
    -- signal not_contiguous_sector : boolean;                                 -- conpare the previous sector operated with the next one
    signal multiple_op_done      : std_logic;                               -- set when NB_MULTIPLE_OP have been done
    signal mult_last_op          : std_logic;                               -- set when the next op is the last one of the batch
    signal multiple_i            : std_logic;
    signal mul_end_last_op       : std_logic;                               -- set when the last op is being done

    constant NB_SDCARD_BUFFER   : integer := 2**BUFFER_NUM_PW2;                 -- Number of data buffer generated in SDcard raw_access module
    signal mul_buffer_free    : std_logic_vector(NB_SDCARD_BUFFER-1 downto 0);  -- state of SDcard raw access buffers : 0 = free ; 1 = full
    signal mul_buff_in_ptr    : integer range 0 to NB_SDCARD_BUFFER-1;          -- pointer to the buffer state table (mul_buffer_free) used by SD_manager_fsm
    signal mul_buff_out_ptr   : integer range 0 to NB_SDCARD_BUFFER-1;          -- pointer to the buffer state table (mul_buffer_free) used by SD_op_fsm
    signal SDm_op_buff        : std_logic_vector(BUFFER_NUM_PW2-1 downto 0);    -- used to select the right buffer to use when doing an operation with the SD card
    signal SDm_data_buff      : std_logic_vector(BUFFER_NUM_PW2-1 downto 0);    -- used to select the right buffer to use when transfert a sector between SD raw access and a file manager


    signal SDm_sector_cnt : integer range 0 to NB_MULTIPLE_OP-1; -- cnt the number of sector transfered to/from SDcard buffers

begin


    Process(clk)
    begin
        if rising_edge(clk) then
            if (module_fsm = ejected and reset_in_reg = '1') or (module_fsm = initializing and reset = '1') or synced_CD = '1' then
                longreset <= '1';
            elsif module_fsm = reseting then
                longreset <= '0';
            end if;
        end if;
    end process;


    -- Get user operation requests
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                user_admin_op <= '0';
                user_file_op  <= '0';
                user_op_bsy   <= '0';
            elsif busy_i = '0' then
                if format = '1' or reset = '1' or eject_card = '1' then
                    user_admin_op <= '1';
                    user_op_bsy   <= '1';
                elsif open_file = '1' or close_file = '1' or open_dir = '1' or close_dir = '1' then
                    user_file_op <= '1';
                    user_op_bsy  <= '1';
                end if;
            else
                if user_admin_op = '1' and SDm_fsm = idle and module_fsm = idle then            -- start user op
                    user_admin_op <= '0';
                    user_op_bsy <= '1';
                elsif user_file_op = '1' and module_fsm = idle then
                    user_file_op <= '0';
                    user_op_bsy  <= '1';
                elsif user_admin_op = '0' and user_file_op = '0' and (module_fsm = idle or module_fsm = ejected) then  -- user op done
                    user_admin_op <= '0';
                    user_op_bsy   <= '0';
                end if;
            end if;
        end if;
    end process;

    -- Copy user operation inputs
    -- TODO : - remove these copy of filename, date_time, reserv_clust and read_write. It is already done below with FD_name,...
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                eject_in_reg  <= '0';
                reset_in_reg  <= '0';
                format_in_reg <= '0';
            -- if no user operation waiting or processing, accept a new user op.
            elsif busy_i = '0' then
                -- SDcard global operation
                if format = '1' then
                    format_in_reg <= '1';
                elsif reset = '1' then
                    reset_in_reg <= '1';
                elsif eject_card = '1' then
                    eject_in_reg <= '1';
                end if;
            -- when the user op is being processing, reset the flag op. request
            elsif user_admin_op = '0' then
                eject_in_reg  <= '0';
                -- reset_in_reg  <= '0'; -- Not doing it here to remember the reset
                format_in_reg <= '0';
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if busy_i = '0' and not(format = '1' or reset = '1' or eject_card = '1') then
                if open_file = '1' then
                    file_id_in_reg      <= file_id;
                    read_write_in_reg   <= read_write;
                    filename_in_reg     <= filename;
                    date_time_in_reg    <= date_time;
                    reserv_clust_in_reg <= reserv_clust;
                elsif close_file = '1' then 
                    file_id_in_reg      <= file_id;
                elsif open_dir = '1' then
                    read_write_in_reg   <= read_write;
                    filename_in_reg     <= filename;
                    date_time_in_reg    <= date_time;
                end if;
            end if;
        end if;
    end process;

    -- Add a delay of 1 clk cycle to operation related with files to let time to file_id_in_reg, or  filename_in_reg,..., to take their new value
    Process(clk)
    begin
        if rising_edge(clk) then
            if busy_i = '0' and not(format = '1' or reset = '1' or eject_card = '1') then
                -- if no user operation waiting or processing, accept a new user op.
                if open_file = '1' then
                    open_file_buff <= '1';
                elsif close_file = '1' then
                    close_file_buff <= '1';
                elsif open_dir = '1' then
                    open_dir_buff <= '1';
                elsif close_dir = '1' then
                    close_dir_buff <= '1';
                end if;
            else
                open_file_buff  <= '0';
                close_file_buff <= '0';
                open_dir_buff   <= '0';
                close_dir_buff  <= '0';
            end if;
        end if;
    end process;

    -- Detect the beginning of the ejection
    Process(clk)
    begin
        if rising_edge(clk) then
            if (eject_in_reg = '1' or reset_in_reg = '1') and module_fsm = idle and SDm_fsm = idle and start_eject = '0' then
                start_eject <= '1';
            else
                start_eject <= '0';
            end if;
        end if;
    end process;

    -- This determine when the module eject the SD card
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                eject_busy <= '0';
            elsif start_eject = '1' then
                eject_busy <= '1';
            elsif module_fsm = ejected then
                eject_busy <= '0';
            end if;
        end if;
    end process;
    
    
    -- Here we determine which is the next file to close when the SDcard is ejecting
    Process(clk)
    begin
        if rising_edge(clk) then
            if eject_busy = '1' and not all_file_closed and module_fsm = close_files then
                if load_file_to_close = '0' then
                    load_file_to_close <= '1';
                    for i in 0 to MAX_FILES-1 loop
                        file_to_close_shift(i) <= file_out(i).opened;
                    end loop;
                    file_to_close_id    <= 0;
                    file_to_close_found <= '0';
                else
                    if file_to_close_shift(0) = '1' then
                        file_to_close_found <= '1';
                    else
                        file_to_close_shift <= '0' & file_to_close_shift(MAX_FILES-1 downto 1);
                        if MAX_FILES > 1 then   -- this is to avoid simulation error... 
                            file_to_close_id <= file_to_close_id + 1;
                        else
                            file_to_close_id <= 0;
                        end if;
                    end if;
                end if;
            else
                file_to_close_found <= '0';
                load_file_to_close  <= '0';
            end if;
        end if;
    end process;

    -- All file manager are off when all file are closed
    all_file_closed <= file_opened_i = (MAX_FILES-1 downto 0 => '0');

    -----------------------------------------------------------------
    -- Module FSM
    -----------------------------------------------------------------
    -- TODO : - if reset = '1' then module_fsm <= ejecting_SDcard; end if;
    Process(clk)
    begin
        if rising_edge(clk) then
            if synced_CD = '1' then
                module_fsm <= no_SD_card;
            elsif (module_fsm = ejected and reset_in_reg = '1') or (module_fsm = initializing and reset = '1') then
                module_fsm <= reseting;
            else
                case module_fsm is
                    when no_SD_card         => if synced_CD = '0'                     then module_fsm <= reseting;          end if;
                    when reseting           =>                                             module_fsm <= initializing;
                    when initializing       => if SD_busy = '0'                       then module_fsm <= boot_record;       end if;
                    when boot_record        => if BR_fsm = BR_end                     then module_fsm <= fat;
                                            elsif BR_fsm = BR_issue                   then module_fsm <= format_problem;    end if;
                    when fat                => if FAT_fsm = idle                      then module_fsm <= idle;
                                            elsif FAT_fsm = fat_issue                 then module_fsm <= format_problem;    end if;
                    when idle               => if user_admin_op='1'and SDm_fsm = idle then
                                                   if    format_in_reg = '1' then module_fsm <= formating;
                                                   elsif reset_in_reg  = '1' then module_fsm <= close_files;
                                                   elsif eject_in_reg  = '1' then module_fsm <= close_files; end if;
                                            elsif open_file_buff = '1'                then
                                                   if file_opened_i(file_id_user) = '0' then module_fsm <= open_fd;
                                                   else                                      module_fsm <= idle;    end if;
                                            elsif close_file_buff = '1'               then
                                                   if file_opened_i(file_id_user) = '1' then module_fsm <= close_fd;
                                                   else                                      module_fsm <= idle;    end if;
                                            elsif open_dir_buff = '1'                 then
                                                   if not tree_depth_full               then module_fsm <= open_fd;
                                                   else                                      module_fsm <= idle;    end if;
                                            elsif close_dir_buff = '1'                then
                                                   if tree_depth /= 0                   then module_fsm <= close_directory;
                                                   else                                      module_fsm <= idle;    end if; end if;
                    when open_fd            => if check_fd_fsm = check_over           then module_fsm <= idle;              end if;
                    when close_fd           => if file_opened_i(file_id_user) = '0' and eject_busy = '0' then module_fsm <= idle;
                                            elsif file_opened_i(file_id_user) = '0' and eject_busy = '1' then module_fsm <= close_files; end if;
                    when erase_fd           => if erase_file_ok = '1'                 then module_fsm <= idle;              end if;
                    when close_directory    =>                                             module_fsm <= idle;
                    when formating          => if formating_done = '1'                then module_fsm <= boot_record;       end if;
                    when format_problem     => if format = '1'                        then module_fsm <= formating;         end if;
                    when close_files        => if all_file_closed                     then module_fsm <= recopy_FAT;
                                            elsif file_to_close_found = '1'           then module_fsm <= close_fd;          end if;
                    when recopy_FAT         => if recopy_FAT_fsm = recopy_end         then module_fsm <= ejected;           end if;
                    when ejected            => null;
                end case;
            end if;
        end if;
    end process;

    not_a_file_op <= '1' when module_fsm = no_SD_card     or
                              module_fsm = initializing   or
                              module_fsm = boot_record    or
                              module_fsm = fat            or
                              module_fsm = close_files    or
                              module_fsm = recopy_FAT     or
                              module_fsm = ejected        or
                              module_fsm = formating      or
                              module_fsm = format_problem else '0';

    busy   <= busy_i;
    busy_i <= '0' when (user_op_bsy = '0' and module_fsm = idle) or module_fsm = ejected else '1';

    file_opened <= file_opened_i;


    -- file ID save, to remember which file manager we have to interact with during 'user' operation
    file_id_user <= file_to_close_id when eject_busy = '1' else to_integer(unsigned(file_id_in_reg));
    -- file_id_user <= 0;



    -----------------------------------------------------------------
    --
    --                   Boot Record managment
    --
    -----------------------------------------------------------------
    -- TODO : only get useful parameters and fetch FSI sector as soon as we can, after checking jump code
    -- Here we have the Boot Record management FSM
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                BR_fsm <= off;
            elsif BR_error = '1' then
                BR_fsm <= BR_issue;
            else
                case BR_fsm is
                    when off                      => if module_fsm = boot_record then BR_fsm <= read_BR;                  end if;
                    -- Boot Record
                    when read_BR                  => if SD_busy = '0'            then BR_fsm <= analyse_BR;               end if;
                    when analyse_BR               =>                                  BR_fsm <= get_jump_code;
                    when get_jump_code            => if BR_bytes_cnt = 0         then BR_fsm <= get_OEM_name;             end if;
                    when get_OEM_name             => if BR_bytes_cnt = 0         then BR_fsm <= get_bytes_per_sector;     end if;
                    when get_bytes_per_sector     => if BR_bytes_cnt = 0         then BR_fsm <= get_sectors_per_cluster;  end if;
                    when get_sectors_per_cluster  => if BR_bytes_cnt = 0         then BR_fsm <= get_reserved_sectors;     end if;
                    when get_reserved_sectors     => if BR_bytes_cnt = 0         then BR_fsm <= get_nb_FAT;               end if;
                    when get_nb_FAT               => if BR_bytes_cnt = 0         then BR_fsm <= get_media_descriptor;     end if;
                    when get_media_descriptor     => if BR_bytes_cnt = 0         then BR_fsm <= get_sectors_per_track;    end if;
                    when get_sectors_per_track    => if BR_bytes_cnt = 0         then BR_fsm <= get_nb_heads;             end if;
                    when get_nb_heads             => if BR_bytes_cnt = 0         then BR_fsm <= get_nb_hidden_sectors;    end if;
                    when get_nb_hidden_sectors    => if BR_bytes_cnt = 0         then BR_fsm <= get_nb_sectors_partition; end if;
                    when get_nb_sectors_partition => if BR_bytes_cnt = 0         then BR_fsm <= get_nb_sectors_per_FAT;   end if;
                    when get_nb_sectors_per_FAT   => if BR_bytes_cnt = 0         then BR_fsm <= get_active_FAT;           end if;
                    when get_active_FAT           => if BR_bytes_cnt = 0         then BR_fsm <= get_root_dir_cluster;     end if;
                    when get_root_dir_cluster     => if BR_bytes_cnt = 0         then BR_fsm <= get_FSI_sector;           end if;
                    when get_FSI_sector           => if BR_bytes_cnt = 0         then BR_fsm <= get_backup_boot_sector;   end if;
                    when get_backup_boot_sector   => if BR_bytes_cnt = 0         then BR_fsm <= get_logical_drive_nb;     end if;
                    when get_logical_drive_nb     => if BR_bytes_cnt = 0         then BR_fsm <= get_extended_signature;   end if;
                    when get_extended_signature   => if BR_bytes_cnt = 0         then BR_fsm <= get_serial_nb;            end if;
                    when get_serial_nb            => if BR_bytes_cnt = 0         then BR_fsm <= get_volume_name;          end if;
                    when get_volume_name          => if BR_bytes_cnt = 0         then BR_fsm <= get_FAT_name;             end if;
                    when get_FAT_name             => if BR_bytes_cnt = 0         then BR_fsm <= get_boot_signature;       end if;
                    when get_boot_signature       => if BR_bytes_cnt = 0         then BR_fsm <= read_FSI;                 end if;
                    -- File System Information (opt.)
                    when read_FSI                 => if SD_busy = '0'            then BR_fsm <= analyse_FSI;              end if;
                    when analyse_FSI              =>                                  BR_fsm <= get_nb_free_cluster;
                    when get_nb_free_cluster      => if BR_bytes_cnt = 0         then BR_fsm <= get_recent_cluster;       end if;
                    when get_recent_cluster       => if BR_bytes_cnt = 0         then BR_fsm <= final_check;              end if;
                    when final_check              =>                                  BR_fsm <= BR_end;

                    when BR_issue                 => if module_fsm = formating   then BR_fsm <= off;                      end if;
                    when BR_end                   => null;
                end case;
            end if;
        end if;
    end process;



    -----------------------------------------------------------------
    -- Here we store Boot Record values
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                jump_code_NOP                <= (others => '0');
                OEM_name                     <= (others => '0');
                bytes_per_sector             <= (others => '0');
                sectors_per_cluster          <= (others => '0');
                reserved_sectors             <= (others => '0');
                nb_FAT                       <= (others => '0');
                media_descriptor             <= (others => '0');
                sectors_per_track            <= (others => '0');
                nb_heads                     <= (others => '0');
                nb_hidden_sectors            <= (others => '0');
                nb_sectors_in_partition      <= (others => '0');
                nb_sectors_per_FAT           <= (others => '0');
                active_FAT                   <= (others => '0');
                root_directory_cluster       <= (others => '0');
                sector_nb_file_system_info   <= (others => '0');
                sector_nb_backup_boot_sector <= (others => '0');
                logical_drive_nb             <= (others => '0');
                extended_signature           <= (others => '0');
                serial_nb_of_partition       <= (others => '0');
                volume_name_of_partition     <= (others => '0');
                FAT_name                     <= (others => '0');
                boot_signature               <= (others => '0');
            else
                case BR_fsm is
                    when get_jump_code            => jump_code_NOP                <= SD_data_out & jump_code_NOP               (23 downto 8);
                    when get_OEM_name             => OEM_name                     <= SD_data_out & OEM_name                    (63 downto 8);
                    when get_bytes_per_sector     => bytes_per_sector             <= SD_data_out & bytes_per_sector            (15 downto 8);
                    when get_sectors_per_cluster  => sectors_per_cluster          <= SD_data_out;
                    when get_reserved_sectors     => reserved_sectors             <= SD_data_out & reserved_sectors            (15 downto 8);
                    when get_nb_FAT               => nb_FAT                       <= SD_data_out;
                    when get_media_descriptor     => media_descriptor             <= SD_data_out;
                    when get_sectors_per_track    => sectors_per_track            <= SD_data_out & sectors_per_track           (15 downto 8);
                    when get_nb_heads             => nb_heads                     <= SD_data_out & nb_heads                    (15 downto 8);
                    when get_nb_hidden_sectors    => nb_hidden_sectors            <= SD_data_out & nb_hidden_sectors           (31 downto 8);
                    when get_nb_sectors_partition => nb_sectors_in_partition      <= SD_data_out & nb_sectors_in_partition     (31 downto 8);
                    when get_nb_sectors_per_FAT   => nb_sectors_per_FAT           <= SD_data_out & nb_sectors_per_FAT          (31 downto 8);
                    when get_active_FAT           => active_FAT                   <= SD_data_out & active_FAT                  (15 downto 8);
                    when get_root_dir_cluster     => root_directory_cluster       <= SD_data_out & root_directory_cluster      (31 downto 8);
                    when get_FSI_sector           => sector_nb_file_system_info   <= SD_data_out & sector_nb_file_system_info  (15 downto 8);
                    when get_backup_boot_sector   => sector_nb_backup_boot_sector <= SD_data_out & sector_nb_backup_boot_sector(15 downto 8);
                    when get_logical_drive_nb     => logical_drive_nb             <= SD_data_out;
                    when get_extended_signature   => extended_signature           <= SD_data_out;
                    when get_serial_nb            => serial_nb_of_partition       <= SD_data_out & serial_nb_of_partition      (31 downto 8);
                    when get_volume_name          => volume_name_of_partition     <= SD_data_out & volume_name_of_partition    (87 downto 8);
                    when get_FAT_name             => FAT_name                     <= SD_data_out & FAT_name                    (63 downto 8);
                    when get_boot_signature       => boot_signature               <= SD_data_out & boot_signature              (15 downto 8);
                    when get_nb_free_cluster      => BR_nb_free_cluster           <= SD_data_out & BR_nb_free_cluster          (31 downto 8);
                    when get_recent_cluster       => recent_cluster               <= SD_data_out & recent_cluster              (31 downto 8);
                    when final_check              => if recent_cluster = x"FFFFFFFF"  
                                                     or to_integer(unsigned(recent_cluster)) > nb_total_cluster then recent_cluster <= x"00000002";
                                                     end if;
                                                     if to_integer(unsigned(BR_nb_free_cluster)) > nb_total_cluster then BR_nb_free_cluster <= (others => '0'); end if;
                    when others                   => null;
                end case;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- Here we count byte received for each parameters
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                BR_bytes_cnt  <= 0;
            elsif module_fsm = boot_record then
                if BR_bytes_cnt = 0 then
                    case BR_fsm is
                        when analyse_BR               => BR_bytes_cnt <=  2;
                        when get_jump_code            => BR_bytes_cnt <=  7;
                        when get_OEM_name             => BR_bytes_cnt <=  1;
                        when get_bytes_per_sector     => BR_bytes_cnt <=  0;
                        when get_sectors_per_cluster  => BR_bytes_cnt <=  1;
                        when get_reserved_sectors     => BR_bytes_cnt <=  0;
                        when get_nb_FAT               => BR_bytes_cnt <=  0;
                        when get_media_descriptor     => BR_bytes_cnt <=  1;
                        when get_sectors_per_track    => BR_bytes_cnt <=  1;
                        when get_nb_heads             => BR_bytes_cnt <=  3;
                        when get_nb_hidden_sectors    => BR_bytes_cnt <=  3;
                        when get_nb_sectors_partition => BR_bytes_cnt <=  3;
                        when get_nb_sectors_per_FAT   => BR_bytes_cnt <=  1;
                        when get_active_FAT           => BR_bytes_cnt <=  3;
                        when get_root_dir_cluster     => BR_bytes_cnt <=  1;
                        when get_FSI_sector           => BR_bytes_cnt <=  1;
                        when get_backup_boot_sector   => BR_bytes_cnt <=  0;
                        when get_logical_drive_nb     => BR_bytes_cnt <=  0;
                        when get_extended_signature   => BR_bytes_cnt <=  3;
                        when get_serial_nb            => BR_bytes_cnt <= 10;
                        when get_volume_name          => BR_bytes_cnt <=  7;
                        when get_FAT_name             => BR_bytes_cnt <=  1;
                        when get_boot_signature       => BR_bytes_cnt <=  0;
                        when analyse_FSI              => BR_bytes_cnt <=  3;
                        when get_nb_free_cluster      => BR_bytes_cnt <=  3;
                        when others                   => null;  -- BR_bytes_cnt <=  0;
                    end case;
                else
                    BR_bytes_cnt <= BR_bytes_cnt - 1;
                end if;
            end if;
        end if;
    end process;

    -- Here we manage the buffer when analysing the boot record or the file system info
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                BR_loc_buffer <= "010";
            elsif BR_fsm = read_BR then
                BR_loc_buffer <= "010";
            elsif BR_fsm = read_FSI then
                BR_loc_buffer <= "011";
            end if;
        end if;
    end process;

    BR_block_num <= (others => '0') when BR_fsm = off or BR_fsm = read_BR else (31 downto 16 => '0') & sector_nb_file_system_info;

    -----------------------------------------------------------------
    -- Here we manage the address
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                BR_address    <= (others => '0');
            else
                case BR_fsm is
                    when read_BR                  =>                          BR_address <=  (others => '0');
                    when analyse_BR               =>                          BR_address <= unsigned(BR_address) + 1;  -- starting counting now to create the pipeline, otherwise the first byte would be read 2 times
                    when get_jump_code            =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"003"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_OEM_name             =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"00B"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_bytes_per_sector     =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"00D"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_sectors_per_cluster  =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"00E"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_reserved_sectors     => if BR_bytes_cnt = 0 then BR_address <= '0' & x"15";
                                                     else                     BR_address <= unsigned(BR_address) + 1;  end if;--if BR_bytes_cnt = 0 then BR_address <=  '0' & x"010"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_nb_FAT               => if BR_bytes_cnt = 0 then BR_address <= '0' & x"18";
                                                     else                     BR_address <= unsigned(BR_address) + 1;  end if;
                    when get_media_descriptor     =>                          BR_address <= unsigned(BR_address) + 1;
                    when get_sectors_per_track    =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"01A"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_nb_heads             =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"01C"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_nb_hidden_sectors    =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"020"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_nb_sectors_partition =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"024"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_nb_sectors_per_FAT   =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"028"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_active_FAT           => if BR_bytes_cnt = 1 then BR_address <= '0' & x"2C";
                                                     else                     BR_address <= unsigned(BR_address) + 1;  end if;
                    when get_root_dir_cluster     =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"030"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_FSI_sector           =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"032"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_backup_boot_sector   => if BR_bytes_cnt = 1 then BR_address <= '0' & x"40"; 
                                                  elsif BR_bytes_cnt = 0 then BR_address <= '0' & x"42";
                                                  else                        BR_address <= unsigned(BR_address) + 1;  end if;
                    when get_logical_drive_nb     => --if BR_bytes_cnt = 0 then BR_address <= '0' & x"42";
                                                     --else                     BR_address <= unsigned(BR_address) +1);  end if;
                                                                              BR_address <= unsigned(BR_address) + 1;
                    when get_extended_signature   =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"043"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_serial_nb            =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"047"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_volume_name          =>                          BR_address <= unsigned(BR_address) + 1;  --if BR_bytes_cnt = 0 then BR_address <=  '0' & x"052"; else  BR_address <= unsigned(BR_address) + 1);  end if;
                    when get_FAT_name             => if BR_bytes_cnt = 1 then BR_address <= '1' & x"FE";
                                                     else                     BR_address <= unsigned(BR_address) + 1;  end if;
                    when get_boot_signature       =>                          BR_address <= unsigned(BR_address) + 1;

                    when read_FSI                 =>                          BR_address <= '1' & x"E8";
                    when analyse_FSI              =>                          BR_address <= unsigned(BR_address) + 1;
                    when get_nb_free_cluster      => if BR_bytes_cnt = 1 then BR_address <= '1' & x"EC";
                                                     else                     BR_address <= unsigned(BR_address) + 1;  end if;
                    when get_recent_cluster       =>                          BR_address <= unsigned(BR_address) + 1;
                    when others                   =>                          BR_address <= (others => '0');
                end case;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- Here we compare results to detect errors
    -----------------------------------------------------------------
    -- TODO :  - Check every parameters to ensure no weird setup ? (ex: FAT32 ok but 0 FAT?)
    --         - Check active FAT (or if set => error)
    -- FIXME : - Check that the Root directory cluster is 2 and not 0 (or more than 2, or need to be implemented).
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1'  then
                BR_error <= '0';
            elsif BR_error = '1' then
                BR_error <= '0';
            else
                case BR_fsm is
                    when get_OEM_name             => if jump_code_NOP(23 downto 16) /= x"90" or jump_code_NOP(7 downto 0) /= x"EB" then BR_error <= '1'; end if; --jump_code_NOP /= identifier
                    when get_sectors_per_cluster  => if bytes_per_sector /= bytes_per_sector_th                                    then BR_error <= '1'; end if;
                    -- when get_reserved_sectors     => 
                    -- when get_nb_FAT               => 
                    -- when get_media_descriptor     => -- check this ? It should be xf8 every time ?
                    -- when get_sector_per_track     => 
                    -- when get_nb_heads             => 
                    -- when get_nb_hidden_sectors    => 
                    -- when get_nb_sectors_partition => -- It should be SD_nb_blocks or less
                    when get_nb_sectors_per_FAT   => if nb_sectors_in_partition > SD_nb_blocks  then BR_error <= '1'; end if;
                    -- when get_active_FAT           => 
                    -- when get_root_dir_cluster     => 
                    when get_FSI_sector           => if root_directory_cluster /= x"00000002"   then BR_error <= '1'; end if;
                    -- when get_backup_boot_sector   => 
                    -- when get_logical_drive_nb     => 
                    -- when get_extended_signature   =>
                    when get_serial_nb            => if extended_signature /= x"29"             then BR_error <= '1'; end if;
                    -- when get_volume_name          => 
                    -- when get_FAT_name             => 
                    when get_boot_signature       => if FAT_name         /= FAT_name_th         then BR_error <= '1'; end if;
                    when read_FSI                 => if boot_signature   /= boot_signature_th   then BR_error <= '1'; end if;
                    when others                   => null;
                end case;
            end if;
        end if;
    end process;

    -- Calculate the maximum number of cluster and the beginning of the data sectors
    -- TODO : - Make addition (on multiple clock cycle) instead of multiplication for total_size_of_FATs
    --        - remove unsigned(sectors_per_cluster) * (unsigned(root_directory_cluster) - 2) from first_data_sector to improve latency ? (as it should always be equal to 0...)
    -- FIXME: - case where root_directory_sector < 2 (value 0 is possible according to doc) will not work (might be fixed)
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                total_size_of_FATs     <= (others => '0');
                nb_fat_cpy             <= (others => '0');
                nb_total_cluster       <= (others => '0');
                sector_per_cluster_cpy <= (others => '0');
                data_area_begin_sector <= (others => '0');
                root_sector_buffer     <= (others => '0');
                root_directory_sector  <= (others => '0');
            elsif BR_fsm = get_active_FAT then
                total_size_of_FATs <= unsigned(nb_sectors_per_FAT);
                nb_fat_cpy         <= unsigned(nb_FAT) - 1;
            elsif nb_fat_cpy /= 0 then
                total_size_of_FATs <= total_size_of_FATs + unsigned(nb_sectors_per_FAT);
                nb_fat_cpy         <= nb_FAT_cpy - 1;
            -- elsif BR_fsm = read_FSI then
                -- nb_total_cluster <= unsigned(nb_sectors_in_partition) - (unsigned(reserved_sectors) + total_size_of_FATs);
            elsif BR_fsm = analyse_FSI then
                nb_total_cluster       <= unsigned(nb_sectors_in_partition) - (unsigned(reserved_sectors) + total_size_of_FATs);
                sector_per_cluster_cpy <= '0' & unsigned(sectors_per_cluster(7 downto 1));  -- / 2;
                data_area_begin_sector <= unsigned(reserved_sectors) + total_size_of_FATs;
                root_sector_buffer     <= (unsigned(root_directory_cluster) - 2);
                root_directory_sector  <= unsigned(reserved_sectors) + total_size_of_FATs;
            elsif sector_per_cluster_cpy /= 0 then
                nb_total_cluster       <= '0' & nb_total_cluster(31 downto 1);      -- / 2;
                sector_per_cluster_cpy <= '0' & sector_per_cluster_cpy(7 downto 1); -- / 2;
                -- if unsigned(root_directory_cluster) > 2 then
                    root_sector_buffer <= root_sector_buffer(30 downto 0) & '0';    -- * 2 ;
                -- end if;
            elsif root_sector_buffer /= 0 then   -- last case of this process, we add the offset of the root directory (in case it is not in cluster 2)
                root_directory_sector <= root_directory_sector + root_sector_buffer;
                root_sector_buffer <= (others => '0');
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    --
    --                    FAT management
    --
    -----------------------------------------------------------------
    -- FAT init. FSM
    -- TODO : - implement the case where there is only 1 FAT, and the case where we format the card (where we might skip check and free cluster cnt)?
    --        - implement eject (FAT copy/paste) (here or not ?)
    --        - start to read FATs sooner, and so remove SD_busy to go to read_fat_1
    --        - start reading FATs during check to accelerate checking. Here it is done to work in simulation (because reading in simulation is too fast).
    --        - use multiple reading and the 8 buffers (4*2) to read faster, and then check FATs. Or use every buffer and read sectors by sectors, but continously check FATs
    --        - also read the second sector of the FAT to possibly gain time at the beginning of operation ?
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FAT_fsm <= off;
            elsif FAT_difference = '1' then
                FAT_fsm <= fat_issue;
            else
                case FAT_fsm is
                    when off               => if module_fsm = fat and SD_busy = '0'   then
                                                  if nb_FAT = x"01"                       then  FAT_fsm <= read_whole_FAT;
                                                  else                                          FAT_fsm <= read_fat_1;        end if;
                                              end if;
                    when read_fat_1        =>                                               FAT_fsm <= read_fat_1_wait;
                    when read_fat_1_wait   => if SD_busy = '0' and SD_read_blk = '0'  then  FAT_fsm <= read_fat_k;        end if;
                    when read_fat_k        =>                                               FAT_fsm <= read_fat_k_wait;
                    when read_fat_k_wait   => if SD_busy = '0' and SD_read_blk = '0'  then  FAT_fsm <= start_check;       end if;
                    when start_check       =>                                               FAT_fsm <= waiting_check;
                    when waiting_check     => if  FAT_check_sector_done = '1' then
                                                  if FAT_nb_fat_check = unsigned(nb_FAT)  then  FAT_fsm <= FAT_check_done;
                                                  else                                          FAT_fsm <= read_fat_1;        end if;
                                              end if;
                    when read_whole_FAT    => null;
                    when FAT_check_done    =>                                               FAT_fsm <= idle;
                    when idle              => if request_frag = '1'                   then  FAT_fsm <= frag_requested;    end if;
                    when frag_requested    =>                                               FAT_fsm <= update_frag;
                    when update_frag       =>                                               FAT_fsm <= idle;
                    when fat_issue         => if module_fsm = formating               then  FAT_fsm <= off;               end if;
                    when FAT_end           => null;
                end case;
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    -- FAT checked counter
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FAT_nb_fat_check <= x"01";
            elsif FAT_all_sectors_read = '1' and FAT_fsm = start_check then
                if FAT_nb_fat_check = unsigned(nb_FAT) then
                    FAT_nb_fat_check <= FAT_nb_fat_check;
                else
                    FAT_nb_fat_check <= FAT_nb_fat_check + 1;
                end if;
            end if;
        end if;
    end process;

    FAT_all_sectors_read <= '1' when FAT_block_num_1 = unsigned(reserved_sectors) + unsigned(nb_sectors_per_FAT) else '0';

    -----------------------------------------------------------------
    -- Here we determine which sectors will be read
    -----------------------------------------------------------------
    FAT_block_num <= FAT_block_num_k when (FAT_fsm = read_FAT_1_wait or FAT_fsm = read_fat_k) or recopy_FAT_fsm = write_FAT else FAT_block_num_1;
    -- TODO : rework this ugly process, not really readable
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' or (SD_busy = '0' and FAT_fsm = off) then
                FAT_block_num_1 <= (31 downto 16 => '0') & unsigned(reserved_sectors);
                FAT_block_num_k <= unsigned(reserved_sectors) + unsigned(nb_sectors_per_FAT);
                FAT_op_buff     <= "100";
            elsif FAT_fsm = read_fat_1_wait then
                FAT_op_buff <= "101";
            elsif FAT_fsm = read_fat_k_wait and SD_busy = '0' and SD_read_blk = '0' then
                if FAT_all_sectors_read = '1' then
                    FAT_block_num_1 <= (31 downto 16 => '0') & unsigned(reserved_sectors);
                else
                    FAT_block_num_1 <= FAT_block_num_1 + 1;
                end if;
                FAT_block_num_k <= FAT_block_num_k + 1;
                FAT_op_buff     <= "100";
            elsif FAT_fsm = waiting_check and FAT_all_sectors_read = '1' then
                FAT_block_num_1 <= (31 downto 16 => '0') & unsigned(reserved_sectors);
                FAT_op_buff     <= "100";
            
            -- Use the same signals to copy FAT when ejecting the SDcard
            elsif module_fsm = recopy_FAT and recopy_FAT_fsm = off then
                FAT_block_num_1 <= (31 downto 16 => '0') & unsigned(reserved_sectors);
                FAT_block_num_k <= unsigned(reserved_sectors) + unsigned(nb_sectors_per_FAT);
            elsif recopy_FAT_fsm = wait_read and SD_read_blk = '0' and SD_busy = '0' then
                FAT_block_num_1 <= FAT_block_num_1 + 1;
                elsif recopy_FAT_fsm = wait_write and SD_write_blk = '0' and SD_busy = '0' then
                FAT_block_num_k <= FAT_block_num_k + 1;
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    -- Here we handle the recopy of the FAT when ejecting the SDcard
    -----------------------------------------------------------------
    -- Recopy FAT FSM : This FSM is used to update the 2nd FAT of the SDcard.
    -- It simply read a sector from the FAT and write them in the other FAT without changes.
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                recopy_FAT_fsm <= off;
            else
                case recopy_FAT_fsm is
                    when off        => if module_fsm = recopy_FAT and     no_written_op                       then recopy_FAT_fsm <= recopy_end;        -- if nothing has been written on the SD card, skip this task
                                    elsif module_fsm = recopy_FAT and not no_written_op                       then recopy_FAT_fsm <= read_FAT;   end if;
                    when read_FAT   =>                                                                             recopy_FAT_fsm <= wait_read;
                    when wait_read  => if SD_read_blk = '0'  and SD_busy = '0'                                then recopy_FAT_fsm <= write_FAT;  end if;
                    when write_FAT  =>                                                                             recopy_FAT_fsm <= wait_write;
                    when wait_write => if SD_write_blk = '0' and SD_busy = '0' and FAT_all_sectors_read = '0' then recopy_FAT_fsm <= read_FAT;
                                    elsif SD_write_blk = '0' and SD_busy = '0' and FAT_all_sectors_read = '1' then recopy_FAT_fsm <= recopy_end; end if;
                    when recopy_end =>                                                                             recopy_FAT_fsm <= off;
                end case;
            end if;
        end if;
    end process;

    -- Here is remembered if somehting has been written on the SD card
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                no_written_op <= True;
            elsif SD_write_blk = '1' then
                no_written_op <= False;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- Here we compare sectors from different FAT
    -----------------------------------------------------------------
    -- Start / Stop
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FAT_check_sector_done <= '1';
                FAT_check_last_addr   <= '0';
            elsif FAT_check_last_addr = '1' then
                FAT_check_sector_done <= '1';
                FAT_check_last_addr <= '0';
            elsif FAT_address = 511 and FAT_loc_buffer = "101" then -- here we check the whole sector. We might use this condition to just check clusters :
                                                                    -- FAT_cluster_cnt = to_integer(nb_total_cluster(27 downto 0)) and FAT_loc_buffer = "101"
                FAT_check_last_addr <= '1'; -- We delay the end of the check by one clock cycle to read the last sector of the k-th FAT
            elsif FAT_fsm = start_check then
                FAT_check_sector_done <= '0';
            end if;
        end if;
    end process;
    
    -- Compare FATs sectors
    -- TODO : Check that the first byte read from FATs is media descriptor
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FAT_1_buffer    <= (others => '0');
                FAT_loc_buffer  <= "100";
                FAT_address     <= (others => '0');
                FAT_difference  <= '0';
                FAT_read_buffer <= '0';
            elsif FAT_fsm = FAT_issue then
                FAT_difference <= '0';
            elsif FAT_check_sector_done = '0' then
                if fragment_ordering = '0' then
                    if FAT_read_buffer = '0' then
                        FAT_read_buffer <= '1';
                        FAT_loc_buffer  <= "101";
                    else
                        if FAT_loc_buffer = "100" then 
                            if FAT_1_buffer /= SD_data_out then
                                FAT_difference <= '1';
                            end if;
                            FAT_loc_buffer  <= "101";
                        else
                            FAT_loc_buffer <= "100";
                            FAT_address    <= FAT_address + 1;
                            FAT_1_buffer   <= SD_data_out;
                        end if;
                    end if;
                else
                    FAT_read_buffer <= '0';
                end if;
            else
                FAT_1_buffer    <= (others => '0');
                FAT_loc_buffer  <= "100";
                FAT_address     <= (others => '0');
                FAT_difference  <= '0';
                FAT_read_buffer <= '0';
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    -- Here we find free clusters in the FAT
    -----------------------------------------------------------------
    -- Here we reconstruct clusters from single bytes
    -- TODO / OPTIMIZE : - use FAT_1_buffer and an impulse signal(delayed of 1 clk cycle of FAT_loc_buffer = "101")
    --                     to reduce fanout(?) on SD_data_out
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FAT_cluster   <= (others => '0');
            elsif FAT_loc_buffer = "101" and FAT_check_sector_done = '0' and fragment_ordering = '0' then -- we must test that FAT_check_sector_done is low to not read one more byte per sector (513 instead of 512)
                FAT_cluster <= SD_data_out & FAT_cluster(31 downto 8);
            end if;
        end if;
    end process;

    -- Cluster counter process
    -- FIXME : - Not the right value for free cluster (we lose some in the process, but how ??)
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FAT_bytes_cnt       <= 0;
                FAT_cluster_cnt     <= (others => '0');
                FAT_cluster_cnt_dly <= '0';
                FAT_new_cluster     <= '0';
            elsif module_fsm = fat and FAT_loc_buffer = "101" and FAT_store_last_chain = '0' and fragment_ordering = '0' and FAT_check_sector_done = '0' then  -- should be an impulse
                if FAT_bytes_cnt = 3 then
                    FAT_bytes_cnt   <= 0;
                    if FAT_cluster_cnt_dly = '0' then
                        FAT_cluster_cnt_dly <= '1';
                    else
                        FAT_cluster_cnt <= FAT_cluster_cnt + 1;
                    end if;
                    FAT_new_cluster <= '1';
                else
                    FAT_bytes_cnt   <= FAT_bytes_cnt + 1;
                    FAT_new_cluster <= '0';
                end if;
            else
                FAT_new_cluster <= '0';
            end if;
        end if;
    end process;


    -- Here the chain of consecutive free clusters is managed
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FAT_free_clusters_start <= (others => '0');
                FAT_free_clusters_cnt   <= (others => '0');
                FAT_nb_free_clusters    <= 0;           -- Maybe don't put this here but in a separate process, or add a test depending on module_fsm's state
                store_new_fragment      <= '0';
                FAT_new_chain_to_store      <= '0';
                FAT_store_last_chain    <= '0';
            elsif module_fsm = fat and FAT_store_last_chain = '0' and FAT_cluster_cnt = to_integer(nb_total_cluster(27 downto 0)) then -- when we have read the last cluster, we store the last chain if it exist
                FAT_store_last_chain <= '1';
                if FAT_free_clusters_cnt /= 0 then 
                    FAT_chain_to_store     <= std_logic_vector(FAT_free_clusters_cnt) & FAT_free_clusters_start;
                    store_new_fragment <= '1';
                    FAT_new_chain_to_store <= '1';
                else
                    store_new_fragment <= '0';
                    FAT_new_chain_to_store <= '0';
                end if;
                FAT_free_clusters_cnt <= (others => '0');
            elsif FAT_new_cluster = '1' and FAT_cluster_cnt >= 2 then -- and FAT_free_clusters_cnt < (28 downto 0 => '1') ???  and FAT_cluster_cnt >= 2 => useless test ?
                if FAT_cluster = x"00000000" then
                    FAT_nb_free_clusters   <= FAT_nb_free_clusters + 1;
                    FAT_free_clusters_cnt  <= FAT_free_clusters_cnt + 1;
                    store_new_fragment <= '0';
                    FAT_new_chain_to_store <= '0';
                    if FAT_free_clusters_cnt = 0 then
                        FAT_free_clusters_start <= std_logic_vector(FAT_cluster_cnt);
                    end if;
                else
                    if FAT_free_clusters_cnt /= 0 then 
                        FAT_chain_to_store <= std_logic_vector(FAT_free_clusters_cnt) & FAT_free_clusters_start;
                        store_new_fragment <= '1';
                        FAT_new_chain_to_store <= '1';
                    else
                        FAT_new_chain_to_store <= '0';
                        store_new_fragment <= '0';
                    end if;
                    FAT_free_clusters_cnt <= (others => '0');
                end if;
            else
                store_new_fragment <= '0';
                FAT_new_chain_to_store <= '0';
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    -- Here we store free clusters chains in a sorted table (sort of FIFO)
    -----------------------------------------------------------------
    -- FIXME : clear the table after reset / synced_CD (during boot_record or initialisation for example)
    -- OPTIMIZATION : new_free_frag could be remove (save FF) if we are sure that FAT_chain_to_store wont change during the process (i.e. ~512 clk cycles), but that's not the case now
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                fragment_ordering <= '0';
                new_free_frag     <= (others => '0');   -- useless
            elsif new_free_frag_inserted = '1' then
                fragment_ordering <= '0';
            elsif store_new_fragment = '1' then
                fragment_ordering <= '1';
                new_free_frag     <= FAT_chain_to_store;
            end if;
        end if;
    end process;

    free_frag_table_full <= free_frag_table_size = FREE_FRAGMENT_TABLE_SIZE;
    -- update the number of fragment stored at the moment it as been inserted
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                free_frag_table_size <= 0;
            elsif new_free_frag_inserted = '1' then
                if free_frag_table_size = FREE_FRAGMENT_TABLE_SIZE then
                    free_frag_table_size <= FREE_FRAGMENT_TABLE_SIZE;
                else
                    free_frag_table_size <= free_frag_table_size + 1;
                end if;
            end if;
        end if;
    end process;


    -- Data to write in the free fragment table : it chooses the shortest fragment between a fragment from the table and the fragment to store
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then -- useless ?
                free_frag_to_write     <= (others => '0');
                new_free_frag_inserted <= '0';
            elsif fragment_ordering = '1' and new_free_frag_inserted = '0' then
                if free_frag_table_size = 0 then -- 1st fragment of the table
                    free_frag_to_write     <= new_free_frag;
                    new_free_frag_inserted <= '1';
                -- elsif free_frag_table_size = 1 then -- 2nd fragment of the table

                -- elsif free_frag_table_size = 2 then -- 3rd fragment of the table
                
                -- not complete
                elsif free_frag_read_ptr = 0 and free_frag_write_ptr = 0   then  -- case were we store the biggest fragment
                    free_frag_to_write     <= new_free_frag;
                    new_free_frag_inserted <= '1';
                -- not complete
                -- elsif free_frag_read_ptr = 0 and free_frag_write_ptr = 1   then  -- case were we store the biggest fragment
                --     free_frag_to_write     <= new_free_frag;
                --     new_free_frag_inserted <= '1';
                
                elsif free_frag_read_ptr < free_frag_table_size-1 then                                          -- others case where we are not on "edges"
                    if free_frag_read(FREE_FRAG_SIZE-1 downto 28) < new_free_frag(FREE_FRAG_SIZE-1 downto 28) then
                        free_frag_to_write <= free_frag_read;
                    else
                        free_frag_to_write     <= new_free_frag;
                        new_free_frag_inserted <= '1';
                    end if;
                else 
                    new_free_frag_inserted <= '0';
                end if;
            
            -- Giving & updating fragments
            elsif FAT_fsm = frag_requested then
                if request_frag_size < current_frag_size then
                    free_frag_to_write <= std_logic_vector(current_frag_size - request_frag_size) & std_logic_vector(current_frag_cluster + request_frag_size);
                else
                    free_frag_to_write <= (others => '0');  -- clear memory input (useless?)
                end if;
            else
                new_free_frag_inserted <= '0';
            end if;
        end if;
    end process;


    -- Read in free fragment memory pointer
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                free_frag_read_ptr <= 0;
            elsif FAT_fsm = FAT_check_done then  -- reset pointer after finding every fragments
                free_frag_read_ptr  <= 0;
            elsif FAT_fsm = waiting_check then
                if fragment_ordering = '1' or store_new_fragment = '1' then
                    if free_frag_read_ptr = 0 then
                        free_frag_read_ptr <= 0;
                    else
                        free_frag_read_ptr <= free_frag_read_ptr - 1;
                    end if;
                else
                    if free_frag_table_size = 0 then
                        free_frag_read_ptr  <= 0;
                    else
                        free_frag_read_ptr <= free_frag_table_size-1;
                    end if;
                end if;
            elsif FAT_fsm = update_frag then
                if request_frag_size >= current_frag_size then
                    free_frag_read_ptr <= free_frag_read_ptr + 1;
                end if;
            end if;
        end if;
    end process;

    -- Write in free fragment memory pointer
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                free_frag_write_ptr <= 0;
            elsif FAT_fsm = FAT_check_done then  -- reset pointer after finding every fragments
                free_frag_write_ptr  <= 0;
            elsif FAT_fsm = waiting_check then
                if free_frag_write_en = '1' then
                    if free_frag_write_ptr > 0 then
                        free_frag_write_ptr <= free_frag_write_ptr - 1;
                    end if;
                else
                    if free_frag_table_full then
                        free_frag_write_ptr <= free_frag_table_size-1;
                    else
                        free_frag_write_ptr <= free_frag_table_size;
                    end if;
                end if;
            elsif FAT_fsm = idle then
                free_frag_write_ptr <= free_frag_read_ptr;
            end if;
        end if;
    end process;

    -- delay writing for 1 clk cycle when table is full is determine here
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                free_frag_write_dly <= '0';
            elsif new_free_frag_inserted = '1' then
                free_frag_write_dly <= '0';
            elsif free_frag_table_full then
                if fragment_ordering = '1' then
                    free_frag_write_dly <= '1';
                else
                    free_frag_write_dly <= '0';
                end if;
            elsif store_new_fragment = '1' then
                free_frag_write_dly <= '1';
            end if;
        end if;
    end process;

    -- here we enable write for the free fragment memory
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                free_frag_write_en <= '0';
            elsif new_free_frag_inserted = '0' and free_frag_write_dly = '1' then   -- fragment_ordering = '1' test is now useless
                free_frag_write_en <= '1';
            elsif FAT_fsm = update_frag then
                free_frag_write_en <= '1';
            else
                free_frag_write_en <= '0';
            end if;
        end if;
    end process;

    current_frag_cluster <= unsigned(free_frag_read(MAX_NB_CLUSTER_PW2-1 downto 0));
    current_frag_size    <= unsigned(free_frag_read(FREE_FRAG_SIZE-1 downto FREE_FRAG_SIZE-MAX_NB_CONSECUTIVE_CLUSTER_PW2));
    -- Access to the free fragments memory
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                free_frag_read <= (others => '0');
            else
                free_frag_read <= free_frag_table(free_frag_read_ptr);
                if free_frag_write_en = '1' then
                    free_frag_table(free_frag_write_ptr) <= free_frag_to_write;
                end if;
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                request_frag      <= '0';
                request_frag_size <= (others => '0');
            elsif SDm_fsm = give_free_frag then
                request_frag      <= '1';
                request_frag_size <= unsigned(file_out(file_id_op).reserve_clusters_out);
            elsif check_fd_fsm = create_dir and create_dir_fsm = off then
                request_frag      <= '1';
                request_frag_size <= to_unsigned(1, 22);
            elsif request_frag = '1' then
                request_frag <= '0';
            end if;
        end if;
    end process;

    -- Concatenate cluster and size to send it to file manager
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                frag_out.cluster <= (others => '0');
                frag_out.size    <= (others => '0');
            elsif FAT_fsm = frag_requested then
                if unsigned(request_frag_size) < current_frag_size then
                    frag_out.cluster <= std_logic_vector(current_frag_cluster);
                    frag_out.size    <= request_frag_size;
                    -- frag_out <= std_logic_vector(current_frag_cluster) & std_logic_vector(request_frag_size);
                else
                    frag_out.cluster <= std_logic_vector(current_frag_cluster);
                    frag_out.size    <= current_frag_size(21 downto 0);
                    -- frag_out <= std_logic_vector(current_frag_cluster & current_frag_size(21 downto 0));
                end if;
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    --
    -- cluster/fragment to write in FAT when closing a file
    --
    -----------------------------------------------------------------
    -- FSM
    -- MAYBE FIXME : - Problem if we the first fragment we receive has a size of 0. This should not happen since the file manager ask new fragment when
    --                 it needs to write data, it means the minimum size should be 1. But beware of potential changes on this point.
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                frag_fsm <= off;
            else
                case frag_fsm is
                    when off            => if module_fsm = close_fd and file_out(file_id_user).write_clusters_chain = '1' and SD_busy = '0' then frag_fsm <= get_fragment;   end if;
                    when get_fragment   =>                                                                                                        frag_fsm <= is_over;
                    when is_over        => if frag_size = 0                                                                                  then frag_fsm <= off;
                                           else                                                                                                   frag_fsm <= read_sector;    end if;
                    when read_sector    =>                                                                                                        frag_fsm <= wait_read;
                    when wait_read      => if SD_busy = '0' and SD_read_blk = '0'                                                            then frag_fsm <= write_fragment; end if;
                    when write_fragment => if frag_out_of_sector or frag_size = 0                                                            then frag_fsm <= write_sector;   end if;
                    when write_sector   =>                                                                                                        frag_fsm <= wait_write;
                    when wait_write     => if SD_busy = '0' and SD_write_blk = '0'                                                           then 
                                               if frag_size = 0 then frag_fsm <= get_fragment;
                                               else                  frag_fsm <= read_sector;  end if; end if; -- we go here to update the sector num as it is done when frag_fsm = is_over
                end case;
            end if;
        end if;
    end process;


    -- First copy the fragment to write, and then update the size of the remaining fragment until it has a null size
    Process(clk)
    begin
        if rising_edge(clk) then
            if frag_fsm = off then
                frag_start_cluster <= (others => '0');
                frag_size          <= (others => '0');
            elsif frag_fsm = get_fragment then
                frag_start_cluster <= file_out(file_id_user).fragment_out.cluster;
                frag_size          <= file_out(file_id_user).fragment_out.size;
            -- elsif frag_fsm = write_fragment and frag_bytes_cnt = 2 then             -- update at 1 to just test frag_size = 0 without adding any delay ?
            elsif frag_fsm = write_fragment and frag_bytes_cnt = 1 then -- reverse order (3->0) -- update at 1 to just test frag_size = 0 without adding any delay ?
                frag_size <= frag_size - 1;
            end if;
        end if;
    end process;

    -- Bytes counter
    Process(clk)
    begin
        if rising_edge(clk) then
            if frag_fsm /= write_fragment then
                -- frag_bytes_cnt <= 0;
                frag_bytes_cnt <= 3;                         -- reverse order (3->0)
            elsif frag_fsm = write_fragment then
                -- if frag_bytes_cnt = 3 then
                if frag_bytes_cnt = 0 then                   -- reverse order (3->0)
                    -- frag_bytes_cnt <= 0;
                    frag_bytes_cnt <= 3;                     -- reverse order (3->0)
                else
                    -- frag_bytes_cnt <= frag_bytes_cnt + 1;
                    frag_bytes_cnt <= frag_bytes_cnt - 1;    -- reverse order (3->0)
                end if;
            end if;
        end if;
    end process;

    -- Sector number to read / write back
    -- It is updated only when we need to read a new sector, as the frag_size signal is updated over time
    Process(clk)
        variable last_cluster_of_fragment : unsigned(27 downto 0);
    begin
        if rising_edge(clk) then
            if frag_fsm = is_over or (frag_fsm = wait_write and SD_busy = '0' and SD_write_blk = '0') then  -- ADD a test on frag_size = 0 ? Shouldn't bother as new fragment will be fetch if it's 0
                last_cluster_of_fragment := unsigned(frag_start_cluster) + (frag_size - 1);
                frag_block_num <= std_logic_vector(to_unsigned(to_integer(unsigned(reserved_sectors) + last_cluster_of_fragment(27 downto 7)), 32));--unsigned(frag_start_cluster(27 downto 7)) + size_minus1(21 downto 7)), 32));
            -- elsif frag_fsm = wait_write and SD_busy = '0' and SD_write_blk = '0' then
            --     frag_block_num <= std_logic_vector(to_unsigned(to_integer(unsigned(reserved_sectors) + frag_cluster_nb(27 downto 7)), 32));
            end if;
        end if;
    end process;

    -- updating the address
    frag_buff_addr <= std_logic_vector(frag_cluster_nb(6 downto 0) & to_unsigned(frag_bytes_cnt, 2));
    
    -- check if we are at the bottom edge of a sector
    -- TODO : - Check if this really works as intended
    Process(clk)
    begin
        if rising_edge(clk) then
            if frag_fsm /= write_fragment then
                frag_out_of_sector <= False;
            -- elsif frag_fsm = write_fragment and unsigned(frag_buff_addr) = 2 then
            elsif frag_fsm = write_fragment and unsigned(frag_buff_addr) = 1 then       -- reverse order (3->0)
                frag_out_of_sector <= True;
            end if;
        end if;
    end process;


    -- Values of the cluster chain is determined here, with the end value "0FFFFFFF" being the first to be written into the FAT, as we are writting from the last to the first cluster 
    -- frag_next_cluster_B <= frag_next_cluster(7 downto 0);
    frag_next_cluster_B <= frag_next_cluster(31 downto 24);       -- reverse order (3->0)
    Process(clk)
    begin
        if rising_edge(clk) then
            if frag_fsm = off then
                frag_next_cluster <= x"0FFFFFFF";
                frag_cluster_nb   <= (others => '0');
            elsif frag_fsm = wait_read then
                    frag_cluster_nb <= unsigned(frag_start_cluster) + frag_size - 1;
            elsif frag_fsm = write_fragment then
                -- if frag_bytes_cnt = 3 then
                if frag_bytes_cnt = 0 then                                                   -- reverse order (3->0)
                    frag_next_cluster <= "0000" & std_logic_vector(frag_cluster_nb);
                    frag_cluster_nb   <= frag_cluster_nb - 1;
                else 
                    -- frag_next_cluster(23 downto 0) <= frag_next_cluster(31 downto 8);
                    frag_next_cluster(31 downto 8) <= frag_next_cluster(23 downto 0);        -- reverse order (3->0)
                end if;
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    --
    --                   Checking file existence
    --
    -----------------------------------------------------------------
    -- TODO : - add directory manegement (search & open & create)
    --        - check if free space in folder has been found before trying to write the file entry
    --        - divide this FSM into smaller part to ease synthesis
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                check_fd_fsm <= off;
            else
                case check_fd_fsm is
                    when off               => if module_fsm = open_fd and SDm_fsm = idle then  check_fd_fsm <= load_directory;    end if;
                    when load_directory    =>                                               check_fd_fsm <= wait_directory;
                    when wait_directory    => if SD_busy = '0' and SD_read_blk = '0'  then  check_fd_fsm <= start_check;       end if;
                    when start_check       => if FD_bytes_cnt = 0                     then  check_fd_fsm <= check_filename;    end if;
                    when check_filename    => if FD_name_diff                         then    -- === not the good filename
                                                  if FD_fd_addr < 15                                      then  check_fd_fsm <= next_file;
                                               elsif FD_sector_in_clust = unsigned(sectors_per_cluster)-1 then  check_fd_fsm <= read_dir_fat;
                                                else                                                            check_fd_fsm <= next_sector;   end if;
                                           elsif FD_bytes_cnt = 0                     then    -- === if filename found
                                                  if FD_rw_mode = '0'                                     then  check_fd_fsm <= check_attribute;
                                                  else                                                          check_fd_fsm <= check_over;    end if;
                                          end if;
                    when check_attribute   => if FD_attribute(5) = not FD_file_type   then  check_fd_fsm <= get_start_cluster;      -- file to read found
                                           elsif FD_attribute(4) =     FD_file_type   then  check_fd_fsm <= get_start_cluster;
                                            else                                            check_fd_fsm <= check_over;        end if;
                    when next_file         =>                                               check_fd_fsm <= start_check;
                    when next_sector       =>                                               check_fd_fsm <= load_directory;
                    when read_dir_fat      =>                                               check_fd_fsm <= wait_dir_fat;
                    when wait_dir_fat      => if SD_busy = '0' and SD_read_blk = '0'  then  check_fd_fsm <= get_next_cluster;  end if;
                    when get_next_cluster  => if FD_bytes_cnt = 0                     then  check_fd_fsm <= next_cluster;      end if;
                    when next_cluster      => if FD_next_cluster >= x"FFFFFF8"        then  check_fd_fsm <= end_of_dir;
                                              elsif calc_sector_bsy = '0'             then  check_fd_fsm <= calc_next_cluster; end if;
                    when calc_next_cluster => if block_num_rdy = '1'                  then  check_fd_fsm <= load_directory;    end if;
                    when end_of_dir        => if FD_rw_mode='1' and free_fd_found='1' then
                                                  if   FD_file_type = '0' then  check_fd_fsm <= read_dir_sector;            -- file can be created
                                                  else                          check_fd_fsm <= create_dir;      end if;
                                           elsif FD_rw_mode='1' and free_fd_found='0' then  check_fd_fsm <= append_directory;    -- not implemented yet
                                              else                                          check_fd_fsm <= check_over;        end if;  -- file note found
                    --read
                    when get_start_cluster => if FD_bytes_cnt = 0 and FD_file_type='0'then  check_fd_fsm <= get_size;
                                           elsif FD_bytes_cnt = 0 and FD_file_type='1'then  check_fd_fsm <= open_directory;    end if;  -- open_directory
                    when get_size          => if FD_bytes_cnt = 0                     then  check_fd_fsm <= is_file_empty;     end if;
                    when is_file_empty     => if unsigned(FD_size) = 0                then  check_fd_fsm <= check_over;
                                              else                                          check_fd_fsm <= read_fat;          end if;
                    when read_fat          =>                                               check_fd_fsm <= wait_fat;
                    when wait_fat          => if SD_busy = '0' and SD_read_blk = '0'  then  check_fd_fsm <= list_clusters;     end if;
                    when list_clusters     => if FD_bytes_cnt = 0                     then  check_fd_fsm <= list_clust_dly;    end if;
                    when list_clust_dly    =>                                               check_fd_fsm <= is_list_done;
                    when is_list_done      => if FD_next_cluster >= x"FFFFFF8"        then  check_fd_fsm <= check_over;
                                           elsif FD_cant_list_all_frag = '1'          then  check_fd_fsm <= check_over;
                                           elsif diff_sector = '1'                    then  check_fd_fsm <= read_fat;
                                            else                                            check_fd_fsm <= list_clusters;     end if;
                    -- write
                    when create_dir        => if create_dir_fsm = done               then  check_fd_fsm <= read_dir_sector;   end if;
                    when read_dir_sector   =>                                               check_fd_fsm <= wait_dir_sector;
                    when wait_dir_sector   => if SD_busy = '0' and SD_read_blk = '0'  then  check_fd_fsm <= create_file;       end if;
                    when append_directory  => check_fd_fsm <= check_over;
                    when create_file       => if FD_bytes_cnt = 0                     then  check_fd_fsm <= write_sector_back; end if;
                    when write_sector_back =>                                               check_fd_fsm <= wait_write_end;
                    when wait_write_end    => if SD_busy = '0' and SD_write_blk = '0' then  check_fd_fsm <= check_over;        end if;

                    when open_directory    => if block_num_rdy = '1'                  then  check_fd_fsm <= check_over;        end if;
                    when check_over        =>                                               check_fd_fsm <= off;
                end case;
            end if;
        end if;
    end process;

    -- TODO : - add possibility to save FF by maintaining FD_name at the input of the module
    --        - check what are the differences between this process and the next (commentated)
    Process(clk)
    begin
        if rising_edge(clk) then
            if busy_i = '0' and not(format = '1' or reset = '1' or eject_card = '1') then
                if open_file = '1' then
                    FD_name        <= filename;
                    FD_rw_mode     <= read_write;
                    FD_file_type   <= '0';
                    reserv_clust_i <= reserv_clust;
                    date_time_buff <= date_time;
                elsif open_dir = '1' then
                    FD_name        <= filename;
                    FD_rw_mode     <= read_write;
                    FD_file_type   <= '1';
                    reserv_clust_i <= reserv_clust;
                    date_time_buff <= date_time;
                end if;
            end if;
        end if;
    end process;
    -- Process(clk)
    -- begin
    --     if rising_edge(clk) then
    --         if open_file = '1' or open_dir = '1' then
    --             FD_name        <= filename;
    --             FD_rw_mode     <= read_write;
    --             reserv_clust_i <= reserv_clust;
    --             date_time_buff <= date_time;
    --             if open_file = '1' then
    --                 FD_file_type   <= '0';
    --             else
    --                 FD_file_type   <= '1';
    --             end if;
    --             -- or even
    --             FD_file_type <= open_dir;
    --         end if;
    --     end if;
    -- end process;

    -- file_in(file_id_user).filename             <= FD_name;
    -- file_in(file_id_user).read_write_in        <= FD_rw_mode;
    -- file_in(file_id_user).reserve_clusters_in  <= reserv_clust_i;

    -- file_in(file_id_user).open_file            <= FD_op_success;


    -- getting data out of SDcard module
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_name_diff   <= False;
                FD_name_buffer <= (others => '0');
            elsif check_fd_fsm = wait_directory or check_fd_fsm = next_file then
                FD_name_buffer <= FD_name(87 downto 0);
                FD_name_diff   <= False;
            elsif check_fd_fsm = check_filename and FD_bytes_cnt = 0 then
                FD_attribute <= SD_data_out;
            elsif check_fd_fsm = check_filename or (check_fd_fsm = start_check and FD_bytes_cnt = 0) then
                if SD_data_out /= FD_name_buffer(87 downto 80) then
                    FD_name_diff   <= True;
                end if;
                FD_name_buffer <= FD_name_buffer(79 downto 0) &  (7 downto 0 => '0');
            elsif check_fd_fsm = get_start_cluster then
                FD_start_clust <= SD_data_out & FD_start_clust(31 downto 8);
            elsif check_fd_fsm = get_size then
                FD_size <= SD_data_out & FD_size(31 downto 8);
            end if;
        end if;
    end process;

    -- informing the design (module FSM at least) the result of the operation
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_op_success <= '0';
            elsif check_fd_fsm = off then
                FD_op_success <= '0';
            elsif FD_op_success = '0' and (check_fd_fsm = get_start_cluster or check_fd_fsm = create_file) then
                FD_op_success <= '1';
            end if;
        end if;
    end process;

    -- Here we find the first free file descriptor we encounter
    -- FIXME : - free file descriptor found when the first byte of the fd is 0x00. This might be wrong, even if filename shoudln't have this value
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' or check_fd_fsm = off then
                free_fd_found   <= '0';
                free_fd_sector  <= (others => '0');
                free_fd_address <= (others => '0');
            elsif check_fd_fsm = start_check and FD_bytes_cnt = 0 and free_fd_found = '0' then
                if SD_data_out = x"e5" or SD_data_out = x"00" then
                    free_fd_found   <= '1';
                    free_fd_address <= std_logic_vector(FD_fd_addr);
                    free_fd_sector  <= SD_block_num;
                end if;
            end if;
        end if;
    end process;

    -- Byte counter manager
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_bytes_cnt <= 0;
            elsif check_fd_fsm = wait_directory or check_fd_fsm = next_file then
                FD_bytes_cnt <= 1;
            elsif check_fd_fsm = wait_dir_fat or check_fd_fsm = wait_fat or check_fd_fsm = is_list_done then
                FD_bytes_cnt <= 3;
            elsif check_fd_fsm = wait_dir_sector then
                FD_bytes_cnt <= 31;
            elsif FD_bytes_cnt = 0 then
                case check_fd_fsm is
                    when wait_directory | next_file => FD_bytes_cnt <=  1;
                    when start_check                => FD_bytes_cnt <= 10;
                    when check_filename             => FD_bytes_cnt <=  0;
                    when check_attribute            => FD_bytes_cnt <=  3;
                    when get_start_cluster          => FD_bytes_cnt <=  3;
                    -- when wait_dir_fat               => FD_bytes_cnt <=  3;

                    -- Create file descriptor
                    when others                     => FD_bytes_cnt <=  0;
                end case;
            else
                FD_bytes_cnt <= FD_bytes_cnt - 1;
            end if;
        end if;
    end process;

    -- Here we address byte in focused file descriptor
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_intern_addr <= (others => '0');
            else
                case check_fd_fsm is
                    when wait_directory | next_file =>                          FD_intern_addr <= '0' & x"0";
                    when start_check                =>                          FD_intern_addr <= FD_intern_addr + 1;
                    when check_filename             =>                          -- FD_intern_addr <= FD_intern_addr + 1;
                                                       if FD_bytes_cnt = 0 then FD_intern_addr <= '1' & x"A";
                                                       else                     FD_intern_addr <= FD_intern_addr + 1; end if;
                    when check_attribute            =>                          FD_intern_addr <= FD_intern_addr + 1;
                    -- we need a 2-bit counter to get the 4 bytes of the next cluster number in the fat, so we use this counter 
                    -- and start counting 1 clk cycle before taking data to start "pipeline"
                    when wait_dir_fat               => if SD_busy = '1'    then FD_intern_addr <= '0' & x"0";
                                                       else                     FD_intern_addr <= FD_intern_addr + 1; end if;
                    when get_next_cluster           =>                          FD_intern_addr <= FD_intern_addr + 1;
                    when get_start_cluster          => if FD_bytes_cnt = 3 then FD_intern_addr <= '1' & x"4";
                                                    elsif FD_bytes_cnt = 1 then FD_intern_addr <= '1' & x"C";
                                                     else                       FD_intern_addr <= FD_intern_addr + 1; end if;
                    when get_size                   =>                          FD_intern_addr <= FD_intern_addr + 1;
                    -- we need a 2-bit counter to get the 4 bytes of the next cluster number in the fat, so we use this counter 
                    -- and start counting 1 clk cycle before taking data to start "pipeline"
                    when wait_fat                   => if SD_busy = '1'    then FD_intern_addr <= '0' & x"0";
                                                       else                     FD_intern_addr <= FD_intern_addr + 1; end if;
                    when list_clusters              => if FD_bytes_cnt = 0 then FD_intern_addr <= '0' & x"0";
                                                       else                     FD_intern_addr <= FD_intern_addr + 1; end if;
                    when is_list_done               =>                          FD_intern_addr <= FD_intern_addr + 1;

                    -- Create file descriptor
                    when wait_dir_sector            =>                          FD_intern_addr <= '0' & x"0";
                    when create_file                =>                          FD_intern_addr <= FD_intern_addr + 1;
                    when others                     =>                          FD_intern_addr <= '0' & x"0";
                end case;
            end if;
        end if;
    end process;


    -- file descriptor content is manage here
    -- TODO : - add directory
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_data <= (others => '0');
            elsif check_fd_fsm = wait_directory then
                FD_data_buffer <= FD_name(79 downto  0);
                FD_data        <= FD_name(87 downto 80);
            elsif check_fd_fsm = create_file then
                if FD_bytes_cnt > 21 then                                                   -- name
                    FD_data_buffer <= FD_data_buffer(71 downto 0) & (7 downto 0 => '0');
                    FD_data <= FD_data_buffer(79 downto 72);
                elsif FD_bytes_cnt = 21 then                                                -- attributes
                    if FD_file_type = '0' then
                        FD_data <= "00100000";  -- archive
                    else
                        FD_data <= "00010000";  -- directory
                    end if;
                elsif FD_bytes_cnt = 20 then                                                -- NT
                    FD_data <= "00000000";
                    FD_data_buffer(39 downto 0) <= date_time_buff;

                elsif FD_bytes_cnt > 11 then                                                -- created time
                    if FD_bytes_cnt = 12 and FD_file_type = '1' then
                        FD_data <= "0000" & c_dir_cluster_nb(27 downto 24);
                        FD_data_buffer(39 downto 0) <= c_dir_cluster_nb(23 downto 16) & (31 downto 0 => '0');
                    else
                        FD_data <= FD_data_buffer(39 downto 32);
                        FD_data_buffer(39 downto 0) <= FD_data_buffer(31 downto 0) & (7 downto 0 => '0');
                    end if;

                elsif FD_bytes_cnt = 11 or FD_bytes_cnt = 10 then                           -- start cluster (MSB)
                    if FD_file_type = '1' then
                        FD_data <= FD_data_buffer(39 downto 32);
                        if FD_bytes_cnt = 10 then
                            FD_data_buffer(31 downto 0) <= date_time_buff(31 downto 0);
                        else
                            FD_data_buffer(39 downto 0) <= FD_data_buffer(31 downto 0)  & (7 downto 0 => '0');
                        end if;
                    else
                        FD_data <= "00000000";
                        FD_data_buffer(31 downto 0) <= date_time_buff(31 downto 0);
                    end if;

                elsif FD_bytes_cnt = 9 or FD_bytes_cnt = 8 then                             -- time of last write to file
                    FD_data <= FD_data_buffer(31 downto 24);
                    FD_data_buffer(31 downto 0) <= FD_data_buffer(23 downto 0) & (7 downto 0 => '0');

                elsif FD_bytes_cnt = 7 or FD_bytes_cnt = 6 then                             -- date of last write to file
                    if FD_bytes_cnt = 6 and FD_file_type = '1' then
                        FD_data <= c_dir_cluster_nb(7 downto 0);
                        FD_data_buffer(39 downto 0) <= c_dir_cluster_nb(15 downto 8) & (31 downto 0 => '0');
                    else
                        FD_data <= FD_data_buffer(31 downto 24);
                        FD_data_buffer(31 downto 0) <= FD_data_buffer(23 downto 0) & (7 downto 0 => '0');
                    end if;

                elsif FD_bytes_cnt = 5 or FD_bytes_cnt = 4 then                             -- start cluster (LSB)
                    if FD_file_type = '1' then
                        FD_data <= FD_data_buffer(39 downto 32);
                        FD_data_buffer(39 downto 0) <= FD_data_buffer(31 downto 0)  & (7 downto 0 => '0');
                    else
                        FD_data <= "00000000";
                    end if;
                else                                                                        -- file size
                    FD_data <= "00000000";
                end if;
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    -- Cluster / sector / fd adress management
    -----------------------------------------------------------------
    -- building address to read SDcard buffers
    Process(check_fd_fsm, FD_current_cluster, FD_fd_addr, FD_intern_addr, FD_buff_addr, free_fd_address)
    begin
        case check_fd_fsm is
            when wait_dir_fat | get_next_cluster |
                 wait_fat     | list_clusters    | list_clust_dly | is_list_done => FD_buff_addr <= FD_current_cluster(6 downto 0) & std_logic_vector(FD_intern_addr(1 downto 0));
            when create_file                                                     => FD_buff_addr <= free_fd_address & std_logic_vector(FD_intern_addr);
            when others                                                          => FD_buff_addr <= std_logic_vector(FD_fd_addr & FD_intern_addr);
        end case;
    end process;

    -- loc_buffer and op_buffer "management"
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_loc_buffer <= "110";
            elsif check_fd_fsm = wait_directory or check_fd_fsm = load_directory then
                FD_loc_buffer <= "110";
            elsif check_fd_fsm = read_dir_fat or check_fd_fsm = read_fat then
                FD_loc_buffer <= "101";
            elsif check_fd_fsm = end_of_dir then
                FD_loc_buffer <= "110";
            end if;
        end if;
    end process;

    -- file descriptor counter
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_fd_addr <= (others => '0');
            elsif check_fd_fsm = off or check_fd_fsm = next_sector or check_fd_fsm = next_cluster then
                FD_fd_addr <= (others => '0');
            elsif check_fd_fsm = next_file then
                FD_fd_addr <= FD_fd_addr + 1;
            end if;
        end if;
    end process;

    -- sector in cluster counter
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_sector_in_clust <= (others => '0');
            elsif check_fd_fsm = off or check_fd_fsm = next_cluster then
                FD_sector_in_clust <= (others => '0');
            elsif check_fd_fsm = next_sector then
                FD_sector_in_clust <= FD_sector_in_clust + 1;
            end if;
        end if;
    end process;

    -- determine the next cluster to read
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_next_cluster <= (others => '0');
            elsif check_fd_fsm = off then
                FD_next_cluster <= tree_structure_cluster(tree_depth);
            elsif check_fd_fsm = is_file_empty then
                FD_next_cluster <= FD_start_clust(27 downto 0);
            elsif check_fd_fsm = get_next_cluster or check_fd_fsm = list_clusters then
                if FD_bytes_cnt = 0 then
                    FD_next_cluster <= SD_data_out(3 downto 0) & FD_next_cluster(27 downto 4);
                else
                    FD_next_cluster <= SD_data_out & FD_next_cluster(27 downto 8);
                end if;
            end if;
        end if;
    end process;

    -- update the current cluster
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_current_cluster <= (others => '0');
            elsif (check_fd_fsm = wait_directory and SD_read_blk = '1') or check_fd_fsm = wait_fat or check_fd_fsm = list_clust_dly then
                FD_current_cluster <= FD_next_cluster;
            end if;
        end if;
    end process;


    -- determine if the next cluster (of the cluster chain) is in the same sector as the current
    Process(clk)
    begin
        if rising_edge(clk) then
            if check_fd_fsm = list_clust_dly and FD_current_cluster(27 downto 7) /= FD_next_cluster(27 downto 7) then
                diff_sector <= '1';
            else
                diff_sector <= '0';
            end if;
        end if;
    end process;

    -- the block to read is updated here
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                FD_block_num <= (others => '0');
            elsif check_fd_fsm = off then
                FD_block_num <= unsigned(tree_structure(tree_depth));
            elsif check_fd_fsm = next_sector then
                FD_block_num <= FD_block_num + 1;
            elsif check_fd_fsm = calc_next_cluster and block_num_rdy = '1'then
                FD_block_num <= block_num;
            elsif (check_fd_fsm = check_filename and FD_sector_in_clust = unsigned(sectors_per_cluster)-1 and FD_name_diff) or check_fd_fsm = read_dir_fat then
                FD_block_num <= to_unsigned(to_integer(unsigned(reserved_sectors) + unsigned(FD_current_cluster(27 downto 7))), 32);
            elsif check_fd_fsm = get_size then
                FD_block_num <= to_unsigned(to_integer(unsigned(reserved_sectors) + unsigned(FD_start_clust(27 downto 7))), 32);
            elsif check_fd_fsm = is_list_done then  
                FD_block_num <= to_unsigned(to_integer(unsigned(reserved_sectors) + unsigned(FD_next_cluster(27 downto 7))), 32);
            end if;
        end if;
    end process;


    -- Collect files' fragment (read only)
    Process(clk)
    begin
        if rising_edge(clk) then
            if check_fd_fsm = get_size then
                FD_fragment.cluster <= FD_start_clust(27 downto 0);
                FD_fragment.size    <= (others => '0');
                FD_nb_frag          <= 1;
                FD_new_fragment     <= '0';
            elsif check_fd_fsm = list_clust_dly and FD_next_cluster >= x"FFFFFF8" then
                FD_completed_fragment <= FD_fragment;
                FD_new_fragment       <= '1';
                FD_nb_frag            <= FD_nb_frag + 1;
                FD_new_fragment       <= '1';
            elsif check_fd_fsm = list_clust_dly and unsigned(FD_current_cluster) +1 /= unsigned(FD_next_cluster) then
                -- transfert the current fragment...
                FD_completed_fragment <= FD_fragment;
                FD_new_fragment       <= '1';
                FD_nb_frag            <= FD_nb_frag + 1;
                -- the next fragment...
                FD_fragment.cluster   <= FD_next_cluster;
                FD_fragment.size      <= (others => '0');
            elsif check_fd_fsm = list_clust_dly and unsigned(FD_current_cluster) +1 = unsigned(FD_next_cluster) then
                FD_fragment.size <= FD_fragment.size + 1;
            else
                FD_new_fragment <= '0';
            end if;
        end if;
    end process;

    -- Watch out the last fragment of the file
    Process(clk)
    begin
        if rising_edge(clk) then
            if check_fd_fsm = list_clust_dly and FD_next_cluster >= x"FFFFFF8" then
                FD_last_fragment <= '1';
            else
                FD_last_fragment <= '0';
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if check_fd_fsm = is_list_done and FD_new_fragment = '1' then
                file_last_fragment <= FD_last_fragment;
            else
                file_last_fragment <= FD_last_fragment;
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if check_fd_fsm = get_size then
                FD_cant_list_all_frag <= '0';
            elsif check_fd_fsm = list_clust_dly and unsigned(FD_current_cluster) +1 /= unsigned(FD_next_cluster) and FD_nb_frag = 511 then
                FD_cant_list_all_frag <= '1';
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    --  Create or append a directory
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                create_dir_fsm <= off;
            else
                case create_dir_fsm is
                    when off            => if check_fd_fsm = create_dir            then create_dir_fsm <= get_1_cluster;    end if;
                    when get_1_cluster  => if FAT_fsm = update_frag                then create_dir_fsm <= calcul_sector;    end if;
                    when calcul_sector  => if block_num_rdy = '1'                  then create_dir_fsm <= clear_buffer;     end if;
                    when clear_buffer   => if c_dir_bytes_cnt = 0                  then create_dir_fsm <= write_sector;     end if;
                    when write_sector   =>                                              create_dir_fsm <= wait_write;
                    when wait_write     => if SD_busy = '0' and SD_write_blk = '0' then 
                                               if c_dir_multiple_cnt = 0 then create_dir_fsm <= read_FAT; 
                                               else                           create_dir_fsm <= write_sector;       end if; end if;
                    when read_FAT       =>                                              create_dir_fsm <= wait_FAT_read;
                    when wait_FAT_read  => if SD_busy = '0' and SD_read_blk = '0'  then create_dir_fsm <= update_cluster;   end if;
                    when update_cluster => if c_dir_bytes_cnt = 0                  then create_dir_fsm <= write_FAT;        end if;
                    when write_FAT      =>                                              create_dir_fsm <= wait_FAT_write;
                    when wait_FAT_write => if SD_busy = '0' and SD_write_blk = '0' then create_dir_fsm <= done;             end if;
                                        -- elsif                                      then create_dir_fsm <= read_FAT;         end if; -- when happen a directory (because it is full)
                    when done           =>                                              create_dir_fsm <= off;
                end case;
            end if;
        end if;
    end process;

    -- Get the directory cluster
    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = get_1_cluster and FAT_fsm = update_frag then
                c_dir_cluster_nb <= frag_out.cluster;
            end if;
        end if;
    end process;

    -- Start calculation of the 1st sector of the cluster
    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = off then
                c_dir_start_calc  <= '0';
                c_dir_calculating <= '0';
            elsif create_dir_fsm = calcul_sector and c_dir_calculating = '0' then
                c_dir_start_calc  <= '1';
                c_dir_calculating <= '0';
            else
                c_dir_start_calc  <= '0';
            end if;
        end if;
    end process;

    -- get the directory first sector (when calculation ends)
    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = calcul_sector and block_num_rdy = '1' then
                c_dir_sector <= std_logic_vector(block_num);
            elsif create_dir_fsm = wait_write and c_dir_multiple_cnt = 0 then
                c_dir_sector <= std_logic_vector(to_unsigned(to_integer(unsigned(reserved_sectors) + unsigned(c_dir_cluster_nb(27 downto 7))), 32));
            end if;
        end if;
    end process;

    -- Count the number of bytes to write in the sector
    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = off then
                c_dir_bytes_cnt <= 511;
            elsif create_dir_fsm = read_FAT then
                c_dir_bytes_cnt <= 3;
            elsif create_dir_fsm = get_1_cluster or create_dir_fsm = calcul_sector or create_dir_fsm = clear_buffer or create_dir_fsm = update_cluster then
                if c_dir_bytes_cnt > 0 then
                    c_dir_bytes_cnt <= c_dir_bytes_cnt - 1;
                end if;
            end if;
        end if;
    end process;

    -- (de)count the number of sector cleared
    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = off then
                c_dir_multiple_cnt <= to_integer(unsigned(sectors_per_cluster));
            elsif create_dir_fsm = write_sector then
                c_dir_multiple_cnt <= c_dir_multiple_cnt - 1;
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = write_sector then
                c_dir_multiple <= '1';
            elsif c_dir_multiple_cnt = 0 then
                c_dir_multiple <= '0';
            end if;
        end if;
    end process;

    -- Address fro SD raw access buffer
    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = off then
                c_dir_buff_addr <= (8 downto 0 => '0');
            elsif create_dir_fsm = get_1_cluster or create_dir_fsm = calcul_sector or create_dir_fsm = clear_buffer then
                c_dir_buff_addr <= c_dir_buff_addr + 1;
            elsif create_dir_fsm = read_fat then
                c_dir_buff_addr <= unsigned(c_dir_cluster_nb(6 downto 0)) & "00";
            elsif create_dir_fsm = update_cluster then
                c_dir_buff_addr <= c_dir_buff_addr + 1;
            end if;
        end if;
    end process;

    -- data to write in sector (zeros) and in the FAT to end the directory cluster chain
    Process(clk)
    begin
        if rising_edge(clk) then
            if create_dir_fsm = off then
                c_dir_data_in <= x"00";
            elsif create_dir_fsm = read_FAT then
                c_dir_data_in <= x"FF";
            elsif create_dir_fsm = update_cluster and c_dir_bytes_cnt = 1 then
                c_dir_data_in <= x"0F";
            end if;
        end if;
    end process;



    -----------------------------------------------------------------
    --
    --                      Update file information
    --
    -----------------------------------------------------------------
    -- The related FSM
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                info_fsm <= off;
            else
                case info_fsm is
                    when off          => if module_fsm = close_fd and frag_fsm = off and file_out(file_id_user).update_file_info = '1' then info_fsm <= read_sector;  end if;
                    when read_sector  =>                                                                                                    info_fsm <= wait_read;
                    when wait_read    => if SD_busy = '0' and SD_read_blk = '0'                                                        then info_fsm <= copy;         end if;
                    when copy         => if info_bytes_cnt = 7                                                                         then info_fsm <= write_sector; end if;
                    when write_sector =>                                                                                                    info_fsm <= wait_write;
                    when wait_write   => if SD_busy = '0' and SD_write_blk = '0'                                                       then info_fsm <= over;         end if;
                    when over         =>                                                                                                    info_fsm <= off;
                end case;
            end if;
        end if;
    end process;

    -- "rename" sector num and address in the sector (act as a MUX when multiple files)
    info_sector_num <= file_out(file_id_op).sector_num;
    info_addr       <= file_out(file_id_op).info_addr_offset_out;

    -- process the address in the buffer
    Process(clk)
    begin
        if rising_edge(clk) then
            if info_fsm = wait_read and SD_busy = '0' and SD_read_blk = '0' then
                info_true_addr <= info_addr & MSB_CLUSTER_OFFSET;
            elsif info_fsm = copy then
                if info_bytes_cnt = 1 then
                    info_true_addr <= info_addr & LSB_CLUSTER_OFFSET;
                else
                    info_true_addr <= std_logic_vector(unsigned(info_true_addr) + 1);
                end if;
            end if;
        end if;
    end process;

    -- get the right byte to insert in file info
    info_buffer_byte <= info_buffer(7 downto 0);
    Process(clk)
    begin
        if rising_edge(clk) then
            if info_fsm = wait_read and SD_busy = '0' and SD_read_blk = '0' then
                info_buffer <= file_out(file_id_user).starting_cluster(15 downto 0) & "0000" & file_out(file_id_user).starting_cluster(27 downto 16);
            elsif info_fsm = copy then
                if info_bytes_cnt = 3 then
                    info_buffer <= file_size_i;
                else
                    info_buffer(23 downto 0) <= info_buffer(31 downto 8);
                end if;
            end if;
        end if;
    end process;

    -- finaly the counter
    Process(clk)
    begin
        if rising_edge(clk) then
            if info_fsm = off then
                info_bytes_cnt <= 0;
            elsif info_fsm = copy then
                info_bytes_cnt <= info_bytes_cnt + 1;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    --
    --                    Directories management
    --
    -----------------------------------------------------------------
    tree_depth_full <= tree_depth = TREE_STRUCTURE_MAX;

    -- Here are stored the first sector of each opened directories and the number of opened directories
    -- It also stored the first cluster of each opened directories (to know the other clusters of the directories in the FAT)
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' or module_fsm = no_SD_card then
                tree_depth             <= 0;
                tree_structure         <= (others => (others => '0'));
                tree_structure_cluster <= (others => (others => '0'));
            elsif module_fsm = fat then
                tree_depth                <= 0;
                tree_structure(0)         <= std_logic_vector(root_directory_sector);
                tree_structure_cluster(0) <= root_directory_cluster(27 downto 0);
            elsif check_fd_fsm = open_directory and block_num_rdy = '1' then
                tree_depth                           <= tree_depth + 1;
                tree_structure(tree_depth+1)         <= std_logic_vector(block_num);
                tree_structure_cluster(tree_depth+1) <= FD_start_clust(27 downto 0);
            elsif module_fsm = close_directory and tree_depth /= 0 then
                tree_depth <= tree_depth - 1;
            end if;
        end if;
    end process;

    -- Here is created an impulse to start the calculation of the first sector of the newly opened directory 
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' or check_fd_fsm = off then
                open_dir_busy   <= '0';
                calc_dir_sector <= '0';
            elsif check_fd_fsm = open_directory and open_dir_busy = '0' then
                open_dir_busy   <= '1';
                calc_dir_sector <= '1';
            elsif check_fd_fsm = open_directory and block_num_rdy = '1' then
                open_dir_busy   <= '0';
                calc_dir_sector <= '0';
            else
                calc_dir_sector <= '0';
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    --
    --       Block num / Sector depending on cluster number
    --
    -----------------------------------------------------------------

    calc_sector <= '1' when (check_fd_fsm = next_cluster and FD_next_cluster < x"FFFFFF8") else
                   '1' when calc_dir_sector = '1'                                          else
                   '1' when c_dir_start_calc = '1'                                         else
                   '0';

    mux_out_cluster <= FD_next_cluster             when check_fd_fsm = next_cluster   else
                       FD_start_clust(27 downto 0) when check_fd_fsm = open_directory else
                       c_dir_cluster_nb            when check_fd_fsm = create_dir     else
                       (others => '0');


    -- here we calculate to sector with a given cluster
    -- TODO : - Use block_num to do shift operation, this could save 32 FF but we must be sure we use it only when block_num_rdy is set.
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                calc_sector_bsy <= '0';
                -- block_num       <= (others => '0');
                block_num_rdy   <= '0';
            elsif calc_sector_bsy = '0' then
                if calc_sector = '1' then
                    if prev_cluster = mux_out_cluster then
                        rem_shifts <= (others => '0');
                    else
                        rem_shifts      <= unsigned(sectors_per_cluster(7 downto 1));
                        buffer_to_shift <= (31 downto 28 => '0') & (unsigned(mux_out_cluster) - 2);
                    end if;
                    prev_cluster <= mux_out_cluster;
                    calc_sector_bsy <= '1';
                    block_num_rdy   <= '0';
                else
                    block_num_rdy   <= '0';
                end if;
            else
                if rem_shifts = 0 then
                    calc_sector_bsy <= '0';
                    block_num_rdy   <= '1';
                    block_num       <= data_area_begin_sector + buffer_to_shift;
                else
                    rem_shifts      <= '0' & rem_shifts(6 downto 1);
                    buffer_to_shift <= buffer_to_shift(30 downto 0) & '0';
                end if;
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    --
    --              Files instanciation and management
    --
    -----------------------------------------------------------------
    
    files_inst : for ID in 0 to MAX_FILES-1 generate
        file_manager : Sdcard_FAT32_file
            generic map(CLK_FREQ_HZ    => CLK_FREQ_HZ,
                        VIVADO_SYNTH   => VIVADO_SYNTH,
                        NB_MULTIPLE_OP => NB_MULTIPLE_OP,
                        WORD_SIZE_PW2  => WORD_SIZE_PW2,
                        FIFO_DEPTH     => FIFO_DEPTH)
            port map(clk                 => clk,
                    reset                => longreset,
            
                    -- input interface
                    filename             => FD_name,                                -- file name : 8 + '.' + 3
                    date_time            => date_time_buff,                         -- Creation time, can be ignored
                    read_write_in        => FD_rw_mode,                             -- read = '0' ; write = '1'
                    read_write_out       => file_out(ID).read_write_out,            -- read = '0' ; write = '1'
                    reserve_clusters_in  => reserv_clust_i,                         -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case
                    reserve_clusters_out => file_out(ID).reserve_clusters_out,      -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case

                    opened               => file_out(ID).opened,                    -- high if the file is opened
                    size_in              => FD_size,                                -- size of the file to read in bytes 
                    size                 => file_out(ID).size,                      -- return the file size (in bytes)

                    -- control signals
                    open_file            => file_in(ID).open_file,                  -- try to open the file_name on the stream ID
                    close_file           => file_in(ID).close_file,                 -- close the file
                    format               => format,                                 -- format the card

                    -- data interface
                    data_in              => file_in(ID).data_in,                     -- data input
                    data_out             => file_out(ID).data_out,                  -- data output
                    enable_in            => file_in(ID).enable_in,                  -- tell the module to write data_in / read next data
                    enable_out           => file_out(ID).enable_out,                -- '1' when data_in can be write / data_out can be read

                    -- constant from SD card
                    sectors_per_cluster  => sectors_per_cluster,                      -- nb of sector per cluster (constant from boot record)
                    data_area_offset     => std_logic_vector(data_area_begin_sector), -- offset for the begining of the data area, after boot record and fats (constant deducted/calculated from boot record)

                    -- these access are control or info from file data fifo
                    fifo_read            => file_in(ID).fifo_read,                  -- used by the SD manager to read data from FIFO
                    fifo_write           => file_in(ID).fifo_write,                 -- used by the SD manager to write data on FIFO
                    fifo_empty           => file_out(ID).fifo_empty,                -- inform the module about the data fifo's filling
                    fifo_full            => file_out(ID).fifo_full,                 -- inform the module about the data fifo's filling

                    -- sector/cluster management
                    sector_num           => file_out(ID).sector_num,                -- sector to read/write
                    request_op           => file_out(ID).request_op,                -- request a read or write operation
                    request_clusters     => file_out(ID).request_clusters,          -- request a fragment of free clusters for writing

                    -- fragment transfert
                    fragment_in          => file_in(ID).fragment_in,                -- fragment receive from free fragment manager
                    last_fragment        => file_in(ID).last_fragment,              -- annonce the last fragment (when collecting file's fragment for reading)
                    write_clusters_chain => file_out(ID).write_clusters_chain,      -- set when the clusters chain is ready to be written (into FAT)
                    fragment_out         => file_out(ID).fragment_out,              -- fragment to write in the FAT

                    -- file information (writing)
                    update_file_info     => file_out(ID).update_file_info,          -- set to request the update of file information, which will result in closing the file
                    info_sector_num      => file_in(ID).info_sector_num,            -- file info sector number (=> file descritor sector)
                    info_addr_offset_in  => file_in(ID).info_addr_offset_in,        -- address offset of the file descriptor in the sector
                    info_addr_offset_out => file_out(ID).info_addr_offset_out,      -- address offset of the file descriptor in the sector
                    starting_cluster     => file_out(ID).starting_cluster,          -- starting cluster of the file. Used to save it in file information

                    -- ackknoledge
                    op_ack               => file_in(ID).op_ack,                     -- top module inform that the operation is accepted/can start

                    -- debug
                    file_debug           => file_out(ID).file_debug,                -- debug/error data
                    file_error           => file_out(ID).file_error,                -- high when an error on the FAT occurs

                    -- Debug
                    debug_addr           => file_debug_addr,                       -- address of the register to look out
                    debug_data           => open);--file_debug_data);                      -- the data to look out
    end generate;

    -- mux for size output
    file_size   <= file_size_i;
    file_size_i <= file_out(file_id_user).size;

    -- create a vector for files' enable out
    Process(file_out)
    begin
        for i in 0 to MAX_FILES-1 loop
            -- if eject_in_reg ='1' or start_eject = '1' or eject_busy = '1' then
            --     enable_out(i) <= '0';
            -- else
            enable_out(i) <= file_out(i).enable_out;
        end loop;
    end process;
    -- create a vector for files' opened state
    Process(file_out)
    begin
        for i in 0 to MAX_FILES-1 loop
            file_opened_i(i) <= file_out(i).opened;
        end loop;
    end process;
    -- create a vector for files' open mode
    Process(file_out)
    begin
        for i in 0 to MAX_FILES-1 loop
            rw_mode_i(i) <= file_out(i).read_write_out;
        end loop;
    end process;

    -- data out mux
    Process(file_out)
    begin
        for i in 0 to MAX_FILES-1 loop
            if file_out(i).read_write_out = '0' then
                data_out(8*(i+1)*(2**WORD_SIZE_PW2)-1 downto 8*i*(2**WORD_SIZE_PW2)) <= file_out(i).data_out;
            else
                data_out(8*(i+1)*(2**WORD_SIZE_PW2)-1 downto 8*i*(2**WORD_SIZE_PW2)) <= (others => '0');
            end if;
        end loop;
    end process;

    SDm_data_write <= (others => '0') when file_out(file_id_op).fifo_empty = '1' else file_out(file_id_op).data_out;
 
    -- mux for file_id to focus for op_ack and fragment_in
    file_id_all <= file_id_user when module_fsm = open_fd and not(check_fd_fsm = off or check_fd_fsm = check_over) else file_id_op;

    -- Process(FD_op_success, FD_file_type, file_last_fragment, close_file_buff, file_to_close_found, 
    --         file_info_sector_num, file_info_addr_offset_in, file_op_ack, file_fragment_in,
    --         file_fifo_read, file_fifo_write,
    --         file_id_user, file_id_op, file_id_all, enable_in,
    --         file_out, data_in, SDm_data_read,
    --         eject_in_reg, start_eject)
    -- begin
    --     if FD_file_type = '0' then
    --         file_in(file_id_user).open_file <= FD_op_success;
    --     else
    --         file_in(file_id_user).open_file <= '0';
    --     end if;
    --     file_in(file_id_user).last_fragment <= file_last_fragment;

    --     if eject_in_reg ='1' or start_eject = '1' then
    --         for i in 0 to MAX_FILES-1 loop
    --             file_in(i).close_file <= '1';
    --         end loop;
    --     elsif close_file_buff = '1' or file_to_close_found = '1' then
    --         file_in(file_id_user).close_file <= '1';
    --     else
    --         file_in(file_id_user).close_file <= '0';
    --     end if;

    --     file_in(file_id_user).info_sector_num     <= file_info_sector_num;
    --     file_in(file_id_user).info_addr_offset_in <= file_info_addr_offset_in;
    --     file_in(file_id_all).op_ack                <= file_op_ack;
    --     -- file_in(file_id_user).fragment_in         <= file_fragment_in;
    --     file_in(file_id_all).fragment_in         <= file_fragment_in;

    --     file_in(file_id_op).fifo_read             <= file_fifo_read;
    --     file_in(file_id_op).fifo_write            <= file_fifo_write;

    --     for i in 0 to MAX_FILES-1 loop
    --         file_in(i).enable_in <= enable_in(i);
    --         if file_out(i).read_write_out = '1' then
    --             file_in(i).data_in <= data_in(8*(i+1)*(2**WORD_SIZE_PW2)-1 downto 8*i*(2**WORD_SIZE_PW2));
    --         else
    --             file_in(i).data_in <= SDm_data_read;
    --         end if;
    --     end loop;
    -- end process;

    Process(FD_op_success, FD_file_type, file_last_fragment, close_file_buff, file_to_close_found, 
            file_info_sector_num, file_info_addr_offset_in, file_op_ack, file_fragment_in,
            file_fifo_read, file_fifo_write,
            file_id_user, file_id_op, file_id_all, enable_in,
            file_out, data_in, SDm_data_read,
            eject_in_reg, reset_in_reg, start_eject)
    begin
        for ID in 0 to MAX_FILES-1 loop

            if eject_in_reg ='1' or reset_in_reg = '1' or start_eject = '1' then
                file_in(ID).close_file <= '1';
            elsif ID = file_id_user and (close_file_buff = '1' or file_to_close_found = '1') then
                file_in(ID).close_file <= '1';
            else
                file_in(ID).close_file <= '0';
            end if;

            if ID = file_id_user then
                if FD_file_type = '0' then
                    file_in(ID).open_file <= FD_op_success;
                else
                    file_in(ID).open_file <= '0';
                end if;
                file_in(ID).last_fragment <= file_last_fragment;
                file_in(ID).info_sector_num     <= file_info_sector_num;
                file_in(ID).info_addr_offset_in <= file_info_addr_offset_in;
            else
                file_in(ID).open_file <= '0';
                file_in(ID).last_fragment <= '0';
                file_in(ID).info_sector_num     <= (others => '0');
                file_in(ID).info_addr_offset_in <= (others => '0');

            end if;

            if ID = file_id_op then
                file_in(ID).fifo_read  <= file_fifo_read;
                file_in(ID).fifo_write <= file_fifo_write;
            else
                file_in(ID).fifo_read  <= '0';
                file_in(ID).fifo_write <= '0';
            end if;

            if ID = file_id_all then
                file_in(ID).op_ack      <= file_op_ack;
                file_in(ID).fragment_in <= file_fragment_in;
            else
                file_in(ID).op_ack      <= '0';
                file_in(ID).fragment_in <= (cluster => (others => '0'), size => (others => '0'));
            end if;

            file_in(ID).enable_in <= enable_in(ID);
            if file_out(ID).read_write_out = '1' then
                file_in(ID).data_in <= data_in(8*(ID+1)*(2**WORD_SIZE_PW2)-1 downto 8*ID*(2**WORD_SIZE_PW2));
            else
                file_in(ID).data_in <= SDm_data_read;
            end if;

        end loop;
    end process;


    -- Here we communicate with the file manager to transfert parameters and/or acknowledge
    Process(clk)
    begin
        if rising_edge(clk) then
            -- Read parameters
            if check_fd_fsm = is_file_empty then
                file_op_ack  <= '1';

            -- Write parameters
            elsif check_fd_fsm = write_sector_back then
                file_info_sector_num     <= free_fd_sector;
                file_info_addr_offset_in <= free_fd_address;
                file_op_ack              <= '1';

            -- list fragment of clusters to read
            elsif check_fd_fsm = is_list_done and FD_new_fragment = '1' then
                file_fragment_in  <= FD_completed_fragment;
                file_op_ack       <= '1';

            -- Operation for a file
            elsif SDm_fsm = ack_file_op then
                file_op_ack <= '1';
            -- Acknoledge the end of operation
            elsif SDm_fsm = end_op and SD_op_fsm = off and SD_busy = '0' then
                file_op_ack <= '1';
            -- -- Read sector for a file
            -- elsif SDm_fsm = wait_op and SDm_last_byte_dly = '1' then
            --     file_op_ack <= '1';
            -- -- Write sector for a file;
            -- elsif SDm_fsm = wait_op and SD_op_fsm = write_sector then
            --     file_op_ack <= '1';

            -- Request fragment
            elsif FAT_fsm = update_frag and check_fd_fsm /= create_dir then
                -- file_fragment_in.cluster <= frag_out(49 downto 22);               -- TODO: - use type fragment for frag_out 
                -- file_fragment_in.size    <= unsigned(frag_out(21 downto  0));     -- TODO: - use type fragment for frag_out 
                file_fragment_in <= frag_out;
                file_op_ack      <= '1';

            -- Fragment for FAT cluster chain acknoledge
            elsif module_fsm = close_fd and (frag_fsm = is_over and frag_size /= 0) then
                file_op_ack <= '1';

            -- file info update
            elsif info_fsm = wait_write  and SD_busy = '0' and SD_write_blk = '0' then
                file_op_ack <= '1';

            else
                file_op_ack <= '0';
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    -- Scheduler of file operation & priority
    -----------------------------------------------------------------
    -- Fetch files op and take the first one found (from file id 0 to file id MAX_FILES-1)
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                SDm_file_op_shift      <= (others => '0');
                -- SDm_request_frag_shift <= (others => '0');
                file_id_op             <= 0;
                SDm_free_frag          <= '0';
                SDm_file_op            <= '0';
                SDm_write_op           <= '0';
            -- elsif not_a_file_op = '0' then 
            elsif SDm_fsm = idle and user_op_bsy = '1' then
                    if file_out(file_id_user).request_clusters = '1' then
                        SDm_free_frag <= '1';
                        SDm_file_op   <= '0';
                    elsif file_out(file_id_user).request_op = '1' then
                        SDm_free_frag <= '0';
                        SDm_file_op   <= '1';
                    else
                        SDm_free_frag <= '0';
                        SDm_file_op   <= '0';
                    end if;
                    SDm_write_op <= file_out(file_id_user).read_write_out;
                    file_id_op   <= file_id_user;
            elsif not_a_file_op = '0' then 
                if SDm_fsm = idle and SDm_file_op = '0' then
                    -- We charge new file request when no request are in the shift register
                    if SDm_file_op_shift = (MAX_FILES-1 downto 0 => '0') then
                        for i in 0 to MAX_FILES-1 loop
                            SDm_file_op_shift(i) <= file_out(i).request_op or file_out(i).request_clusters;
                        end loop;
                        file_id_op <= 0;
                    -- Next we check if the first 'file' of the shift register is requesting an op. If it is, we grant it and inform SDm_fsm
                    elsif SDm_file_op_shift(0) = '1' then
                        if file_out(file_id_op).request_clusters = '1' then --todo: - we could use another shift register to store request_cluters bits and avoid this comparison (which should use numerous LUT when instanciating a lot of files)
                            SDm_free_frag <= '1';
                        else
                            SDm_free_frag <= '0';
                        end if;
                        SDm_write_op <= file_out(file_id_op).read_write_out;
                        SDm_file_op <= '1';
                    -- Finally, we shift the register to check other file manager. File ID is incremented at the same time to know the file manager which will be checked
                    else
                        SDm_file_op_shift <= '0' & SDm_file_op_shift(MAX_FILES-1 downto 1);
                        file_id_op        <= file_id_op + 1;
                    end if;
                -- We give a write operation to the file manager which has just requested free cluster(s)
                elsif SDm_fsm = wait_next_op and file_out(file_id_op).request_op = '1' then
                    SDm_write_op <= file_out(file_id_op).read_write_out;
                -- File op flag are reset when they have been accounted
                elsif SDm_fsm = give_free_frag or SDm_fsm = ack_file_op then
                    SDm_file_op_shift <= (others => '0');
                    SDm_free_frag     <= '0';
                    SDm_file_op       <= '0';
                end if;
            else
                SDm_file_op_shift <= (others => '0');
                SDm_free_frag     <= '0';
                SDm_file_op       <= '0';
            end if;
        end if;
    end process;



    -- FSM which start and handle files op
    -- todo : - To speed up thing a little (but add more tests... so maybe todo when using multiple op well and a lot):
    --            - stop a read multiple op when all the data has been already read
    --            - stop write multiple op when all the data has been written (when closing a file)
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                SDm_fsm <= idle;
            else
                case SDm_fsm is
                    when idle            => if SDm_free_frag = '1'                                           then SDm_fsm <= give_free_frag;
                                         elsif SDm_file_op   = '1'                                           then SDm_fsm <= ack_file_op;     end if;
                    when give_free_frag  =>                                                                       SDm_fsm <= wait_next_op;
                    when wait_next_op    => if file_out(file_id_op).request_op = '1'                         then SDm_fsm <= ack_file_op;     end if;
                    when ack_file_op     =>                                                                       SDm_fsm <= wait_buffer;
                    when wait_buffer     => if mul_buffer_free(mul_buff_in_ptr) = '0' and SDm_write_op = '0' then SDm_fsm <= read_sector;
                                         elsif mul_buffer_free(mul_buff_in_ptr) = '1' and SDm_write_op = '1' then SDm_fsm <= write_sector;    end if;
                                        --  elsif mul_buffer_free(mul_buff_in_ptr) = SDm_write_op            then SDm_fsm <= write_sector;    end if;
    
                    when read_sector     =>                                                                       SDm_fsm <= wait_read;
                    when wait_read       => if SD_busy = '0' and SD_read_blk = '0' and multiple_op_done='1'  then SDm_fsm <= end_op;
                                         elsif SD_busy = '0' and SD_read_blk = '0' and multiple_op_done='0'  then SDm_fsm <= wait_buffer;     end if;

                    when write_sector    =>                                                                       SDm_fsm <= wait_write;
                    when wait_write      => if SD_busy = '0' and SD_write_blk = '0' and multiple_op_done='1' then SDm_fsm <= end_op;
                                         elsif SD_busy = '0' and SD_write_blk = '0' and multiple_op_done='0' then SDm_fsm <= wait_buffer;     end if;

                    when end_op          => if SD_op_fsm = off and SD_busy = '0'                             then SDm_fsm <= idle;            end if;
                end case;
            end if;
        end if;
    end process;

    -- FSM that manage data transfert between files and the SDcard
    -- todo : - put read_dly state before transfert_read (or change the flow of the FSM or remove it and handle it outside of the FSM) to ease some test below (and save some LUTs)
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                SD_op_fsm <= off;
            else
                case SD_op_fsm is
                    when off             => if SDm_fsm = idle and (SDm_file_op = '1' or SDm_free_frag = '1')   then SD_op_fsm <= wait_buffer;     end if;
                    
                    when wait_buffer     => if SDm_write_op = '0' and mul_buffer_free(mul_buff_out_ptr) = '1'  then SD_op_fsm <= transfert_read;
                                         elsif SDm_write_op = '1' and mul_buffer_free(mul_buff_out_ptr) = '0'  then SD_op_fsm <= transfert_write; end if;
                                        --  elsif mul_buffer_free(mul_buff_out_ptr) = not (SDm_write_op)       then SD_op_fsm <= transfert_write; end if;

                    when transfert_read  => if SDm_last_byte_dly = '1' and SDm_sector_cnt = NB_MULTIPLE_OP-1   then SD_op_fsm <= off;
                                         elsif SDm_last_byte_dly = '1'                                         then SD_op_fsm <= read_dly;        end if;
                    when read_dly        =>                                                                         SD_op_fsm <= wait_buffer;

                    when transfert_write => if SDm_buff_addr = 511 and SDm_sector_cnt = NB_MULTIPLE_OP-1       then SD_op_fsm <= off;
                                         elsif SDm_buff_addr = 511                                             then SD_op_fsm <= wait_buffer;     end if;
                end case;
            end if;
        end if;
    end process;

    -- Sector to operate is stored when the file ID is known. The buffer to use is also determined.
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = ack_file_op then
                SDm_sector_num <= file_out(file_id_op).sector_num;
            end if;
        end if;
    end process;

    -- Determine when a multiple op is over
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = idle then
                multiple_op_done <= '0';
            elsif mul_op_cnt = NB_MULTIPLE_OP-1 and (SDm_fsm = read_sector or SDm_fsm = write_sector) then
                multiple_op_done <= '1';
            end if;
        end if;
    end process;

    -- Count the number of multiple op realised for a file operation
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = idle then
                mul_op_cnt <= 0;
            elsif SDm_fsm = read_sector or SDm_fsm = write_sector then
                if mul_op_cnt = NB_MULTIPLE_OP-1 then
                    mul_op_cnt <= 0;
                else
                    mul_op_cnt <= mul_op_cnt + 1;
                end if;
            end if;
        end if;
    end process;

    -- The multiple bit is set and reset there
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = idle or SDm_fsm = end_op then
                multiple_i <= '0';
            elsif (SDm_fsm = read_sector or SDm_fsm = write_sector) and multiple_op_done = '0' then
                multiple_i <= '1';
            elsif ((SDm_fsm = wait_read and SD_read_blk = '0') or (SDm_fsm = wait_write or SD_write_blk = '0')) and multiple_op_done = '1' then
                multiple_i <= '0';
            end if;
        end if;
    end process;

    -- Select the buffer to operate with SDcard raw_acess 
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = idle then
                SDm_op_buff <= (others => '0');
            elsif SDm_fsm = wait_buffer then
                SDm_op_buff <= std_logic_vector(to_unsigned(mul_buff_in_ptr, BUFFER_NUM_PW2));
            end if;
        end if;
    end process;

    -- Update ptr use to operator sector
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = idle then
                mul_buff_in_ptr  <= 0;
            elsif (SDm_fsm = wait_read  and SD_busy = '0' and SD_read_blk  = '0')
               or (SDm_fsm = wait_write and SD_busy = '0' and SD_write_blk = '0') then
                if mul_buff_in_ptr = NB_SDCARD_BUFFER-1 then
                    mul_buff_in_ptr <= 0;
                else
                    mul_buff_in_ptr <= mul_buff_in_ptr + 1;
                end if;
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if SD_op_fsm = off then
                SDm_data_buff <= (others => '0');
            elsif SD_op_fsm = read_dly or SD_op_fsm = wait_buffer then
                SDm_data_buff <= std_logic_vector(to_unsigned(mul_buff_out_ptr, BUFFER_NUM_PW2));
            end if;
        end if;
    end process;

    -- Update ptr use to operator SDcard buffers
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = idle then
                mul_buff_out_ptr  <= 0;
            -- elsif (SD_op_fsm = transfert_read or SD_op_fsm = transfert_write) and SDm_last_byte_dly = '1' then
            elsif (SD_op_fsm = transfert_read and SDm_last_byte_dly = '1') or (SD_op_fsm = transfert_write and SDm_buff_addr = 511) then -- hotfix. todo : use SDm_last_byte_dly for both, but delay fsm by one. This could save 1 or 2 LUT
                if mul_buff_out_ptr = NB_SDCARD_BUFFER-1 then
                    mul_buff_out_ptr <= 0;
                else
                    mul_buff_out_ptr <= mul_buff_out_ptr + 1;
                end if;
            end if;
        end if;
    end process;

    -- Update the state and readyness of SDcard buffers
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_fsm = idle then
                mul_buffer_free <= (others => '0');
            else
                -- Operations buffers update
                if SDm_fsm = wait_read and SD_busy = '0' and SD_read_blk = '0' then
                    mul_buffer_free(mul_buff_in_ptr) <= '1';
                elsif SDm_fsm = wait_write and SD_busy = '0' and SD_write_blk = '0' then
                    mul_buffer_free(mul_buff_in_ptr) <= '0';
                end if;

                -- Data buffers update
                if    SDm_write_op = '0' and SDm_last_byte_dly = '1' then
                    mul_buffer_free(mul_buff_out_ptr) <= '0';
                elsif SDm_write_op = '1' and SDm_buff_addr = 511     then
                    mul_buffer_free(mul_buff_out_ptr) <= '1';
                end if;
                -- if SDm_last_byte_dly = '1' then
                --     mul_buffer_free(mul_buff_out_ptr) <= SDm_write_op;
                -- end if;
            end if;
        end if;
    end process;

    -- here is manage the address used to write on SDcard raw_access buffers
    Process(clk)
    begin
        if rising_edge(clk) then
            if SD_op_fsm = off or SD_op_fsm = read_dly or (SD_op_fsm = wait_buffer and not(SDm_write_op = '0' and mul_buffer_free(mul_buff_out_ptr) = '1')) then
                SDm_buff_addr <= (others => '0');
            elsif SD_op_fsm = transfert_read  
               or (SD_op_fsm = wait_buffer and SDm_write_op = '0' and mul_buffer_free(mul_buff_out_ptr) = '1')
               or SD_op_fsm = transfert_write                                                                  then
                SDm_buff_addr <= SDm_buff_addr + 1;
            end if;
        end if;
    end process;

    -- Delay the last byte read from SDcard buffer of 1 clk cycle
    Process(clk)
    begin
        if rising_edge(clk) then
            if SDm_buff_addr = 511 then
                SDm_last_byte_dly <= '1';
            else
                SDm_last_byte_dly <= '0';
            end if;
        end if;
    end process;

    -- write data read into file's fifo
    Process(clk)
    begin
        if rising_edge(clk) then
            if SD_op_fsm = off then
                SDm_data_read <= (others => '0');
                file_fifo_write <= '0';
            elsif SD_op_fsm = transfert_read then
                SDm_data_read <= SD_data_out;
                file_fifo_write <= '1';
            else
                file_fifo_write <= '0';
            end if;
        end if;
    end process;

    -- read data from file's fifo
    Process(clk)
    begin
        if rising_edge(clk) then
            if (SD_op_fsm = transfert_write and SDm_buff_addr /= 511) or (SD_op_fsm = wait_buffer and SDm_write_op = '1' and mul_buffer_free(mul_buff_out_ptr) = '0') then
                file_fifo_read <= '1';
            else
                file_fifo_read <= '0';
            end if;
        end if;
    end process;

    -- Count the number of sector transfered
    Process(clk)
    begin
        if rising_edge(clk) then
            if SD_op_fsm = off then
                SDm_sector_cnt <= 0;
            elsif (SD_op_fsm = transfert_read and SDm_last_byte_dly = '1') or (SD_op_fsm = transfert_write and SDm_buff_addr = 511) then
                if SDm_sector_cnt = NB_MULTIPLE_OP-1 then
                    SDm_sector_cnt <= 0;
                else
                    SDm_sector_cnt <= SDm_sector_cnt + 1;
                end if;
            end if;
        end if;
    end process;

    -----------------------------------------------

    
    -----------------------------------------------------------------
    --
    --                    SDcard management
    --
    -----------------------------------------------------------------

    -- operate the SDcard
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                SD_read_blk  <= '0';
                SD_write_blk <= '0';
                SD_erase_blk <= '0';
                SD_op_buff   <= (others => '0');
                SD_block_num <= (others => '0');
            elsif SD_busy = '0' and SD_read_blk = '0' and SD_write_blk = '0' and SD_erase_blk = '0' then
                -- Read 1st sector = Boot Record
                if module_fsm = initializing and SD_busy = '0' then
                    SD_read_blk  <= '1';
                    SD_op_buff   <= "010";
                    SD_block_num <= BR_block_num;
                -- Read FSI sector as soon as we got its address
                elsif BR_fsm = get_backup_boot_sector then
                    SD_read_blk <= '1';
                    SD_op_buff  <= "011";
                    SD_block_num <= BR_block_num;
                
                -- Read FAT sectors for boot up check
                elsif module_fsm = fat and (FAT_fsm = read_fat_1 or FAT_fsm = read_fat_k) then
                    SD_read_blk  <= '1';
                    SD_op_buff   <= FAT_op_buff;
                    SD_block_num <= std_logic_vector(FAT_block_num);

                -- Read directory entries or directory's next clusters (in FAT)
                elsif module_fsm = open_fd and (check_fd_fsm = load_directory or check_fd_fsm = read_dir_fat or check_fd_fsm = read_dir_fat or check_fd_fsm = read_fat) then
                    SD_read_blk  <= '1';
                    -- SD_op_buff   <= FD_loc_buffer;
                    if check_fd_fsm = wait_directory or check_fd_fsm = load_directory then
                        SD_op_buff <= "110";
                    elsif check_fd_fsm = read_dir_fat or check_fd_fsm = read_fat then
                        SD_op_buff <= "101";
                    elsif check_fd_fsm = end_of_dir then
                        SD_op_buff <= "110";
                    end if;
                    SD_block_num <= std_logic_vector(FD_block_num);
                
                -- Read the sector where the file entry will be written
                elsif module_fsm = open_fd and check_fd_fsm = read_dir_sector then
                    SD_read_blk  <= '1';
                    SD_op_buff   <= "110";
                    SD_block_num <= free_fd_sector;
                -- Write the sector back in the directory to create the file descriptor
                elsif module_fsm = open_fd and check_fd_fsm = write_sector_back then
                    SD_write_blk <= '1';
                    SD_op_buff   <= "110";
                    SD_block_num <= free_fd_sector;


                -- Clear sectors of the new directory
                elsif create_dir_fsm = write_sector then
                    SD_write_blk  <= '1';
                    SD_op_buff   <= "000";
                    SD_block_num <= c_dir_sector;
                elsif create_dir_fsm = read_FAT then
                    SD_read_blk  <= '1';
                    SD_op_buff   <= "000";
                    SD_block_num <= c_dir_sector;
                elsif create_dir_fsm = write_FAT then
                    SD_write_blk  <= '1';
                    SD_op_buff   <= "000";
                    SD_block_num <= c_dir_sector;



                -- read a sector for a file
                elsif SDm_fsm = read_sector then
                    SD_read_blk  <= '1';
                    SD_op_buff   <= SDm_op_buff;
                    SD_block_num <= SDm_sector_num;

                elsif SDm_fsm = write_sector then
                    SD_write_blk  <= '1';
                    SD_op_buff   <= SDm_op_buff;
                    SD_block_num <= SDm_sector_num;

                -- read the sector where used clusters are indexed
                elsif module_fsm = close_fd and frag_fsm = read_sector then             -- the test ' module_fsm = close_fd ' is useless.
                    SD_read_blk  <= '1';
                    SD_op_buff   <= "001";
                    SD_block_num <= frag_block_num;
                -- write back the sector with used clusters to update the FAT
                elsif module_fsm = close_fd and frag_fsm = write_sector  then           -- the test ' module_fsm = close_fd ' is useless.
                    SD_write_blk <= '1';
                    SD_op_buff   <= "001";
                    SD_block_num <= frag_block_num;
                
                -- read the file info sector
                elsif file_opened_i(file_id_user) = '1' and info_fsm = read_sector then
                    SD_read_blk <= '1';
                    SD_op_buff   <= "001";
                    SD_block_num <= info_sector_num;
                -- write back the file info sector updated
                elsif file_opened_i(file_id_user) = '1' and info_fsm = write_sector then
                    SD_write_blk <= '1';
                    SD_op_buff   <= "001";
                    SD_block_num <= info_sector_num;

                -- Eject the SDcard
                -- Read a sector from the modified FAT
                elsif recopy_FAT_fsm = read_FAT then
                    SD_read_blk <= '1';
                    SD_op_buff   <= "000";
                    SD_block_num <= std_logic_vector(FAT_block_num);
                -- Write the just read sector into another FAT
                elsif recopy_FAT_fsm = write_FAT then
                    SD_write_blk <= '1';
                    SD_op_buff   <= "000";
                    SD_block_num <= std_logic_vector(FAT_block_num);
                end if;
                    

            else
                SD_read_blk  <= '0';
                SD_write_blk <= '0';
                SD_erase_blk <= '0';
            end if;
        end if;
    end process;

    SD_multiple  <= c_dir_multiple when check_fd_fsm = create_dir else multiple_i;

    SD_write   <= '1'           when check_fd_fsm = create_file  else
                  '1'           when SD_op_fsm = transfert_write else
                  '1'           when frag_fsm = write_fragment   else
                  '1'           when info_fsm = copy             else
                  '1'           when create_dir_fsm = clear_buffer or create_dir_fsm = get_1_cluster or create_dir_fsm = calcul_sector or create_dir_fsm = update_cluster else
                  '0';

    SD_data_in <= c_dir_data_in        when create_dir_fsm /= off       else
                  FD_data              when module_fsm = open_fd        else
                  SDm_data_write       when SD_op_fsm = transfert_write else
                  frag_next_cluster_B  when frag_fsm = write_fragment   else
                  info_buffer_byte     when info_fsm = copy             else
                  (others => '0');

    -- Order matter !!!
    SD_buffer  <= BR_loc_buffer  when module_fsm = boot_record  else
                  FAT_loc_buffer when module_fsm = fat          else
                  "000"          when create_dir_fsm /= off     else
                  -- SDm_op_buff   when SD_op_fsm /= off          else
                  SDm_data_buff  when SD_op_fsm /= off          else
                  FD_loc_buffer  when module_fsm = open_fd      else
                  "001"          when frag_fsm = write_fragment else
                  "001"          when info_fsm = copy           else
                  (others => '0');

    SD_address <= std_logic_vector(BR_address)        when module_fsm = boot_record  else
                  std_logic_vector(FAT_address)       when module_fsm = fat          else
                  std_logic_vector(c_dir_buff_addr)   when create_dir_fsm /= off     else
                  std_logic_vector(SDm_buff_addr)     when SD_op_fsm /= off          else
                  FD_buff_addr                        when module_fsm = open_fd      else
                  frag_buff_addr                      when frag_fsm = write_fragment else
                  info_true_addr                      when info_fsm = copy           else
                  (others => '0');


    -- Synchronous card detect / write protect
    Process(clk)
    begin
        if rising_edge(clk) then
            synced_CD <= SD_CD;
            synced_WP <= SD_WP;
        end if;
    end process;
    
    -----------------------------------------------------------------
    -- The call to the raw SDblock controler
    -----------------------------------------------------------------

    SDcard_reset <= longreset or synced_CD;

    -- SD_raw_controler : entity work.SDcard_raw_access_V2
    SD_raw_simulated : entity work.SDcard_raw_access_simmodel_V2
        GENERIC MAP ( CLK_FREQ_HZ         => CLK_FREQ_HZ,
                    --   VIVADO_SYNTH        => VIVADO_SYNTH, --
                      DIRNAME             => DIRNAME,
                      WORD_SIZE_PW2       => SD_WORD_SIZE_PW2,
                      BUFFER_NUM_PW2      => BUFFER_NUM_PW2,
                      PARAMETER_BUFFERING => False,
                    --   HIGH_SPEED_IF       => HIGH_SPEED_IF, --
                      LITTLE_ENDIAN       => LITTLE_ENDIAN)
        PORT MAP ( clk        => clk,
                   reset      => SDcard_reset,

                   SD_block    => SD_block_num,
                   TR_buffer   => SD_op_buff,
                   read_block  => SD_read_blk,
                   write_block => SD_write_blk,
                   erase_block => SD_erase_blk,
                   multiple    => SD_multiple,
                   --prep_block  => SD_prep_block,
                   busy        => SD_busy,
                   err_code    => SD_err_code,
                   blocks      => SD_nb_blocks,
                   
                   loc_buffer  => SD_buffer,
                   address     => SD_address,
                   data_write  => SD_write,
                   data_in     => SD_data_in,
                   data_out    => SD_data_out,

                   SD_DAT     => SD_DAT,
                   SD_CD      => SD_CD,
                --    SD_WP      => SD_WP, --
                   SD_CLK     => SD_CLK,
                   SD_CMD     => SD_CMD);



    -----------------------------------------------------------------
    --
    --                   Debug
    --
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' or module_fsm = no_SD_card then
                debug     <= (others => '0');
                fat_error <= '0';
            elsif module_fsm = open_fd and check_fd_fsm = check_over and FD_op_success = '0' and FD_rw_mode = '0' and FD_file_type = '0' then
                debug     <= NO_FILE_FOUND;
                fat_error <= '1';
            elsif module_fsm = open_fd and check_fd_fsm = check_over and FD_op_success = '0' and FD_rw_mode = '0' and FD_file_type = '1' then
                debug     <= NO_DIRECTORY_FOUND;
                fat_error <= '1';
            elsif module_fsm = open_fd and check_fd_fsm = check_over and FD_op_success = '0' and FD_rw_mode = '1' and FD_file_type = '0' then
                debug     <= FILE_ALREADY_EXIST;
                fat_error <= '1';
            elsif module_fsm = open_fd and check_fd_fsm = check_over and FD_op_success = '0' and FD_rw_mode = '1' and FD_file_type = '1' then
                debug     <= DIRECTORY_ALREADY_EXIST;
                fat_error <= '1';
            elsif module_fsm = open_fd and check_fd_fsm = is_list_done and FD_cant_list_all_frag = '1' then
                debug     <= TOO_MANY_FRAG;
                fat_error <= '1';
            elsif module_fsm = idle and open_dir = '1' and tree_depth_full then
                debug     <= TREE_STRUCTURE_FULL;
                fat_error <= '1';
            elsif module_fsm = idle and close_dir = '1' and tree_depth = 0 then
                debug     <= TREE_STRUCTURE_ROOT;
                fat_error <= '1';
            elsif eject_busy = '1' then
                debug <= SDCARD_EJECTING;
            elsif module_fsm = ejected then
                debug <= SDCARD_EJECTED;
            else
                fat_error <= '0';
            end if;
        end if;
    end process;





    file_debug_addr <= debug_addr;
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                debug_data <= (others => '0');
            else
                case debug_addr is

                    -- Boot Record : 0xF1..
                    when x"F100" => debug_data <= (31 downto 24 => '0') & jump_code_NOP;
                    when x"F101" => debug_data <=                         OEM_name(63 downto 32);
                    when x"F102" => debug_data <=                         OEM_name(31 downto  0);
                    when x"F103" => debug_data <= (31 downto 16 => '0') & bytes_per_sector;
                    when x"F104" => debug_data <= (31 downto  8 => '0') & sectors_per_cluster;
                    when x"F105" => debug_data <= (31 downto 16 => '0') & reserved_sectors;
                    when x"F106" => debug_data <= (31 downto  8 => '0') & nb_FAT;
                    when x"F107" => debug_data <= (31 downto  8 => '0') & media_descriptor;
                    when x"F108" => debug_data <= (31 downto 16 => '0') & sectors_per_track;
                    when x"F109" => debug_data <= (31 downto 16 => '0') & nb_heads;
                    when x"F10A" => debug_data <=                         nb_hidden_sectors;
                    when x"F10B" => debug_data <=                         nb_sectors_in_partition;
                    when x"F10C" => debug_data <=                         nb_sectors_per_FAT;
                    when x"F10D" => debug_data <= (31 downto 16 => '0') & active_FAT;
                    when x"F10E" => debug_data <=                         root_directory_cluster;
                    when x"F10F" => debug_data <= (31 downto 16 => '0') & sector_nb_file_system_info;
                    when x"F110" => debug_data <= (31 downto 16 => '0') & sector_nb_backup_boot_sector;
                    when x"F111" => debug_data <= (31 downto  8 => '0') & logical_drive_nb;
                    when x"F112" => debug_data <= (31 downto  8 => '0') & extended_signature;
                    when x"F113" => debug_data <=                         serial_nb_of_partition;
                    when x"F114" => debug_data <= (95 downto 88 => '0') & volume_name_of_partition(87 downto 64);
                    when x"F115" => debug_data <=                         volume_name_of_partition(63 downto 32);
                    when x"F116" => debug_data <=                         volume_name_of_partition(31 downto  0);
                    when x"F117" => debug_data <=                         FAT_name(63 downto 32);
                    when x"F118" => debug_data <=                         FAT_name(31 downto  0);
                    when x"F119" => debug_data <= (31 downto 16 => '0') & boot_signature;
                    when x"F11A" => debug_data <=                         BR_nb_free_cluster;
                    when x"F11B" => debug_data <=                         recent_cluster;

                    when x"F140" => debug_data <=                       std_logic_vector(total_size_of_FATs);
                    when x"F141" => debug_data <=                       std_logic_vector(nb_total_cluster);
                    when x"F142" => debug_data <=(31 downto 8 => '0') & std_logic_vector(sector_per_cluster_cpy);
                    when x"F143" => debug_data <=                       std_logic_vector(data_area_begin_sector);

                    when x"F180" => debug_data <= std_logic_vector(to_unsigned(BR_bytes_cnt, 32));
                    when x"F181" => debug_data <=                                     BR_block_num;
                    when x"F182" => debug_data <= (31 downto BUFFER_NUM_PW2 => '0') & BR_loc_buffer;
                    when x"F183" => debug_data <= (31 downto  9 => '0')             & std_logic_vector(BR_address);

                    -- FAT : 0xF2..
                    when x"F200" => debug_data <= (31 downto  8 => '0') & std_logic_vector(FAT_nb_fat_check);
                    when x"F201" => debug_data <= (31 downto  1 => '0') &                  FAT_check_sector_done;
                    when x"F202" => debug_data <= (31 downto  1 => '0') &                  FAT_difference;
                    when x"F203" => debug_data <= (31 downto  1 => '0')                  & FAT_read_buffer;
                    when x"F204" => debug_data <= (31 downto  8 => '0')                  & FAT_1_buffer;
                    when x"F205" => debug_data <= (31 downto  1 => '0')                  & FAT_all_sectors_read;

                    when x"F241" => debug_data <= (31 downto 28 => '0') &                  FAT_free_clusters_start;
                    when x"F242" => debug_data <= (31 downto 28 => '0') & std_logic_vector(FAT_free_clusters_cnt);
                    when x"F244" => debug_data <=             std_logic_vector(to_unsigned(FAT_nb_free_clusters, 32));
                    when x"F245" => debug_data <= (63 downto 56 => '0')                  & FAT_chain_to_store(55 downto 32);
                    when x"F246" => debug_data <=                                          FAT_chain_to_store(31 downto  0);
                    when x"F247" => debug_data <= (31 downto  1 => '0')                  & FAT_new_chain_to_store;
                    when x"F248" => debug_data <=                                          FAT_cluster;
                    when x"F249" => debug_data <=             std_logic_vector(to_unsigned(FAT_bytes_cnt, 32));
                    when x"F24A" => debug_data <= (31 downto 28 => '0') & std_logic_vector(FAT_cluster_cnt);
                    when x"F24B" => debug_data <= (31 downto  1 => '0')                  & FAT_new_cluster;
                    when x"F250" => debug_data <= (31 downto  1 => '0')                  & fragment_ordering;

                    when x"F280" => debug_data <=                         std_logic_vector(FAT_block_num);
                    when x"F281" => debug_data <=                         std_logic_vector(FAT_block_num_1);
                    when x"F282" => debug_data <=                         std_logic_vector(FAT_block_num_k);
                    when x"F283" => debug_data <= (31 downto BUFFER_NUM_PW2 => '0')      & FAT_op_buff;
                    when x"F284" => debug_data <= (31 downto BUFFER_NUM_PW2 => '0')      & FAT_loc_buffer;
                    when x"F285" => debug_data <= (31 downto  9 => '0') & std_logic_vector(FAT_address);
                    when x"F286" => debug_data <= (31 downto  1 => '0') &                  FAT_multiple;



                    -- SDcard control signals : 0FA..
                    when x"FA00" => debug_data <=                                     SD_nb_blocks; -- Number of blocks available in SDcard
                    when x"FA01" => debug_data <=                                     SD_block_num;
                    when x"FA02" => debug_data <= (31 downto BUFFER_NUM_PW2 => '0') & SD_op_buff;
                    when x"FA03" => debug_data <= (31 downto  1 => '0')             & SD_read_blk;
                    when x"FA04" => debug_data <= (31 downto  1 => '0')             & SD_write_blk;
                    when x"FA05" => debug_data <= (31 downto  1 => '0')             & SD_erase_blk;
                    when x"FA06" => debug_data <= (31 downto  1 => '0')             & SD_multiple;
                    when x"FA07" => debug_data <= (31 downto  1 => '0')             & SD_busy;
                    when x"FA09" => debug_data <= (31 downto BUFFER_NUM_PW2 => '0') & SD_buffer;
                    when x"FA0A" => debug_data <= (31 downto  9 => '0')             & SD_address;
                    when x"FA0B" => debug_data <= (31 downto  1 => '0')             & SD_write;
                    when x"FA0C" => debug_data <= (31 downto  8 => '0')             & SD_data_in;
                    when x"FA0D" => debug_data <= (31 downto  8 => '0')             & SD_data_out;
                    when x"FA0E" => debug_data <= (31 downto  4 => '0')             & SD_err_code;  -- Error code from SDcard raw access
                    when x"FA0F" => debug_data <= (31 downto  1 => '0')             & synced_CD;    -- Card detect

                    -- FSM : 0xFF..
                    -- when x"FF02" => debug_data <= SD_state;                         -- SD FSM state
                    when x"FF03" => debug_data <= module_state;                     -- module FSM state
                    when x"FF04" => debug_data <= BR_state;
                    when x"FF05" => debug_data <= BR_state_err;
                    when x"FF06" => debug_data <= FAT_state;
                    when x"FF07" => debug_data <= frag_state;
                    when x"FF08" => debug_data <= check_fd_state;
                    when x"FF09" => debug_data <= info_state;
                    when x"FF0A" => debug_data <= SD_manag_state;
                    when x"FF0B" => debug_data <= SD_op_state;
                    when x"FF0F" => debug_data <= file_debug_data;  -- file fsm state


                    -- Some file debug data
                    when x"D000" => debug_data <= file_debug_data;
                    when x"D001" => debug_data <= file_debug_data;
                    when x"D002" => debug_data <= file_debug_data;
                    when x"D003" => debug_data <= file_debug_data;
                    when x"D004" => debug_data <= file_debug_data;
                    when x"D005" => debug_data <= file_debug_data;
                    when x"D006" => debug_data <= file_debug_data;
                    when x"D007" => debug_data <= file_debug_data;
                    when x"D008" => debug_data <= file_debug_data;
                    when x"D009" => debug_data <= file_debug_data;
                    when x"D00A" => debug_data <= file_debug_data;
                    when x"D00B" => debug_data <= file_debug_data;
                    when x"D00C" => debug_data <= file_debug_data;
                    when x"D00D" => debug_data <= file_debug_data;

                    when x"FFFE" => debug_data <= file_debug_data;
                    when x"FFFF" => debug_data <= x"FF89A002";
                    when others  => debug_data <= (others => '0');
                end case;
            end if;
        end if;
    end process;


    -----------------------------------------------------------------
    -- Main fsm debug
    -----------------------------------------------------------------
    Process(module_fsm)
    begin
        case module_fsm is
            when no_SD_card      => module_state <= x"00000000";
            when initializing    => module_state <= x"00000001";
            when boot_record     => module_state <= x"00000002";
            when fat             => module_state <= x"00000004";
            when idle            => module_state <= x"0000000C";
            when open_fd         => module_state <= x"00000101";
            when close_fd        => module_state <= x"00000102";
            when erase_fd        => module_state <= x"00000103";
            when close_directory => module_state <= x"00000105";
            when close_files     => module_state <= x"00000201";
            when recopy_FAT      => module_state <= x"00000202";
            when ejected         => module_state <= x"0000F0F0";
            when formating       => module_state <= x"00000401";
            when format_problem  => module_state <= x"00000402";
            when others          => module_state <= x"00001001";
        end case;
    end process;


    -----------------------------------------------------------------
    -- Boot Record fsm debug
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if longreset = '1' then
                BR_state_err <= (others => '0');
            elsif BR_error = '1' then
                BR_state_err <= BR_state;
            end if;
        end if;
    end process;

    Process(BR_fsm)
    begin
        case BR_fsm is
            when off                      => BR_state <= x"00000001";

            when read_BR                  => BR_state <= x"00000101";
            when analyse_BR               => BR_state <= x"00000102";
            when get_jump_code            => BR_state <= x"00000103";
            when get_OEM_name             => BR_state <= x"00000104";
            when get_bytes_per_sector     => BR_state <= x"00000105";
            when get_sectors_per_cluster  => BR_state <= x"00000106";
            when get_reserved_sectors     => BR_state <= x"00000107";
            when get_nb_FAT               => BR_state <= x"00000108";
            when get_media_descriptor     => BR_state <= x"00000109";
            when get_sectors_per_track    => BR_state <= x"0000010A";
            when get_nb_heads             => BR_state <= x"0000010B";
            when get_nb_hidden_sectors    => BR_state <= x"0000010C";
            when get_nb_sectors_partition => BR_state <= x"0000010D";
            when get_nb_sectors_per_FAT   => BR_state <= x"0000010E";
            when get_active_FAT           => BR_state <= x"0000010F";
            when get_root_dir_cluster     => BR_state <= x"00000110";
            when get_FSI_sector           => BR_state <= x"00000111";
            when get_backup_boot_sector   => BR_state <= x"00000112";
            when get_logical_drive_nb     => BR_state <= x"00000113";
            when get_extended_signature   => BR_state <= x"00000114";
            when get_serial_nb            => BR_state <= x"00000115";
            when get_volume_name          => BR_state <= x"00000116";
            when get_FAT_name             => BR_state <= x"00000117";
            when get_boot_signature       => BR_state <= x"00000118";

            when read_FSI                 => BR_state <= x"00000201";
            when analyse_FSI              => BR_state <= x"00000202";
            when get_nb_free_cluster      => BR_state <= x"00000203";
            when get_recent_cluster       => BR_state <= x"00000204";
            when final_check              => BR_state <= x"00000205"; 

            when BR_issue                 => BR_state <= x"0000FF01";
            when BR_end                   => BR_state <= x"0000FFFF";
        end case;
    end process;


    -----------------------------------------------------------------
    -- FAT fsm debug
    -----------------------------------------------------------------
    Process(FAT_fsm)
    begin
        case FAT_fsm is
            when off               => FAT_state <= x"00000001";
            when read_fat_1        => FAT_state <= x"00000101";
            when read_fat_1_wait   => FAT_state <= x"00000102";
            when read_fat_k        => FAT_state <= x"00000103";
            when read_fat_k_wait   => FAT_state <= x"00000104";
            when start_check       => FAT_state <= x"00000105";
            when waiting_check     => FAT_state <= x"00000106";
            when read_whole_FAT    => FAT_state <= x"00000201";
            when FAT_check_done    => FAT_state <= x"00000301";
            when idle              => FAT_state <= x"00000401";
            when frag_requested    => FAT_state <= x"00000402";
            when update_frag       => FAT_state <= x"00000403";
            when fat_issue         => FAT_state <= x"0000FF01";
            when FAT_end           => FAT_state <= x"0000FFFF";
        end case;
    end process;


    -----------------------------------------------------------------
    -- write fragment/cluster chain in FAT fsm debug
    -----------------------------------------------------------------
    Process(frag_fsm)
    begin
        case frag_fsm is
            when off            => frag_state <= x"00000001";
            when get_fragment   => frag_state <= x"00000101";
            when is_over        => frag_state <= x"00000102";
            when read_sector    => frag_state <= x"00000201";
            when wait_read      => frag_state <= x"00000202";
            when write_fragment => frag_state <= x"00000301";
            when write_sector   => frag_state <= x"00000401";
            when wait_write     => frag_state <= x"00000402";
        end case;
    end process;

    -----------------------------------------------------------------
    -- check_fd fsm debug
    -----------------------------------------------------------------
    Process(check_fd_fsm)
    begin
        case check_fd_fsm is
            when off               => check_fd_state <= x"00000001";
            when load_directory    => check_fd_state <= x"00000101";
            when wait_directory    => check_fd_state <= x"00000102";
            when start_check       => check_fd_state <= x"00000201";
            when check_filename    => check_fd_state <= x"00000202";
            when check_attribute   => check_fd_state <= x"00000203";
            when next_file         => check_fd_state <= x"00000204";
            when next_sector       => check_fd_state <= x"00000206";
            when read_dir_fat      => check_fd_state <= x"00000207";
            when wait_dir_fat      => check_fd_state <= x"00000208";
            when get_next_cluster  => check_fd_state <= x"00000209";
            when next_cluster      => check_fd_state <= x"0000020A";
            when calc_next_cluster => check_fd_state <= x"0000020B";
            when end_of_dir        => check_fd_state <= x"0000020C";
            when get_start_cluster => check_fd_state <= x"00000301";
            when get_size          => check_fd_state <= x"00000302";
            when is_file_empty     => check_fd_state <= x"00000303";
            when read_fat          => check_fd_state <= x"00000304";
            when wait_fat          => check_fd_state <= x"00000305";
            when list_clusters     => check_fd_state <= x"00000306";
            when list_clust_dly    => check_fd_state <= x"00000307";
            when is_list_done      => check_fd_state <= x"00000308";
            when read_dir_sector   => check_fd_state <= x"00000401";
            when wait_dir_sector   => check_fd_state <= x"00000402";
            when append_directory  => check_fd_state <= x"00000403";
            when create_dir        => check_fd_state <= x"00000404";
            when create_file       => check_fd_state <= x"00000405";
            when write_sector_back => check_fd_state <= x"00000406";
            when wait_write_end    => check_fd_state <= x"00000407";
            when open_directory    => check_fd_state <= x"00000501";
            when check_over        => check_fd_state <= x"00000601";
        end case;
    end process;


    -----------------------------------------------------------------
    -- update file info fsm debug
    -----------------------------------------------------------------
    Process(info_fsm)
    begin
        case info_fsm is
            when off          => info_state <= x"00000001";
            when read_sector  => info_state <= x"00000101";
            when wait_read    => info_state <= x"00000102";
            when copy         => info_state <= x"00000201";
            when write_sector => info_state <= x"00000301";
            when wait_write   => info_state <= x"00000302";
            when over         => info_state <= x"00000401";
        end case;
    end process;


    -----------------------------------------------------------------
    -- SD manager fsm debug
    -----------------------------------------------------------------
    -- Process(SDm_fsm)
    -- begin
    --     case SDm_fsm is
    --         when idle           => SD_manag_state <= x"00000001";
    --         when give_free_frag => SD_manag_state <= x"00000201";
    --         when wait_next_op   => SD_manag_state <= x"00000201";
    --         when ack_file_op    => SD_manag_state <= x"00000301";
    --         when wait_op        => SD_manag_state <= x"00000302";
    --     end case;
    -- end process;

    -----------------------------------------------------------------
    -- SD operation fsm debug
    -----------------------------------------------------------------
    -- Process(SD_op_fsm)
    -- begin
    --     case SD_op_fsm is
    --         when off             => SD_op_state <= x"00000001";
    --         when read_sector     => SD_op_state <= x"00000201";
    --         when wait_read       => SD_op_state <= x"00000202";
    --         when transfert_read  => SD_op_state <= x"00000203";
    --         when transfert_write => SD_op_state <= x"00000301";
    --         when write_sector    => SD_op_state <= x"00000302";
    --         when wait_write      => SD_op_state <= x"00000303";
    --     end case;
    -- end process;


end Behavioral;