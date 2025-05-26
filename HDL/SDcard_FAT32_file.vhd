----------------------------------------------------------------------------------------------------
-- Sdcard_FAT32_file
--    Version V1.0 (2021/09/24)
--    (c)2021 J. CHESLET - student at Bordeaux INP / ENSEIRB-MATMECA - Internship at IMS Laboratory
--    This module manages a file for a FAT32 file system (on SD card).
--
-- @copyright
-- SPDX-FileCopyrightText: © 2021 Jeremy Cheslet <jeremy.cheslet@u-bordeaux.fr>
-- SPDX-License-Identifier: MIT
----------------------------------------------------------------------------------------------------
-- Working version.
-- TODO list :
--    Handle written file of more than 2GB of data
--    Inform the user when a file reach 512 fragments (writting) (and close the file ?) (it's maybe
--    possible to write the cluster chain in the FAT to empty to table and go on...)
--
--
-- known bugs :
--    - No limitation for the number of fragment reserved (write). Exceed 512 fragments should
--    - overwrite the first fragment and broke the file clusters chain...
--
--
-- Unknow behavior: - File with more than 512 fragments (this should be ok but hasn't been tested :/)
--                  -
--
-------------------------------------------------------------------------------------------------------
-- What this module does :
-------------------------------------------------------------------------------------------------------
-- This module handle operation and data for 1 file. It works in read mode or in write mode, and cannot
-- be changed until the file manager is shutdown/reset.
-- The operator have a direct access to FIFO i/o from/through the main module.
-- It can request an operation from the main module  to empty his FIFO (read) / to feed his FIFO (write).
-- Each operation requests do NB_MULTIPLE_OP operation when it is accepted by the scheduler. The intern
-- sector number used to address to right sector is increased by NB_MULTIPLE_OP when the operation has
-- been acknoledge.
--
--
--
-- BRAM consumption: 
--    - It consums 1 BRAM to store fragments of clusters (read and write)
--    - It consums [ FIFO_DEPTH*((2**WORD_SIZE_PW2)-1) ] / 36kb BRAM, i.e. 1 BRAM for a FIFO depth of 
--      4096 and 8-bit data (WORD_SIZE_PW2 = 0). So the minimum is 1 BRAM (maybe 1/2 BRAM is possible).
-- => Minimal BRAM consumption per file instanciated is 2 BRAM (maybe 3/2 BRAM)
--
--
--
--
-- close file in read mode:
----------------------------------------------------------
-- To close a file in read mode, when no more data can be read for the user or when he ask for closure,
-- the module, basically, reset itself and put opened flag to '0'. No more action is required.
-- Then, the main module can use this file manager again.
--
--
-- close file in write mode:
----------------------------------------------------------
-- To close a file in write mode, when the user command it directly or because of eject command,
-- this module start by writting the remaining data in the FIFO onto the SD card.
-- When the FIFO is empty, it requests to write the chain of used clusters in the FAT and 
-- transferts fragments to the used fragment manager in the main module, one by one, each time
-- a fragment has been written on the SD card.
-- When the clusters chain is done, the module request to write/update file informations of
-- the current file in the file descriptor. Thus, the main module use the sector number,
-- the address of the file descriptor in that sector, the starting cluster of the file and
-- the file size (all) from this module interface.
-- Finaly, when the file info update operation has been acknowledge by the main module,
-- the file manager closes.
-- 
--
-- read/write data from/to the file:
----------------------------------------------------------
-- Data is read/written from/to& the file when enable_in = '1' and enable_out = '1'
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
use work.data_bus_pkg.all;

-- TODO: - implement VIVADO_SUNTH generic
--       - implement WORD_SIZE_PW2 generic in main module input...
--       - do something with CLK_FREQ__HZ generic ? (seems very useless in this sub module)

entity Sdcard_FAT32_file is
    Generic(CLK_FREQ_HZ    : integer := 100000000; -- the frequency of the clock expressed in Hz
            VIVADO_SYNTH   : boolean := True;      -- select the correct part of code to use to get a proper RAM implementation in vivado
            NB_MULTIPLE_OP : integer :=    8;      -- Number of sectors to operate at each operation
            WORD_SIZE_PW2  : integer :=    0;      -- power of 2 of the size or word to use
            FIFO_DEPTH     : integer := 4096);     -- maximum size of internal fifo for file reading

    Port (clk                  : in  STD_LOGIC;                         -- Main clock, frequency given by generic CLK_FREQ_HZ
          reset                : in  STD_LOGIC;                         -- reset, active high

          -- input interface
          filename             : in  STD_LOGIC_VECTOR (87 downto 0);    -- file name : 8 + '.' + 3
          date_time            : in  STD_LOGIC_VECTOR (39 downto 0) := (others => '0'); -- Creation time, can be ignored
          read_write_in        : in  STD_LOGIC;                         -- read = '0' ; write = '1'
          read_write_out       : out STD_LOGIC;                         -- read = '0' ; write = '1'
          reserve_clusters_in  : in  STD_LOGIC_VECTOR (21 downto 0);    -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case
          reserve_clusters_out : out STD_LOGIC_VECTOR (21 downto 0);    -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case
          
          opened               : out STD_LOGIC;                         -- high if the file is openned
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
          fragment_in          : in  fragment;                          -- fragment receive from free fragment manager (write mode) or file fragment (read mode)
          last_fragment        : in  STD_LOGIC;                         -- annonce the last fragment (when collecting file fragment for reading)
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
end Sdcard_FAT32_file;

architecture Behavioral of Sdcard_FAT32_file is
    -------------------------------------------------------------------------
    --
    --                      Debug signals
    --
    -------------------------------------------------------------------------
    constant NOTHING_TO_SAY : std_logic_vector(7 downto 0) := "00000000";

    -- INFORMATION CODES
    constant FILE_OPENED_R : std_logic_vector(7 downto 0) := "00000010"; -- the file succesfuly opened and load every cluster (READ)
    constant FILE_OPENED_W : std_logic_vector(7 downto 0) := "00000011"; -- the file succesfuly opened and the file descriptor has been writed

    -- WARNING CODES
    constant CANNOT_LOAD_ALL_FRAGMENT : std_logic_vector(7 downto 0) := "01000001"; -- file is too much fragmented, some data will be skipped

    -- ERROR CODES
    -- TODO: move the next error into a warning or information, i.e. create multpiple file when a file exceed 2GB
    constant FILE_FULL : std_logic_vector(7 downto 0) := "10000001"; -- writing is not possible, no space available



    signal read_state  : std_logic_vector(31 downto 0);  -- current read fsm state
    signal write_state : std_logic_vector(31 downto 0);  -- current write fsm state
    signal file_state  : std_logic_vector(31 downto 0);  -- current file manager state



    constant BYTES_PER_SECTOR    : integer := 512;                        -- inherent to SDcard
    constant WORD_SIZE           : integer := 8*(2**WORD_SIZE_PW2);
    constant SD_write_buffer     : std_logic_vector(2 downto 0) := "000"; -- might be not used.
    constant SD_read_buffer      : std_logic_vector(2 downto 0) := "010"; -- might be not used.

    -------------------------------------------------------------------------
    -- Signals used to store input parameters
    -------------------------------------------------------------------------

    signal read_write_i       : std_logic;                     -- file opening mode
    signal filename_i         : std_logic_vector(87 downto 0); -- copy of filename
    signal date_time_i        : std_logic_vector(39 downto 0); -- copy of the creation date & time
    signal reserve_clusters_i : std_logic_vector(21 downto 0); -- number of cluster to reserve each time the file need to write on SDcard

    -------------------------------------------------------------------------
    -- read file FSM
    -------------------------------------------------------------------------
    type t_read_fsm is (off,           -- off
                        get_size,      -- get the size in bytes of the file
                        list_clusters, -- get the clusters used by the file
                        idle_read,     -- let user read
                        request_read,  -- request the SD manager to read a sector
                        read_data,     -- get the data read from the sector
                        closed);       -- close the file and reset RAMs                 useless ??
    -- signal read_fsm : t_read_fsm;

    signal listing_cluster_done  : boolean; -- used to inform read_fsm. True when every clusters have been stored (in the limit of available place)
    signal cluster_list_complete : boolean; -- True when every clusters of the file have been stored
    -- if both of the above signal are true at the same time, then the file can be fully read 

    signal can_read   : boolean;  -- use to inform read_fsm that a sector can be read
    signal fully_read : boolean;  -- true when the file has copied every bytes of a file into the fifo or when no more data can be read (cluster list hasn't been fully saved).
                                  -- In short, we reach the end of the file when every bytes have been read or when we are on the last fragment and we ask the next one

    -------------------------------------------------------------------------
    -- write file FSM
    -------------------------------------------------------------------------
    type t_write_fsm is (off,               -- off
                         get_directory,     -- get directory sector and address offset of te file descriptor (in the sector)
                         idle_write,        -- let user write data
                         reserve_clusters,  -- reserve cluster to write data on SDcard
                         request_write,     -- request the SD manager to write a sector
                         write_data,        -- transfer data to the SD manager buffer
                         write_chain,       -- request the cluster chain update and transfert fragment of used clusters
                         file_info);        -- update the file info (starting cluster and file final size)
    -- signal write_fsm : t_write_fsm;

    signal can_write            : boolean; -- their is enough data to 
    signal no_clusters_reserved : boolean; -- true when the file manager is out of clusters
    signal cluster_chain_done   : boolean; -- every fragment used by the file has been sent (to be written as the cluster chain)

    -- file information (only for writing)
    signal info_sector_num_i  : std_logic_vector(31 downto 0);           -- sector where file's infos are stored
    signal info_addr_offset_i : std_logic_vector( 3 downto 0);           -- 512/32 = 16 files per sector, this signal times 32 store the address of the beginning of the file into
    signal starting_cluster_i : std_logic_vector(27 downto 0);         -- save/copy the starting cluster of the file

    -------------------------------------------------------------------------
    -- size of the file signals + opened state
    -------------------------------------------------------------------------
    signal opened_i        : std_logic;                  -- state of the file manager
    signal size_i          : unsigned(31 downto 0);      -- size of the file
    signal total_bytes_cnt : integer range 0 to 2**31-1;   -- bytes read or write to a file

    -------------------------------------------------------------------------
    -- these are used to request operation to SD FAT manager (top module)
    -------------------------------------------------------------------------
    signal read_sector_i  : std_logic;  -- set to request a read operation
    signal write_sector_i : std_logic;  -- set to request a write operation
    signal request_close  : std_logic;  -- set when the file is request to close

    -- File Module FSM:
    -- It contains 2 separates way, starting from the idle state OFF:
    --      - file in read mode:
    --              - When a file to read has been successfuly found and the file id to use is this file manager id,
    --                the current state goes from off to the read branch.
    --              - It starts by waiting the main module to send it the size (in bytes) of the file to read.
    --              - Then, it waits all fragments of clusters (received one by one) of the file (MAX: 512).
    --              - Finaly, it enter the state idle_read where the file become opened. From here, the file manager
    --                can request to read data from SD card if the FIFO has enough free memory. The user can read
    --                data from the FIFO as long as it contains data.
    --              - When the user close the file (eject is similar) or when there is no more sectors to read
    --                on the SD card and the user has emptyed the FIFO, i.e. all the file data has been read by the
    --                user, the file close in one clock cycle, making the file manager free to open another file.
    --
    --      - file in write mode:
    --              - When no file has been found with the given filename in the current directory and then the file
    --                has successfuly been created (and it file id match this file manager id), the current state goes
    --                from off to the write branch.
    --              - It starts by receiving the sector where the file descriptor is and the address offset of the file
    --                in this sector.
    --              - Then, the file is considered open and the user can write in the FIFO, which will be transfered to
    --                SD card on due time.
    --              - When enough data are in the data FIFO, the file manager request a write operation to the main
    --                module if it has free reserved clusters to use. If not, it requests free clusters to the main
    --                module and it will receive a fragment between 1 and the reserve_clusters nb (which was given 
    --                at the opening of the file)
    --              - When the user close the file (eject is similar), the file manager request write operation until
    --                his FIFO become empty (even if the FIFO doesn't have the minimum requiered data to request a 
    --                write operation).
    --              - Once all the data has been written, the file manager request to write the file clusters chain.
    --                It transferts used fragments one by one, from the last to the first one. The last fragment is
    --                followed by a fragment with a size of 0, to inform the used fragment manager (from the main
    --                module) that the clusters chain is complete.
    --              - Finaly, it requests to update file information, i.e. the starting cluster and the size in bytes.
    --                When this last request is acknowledge by the main module, the file is closed abd the file
    --                manager goes the OFF, ready to open another file.
    -- 
    -- Since multiple has been added, it read and write NB_MULTIPLE_OP sectors for each request
    type t_file_fsm is (off,                -- file manager is free to open a file
                        -- read
                        get_size,           -- get the size in bytes of the file
                        list_clusters,      -- get the clusters used by the file
                        idle_read,          -- let user read
                        request_read,       -- request the SD manager to read NB_MULTIPLE_OP sectors
                        read_data,          -- get the data read from the sector
                        -- write
                        get_directory,      -- get directory sector and address offset of the file descriptor (in the sector)
                        idle_write,         -- let user write data
                        reserve_clusters,   -- reserve cluster to write data on SDcard
                        request_write,      -- request the SD manager to write NB_MULTIPLE_OP sectors
                        write_data,         -- transfer data to the SD manager buffer
                        write_chain,        -- request the cluster chain update and transfert fragment of used clusters
                        file_info);         -- update the file info (starting cluster and file final size)
    signal file_fsm : t_file_fsm;

    -- signal and array use to get/use fragments of clusters
    -------------------------------------------------------------------------
    -- signals and RAM to manage fragment (clusters & nb of consecutive clusters)
    -------------------------------------------------------------------------
    constant CLUSTER_BIT            : integer := 28;
    constant MAX_CLUSTER_PER_FILE_PW2: integer := 22;
    constant FRAG_BIT_SIZE          : integer := CLUSTER_BIT + MAX_CLUSTER_PER_FILE_PW2;
    constant MAX_NB_FRAG            : integer := 512;
    type t_file_frag is array (0 to MAX_NB_FRAG-1) of fragment;
    -- Fragment are composed of the starting cluster number (28 bits) concatenated with the number of consecutive cluster (22 bits)
    -- Fragment with a size of 0 means the cluster is alone. A size of 1 means 2 clusters,...
    -- A cluster number of 0 means there is no entry as cluster number cannot be 0 (reserved by FAT32 specification)
    signal frag             : t_file_frag;                                  -- the BRAM use to store fragment to read or to write
    signal fragment_read    : fragment;                                     -- output fragment of the BRAM (pointed by frag_ptr)
    signal fragment_write   : fragment;                                     -- fragment to write in the BRAM
    signal frag_write_en    : std_logic;                                    -- write enable for the BRAM
    signal frag_ptr         : integer range 0 to MAX_NB_FRAG-1;             -- fragment table pointer
    signal nb_frag          : integer range 0 to MAX_NB_FRAG;               -- current number of fragment in the BRAM

    signal last_fragment_dly : std_logic;                                       -- delay of 1 clk cycle of last_fragment
    signal concatenate_frag  : boolean;                                         -- indicate to the fragment RAM manager if the new reserved fragment is an extension of the precedent fragment
    signal reserve_ack_dly   : std_logic;                                       -- Delay the insertion of the newly reserved fragment of 1 clk cycle to let time to compute if we can concatenate the fragment. It should permit to help timing implemantation.

    signal load_first_frag    : std_logic;                                      -- load the first fragment for sector management(i.e. calculate the first sector of the file to read)
    signal next_frag          : std_logic;                                      -- tell the fragment manager to put the next fragment in current frag
    signal waiting_new_sector : std_logic;                                      -- set when we have reach the last sector of the current fragment and reset when the new sector has been calculated. It also makes next_frag an impulse

    signal wait_new_fragment    : std_logic_vector(2 downto 0);                 -- shift register to delay the calcul of the new sector after it has been reserve (and read from the RAM)

    -------------------------------------------------------------------------
    -- these are used to request operation to SD FAT manager (top module)
    -------------------------------------------------------------------------
    signal current_frag      : fragment;                                        -- current frag read
    signal start_cluster     : std_logic_vector(CLUSTER_BIT-1 downto 0);        -- starting cluster of the current fragment
    signal current_frag_size : unsigned(MAX_CLUSTER_PER_FILE_PW2-1 downto 0);   -- size of the current fragment
    signal prev_frag_size    : unsigned(MAX_CLUSTER_PER_FILE_PW2-1 downto 0);   -- size of the previous fragment, used when new fragment is concatenated with the previous fragment

    -- sector number and sector counter in cluster, in fragment
    signal sector_num_i      : unsigned(31 downto 0);                           -- sector to operate
    signal sector_ready      : std_logic;                                       -- indicate if the first sector of the cluster has been calculated
    signal sect_in_clust_cnt : unsigned(7 downto 0);                            -- cnt the sector to read in the cluster, and fetch next cluster when a cluster has been read
    signal clust_in_frag_cnt : unsigned(MAX_CLUSTER_PER_FILE_PW2-1 downto 0);   -- sector counter, in a cluster, in a fragment

    -------------------------------------------------------------------------
    -- calculate sector given by a cluster
    -------------------------------------------------------------------------
    -- Signals used to calculate to sector with a given cluster and the sector focused in this cluster
    signal calc_sector     : std_logic;                     -- start signal to calculate
    signal calc_sector_bsy : std_logic;                     -- set if calculating
    signal rem_shifts      : unsigned( 6 downto 0);         -- copy of sector per clusters during init, to know how many shift to do
    signal buffer_to_shift : unsigned(31 downto 0);         -- shift register
    signal block_num       : unsigned(31 downto 0);         -- final sector address
    signal block_num_rdy   : std_logic;                     -- set when block num is ready

    -------------------------------------------------------------------------
    -- signals related to DATA
    -------------------------------------------------------------------------
    -- fifo 
    signal fifo_data_in      : std_logic_vector(WORD_SIZE-1 downto 0);
    signal fifo_dwrite       : std_logic;
    signal fifo_full_n       : std_logic;
    signal fifo_elemts       : integer range 0 to FIFO_DEPTH;
    
    signal fifo_data_out     : std_logic_vector(WORD_SIZE-1 downto 0);
    signal fifo_dread        : std_logic;
    signal fifo_empty_n      : std_logic;

    signal fifo_reset        : std_logic;   -- signal used to reset the fifo
    
    signal enable_out_i      : std_logic;                               -- internal value of enable_out, might be unused now

    component FIFO_Generic is
        Generic (DATA_SIZE  : integer;
                 FIFO_DEPTH : integer := 512);
        Port ( clk      : in  std_logic;
               reset    : in  std_logic;

               data_in  : in  std_logic_vector(DATA_SIZE - 1 downto 0);
               dwrite   : in  std_logic;
               full_n   : out std_logic;

               elemts   : out integer range 0 to FIFO_DEPTH;
               data_out : out std_logic_vector(DATA_SIZE - 1 downto 0);
               dread    : in  std_logic;
               empty_n  : out std_logic);
    end component;

begin

    -----------------------------------------------------------------
    -- We get input parameters
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off and open_file = '1' then
                read_write_i <= read_write_in;
                if read_write_in = '1' then
                    if unsigned(reserve_clusters_in) = 0 then
                        reserve_clusters_i <= (21 downto 1 => '0') & '1';   -- reserve 1 cluster at each reservation if no nb have been assigned
                    else
                        reserve_clusters_i <= reserve_clusters_in;
                    end if;
                    filename_i         <= filename;
                    date_time_i        <= date_time;
                end if;
            end if;
        end if;
    end process;

    read_write_out       <= read_write_i;
    reserve_clusters_out <= reserve_clusters_i;
    -- the next affectation is better but some problems occurs when sending the last fragment of the file (first to write in FAT)
    -- reserve_clusters_out <= (21 downto 1 => '0') & '1' when request_close = '1' and read_write_i = '1' else reserve_clusters_i;



    -----------------------------------------------------------------
    -- File management FSM
    -----------------------------------------------------------------
    -- -- Read FSM
    -- Process(clk)
    -- begin
    --     if rising_edge(clk) then
    --         if reset = '1' or format = '1' then
    --             read_fsm <= off;
    --         else
    --             case read_fsm is
    --                 when off           => if open_file = '1' and read_write_in = '0' and write_fsm = off then read_fsm <= get_size;      end if;
    --                 when get_size      => if op_ack = '1'                                                then read_fsm <= list_clusters; end if;
    --                 when list_clusters => if size_i = 0                                                  then read_fsm <= closed;
    --                                    elsif last_fragment_dly = '1' or nb_frag = 512                    then read_fsm <= idle_read;     end if;
    --                 when idle_read     => if request_close = '1'                                         then read_fsm <= closed;
    --                                    elsif can_read                                                    then read_fsm <= request_read;
    --                                    elsif fifo_empty_n = '0'                                          then read_fsm <= closed;        end if; -- and fully_read
    --                 when request_read  => if op_ack = '1'                                                then read_fsm <= read_data;     end if;
    --                 when read_data     => if op_ack = '1'                                                then read_fsm <= idle_read;     end if;
    --                 when closed        =>                                                                     read_fsm <= off;
    --             end case;
    --         end if;
    --     end if;
    -- end process;

    -- -- Write FSM
    -- Process(clk)
    -- begin
    --     if rising_edge(clk) then
    --         if reset = '1' or format = '1' then
    --             write_fsm <= off;
    --         else
    --             case write_fsm is
    --                 when off              => if open_file = '1' and read_write_in = '1' and read_fsm = off then write_fsm <= get_directory;    end if;
    --                 when get_directory    => if op_ack = '1'                                               then write_fsm <= idle_write;       end if;
    --                 when idle_write       => if request_close = '1' and size_i = 0 and fifo_empty_n = '0'  then write_fsm <= off;
    --                                       elsif can_write and no_clusters_reserved                         then write_fsm <= reserve_clusters;
    --                                       elsif can_write                                                  then write_fsm <= request_write;
    --                                       elsif request_close = '1' and fifo_empty_n = '0'                 then write_fsm <= write_chain;      end if;
    --                 when reserve_clusters => if op_ack = '1'                                               then write_fsm <= request_write;    end if;
    --                 when request_write    => if op_ack = '1'                                               then write_fsm <= write_data;       end if;
    --                 when write_data       => if op_ack = '1'                                               then write_fsm <= idle_write;       end if;
    --                 when write_chain      => if cluster_chain_done                                         then write_fsm <= file_info;        end if;
    --                 when file_info        => if op_ack = '1'                                               then write_fsm <= off;              end if;
    --             end case;
    --         end if;
    --     end if;
    -- end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' or format = '1' then
                file_fsm <= off;
            else
                case file_fsm is
                    when off              => if open_file = '1' and read_write_in = '0'                   then file_fsm <= get_size;
                                          elsif open_file = '1' and read_write_in = '1'                   then file_fsm <= get_directory;    end if;
                    -- Read
                    when get_size         => if op_ack = '1'                                              then file_fsm <= list_clusters;    end if;
                    when list_clusters    => if size_i = 0                                                then file_fsm <= off;
                                          elsif last_fragment_dly = '1' or nb_frag = 512                  then file_fsm <= idle_read;        end if;
                    when idle_read        => if request_close = '1'                                       then file_fsm <= off;
                                          elsif can_read                                                  then file_fsm <= request_read;
                                          elsif fifo_empty_n = '0'                                        then file_fsm <= off;              end if;
                    when request_read     => if op_ack = '1'                                              then file_fsm <= read_data;        end if;
                    when read_data        => if op_ack = '1'                                              then file_fsm <= idle_read;        end if;

                    -- Write
                    when get_directory    => if op_ack = '1'                                              then file_fsm <= idle_write;       end if;
                    when idle_write       => if request_close = '1' and size_i = 0 and fifo_empty_n = '0' then file_fsm <= off;
                                          elsif can_write and no_clusters_reserved                        then file_fsm <= reserve_clusters;
                                          elsif can_write                                                 then file_fsm <= request_write;
                                          elsif request_close = '1' and fifo_empty_n = '0'                then file_fsm <= write_chain;      end if;
                    when reserve_clusters => if reserve_ack_dly = '1'                                     then file_fsm <= request_write;    end if;-- op_ack = '1'
                    when request_write    => if op_ack = '1'                                              then file_fsm <= write_data;       end if; 
                    when write_data       => if op_ack = '1'                                              then file_fsm <= idle_write;       end if;
                    when write_chain      => if cluster_chain_done                                        then file_fsm <= file_info;        end if;
                    when file_info        => if op_ack = '1'                                              then file_fsm <= off;              end if;
                end case;
            end if;
        end if;
    end process;

    -- close file management
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                request_close <= '0';
            elsif close_file = '1' then
                request_close <= '1';
            end if;
        end if;
    end process;


    -- opened state update
    opened <= opened_i;
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                opened_i <= '0';
            elsif file_fsm = list_clusters or file_fsm = get_directory then --read_fsm = idle_read or file_fsm = idle_write then
                opened_i <= '1';
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- file information (size, starting cluster, sector nb) 
    -----------------------------------------------------------------
    -- file size update
    size <= std_logic_vector(size_i);
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                size_i <= (others => '0');
            elsif file_fsm = get_size and op_ack = '1' then     -- read
                size_i <= unsigned(size_in);
            elsif file_fsm = idle_write or file_fsm = reserve_clusters or file_fsm = request_write or file_fsm = write_data then      -- write
                size_i <= to_unsigned(total_bytes_cnt, 32);
            end if;
        end if;
    end process;


    -- file info sector for writing mode
    info_addr_offset_out <= info_addr_offset_i; -- output the index of the file descriptor in the (directory) sector (eq to an offset)
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = get_directory and op_ack = '1' then
                info_sector_num_i  <= info_sector_num;
                info_addr_offset_i <= info_addr_offset_in;
            end if;
        end if;
    end process;

    -- save starting cluster to ease other operation, i.e. the first cluster of the first fragment which has been reserved
    starting_cluster <= starting_cluster_i;
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                starting_cluster_i <= (others => '0');
            elsif file_fsm = reserve_clusters and op_ack = '1' and nb_frag = 0 then
            -- elsif file_fsm = reserve_clusters and reserve_ack_dly = '1' and nb_frag = 0 then
                starting_cluster_i <= fragment_in.cluster;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- Fragments of clusters 
    -----------------------------------------------------------------
    -- Read only. Check when the cluster listing is over and if it is complete.
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                listing_cluster_done  <= false;
                cluster_list_complete <= false;
            elsif file_fsm = list_clusters and op_ack = '1' and last_fragment = '1' then
                listing_cluster_done  <= true;
                cluster_list_complete <= true;
            elsif file_fsm = list_clusters and op_ack = '1' and nb_frag = 511 and last_fragment = '0' then
                listing_cluster_done  <= true;
                cluster_list_complete <= false;
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                last_fragment_dly <= '0';
            elsif file_fsm = list_clusters then
                last_fragment_dly <= last_fragment;
            else
                last_fragment_dly <= '0';
            end if;
        end if;
    end process;
    
    -- follow the number of fragment
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                nb_frag <= 0;
            elsif (file_fsm = list_clusters    and op_ack = '1')
            --    or (file_fsm = reserve_clusters and op_ack = '1' and nb_frag < MAX_NB_FRAG and not(concatenate_frag)) then  -- test on nb_frag < MAX_NB_FRAG, why ? we shouldn't ask fragment if we cannot accept it
               or (file_fsm = reserve_clusters and reserve_ack_dly = '1' and nb_frag < MAX_NB_FRAG and not(concatenate_frag)) then  -- test on nb_frag < MAX_NB_FRAG, why ? we shouldn't ask fragment if we cannot accept it
                nb_frag <= nb_frag + 1;
            end if;
        end if;
    end process;

    -- Determine whether the newly reversed fragment of free cluster can be concatenate with the previous fragment
    -- concatenate_frag <= unsigned(fragment_in.cluster) = unsigned(fragment_read.cluster) + fragment_read.size and nb_frag /= 0; -- and file_fsm = reserve_clusters and op_ack = '1';

    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = reserve_clusters and op_ack = '1' then
                concatenate_frag <= unsigned(fragment_in.cluster) = unsigned(fragment_read.cluster) + fragment_read.size and nb_frag /= 0;
            end if;
        end if;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if op_ack = '1' then
                reserve_ack_dly <= '1';
            else
                reserve_ack_dly <= '0';
            end if;
        end if;
    end process;

    -- Frag's RAM pointer management
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                frag_ptr <= 0;
            -- reading
            elsif file_fsm = list_clusters and (last_fragment_dly = '1' or nb_frag = 512) then  -- End of the cluster chain or no more space available to store more clusters (fragment)
                frag_ptr <= 0;
            elsif file_fsm = list_clusters and op_ack = '1' and nb_frag /= 0 then
                frag_ptr <= frag_ptr + 1;
            elsif (file_fsm = idle_read or file_fsm = request_read or file_fsm = read_data) and next_frag = '1' and frag_ptr /= MAX_NB_FRAG-1 then    -- request the next fragment to read
                frag_ptr <= frag_ptr + 1;
            -- writing
            -- elsif file_fsm = reserve_clusters and op_ack = '1' and nb_frag /= 0 and not(concatenate_frag) then
            elsif file_fsm = reserve_clusters and reserve_ack_dly = '1' and nb_frag /= 0 and not(concatenate_frag) then
                frag_ptr <= frag_ptr + 1;
            elsif file_fsm = write_chain and op_ack = '1' and frag_ptr /= 0 then
                frag_ptr <= frag_ptr - 1;
            end if;
        end if;
    end process;

    -- Here is update the data to write in the frag RAM
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                fragment_write.cluster <= (others => '0');
                fragment_write.size    <= (others => '0');
            elsif file_fsm = list_clusters    and op_ack = '1' then
                fragment_write <= fragment_in;
            -- elsif file_fsm = reserve_clusters and op_ack = '1' then
            elsif file_fsm = reserve_clusters and reserve_ack_dly = '1' then
                if concatenate_frag then
                -- if unsigned(fragment_in(FRAG_BIT_SIZE-1 downto FRAG_BIT_SIZE-CLUSTER_BIT)) = unsigned(start_cluster) + current_frag_size and frag_ptr > 0 then -- or fragment_read(cluster_nb) + fragment_read(it_size) = fragment_in(cluster_nb)
                    fragment_write.cluster <= fragment_read.cluster;
                    fragment_write.size    <= fragment_read.size + fragment_in.size;
                else
                    fragment_write <= fragment_in;
                end if;
            end if;
        end if;
    end process;

    -- Write en for frag's BRAM 
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                frag_write_en <= '0';
            elsif (file_fsm = list_clusters    and op_ack = '1')
            --    or (file_fsm = reserve_clusters and op_ack = '1') then
               or (file_fsm = reserve_clusters and reserve_ack_dly = '1') then
                frag_write_en <= '1';
            else
                frag_write_en <= '0';
            end if;
        end if;
    end process;

    -- Access to RAM
    Process(clk)
    begin
        if rising_edge(clk) then
            fragment_read <= frag(frag_ptr);
            if frag_write_en = '1' then
                frag(frag_ptr) <= fragment_write;
            end if;
        end if;
    end process;

    -- TODO: - it seems that this mux is totally useless
    current_frag <= (cluster => (others => '0'), size => (others => '0')) when (file_fsm = off) or (file_fsm = write_chain and nb_frag = 0) else fragment_read;


    -----------------------------------------------------------------
    -- fragment out (to update file's clusters chain) 
    -----------------------------------------------------------------
    write_clusters_chain <= '1' when file_fsm = write_chain else '0';
    Process(current_frag, reserve_clusters_i, clust_in_frag_cnt, file_fsm, frag_ptr, nb_frag, fragment_read, sect_in_clust_cnt)
    begin
        if file_fsm = write_chain and frag_ptr+1 = nb_frag then       -- for the first fragment to write (the last use), we just send used clusters and ignore the unused
            fragment_out.cluster <= current_frag.cluster;
            if clust_in_frag_cnt = 0 then
                fragment_out.size <= (21 downto 1 => '0') & '1'; -- current_frag.size - unsigned(reserve_clusters_i) + 1;
            elsif sect_in_clust_cnt /= 0 then
                fragment_out.size <= clust_in_frag_cnt+1;
            else
                fragment_out.size <= clust_in_frag_cnt;--current_frag.size - unsigned(reserve_clusters_i) + clust_in_frag_cnt;
            end if;
        elsif file_fsm = write_chain then                             -- others fragments
            fragment_out <= fragment_read;
        else                                                           -- when not writing the fragment used in FAT. ALSO used to tell if all the fragment have been sent
            fragment_out.cluster <= (others => '0');
            fragment_out.size    <= (others => '0');
        end if;
    end process;

    -- finaly, we determine when the last fragment (to write the cluster chain) has been sent
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = write_chain and frag_ptr = 0 and op_ack = '1' then
                cluster_chain_done <= true;
            else
                cluster_chain_done <= false;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- here we determined if we need to reserve clusters (writing)
    -----------------------------------------------------------------
    -- request_clusters <= '1' when file_fsm = reserve_clusters and op_ack = '0' else '0';
    request_clusters <= '1' when file_fsm = reserve_clusters and op_ack = '0' and reserve_ack_dly = '0' else '0';

    -- Update the request cluster flag
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                no_clusters_reserved <= True;
            -- elsif file_fsm = reserve_clusters and op_ack = '1' then
            elsif file_fsm = reserve_clusters and reserve_ack_dly = '1' then
                no_clusters_reserved <= False;
            elsif sector_ready = '0' and not(file_fsm = request_write) then
                no_clusters_reserved <= True;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- Sector management 
    -----------------------------------------------------------------
    -- Parse the fragment value into the cluster and the nb of consecutive after this cluster
    start_cluster     <= current_frag.cluster;
    current_frag_size <= current_frag.size;
    
    -- Save the previous fragment size for the case where the new free clusters fragment can be concatenated with the previous fragment used,
    -- so we the cluster in fragment counter doesn't reset completely (=> it doesn't rewrite data in former free clusters)
    Process(clk)
    begin
        if rising_edge(clk) then
            -- if file_fsm = reserve_clusters and op_ack = '0' then
            if file_fsm = reserve_clusters and reserve_ack_dly = '0' then
                prev_frag_size <= current_frag_size;
            end if;
        end if;
    end process;


    sector_num    <= std_logic_vector(sector_num_i);

    request_op <= read_sector_i or write_sector_i;
    
    read_sector_i  <= '1' when sector_ready = '1' and file_fsm = request_read  else '0';
    write_sector_i <= '1' when sector_ready = '1' and file_fsm = request_write else '0';

    -- Request a read operation for the file information sector where we are about to close the file
    update_file_info <= '1' when file_fsm = file_info and op_ack = '0' else '0';

    -- update sector to operate
    -- It counts sector in cluster and then cluster in fragment.
    -- It caculates the sector number from the cluster number only for the first fragment of the new fragment.
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                sector_num_i      <= (others => '0');
                sector_ready      <= '0';
                clust_in_frag_cnt <= (others => '0');
                sect_in_clust_cnt <= (others => '0');
            -- elsif file_fsm = reserve_clusters and op_ack = '1' and concatenate_frag and nb_frag > 0 then
            elsif file_fsm = reserve_clusters and reserve_ack_dly = '1' and concatenate_frag and nb_frag > 0 then
                sector_ready      <= '1';
                clust_in_frag_cnt <= prev_frag_size;
            elsif sector_ready = '0' and block_num_rdy = '1' then
                sector_num_i <= block_num;
                sector_ready <= '1';

            -- Reading
            elsif read_sector_i = '1' and op_ack = '1' then
                -- sector_num_i <= sector_num_i + 1;
                sector_num_i <= sector_num_i + NB_MULTIPLE_OP;
                -- if (sect_in_clust_cnt >= unsigned(sectors_per_cluster)-1) or (NB_MULTIPLE_OP = unsigned(sectors_per_cluster)) then
                if (sect_in_clust_cnt >= unsigned(sectors_per_cluster)-1) or (sect_in_clust_cnt >= unsigned(sectors_per_cluster)-NB_MULTIPLE_OP) then
                    sect_in_clust_cnt <= (others => '0');
                    if clust_in_frag_cnt = current_frag_size then
                        sector_ready      <= '0';
                        clust_in_frag_cnt <= (others => '0');
                    else
                        clust_in_frag_cnt <= clust_in_frag_cnt + 1;
                    end if;
                else
                    -- sect_in_clust_cnt <= sect_in_clust_cnt + 1;
                    sect_in_clust_cnt <= sect_in_clust_cnt + NB_MULTIPLE_OP;
                end if;

            -- Writing (almost same code as reading except for the condition the 1st condition)
            elsif write_sector_i = '1' and op_ack = '1' then
                -- sector_num_i <= sector_num_i + 1;
                sector_num_i <= sector_num_i + NB_MULTIPLE_OP;
                -- if (sect_in_clust_cnt >= unsigned(sectors_per_cluster)-1) or (NB_MULTIPLE_OP = unsigned(sectors_per_cluster)) then
                if (sect_in_clust_cnt >= unsigned(sectors_per_cluster)-1) or (sect_in_clust_cnt >= unsigned(sectors_per_cluster)-NB_MULTIPLE_OP) then
                    sect_in_clust_cnt <= (others => '0');
                    if clust_in_frag_cnt = current_frag_size-1 then
                        sector_ready      <= '0';
                        clust_in_frag_cnt <= (others => '0');
                    else
                        clust_in_frag_cnt <= clust_in_frag_cnt + 1;
                    end if;
                else
                    -- sect_in_clust_cnt <= sect_in_clust_cnt + 1;
                    sect_in_clust_cnt <= sect_in_clust_cnt + NB_MULTIPLE_OP;
                end if;

            -- Update file information
            elsif file_fsm = file_info then
                sector_num_i <= unsigned(info_sector_num_i);
            end if;
        end if;
    end process;

    -- this process generate an impulse to increase the frag_ptr by 1 when the module need a new fragment
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                waiting_new_sector <= '0';
                next_frag          <= '0';
                load_first_frag    <= '0';
            elsif file_fsm = get_size or file_fsm = list_clusters then
                load_first_frag    <= '0';
            elsif sector_ready = '0' and waiting_new_sector = '0' then
                waiting_new_sector <= '1';
                if load_first_frag = '1' then -- and frag_ptr < nb_frag-1
                    next_frag      <= '1';
                else
                    load_first_frag <= '1';
                end if;
            elsif block_num_rdy = '1' then
                waiting_new_sector <= '0';
                next_frag          <= '0';
            else
                next_frag <= '0';
            end if;
        end if;
    end process;

    -- Delay the start of the calcul of the first sector of the new fragment (we basically wait to read it from RAM)
    calc_sector <= wait_new_fragment(2);
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                wait_new_fragment <= "000";
            elsif sector_ready = '0' and waiting_new_sector = '0' and (file_fsm = idle_read or file_fsm = request_read or file_fsm = read_data) and last_fragment_dly = '0' then-- not(file_fsm = off or file_fsm = get_size or file_fsm = list_clusters) and last_fragment_dly = '0' then
                wait_new_fragment <= "001";
            -- elsif file_fsm = reserve_clusters and op_ack = '1' and (nb_frag = 0 or not(concatenate_frag)) then
            elsif file_fsm = reserve_clusters and reserve_ack_dly = '1' and (nb_frag = 0 or not(concatenate_frag)) then
                wait_new_fragment <= "001";
            else
                wait_new_fragment <= wait_new_fragment(1 downto 0) & '0';
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    --       Block num / Sector depending on cluster number
    -----------------------------------------------------------------
    -- here we calculate to sector with a given cluster
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                calc_sector_bsy <= '0';
                rem_shifts      <= (others => '0');
                buffer_to_shift <= (others => '0');
                block_num       <= (others => '0');
                block_num_rdy   <= '0';
            elsif calc_sector_bsy = '0' then
                if calc_sector = '1' then
                    rem_shifts      <= unsigned(sectors_per_cluster(7 downto 1));
                    buffer_to_shift <= (31 downto 28 => '0') & (unsigned(start_cluster) - 2);
                    calc_sector_bsy <= '1';
                    block_num_rdy   <= '0';
                else
                    block_num_rdy   <= '0';
                end if;
            else
                if rem_shifts = 0 then
                    calc_sector_bsy <= '0';
                    block_num_rdy   <= '1';
                    block_num       <= unsigned(data_area_offset) + buffer_to_shift;
                else
                    rem_shifts      <= '0' & rem_shifts(6 downto 1);
                    buffer_to_shift <= buffer_to_shift(30 downto 0) & '0';
                end if;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- data management
    -----------------------------------------------------------------
    -- Module interface and FIFO interface routing
    fifo_data_in <= data_in;
    data_out     <= fifo_data_out;

    fifo_dread <= enable_in when read_write_i = '0' else
                  fifo_read when read_write_i = '1' else
                  '0';

    fifo_dwrite <= fifo_write when read_write_i = '0' and not fully_read    else
                   enable_in  when read_write_i = '1' and fifo_full_n = '1' and size_i < (30 downto 0 => '1') and enable_out_i = '1' else
                   '0';

    -- Bytes counter for the file
    -- Read  : count the bytes read from the SDcard (copied into the fifo)
    -- Write : count the bytes written by the user into the fifo, which will be written in due time on the SDcard
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = off then
                total_bytes_cnt <= 0;
            elsif (read_write_i = '0' and fifo_dwrite = '1')                            -- read
               or (read_write_i = '1' and enable_in = '1' and enable_out_i = '1') then   -- write
                total_bytes_cnt <= total_bytes_cnt + (1 * WORD_SIZE/8);
            end if;
        end if;
    end process;

    -- Availability of data:
    --  Reading: tell to the user if there is data available to read in the "file"
    --  Writing: tell to the user if data can be written into the file manager's buffer, which will be saved on the SDcard in due time
    enable_out   <= enable_out_i;
    enable_out_i <= '0'          when opened_i = '0' or request_close = '1' or close_file = '1' else    -- closed or requested to close
                    fifo_empty_n when read_write_i = '0'                                        else    -- read,  opened and not requested to close
                    fifo_full_n  when size_i < '0' & (29 downto 0 => '1')                       else    -- write, opened and not requested to close
                    '0';                                                    

    fifo_empty <= not fifo_empty_n;
    fifo_full  <= not fifo_full_n;

    fully_read <= total_bytes_cnt = size_i or (frag_ptr = MAX_NB_FRAG-1 and next_frag = '1');
    -- can_read   <= fifo_elemts <= FIFO_DEPTH - BYTES_PER_SECTOR/(2**WORD_SIZE_PW2) and not fully_read;
    can_read   <= fifo_elemts <= FIFO_DEPTH - NB_MULTIPLE_OP*BYTES_PER_SECTOR and not fully_read; -- this was missing (Should work)

    -- For now, the limitation of a file is 2GB due to FAT limitations.
    -- TODO: - increase files' size to the remaining size of the SDcard by creating multiple file
    -- can_write <= fifo_elemts >= BYTES_PER_SECTOR/(2**WORD_SIZE_PW2) and size_i < 2**31-1;
    -- can_write <= fifo_elemts >= BYTES_PER_SECTOR/(2**WORD_SIZE_PW2) or (request_close = '1' and fifo_empty_n = '1');
    can_write <= (fifo_elemts >= NB_MULTIPLE_OP*BYTES_PER_SECTOR or fifo_full_n = '0') or (request_close = '1' and fifo_empty_n = '1');

    -- generate the reset for FIFO
    fifo_reset <= '1' when (file_fsm = off) or format = '1' else '0';

    -- FIFO component
    data_fifo : FIFO_Generic
    Generic map(DATA_SIZE  => WORD_SIZE,
                FIFO_DEPTH => FIFO_DEPTH)
    Port map(clk      => clk,
             reset    => fifo_reset,

             data_in  => fifo_data_in,
             dwrite   => fifo_dwrite,
             full_n   => fifo_full_n,

             elemts   => fifo_elemts,
             data_out => fifo_data_out,
             dread    => fifo_dread,
             empty_n  => fifo_empty_n);

    -----------------------------------------------------------------
    -- Debug part.
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if file_fsm = list_clusters and nb_frag = MAX_NB_FRAG and last_fragment_dly = '0' then
                file_debug <= CANNOT_LOAD_ALL_FRAGMENT;
                file_error <= '1';
            else
                file_error <= '0';
            end if;
        end if;
    end process;



    -----------------------------------------------------------------
    --
    --                   Debug
    --
    -----------------------------------------------------------------
    Process(debug_addr, file_state,
            read_write_i, size_i, total_bytes_cnt,
            info_sector_num_i, info_addr_offset_i, starting_cluster_i,
            read_sector_i, write_sector_i, request_close,
            fifo_full_n, fifo_empty_n,
            sector_num_i)
    begin
        case debug_addr is
            when x"D000" => debug_data <=      (31 downto  1 => '0') & read_write_i;
            when x"D001" => debug_data <=             std_logic_vector(size_i);
            when x"D002" => debug_data <= std_logic_vector(to_unsigned(total_bytes_cnt, 32));
            when x"D003" => debug_data <=                              info_sector_num_i;
            when x"D004" => debug_data <=      (31 downto  4 => '0') & info_addr_offset_i;
            when x"D005" => debug_data <=      (31 downto 28 => '0') & starting_cluster_i;
            when x"D006" => debug_data <=      (31 downto  1 => '0') & read_sector_i;
            when x"D007" => debug_data <=      (31 downto  1 => '0') & write_sector_i;
            when x"D008" => debug_data <=      (31 downto  1 => '0') & request_close;
            when x"D009" => debug_data <=      (31 downto  1 => '0') & fifo_full_n;
            when x"D00A" => debug_data <=      (31 downto  1 => '0') & fifo_empty_n;
            when x"D00B" => debug_data <=             std_logic_vector(sector_num_i);


            when x"FF0F" => debug_data <= file_state;

            when x"FFFE" => debug_data <= x"FF89A002";
            when others  => debug_data <= (others => '0');
        end case;
    end process;

    -- file_state <= x"00AA0001" when read_fsm  = off and file_fsm  = off else
    --               write_state when read_fsm  = off and write_fsm /= off else
    --               read_state  when read_fsm /= off and write_fsm  = off else
    --               x"12345670";

    -- Process(read_fsm)
    -- begin
    --     case read_fsm is
    --         when off           => read_state <= x"00000001";
    --         when get_size      => read_state <= x"00000101";
    --         when list_clusters => read_state <= x"00000201";
    --         when idle_read     => read_state <= x"00000301";
    --         when request_read  => read_state <= x"00000401";
    --         when read_data     => read_state <= x"00000402";
    --         when closed        => read_state <= x"00000501";
    --     end case;
    -- end process;


    -- Process(write_fsm)
    -- begin
    --     case write_fsm is
    --         when off              => write_state <= x"00FF0001";
    --         when get_directory    => write_state <= x"00FF0101";
    --         when idle_write       => write_state <= x"00FF0201";
    --         when reserve_clusters => write_state <= x"00FF0301";
    --         when request_write    => write_state <= x"00FF0401";
    --         when write_data       => write_state <= x"00FF0402";
    --         when write_chain      => write_state <= x"00FF0501";
    --         when file_info        => write_state <= x"00FF0502";
    --     end case;
    -- end process;

end Behavioral;