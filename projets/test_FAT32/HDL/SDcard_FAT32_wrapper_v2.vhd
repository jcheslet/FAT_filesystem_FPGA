library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.data_bus_pkg.all;


entity Sdcard_FAT32_wrapper_v2 is                                                                  
    Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
           reset        : in    STD_LOGIC;                                          -- reset, active high

           -- debug/instructions
           tx           : out   STD_LOGIC;
           rx           : in    STD_LOGIC;

           sw           : in    STD_LOGIC_VECTOR (15 downto 0);
           led          : out   STD_LOGIC_VECTOR (15 downto 0);
           
           SD_DAT       : inout STD_LOGIC_VECTOR ( 3 downto 0);                     -- DATA line for SDcard access
           SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
           SD_WP        : in    STD_LOGIC := '0';                                   -- Write Protect (Not used)
           SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
           SD_CMD       : inout STD_LOGIC);                                         -- Command line
end Sdcard_FAT32_wrapper_v2;

architecture Behavioral of Sdcard_FAT32_wrapper_v2 is

    -------------------------------------------------------------------------
    -- SDcard's component
    -------------------------------------------------------------------------
    component Sdcard_FAT32 is
        Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
                VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
                HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
                PARAMETER_BUFFERING : boolean := True;                -- if true, the module keeps its own copy of input parameter SD_block
                                                                    --     setting this generic to false may save slice FFs, but SD_block
                                                                    --     input MUST NOT change as long as read_block, write_block or busy are set.
                LITTLE_ENDIAN       : boolean := True;               -- when multiple bytes per word, tells which byte is stored at lower address
                                                                    --     little endian (True) mean least significant byte first

                MAX_FILES           : integer := 8;                  -- nb of files that can be openned at the same time
                WORD_SIZE           : integer := 8;                  -- ??
                FORMAT_CLUSTER_SIZE : integer range 1 to 8 := 7;     -- power of 2 => size of the cluster (Check min/max cluster size, here are 'random' value)
                FIFO_SIZE           : integer := 512;                -- maximum size of internal fifo for file reading
                TREE_STRUCTURE_MAX  : integer :=   3;                -- maximum sub folder from root directory
                FRAG_CLUST          : integer :=  1);                -- initial reserved cluster per file, 0 mean auto calculated value ? 

        Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
               reset        : in    STD_LOGIC;                                          -- reset, active high

               -- User interface
               filename     : in    STD_LOGIC_VECTOR (87 downto 0);                     -- file name : 8 + '.' + 3
               date_time    : in    STD_LOGIC_VECTOR (39 downto 0) := (others => '0');  -- Creation time, can be ignored
               file_ID      : in    STD_LOGIC_VECTOR(MAX_FILES-1 downto 0);             -- ID (or integer? or...?) operation done on file_ID
               open_file    : in    STD_LOGIC;                                          -- try to open the file_name on the stream ID
               file_opened  : out   STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);            -- high if the file has been openned
               file_size    : out   STD_LOGIC_VECTOR(31 downto 0);                      -- return the file size (in bytes) if the file is open for reading
               reserv_clust : in    STD_LOGIC_VECTOR (21 downto 0);                     -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case
               read_write   : in    STD_LOGIC;                                          -- read = '0' ; write = '1'
               close_file   : in    STD_LOGIC;                                          -- close the file which is openned at ID
               open_dir     : in    STD_LOGIC;                                          -- go into the directory specified in file_name
               close_dir    : in    STD_LOGIC;                                          -- go back to the parent directory (same as cd ../)
                --    erase_file   : in    STD_LOGIC;
               format       : in    STD_LOGIC;                                          -- format the card
               eject_card   : in    STD_LOGIC;                                          -- eject the card : close remaining open files and write/copy FATs, then the card can be removed
               busy         : out   STD_LOGIC;                                          -- the file system is busy

               fat_info     : out   STD_LOGIC_VECTOR ( 7 downto 0);                     -- info about the file system (to develop...)
               fat_new_info : out   STD_LOGIC;                                          -- new info about the file system is available
               debug        : out   STD_LOGIC_VECTOR ( 7 downto 0);                     -- debug/error data
               fat_error    : out   STD_LOGIC;                                          -- high when an error on the FAT occurs

               --    data_in      : in    data_bus(0 to MAX_FILES-1)(WORD_SIZE-1 downto 0);
               --    data_out     : out   data_bus(0 to MAX_FILES-1)(WORD_SIZE-1 downto 0);
               --    data_in      : in    array (0 to MAX_FILES-1) of STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0);
               --    data_out     : out   array (0 to MAX_FILES-1) of STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0);
               data_in      : in    STD_LOGIC_VECTOR (MAX_FILES*WORD_SIZE-1 downto 0);
               data_out     : out   STD_LOGIC_VECTOR (MAX_FILES*WORD_SIZE-1 downto 0);
               enable_in    : in    STD_LOGIC;                                          -- tell the module to write/read data_in/next data
               enable_out   : out   STD_LOGIC;                                          -- '1' when data_in/data_out can be write/read or
               
               -- Debug
               debug_addr   : in    STD_LOGIC_VECTOR (15 downto 0);                     -- address of the register to look out
               debug_data   : out   STD_LOGIC_VECTOR (31 downto 0);                     -- the data to look out
               
               SD_DAT       : inout STD_LOGIC_VECTOR ( 3 downto 0);                      -- DATA line for SDcard access
               SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
               SD_WP        : in    STD_LOGIC := '0';                                   -- Write Protect (Not used)
               SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
               SD_CMD       : inout STD_LOGIC);                                         -- Command line
    end component;

    -------------------------------------------------------------------------
    -- SDcard interface's signals
    -------------------------------------------------------------------------
    constant CLK_FREQ_HZ         : integer := 100000000;
    constant VIVADO_SYNTH        : boolean := True;
    constant HIGH_SPEED_IF       : boolean := False;
    constant WORD_SIZE_PW2       : integer range 0 to 3 := 0;
    constant BUFFER_NUM_PW2      : integer range 1 to 8 := 2;
    constant PARAMETER_BUFFERING : boolean := True;

    constant LITTLE_ENDIAN       : boolean := True;

    constant FORMAT_CLUSTER_SIZE : integer range 1 to 8 := 7;
    constant MAX_FILES           : integer :=   1;
    constant WORD_SIZE           : integer :=   8;
    constant FIFO_SIZE           : integer := 512;
    constant TREE_STRUCTURE_MAX  : integer :=   3;
    constant FRAG_CLUST          : integer :=   1;


    signal filename      : std_logic_vector(87 downto 0);
    signal date_time     : std_logic_vector(39 downto 0) := (others => '0');
    signal file_id       : std_logic_vector(log2(MAX_FILES)-1 downto 0);
    signal open_file     : std_logic;
    signal file_opened   : std_logic_vector(MAX_FILES-1 downto 0);
    signal file_size     : std_logic_vector(31 downto 0);
    signal reserv_clust  : std_logic_vector(21 downto 0);
    signal read_write    : std_logic;
    signal close_file    : std_logic;
    signal open_dir      : std_logic;
    signal close_dir     : std_logic;
    signal format        : std_logic;
    signal eject_card    : std_logic;
    signal busy          : std_logic;

    signal fat_info      : std_logic_vector( 7 downto 0);
    signal fat_new_info  : std_logic;
    signal debug         : std_logic_vector( 7 downto 0);
    signal fat_error     : std_logic;

    signal data_in      : std_logic_vector(MAX_FILES*WORD_SIZE-1 downto 0) := (others => '0');
    signal data_out     : std_logic_vector(MAX_FILES*WORD_SIZE-1 downto 0) := (others => '0');
    signal enable_in    : std_logic := '0';
    signal enable_out   : std_logic := '0';


    -------------------------------------------------------------------------
    --
    -- Debug
    --
    -------------------------------------------------------------------------
    component FIFO_Generic is
        Generic (DATA_SIZE : integer;
                 FIFO_DEPTH : integer := 1024);
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
    
    component FIFO is
        Generic ( WORD_SIZE : integer := 8;
                  FIFO_SIZE : integer := 64 );
        Port ( clk       : in  STD_LOGIC;
               reset     : in  STD_LOGIC; 

               data_in   : in  STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0);  -- new data input
               new_data  : in  STD_LOGIC;                                -- Write new data when high, if fifo isn't full

               data_out  : out STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0);
               data_read : in  STD_LOGIC;                                -- data read, tells the module to send next data

               empty     : out STD_LOGIC;                                -- tell if there is data in the fifo
               full      : out STD_LOGIC);                               -- tell when fifo is (~almost) full
    end component;

    component registers is
        Generic ( WORD_SIZE    : integer := 32;
                  ADDR_SIZE    : integer := 16;
                  NB_REGISTERS : integer := 16;
                  ADDR_OFFSET  : unsigned(15 downto 0) :=  (others => '0'));
        Port ( clk   : in  STD_LOGIC;
               reset : in  STD_LOGIC; 
    
               din   : in  STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0);
               addr  : in  STD_LOGIC_VECTOR (ADDR_SIZE-1 downto 0);
               wen   : in  STD_LOGIC;
    
               dout  : out STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0));
    end component;

    -------------------------------------------------------------------------
    -- Commands & constants
    -------------------------------------------------------------------------
    constant cmd_NOP          : std_logic_vector(7 downto 0) := "00000000";  -- args: 0.                  Returns: ack.            Description: Do nothing.
    constant cmd_read_addr    : std_logic_vector(7 downto 0) := "00000010";  -- args: 1 (addr).           Returns: ack, data.      Description: Read a register and return its content
    constant cmd_write_addr   : std_logic_vector(7 downto 0) := "00000011";  -- args: 2 (addr, data).     Returns: ack.            Description: Write data in register located at addr
    constant cmd_read_stream  : std_logic_vector(7 downto 0) := "00000100";  -- args: 2 (ff, size).       Returns: ack, size*data. Description: Read size data of stream ff.
    constant cmd_write_stream : std_logic_vector(7 downto 0) := "00000101";  -- args: 3 (ff, size, data). Returns: 2 ack.          Description: Write size data in stream ff.
    constant cmd_RW_SPI       : std_logic_vector(7 downto 0) := "00000111";  -- args: 2 (size, data).     Returns ack, size*data.  Description: Write size data with SPI and returns SPI's data.
    constant cmd_ACK          : std_logic_vector(7 downto 0) := "10100000";  -- ACK  : "1010" + "cmd_identifier"
    constant cmd_NACK         : std_logic_vector(7 downto 0) := "11110000";  -- NACK : "1111" + "cmd_identifier"

    constant BAUDRATE       : integer := 921600;    -- maximum baudrate to communicate with pc
    constant UART_DATA_SIZE : integer :=      8;
    constant R_ADDR_SIZE    : integer :=     16;    -- Size of the address of registers in bits
    constant R_WORD_SIZE    : integer :=     32;    -- Size of a word in registers in bits
    constant nb_max_stream  : integer :=      8;    -- power of 2
    constant max_data_size  : integer :=     16;    -- power of 2
    constant FIFO_MAX_SIZE  : integer :=   1024;
    
    -------------------------------------------------------------------------
    -- Instruction FSM
    -------------------------------------------------------------------------
    type t_inst_fsm is (idle,           -- waiting a new command
                        identify_cmd,   -- analyse the command
                        get_addr,       -- store the address
                        get_data,       -- store the word(s) to write
                        get_stream,     -- store the stream to "connect"
                        get_size,       -- store the nb of data to read or write in stream mode
                        read_data,      -- read data in a register at the given address
                        read_data_b,    -- buffer state to get the right data at the new address
                        read_data_b2,   -- Another buffer to ensure we have good data when reading FAT32 debugs
                        read_data_b3,   -- Another buffer to ensure we have good data when reading FAT32 debugs
                        write_data,     -- write the given data in a register at the given address
                        read_stream,    -- read nb words from the specified stream
                        write_stream,   -- write nb words from the specified stream
                        rw_spi,         -- unused
                        return_ack,     -- send ACK to the host
                        return_data,    -- send word(s) read to the host
                        return_nack);   -- send NACK to the host

    signal inst_FSM    : t_inst_fsm := idle;
    signal recv_cmd    : std_logic_vector(UART_DATA_SIZE-1 downto 0);       -- store the received command
    signal recv_addr   : std_logic_vector(R_ADDR_SIZE-1 downto 0);          -- store the received addr
    signal recv_word   : std_logic_vector(R_WORD_SIZE-1 downto 0);          -- store the received word
    signal recv_stream : std_logic_vector(nb_max_stream-1 downto 0);        -- store the received stream to select
    signal recv_size_b : std_logic_vector(max_data_size-1 downto 0);        -- buffer that store the nb of words associated with the command
    signal recv_size   : integer range 1 to 2**max_data_size-1 := 1;        -- store the nb of words associated with the command
    
    signal recv_bytes_cnt   : integer range 0 to 2**max_data_size-1;    -- cnt bytes received
    signal stream_bytes_cnt : integer range 0 to 2**max_data_size-1;    -- stream's maximum data size (Not used yet)
    signal word_cnt         : integer range 0 to 2**max_data_size-1;    -- read/write register/stream counter
    signal bytes_in_word_cnt: integer range 0 to R_WORD_SIZE/8-1;       -- cnt bytes in word received (to put word in FIFO_recv)

    signal inst_ack    : std_logic_vector(UART_DATA_SIZE-1 downto 0);       -- ack/nack command

    -------------------------------------------------------------------------
    -- UART signals
    -------------------------------------------------------------------------
    signal UART_recv_dout : std_logic_vector(UART_DATA_SIZE-1 downto 0);    -- UART receive: data
    signal UART_recv_den  : std_logic;                                      -- UART receive: new data avaible

    signal UART_send_din   : std_logic_vector(UART_DATA_SIZE-1 downto 0);   -- UART send: data
    signal UART_send_den   : std_logic;                                     -- UART send: send din
    signal UART_send_bsy   : std_logic;                                     -- UART send: high when UART_send is sending a byte
    signal UART_bytes_sent : integer range 0 to R_WORD_SIZE / UART_DATA_SIZE;          -- Internal counter to send word through UART
    signal UART_send_word  : std_logic_vector(R_WORD_SIZE-UART_DATA_SIZE-1 downto 0);  -- Internal buffer that copy the word to send through UART
    signal UART_send_parse : std_logic;                                                -- Busy signal of UART send words (high while every bytes aren't sent)

    -- FIFO's signals for data from host
    signal FIFO_recv_din   : std_logic_vector(R_WORD_SIZE-1 downto 0);  -- FIFO data input
    signal FIFO_recv_wen   : std_logic;                                 -- FIFO write enable, to add a data in the fifo
    signal FIFO_recv_dout  : std_logic_vector(R_WORD_SIZE-1 downto 0);  -- FIFO output
    signal FIFO_recv_dread : std_logic;                                 -- FIFO acknowledge, to fetch the next datalen
    signal FIFO_recv_empty : std_logic;                                 -- is FIFO empty
    signal FIFO_recv_full  : std_logic;                                 -- is FIFO (almost?) full

    -- FIFO's signals for data to send back
    signal FIFO_send_din   : std_logic_vector(R_WORD_SIZE-1 downto 0);  -- FIFO data input
    signal FIFO_send_wen   : std_logic;                                 -- FIFO write enable, to add a data in the fifo
    signal FIFO_send_dout  : std_logic_vector(R_WORD_SIZE-1 downto 0);  -- FIFO output
    signal FIFO_send_dread : std_logic;                                 -- FIFO acknowledge, to fetch the next datalen
    signal FIFO_send_empty : std_logic;                                 -- is FIFO empty
    signal FIFO_send_full  : std_logic;                                 -- is FIFO (almost?) full

    -------------------------------------------------------------------------
    -- Registers signals
    -------------------------------------------------------------------------
    signal reg_addr : std_logic_vector(R_ADDR_SIZE-1 downto 0);         -- Registers address
    signal reg_wen  : std_logic;                                        -- high when writing to a register
    signal reg_in   : std_logic_vector(R_WORD_SIZE-1 downto 0);         -- Registers input
    signal reg_out  : std_logic_vector(R_WORD_SIZE-1 downto 0);         -- Registers output

    signal led_reg    : std_logic_vector(R_WORD_SIZE-1 downto 0);        -- LED regiters
    signal sw_reg     : std_logic_vector(R_WORD_SIZE-1 downto 0);        -- Switches registers

    signal SD_debug_data : std_logic_vector(R_WORD_SIZE-1 downto 0);        -- Debug data from SDcard FAT32 manager
    signal inst_state    : std_logic_vector(R_WORD_SIZE-1 downto 0);        -- Current instruction's FSM state


    signal SDraw_blocks_num : std_logic_vector(R_WORD_SIZE-1 downto 0);


    -- signal SD_access : std_logic_vector(R_WORD_SIZE-1 downto 0):= x"00000000";            -- Choose the SDcard access : FAT32 (default) or raw

    -- signal SD_FAT32_DAT : std_logic_vector(3 downto 0);
    -- signal SD_FAT32_CD  : std_logic;
    -- signal SD_FAT32_WP  : std_logic;
    -- signal SD_FAT32_CLK : std_logic;
    -- signal SD_FAT32_CMD : std_logic;

    -- signal SDraw_card_reset : std_logic;
    -- signal SDraw_block_num  : std_logic_vector(31 downto 0);
    -- signal SDraw_op_buff    : std_logic_vector(BUFFER_NUM_PW2-1 downto 0);
    -- signal SDraw_read_blk   : std_logic;
    -- signal SDraw_write_blk  : std_logic;
    -- signal SDraw_erase_blk  : std_logic;
    -- signal SDraw_multiple   : std_logic;
    -- signal SDraw_busy       : std_logic;
    -- signal SDraw_err_code   : std_logic_vector(3 downto 0);
    -- signal SDraw_nb_blocks  : std_logic_vector(31 downto 0);

    -- signal SDraw_buffer     : std_logic_vector(31 downto 0);
    -- signal SDraw_address    : std_logic_vector(31 downto 0);
    -- signal SDraw_write      : std_logic;
    -- signal SDraw_data_in    : std_logic_vector(8*(2**WORD_SIZE_PW2)-1 downto 0);
    -- signal SDraw_data_out   : std_logic_vector(8*(2**WORD_SIZE_PW2)-1 downto 0);

    -- signal SDraw_DAT : std_logic_vector(3 downto 0);
    -- signal SDraw_CD  : std_logic;
    -- signal SDraw_WP  : std_logic;
    -- signal SDraw_CLK : std_logic;
    -- signal SDraw_CMD : std_logic;

begin

    -- SD_DAT <= SD_FAT32_DAT when SD_access(0) else SDraw_DAT;
    -- SD_CD  <= SD_FAT32_CD  when SD_access(0) else SDraw_CD;
    -- SD_WP  <= SD_FAT32_WP  when SD_access(0) else SDraw_WP;
    -- SD_CLK <= SD_FAT32_CLK when SD_access(0) else SDraw_CLK;
    -- SD_CMD <= SD_FAT32_CMD when SD_access(0) else SDraw_CMD;

    -----------------------------------------------------------------
    --
    --                   SDcard manager
    --
    -----------------------------------------------------------------
    SD_FAT32 :  Sdcard_FAT32
    generic map (CLK_FREQ_HZ        => CLK_FREQ_HZ,
                 VIVADO_SYNTH       => VIVADO_SYNTH,
                 HIGH_SPEED_IF      => HIGH_SPEED_IF,
                 PARAMETER_BUFFERING=> PARAMETER_BUFFERING,

                 LITTLE_ENDIAN      => LITTLE_ENDIAN,

                 MAX_FILES          => MAX_FILES,
                 WORD_SIZE          => WORD_SIZE,
                 FORMAT_CLUSTER_SIZE=> FORMAT_CLUSTER_SIZE,
                 FIFO_SIZE          => FIFO_SIZE,
                 TREE_STRUCTURE_MAX => TREE_STRUCTURE_MAX,
                 FRAG_CLUST         => FRAG_CLUST)
    Port map ( clk          => clk,
               reset        => reset,

               filename     => filename,
               date_time    => date_time,
               file_id      => file_id,
               open_file    => open_file,
               file_opened  => file_opened,
               file_size    => file_size,
               reserv_clust => reserv_clust,
               read_write   => read_write,
               close_file   => close_file,
               open_dir     => open_dir,
               close_dir    => close_dir,
            --    erase_file   => erase_file,
               format       => format,
               eject_card   => eject_card, 
               busy         => busy,

               fat_info     => fat_info,
               fat_new_info => fat_new_info,
               debug        => debug,
               fat_error    => fat_error,

               data_in      => data_in,
               data_out     => data_out,
               enable_in    => enable_in,
               enable_out   => enable_out,

               debug_addr   => reg_addr,
               debug_data   => SD_debug_data,

               SD_DAT       => SD_DAT,
               SD_CD        => SD_CD,
               SD_WP        => SD_WP,
               SD_CLK       => SD_CLK,
               SD_CMD       => SD_CMD);


    -- SD_raw_controler_debug : entity work.SDcard_raw_access_V2
    -- --SD_raw_simulated : entity work.SDcard_raw_access_simmodel
    --     GENERIC MAP ( CLK_FREQ_HZ           => CLK_FREQ_HZ,
    --                     VIVADO_SYNTH        => VIVADO_SYNTH,
    --                     FILENAME            => "E:\sdf.txt",
    --                     WORD_SIZE_PW2       => WORD_SIZE_PW2,
    --                     BUFFER_NUM_PW2      => BUFFER_NUM_PW2,
    --                     PARAMETER_BUFFERING => False,
    --                     HIGH_SPEED_IF       => HIGH_SPEED_IF,
    --                     LITTLE_ENDIAN       => LITTLE_ENDIAN)
    --     PORT MAP ( clk          => clk,
    --                 reset       => SDraw_card_reset,

    --                 SD_block    => SDraw_block_num,
    --                 TR_buffer   => SDraw_op_buff,
    --                 read_block  => SDraw_read_blk,
    --                 write_block => SDraw_write_blk,
    --                 erase_block => SDraw_erase_blk,
    --                 multiple    => SDraw_multiple,
    --                 busy        => SDraw_busy,
    --                 err_code    => SDraw_err_code,
    --                 blocks      => SDraw_nb_blocks,
                    
    --                 loc_buffer  => SDraw_buffer,
    --                 address     => SDraw_address,
    --                 data_write  => SDraw_write,
    --                 data_in     => SDraw_data_in,
    --                 data_out    => SDraw_data_out,

    --                 SD_DAT     => SDraw_DAT,
    --                 SD_CD      => SDraw_CD,
    --                 SD_WP      => SDraw_WP,
    --                 SD_CLK     => SDraw_CLK,
    --                 SD_CMD     => SDraw_CMD);

   -----------------------------------------------------------------
   --
   --                   Instruction & debug
   --
   -----------------------------------------------------------------
    -----------------------------------------------------------------
    -- Instructions FSM
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                inst_FSM <= idle;
            else
                case inst_FSM is
                    when idle         => if UART_recv_den = '1'                         then inst_FSM <= identify_cmd; end if;
                    when identify_cmd => if recv_cmd = cmd_NOP                          then inst_FSM <= return_ack;
                                      elsif recv_cmd = cmd_read_addr            
                                         or recv_cmd = cmd_write_addr                   then inst_FSM <= get_addr;
                                      elsif recv_cmd = cmd_read_stream          
                                         or recv_cmd = cmd_write_stream                 then inst_FSM <= get_stream;
                                      elsif recv_cmd = cmd_RW_SPI                       then inst_FSM <= get_size;
                                       else                                                  inst_FSM <= return_nack;  end if;
                    -- Get arguments
                    when get_addr     => if recv_bytes_cnt = R_ADDR_SIZE/8              then
                                             if recv_cmd = cmd_read_addr     then inst_FSM <= read_data;
                                             else                                 inst_FSM <= get_data;        end if; end if;
                    when get_data     => if recv_bytes_cnt = R_WORD_SIZE/8 * recv_size  then
                                             if recv_cmd = cmd_RW_SPI        then inst_FSM <= rw_spi;
                                          elsif recv_cmd = cmd_write_addr    then inst_FSM <= write_data;
                                           else                                   inst_FSM <= write_stream;            end if;
                                      elsif FIFO_recv_full = '1'                        then inst_FSM <= return_nack;  end if;
                    when get_stream   => if recv_bytes_cnt = nb_max_stream/8            then inst_FSM <= get_size;     end if;
                    when get_size     => if recv_bytes_cnt = max_data_size/8            then 
                                             if recv_cmd = cmd_read_stream   then inst_FSM <= read_stream;
                                          elsif recv_cmd = cmd_write_stream  then inst_FSM <= write_stream;
                                           else                                   inst_FSM <= rw_spi; end if;end if;
                    -- Do commands
                    when read_data    =>                                                     inst_FSM <= read_data_b;
                    when read_data_b  =>                                                     inst_FSM <= read_data_b2;
                    when read_data_b2 =>                                                     inst_FSM <= read_data_b3;
                    when read_data_b3 =>                                                     inst_FSM <= return_ack;
                    when write_data   =>                                                     inst_FSM <= return_ack;
                    when read_stream  => if word_cnt = recv_size                        then inst_FSM <= return_ack;   
                                      elsif FIFO_send_full = '1'                        then inst_FSM <= return_nack;  end if;
                    when write_stream => if word_cnt = recv_size                        then inst_FSM <= return_ack;   end if;
                    -- Ignored state bc useless for now
                    when rw_spi       =>                                           inst_FSM <=  return_ack;
                    -- Returns
                    when return_ack   => if UART_bytes_sent = 1              then 
                                             if recv_cmd = cmd_read_addr
                                             or recv_cmd = cmd_read_stream 
                                             or recv_cmd = cmd_RW_SPI        then inst_FSM <= return_data;
                                             else                                 inst_FSM <= idle;            end if; end if;
                    when return_data  => if FIFO_send_empty = '1' 
                                        and UART_bytes_sent = 0                         then inst_FSM <= idle;         end if;
                    when return_nack  => if UART_bytes_sent = 1                         then inst_FSM <= idle;         end if;
                end case;
            end if;
        end if;
    end process;

   -----------------------------------------------------------------
   -- Receiving part
   -----------------------------------------------------------------
    -- Bytes received counter
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                recv_bytes_cnt    <= 0;
            elsif inst_FSM = identify_cmd
               or (inst_FSM = get_addr    and recv_bytes_cnt = R_ADDR_SIZE   / 8)
               or (inst_FSM = get_data    and recv_bytes_cnt = R_WORD_SIZE   / 8 * recv_size)
               or (inst_FSM = get_stream  and recv_bytes_cnt = nb_max_stream / 8)
               or (inst_FSM = get_size    and recv_bytes_cnt = max_data_size / 8)     then
                recv_bytes_cnt    <= 0;
            elsif UART_recv_den = '1' then
                recv_bytes_cnt <= recv_bytes_cnt + 1;
            end if;
        end if;
    end process;

    -- Receive and Store commands & arguments
    recv_size   <= to_integer(unsigned(recv_size_b));
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                recv_cmd    <= (others => '0');
                recv_addr   <= (others => '0');
                recv_word   <= (others => '0');
                recv_stream <= (others => '0');
                recv_size_b <= std_logic_vector(to_unsigned(1,max_data_size));
            else
                case inst_FSM is
                    when idle         => if UART_recv_den = '1' then  recv_cmd    <= UART_recv_dout;                                                    end if;
                    when identify_cmd =>                              recv_size_b <= std_logic_vector(to_unsigned(1,max_data_size));  -- Benefit this state to reset data size
                    when get_addr     => if UART_recv_den = '1' then  recv_addr   <= UART_recv_dout & recv_addr(R_ADDR_SIZE-1 downto UART_DATA_SIZE);   end if;
                    when get_data     => if UART_recv_den = '1' then  recv_word   <= UART_recv_dout & recv_word(R_WORD_SIZE-1 downto UART_DATA_SIZE);   end if;
                    when get_stream   => if UART_recv_den = '1' then  recv_stream <= UART_recv_dout;                                                    end if;
                    when get_size     => if UART_recv_den = '1' then  recv_size_b <= UART_recv_dout & recv_size_b(R_ADDR_SIZE-1 downto UART_DATA_SIZE); end if;
                    when others       => null;
                end case;            
            end if;
        end if;
    end process;

    -- Receive FIFO managment : feed FIFO_recv while data received from UART
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                FIFO_recv_wen     <= '0';
                bytes_in_word_cnt <= 0;
                -- FIFO_recv_din     <= (others => '0');
            elsif UART_recv_den = '1' and (inst_FSM = read_stream or inst_FSM = write_stream) then
                if bytes_in_word_cnt = R_WORD_SIZE/8-1 then
                    FIFO_recv_wen     <= '1';   -- FIFO_recv_full is manage in the FSM so we ignore it here
                    bytes_in_word_cnt <= 0;
                    -- FIFO_recv_din     <= recv_word;
                else
                    FIFO_recv_wen     <= '0';
                    bytes_in_word_cnt <= bytes_in_word_cnt + 1;
                end if;
            else
                FIFO_recv_wen     <= '0';
            end if;
        end if;
    end process;

    FIFO_recv_din <= recv_word;
    FIFO_recv : FIFO
    generic map ( WORD_SIZE  => R_WORD_SIZE,
                  FIFO_SIZE => FIFO_MAX_SIZE )
    port    map ( clk       => clk,
                  reset     => reset,

                  data_in   => FIFO_recv_din,
                  new_data  => FIFO_recv_wen,
                  full      => FIFO_recv_full,

                --   elemts   => open,
                  data_out  => FIFO_recv_dout,
                  data_read => FIFO_recv_dread,
                  empty     => FIFO_recv_empty);

    UART_receiver: entity work.UART_RECV_generic
    Generic map (CLK_FREQU => CLK_FREQ_HZ,
                 BAUDRATE  => BAUDRATE,
                 TIME_PREC => 100,
                 DATA_SIZE => UART_DATA_SIZE)
    Port map( clk   => clk,
              reset => reset,
              RX    => rx,
              dout  => UART_recv_dout,
              den   => UART_recv_den);

    -----------------------------------------------------------------
    -- Sending part
    -----------------------------------------------------------------
     -- Here we determine the ack/nack to send back
     inst_ack <= "1010" & recv_cmd(3 downto 0) when inst_FSM = return_ack else -- ACK
                 "1111" & recv_cmd(3 downto 0);                                  -- NACK

    -- Here we manage data to send in UART, either ack/nack or words from FIFO_send
    Process( clk )
    begin
        if clk'event and clk ='1' then
            if reset = '1' then
                UART_send_den   <= '0';
                UART_send_din   <= (others => '0');
                UART_send_word  <= (others => '0');
                UART_bytes_sent <= 0;
                UART_send_parse <= '0';
                FIFO_send_dread <= '0';
            elsif UART_send_parse = '0' then
                if UART_send_bsy = '0' and
                   (inst_FSM = return_ack or inst_FSM = return_nack or (FIFO_send_empty = '0' and inst_FSM = return_data)) then
                    UART_send_parse <= '1';
                    UART_send_den   <= '1';
                    UART_bytes_sent <= 1;
                    if (inst_FSM = return_ack or inst_FSM = return_nack) then
                        UART_send_word  <= (others => '0');
                        UART_send_din   <= inst_ack;
                        FIFO_send_dread <= '0';
                    else
                        UART_send_word  <= FIFO_send_dout(R_WORD_SIZE-1 downto UART_DATA_SIZE);                 -- little endian
                        UART_send_din   <= FIFO_send_dout(UART_DATA_SIZE-1 downto 0);                           -- little endian
                        FIFO_send_dread <= '1';
                    end if;
                else
                    UART_send_parse <= '0';
                    UART_send_den   <= '0';
                    FIFO_send_dread <= '0'; 
                end if;
            else
                FIFO_send_dread  <= '0';
                if (inst_FSM = return_ack or inst_FSM = return_nack) then
                    UART_bytes_sent <= 0;
                    UART_send_parse <= '0';
                    UART_send_den   <= '0';
                elsif UART_bytes_sent = R_WORD_SIZE / UART_DATA_SIZE then
                    UART_bytes_sent <= 0;
                    UART_send_parse <= '0';
                    UART_send_den   <= '0';
                else
                    if UART_send_bsy = '0' and UART_send_den = '0' then
                        UART_send_din   <= UART_send_word(UART_DATA_SIZE-1 downto 0);                                                               -- little endian
                        UART_send_word  <= (UART_DATA_SIZE-1 downto 0 => '0') & UART_send_word(R_WORD_SIZE-UART_DATA_SIZE-1 downto UART_DATA_SIZE); -- little endian
                        UART_send_den   <= '1';
                        UART_bytes_sent <= UART_bytes_sent + 1;
                    else
                        UART_send_den <= '0';
                    end if;
                end if;
            end if;
        end if;
    end process;

    FIFO_send : FIFO
    generic map ( WORD_SIZE  => R_WORD_SIZE,
                  FIFO_SIZE  => FIFO_MAX_SIZE )
    port    map ( clk       => clk,
                  reset     => reset,
                    
                  data_in   => FIFO_send_din,
                  new_data  => FIFO_send_wen,
                  full      => FIFO_send_full,

                --   elemts   => open,
                  data_out  => FIFO_send_dout,
                  data_read => FIFO_send_dread,
                  empty     => FIFO_send_empty);

    UART_send : entity work.UART_SEND_generic 
    generic map (CLK_FREQU => CLK_FREQ_HZ,
                 BAUDRATE  => BAUDRATE,
                 TIME_PREC => 100,
                 DATA_SIZE => UART_DATA_SIZE )
    port map ( clk   => clk,
               reset => reset,
               TX    => tx,
               din   => UART_send_din,
               den   => UART_send_den,
               bsy   => UART_send_bsy );

    -----------------------------------------------------------------
    --
    --                      Registers
    --
    -----------------------------------------------------------------
    -----------------------------------------------------------------
    -- Reading & wrinting in registers
    -----------------------------------------------------------------
    -- Write in registers
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                -- reg_in  <= (others => '0');
                reg_wen <= '0';                
            elsif inst_FSM = write_data then
                reg_in  <= recv_word;
                reg_wen <= '1';
            else
                reg_wen <= '0';
            end if;
        end if;
    end process;

    -- Read registers
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                FIFO_send_wen <= '0';
                -- FIFO_send_din <= (others => '0');
            elsif inst_FSM = read_data_b3 then
                FIFO_send_din <= reg_out;
                FIFO_send_wen <= '1';
            else
                FIFO_send_wen <= '0';
            end if;
        end if;
    end process;

    -- Set registers address
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                reg_addr <= (others => '0');
            elsif inst_FSM = read_data or inst_FSM = write_data then
                reg_addr <= recv_addr;
            end if;
        end if;
    end process;

    -----------------------------------------------------------------
    -- Registers
    -----------------------------------------------------------------
    -- Registers' output
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                reg_out <= (others => '0');
            elsif reg_addr >= x"F000" then      -- Debug data from SDcard_FAT32 module
                reg_out <= SD_debug_data;
            else
                case reg_addr is
                    when x"0000" => reg_out <= led_reg;         -- LED states
                    when x"0001" => reg_out <= sw_reg;          -- Switches states
                    when x"0002" => reg_out <= inst_state;      -- Intruction FSM state
                    when x"0003" => reg_out <= led_reg;
                    when x"0004" => reg_out <= led_reg;
                    when x"0005" => reg_out <= led_reg;
                    when x"0006" => reg_out <= led_reg;
                    when x"0007" => reg_out <= led_reg;
                    when x"0008" => reg_out <= led_reg;

                    when x"EFFF" => reg_out <= x"FF550001";
                    when others  => reg_out <= (others => '0');
                end case;
            end if;
        end if;
    end process;


   -- LEDs
    led      <= led_reg(15 downto 0);
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                led_reg <= (others => '0');
            elsif reg_addr = std_logic_vector(to_unsigned(0, R_ADDR_SIZE)) and reg_wen = '1' then
                led_reg <= reg_in;
            end if;
        end if;
    end process;
    
    -- Switches
    Process(clk)
    begin
        if rising_edge(clk) then
            sw_reg <= (15 downto 0 => '0') & sw;
        end if;
    end process;

    -- Instruction FSM transcription
    process(inst_FSM)
    begin
        case inst_FSM is
            when idle            => inst_state <= x"00000001";
            when identify_cmd    => inst_state <= x"00000002";
            when get_addr        => inst_state <= x"80000001";
            when get_data        => inst_state <= x"80000002";
            when get_stream      => inst_state <= x"80000004";
            when get_size        => inst_state <= x"80000008";
            when read_data       => inst_state <= x"C0000001";
            when read_data_b     => inst_state <= x"C0000002";
            when read_data_b2    => inst_state <= x"C0000003";
            when read_data_b3    => inst_state <= x"C0000004";
            when write_data      => inst_state <= x"C0000008";
            when read_stream     => inst_state <= x"CC000003";
            when write_stream    => inst_state <= x"CC00000C";
            when rw_spi          => inst_state <= x"FF000000";
            when return_ack      => inst_state <= x"F0000001";
            when return_data     => inst_state <= x"F0000002";
            when return_nack     => inst_state <= x"F0000004";
        end case;
    end process;


end Behavioral;