library IEEE;
use IEEE.Std_logic_1164.all;
use IEEE.Numeric_Std.all;
use work.data_bus_pkg.all;


LIBRARY std;
USE std.textio.all;

entity tb_SDcard_FAT32_multi_files is
    generic (IMAGE_DIRNAME   : string := "/tmp/FAT_script/2/random_dir/";   -- path of the parsed FAT image
             MAX_FILES       : integer range 1 to 64 := 3;
             -----------------------------------------------------------------------------------------------------------------------------------------
             FILE_0_NAME_IN  : string := "00000251DAT";                         -- name of the file to read or to write for file 0
             FILE_0_NAME_OUT : string := "/tmp/FAT_script/2/file_out_0.dat";    -- data read or written for file 0
             FILE_0_RW       : std_logic := '0';                                -- opening mode
             FILE_0_NB_DATA  : integer := 512*8*2 + 577;                        -- 512 bytes * 8 sectors per cluster * nb_clusters of data + random number of data
             -----------------------------------------------------------------------------------------------------------------------------------------
             FILE_1_NAME_IN  : string := "01000099DAT";  --"00000254DAT";       -- name of the file to read or to write for file 1
             FILE_1_NAME_OUT : string := "/tmp/FAT_script/2/file_out_1.dat";    -- data read or written for file 1
             FILE_1_RW       : std_logic := '1';                                -- opening mode
             FILE_1_NB_DATA  : integer := 512*8*2 + 647;                        -- 512 bytes * 8 sectors per cluster * nb_clusters of data + random number of data
             -----------------------------------------------------------------------------------------------------------------------------------------
             FILE_2_NAME_IN  : string := "01000001DAT";                         -- name of the file to read or to write for file 2
             FILE_2_NAME_OUT : string := "/tmp/FAT_script/2/file_out_2.dat";    -- data read or written for file 2
             FILE_2_RW       : std_logic := '1';                                -- opening mode
             FILE_2_NB_DATA  : integer := 512*8*4 + 31;                         -- 512 bytes * 8 sectors per cluster * nb_clusters of data + random number of data
             -----------------------------------------------------------------------------------------------------------------------------------------
             FILE_3_NAME_IN  : string := "01000002DAT";                         -- name of the file to read or to write for file 3
             FILE_3_NAME_OUT : string := "/tmp/FAT_script/2/file_out_3.dat";    -- data read or written for file 3
             FILE_3_RW       : std_logic := '1';                                -- opening mode
             FILE_3_NB_DATA  : integer := 512*8*5 + 1487;                       -- 512 bytes * 8 sectors per cluster * nb_clusters of data + random number of data
             -----------------------------------------------------------------------------------------------------------------------------------------
             FILE_4_NAME_IN  : string := "01000003DAT";                         -- name of the file to read or to write for file 4
             FILE_4_NAME_OUT : string := "/tmp/FAT_script/2/file_out_4.dat";    -- data read or written for file 4
             FILE_4_RW       : std_logic := '1';                                -- opening mode
             FILE_4_NB_DATA  : integer := 512*8*1 + 4885;                       -- 512 bytes * 8 sectors per cluster * nb_clusters of data + random number of data
             -----------------------------------------------------------------------------------------------------------------------------------------
             FILE_TO_ANALYSE : string := "01000001DAT";--"00000251DAT";--"01000001DAT";
             FILE_ANALYSED   : string := "/tmp/FAT_script/2/file_out.dat";
             RW_MODE         : std_logic := '1';
             NB_DATA_TO_OP   : integer := 4800+512*8*4); -- 512 bytes * 8 sectors per cluster * nb_clusters of data
end;

architecture bench of tb_SDcard_FAT32_multi_files is

    function to_slv(str: string) return std_logic_vector is
        variable filename : std_logic_vector(87 downto 0) := x"2020202020202020202020"; 
        variable i        : integer;
    begin 
        i := 0;
        while (i < 11 or i < str'length) loop
            i := i + 1;
            filename := filename(79 downto 0) & std_logic_vector(to_unsigned(character'pos(str(i)),8));
        end loop; 
        return filename; 
    end function;

    component Sdcard_FAT32 is
        Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
                DIRNAME             : string  := "simulation only"; -- for simulation purpose
                VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
                HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
                -- PARAMETER_BUFFERING : boolean := True;                -- if true, the SDcard module keeps its own copy of input parameter SD_block
                                                                      --     setting this generic to false may save slice FFs, but SD_block
                                                                      --     input MUST NOT change as long as read_block, write_block or busy are set.
                LITTLE_ENDIAN       : boolean := True;                -- when multiple bytes per word, tells which byte is stored at lower address
                                                                      --     little endian (True) mean least significant byte first
    
                MAX_FILES           : integer :=    8;               -- nb of files that can be openned at the same time
                WORD_SIZE_PW2       : integer :=    0;               -- ??
                FIFO_DEPTH          : integer := 4096;               -- maximum size of internal fifo for file reading
                FORMAT_CLUSTER_SIZE : integer range 1 to 8 := 7;     -- power of 2 => size of the cluster (default: 7 => 7 * 512 bytes, i.e. 64KB per clusters)
                TREE_STRUCTURE_MAX  : integer :=    3);              -- maximum sub folder from root directory

        Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
               reset        : in    STD_LOGIC;                                          -- reset, active high

               -- User interface
               filename     : in    STD_LOGIC_VECTOR (87 downto 0);                     -- file name : 8 + '.' + 3
               date_time    : in    STD_LOGIC_VECTOR (39 downto 0) := (others => '0');  -- Creation time, can be ignored
               file_id      : in    STD_LOGIC_VECTOR (log2(MAX_FILES)-1 downto 0);      -- ID (or integer? or...?) operation done on file_ID
               open_file    : in    STD_LOGIC;                                          -- try to open the file_name on the stream ID
               file_opened  : out   STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);            -- high if the file has been openned
               file_size    : out   STD_LOGIC_VECTOR (31 downto 0);                     -- return the file size (in bytes) if the file is open for reading
               reserv_clust : in    STD_LOGIC_VECTOR (21 downto 0);                     -- consecutive number of cluster to reserve each time we need to write data on SDcard, 2**22 give 2GB (i.e. max) with cluster of 512 bytes (i.e. min) => "worst" case
               read_write   : in    STD_LOGIC;                                          -- read = '0' ; write = '1'
               close_file   : in    STD_LOGIC;                                          -- close the file which is openned at ID
               open_dir     : in    STD_LOGIC;                                          -- go into the directory specified in file_name if reading, create it if writing
               close_dir    : in    STD_LOGIC;                                          -- go back to the parent directory (same as cd ../)
            --    erase_file   : in    STD_LOGIC;
               format       : in    STD_LOGIC;                                          -- format the card
               eject_card   : in    STD_LOGIC;                                          -- eject the card : close remaining open files and write/copy FATs, then the card can be removed
               busy         : out   STD_LOGIC;                                          -- the file system is busy

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
    end component;


    ----------------------------------------------------------------------------------
    -- UART component
    ----------------------------------------------------------------------------------
    component UART_RECV_generic is
        Generic (CLK_FREQU : integer := 100000000;
                 BAUDRATE  : integer :=  33333333;
                 TIME_PREC : integer :=    100000;
                 DATA_SIZE : integer := 8);
        Port ( clk   : in STD_LOGIC;
               reset : in STD_LOGIC;
               RX    : in STD_LOGIC;
               dout  : out STD_LOGIC_VECTOR (DATA_SIZE - 1 downto 0);
               den   : out STD_LOGIC);
    end component;

    component UART_SEND_generic is
        Generic (CLK_FREQU : integer := 100000000;
             BAUDRATE  : integer :=  33333333;
             TIME_PREC : integer :=    100000;
             DATA_SIZE : integer := 8);
    Port ( clk   : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           TX    : out STD_LOGIC;
           din   : in  STD_LOGIC_VECTOR (DATA_SIZE - 1 downto 0);
           den   : in  STD_LOGIC;
           bsy   : out STD_LOGIC);
    end component;

    constant CLK_FREQ_HZ         : integer := 100000000;
    constant VIVADO_SYNTH        : boolean := True;
    constant HIGH_SPEED_IF       : boolean := False;
    constant WORD_SIZE_PW2       : integer range 0 to 3 := 0;
    constant BUFFER_NUM_PW2      : integer range 1 to 8 := 2;
    constant PARAMETER_BUFFERING : boolean := True;

    constant LITTLE_ENDIAN       : boolean := True;

    constant FORMAT_CLUSTER_SIZE : integer range 1 to 8 := 7;
    constant WORD_SIZE           : integer := 8*(2**WORD_SIZE_PW2);
    constant FIFO_DEPTH          : integer := 4096;
    constant TREE_STRUCTURE_MAX  : integer :=    3;
    constant FRAG_CLUST          : integer :=    1;


    signal filename      : std_logic_vector(87 downto 0);
    signal date_time     : std_logic_vector(39 downto 0) := (others => '0');
    signal file_id       : std_logic_vector(log2(MAX_FILES)-1 downto 0);
    signal file_id_int   : integer range 0 to MAX_FILES-1;
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

    signal debug         : std_logic_vector( 7 downto 0);
    signal fat_error     : std_logic;


    signal data_in      : std_logic_vector(MAX_FILES*WORD_SIZE-1 downto 0) := (others => '0');
    signal data_out     : std_logic_vector(MAX_FILES*WORD_SIZE-1 downto 0) := (others => '0');
    signal enable_in    : std_logic_vector(MAX_FILES-1 downto 0) := (others => '0');
    signal enable_out   : std_logic_vector(MAX_FILES-1 downto 0) := (others => '0');

    signal debug_addr   : std_logic_vector(15 downto 0) := (others => '0');
    signal debug_data   : std_logic_vector(31 downto 0) := (others => '0');

    --------

    signal clk          : std_logic;
    signal reset        : std_logic;

    signal tx           : std_logic;
    signal rx           : std_logic;

    signal SD_DAT       : std_logic_vector ( 3 downto 0);  
    signal SD_CD        : std_logic;
    signal SD_WP        : std_logic := '0';
    signal SD_CLK       : std_logic;
    signal SD_CMD       : std_logic;


    --------

    -- handle file read
    type char_file_t is file of character;
    file file_0 : char_file_t;
    file file_1 : char_file_t;
    file file_2 : char_file_t;
    file file_3 : char_file_t;
    file file_4 : char_file_t;


    -- "random" data
    component random_gen_V1 is
        generic (size  : integer := 168;  -- the cycle of the shift register
                 seed  : integer := 2016; -- the number of cycles used to seed the generator
                 cycle : integer := 168;  -- how much bit shifts to draw a new value ? (may reduce cycle size if submultiple of (2**size - 1)
                 outpt : integer := 8);   -- output size (must be lower than 'size')
        Port ( clk      : in  STD_LOGIC;
               reset    : in  STD_LOGIC;
               update   : in  STD_LOGIC;
               rdy      : out STD_LOGIC;
               rand_val : out STD_LOGIC_VECTOR (outpt - 1 downto 0));
    end component;
    
    constant RDG_SIZE  : integer := 168;
    constant RDG_SEED  : integer := 2021;
    constant RDG_CYCLE : integer := 168;
    constant RDG_OUTPT : integer := WORD_SIZE;
    signal rdg_update   : std_logic;
    signal rdg_rdy      : std_logic;
    signal rdg_rand_val : std_logic_vector(RDG_OUTPT-1 downto 0);

    signal rd_data_0 : unsigned(WORD_SIZE-1 downto 0); -- incremental counter used for debug (maybe unused)
    signal rd_data_1 : unsigned(WORD_SIZE-1 downto 0); -- incremental counter used for debug (maybe unused)
    signal rd_data_2 : unsigned(WORD_SIZE-1 downto 0); -- incremental counter used for debug (maybe unused)
    signal rd_data_3 : unsigned(WORD_SIZE-1 downto 0); -- incremental counter used for debug (maybe unused)
    signal rd_data_4 : unsigned(WORD_SIZE-1 downto 0); -- incremental counter used for debug (maybe unused)
    signal nb_data_rw_0   : integer; -- cnt data read or write
    signal nb_data_rw_1   : integer; -- cnt data read or write
    signal nb_data_rw_2   : integer; -- cnt data read or write
    signal nb_data_rw_3   : integer; -- cnt data read or write
    signal nb_data_rw_4   : integer; -- cnt data read or write
    signal data_to_read_0 : integer; -- get the nb of bytes in a file which is opened in READ mode
    signal data_to_read_1 : integer; -- get the nb of bytes in a file which is opened in READ mode
    signal data_to_read_2 : integer; -- get the nb of bytes in a file which is opened in READ mode
    signal data_to_read_3 : integer; -- get the nb of bytes in a file which is opened in READ mode
    signal data_to_read_4 : integer; -- get the nb of bytes in a file which is opened in READ mode

    constant clock_period : time := 10 ns;

begin

    
    SDcard_FAT :  Sdcard_FAT32
    generic map (CLK_FREQ_HZ        => CLK_FREQ_HZ,
                 DIRNAME            => IMAGE_DIRNAME, -- for simulation purpose
                 VIVADO_SYNTH       => VIVADO_SYNTH,
                 HIGH_SPEED_IF      => HIGH_SPEED_IF,
                --  PARAMETER_BUFFERING=> PARAMETER_BUFFERING,
                 
                 LITTLE_ENDIAN      => LITTLE_ENDIAN,

                 MAX_FILES          => MAX_FILES,
                 FORMAT_CLUSTER_SIZE=> FORMAT_CLUSTER_SIZE,
                 FIFO_DEPTH         => FIFO_DEPTH,
                 TREE_STRUCTURE_MAX => TREE_STRUCTURE_MAX)
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

               debug        => debug,
               fat_error    => fat_error,

               data_in      => data_in,
               data_out     => data_out,
               enable_in    => enable_in,
               enable_out   => enable_out,
               
               debug_addr   => debug_addr,
               debug_data   => debug_data,
               
               SD_DAT       => SD_DAT,
               SD_CD        => SD_CD,
               SD_WP        => SD_WP,
               SD_CLK       => SD_CLK,
               SD_CMD       => SD_CMD);

    file_id <= std_logic_vector(to_unsigned(file_id_int, log2(MAX_FILES)));


    stimulus: process
    begin

        -----------------------------------------------------------------
        --  RESET
        -----------------------------------------------------------------

        reset        <= '1';
        eject_card   <= '0';
        format       <= '0';
        read_write   <= '0';
        open_file    <= '0';
        close_file   <= '0';
        open_dir     <= '0';
        close_dir    <= '0';
        enable_in(0) <= '0';
        file_id_int  <= 0;
        reserv_clust <= std_logic_vector(to_unsigned(10, reserv_clust'length));
        filename     <= (others => '0');
        date_time    <= (others => '0');

        SD_DAT   <= (others => '0');
        SD_CD    <= '0';
        SD_WP    <= '0';
        SD_CLK   <= '0';
        wait for 16 ns;
        reset <= '0';
        wait for 5 ns;
        
        -----------------------------------------------------------------
        --  waiting the end of init
        -----------------------------------------------------------------
        wait until falling_edge(busy);



        wait for 10 us;
        -----------------------------------------------------------------
        --  Create a directory
        -----------------------------------------------------------------
        -- filename <= to_slv("01000001DAT");
        filename <= to_slv("CAMARCHE   ");
        read_write <= '1';
        wait for 2 us;
        open_dir <= '1';
        wait for clock_period;
        open_dir <= '0';
        wait until falling_edge(busy);


        wait for 10 us;


        -----------------------------------------------------------------
        --  Opening a directory
        -----------------------------------------------------------------
        -- filename <= to_slv("01000001DAT");
        filename <= to_slv("CAMARCHE   ");
        read_write <= '0';
        wait for 2 us;
        open_dir <= '1';
        wait for clock_period;
        open_dir <= '0';
        wait until falling_edge(busy);


        wait for 10 us;

        -----------------------------------------------------------------
        --  Opening FILE 1
        -----------------------------------------------------------------
        filename    <= to_slv(FILE_1_NAME_IN);
        read_write  <= FILE_1_RW;
        file_id_int <= 1;
        open_file   <= '1';
        wait for clock_period;
        open_file <= '0';

        wait until falling_edge(busy);
        data_to_read_1 <= to_integer(unsigned(file_size));
        wait for clock_period;

        enable_in(1) <= '1';

        wait until nb_data_rw_1 >= FILE_1_NB_DATA;

        enable_in(1) <= '0';

        wait for 10 us;

        eject_card <= '1';
        wait for clock_period;
        eject_card <= '0';


        wait;


        -----------------------------------------------------------------
        -- Opening FILE 0
        -----------------------------------------------------------------
        filename    <= to_slv(FILE_0_NAME_IN);
        read_write  <= FILE_0_RW;
        file_id_int <= 0;
        wait for 10 us;
        open_file <= '1';
        wait for clock_period;
        open_file <= '0';

        -----------------------------------------------------------------
        --  Reading / writing
        -----------------------------------------------------------------
        wait until falling_edge(busy);
        data_to_read_0 <= to_integer(unsigned(file_size));
        wait for clock_period;
        -- if read_write = '1' then
        --     wait until rdg_rdy = '1';
        -- end if;
        wait for 10 us;
        enable_in(0) <= '1';

        if FILE_0_RW = '1' then
            wait until nb_data_rw_0 >= FILE_0_NB_DATA;
        else
            wait until nb_data_rw_0 >= data_to_read_0;
            -- wait until falling_edge(file_opened(0));
        end if;

        wait until file_opened(0) = '0';

        enable_in(0) <= '0';

        wait for 10 ns;

        -- -----------------------------------------------------------------
        -- --  Opening a directory
        -- -----------------------------------------------------------------
        -- -- filename <= to_slv("01000001DAT");
        -- filename <= to_slv("TEST2001   ");
        -- read_write <= '0';
        -- wait for 2 us;
        -- open_dir <= '1';
        -- wait for clock_period;
        -- open_dir <= '0';
        -- wait until falling_edge(busy);

        -----------------------------------------------------------------
        --  Opening FILE 1
        -----------------------------------------------------------------
        filename    <= to_slv(FILE_1_NAME_IN);
        read_write  <= FILE_1_RW;
        file_id_int <= 1;
        open_file   <= '1';
        wait for clock_period;
        open_file <= '0';

        wait until falling_edge(busy);
        data_to_read_1 <= to_integer(unsigned(file_size));
        wait for clock_period;

        enable_in(1) <= '1';

        -- if FILE_1_RW = '1' then
        --     wait until nb_data_rw_1 >= FILE_1_NB_DATA * 3/4;
        -- else
        --     wait until nb_data_rw_1 >= data_to_read_1 / 3/4;
        --     -- wait until falling_edge(file_opened(1));
        -- end if;


        -- if FILE_0_RW = '1' then
        --     wait until nb_data_rw_0 >= FILE_0_NB_DATA;
        -- else
        --     wait until nb_data_rw_0 >= data_to_read_0;
        --     -- wait until falling_edge(file_opened(0));
        -- end if;



        wait for clock_period * 20;


        -- enable_in(1) <= '0';
        -- if file_opened(1) = '1' then
        --     file_id_int <= 1;
        --     -- close_file  <= '1';
        --     -- wait for clock_period;
        --     -- close_file <= '0';
        --     wait until falling_edge(file_opened(1));
        -- end if;

        -- enable_in(1) <= '0';


        -----------------------------------------------------------------
        --  Opening FILE 2
        -----------------------------------------------------------------
        filename    <= to_slv(FILE_2_NAME_IN);
        read_write  <= FILE_2_RW;
        file_id_int <= 2;
        open_file   <= '1';
        wait for clock_period;
        open_file <= '0';

        wait until falling_edge(busy);
        data_to_read_2 <= to_integer(unsigned(file_size));
        wait for clock_period;


        -- Start writing on both files

        enable_in(1) <= '1';
        enable_in(2) <= '1';
        wait until nb_data_rw_1 >= FILE_1_NB_DATA;


        wait until nb_data_rw_2 >= FILE_2_NB_DATA;
        enable_in(2) <= '0';
        enable_in(1) <= '0';

        wait for 100 ns;

        eject_card <= '1';
        wait for clock_period;
        eject_card <= '0';


        wait until falling_edge(busy);

        wait for 1 us;
        
        
            
        -- SD_CD <= '1';
        -- wait for 100 ns;
        -- SD_CD <= '0';
        -- wait for 40 ns;

        -- reset <= '1';
        -- wait for clock_period;
        -- reset <= '0';
        -- wait for 5 ns;

        -- wait until falling_edge(busy);

        -- -----------------------------------------------------------------
        -- --  Opening a directory
        -- -----------------------------------------------------------------
        -- -- filename <= to_slv("01000001DAT");
        -- filename <= to_slv("TEST2001   ");
        -- read_write <= '0';
        -- wait for 2 us;
        -- open_dir <= '1';
        -- wait for clock_period;
        -- open_dir <= '0';
        -- wait until falling_edge(busy);
        

        -- -----------------------------------------------------------------
        -- --  Opening FILE 1
        -- -----------------------------------------------------------------
        -- filename    <= to_slv(FILE_1_NAME_IN);
        -- read_write  <= FILE_1_RW;
        -- file_id_int <= 1;
        -- open_file   <= '1';
        -- wait for clock_period;
        -- open_file <= '0';

        -- wait until falling_edge(busy);
        -- data_to_read_1 <= to_integer(unsigned(file_size));
        -- wait for clock_period;


        -- -- if FILE_1_RW = '1' then
        -- --     wait until nb_data_rw_1 >= FILE_1_NB_DATA * 3/4;
        -- -- else
        -- --     wait until nb_data_rw_1 >= data_to_read_1 / 3/4;
        -- --     -- wait until falling_edge(file_opened(1));
        -- -- end if;
        

        -- wait for clock_period * 10;

        -- enable_in(1) <= '1';
        -- wait until nb_data_rw_1 >= FILE_1_NB_DATA;
        -- enable_in(1) <= '0';

        -- wait for 50 ns;

        -- eject_card <= '1';
        -- wait for clock_period;
        -- eject_card <= '0';
        
        wait for 50 ns;
        -- file_id_int <= 1;
        -- close_file  <= '1';
        -- wait for clock_period;
        -- close_file <= '0';
        -- wait for 1 us;

        -- wait until falling_edge(file_opened(1));

        -- wait for 20 ns;
        
        -- file_id_int <= 2;
        -- close_file  <= '1';
        -- wait for clock_period;
        -- close_file <= '0';
        -- wait for 1 us;

        -- wait until file_opened(2) = '0';

        -- wait for 10 us;


        -- eject_card <= '1';
        -- wait for clock_period;
        -- eject_card <= '0';


        -- wait for 10 us;

        -- close_dir <= '1';
        -- wait for clock_period;
        -- close_dir <= '0';

        wait;
    end process;


    -- Process
    -- begin
    --     wait until rising_edge(file_opened(0));
    --     wait 5 * clock_period;
    --     enable_in(0) <= '1';

    --     if FILE_0_RW = '1' then
    --         while nb_data_rw_0 <= FILE_0_NB_DATA loop


    --         end loop;


    --         wait until nb_data_rw_0 >= FILE_0_NB_DATA * 3/4;
    --     else
    --         wait until nb_data_rw_0 >= data_to_read_0 / 3/4;
    --         -- wait until falling_edge(file_opened(1));
    --     end if;
        

    -- end process;

    -----------------------------------------------------------------
    --  put data read or writted into a file for futur check
    -----------------------------------------------------------------
    ------------------------------------------------------------------ FILE 0 ------------------------------------------------------------------
    log_file_0 : process
        variable char_v : character;                  -- the char actually read/written to the file
        variable byte_v : natural range 0 to 255;     -- integer version of the previous char
    begin
    wait until rising_edge(open_file) and file_id_int = 0;
        file_open(file_0, FILE_0_NAME_OUT, write_mode);
        while not(close_file = '1' and file_id_int = 0) loop
            if enable_in(0) = '1' and enable_out(0) = '1' then
                if FILE_0_RW = '1' then
                    byte_v := natural(to_integer(unsigned(data_in(7 downto 0))));
                else
                    byte_v := natural(to_integer(unsigned(data_out(7 downto 0))));
                end if;
                char_v := character'val(byte_v);
                write(file_0, char_v);
            end if;
            wait for clock_period;
        end loop;
        file_close(file_0);
        wait;
    end process;

    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                rd_data_0         <= (others => '0');
                nb_data_rw_0 <= 0;
            elsif enable_in(0) = '1' and enable_out(0) = '1' then
                rd_data_0 <= rd_data_0 + 1;
                nb_data_rw_0 <= nb_data_rw_0 + 1;
            end if;
        end if;
    end process;
    ------------------------------------------------------------------ FILE 1 ------------------------------------------------------------------
    log_file_1 : if MAX_FILES > 1 generate
        process
            variable char_v : character;                  -- the char actually read/written to the file
            variable byte_v : natural range 0 to 255;     -- integer version of the previous char
        begin
        wait until rising_edge(open_file) and file_id_int = 1;
            file_open(file_1, FILE_1_NAME_OUT, write_mode);
            while not(close_file = '1' and file_id_int = 1) loop
                if enable_in(1) = '1' and enable_out(1) = '1' then
                    if FILE_1_RW = '1' then
                        byte_v := natural(to_integer(unsigned(data_in(15 downto 8))));
                    else
                        byte_v := natural(to_integer(unsigned(data_out(15 downto 8))));
                    end if;
                    char_v := character'val(byte_v);
                    write(file_1, char_v);
                end if;
                wait for clock_period;
            end loop;
            file_close(file_1);
            wait;
        end process;

        Process(clk)
        begin
            if rising_edge(clk) then
                if reset = '1' then
                    rd_data_1         <= (others => '0');
                    nb_data_rw_1 <= 0;
                elsif enable_in(1) = '1' and enable_out(1) = '1' then
                    rd_data_1 <= rd_data_1 + 1;
                    nb_data_rw_1 <= nb_data_rw_1 + 1;
                end if;
            end if;
        end process;
    end generate;
    ------------------------------------------------------------------ FILE 2 ------------------------------------------------------------------
    log_file_2 : if MAX_FILES > 2 generate
        process
            variable char_v : character;                  -- the char actually read/written to the file
            variable byte_v : natural range 0 to 255;     -- integer version of the previous char
        begin
        wait until rising_edge(open_file) and file_id_int = 2;
            file_open(file_2, FILE_2_NAME_OUT, write_mode);
            while not(close_file = '1' and file_id_int = 2) loop
                if enable_in(2) = '1' and enable_out(2) = '1' then
                    if FILE_2_RW = '1' then
                        byte_v := natural(to_integer(unsigned(data_in(23 downto 16))));
                    else
                        byte_v := natural(to_integer(unsigned(data_out(23 downto 16))));
                    end if;
                    char_v := character'val(byte_v);
                    write(file_2, char_v);
                end if;
                wait for clock_period;
            end loop;
            file_close(file_2);
            wait;
        end process;

        Process(clk)
        begin
            if rising_edge(clk) then
                if reset = '1' then
                    rd_data_2         <= (others => '0');
                    nb_data_rw_2 <= 0;
                elsif enable_in(2) = '1' and enable_out(2) = '1' then
                    rd_data_2 <= rd_data_2 + 1;
                    nb_data_rw_2 <= nb_data_rw_2 + 1;
                end if;
            end if;
        end process;
    end generate;
    ------------------------------------------------------------------ FILE 3 ------------------------------------------------------------------
    log_file_3 : if MAX_FILES > 3 generate
        process
            variable char_v : character;                  -- the char actually read/written to the file
            variable byte_v : natural range 0 to 255;     -- integer version of the previous char
        begin
        wait until rising_edge(open_file) and file_id_int = 3;
            file_open(file_3, FILE_3_NAME_OUT, write_mode);
            while not(close_file = '1' and file_id_int = 3) loop
                if enable_in(3) = '1' and enable_out(3) = '1' then
                    if FILE_3_RW = '1' then
                        byte_v := natural(to_integer(unsigned(data_in(31 downto 24))));
                    else
                        byte_v := natural(to_integer(unsigned(data_out(31 downto 24))));
                    end if;
                    char_v := character'val(byte_v);
                    write(file_3, char_v);
                end if;
                wait for clock_period;
            end loop;
            file_close(file_3);
            wait;
        end process;

        Process(clk)
        begin
            if rising_edge(clk) then
                if reset = '1' then
                    rd_data_3         <= (others => '0');
                    nb_data_rw_3 <= 0;
                elsif enable_in(3) = '1' and enable_out(3) = '1' then
                    rd_data_3 <= rd_data_3 + 1;
                    nb_data_rw_3 <= nb_data_rw_3 + 1;
                end if;
            end if;
        end process;
    end generate;
    ------------------------------------------------------------------ FILE 4 ------------------------------------------------------------------
    log_file_4 : if MAX_FILES > 4 generate
        process
            variable char_v : character;                  -- the char actually read/written to the file
            variable byte_v : natural range 0 to 255;     -- integer version of the previous char
        begin
        wait until rising_edge(open_file) and file_id_int = 4;
            file_open(file_4, FILE_4_NAME_OUT, write_mode);
            while not(close_file = '1' and file_id_int = 4) loop
                if enable_in(4) = '1' and enable_out(4) = '1' then
                    if FILE_4_RW = '1' then
                        byte_v := natural(to_integer(unsigned(data_in(39 downto 32))));
                    else
                        byte_v := natural(to_integer(unsigned(data_out(39 downto 32))));
                    end if;
                    char_v := character'val(byte_v);
                    write(file_4, char_v);
                end if;
                wait for clock_period;
            end loop;
            file_close(file_4);
            wait;
        end process;

        Process(clk)
        begin
            if rising_edge(clk) then
                if reset = '1' then
                    rd_data_4         <= (others => '0');
                    nb_data_rw_4 <= 0;
                elsif enable_in(4) = '1' and enable_out(4) = '1' then
                    rd_data_4 <= rd_data_4 + 1;
                    nb_data_rw_4 <= nb_data_rw_4 + 1;
                end if;
            end if;
        end process;
    end generate;


    -- data_in(WORD_SIZE-1 downto 0) <= std_logic_vector(rd_data);
    data_in( 7 downto  0) <= rdg_rand_val and "11111111";
    data_in(15 downto  8) <= rdg_rand_val and "11111110";
    -- data_in(23 downto 16) <= rdg_rand_val & "11111101";
    -- data_in(31 downto 24) <= rdg_rand_val & "11111011";
    -- data_in(39 downto 32) <= rdg_rand_val & "11110111";

    -----------------------------------------------------------------
    --  Data creation
    -----------------------------------------------------------------
    random_gene : random_gen_V1
        generic map (size  => RDG_SIZE,    -- the cycle of the shift register
                     seed  => RDG_SEED,    -- the number of cycles used to seed the generator
                     cycle => RDG_CYCLE,   -- how much bit shifts to draw a new value ? (may reduce cycle size if submultiple of (2**size - 1)
                     outpt => RDG_OUTPT)   -- output size (must be lower than 'size')
        Port map ( clk      => clk,
                   reset    => reset,
                   update   => rdg_update,
                   rdy      => rdg_rdy,
                   rand_val => rdg_rand_val);

    rdg_update <= (enable_in(0) and enable_out(0)) or
                  (enable_in(1) and enable_out(1)) or
                  (enable_in(2) and enable_out(2));


    -----------------------------------------------------------------
    --  Generate clock
    -----------------------------------------------------------------
    clocking: process
    begin
        while true loop
            clk <= '0', '1' after clock_period / 2;
            wait for clock_period;
        end loop;
        wait;
    end process;
end;