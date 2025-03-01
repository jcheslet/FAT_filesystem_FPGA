library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


-- package data_bus_pkg is
--     type data_bus is array (natural range <>) of std_logic_vector;
-- end package;

-- library IEEE;
-- use IEEE.STD_LOGIC_1164.ALL;
-- use IEEE.NUMERIC_STD.ALL;
-- use work.data_bus_pkg.all;


entity Sdcard_FAT32_wrapper_v1 is                                                                  
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
end Sdcard_FAT32_wrapper_v1;

architecture Behavioral of Sdcard_FAT32_wrapper_v1 is

    component Sdcard_FAT32 is
        Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
                VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
                HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
                WORD_SIZE_PW2       : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
                BUFFER_NUM_PW2      : integer range 1 to 8 := 2;      -- codes the number of 512B data buffers for transfers, actual value will be 2^BUFFER_NUM_PW2
                PARAMETER_BUFFERING : boolean := True;                -- if true, the module keeps its own copy of input parameter SD_block
                                                                    --     setting this generic to false may save slice FFs, but SD_block
                                                                    --     input MUST NOT change as long as read_block, write_block or busy are set.
                LITTLE_ENDIAN       : boolean := True;               -- when multiple bytes per word, tells which byte is stored at lower address
                                                                    --     little endian (True) mean least significant byte first

                FULL_WORDS_ONLY     : boolean := True;                -- if file length is not a multiple of WORD_SIZE, skip last incomplete data ?

                MAX_FILES           : integer := 8;                  -- nb of files that can be openned at the same time
                WORD_SIZE           : integer := 8;                  -- ??
                FORMAT_CLUSTER_SIZE : integer range 12 to 18 := 12;  -- power of 2 => size of the cluster (Check min/max cluster size, here are 'random' value)
                FIFO_SIZE           : integer := 512;                -- maximum size of internal fifo for file reading
                TREE_STRUCTURE_MAX  : integer :=   3;                -- maximum sub folder from root directory
                FRAG_CLUST          : integer := 1);                 -- initial reserved cluster per file, 0 mean auto calculated value ? 
                                                                    
        Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
               reset        : in    STD_LOGIC;                                          -- reset, active high
   
               -- debug/instructions
               tx           : out   STD_LOGIC;
               rx           : in    STD_LOGIC;
               sw           : in    STD_LOGIC_VECTOR (15 downto 0);
               led          : out   STD_LOGIC_VECTOR (15 downto 0);
               
               -- User interface
               file_name    : in    STD_LOGIC_VECTOR (87 downto 0);               -- file name : 8 + '.' + 3
               date_time    : in    STD_LOGIC_VECTOR (39 downto 0) := (others => '0');
               open_file    : in    STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);      -- Select a stream
               opened_file  : in    STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);      -- ACK
               rw_file     : in    STD_LOGIC_VECTOR (MAX_FILES-1 downto 0);      -- high to write, low to read
               busy         : in    STD_LOGIC;
               close_file   : in    STD_LOGIC;
               open_dir     : in    STD_LOGIC;
               erase_file   : in    STD_LOGIC;
               
               fat_error    : out   STD_LOGIC_VECTOR (7 downto 0);
               format       : in    STD_LOGIC;
   
   
               --    data_in      : in    data_bus(0 to MAX_FILES-1)(WORD_SIZE-1 downto 0);
               --    data_out     : out   data_bus(0 to MAX_FILES-1)(WORD_SIZE-1 downto 0);
               --    data_in      : in    array (0 to MAX_FILES-1) of STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0);
               --    data_out     : out   array (0 to MAX_FILES-1) of STD_LOGIC_VECTOR (WORD_SIZE-1 downto 0);
               data_in      : in    STD_LOGIC_VECTOR (MAX_FILES*WORD_SIZE-1 downto 0);
               data_out     : out   STD_LOGIC_VECTOR (MAX_FILES*WORD_SIZE-1 downto 0);
               enable_in    : in    STD_LOGIC;                                          -- tell the module to write/read data_in/next data
               enable_out   : out   STD_LOGIC;                                          -- '1' when data_in/data_out can be write/read or
               
               debug        : out   STD_LOGIC_VECTOR ( 5 downto 0);                      -- only meaningfull for debug or error read
               sd_error     : out   STD_LOGIC;                                          -- '1' if an error occured, error value is in debug output
               
               SD_DAT       : inout STD_LOGIC_VECTOR ( 3 downto 0);                      -- DATA line for SDcard access
               SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
               SD_WP        : in    STD_LOGIC := '0';                                   -- Write Protect (Not used)
               SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
               SD_CMD       : inout STD_LOGIC);                                         -- Command line
    end component;

    constant CLK_FREQ_HZ         : integer := 100000000;
    constant VIVADO_SYNTH        : boolean := True;
    constant HIGH_SPEED_IF       : boolean := False;
    constant WORD_SIZE_PW2       : integer range 0 to 3 := 0;
    constant BUFFER_NUM_PW2      : integer range 1 to 8 := 2;
    constant PARAMETER_BUFFERING : boolean := True;

    constant LITTLE_ENDIAN       : boolean := True;

    constant FULL_WORDS_ONLY     : boolean := True;

    constant FORMAT_CLUSTER_SIZE : integer range 12 to 18 := 12;
    constant MAX_FILES           : integer :=   8;
    constant WORD_SIZE           : integer :=   8;
    constant FIFO_SIZE           : integer := 512;
    constant TREE_STRUCTURE_MAX  : integer :=   3;
    constant FRAG_CLUST          : integer :=   1;


    signal file_name    : std_logic_vector(87 downto 0) := (others => '0');
    signal date_time    : std_logic_vector(39 downto 0) := (others => '0');
    signal open_file    : std_logic_vector(MAX_FILES-1 downto 0) := (others => '0');
    signal opened_file  : std_logic_vector(MAX_FILES-1 downto 0) := (others => '0');
    signal rw_file      : std_logic_vector(MAX_FILES-1 downto 0) := (others => '0');
    signal busy         : std_logic := '0';
    signal close_file   : std_logic := '0';
    signal open_dir     : std_logic := '0';
    signal erase_file   : std_logic := '0';

    signal fat_error    : std_logic_vector( 7 downto 0) := (others => '0');
    signal format       : std_logic := '0';

    signal data_in      : std_logic_vector(MAX_FILES*WORD_SIZE-1 downto 0) := (others => '0');
    signal data_out     : std_logic_vector(MAX_FILES*WORD_SIZE-1 downto 0) := (others => '0');
    signal enable_in    : std_logic := '0';
    signal enable_out   : std_logic := '0';

    signal debug        : std_logic_vector( 5 downto 0) := (others => '0');
    signal sd_error     : std_logic := '0';

    


begin



    SD_1 :  Sdcard_FAT32
    generic map (CLK_FREQ_HZ        => CLK_FREQ_HZ,
                 VIVADO_SYNTH       => VIVADO_SYNTH,
                 HIGH_SPEED_IF      => HIGH_SPEED_IF,
                 WORD_SIZE_PW2      => WORD_SIZE_PW2,
                 BUFFER_NUM_PW2     => BUFFER_NUM_PW2,
                 PARAMETER_BUFFERING=> PARAMETER_BUFFERING,
                 
                 LITTLE_ENDIAN      => LITTLE_ENDIAN,

                 FULL_WORDS_ONLY    => FULL_WORDS_ONLY,
                 MAX_FILES          => MAX_FILES,
                 WORD_SIZE          => WORD_SIZE,
                 FORMAT_CLUSTER_SIZE=> FORMAT_CLUSTER_SIZE,
                 FIFO_SIZE          => FIFO_SIZE,
                 TREE_STRUCTURE_MAX => TREE_STRUCTURE_MAX,
                 FRAG_CLUST         => FRAG_CLUST)
    Port map ( clk          => clk,
               reset        => reset,

               tx           => tx,
               rx           => rx,
               sw           => sw,
               led          => led,
               
               file_name    => file_name,
               date_time    => date_time,
               open_file    => open_file,
               opened_file  => opened_file,
               rw_file     => rw_file,
               busy         => busy,
               close_file   => close_file,
               open_dir     => open_dir,
               erase_file   => erase_file,
               
               fat_error    => fat_error,
               format       => format,

               data_in      => data_in,
               data_out     => data_out,
               enable_in    => enable_in,
               enable_out   => enable_out,
               
               debug        => debug,
               sd_error     => sd_error,
               
               SD_DAT       => SD_DAT,
               SD_CD        => SD_CD,
               SD_WP        => SD_WP,
               SD_CLK       => SD_CLK,
               SD_CMD       => SD_CMD);


end Behavioral;