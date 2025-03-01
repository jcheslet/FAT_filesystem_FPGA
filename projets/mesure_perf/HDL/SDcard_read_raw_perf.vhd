library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity SDcard_read_raw_perf is
    Port ( clk          : in    STD_LOGIC;
           reset        : in    STD_LOGIC; 

           transfert_en : in    STD_LOGIC;

           --rx           : in  STD_LOGIC;
           tx           : out   STD_LOGIC;

           LED          : out   STD_LOGIC_VECTOR (6 downto 0);

           SD_DAT       : inout STD_LOGIC_VECTOR (3 downto 0);
           SD_CD        : in    STD_LOGIC;
           SD_WP        : in    STD_LOGIC;
           SD_CLK       : out   STD_LOGIC;
           SD_CMD       : inout STD_LOGIC);
end SDcard_read_raw_perf;

architecture Behavioral of SDcard_read_raw_perf is

    -------------------------------------------------------------------------
    -- SD card read raw stream
    -------------------------------------------------------------------------
    component Sdcard_readstream is
        Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
                VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
                WORD_SIZE_PW2       : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
                HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
                LITTLE_ENDIAN       : boolean := True);               -- when multiple bytes per word, tells which byte is stored at lower address
                                                                      --     little endian (True) mean least significant byte first
                                                                      
        Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
               reset        : in    STD_LOGIC;                                          -- reset, active high
               
               data_out     : out   STD_LOGIC_VECTOR(8*(2**WORD_SIZE_PW2)-1 downto 0);  -- stream data output
               data_read    : in    STD_LOGIC;                                          -- data read, tells the module to send next data
               data_empty_n : out   STD_LOGIC;                                          -- '1' when data_out can be read
               
               debug        : out   STD_LOGIC_VECTOR( 5 downto 0);                      -- only meaningfull for debug or error read
               sd_error     : out   STD_LOGIC;                                          -- '1' if an error occured, error value is in debug output
               
               SD_DAT       : inout STD_LOGIC_VECTOR( 3 downto 0);                      -- DATA line for SDcard access
               SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
               SD_WP        : in    STD_LOGIC := '0';                                   -- SDcard protected (useless, just for convenience)
               SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
               SD_CMD       : inout STD_LOGIC);                                         -- Command line
    
    end component;

    -------------------------------------------------------------------------
    -- UART components
    -------------------------------------------------------------------------
    component UART_SEND_generic is
        Generic (CLK_FREQU : integer := 100000000;
             BAUDRATE      : integer :=  33333333;
             TIME_PREC     : integer :=    100000;
             DATA_SIZE     : integer :=        8);
    Port ( clk   : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           TX    : out STD_LOGIC;
           din   : in  STD_LOGIC_VECTOR (DATA_SIZE - 1 downto 0);
           den   : in  STD_LOGIC;
           bsy   : out STD_LOGIC);
    end component;


    -------------------------------------------------------------------------
    -- FIFO component
    -------------------------------------------------------------------------
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

    -------------------------------------------------------------------------
    -- Components constants
    -------------------------------------------------------------------------
    constant CLK_FREQ_HZ   : integer := 100000000;   
    constant WORD_SIZE_PW2 : integer range 0 to 3 := 0; 
    constant HIGH_SPEED_IF : boolean := False;          
    constant LITTLE_ENDIAN : boolean := True;
    constant VIVADO_SYNTH  : boolean := True;
    
    constant BAUDRATE       : integer := 921600;
    constant UART_DATA_SIZE : integer :=     8;

    constant PERF_DATA_SIZE : integer := 24;                           -- size in bits of the clock cycle counter in each block
    constant FIFO_MAX_SIZE  : integer := 2**15 * 110 / PERF_DATA_SIZE; -- Half of BRAMs are used

    -------------------------------------------------------------------------
    -- SDcard read interface's signals
    -------------------------------------------------------------------------
    signal SDread_data_out     : std_logic_vector( 8*(2**WORD_SIZE_PW2)-1 downto 0 );
    signal SDread_data_read    : std_logic;
    signal SDread_data_empty_n : std_logic;
    signal SDread_debug        : std_logic_vector( 5 downto 0 );
    signal SDread_error        : std_logic;

    signal SDread_error_mem    : std_logic; -- store error to send it through UART

    -------------------------------------------------------------------------
    -- UART SEND signals
    -------------------------------------------------------------------------
    signal UART_send_din : std_logic_vector(UART_DATA_SIZE - 1 downto 0);
    signal UART_send_den : std_logic;
    signal UART_send_bsy : std_logic;

    signal UART_send_word  : std_logic_vector(PERF_DATA_SIZE-UART_DATA_SIZE-1 downto 0);
    signal UART_bytes_sent : integer range 0 to PERF_DATA_SIZE / 8;
    signal UART_send_parse : std_logic;   -- High when no data is being transfered

    -------------------------------------------------------------------------
    -- FIFO signals
    -------------------------------------------------------------------------
    signal FIFO_data_in   : STD_LOGIC_VECTOR(PERF_DATA_SIZE-1 downto 0);
    signal FIFO_new_data  : STD_LOGIC;
    signal FIFO_data_out  : STD_LOGIC_VECTOR(PERF_DATA_SIZE-1 downto 0);
    signal FIFO_data_read : STD_LOGIC;
    signal FIFO_full      : STD_LOGIC;
    signal FIFO_empty     : STD_LOGIC;

    -------------------------------------------------------------------------
    -- Measurement signals
    -------------------------------------------------------------------------
    signal clk_cycle_cnt : integer range 0 to 2**PERF_DATA_SIZE-1 := 0;         -- nb of clock cycle to read a complete block, starting at the moment we want to read it
    signal bytes_cnt     : integer range 0 to 512 := 0;                         -- cnt bytes read in a block
    signal new_block     : std_logic;                                           -- high the moment a block has been completly read, i.e. when a new block is fetch 

begin

    -------------------------------------------------------------------------
    -- Error management (not really yet)
    -------------------------------------------------------------------------
    LED( 0 ) <= SDread_error_mem;
    LED( 6 downto 1 ) <= SDread_debug;

    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                SDread_error_mem <= '0';
            elsif SDread_error = '1' then
                SDread_error_mem <= '1';
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- Performances management
    -------------------------------------------------------------------------
    -- Clk_cycle counter
    Process( clk )
    begin
        if rising_edge(clk) then
            if reset = '1' then
                clk_cycle_cnt <= 0;
            elsif new_block = '1' then
                clk_cycle_cnt <= 0;
            elsif SDread_data_read = '1' then
                if clk_cycle_cnt = 2**PERF_DATA_SIZE-1 then
                    clk_cycle_cnt <= clk_cycle_cnt;
                else
                    clk_cycle_cnt <= clk_cycle_cnt + 1;
                end if;
            end if;
        end if;
    end process;

    Process( clk )
    begin
        if rising_edge(clk) then
            if reset = '1' then
                bytes_cnt <= 0;
                new_block <= '0';
            elsif SDread_data_empty_n = '1' and SDread_data_read = '1' and SDread_debug /= "000001" then
                if bytes_cnt = 512 then
                    bytes_cnt <= 0;
                    new_block <= '1';
                else
                    bytes_cnt <= bytes_cnt + 1;
                    new_block <= '0';
                end if;
            else
                new_block <= '0';
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- SD card management 
    -------------------------------------------------------------------------
    -- Enable write to SD card and update data
    SDread_data_read <= '1' when transfert_en = '1' and FIFO_full = '0' else '0';

    -------------------------------------------------------------------------
    -- FIFO management
    -------------------------------------------------------------------------
    Process( clk )
    begin
        if rising_edge(clk) then
            if reset = '1' then
                FIFO_data_in  <= (others => '0');
                FIFO_new_data <= '0';
            elsif new_block = '1'  then
                FIFO_data_in  <= std_logic_vector(to_unsigned(clk_cycle_cnt, PERF_DATA_SIZE));
                FIFO_new_data <= '1';
            else
                FIFO_new_data <= '0';
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- UART management (and FIFO_data_read)
    -------------------------------------------------------------------------
    Process( clk )
    begin
        if clk'event and clk ='1' then
            if reset = '1' then
                UART_send_den   <= '0';
                UART_send_din   <= (others => '0');
                UART_send_word  <= (others => '0');
                UART_bytes_sent <= 0;
                UART_send_parse <= '0';
                FIFO_data_read  <= '0'; 
            elsif UART_send_parse = '0' then
                if FIFO_empty = '0' and UART_send_bsy = '0' then
                    UART_send_parse <= '1';
                    UART_send_word  <= FIFO_data_out(PERF_DATA_SIZE-1 downto UART_DATA_SIZE);
                    UART_send_din   <= FIFO_data_out(UART_DATA_SIZE-1 downto 0);
                    UART_send_den   <= '1';
                    UART_bytes_sent <= 1;
                    FIFO_data_read  <= '1'; 
                else
                    UART_send_parse <= '0';
                    UART_send_den   <= '0';
                    FIFO_data_read  <= '0'; 
                end if;
            else
                FIFO_data_read  <= '0'; 
                if UART_bytes_sent = PERF_DATA_SIZE / 8 then
                    UART_bytes_sent <= 0;
                    UART_send_parse <= '0';
                    UART_send_den <= '0';
                else
                    if UART_send_bsy = '0' and UART_send_den = '0' then
                        UART_send_din   <= UART_send_word(UART_DATA_SIZE-1 downto 0);
                        UART_send_word  <= (UART_DATA_SIZE-1 downto 0 => '0') & UART_send_word(PERF_DATA_SIZE-UART_DATA_SIZE-1 downto UART_DATA_SIZE);
                        UART_send_den   <= '1';
                        UART_bytes_sent <= UART_bytes_sent + 1;
                    else
                        UART_send_den <= '0';
                    end if;
                end if;
            end if;
        end if;
    end process;
    SD_read : Sdcard_readstream generic map ( CLK_FREQ_HZ   => CLK_FREQ_HZ,
                                              VIVADO_SYNTH  => VIVADO_SYNTH,
                                              WORD_SIZE_PW2 => WORD_SIZE_PW2,
                                              HIGH_SPEED_IF => HIGH_SPEED_IF,
                                              LITTLE_ENDIAN => LITTLE_ENDIAN )
                                port    map ( clk          => clk,
                                              reset        => reset,
                                              
                                              data_out     => SDread_data_out,
                                              data_read    => SDread_data_read,
                                              data_empty_n => SDread_data_empty_n,
                                              
                                              debug        => SDread_debug,
                                              sd_error     => SDread_error,
                                              
                                              SD_DAT       => SD_DAT,  
                                              SD_CD        => SD_CD,  
                                              SD_WP        => SD_WP,  
                                              SD_CLK       => SD_CLK,  
                                              SD_CMD       => SD_CMD );


    UART_send : UART_SEND_generic generic map ( CLK_FREQU => CLK_FREQ_HZ,
                                                BAUDRATE  => BAUDRATE,
                                                TIME_PREC => 100,
                                                DATA_SIZE => UART_DATA_SIZE )
                                  port    map ( clk   => clk,
                                                reset => reset,
                                                
                                                TX    => tx,
                                                din   => UART_send_din,
                                                den   => UART_send_den,
                                                bsy   => UART_send_bsy );

    FIFO_send : FIFO generic map ( WORD_SIZE => PERF_DATA_SIZE,
                                   FIFO_SIZE => FIFO_MAX_SIZE )
                     port    map ( clk       => clk,
                                   reset     => reset,
                                   
                                   data_in   => FIFO_data_in,
                                   new_data  => FIFO_new_data,

                                   data_out  => FIFO_data_out,
                                   data_read => FIFO_data_read,
                                   
                                   empty     => FIFO_empty,
                                   full      => FIFO_full);


end Behavioral;