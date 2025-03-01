library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity SDcard_read_raw is
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
end SDcard_read_raw;

architecture Behavioral of SDcard_read_raw is

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
    -- Components constants
    -------------------------------------------------------------------------
    constant CLK_FREQ_HZ   : integer := 100000000;   
    constant WORD_SIZE_PW2 : integer range 0 to 3 := 0; 
    constant HIGH_SPEED_IF : boolean := False;          
    constant LITTLE_ENDIAN : boolean := True;
    constant VIVADO_SYNTH  : boolean := True;
    
    constant BAUDRATE       : integer := 921600;
    constant UART_DATA_SIZE : integer :=     16;

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

    -------------------------------------------------------------------------
    -- Performances measurement signals
    -------------------------------------------------------------------------
    constant FIFO_MAX_SIZE : integer := 130000;
    type t_FIFO is array (0 to FIFO_MAX_SIZE) of std_logic_vector(15 downto 0); -- around 70 BRAM for 0.6ms latency bloc

    signal FIFO : t_FIFO;
    signal FIFO_output_ptr : integer range 0 to FIFO_MAX_SIZE := 0;
    signal FIFO_input_ptr  : integer range 0 to FIFO_MAX_SIZE := 0;
    signal FIFO_size       : integer range 0 to FIFO_MAX_SIZE := 0;
    signal FIFO_output     : std_logic_vector(15 downto 0);
    signal FIFO_full       : std_logic; -- indicate if the FIFO is full or almost
    
    signal clk_cycle_cnt : integer range 0 to CLK_FREQ_HZ /10 := 0;
    signal bytes_cnt     : integer range 0 to 512 := 0;
    signal blocs_cnt     : integer range 0 to 2**28 := 0;
    signal new_bloc      : std_logic;
    signal new_byte      : std_logic;


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
            elsif SDread_data_empty_n = '1' then
                clk_cycle_cnt <= 0;
            elsif clk_cycle_cnt = CLK_FREQ_HZ /10 then
                clk_cycle_cnt <= clk_cycle_cnt;
            else
                clk_cycle_cnt <= clk_cycle_cnt + 1;
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- SD card management 
    -------------------------------------------------------------------------
    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                SDread_data_read <= '0';
            elsif FIFO_full = '0' and SDread_data_empty_n = '1' and transfert_en = '1' then
                SDread_data_read <= '1';
            else
                SDread_data_read <= '0';
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- FIFO management
    -------------------------------------------------------------------------
    -- FIFO output
    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                FIFO_output_ptr <= 0;
                FIFO_output     <= (others => '0');
            else
                FIFO_output <= FIFO(FIFO_output_ptr);
                if FIFO_output_ptr /= FIFO_input_ptr and UART_send_bsy = '0' and UART_send_den = '1' then
                    if FIFO_output_ptr = FIFO_MAX_SIZE then
                        FIFO_output_ptr <= 0;
                    else
                        FIFO_output_ptr <= FIFO_output_ptr + 1;
                    end if;                
                end if;
            end if;
        end if;
    end process;

    -- FIFO input
    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                FIFO_input_ptr <= 0;
            elsif SDread_data_empty_n = '1' and SDread_data_read = '1' and FIFO_full = '0' then
                FIFO(FIFO_input_ptr) <= std_logic_vector(to_unsigned(clk_cycle_cnt, 16));
                if FIFO_input_ptr = FIFO_MAX_SIZE then
                    FIFO_input_ptr <= 0;
                else
                    FIFO_input_ptr <= FIFO_input_ptr + 1;
                end if;
            end if;
        end if;
    end process;
    
    -- Update FIFO size
    Process( clk )
    begin
        if rising_edge(clk) then
            if reset = '1' then
                FIFO_size <= 0;
                FIFO_full <= '0';
            elsif FIFO_SIZE = FIFO_MAX_SIZE - 1 then
                FIFO_full <= '1';
            else
                FIFO_full <= '0';
                if SDread_data_empty_n = '1' and Sdread_data_read = '1' and
                   FIFO_output_ptr /= FIFO_input_ptr and UART_send_bsy = '0' and UART_send_den = '0' then
                    FIFO_size <= FIFO_size;
                elsif SDread_data_empty_n = '1' and Sdread_data_read = '1' then
                    FIFO_size <= FIFO_size + 1;
                elsif FIFO_output_ptr /= FIFO_input_ptr and UART_send_bsy = '0' and UART_send_den = '0' then
                    FIFO_size <= FIFO_SIZE - 1;
                else
                    FIFO_size <= FIFO_size;
                end if;
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- UART SEND management
    -------------------------------------------------------------------------
    UART_send_din <= FIFO_output;
    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                UART_send_den <= '0';
            elsif FIFO_output_ptr /= FIFO_input_ptr and FIFO_full = '0'
              and UART_send_bsy = '0' and UART_send_den = '0' and transfert_en = '1' then
                UART_send_den <= '1';
            else
                UART_send_den <= '0';
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

end Behavioral;