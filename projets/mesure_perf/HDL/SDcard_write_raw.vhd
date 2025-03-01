library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity SDcard_write_raw is
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
end SDcard_write_raw;

architecture Behavioral of SDcard_write_raw is

    -------------------------------------------------------------------------
    -- SD card read raw stream
    -------------------------------------------------------------------------
    component Sdcard_readstream is
        Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
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

    component Sdcard_writestream is
        Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
                VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
                WORD_SIZE_PW2       : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
                PRE_ERASE           : boolean := True;                -- determines if the SDcard should first be pre-erased to improve performance
                HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
                LITTLE_ENDIAN       : boolean := True);               -- when multiple bytes per word, tells which byte is stored at lower address
                                                              --     little endian (True) mean least significant byte first
                                                                      
        Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
               reset        : in    STD_LOGIC;                                          -- reset, active high
               
               data_in      : in    STD_LOGIC_VECTOR(8*(2**WORD_SIZE_PW2)-1 downto 0);  -- stream data input
               data_write   : in    STD_LOGIC;                                          -- data write, tells the module to write data_in
               data_full_n  : out   STD_LOGIC;                                          -- '1' when it is possible to write
               
               final_flush  : in    STD_LOGIC := '0';                                   -- optional data flush
               
               debug        : out   STD_LOGIC_VECTOR( 5 downto 0);                      -- only meaningfull for debug or error read
               sd_error     : out   STD_LOGIC;                                          -- '1' if an error occured, error value is in debug output
                          
               SD_DAT       : inout STD_LOGIC_VECTOR( 3 downto 0);                      -- DATA line for SDcard access
               SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
               SD_WP        : in    STD_LOGIC:='0';                                     -- (optional) SDcard physical write protection indicator
               SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
               SD_CMD       : inout STD_LOGIC);                                         -- Command line
    
    end component;

    -------------------------------------------------------------------------
    -- UART components
    -------------------------------------------------------------------------
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

    -------------------------------------------------------------------------
    -- Components constants
    -------------------------------------------------------------------------
    constant CLK_FREQ_HZ   : integer := 100000000;   
    constant WORD_SIZE_PW2 : integer range 0 to 3 := 0; 
    constant PRE_ERASE     : boolean := True; -- writestream only
    constant HIGH_SPEED_IF : boolean := False;          
    constant LITTLE_ENDIAN : boolean := True;
    constant VIVADO_SYNTH  : boolean := True;
    
    constant BAUDRATE       : integer := 921600;
    constant UART_DATA_SIZE : integer :=      8;

    -------------------------------------------------------------------------
    -- SDcard read interface's signals
    -------------------------------------------------------------------------
    -- signal SDread_data_out     : std_logic_vector( 8*(2**WORD_SIZE_PW2)-1 downto 0 );
    -- signal SDread_data_read    : std_logic;
    -- signal SDread_data_empty_n : std_logic;
    -- signal SDread_debug        : std_logic_vector( 5 downto 0 );
    -- signal SDread_error        : std_logic;

    -- signal SDread_error_mem    : std_logic; -- store error to send it through UART

    -------------------------------------------------------------------------
    -- SDcard send interface's signals
    -------------------------------------------------------------------------
    signal SDsend_data_in     : std_logic_vector( 8*(2**WORD_SIZE_PW2)-1 downto 0 );
    signal SDsend_data_write  : std_logic;
    signal SDsend_data_full_n : std_logic;
    signal SDsend_debug       : std_logic_vector( 5 downto 0 );
    signal SDsend_error       : std_logic;

    signal SDsend_final_flush : std_logic := '0';

    signal SDsend_error_mem   : std_logic; -- store error to send it through UART

    -------------------------------------------------------------------------
    -- UART SEND signals
    -------------------------------------------------------------------------
    signal UART_send_din : std_logic_vector( UART_DATA_SIZE - 1 downto 0 );
    signal UART_send_den : std_logic;
    signal UART_send_bsy : std_logic;

    -------------------------------------------------------------------------
    -- Data to write
    -------------------------------------------------------------------------
    signal data       : unsigned( 8*(2**WORD_SIZE_PW2)-1 downto 0 );
    signal data_write : std_logic;

begin

    LED( 0 ) <= SDsend_error_mem;
    LED( 6 downto 1 ) <= SDsend_debug;

    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                SDsend_error_mem <= '0';
            elsif SDsend_error = '1' then
                SDsend_error_mem <= '1';
            end if;
        end if;
    end process;

    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                UART_send_den <= '0';
                elsif UART_send_den = '0' and data_write = '1' and UART_send_bsy = '0' then
                    UART_send_den <= '1';
                else
                    UART_send_den <= '0';
            end if;
        end if;
    end process;
                    
    -- Enable write to SD card and update data
    SDsend_data_write <= data_write;
    SDsend_data_in    <= std_logic_vector( data );
    UART_send_din     <= std_logic_vector( data );
    Process( clk )
    begin
        if rising_edge( clk ) then 
            if reset = '1' then
                data_write <= '0';
                data       <= (others => '0');
            elsif transfert_en = '1' and SDsend_data_full_n = '1' and data_write = '0' then
                data_write <= '1';
            elsif data_write = '1' then
                data_write <= '0';
                if data = 255 then
                    data <= (others => '0');
                else
                    data <= data + 1 ;
                end if;
            else
                data_write <= '0';
            end if;
        end if;
    end process;


    SD_read : Sdcard_writestream generic map ( CLK_FREQ_HZ   => CLK_FREQ_HZ,
                                               WORD_SIZE_PW2 => WORD_SIZE_PW2,
                                               PRE_ERASE     => PRE_ERASE,
                                               HIGH_SPEED_IF => HIGH_SPEED_IF,
                                               LITTLE_ENDIAN => LITTLE_ENDIAN )
                                 port    map ( clk         => clk,
                                               reset       => reset,
                                               
                                               data_in     => SDsend_data_in,
                                               data_write  => SDsend_data_write,
                                               data_full_n => SDsend_data_full_n,

                                               final_flush => SDsend_final_flush,

                                               debug       => SDsend_debug,
                                               sd_error    => SDsend_error,
                                                                                              
                                               SD_DAT      => SD_DAT,  
                                               SD_CD       => SD_CD,  
                                               SD_WP       => SD_WP,  
                                               SD_CLK      => SD_CLK,  
                                               SD_CMD      => SD_CMD );


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
