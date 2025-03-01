library IEEE;
use IEEE.Std_logic_1164.all;
use IEEE.Numeric_Std.all;

entity tb_FIFO_Generic is
end;

architecture bench of tb_FIFO_Generic is

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

    constant DATA_SIZE           : integer :=  8;
    constant FIFO_DEPTH          : integer := 64;

    signal data_in  : std_logic_vector(DATA_SIZE-1 downto 0);
    signal dwrite   : std_logic;
    signal full_n   : std_logic;

    signal elemts   : integer range 0 to FIFO_DEPTH;
    signal data_out : std_logic_vector(DATA_SIZE-1 downto 0);
    signal dread    : std_logic;
    signal empty_n  : std_logic;


    signal data            : unsigned(DATA_SIZE - 1 downto 0);
    signal nb_data_writted : integer;
    signal nb_data_read    : integer;
    signal test_nb         : integer;

    signal clk            : std_logic;
    constant clock_period : time := 10 ns;
    signal reset          : std_logic;


begin


    fifo : FIFO_Generic
        Generic map (DATA_SIZE => DATA_SIZE,
                 FIFO_DEPTH => FIFO_DEPTH)
        Port map ( clk      => clk,
                   reset    => reset,
                   
                   data_in  => data_in,
                   dwrite   => dwrite,
                   full_n   => full_n,
        
                   elemts   => elemts,
                   data_out => data_out,
                   dread    => dread,
                   empty_n  => empty_n);


    stimulus: process
    begin

        -----------------------------------------------------------------
        --  RESET
        -----------------------------------------------------------------

        reset   <= '1';
        dread   <= '0';
        dwrite  <= '0';
        test_nb <= 0;

        wait for 16 ns;
        reset <= '0';
        wait for 5 ns;

        wait for 30 ns;


        ----------------------------------------------------
        test_nb <= 1;
        ----------------------------------------------------

        dwrite <= '1';
        wait for clock_period * 20;
        dwrite <= '0';
        wait for clock_period;

        ----------------------------------------------------
        test_nb <= 2;
        ----------------------------------------------------

        dread <= '1';
        wait for clock_period * 10;
        dwrite <= '1';
        wait for clock_period * 5;
        dwrite <= '0';
        wait until falling_edge(empty_n);
        dread <= '0';
        wait for clock_period;

        ----------------------------------------------------
        test_nb <= 3;
        ----------------------------------------------------

        dwrite <= '1';
        wait for clock_period;
        dwrite <= '0';
        wait for clock_period;

        dwrite <= '1';
        wait for clock_period;
        dwrite <= '0';
        wait for clock_period;
        
        dread <= '1';
        wait for 2 * clock_period;
        dread <= '0';
        wait for clock_period;

        ----------------------------------------------------
        test_nb <= 4;   -- test 3 bis
        ----------------------------------------------------

        dwrite <= '1';
        wait for clock_period;
        dwrite <= '0';
        wait for 2*clock_period;

        dwrite <= '1';
        wait for clock_period;
        dwrite <= '0';
        wait for clock_period;

        dwrite <= '1';
        wait for clock_period;
        dwrite <= '0';
        wait for clock_period;

        dwrite <= '1';
        wait for clock_period;
        dwrite <= '0';
        wait for clock_period;
        
        dread <= '1';
        wait for 4 * clock_period;
        dread <= '0';
        wait for clock_period;

        ----------------------------------------------------
        test_nb <= 5;
        ----------------------------------------------------

        dwrite <= '1';
        dread <= '1';
        wait for 30 * clock_period;
        dread <= '0';
        wait until falling_edge(full_n);
        dwrite <= '0';
        wait for clock_period;

        ----------------------------------------------------
        test_nb <= 6;
        ----------------------------------------------------

        dread <= '1';
        wait for 32 * clock_period;
        dwrite <= '1';
        wait for 4 * clock_period;
        dwrite <= '0';
        wait for 16 * clock_period;
        dwrite <= '1';
        wait for 4 * clock_period;
        dwrite <= '0';
        wait for 12 * clock_period;
        dwrite <= '1';
        wait for 4 * clock_period;
        dwrite <= '0';
        wait for 8 * clock_period;
        dwrite <= '1';
        wait for 4 * clock_period;
        dwrite <= '0';
        wait for 8 * clock_period;
        dwrite <= '1';
        wait for 4 * clock_period;
        dwrite <= '0';
        wait for 6 * clock_period;
        dread <= '0';


        wait for 50 ns;


        ----------------------------------------------------
        test_nb <= 7;
        ----------------------------------------------------
        
        wait;
    end process;



    data_in <= std_logic_vector(data);

    -----------------------------------------------------------------
    --  Data creation
    -----------------------------------------------------------------
    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                data            <= (others => '0');
                nb_data_writted <= 0;
            elsif dwrite = '1' and full_n = '1' then
            -- else
                data            <= data + 1;
                nb_data_writted <= nb_data_writted + 1;
            end if;
        end if;
    end process;


    Process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                nb_data_read <= 0;
            elsif dread = '1' and empty_n = '1' then
                nb_data_read <= nb_data_read + 1;
            end if;
        end if;
    end process;


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