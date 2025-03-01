----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/05/2021 07:37:20 PM
-- Design Name: 
-- Module Name: tb_SDcard_raw_access_simmodel - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use std.textio.all; -- Imports the standard textio package.

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity tb_SDcard_raw_access_simmodel_v2 is
    generic (IMAGE_DIRNAME : string := "/tmp/random_dir/");
--  Port ( );
end tb_SDcard_raw_access_simmodel_v2;

architecture Behavioral of tb_SDcard_raw_access_simmodel_v2 is

    signal clk          : std_logic;
    signal reset        : std_logic;

    signal SD_block     : std_logic_vector(31 downto 0);
    signal TR_buffer    : std_logic_vector( 1 downto 0);

    signal read_block   : std_logic := '0';
    signal write_block  : std_logic := '0';
    signal erase_block  : std_logic := '0';
    signal multiple     : std_logic := '0';
    signal prep_block   : std_logic := '0';
    signal busy         : std_logic;
    signal err_code     : std_logic_vector( 3 downto 0);
    signal blocks       : std_logic_vector(31 downto 0);
               
    signal loc_buffer   : std_logic_vector( 1 downto 0) := "00";
    signal address      : std_logic_vector( 8 downto 0) := "000000000";
    signal data_write   : std_logic:='0';
    signal data_in      : std_logic_vector( 7 downto 0) := x"00";
    signal data_out     : std_logic_vector( 7 downto 0);

    signal SD_DAT       : std_logic_vector( 3 downto 0);
    signal SD_CD        : std_logic;
    signal SD_CLK       : std_logic;
    signal SD_CMD       : std_logic;



begin

    uut : entity work.SDcard_raw_access_simmodel_V2
    generic map(DIRNAME             => IMAGE_DIRNAME,
                CLK_FREQ_HZ         => 100000000,
                WORD_SIZE_PW2       => 0,
                BUFFER_NUM_PW2      => 2,
                PARAMETER_BUFFERING => True,
                LITTLE_ENDIAN       => True )
    port map(  clk          => clk,
               reset        => reset,
               
               SD_block     => SD_block,
               TR_buffer    => TR_buffer,
               read_block   => read_block,
               write_block  => write_block,
               erase_block  => erase_block,
               multiple     => multiple,
               prep_block   => prep_block,
               busy         => busy,
               err_code     => err_code,
               blocks       => blocks,
               
               loc_buffer   => loc_buffer,
               address      => address,
               data_write   => data_write,
               data_in      => data_in,
               data_out     => data_out,

               SD_DAT       => SD_DAT,
               SD_CD        => SD_CD,
               SD_CLK       => SD_CLK,
               SD_CMD       => SD_CMD);
    
    process
    begin
        clk <= '1';
        wait for 5 ns;
        clk <= '0';
        wait for 5 ns;
    end process;

    process
    begin
        reset <= '1';
        wait for 104 ns;
        reset <= '0';
        wait;
    end process;
    

    process
        variable l : line;
    begin
        
        wait until falling_edge(busy);
        
        -- read a single block
        -- to be checked in the simulator
        wait for 50 ns;
        SD_block   <= x"00001000";
        TR_buffer  <= "00";
        read_block <= '1';
        wait for 20 ns;
        read_block <= '0';
        wait until falling_edge(busy);
        
        -- write a block filled with default value
        -- to be checked in the resulting file
        wait for 50 ns;
        SD_block   <= x"00001100";
        TR_buffer  <= "10";
        write_block <= '1';
        wait for 20 ns;
        write_block <= '0';
        wait until falling_edge(busy);

        -- copy block previously read
        wait for 50 ns;
        SD_block   <= x"00000300";
        TR_buffer  <= "00";
        write_block <= '1';
        wait for 20 ns;
        write_block <= '0';
        wait until falling_edge(busy);
        
        -- full buffer with a counter value
        -- and write it to the file
        -- to be check on the corresponding chink file
        wait for 50 ns;
        wait until rising_edge(clk);
        
        for i in 0 to 255 loop
            loc_buffer <= "11";
            address    <= std_logic_vector(to_unsigned(i, 9));
            data_in    <= std_logic_vector(to_unsigned(i, 8));
            data_write <= '1';
            wait until rising_edge(clk);
        end loop;
        for i in 256 to 511 loop
            loc_buffer <= "11";
            address    <= std_logic_vector(to_unsigned(i, 9));
            data_in    <= std_logic_vector(to_unsigned(i-256, 8));
            data_write <= '1';
            wait until rising_edge(clk);
        end loop;
        data_write <= '0';
        
        SD_block   <= x"00000200";
        TR_buffer  <= "11";
        write_block <= '1';
        wait for 20 ns;
        write_block <= '0';
        wait until falling_edge(busy);

        -- erase a block
        -- to be checked in the simulator
        wait for 50 ns;
        SD_block   <= x"00000100";
        erase_block <= '1';
        wait for 20 ns;
        erase_block <= '0';
        wait until falling_edge(busy);

        -- erase multiple block
        -- to be checked in the simulator
        wait for 50 ns;
        SD_block   <= x"00000FFC";
        erase_block <= '1';
        multiple    <= '1';
        wait for 20 ns;
        erase_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        SD_block   <= x"00001003";
        erase_block <= '1';
        wait until rising_edge(clk);
        erase_block <= '0';
        multiple    <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);


        -- perform multiple write
        
        for i in 0 to 511 loop
            loc_buffer <= "10";
            address    <= std_logic_vector(to_unsigned(i, 9));
            data_in    <= std_logic_vector(to_unsigned(42, 8));
            data_write <= '1';
            wait until rising_edge(clk);
        end loop;
        data_write <= '0';
        wait until rising_edge(clk);
        SD_block   <= x"000007FE";
        TR_buffer  <= "10";
        write_block <= '1';
        multiple    <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        wait until falling_edge(busy);
        wait until falling_edge(clk);

        address    <= std_logic_vector(to_unsigned(0, 9));
        data_in    <= std_logic_vector(to_unsigned(43, 8));
        data_write <= '1';
        wait until rising_edge(clk);
        data_write <= '0';
        wait until rising_edge(clk);
        write_block <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);
        
        address    <= std_logic_vector(to_unsigned(0, 9));
        data_in    <= std_logic_vector(to_unsigned(44, 8));
        data_write <= '1';
        wait until rising_edge(clk);
        data_write <= '0';
        wait until rising_edge(clk);
        write_block <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);
        
        address    <= std_logic_vector(to_unsigned(0, 9));
        data_in    <= std_logic_vector(to_unsigned(45, 8));
        data_write <= '1';
        wait until rising_edge(clk);
        data_write <= '0';
        wait until rising_edge(clk);
        write_block <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        TR_buffer   <= "11";
        write_block <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        multiple    <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);



        -- perform multiple read
        
        SD_block   <= x"000017FE";
        TR_buffer  <= "00";
        read_block <= '1';
        multiple   <= '1';
        wait until rising_edge(clk);
        read_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        TR_buffer  <= "01";
        read_block <= '1';
        wait until rising_edge(clk);
        read_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        TR_buffer  <= "10";
        read_block <= '1';
        wait until rising_edge(clk);
        read_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        TR_buffer  <= "11";
        read_block <= '1';
        wait until rising_edge(clk);
        read_block <= '0';
        multiple   <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);


        -- write back results of copy for script checking

        SD_block    <= x"00000500";
        TR_buffer   <= "00";
        write_block <= '1';
        multiple    <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        TR_buffer   <= "01";
        write_block <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        TR_buffer   <= "10";
        write_block <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);

        TR_buffer   <= "11";
        write_block <= '1';
        wait until rising_edge(clk);
        write_block <= '0';
        multiple    <= '0';
        wait until falling_edge(busy);
        wait until rising_edge(clk);






        
        write (l, String'("Simulation Over"));
        writeline (output, l);

        wait;
    end process;


end Behavioral;
