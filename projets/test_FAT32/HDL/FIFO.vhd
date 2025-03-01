library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity FIFO is
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
end FIFO;

architecture Behavioral of FIFO is

    type t_FIFO is array (0 to FIFO_SIZE-1) of std_logic_vector(WORD_SIZE-1 downto 0);

    signal FIFO : t_FIFO;
    signal output_ptr : integer range 0 to FIFO_SIZE := 0;
    signal input_ptr  : integer range 0 to FIFO_SIZE := 0;
    signal size       : integer range 0 to FIFO_SIZE := 0;
    signal i_full     : std_logic; -- indicate if the FIFO is full or almost

begin

    -- FIFO output
    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                output_ptr  <= 0;
            elsif output_ptr /= input_ptr and data_read = '1' then
                if output_ptr = FIFO_SIZE then
                    output_ptr <= 0;
                else
                    output_ptr <= output_ptr + 1;
                end if;
            end if;
        end if;
    end process;

    -- FIFO input
    Process( clk )
    begin
        if rising_edge( clk ) then
            if reset = '1' then
                input_ptr <= 0;
            else
                data_out <= FIFO(output_ptr);
                if new_data = '1' and (i_full = '0' or (i_full = '1' and data_read = '1')) then
                    FIFO(input_ptr) <= data_in;
                    if input_ptr = FIFO_SIZE then
                        input_ptr <= 0;
                    else
                        input_ptr <= input_ptr + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- Process( clk )
    -- begin
    --     if rising_edge( clk ) then
    --         if reset = '1' then
    --             output_ptr  <= 0;
    --         else
    --             data_out <= FIFO(output_ptr);
    --             if output_ptr /= input_ptr and data_read = '1' then
    --                 if output_ptr = FIFO_SIZE then
    --                     output_ptr <= 0;
    --                 else
    --                     output_ptr <= output_ptr + 1;
    --                 end if;                
    --             end if;
    --         end if;
    --     end if;
    -- end process;

    -- -- FIFO input
    -- Process( clk )
    -- begin
    --     if rising_edge( clk ) then
    --         if reset = '1' then
    --             input_ptr <= 0;
    --         elsif new_data = '1' and (i_full = '0' or (i_full = '1' and data_read = '1')) then
    --             FIFO(input_ptr) <= data_in;
    --             if input_ptr = FIFO_SIZE then
    --                 input_ptr <= 0;
    --             else
    --                 input_ptr <= input_ptr + 1;
    --             end if;
    --         end if;
    --     end if;
    -- end process;

    -- Update FIFO size
    Process( clk )
    begin
        if rising_edge(clk) then
            if reset = '1' then
                empty <= '1';
            elsif output_ptr /= input_ptr then
                empty <= '0';
            else
                empty <= '1';
            end if;
        end if;
    end process;

    -- i_full <= '1' when (output_ptr = input_ptr + 1) or (output_ptr = 0 and input_ptr = FIFO_SIZE) else '0';
    i_full <= '1' when size >= FIFO_SIZE - 1  else '0';
    full   <= i_full;
    Process( clk )
    begin
        if rising_edge(clk) then
            if reset = '1' then
                size <= 0;
            elsif new_data = '1' and (data_read = '1' and output_ptr /= input_ptr) then
                size <= size;
            elsif new_data = '1' and i_full = '0' then
                size <= size + 1;
            elsif output_ptr /= input_ptr and data_read = '1' then
                size <= size - 1;
            end if;
        end if;
    end process;

end Behavioral;