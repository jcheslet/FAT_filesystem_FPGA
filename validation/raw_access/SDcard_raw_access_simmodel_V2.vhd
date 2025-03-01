----------------------------------------------------------------------------------
-- SDcard_raw_access_simmodel
--    Version V2.0 (2021/07/06)
--    Y. BORNAT - Bordeaux INP / ENSEIRB-MATMECA
--    This module is a simulation replacement for the SDcard_raw_access_v2 module
----------------------------------------------------------------------------------
--  In addition to the generic values of the SDcard_raw_access_v2 module, it
--  requires the DIRNAME generic which corresponds to the directory that emulates
--  the content of the SDcard.
--
--  SDcard image structure :
--     as file access is tricky in VHDL, the image file must be splits in several
--     parts ("chunks"). All parts must be positionned in a directory whose name
--     is given by the generic DIRNAME. A master file "main.nfo" in the directory
--     provides the number of 512-b blocks in the image, and the filenames of each
--     of them (order matters). File names must be of length 16. The Split_image.py
--     script transforms an SDcard image into a directory and vice versa.
--
-- WARNING : It is not recommended to use this module to write data while the
--           image directory is stored on an SSD. Such behhavior is likely to
--           stress the SSD more than normal because of heavy write/rewrite
--           operations. The "multiple" behavior implementation might improve
--           the behavior of the module.
--
-- TODO / KNOWN BUGS :
--    - not sensitive to card detect (SD_CD)
--    - timings are not accurate to improve simulation speed
--    - no error codes are output
--    - no reaction to prep_block
--    - SDcard interface is left unassigned
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
--use ieee.std_logic_textio.all;

LIBRARY std;
USE std.textio.all;


--  ####### ###    ## ######## ## ######## ##    ##
--  ##      ####   ##    ##    ##    ##     ##  ##
--  #####   ## ##  ##    ##    ##    ##      ####
--  ##      ##  ## ##    ##    ##    ##       ##
--  ####### ##   ####    ##    ##    ##       ##




entity SDcard_raw_access_simmodel_V2 is
    Generic(DIRNAME             : string;                         -- the directory in which all image files are stored
            CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
            WORD_SIZE_PW2       : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
            BUFFER_NUM_PW2      : integer range 1 to 8 := 2;      -- codes the number of 512B data buffers for transfers, actual value will be 2^BUFFER_NUM_PW2
            PARAMETER_BUFFERING : boolean := True;                -- if true, the module keeps its own copy of input parameter SD_block
                                                                  --     setting this generic to false may save slice FFs, but SD_block
                                                                  --     input MUST NOT change as long as read_block, write_block or busy are set.
            LITTLE_ENDIAN       : boolean := True);               -- when multiple bytes per word, tells which byte is stored at lower address
                                                                  --     little endian (True) mean least significant byte first

    Port ( clk          : in    STD_LOGIC;
           reset        : in    STD_LOGIC;
           
           SD_block     : in    STD_LOGIC_VECTOR (31 downto 0);                     -- the block on which to perform the operation
           TR_buffer    : in    STD_LOGIC_VECTOR (BUFFER_NUM_PW2-1 downto 0);       -- the buffer on which to perform the operation
           read_block   : in    STD_LOGIC;                                          -- read operation
           write_block  : in    STD_LOGIC;                                          -- write operation
           erase_block  : in    STD_LOGIC := '0';                                   -- erase operation (different than write zeros, improves further reading)
           multiple     : in    STD_LOGIC := '0';                                   -- enables the multiple block operations
           prep_block   : in    STD_LOGIC := '0';                                   -- indicates the number of blocks to prepare for next operation
           busy         : out   STD_LOGIC;                                          -- module is busy
           err_code     : out   STD_LOGIC_VECTOR ( 3 downto 0);                     -- the error numer
           blocks       : out   STD_LOGIC_VECTOR (31 downto 0);                     -- number of available blocks
           
           loc_buffer   : in    STD_LOGIC_VECTOR (BUFFER_NUM_PW2-1 downto 0);       -- buffer for local access
           address      : in    STD_LOGIC_VECTOR (8-WORD_SIZE_PW2  downto 0);       -- data address in the buffer
           data_write   : in    STD_LOGIC;                                          -- data write to buffer
           data_in      : in    STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0); -- data to be written to buffer
           data_out     : out   STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0); -- data read from buffer

           SD_DAT       : inout STD_LOGIC_VECTOR ( 3 downto 0);                     -- DATA line for SDcard access
           SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
           SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
           SD_CMD       : inout STD_LOGIC);                                         -- Command line


end SDcard_raw_access_simmodel_V2;

architecture Behavioral of SDcard_raw_access_simmodel_V2 is

--   ####### ##  ######  ###    ##  #####  ##      #######
--   ##      ## ##       ####   ## ##   ## ##      ##
--   ####### ## ##   ### ## ##  ## ####### ##      #######
--        ## ## ##    ## ##  ## ## ##   ## ##           ##
--   ####### ##  ######  ##   #### ##   ## ####### #######

    constant FILENAME              : string  := "main.nfo";           -- the name of master file in the directory
    constant WORD_SIZE_BYTES       : integer :=    2**WORD_SIZE_PW2;  -- the data size of words expressed in bytes
    constant WORD_SIZE_BITS        : integer := 8*(2**WORD_SIZE_PW2); -- the data size of words in buffers expressed in bits

    type t_buffers is array(0 to 512*(2**BUFFER_NUM_PW2)/WORD_SIZE_BYTES - 1) of std_logic_vector(WORD_SIZE_BITS-1 downto 0);
    -- signal 'buffers' is duplicated because it is written in two processes
    -- they are constantly copied from one to another on differents clock edges
    signal buffers_r : t_buffers := (others => x"AA");      -- written by main process on falling edges of clkock
    signal buffers_w : t_buffers := (others => x"AA");      -- written by user access process on rising edges of clock

    signal user_addr : std_logic_vector(8 + BUFFER_NUM_PW2 - WORD_SIZE_PW2 downto 0);   -- buffer address, combined from 'loc_buffer' and 'address' inputs

    -- data types for variables that handle data exchanges
    subtype byte_t is natural range 0 to 255;               -- to manipulate bytes
    type chunk_t is array(0 to 1024*1024-1) of byte_t;    -- to store a chunk


    type fnames_t is array(0 to 4000) of string(1 to 16);
    signal fnames : fnames_t := (others => "                ");


    signal last_chunk_size : integer := 0;
    signal last_chunk_num  : integer := 0;
    
begin

    process

--   ##    ##  #####  ######  ##  #####  ######  ##      ####### #######
--   ##    ## ##   ## ##   ## ## ##   ## ##   ## ##      ##      ##
--   ##    ## ####### ######  ## ####### ######  ##      #####   #######
--    ##  ##  ##   ## ##   ## ## ##   ## ##   ## ##      ##           ##
--     ####   ##   ## ##   ## ## ##   ## ######  ####### ####### #######



        variable blockcount       : integer;          -- the number of 512-b blocks in the SDcard image
        variable readover         : boolean;          -- used for main descriptor analysis : when is blockcount complete ?
        variable currfilenum      : integer;          -- used for main descriptor analysis : what filename are we reading ?
        variable namestring       : string(1 to 16);  -- the chunk filename currently being built from main file read
        variable chunk            : chunk_t;          -- the RAM version of the chunk we are working on
        variable chunksize        : integer;          -- the size of current chunk
        variable blockaddr        : integer;          -- the target 512-b block number for current read/write operation
        variable blockaddr2       : integer;          -- second block is needed for multiple erase
        variable fileindex        : integer;          -- the chunk number concerned by current read/write operation
        variable op_buffer        : integer;          -- integer version of local buffer used for current operation
        variable multiple_cncl    : boolean;          -- multiple operation has been cancelled
        variable flush            : boolean;          -- flush required for multiple operations
        

        -- this is to handle files
        type char_file_t is file of character;
        file SDcard_file : char_file_t;

        variable char_v : character;                  -- the char actually read/written to the file
        variable byte_v : byte_t;                     -- integer version of the previous char

  
        variable l : line;
    begin
        -- this statement is mandatory to keep coherence between the two versions of buffers_X
        wait until falling_edge(clk);
        buffers_r <= buffers_w;
        wait until rising_edge(clk);


        if reset='1' then
          
            --  ######  ####### ####### ####### ########
            --  ##   ## ##      ##      ##         ##
            --  ######  #####   ####### #####      ##
            --  ##   ## ##           ## ##         ##
            --  ##   ## ####### ####### #######    ##


          
            SD_DAT <= "0001";
            busy <= '1';
            wait until reset = '0';
            
            -- wait after reset, just for fun ...
            -- (some modules need time to get ready, this one does, being ready
            -- just after reset is not consistent with real behavior)
            for i in 0 to 19 loop
                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                wait until rising_edge(clk);
            end loop;

            -- read first line of the main file to get size information
            file_open(SDcard_file, DIRNAME & FILENAME, read_mode);
            blockcount := 0;
            readover   := False;
            while not readover loop
                read(SDcard_file, char_v);
                byte_v := character'pos(char_v);
                if byte_v>=48 and byte_v<=59 then
                    blockcount := blockcount*10 + byte_v - 48;
                else
                    readover := True;
                end if;
            end loop;


            -- retrieve all chunks filenames
            currfilenum := 0;
            while not endfile(SDcard_file) loop
                read(SDcard_file, char_v);
                byte_v := character'pos(char_v);
                if byte_v<32 then
                    fnames(currfilenum) <= namestring;
                    currfilenum := currfilenum + 1;
                    namestring := "                ";
                else
                    namestring := namestring(2 to 16) & char_v;
                end if;
            end loop;
            file_close(SDcard_file);

            -- just another wait for fun... :)
            for i in 0 to 4 loop
                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                wait until rising_edge(clk);
                blocks <= std_logic_vector(to_unsigned(blockcount, 32));
                last_chunk_num  <= (blockcount + 2047)/2048 - 1;
                last_chunk_size <= (blockcount - (2048 * last_chunk_num))*512;
            end loop;
            busy <= '0';

        elsif read_block = '1' and multiple = '0' then
            --  ####### ## ###    ##  ######  ##      #######     ######  #######  #####  ######
            --  ##      ## ####   ## ##       ##      ##          ##   ## ##      ##   ## ##   ##
            --  ####### ## ## ##  ## ##   ### ##      #####       ######  #####   ####### ##   ##
            --       ## ## ##  ## ## ##    ## ##      ##          ##   ## ##      ##   ## ##   ##
            --  ####### ## ##   ####  ######  ####### #######     ##   ## ####### ##   ## ######


            SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
            busy <= '1';
            blockaddr := to_integer(unsigned(SD_block));
            fileindex := blockaddr/2048; 
            op_buffer := to_integer(unsigned(TR_buffer));


            -- retrieve a local copy of current chunk
            file_open(SDcard_file, DIRNAME & fnames(fileindex), read_mode);
            if fileindex=last_chunk_num then
                chunksize := last_chunk_size;
            else
                chunksize := 1024*1024;
            end if;
            for i in 0 to chunksize-1 loop
                read(SDcard_file, char_v);
                byte_v := character'pos(char_v);
                chunk(i) := byte_v;
            end loop;
            file_close(SDcard_file);         

            wait until falling_edge(clk);
            buffers_r <= buffers_w;
            -- transfer useful data to considered buffer
            for i in 0 to  512-1 loop
                buffers_r(op_buffer*512 + i) <= std_logic_vector(to_unsigned(chunk(i+512*(blockaddr-fileindex*2048)), 8));
            end loop;
            wait until rising_edge(clk);
            
            -- wait a bit, just for fun
            for i in 0 to 9 loop
                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                wait until rising_edge(clk);
            end loop;

            busy <= '0';

        elsif read_block = '1' and multiple = '1' then
            --  ###    ### ##    ## ##   ######## ## ######  ##      #######     ######  #######  #####  ######
            --  ####  #### ##    ## ##      ##    ## ##   ## ##      ##          ##   ## ##      ##   ## ##   ##
            --  ## #### ## ##    ## ##      ##    ## ######  ##      #####       ######  #####   ####### ##   ##
            --  ##  ##  ## ##    ## ##      ##    ## ##      ##      ##          ##   ## ##      ##   ## ##   ##
            --  ##      ##  ######  ####### ##    ## ##      ####### #######     ##   ## ####### ##   ## ######



            SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
            busy <= '1';
            blockaddr := to_integer(unsigned(SD_block));
            fileindex := blockaddr/2048; 
            op_buffer := to_integer(unsigned(TR_buffer));

            flush         := True;
            multiple_cncl := False;

            while not multiple_cncl loop
                if flush then
                    -- retrieve a local copy of current chunk
                    file_open(SDcard_file, DIRNAME & fnames(fileindex), read_mode);
                    if fileindex=last_chunk_num then
                        chunksize := last_chunk_size;
                    else
                        chunksize := 1024*1024;
                    end if;
                    for i in 0 to chunksize-1 loop
                        read(SDcard_file, char_v);
                        byte_v := character'pos(char_v);
                        chunk(i) := byte_v;
                    end loop;
                    file_close(SDcard_file);
                end if;
                flush := False;

                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                -- transfer useful data to considered buffer
                for i in 0 to  512-1 loop
                    buffers_r(op_buffer*512 + i) <= std_logic_vector(to_unsigned(chunk(i+512*(blockaddr-fileindex*2048)), 8));
                end loop;
                wait until rising_edge(clk);
                multiple_cncl := multiple_cncl or multiple = '0';
            
                -- wait a bit, just for fun
                for i in 0 to 9 loop
                    wait until falling_edge(clk);
                    buffers_r <= buffers_w;
                    wait until rising_edge(clk);
                    multiple_cncl := multiple_cncl or multiple = '0';
                    SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
                end loop;

                busy <= '0';
                
                -- wait for next read command
                while read_block = '0' and not multiple_cncl loop
                    wait until falling_edge(clk);
                    buffers_r <= buffers_w;
                    wait until rising_edge(clk);
                    multiple_cncl := multiple_cncl or multiple = '0';                    
                end loop;
                
                if read_block = '1' and not multiple_cncl then
                    SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
                    busy      <= '1';
                    op_buffer := to_integer(unsigned(TR_buffer));
                    blockaddr := blockaddr +1;
                    if fileindex /= blockaddr*2048 then
                        fileindex := blockaddr/2048; 
                        flush     := True;
                    end if;
                end if;
                
            end loop;


        elsif write_block = '1' and multiple = '0' then
            --  ####### ## ###    ##  ######  ##      #######     ##     ## ######  ## ######## #######
            --  ##      ## ####   ## ##       ##      ##          ##     ## ##   ## ##    ##    ##
            --  ####### ## ## ##  ## ##   ### ##      #####       ##  #  ## ######  ##    ##    #####
            --       ## ## ##  ## ## ##    ## ##      ##          ## ### ## ##   ## ##    ##    ##
            --  ####### ## ##   ####  ######  ####### #######      ### ###  ##   ## ##    ##    #######


            SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
            busy <= '1';
            blockaddr := to_integer(unsigned(SD_block));
            fileindex := blockaddr/2048; 
            op_buffer := to_integer(unsigned(TR_buffer));


            -- retrieve a local copy of current chunk
            file_open(SDcard_file, DIRNAME & fnames(fileindex), read_mode);
            if fileindex=last_chunk_num then
                chunksize := last_chunk_size;
            else
                chunksize := 1024*1024;
            end if;
            for i in 0 to chunksize-1 loop
                read(SDcard_file, char_v);
                byte_v := character'pos(char_v);
                chunk(i) := byte_v;
            end loop;
            file_close(SDcard_file);         

            -- update current chunk with data from buffer
            for i in 0 to  512-1 loop
                chunk(i+512*(blockaddr-fileindex*2048)) := to_integer(unsigned(buffers_r(op_buffer*512 + i)));
            end loop;

            -- rewrite updated chunk
            file_open(SDcard_file, DIRNAME & fnames(fileindex), write_mode);
            if fileindex=last_chunk_num then
                chunksize := last_chunk_size;
            else
                chunksize := 1024*1024;
            end if;
            for i in 0 to chunksize-1 loop
                byte_v := chunk(i);
                char_v := character'val(byte_v);
                write(SDcard_file, char_v);
            end loop;
            file_close(SDcard_file);         

            -- wait a bit
            for i in 0 to 9 loop
                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                wait until rising_edge(clk);
            end loop;
            busy <= '0';

        elsif write_block = '1' and multiple = '1' then
            --  ###    ### ##    ## ##   ######## ## ######  ##      #######     ##     ## ######  ## ######## #######
            --  ####  #### ##    ## ##      ##    ## ##   ## ##      ##          ##     ## ##   ## ##    ##    ##
            --  ## #### ## ##    ## ##      ##    ## ######  ##      #####       ##  #  ## ######  ##    ##    #####
            --  ##  ##  ## ##    ## ##      ##    ## ##      ##      ##          ## ### ## ##   ## ##    ##    ##
            --  ##      ##  ######  ####### ##    ## ##      ####### #######      ### ###  ##   ## ##    ##    #######



            SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
            busy <= '1';
            blockaddr := to_integer(unsigned(SD_block));
            fileindex := blockaddr/2048; 
            op_buffer := to_integer(unsigned(TR_buffer));

            flush         := True;
            multiple_cncl := False;

            while not multiple_cncl loop
                if flush then
                    -- retrieve a local copy of current chunk
                    file_open(SDcard_file, DIRNAME & fnames(fileindex), read_mode);
                    if fileindex=last_chunk_num then
                        chunksize := last_chunk_size;
                    else
                        chunksize := 1024*1024;
                    end if;
                    for i in 0 to chunksize-1 loop
                        read(SDcard_file, char_v);
                        byte_v := character'pos(char_v);
                        chunk(i) := byte_v;
                    end loop;
                    file_close(SDcard_file);
                end if;

                -- update current chunk with data from buffer
                for i in 0 to  512-1 loop
                    chunk(i+512*(blockaddr-fileindex*2048)) := to_integer(unsigned(buffers_r(op_buffer*512 + i)));
                end loop;

                -- wait a bit, just for fun
                for i in 0 to 9 loop
                    wait until falling_edge(clk);
                    buffers_r <= buffers_w;
                    wait until rising_edge(clk);
                    multiple_cncl := multiple_cncl or multiple = '0';
                end loop;

                busy <= '0';
                
                -- wait for next write command
                while write_block = '0' and not multiple_cncl loop
                    wait until falling_edge(clk);
                    buffers_r <= buffers_w;
                    wait until rising_edge(clk);
                    multiple_cncl := multiple_cncl or multiple = '0';                    
                end loop;
                
                if write_block = '1' and not multiple_cncl then
                    SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
                    busy      <= '1';
                    op_buffer := to_integer(unsigned(TR_buffer));
                    blockaddr := blockaddr +1;
                    if fileindex /= blockaddr*2048 then
                        flush     := True;
                    end if;
                end if;

                if flush or multiple_cncl then
                    -- rewrite updated chunk
                    file_open(SDcard_file, DIRNAME & fnames(fileindex), write_mode);
                    if fileindex=last_chunk_num then
                        chunksize := last_chunk_size;
                    else
                        chunksize := 1024*1024;
                    end if;
                    for i in 0 to chunksize-1 loop
                        byte_v := chunk(i);
                        char_v := character'val(byte_v);
                        write(SDcard_file, char_v);
                    end loop;
                    file_close(SDcard_file);         
                    fileindex := blockaddr/2048; 
                end if;

            end loop;

        elsif erase_block = '1' and multiple = '0' then
            --  ####### ## ###    ##  ######  ##      #######     ####### ######   #####  ####### #######
            --  ##      ## ####   ## ##       ##      ##          ##      ##   ## ##   ## ##      ##
            --  ####### ## ## ##  ## ##   ### ##      #####       #####   ######  ####### ####### #####
            --       ## ## ##  ## ## ##    ## ##      ##          ##      ##   ## ##   ##      ## ##
            --  ####### ## ##   ####  ######  ####### #######     ####### ##   ## ##   ## ####### #######



            SD_DAT <= SD_DAT(2 downto 0) & SD_DAT(3);
            busy <= '1';
            blockaddr := to_integer(unsigned(SD_block));
            fileindex := blockaddr/2048; 


            -- retrieve a local copy of current chunk
            file_open(SDcard_file, DIRNAME & fnames(fileindex), read_mode);
            if fileindex=last_chunk_num then
                chunksize := last_chunk_size;
            else
                chunksize := 1024*1024;
            end if;
            for i in 0 to chunksize-1 loop
                read(SDcard_file, char_v);
                byte_v := character'pos(char_v);
                chunk(i) := byte_v;
            end loop;
            file_close(SDcard_file);         

            -- update current chunk with data from buffer
            for i in 0 to  512-1 loop
                chunk(i+512*(blockaddr-fileindex*2048)) := 255;
            end loop;

            -- rewrite updated chunk
            file_open(SDcard_file, DIRNAME & fnames(fileindex), write_mode);
            if fileindex=last_chunk_num then
                chunksize := last_chunk_size;
            else
                chunksize := 1024*1024;
            end if;
            for i in 0 to chunksize-1 loop
                byte_v := chunk(i);
                char_v := character'val(byte_v);
                write(SDcard_file, char_v);
            end loop;
            file_close(SDcard_file);         

            -- wait a bit
            for i in 0 to 9 loop
                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                wait until rising_edge(clk);
            end loop;
            busy <= '0';

        elsif erase_block = '1' and multiple ='1' then
            --  ###    ### ##    ## ##   ######## ## ######  ##      #######     ####### ######   #####  ####### #######
            --  ####  #### ##    ## ##      ##    ## ##   ## ##      ##          ##      ##   ## ##   ## ##      ##
            --  ## #### ## ##    ## ##      ##    ## ######  ##      #####       #####   ######  ####### ####### #####
            --  ##  ##  ## ##    ## ##      ##    ## ##      ##      ##          ##      ##   ## ##   ##      ## ##
            --  ##      ##  ######  ####### ##    ## ##      ####### #######     ####### ##   ## ##   ## ####### #######

            SD_DAT        <= SD_DAT(2 downto 0) & SD_DAT(3);
            busy          <= '1';
            blockaddr     := to_integer(unsigned(SD_block));
            multiple_cncl := False;

            -- wait until erase_block is cleared (TODO: not fully complient with original behavior)
            while (erase_block = '1') and not multiple_cncl loop
                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                wait until rising_edge(clk);
                multiple_cncl := multiple_cncl or (multiple = '0');
            end loop;

            -- wait a bit more
            for i in 0 to 2 loop
                wait until falling_edge(clk);
                buffers_r <= buffers_w;
                wait until rising_edge(clk);
                multiple_cncl := multiple_cncl or (multiple = '0');
            end loop;

            -- simulate end of erase_start operation, and wait for end block
            busy          <= '0';
            if not multiple_cncl then
                while (erase_block = '0') and not multiple_cncl loop
                    wait until falling_edge(clk);
                    buffers_r <= buffers_w;
                    wait until rising_edge(clk);
                    multiple_cncl := multiple_cncl or (multiple = '0');
                end loop;
                SD_DAT        <= SD_DAT(2 downto 0) & SD_DAT(3);
                busy          <= '1';
                blockaddr2    := to_integer(unsigned(SD_block));
            end if;

            
            if not multiple_cncl then
                -- actually perform the operation
                
                fileindex     := blockaddr/2048; 
                flush         := True;
                while (blockaddr <= blockaddr2) loop
                    
                    if flush then
                        -- retrieve a local copy of chunk to work on
                        file_open(SDcard_file, DIRNAME & fnames(fileindex), read_mode);
                    if fileindex=last_chunk_num then
                        chunksize := last_chunk_size;
                    else
                        chunksize := 1024*1024;
                    end if;
                    for i in 0 to chunksize-1 loop
                            read(SDcard_file, char_v);
                            byte_v := character'pos(char_v);
                            chunk(i) := byte_v;
                        end loop;
                        file_close(SDcard_file);
                    end if;

                    -- update current chunk with data from buffer
                    for i in 0 to  512-1 loop
                        chunk(i+512*(blockaddr-fileindex*2048)) := 255;
                    end loop;
                    
                    blockaddr := blockaddr + 1;
                    flush     := (blockaddr/2048 /= fileindex) or (blockaddr>blockaddr2);

                    if flush then
                        -- rewrite updated chunk
                        file_open(SDcard_file, DIRNAME & fnames(fileindex), write_mode);
                        if fileindex=last_chunk_num then
                            chunksize := last_chunk_size;
                        else
                            chunksize := 1024*1024;
                        end if;
                        for i in 0 to chunksize-1 loop
                            byte_v := chunk(i);
                            char_v := character'val(byte_v);
                            write(SDcard_file, char_v);
                        end loop;
                        file_close(SDcard_file);
                        fileindex  := blockaddr/2048; 
                    end if;
                    
                end loop;

                -- wait a bit
                for i in 0 to 9 loop
                    wait until falling_edge(clk);
                    buffers_r <= buffers_w;
                    wait until rising_edge(clk);
                end loop;
            
            end if;
            
            busy <= '0';



        else
            --  ## ######  ##      #######
            --  ## ##   ## ##      ##
            --  ## ##   ## ##      #####
            --  ## ##   ## ##      ##
            --  ## ######  ####### #######

        end if;
    end process;


    user_addr <= loc_buffer & address;

--    ##    ## ####### ####### ######      ######  ##    ## ####### #######     ######  #######  #####  ######
--    ##    ## ##      ##      ##   ##     ##   ## ##    ## ##      ##          ##   ## ##      ##   ## ##   ##
--    ##    ## ####### #####   ######      ######  ##    ## #####   #####       ######  #####   ####### ##   ##
--    ##    ##      ## ##      ##   ##     ##   ## ##    ## ##      ##          ##   ## ##      ##   ## ##   ##
--     ######  ####### ####### ##   ##     ######   ######  ##      ##          ##   ## ####### ##   ## ######



    process(clk)
    begin
        if rising_edge(clk) then
            for byte in 0 to (2**WORD_SIZE_PW2)-1 loop
                if LITTLE_ENDIAN then
                    data_out(byte*8 + 7 downto byte*8) <= buffers_r((2**WORD_SIZE_PW2) * to_integer(unsigned(user_addr) + byte));
                else
                    data_out(byte*8 + 7 downto byte*8) <= buffers_r((2**WORD_SIZE_PW2) * to_integer(unsigned(user_addr) + (2**WORD_SIZE_PW2) - 1 - byte));                  
                end if;
            end loop;
        end if;
    end process;


--    ##    ## ####### ####### ######      ######  ##    ## ####### #######     ##     ## ######  ## ######## #######
--    ##    ## ##      ##      ##   ##     ##   ## ##    ## ##      ##          ##     ## ##   ## ##    ##    ##
--    ##    ## ####### #####   ######      ######  ##    ## #####   #####       ##  #  ## ######  ##    ##    #####
--    ##    ##      ## ##      ##   ##     ##   ## ##    ## ##      ##          ## ### ## ##   ## ##    ##    ##
--     ######  ####### ####### ##   ##     ######   ######  ##      ##           ### ###  ##   ## ##    ##    #######



    process(clk)
    begin
        if rising_edge(clk) then
            buffers_w <= buffers_r;
            if data_write = '1' then
                for byte in 0 to (2**WORD_SIZE_PW2)-1 loop
                    if LITTLE_ENDIAN then
                        buffers_w((2**WORD_SIZE_PW2) * to_integer(unsigned(user_addr) + byte))                          <= data_in(byte*8 + 7 downto byte*8);
                    else
                        buffers_w((2**WORD_SIZE_PW2) * to_integer(unsigned(user_addr) + (2**WORD_SIZE_PW2) - 1 - byte)) <= data_in(byte*8 + 7 downto byte*8);
                    end if;
                end loop;
            end if;
        end if;
    end process;



end Behavioral;

