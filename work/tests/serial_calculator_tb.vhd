library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_arith.all;
use     ieee.std_logic_misc.all;

entity SERIAL_CALCULATOR_TB is

    generic (
        F               :natural := 20_000_000;         -- czestotliwosc zegata w [Hz]
        BAUD_RATE       :natural := 2_000_000;          -- predkosc nadawania w [bodach]
        NUM_BITS        :natural := 8;                  -- liczba bitow slowa danych (5-8)
        PARITY_BITS     :natural := 1;                  -- liczba bitow parzystosci (0-1)
        STOP_BITS       :natural := 2;                  -- liczba bitow stopu (1-2)
        MAX_DIGITS      :natural := 3;                  -- liczba cyfr dziesietnych
        MAX_ARGS        :natural := 6                   -- max liczba argumentow
    );
  
end SERIAL_CALCULATOR_TB;

architecture behavioural of SERIAL_CALCULATOR_TB is

    signal   R          :std_logic;
    signal   C          :std_logic;
    signal   RX         :std_logic;
    signal   RX_R       :std_logic;
    signal   TX         :std_logic;
    signal   TX_R       :std_logic;
  
    constant T          :time := 1 sec / F;
    constant BIT_T      :time := 1 sec / BAUD_RATE;

    constant REQUEST    :string := "10 - 20 - 30 =";
    signal   RESULT     :string(REQUEST'length + MAX_DIGITS downto 1);
 
begin

    RX      <= not RX_R;
    TX_R    <= not TX;

    process is
    begin
        C <= '1';
        wait for T / 2;
        C <= '0';
        wait for T / 2;
    end process;
  
    process is
        variable BYTE :std_logic_vector(NUM_BITS - 1 downto 0);
    begin
        R       <= '1';
        RX_R    <= '0';
        BYTE    := (others => '0');
        
        wait for 10 ns;
        
        R       <= '0';
    
        for i in 1 to REQUEST'length loop
            wait for 10 * BIT_T;
            BYTE    := CONV_STD_LOGIC_VECTOR(character'pos(REQUEST(i)), BYTE'length);
            RX_R    <= '1';
            wait for BIT_T;
      
            for i in 0 to NUM_BITS - 1 loop
                RX_R <= BYTE(i);
                wait for BIT_T;
            end loop;
            
            if (PARITY_BITS = 1) then
                RX_R <= XOR_REDUCE(BYTE);
                wait for BIT_T;
            end if;
            
            for i in 0 to STOP_BITS - 1 loop
                RX_R <= '0';
                wait for BIT_T;
            end loop;
        end loop;
        
        wait;
    end process;
  
    serial_sum_inst: entity work.SERIAL_CALCULATOR(behavioural)
        generic map (
            F               => F,
            BAUD_RATE       => BAUD_RATE,
            NUM_BITS        => NUM_BITS,
            PARITY_BITS     => PARITY_BITS,
            STOP_BITS       => STOP_BITS,
            MAX_DIGITS      => MAX_DIGITS,
            MAX_ARGS        => MAX_ARGS
        )
        port map (
            R               => R,
            C               => C,
            RX              => RX,
            TX              => TX
        );

    process is
        variable BYTE   : std_logic_vector(NUM_BITS - 1 downto 0);
        variable error  : boolean;
    begin
        BYTE    := (others => '0');
        RESULT  <= (others => ' ');
    
        loop
            error := FALSE;
            
            wait until TX_R = '1';
            wait for BIT_T / 2;
            
            if (TX_R /= '1') then
                error := TRUE;
            end if;
            
            wait for BIT_T;
      
            for i in 0 to NUM_BITS - 1 loop
                BYTE(BYTE'left - 1 downto 0)    := BYTE(BYTE'left downto 1);
                BYTE(BYTE'left)                 := TX_R;
                wait for BIT_T;
            end loop;
      
            if (PARITY_BITS = 1) then
                if (TX_R /= XOR_REDUCE(BYTE)) then
                    error := TRUE;
                end if;
                
                wait for BIT_T;
            end if;
        
            for i in 0 to STOP_BITS - 1 loop
                if (TX_R /= '0') then
                    error := TRUE;
                end if;
            end loop;
        
            RESULT(RESULT'left downto 2)    <= RESULT(RESULT'left - 1 downto 1);
            RESULT(1)                       <= character'val(CONV_INTEGER(BYTE));
      
            if (error = TRUE) then
                RESULT(1) <= '?';
            end if;
        end loop;
    end process;

end behavioural;

