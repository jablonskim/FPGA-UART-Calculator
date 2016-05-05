library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity SERIAL_RX_TB is

    generic (
        F               :natural := 20_000_000;
        BAUD_RATE       :natural := 2_000_000;
        NUM_BITS        :natural := 8;
        PARITY_BITS     :natural := 1;
        STOP_BITS       :natural := 2
    );
    
end SERIAL_RX_TB;

architecture behavioural of SERIAL_RX_TB is

    signal   R          :std_logic;
    signal   C          :std_logic;
    signal   RX         :std_logic;
    signal   RX_R       :std_logic;
    signal   BYTE       :std_logic_vector(NUM_BITS - 1 downto 0);
    signal   READY      :std_logic;
    signal   ERROR      :std_logic;
  
    constant T          :time := 1 sec / F;
    constant BIT_T      :time := 1 sec / BAUD_RATE;
    signal   D          :std_logic_vector(BYTE'range);
  
begin

    RX <= not RX_R;

    process is
    begin
        C <= '1';
        wait for T / 2;
        C <= '0';
        wait for T / 2;
    end process;
  
    process is
    begin
        R       <= '1';
        RX_R    <= '0';
        D       <= (others => '0');
        wait for 10 ns;
        R       <= '0';
        wait for 10 ns;
        
        loop
            RX_R <= '1';
            wait for BIT_T;
            
            for i in 0 to NUM_BITS - 1 loop
                RX_R <= D(i);
                wait for BIT_T;
            end loop;
            
            if (PARITY_BITS = 1) then
                RX_R <= XOR_REDUCE(D);
                wait for BIT_T;
            end if;
            
            for i in 0 to STOP_BITS - 1 loop
                RX_R <= '0';
                wait for BIT_T;
            end loop;
            
            D <= D + 7;
        end loop;
    end process;
  
    serial_rx_inst: entity work.SERIAL_RX(behavioural)
        generic map (
            F               => F,
            BAUD_RATE       => BAUD_RATE,
            NUM_BITS        => NUM_BITS,
            PARITY_BITS     => PARITY_BITS,
            STOP_BITS       => STOP_BITS
        )
        
        port map (
            R               => R,
            C               => C,
            RX              => RX,
            BYTE            => BYTE,
            READY           => READY,
            ERROR           => ERROR
        );

end behavioural;

