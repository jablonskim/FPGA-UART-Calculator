library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity SERIAL_TX_TB is
    generic (
        F               :natural := 20_000_000;
        BAUD_RATE       :natural := 2_000_000;
        NUM_BITS        :natural := 8;
        PARITY_BITS     :natural := 1;
        STOP_BITS       :natural := 2
    );
end SERIAL_TX_TB;

architecture behavioural of SERIAL_TX_TB is

    signal   R          :std_logic;
    signal   C          :std_logic;
    signal   TX         :std_logic;
    signal   TX_R       :std_logic;
    signal   BYTE       :std_logic_vector(NUM_BITS - 1 downto 0);
    signal   SEND       :std_logic;
    signal   SENDING    :std_logic;

    signal   RECEIVED   :std_logic_vector(NUM_BITS - 1 downto 0);

    constant T          :time := 1 sec / F;

begin

    TX_R <= not TX;

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
        SEND    <= '0';
        BYTE    <= (others => '0');
        
        wait for 5.5 * T;
        
        R <= '0';
        
        loop
            wait for 5 * T;
            SEND  <= '1';
            wait for T;
            SEND  <= '0';
            wait for T;
            wait until SENDING = '0';
            BYTE <= BYTE + 7;
            wait for 0.5 * T;
        end loop;
        
    end process;
  
    serial_tx_inst: entity work.SERIAL_TX(behavioural)
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
            TX              => TX,
            BYTE            => BYTE,
            SEND            => SEND,
            SENDING         => SENDING
        );

    process is
    
        constant O_BITU : time := 1 sec / BAUD_RATE;
        variable blad   : boolean;
        
    begin
    
        RECEIVED <= (others => '0');
        
        loop
            blad := FALSE;
            wait until TX_R = '1';
            wait for O_BITU / 2;
            
            if (TX_R /= '1') then
                blad := TRUE;
            end if;
            
            wait for O_BITU;
            
            for i in 0 to NUM_BITS - 1 loop
                RECEIVED(RECEIVED'left - 1 downto 0) <= RECEIVED(RECEIVED'left downto 1);
                RECEIVED(RECEIVED'left) <= TX_R;
                wait for O_BITU;
            end loop;
            
            if (PARITY_BITS = 1) then
                if (TX_R /= XOR_REDUCE(BYTE)) then
                    blad := TRUE;
                end if;
                
                wait for O_BITU;
            end if;
            
            for i in 0 to STOP_BITS - 1 loop
                if (TX_R /= '0') then
                    blad := TRUE;
                end if;
            end loop;
            
            if (blad = TRUE) then
                RECEIVED <= (others => 'X');
            end if;
        end loop;
    end process;

end behavioural;

