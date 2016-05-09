library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity SERIAL_TX is

    generic (
        F               :natural := 20_000_000;     -- czestotliwosc zegata w [Hz]
        BAUD_RATE       :natural := 9600;           -- predkosc nadawania w [bodach]
        NUM_BITS        :natural := 8;              -- liczba bitow slowa danych (5-8)
        PARITY_BITS     :natural := 1;              -- liczba bitow parzystosci (0-1)
        STOP_BITS       :natural := 2               -- liczba bitow stopu (1-2)
    );
    
    port (
        R           :in  std_logic;                                 -- sygnal resetowania
        C           :in  std_logic;                                 -- zegar taktujacy
        TX          :out std_logic;                                 -- odebrany sygnal szeregowy
        BYTE        :in  std_logic_vector(NUM_BITS - 1 downto 0);   -- odebrane slowo danych
        SEND        :in  std_logic;                                 -- flaga zadania nadania
        SENDING     :out std_logic                                  -- flaga potwierdzenia wysylanie
    );
    
end SERIAL_TX;

architecture behavioural of SERIAL_TX is

    signal   byte_buff      :std_logic_vector(BYTE'range);              -- rejestr kolejno odebranych bitow danych
    signal   parity_flag    :std_logic;                                 -- flaga parzystosci

    type     STATES         is (WAITING, START, DATA, PARITY, STOP);    -- lista etapow pracy odbiornika
    signal   state          :STATES;                                    -- rejestr maszyny stanow odbiornika

    constant T              :positive := F / BAUD_RATE;                 -- czas jednego bodu - liczba taktów zegara
    signal   time_cnt       :natural range 0 to T - 1;                  -- licznik czasu jednego bodu
    signal   bit_cnt        :natural range 0 to NUM_BITS - 1;           -- licznik odebranych bitow danych lub stopu

begin

    process (R, C) is                                                   -- proces odbiornika
    begin

        if (R = '0') then                                               -- asynchroniczna inicjalizacja rejestrow
     
            state       <= WAITING;                                     -- poczatkowy stan pracy odbiornika
            TX          <= '1';                                         -- wyzerowanie sygnalu nadawania szeregowego
            SENDING     <= '0';                                         -- wyzerowanie flagi potwierdzenia nadania

        elsif (rising_edge(C)) then                                     -- synchroniczna praca nadajnika

            TX          <= '1';                                         -- defaultowe ustawienie sygnalu nadawania szeregowego
            SENDING     <= '1';                                         -- defaultowe ustawienie flagi potwierdzenia wysylania

            case state is                                               -- badanie aktualnego stanu maszyny stanow 

                when WAITING =>                                         -- obsluga stanu WAITING
                
                    time_cnt <= 0;                                      -- wyzerowanie licznika czasu bodu

                    if (SEND = '1') then                                -- wykrycie zadania nadawania
                        state       <= START;                           -- przejscie do stanu START
                        byte_buff   <= BYTE;                            -- zapisanie bufora bitow danych
                        parity_flag <= XOR_REDUCE(BYTE);                -- wyznaczenie flagi parzystosci
                    else
                        SENDING <= '0';                                 -- kasowanie flagi potwierdzenia wysylania
                    end if;                                             -- zakonczenie instukcji warunkowej

                when START =>                                           -- obsluga stanu START
                
                    TX <= '0';                                          -- wysylanie bitu STRAT
                    
                    if (time_cnt /= T - 1) then                         -- badanie odliczania okresu T
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T/2
                        time_cnt <= 0;                                  -- wyzerowanie licznika czasu bodu
                        bit_cnt  <= 0;
                        state    <= DATA;                               -- przejscie do stanu DATA
                    end if;                                             -- zakonczenie instukcji warunkowej

                when DATA =>                                            -- obsluga stanu DATA
                    TX <= byte_buff(0);                                 -- wysylanie najmlodszego bitu danych bufora
                    
                    if (time_cnt /= T - 1) then                         -- badanie odliczania okresu T
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T
                        byte_buff(byte_buff'left) <= '0';               -- kasowanie najstarszego bitu danych
                        byte_buff(byte_buff'left - 1 downto 0) <= byte_buff(byte_buff'left downto 1);   -- przesuniecie bitow w buforze
                        time_cnt <= 0;                                  -- wyzerowanie licznika czasu bodu
         
                        if (bit_cnt /= NUM_BITS - 1) then               -- badanie odliczania bitow danych
                            bit_cnt <= bit_cnt + 1;                     -- zwiekszenie o 1 liczby bitow danych
                        else                                            -- zakonczenie odliczania bitow danych
                            bit_cnt <= 0;                               -- wyzerowanie licznika odebranych bitow
                            
                            if (PARITY_BITS = 1) then                   -- badanie odbioru bitu parzystosci
                                state <= PARITY;                        -- przejscie do stanu PARITY
                            else                                        -- brak odbioru bitu parzystosci  
                                state <= STOP;                          -- przejscie do stanu STOP
                            end if;                                     -- zakonczenie instukcji warunkowej
                            
                        end if;                                         -- zakonczenie instukcji warunkowej 

                    end if;                                             -- zakonczenie instukcji warunkowej

                when PARITY =>                                          -- obsluga stanu PARITY
                    TX <= parity_flag;                                  -- wysylanie bitu parzystosci
                    
                    if (time_cnt /= T - 1) then                         -- badanie odliczania okresu T
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T
                        time_cnt <= 0;                                  -- wyzerowanie licznika czasu bodu
                        state    <= STOP;                               -- przejscie do stanu STOP
                    end if;                                             -- zakonczenie instukcji warunkowej

                when STOP =>                                            -- obsluga stanu STOP
                    if (time_cnt /= T - 1) then                         -- badanie odliczania okresu T
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T
                        time_cnt <= 0;                                  -- wyzerowanie licznika czasu bodu

                        if (bit_cnt /= STOP_BITS - 1) then              -- badanie odliczania bitow stopu
                            bit_cnt <= bit_cnt + 1;                     -- zwiekszenie o 1 liczby bitow stopu
                        else                                            -- zakonczenie odliczania bitow stopu
                            SENDING <= '0';                             -- kasowanie flagi potwierdzenia wysylania
                            state   <= WAITING;                         -- przejscie do stanu WAITING
                        end if;                                         -- zakonczenie instukcji warunkowej 

                    end if;                                             -- zakonczenie instukcji warunkowej

            end case;                                                   -- zakonczenie instukcji warunkowego wyboru

        end if;                                                         -- zakonczenie instukcji warunkowej porcesu

    end process;                                                        -- zakonczenie ciala procesu
   
end behavioural;

