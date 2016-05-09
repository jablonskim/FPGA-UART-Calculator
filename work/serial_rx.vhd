library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity SERIAL_RX is

    generic (
        F               :natural := 20_000_000;                         -- czestotliwosc zegata w [Hz]
        BAUD_RATE       :natural := 9600;                               -- predkosc nadawania w [bodach]
        NUM_BITS        :natural := 8;                                  -- liczba bitow slowa danych (5-8)
        PARITY_BITS     :natural := 1;                                  -- liczba bitow parzystosci (0-1)
        STOP_BITS       :natural := 2                                   -- liczba bitow stopu (1-2)
    );
    
    port (
        R       :in  std_logic;                                         -- sygnal resetowania
        C       :in  std_logic;                                         -- zegar taktujacy
        RX      :in  std_logic;                                         -- odebrany sygnal szeregowy
        BYTE    :out std_logic_vector(NUM_BITS - 1 downto 0);           -- odebrane slowo danych
        READY   :out std_logic;                                         -- flaga potwierdzenia odbioru
        ERROR   :out std_logic                                          -- flaga wykrycia bledu w odbiorze
    );
    
end SERIAL_RX;

architecture behavioural of SERIAL_RX is

    signal   input      :std_logic_vector(0 to 1);                      -- podwojny rejestr sygnalu RX

    type     STATES     is (WAITING, START, DATA, PARITY, STOP);        -- lista etapow pracy odbiornika
    signal   state      :STATES;                                        -- rejestr maszyny stanow odbiornika

    constant T          :positive := F / BAUD_RATE;                     -- czas jednego bodu - liczba taktów zegara
    signal   time_cnt   :natural range 0 to T - 1;                      -- licznik czasu jednego bodu
    signal   bit_cnt    :natural range 0 to NUM_BITS - 1;               -- licznik odebranych bitow danych lub stopu
  
    signal   byte_buff  :std_logic_vector(BYTE'range);                  -- rejestr kolejno odebranych bitow danych
    signal   problem    :std_logic;                                     -- rejestr (flaga) wykrytego bledu odbioru

begin

    process (R, C) is                                                   -- proces odbiornika
    begin                                                               -- cialo procesu odbiornika

        if (R = '0') then                                               -- asynchroniczna inicjalizacja rejestrow
        
            input       <= (others => '0');                             -- wyzerowanie rejestru sygnalu RX
            state       <= WAITING;                                     -- poczatkowy stan pracy odbiornika
            time_cnt    <= 0;                                           -- wyzerowanie licznika czasu bodu
            bit_cnt     <= 0;                                           -- wyzerowanie licznika odebranych bitow
            byte_buff   <= (others => '0');                             -- wyzerowanie bufora bitow danych
            problem     <= '0';                                         -- wyzerowanie rejestru bledu odbioru           
            BYTE        <= (others => '0');                             -- wyzerowanie wyjsciowego slowa danych
            READY       <= '0';                                         -- wyzerowanie flagi potwierdzenia odbioru
            ERROR       <= '0';                                         -- wyzerowanie flagi wykrycia bledu w odbiorze

        elsif (rising_edge(C)) then                                     -- synchroniczna praca odbiornika

            READY       <= '0';                                         -- defaultowe skasowanie flagi potwierdzenia odbioru
            ERROR       <= '0';                                         -- defaultowe skasowanie flagi wykrycia bledu w odbiorze
            input(0)    <= not RX;                                      -- zarejestrowanie synchroniczne stanu sygnalu RX
            input(1)    <= input(0);                                    -- zarejestrowanie dwoch kolejnych stanow sygnalu RX

            case state is                                               -- badanie aktualnego stanu maszyny stanow 

                when WAITING =>                                         -- obsluga stanu WAITING
                    time_cnt    <= 0;                                   -- wyzerowanie licznika czasu bodu
                    bit_cnt     <= 0;                                   -- wyzerowanie licznika odebranych bitow
                    byte_buff   <= (others => '0');                     -- wyzerowanie bufora bitow danych
                    problem     <= '0';                                 -- wyzerowanie rejestru bledu odbioru
                    
                    if (input(1) = '0' and input(0) = '1') then         -- wykrycie poczatku bitu START
                        state   <= START;                               -- przejscie do stanu START
                    end if;                                             -- zakonczenie instukcji warunkowej

                when START =>                                           -- obsluga stanu START
                
                    if (time_cnt /= T / 2 - 1) then                     -- badanie odliczania okresu T/2
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T/2
                        time_cnt <= 0;                                  -- wyzerowanie licznika czasu bodu
                        state    <= DATA;                               -- przejscie do stanu DATA
                        
                        if (input(1) = '0') then                        -- badanie nieprawidlowego stanu bitu START
                            problem <= '1';                             -- ustawienie rejestru bledu odbioru
                        end if;                                         -- zakonczenie instukcji warunkowej  
                        
                    end if;                                             -- zakonczenie instukcji warunkowej

                when DATA =>                                            -- obsluga stanu DATA
                
                    if (time_cnt /= T - 1) then                         -- badanie odliczania okresu T
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T
                        byte_buff(byte_buff'left) <= input(1);          -- zapamietanie stanu bitu danych
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
                
                    if (time_cnt /= T - 1) then                         -- badanie odliczania okresu T
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T
                        time_cnt <= 0;                                  -- wyzerowanie licznika czasu bodu
                        state    <= STOP;                               -- przejscie do stanu STOP
                        
                        if ((input(1) xor XOR_REDUCE(not byte_buff)) = '1') then    -- badanie nieprawidlowej parzystosci bitow
                            problem <= '1';                             -- ustawienie rejestru bledu odbioru
                        end if;                                         -- zakonczenie instukcji warunkowej 
                    end if;                                             -- zakonczenie instukcji warunkowej

                when STOP =>                                            -- obsluga stanu STOP
                
                    if (time_cnt /= T - 1) then                         -- badanie odliczania okresu T
                        time_cnt <= time_cnt + 1;                       -- zwiekszenie o 1 stanu licznika czasu
                    else                                                -- zakonczenie odliczania czasu T
                        time_cnt <= 0;                                  -- wyzerowanie licznika czasu bodu

                        if (bit_cnt /= STOP_BITS - 1) then              -- badanie odliczania bitow stopu
                            bit_cnt <= bit_cnt + 1;                     -- zwiekszenie o 1 liczby bitow stopu
                            
                            if (input(1) = '1') then                    -- badanie nieprawidlowego stanu bitu STOP
                                problem <= '1';                         -- ustawienie rejestru bledu odbioru
                            end if;                                     -- zakonczenie instukcji warunkowej 
                        else                                            -- zakonczenie odliczania bitow stopu
                            if (problem = '0' and input(1) = '0') then  -- badanie prawidlowego odbioru szeregowego
                                BYTE    <= not byte_buff;               -- ustawienie na wyjsciu BYTE odebranego slowa 
                                READY   <= '1';                         -- ustawienie na wyjsciu flagi potwierdzenia
                            else                                        -- wykryto nieprawidlowy odbioru szeregowy
                                BYTE    <= (others => '0');             -- wyzerowanie wyjscia danych
                                ERROR   <= '1';                         -- ustawienie na wyjsciu flagi bledu odbioru
                            end if;                                     -- zakonczenie instukcji warunkowej
                            
                            state <= WAITING;                           -- przejscie do stanu WAITING
                        end if;                                         -- zakonczenie instukcji warunkowej 

                    end if;                                             -- zakonczenie instukcji warunkowej

            end case;                                                   -- zakonczenie instukcji warunkowego wyboru

        end if;                                                         -- zakonczenie instukcji warunkowej porcesu

    end process;                                                        -- zakonczenie ciala procesu
   
end behavioural;

