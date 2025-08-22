-- Aleksandr Hrytsevskyi
--------------------------
--			Alex Zor			--
--------------------------
-- Date:				23 May 2025 
-- Design Name:	PentaDiv II
-- Version:			B
-- Description:	ZX Spectrum (Pentagon128) compatible computer with expansion slots and built-in DivMMC based on Mario Prato project (https://github.com/mprato/DivMMC)

library IEEE;
library STD;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity zx is
	port (
	----CPU----
		D_CPU		: inout STD_LOGIC_VECTOR(7 downto 0); 	-- CPU data bus
		A			: in STD_LOGIC_VECTOR(15 downto 0);		-- CPU address bus
		MREQ		: in STD_LOGIC;
		IORQ		: in STD_LOGIC;
		RD			: in STD_LOGIC;
		WR			: in STD_LOGIC;
		M1			: in STD_LOGIC;
		RFSH		: in STD_LOGIC;
		RESET		: in STD_LOGIC;
		NMI		: out STD_LOGIC;
		INT 		: out STD_LOGIC;
		CPUCLK 	: out STD_LOGIC;
	----BUTTONS----
		CONF1		: in STD_LOGIC;			-- 1024kb
		CONF2		: in STD_LOGIC;			-- 512kb
		TURBO		: in STD_LOGIC;			-- turbo mode jumper
		S2X		: in STD_LOGIC;			-- 2X speed mode jumper
		NMI_BUT	: in STD_LOGIC;			-- NMI button
		EPROM    : in STD_LOGIC;			-- eprom jumper (DIV on)
		AD1		: in STD_LOGIC;
		AD2		: in STD_LOGIC;
		AD3		: in STD_LOGIC;
	----ROM----	
		ROMOE		: out STD_LOGIC;
		A14_ROM	: out STD_LOGIC;
		A15_ROM	: inout STD_LOGIC;
		A17_ROM	: out STD_LOGIC;
	----RAM----
		D_RAM		: inout STD_LOGIC_VECTOR(7 downto 0);
		A_RAM		: out STD_LOGIC_VECTOR(18 downto 0);
		RAMBNK	: out STD_LOGIC_VECTOR(1 downto 0); 
		RAMOE		: out STD_LOGIC;
		RAMWE		: out STD_LOGIC;
	----PORT I/O----
		TAPE_OUT	: out STD_LOGIC;		-- tape out
		BEEPER	: out STD_LOGIC;		-- beep out
		RD_FE		: out STD_LOGIC;		-- port FE read
		RD_1F		: out STD_LOGIC;		-- Kempston joysk port
		BC1		: out STD_LOGIC;		-- AY/YM control
		BDIR		: out STD_LOGIC;
	----VIDEO----
		SYNC 		: out STD_LOGIC;			-- video sync out
		BRGI		: out STD_LOGIC_VECTOR(3 downto 0);	-- video out blue/red/green/intensity
	----CLOCKS----
		CLKIN		: in STD_LOGIC;		-- in 14 Mhz
		CLK7		: out STD_LOGIC;		-- out 7 Mhz
		CLK1_75	: out STD_LOGIC;		-- out 1.75 Mhz
	----NEMO----
		CSROM		: out STD_LOGIC;		-- ROM CS out (nemo bus)
		CSROMCE	: in STD_LOGIC;		-- internal or external ROM select (nemo bus)
		IORQGE1	: in STD_LOGIC;		-- nemo bus #1
		IORQGE2	: in STD_LOGIC;		-- nemo bus #2
		IORQ1		: out STD_LOGIC;		-- iorq out for nemo bus #2
	----DIVMMC----
		DIVCS    : out  STD_LOGIC;		-- 1 -> page out spectrum rom
		DIVOE    : out STD_LOGIC;		-- eeprom oe pin
		DIVWE    : out STD_LOGIC;		-- eeprom wr pin
		SD_CS    : out STD_LOGIC;		-- SD card CS 
		SD_CLK   : out STD_LOGIC;		-- SD card clock
		SD_MOSI  : out STD_LOGIC;		-- SD card master out 
		SD_MISO  : in STD_LOGIC;		-- SD card master input		  
		PWRON		: in  STD_LOGIC		-- low pulse on poweron
	);
end zx;

architecture zxpent of zx is
	signal clkdiv		: STD_LOGIC_VECTOR (3 downto 0); 	-- 7, 3.5, 1.75, 0.875 MHz								
	signal clkram		: STD_LOGIC ;								-- ram and sift register clock
	
	signal va_h			: STD_LOGIC_VECTOR (5 downto 0);		-- horizontal address
	signal va_v			: STD_LOGIC_VECTOR (8 downto 0);		-- vertical address
	signal vidaddr		: STD_LOGIC_VECTOR (12 downto 0);
		
	signal regpix		: STD_LOGIC_VECTOR (7 downto 0);		-- ram to buff pixel data	
	signal regatt		: STD_LOGIC_VECTOR (7 downto 0);		-- ram to buff attr data	
	signal pix			: STD_LOGIC_VECTOR (7 downto 0);		-- pixel data
	signal attr			: STD_LOGIC_VECTOR (7 downto 0);		-- attr data
	signal flash		: STD_LOGIC_VECTOR (4 downto 0);		-- Flash timer
	signal pixBRGI		: STD_LOGIC_VECTOR (3 downto 0);		-- screen data
	signal blank		: STD_LOGIC;								-- vblank and hblank
	signal screen		: STD_LOGIC;								-- border/screen	
	
	signal cpummcrq	: STD_LOGIC;								-- mmc read request
	signal cpumemrq	: STD_LOGIC;								-- reques ram
	signal ramreg		: STD_LOGIC_VECTOR (7 downto 0);		-- RAM buffer
	signal ramaddr		: STD_LOGIC_VECTOR (19 downto 0);	-- cpu/video addresses

	signal cpu 			: STD_LOGIC;								-- cpu or video ram access
	signal cputr 		: STD_LOGIC;								

	signal iowr			: STD_LOGIC;								-- IO write
	signal iord			: STD_LOGIC;								-- IO read
	signal outFE		: STD_LOGIC_VECTOR (7 downto 0);		-- port FE reg
	signal out7FFD		: STD_LOGIC_VECTOR (7 downto 0);		-- port 7FFD
	
	signal turbomode	: STD_LOGIC;	-- 7MHz clock, 50Hz int
	signal s2xmode		: STD_LOGIC;	-- 7MHz clock, 100Hz int 

	signal iorqx		: STD_LOGIC;	-- after nemo bus
	signal romsel	: STD_LOGIC;	-- rom sel

	signal rst			: STD_LOGIC;	-- internal reset signal
	signal rsten 		: STD_LOGIC;
	signal rstdel 		: STD_LOGIC_VECTOR(1 downto 0);

	signal dosen 		: STD_LOGIC;

--------------------DIVMMC---------------
	signal zxmmcio : std_logic;
	signal divideio : std_logic;
	signal bank     : std_logic_vector (5 downto 0);
	signal mapcond  : std_logic;
	signal conmem   : std_logic;
	signal mapram   : std_logic;
	signal automap  : std_logic;
	signal bank3    : std_logic;
	signal bankout  : STD_LOGIC_VECTOR (5 downto 0);	--ram bank no.
	signal divramrd : STD_LOGIC;		-- div ram read
	signal divramwr : STD_LOGIC;		-- div ram write
	signal divromoe : std_LOGIC;		-- div rom OE
	signal divromcs : std_logic;		-- div rom CS

	-- Transmission states
	type transStates is (
		IDLE, 			-- Wait for a WR or RD request on port 0xEB
		SAMPLE, 			-- As there is an I/O request, prepare the transmission; sample the CPU databus if required
		TRANSMIT); 		-- Transmission (SEND or RECEIVE)
	signal transState : transStates := IDLE; -- Transmission state (initially IDLE)
	
	signal TState : unsigned(3 downto 0) := (others => '0');		-- Counts the T-States during transmission
	
	signal fromSDByte : std_logic_vector(7 downto 0);	-- Byte received from SD
	signal toSDByte : std_logic_vector(7 downto 0);		-- Byte to send to SD
	signal toCPUByte : std_logic_vector(7 downto 0);	-- Byte seen by the CPU after a byte read
 
	constant divmmc_control_port : std_logic_vector(7 downto 0) := x"E3";	-- bit 7 = conmem = 1 to map in divmmc, 0K-8K will contain the esxdos rom, 8K-16K will contain the selected divmmc bank
																									-- bit 6 = mapram = 1 to replace the esxdos rom with divmmc bank 3
																									-- bits 3:0 = bank = selected divmmc ram bank for 8K-16K region
																									-- conmen can be used to manually control divmmc mapping.
																									-- divmmc automatically maps itself in when instruction fetches hit specific addresses in the rom. When this happens, the esxdos rom (or divmmc bank 3 if mapram is set) appears in 0K-8K and the selected divmmc bank appears as ram in 8K-16K.
																									-- bit 6 can only be set, once set only a power cycle can reset it on the original divmmc.
	constant zxmmc_control_port : std_logic_vector(7 downto 0) := x"E7";	-- era la porta 31 nella zxmmc+
	constant zxmmc_spi_port : std_logic_vector(7 downto 0) := x"EB";	-- era la porta 63 nella zxmmc+
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
begin
	CLK7 <= clkdiv(0);
	CLK1_75 <= clkdiv(2);
	CPUCLK <= not(clkdiv(0)) when turbomode = '1'  else not(clkdiv(1));	-- 7mhz when turbo or 2x speed
	
--------------------VIDEO-------------------------------
	SYNC <= '0' when ((va_h >= 41) and (va_h <= 44)) or ((va_v >= 240) and (va_v <= 255)) else '1';
	screen <= '1' when ((va_h >= 1) and (va_h <= 32) and (va_v <= 191)) else '0';
	blank <= '0' when ((va_v >= 240) and (va_v <= 260)) or ((va_h >= 41) and (va_h <= 48)) else '1';
	
	vidaddr <= va_v(7 downto 6) &  va_v(2 downto 0) & va_v(5 downto 3) & va_h(4 downto 0)
			when clkdiv(3) = '0' 
			else "110" & va_v(7 downto 3) & va_h(4 downto 0) ;	-- Pix/Attr
		
	pixbrgi <= attr(6) & attr(2 downto 0) when (pix(7) xor (attr(7) and flash(4))) = '1' else attr(6) & attr(5 downto 3);
	
--------------------INT---------------------------------
	INT <= '0' when ((va_h >= 40) and (va_h <= '1' & not(turbomode) & turbomode & '0' & turbomode &  turbomode) and (va_v = 239 or va_v = s2xmode & s2xmode & "1001111")) else 'Z';

-----------------------ROM--------------------	  
	romsel <= '0' when RD = '0' and MREQ = '0' and RFSH = '1' and ((A(14) or A(15) or divromcs) = '0' or divromoe = '0') else '1'; -- ROM OE
	CSROM <= romsel and not divromcs;	-- out for nemobus
	ROMOE <= CSROMCE or romsel;
	A14_ROM <= out7FFD(4) and divromoe;			-- 128k/48k ROM
	A15_ROM <= '0' when dosen ='0' and AD1 = '0' and EPROM = '1' else 'Z';	-- TR-DOS page
	A17_ROM <= AD2;		-- alternative ROM bank
	
-----------------------RAM--------------------
	cpumemrq <= '1' when MREQ = '0' and RFSH = '1' and ((A(14) or A(15)) = '1' or (divramrd and divramwr) = '0') else '0'; -- request cpu to ram
	RAMOE <= '1' when cpu = '1' and RD = '1' else '0';
	RAMWE <= '0' when cpu = '1' and RD = '1' and (clkdiv(0) or clkram) = '0' else '1';
	D_RAM <= D_CPU when cpu = '1' and RD = '1' else (others => 'Z');  -- cpu to ram
	
	D_CPU <=	ramreg		when RD = '0' and cpumemrq = '1' else		-- ram or mmc data to cpu
				toCPUByte 	when RD = '0' and cpummcrq = '1' else
				(others => 'Z'); 
	
	ramaddr(13 downto 0) <= '0' & vidaddr when cpu = '0' else  A(13 downto 0); 	   --address to RAM
	-- 128k memory
	ramaddr(14) <= out7FFD(0) when (A(14) and A(15)) = '1' and cpu = '1' else A(14) or not(cpu);
	ramaddr(15) <= out7FFD(1) when (A(14) and A(15)) = '1' and cpu = '1' else (A(15) and cpu) or (out7FFD(3) and not(cpu));
	ramaddr(16) <= out7FFD(2) when (A(14) and A(15)) = '1' and cpu = '1' else A(14) or not(cpu);
	-- 256k memory
	ramaddr(17) <= out7FFD(6) when (A(14) and A(15)) = '1' and cpu = '1' and (CONF1 and CONF2) = '0' else '0';
	-- 512k memory
	ramaddr(18) <= out7FFD(7) when (A(14) and A(15)) = '1' and cpu = '1' and (CONF1 and CONF2) = '0' else '0';
	-- 1024k
	ramaddr(19) <= out7FFD(5) when (A(14) and A(15)) = '1' and cpu = '1' and CONF1 = '0' else '0';

	A_RAM <= (CONF2 or bankout(5)) & bankout(4 downto 0) & ramaddr(12 downto 0) when (divramrd and divramwr) = '0' and cpu = '1' else ramaddr(18 downto 0);	-- divmmc/cpu/video to ram addres bus
	RAMBNK <= CONF2 & not CONF2 when (divramrd and divramwr) = '0' and cpu = '1' else not(ramaddr(19)) & ramaddr(19); 
	
-----------------------IO----------------------
	IORQ1 <= IORQ or IORQGE1;
	iorqx <= IORQ or IORQGE1 or IORQGE2;	-- nemo bus iorq priority
		
	iowr <= iorqx or WR or not(M1) or clkdiv(1);	 
	iord <= iorqx or RD or not(M1);
	
	RD_FE <= iord or A(0);		-- FE read
	RD_1F <= iord or A(5);		-- Kempston joystik (A5 or A5,6,7)
	
	BC1 <= not(iorqx or A(1) or not(A(14) and A(15) and M1)); -- AY/YM control
	BDIR <= not(iorqx or WR or A(1) or not A(15));
	
	BEEPER <= outFE(4);			-- beeper out
	TAPE_OUT <= outFE(3);		-- tape out
	
	process (iowr, rst)
	begin
		if rst = '0' then
			out7FFD <= (others => '0');		-- reset 7FFD
		elsif falling_edge (iowr)  then
			if A(0) = '0' then					-- port FE
				outFE <= D_CPU;
			end if;
	
			if (A(1) or A(15) or (out7FFD(5) and CONF1)) = '0' then		-- port 7FFD with lock bit (pentagon without A14)
				out7FFD <= D_CPU;
			end if;
		end if;
	end process;
-----------------------------------------------------------
process (CLKIN)
begin
	if FALLING_EDGE(CLKIN) then
		clkdiv <= clkdiv + 1;				-- 14MHz divider by 2/4/8/16
	end if;
	if RISING_EDGE(CLKIN) then
		clkram <= clkdiv(0);
	end if;
end process;

process (clkdiv(3))							-- horisontal and vertical counters 875kHz
begin
	if falling_edge(clkdiv(3)) then
		if va_h = 55 then						-- 56 chars (32 visible + 24 border and sync) 
			va_h <= (others => '0');		-- H-counter res
			if va_v = 319 then				-- 320 (256 visible + 64 border and sync) video lines (PENTAGON 128)
				va_v <= (others => '0');	-- V-counter res
				flash <= flash + 1;			-- flash = 1.5 Hz
			else
				va_v <= va_v + 1;				-- inc lines counter
			end if;
		else
			va_h <= va_h + 1;					-- inc chars counter
		end if;
	end if;
end process;

process (clkram)								-- shift pixels 7Mhz
begin
	if FALLING_EDGE(clkram) then
		if (cpu = '1') then
			if (RD = '0') then
				ramreg <= D_RAM;				-- load data to buff
			end if;
		else
			if (clkdiv(3) = '0') then
				regpix <= D_RAM;				-- load pix data to buff
			else
				regatt <= D_RAM;				-- load attr data to buff
			end if;
		end if;
		
		if (clkdiv(3 downto 1) = "111") then		-- load pix	and attributes data to out register
			pix <= regpix;
			attr <= regatt;
		else
			pix <= pix(6 downto 0) & '0';	-- shift pixel data
		end if;
					
		if screen = '1' then					-- paper/border/blank
			BRGI <= pixbrgi;					-- pixel data out
		else
			BRGI <= '0' & (outFE(2 downto 0) and (blank & blank & blank));		-- border data out
		end if;
	end if;
end process;

process (clkdiv(0))
begin
	if RISING_EDGE (clkdiv(0)) then
		if (cpumemrq = '0') then
			cputr <= '0';
		elsif (cputr = '0') then
			cpu <= '1';
			cputr <= '1';
		else
			cpu <= '0';
		end if;
	end if;
end process;

process (TURBO, S2X)							-- turbomode enable
begin
	if va_v = 238 and va_h = 0 then		-- change ststus one time of frame
		turbomode <= not(TURBO) or not(S2X);
		s2xmode <= S2X;
	end if;		
end process;
		
process (va_h(1))			-- reset delay
begin
	if RESET = '0' and rsten = '1' then
		rst <= '0';
		rsten <= '0';
		rstdel <= not flash(4) & flash(3);
	else
		rst <= '1';
		if rstdel = flash(4 downto 3) then
			rsten <= '1';
		end if;
	end if;
end process;

-----------------------------------DIVMMC-------------------------------
 	bank3 <= '1' when bank ="000011" else '0';

	-- ROM RAM read write signals
	divromoe <= not(CONF1) or A(15) or A(14) or A(13) or (not conmem and mapram) or (not conmem and not automap) or (not conmem and  eprom);
	DIVOE <= CSROMCE or divromoe or MREQ or RD;
	
	DIVWE <= '0' when MREQ = '0' and WR ='0' and A(13)='0' and A(14)='0' and A(15)='0' and eprom='1' and conmem='1' else '1';
	
	divramrd <= not(CONF1) or CSROMCE or A(15) or A(14) or ( not A(13) and not mapram) or (not A(13) and conmem) or (not conmem and not automap) or (not conmem and  eprom and not mapram);
   divramwr <= not(CONF1) or CSROMCE or A(15) or A(14) or not A(13) or (not conmem and mapram and bank3 ) or (not conmem and not automap) or (not conmem and  eprom and not mapram);
	
	divromcs <= '1' when CONF1 = '1' and ((automap and not eprom) or (automap and mapram) or conmem ) = '1' else '0' ;
	DIVCS <= divromcs;
	
	-- Divide Automapping logic			  
	process(MREQ, rst)
	begin
		if rst = '0' then		-- reset on ROM bank #0
			dosen <= '0';
		elsif falling_edge(MREQ) then
		   if M1 = '0' then
				if A = x"0000" or A = x"0008" or A = x"0038" or A = x"0066" or A = x"04c6" or A = x"0562" then
					mapcond <= '1';
				end if;
				
				if A(15 downto 8) = "00111101"  then	-- map 3DXX
					automap <= '1';
					mapcond <= '1';
					dosen <= not out7FFD(4);		-- dos on
				end if;
				
				if A(15 downto 3) = "0001111111111" then	-- map 1FF8 - 1FFF
					mapcond <= '0';
				end if;
				
				if (A(15) or A(14)) = '1' then	-- dos off
					dosen <= '1';
				end if;	
			else
				automap <= mapcond;
		  end if;
		end if;	  
	end process;

	NMI <= '0' when (automap or NMI_BUT) = '0' else 'Z';
	
-------------- divide control port------------------------												
	divideio <='0' when iowr = '0' and A(7 downto 0) = divmmc_control_port else '1';  	
	
	process(divideio, rst)
	begin
--		if PWRON ='0' then						-- originally by M.Prato
		if rst = '0' then
			bank   <= "000000";
			mapram <= '0';
			conmem <= '0';
		elsif	rising_edge(divideio) then
			bank(5 downto 0) <= D_CPU(5 downto 0);
			mapram <= D_CPU(6) or mapram;
		   conmem <= D_CPU(7);
		end if;
	end process;	
			
	-- ram banks 
	bankout(0) <= bank(0) or not A(13);
	bankout(1) <= bank(1) or not A(13);
	bankout(2) <= bank(2) and    A(13);
	bankout(3) <= bank(3) and    A(13);
	bankout(4) <= bank(4) and    A(13);
	bankout(5) <= bank(5) and    A(13);

	-- SD CS signal management
	zxmmcio <= '0' when iowr = '0' and A(7 downto 0) = zxmmc_control_port else '1';

	process(rst, zxmmcio)
	begin
		if rst = '0' then
			SD_CS <= '1';
		elsif falling_edge(zxmmcio) then
			SD_CS <= D_CPU(0);
		end if;
	end process;

-- spi transmission/reception

	-- Update transmission state
	process(clkdiv(0), rst)
	begin
		if rst = '0' then
			transState <= IDLE;
			TState <= (others => '0');
			fromSDByte <= (others => '1');
			toSDByte <= (others => '1');
			toCPUByte <= (others => '1');

		elsif falling_edge(clkdiv(0)) then
			case transState is
				
				when IDLE => -- Intercept a new transmission request (port 0x3F)
					if A(7 downto 0) = zxmmc_spi_port and iorqx = '0' and M1 = '1' then -- If there is a transmission request, prepare to SAMPLE the databus
					transState <= SAMPLE;
				end if;
			
				when SAMPLE =>
					if WR = '0' then -- If it is a SEND request, sample the CPU data bus
						toSDByte <= D_CPU;
					end if;
					transState <= TRANSMIT; -- then start the transmission
				
				when TRANSMIT =>
					TState <= TState + 1;
					
					if TState = 15 then
						transState <= IDLE;
						toCPUByte <= fromSDByte(6 downto 0) & SD_MISO;
					else
						if TState(0) = '1' then
							toSDByte   <= toSDByte(6 downto 0) & '1';
							fromSDByte <= fromSDByte(6 downto 0) & SD_MISO;
						end if;
					end if;
				when OTHERS =>
					null;
			end case;
		end if;
	-- SPI SD Card pins
	SD_CLK <= TState(0);
	SD_MOSI <= toSDByte(7);
	end process;
	
	cpummcrq <= '1' when iord = '0' and A(7 downto 0) = zxmmc_spi_port else '0';

end zxpent;


