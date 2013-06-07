--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Implementação de um sistema completo usando o processador MIPS.
--		Este arquivo contém descrições de:
--			- um pacote de definições relacionadas a memórias 
--				de instruções e de dados (package aux_functions)
--			- uma implementação de uma RAM com acesso assíncrono
--				(par E/A RAM_mem)
--			- um testbench que instancia a memória de dados, a
--				memória de instruções a CPU do MIPS e carrega
--				as memórias com instruções e dados de um arquivo
--				externo
--		Note-se que:
--			- o processador é mantido em estado de reset até
--		que as memórias de instruções e de dados tenham sido
--		preenchidas
--			- Assume-se uma organização do tipo HARVARD, com memórias
--				de instruções e de dados distintas e independentes
--
--	Versão 	Inicial 	- Moraes 13/outubro/2004
--			Revisado 	- Ney 24/outubro/2008 - Alteração para eliminar
-- 						o registrador IR e tornar a MIPS_V0 realmente
--						monociclo.
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library IEEE;
use IEEE.Std_Logic_1164.all;

package aux_functions is  

   subtype reg32  is std_logic_vector(31 downto 0);
   subtype reg16  is std_logic_vector(15 downto 0);
   subtype reg8   is std_logic_vector( 7 downto 0);
   subtype reg4   is std_logic_vector( 3 downto 0);

   -- definição do tipo 'memory', que será utilizado para as memórias de dados/instruções
   constant MEMORY_SIZE : integer := 2048;     
   type memory is array (0 to MEMORY_SIZE) of reg8;

   constant TAM_LINHA : integer := 200;
   
   function CONV_VECTOR( letra : string(1 to TAM_LINHA);  pos: integer ) return std_logic_vector;
   
end aux_functions;

package body aux_functions is

  --
  -- converte um caracter de uma dada linha em um std_logic_vector
  --
  function CONV_VECTOR( letra:string(1 to TAM_LINHA);  pos: integer ) return std_logic_vector is         
     variable bin: reg4;
   begin
      case (letra(pos)) is  
              when '0' => bin := "0000";
              when '1' => bin := "0001";
              when '2' => bin := "0010";
              when '3' => bin := "0011";
              when '4' => bin := "0100";
              when '5' => bin := "0101";
              when '6' => bin := "0110";
              when '7' => bin := "0111";
              when '8' => bin := "1000";
              when '9' => bin := "1001";
              when 'A' | 'a' => bin := "1010";
              when 'B' | 'b' => bin := "1011";
              when 'C' | 'c' => bin := "1100";
              when 'D' | 'd' => bin := "1101";
              when 'E' | 'e' => bin := "1110";
              when 'F' | 'f' => bin := "1111";
              when others =>  bin := "0000";  
      end case;
     return bin;
  end CONV_VECTOR;

end aux_functions;     

--------------------------------------------------------------------------
-- Módulo que implementa um modelo comportamental de uma RAM Assíncrona
--------------------------------------------------------------------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.STD_LOGIC_UNSIGNED.all;
use work.aux_functions.all;

entity RAM_mem is
      generic(  START_ADDRESS: reg32 := (others=>'0')  );
      port( RAM_ce_n, RAM_we_n, RAM_oe_n, RAM_bw: in std_logic;
	  RAM_address: in reg32;   RAM_data: inout reg32);
end RAM_mem;

architecture RAM_mem of RAM_mem is 
   signal RAM : memory;
   signal tmp_address: reg32;
   alias  low_address: reg16 is tmp_address(15 downto 0);
   	--  baixa para 16 bits devido ao CONV_INTEGER --
begin     

   tmp_address <= (RAM_address - START_ADDRESS) after 0.01 ns;   
   	--  offset do endereçamento  -- 
   
   -- escreve sincronamente na memória  -- LITTLE ENDIAN -------------------
   process(RAM_ce_n, RAM_we_n, low_address)
   begin
	   if RAM_ce_n='0' and RAM_we_n='0' then
		   if CONV_INTEGER(low_address)>=0
			   and CONV_INTEGER(low_address+3)<=MEMORY_SIZE then
			   if RAM_bw='1' then
				RAM(CONV_INTEGER(low_address+3)) <= RAM_data(31 downto 24);
				RAM(CONV_INTEGER(low_address+2)) <= RAM_data(23 downto 16);
				RAM(CONV_INTEGER(low_address+1)) <= RAM_data(15 downto  8);
         	   end if;
           	RAM(CONV_INTEGER(low_address)) <= RAM_data( 7 downto  0); 
       	   end if;
        end if;
    end process;   
    
   -- lê da memória
   process(RAM_ce_n, RAM_oe_n, low_address)
     begin
       if RAM_ce_n='0' and RAM_oe_n='0' and
          CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address+3)<=MEMORY_SIZE then
            RAM_data(31 downto 24) <= RAM(CONV_INTEGER(low_address+3));
            RAM_data(23 downto 16) <= RAM(CONV_INTEGER(low_address+2));
            RAM_data(15 downto  8) <= RAM(CONV_INTEGER(low_address+1));
            RAM_data( 7 downto  0) <= RAM(CONV_INTEGER(low_address  ));
        else
            RAM_data(31 downto 24) <= (others=>'Z');
            RAM_data(23 downto 16) <= (others=>'Z');
            RAM_data(15 downto  8) <= (others=>'Z');
            RAM_data( 7 downto  0) <= (others=>'Z');
        end if;
   end process;   

end RAM_mem;

-------------------------------------------------------------------------
--  TESTBENCH de SIMULAÇÃO do MIPS  
-------------------------------------------------------------------------
library ieee;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;          
use STD.TEXTIO.all;
use work.aux_functions.all;

entity MRstd_tb is
end MRstd_tb;

architecture cpu_tb of MRstd_tb is
    
    signal tb_Dadress, tb_Ddata, tb_Iadress, tb_Idata,
           tb_i_cpu_address, tb_d_cpu_address, tb_data_cpu, tb_add, tb_data : reg32 := (others => '0' );
    
    signal tb_Dce_n, tb_Dwe_n, tb_Doe_n, tb_Ice_n, tb_Iwe_n, tb_Ioe_n, tb_ck, tb_rst, tb_rstCPU, 
           tb_go_i, tb_go_d, tb_ce, tb_rw, tb_bw: std_logic;
    
    file ARQ : TEXT open READ_MODE is "prog3.txt";
 
begin
           
    Data_mem:  entity work.RAM_mem 
               generic map( START_ADDRESS => x"10010000" )
               port map (RAM_ce_n=>tb_Dce_n, RAM_we_n=>tb_Dwe_n, 
			   RAM_oe_n=>tb_Doe_n, RAM_bw=>tb_bw, RAM_address=>tb_Dadress,
			   RAM_data=>tb_Ddata);
                                            
    Instr_mem: entity work.RAM_mem 
               generic map( START_ADDRESS => x"00400000" )	-- ATENÇÃO a este VALOR!! 
	   										-- Ele depende do simulador!!
	   										-- Para o SPIM --> 	use x"00400020"
											-- Para o MARS -->	use x"00400000"
               port map (RAM_ce_n=>tb_Ice_n, RAM_we_n=>tb_Iwe_n, 
			   RAM_oe_n=>tb_Ioe_n, RAM_bw=>'1', RAM_address=>tb_Iadress,
			   RAM_data=>tb_Idata);
        
    -- sinais para a memória de dados --------------------------------------------------------
    tb_Dce_n <= '0' when  tb_ce='1' or tb_go_d='1'             else '1';
    tb_Doe_n <= '0' when (tb_ce='1' and tb_rw='1')             else '1';       
    tb_Dwe_n <= '0' when (tb_ce='1' and tb_rw='0') or tb_go_d='1' else '1';    

    tb_Dadress <= tb_add  when tb_rstCPU='1' else tb_d_cpu_address;
    tb_Ddata   <= tb_data when tb_rstCPU='1' else tb_data_cpu when (tb_ce='1' and tb_rw='0') else (others=>'Z'); 
    
    tb_data_cpu <= tb_Ddata when (tb_ce='1' and tb_rw='1') else (others=>'Z');
    
    -- sinais para a memória de instruções --------------------------------------------------------
    tb_Ice_n <= '0';                                 
    tb_Ioe_n <= '1' when tb_rstCPU='1' else '0';           -- impede leitura enquanto está escrevendo                             
    tb_Iwe_n <= '0' when tb_go_i='1'   else '1';           -- escrita durante a leitura do arquivo 
    
    tb_Iadress <= tb_add  when tb_rstCPU='1' else tb_i_cpu_address;
    tb_Idata   <= tb_data when tb_rstCPU='1' else (others => 'Z'); 
  

    tb_cpu: entity work.MRstd  port map(
              G_clock=>tb_ck, G_reset=>tb_rstCPU,
              G_i_address => tb_i_cpu_address,
              G_instruction => tb_Idata,
              G_ce=>tb_ce,  G_rw=>tb_rw,  G_bw=>tb_bw,
              G_d_address => tb_d_cpu_address,
              G_data => tb_data_cpu
        ); 

    tb_rst <='1', '0' after 25 ns;       -- gera o sinal de reset

    process                          -- gera o clock
        begin
        tb_ck <= '0', '1' after 10 ns;
        wait for 20 ns;
    end process;

    
    -----------------------------------------------------------------------------
    -- Este processo carrega a memória de instruções e a de dados durante o reset
    --		Trata-se de um parser que lê código gerado por um simulador (SPIM/MARS)
	--		com formato mais ou menos livre, tentando encontrar números que
	--		representem endereços e códigos, que devem ter a forma de "0x" seguido
	--		de exatamente 8 dígitos hexadecimais. Em cada linha, assume-se que
	--		o primeiro número é o endereço inicial da linha, enquanto os números
	--		seguintes são os conteúdos a partir daquele endereço (sejam instruções
	--		ou dados). Por exemplo, o SPIM gera:
    --
    --      .CODE
    --      [0x00400020] 0x3c011001  lui $1, 4097 [d2]   ; 16: la    $t0, d2
    --      [0x00400024] 0x34280004  ori $8, $1, 4 [d2]
    --      [0x00400028] 0x8d080000  lw $8, 0($8)        ; 17: lw    $t0,0($t0)
    --      .....
    --      [0x00400048] 0x0810000f  j 0x0040003c [loop] ; 30: j     loop
    --      [0x0040004c] 0x01284821  addu $9, $9, $8     ; 32: addu $t1, $t1, $t0
    --      [0x00400050] 0x08100014  j 0x00400050 [x]    ; 34: j     x
    --      .DATA
    --      [0x10010000]     0x0000faaa  0x00000083  0x00000000  0x00000000
    --		   
    ----------------------------------------------------------------------------
    process
        variable ARQ_LINE : LINE;
        variable tb_line_arq : string(1 to 200);
        variable tb_code     : boolean;
        variable tb_i, tb_address_flag : integer;
    begin  
        tb_go_i <= '0';
        tb_go_d <= '0';
        tb_rstCPU <= '1';	-- segura o processador em reset 
			-- durante a leitura do arquivo
        tb_code:=true;     -- valor "default" de code é 1 (código, e não dados)
                                 
        wait until tb_rst = '1';
        
        while NOT (endfile(ARQ)) loop    
		-- INÍCIO DA LEITURA DO ARQUIVO CONTENDO INSTRUÇÃO E DADOS -----
            readline(ARQ, ARQ_LINE);      
            read(ARQ_LINE, tb_line_arq(1 to  ARQ_LINE'length) );
                        
            if tb_line_arq(1 to 5)=".CODE" then 
                   tb_code:=true;                     -- código
            elsif tb_line_arq(1 to 5)=".DATA" then
                   tb_code:=false;                    -- dados 
            else 
               tb_i := 1;	-- LEITORA DE LINHA - analizar o loop 
				   		-- abaixo para compreender 
               tb_address_flag := 0; -- para INSTRUÇÃO é um para (end,inst)
                                  -- para DADO aceita (end, dado 0, dado 1, dado 2 ....)
               loop                                     
                  if tb_line_arq(tb_i) = '0' and tb_line_arq(tb_i+1) = 'x' then      
					-- encontrou indicação de número hexa: '0x'
                         tb_i := tb_i + 2;
                         if tb_address_flag=0 then
                               for tb_w in 0 to 7 loop
                                   tb_add( (31-tb_w*4) downto (32-(tb_w+1)*4))  <=
								   	CONV_VECTOR(tb_line_arq,tb_i+tb_w);
                               end loop;    
                               tb_i := tb_i + 8; 
                               tb_address_flag := 1;    
							   -- sinaliza que já leu o conteúdo do endereço;
                         else
                               for tb_w in 0 to 7 loop
                                   tb_data( (31-tb_w*4) downto (32-(tb_w+1)*4))  <=
								   	CONV_VECTOR(tb_line_arq,tb_i+tb_w);
                               end loop;    
                               tb_i := tb_i + 8;
                               
                               wait for 0.1 ns;
                               
                               if tb_code=true then tb_go_i <= '1';    
								   -- o sinal go_i habilita a escrita na memória de instruções
							   else tb_go_d <= '1';    
								   -- o sinal go_i habilita a escrita na memória de dados
                               end if; 
                               
                               wait for 0.1 ns;
                               
                               tb_add <= tb_add + 4;       
							   -- *great!* consigo ler mais de uma word por linha!
                               tb_go_i <= '0';
                               tb_go_d <= '0'; 
                               
                               tb_address_flag := 2;    
							   -- sinaliza que já leu o conteúdo do endereço;

                         end if;
                  end if;
                  tb_i := tb_i + 1;
                  
                  -- sai da linha quando chegou no seu final OU já 
				  -- leu par(endereço, instrução) no caso de código
                  exit when tb_i=TAM_LINHA or (tb_code=true and tb_address_flag=2);
               end loop;
            end if;
            
        end loop;                       
		-- FINAL DA LEITURA DO ARQUIVO CONTENDO INSTRUÇÃO E DADOS -----
        
        tb_rstCPU <= '0';   -- libera o processador para executar o programa
        wait for 2 ns;   -- Espera para ativar o sinal de reset da CPU
        wait until tb_rst = '1';  -- para entrar em Hold de novo!
        
    end process;
    
end cpu_tb;
