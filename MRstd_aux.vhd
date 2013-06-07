--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Implementação em hardware sintetizável de uma organização monociclo
--	do processador MIPS. Apenas 9 das instruções do MIPS são aceitas
--	por esta organização:
--	ADDU, SUBU, AAND, OOR, XXOR, NNOR, LW, SW e ORI	
--
--	Versão 	Inicial 	- Moraes 20/setembro/2006
--			Revisão 	- Ney 07/maio/2008
--			Revisão 	- Ney 09/maio/2008 - removido bug do ORI
--				ORI opera agora com extensão de 0 e não extensão de sinal
--			Revisado 	- Ney 24/outubro/2008 - Alteração para eliminar
-- 						o registrador IR e tornar a MIPS_V0 realmente
--						monociclo.
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- package com tipos básicos
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;

package p_MI0 is  
  
  subtype reg32 is std_logic_vector(31 downto 0);  
  -- tipo para os barramentos de 32 bits
  -- acrescentar lui
  type inst_type is (ADDU, SUBU, AAND, OOR, XXOR, NNOR, LW, SW, 
  	ORI, invalid_instruction, ADDIU, MUL, J, JAL, JR, LUI, BGTZ );

  type microinstruction is record
 -- ce e rw são os controles da memória
    ce:    std_logic; -- liberação da memória
    rw:    std_logic; -- escrita na memória
    i:     inst_type; -- tipo de instrução       
    wreg:  std_logic;       -- wreg diz se o banco de registradores
							-- deve ou não ser escrito
  end record;
    
end p_MI0;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Registrador genérico sensível à borda de subida do clock
-- com possibilidade de inicialização de valor
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_MI0.all;

entity reg32_ce is
           generic( INIT_VALUE : reg32 := (others=>'0') );
           port(  ck, rst, ce : in std_logic;
                  D : in  reg32; -- entra um vetor de 32
                  Q : out reg32 -- sai um vetor de 32
               );
end reg32_ce;

architecture reg32_ce of reg32_ce is 
begin

  process(ck, rst)
  begin
       if rst = '1' then
              Q <= INIT_VALUE(31 downto 0); -- saida receb um vetor de 32 zerado
       elsif ck'event and ck = '1' then -- se ta na borda de subida
           if ce = '1' then
              Q <= D; -- a saida recebe o vetor 32 da entrada
           end if;
       end if;
  end process;
        
end reg32_ce;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Banco de registradores - 31 registradores de uso geral - reg(0): cte 0
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use ieee.STD_LOGIC_UNSIGNED.all;
use work.p_MI0.all;

entity reg_bank is
       port( ck, rst, wreg :    in std_logic;
             AdRs, AdRt, adRD : in std_logic_vector( 4 downto 0);
             RD : in reg32; -- entrada de um vetor de 32
             R1, R2: out reg32 -- 2 saidas de um vetor de 32 
           );
end reg_bank;

architecture reg_bank of reg_bank is
   type bank is array(0 to 31) of reg32; -- define uma matriz de 32x32
   signal reg : bank ; -- sinal de uma matriz 32x32                           
   signal wen : reg32 ; -- sinal de um vetor de 32
begin            

    g1: for i in 0 to 31 generate -- cria 32 estruturas da caixa abaixo       

        wen(i) <= '1' when i/=0 -- Diferente do reg0 
				  and adRD=i -- reg de destino
				  and wreg='1' -- escrita no reg
				  else '0';
         
        rx: entity work.reg32_ce  
			port map(ck=>ck, rst=>rst, ce=>wen(i), D=>RD, Q=>reg(i));                   
        
    end generate g1;      

    R1 <= reg(CONV_INTEGER(AdRs));    -- seleção do fonte 1  

    R2 <= reg(CONV_INTEGER(AdRt));    -- seleção do fonte 2 
   
end reg_bank;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ALU - a operação depende somente da instrução corrente que é
-- 	decodificada na Unidade de Controle
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use 	work.p_MI0.all;

entity alu is
       port( alu_op1, alu_op2 : in  reg32;
	   		alu_outalu :   out reg32; 
	   		alu_zero : out std_logic;
             	alu_op_alu :   in  inst_type   
           );
end alu;

architecture alu_arq of alu is 
signal alu_int_alu	: reg32;
begin
    alu_outalu <= alu_int_alu;
    alu_int_alu <=  
        alu_op1 - alu_op2      when  alu_op_alu=SUBU 				else
        alu_op1 and alu_op2    when  alu_op_alu=AAND				else 
        alu_op1 or  alu_op2    when  alu_op_alu=OOR  or alu_op_alu=ORI	else 
        alu_op1 xor alu_op2    when  alu_op_alu=XXOR              	else 
        alu_op1 nor alu_op2    when  alu_op_alu=NNOR             	else 
        alu_op1 * alu_op2      when  alu_op_alu=MUL             	     else 
        alu_op1 + alu_op2;      --- default é a soma
	alu_zero <= '1' when alu_int_alu=x"00000000" else '0';
end alu_arq;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Descrição do Bloco de Dados (Datapath)
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; 
use work.p_MI0.all;
   
entity datapath is
      port(  DP_ck, DP_rst :     in    std_logic;
             DP_i_address :   out   reg32;
             DP_instruction : in    reg32;
             DP_d_address :   out   reg32;
             DP_data :        inout reg32;  
             DP_uins :        in    microinstruction;
             DP_IR_OUT :      out   reg32
          );
end datapath;

architecture datapath_arq of datapath is
   signal DP_incpc, DP_ent_pc, DP_pc_mais_offset, DP_pc, DP_result, DP_R1, DP_R2, DP_ext32, DP_op2, DP_reg_dest: reg32;
   signal DP_adD   : std_logic_vector(4 downto 0) ;       
   signal DP_instR, DP_instI, DP_instJ, DP_zero : std_logic ;       
begin           
        
   -- ========================================
   -- === Instruções sendo acrescentadas =====
   -- ========================================

   -- Instrução que são do tipo R.
   DP_instR <= '1' when DP_uins.i=ADDU or DP_uins.i=SUBU or 
		DP_uins.i=AAND or DP_uins.i=OOR or DP_uins.i=XXOR or
	 	DP_uins.i=NNOR or DP_uins.i=SSLL or DP_uins.i=MUL or 
		DP_uins.i=JR else '0';


   -- Verificar se realmente necessário fazer isto com leitura e escrita na memória e branchs
   DP_instI <= '1' when DP_uins.i=LW or DP_uins.i=SW or 
		DP_uins.i=LUI or DP_uins.i=ORI or DP_uins.i=ADDIU or
 		DP_uins.i=BGTZ else '0';

   -- Instrução do tipo J desvio incodicional 
   DP_instJ <= '1' when DP_uins.i=J or DP_uins.i=JAL else '0';

   -- ========================================
   -- === Final Instruções sendo acrescentadas =====
   -- ========================================

   --======  Hardware para a busca de instruções  =============================================
 
   DP_incpc <= DP_pc + 4; -- incrementa o PC
   
   rpc: entity work.reg32_ce 
	   generic map(INIT_VALUE=>x"00400000")	-- ATENÇÃO a este VALOR!! 
	   										-- Ele depende do simulador!!
	   										-- Para o SPIM --> 	use x"00400020"
											-- Para o MARS -->	use x"00400000"
              port map(ck=>DP_ck, rst=>DP_rst, ce=>'1', 
			  D=>DP_incpc, Q=>DP_pc);
           
   DP_IR_OUT <= DP_instruction ;	
   -- IR_OUT é o sinal de saída do Bloco de Dados, que contém
   -- o código da instrução em execução no momento. É passado
   -- ao Bloco de Controle             
   DP_i_address <= DP_pc;
   
   --======== hardware do banco de registradores e extensão de sinal ou de 0 ================
   
   -- Se a intrução for do tipo R
   DP_adD <= DP_instruction(15 downto 11) when DP_instR='1'  else
          DP_instruction(20 downto 16) ;            
                         
   REGS: entity work.reg_bank port map
	   (ck=>DP_ck, rst=>DP_rst, wreg=>DP_uins.wreg, AdRs=>DP_instruction(25 downto 21),
	   		AdRt=>DP_instruction(20 downto 16), adRD=>DP_adD, RD=>DP_reg_dest, R1=>DP_R1, R2=>DP_R2);
    
   -- Extensão de 0 ou extensão de sinal
   DP_ext32 <=	x"FFFF" & DP_instruction(15 downto 0) when (DP_instruction(15)='1' 
   and (DP_uins.i=LW or DP_uins.i=SW)) else
	   		-- LW and SW use signal extension, ORI uses 0-extension
			x"0000" & DP_instruction(15 downto 0);
	   		-- other instructions do not use this information,
			-- thus anything is good 0 or sign extension
    
   --=========  hardware da ALU e em volta dela ==========================================
                          
   DP_op2 <= DP_R2 when DP_instR='1' else DP_ext32; 
                 
   -- Acesso a ULA 
   -- Para a multiplicação o resultado final deve ser jogado para um reg dest
   alu1: entity work.alu port map (alu_op1=>DP_R1, alu_op2=>DP_op2,
	   alu_outalu=>DP_result, alu_zero=>DP_zero, alu_op_alu=>DP_uins.i);
                                               
   -- operação com a memória de dados, ele pode ser tanto um endereço como um valor 
   DP_d_address <= DP_result;

   DP_data <= DP_R2 when DP_uins.ce='1' and DP_uins.rw='0' else (others=>'Z');  
 
   DP_reg_dest <=  DP_data when DP_uins.i=LW else DP_result;

end datapath_arq;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--  Unidade de Controle - decodifica a instrução e gera os sinais de controle
--		nesta implementação é um bloco puramente combinacional
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MI0.all;

entity control_unit is
	port(CT_ck, CT_rst: in std_logic; 	-- estes sinais são inúteis nesta versão da
									-- Unidade de Controle, pois ela é combinacional
         CT_uins :   out microinstruction; -- Controle
         CT_ir :     in reg32 
        );
end control_unit;
                   
architecture control_unit of control_unit is
  signal i : inst_type; -- i = tipo de instrução como ADD
begin
    
    CT_uins.i <= i; -- coloca a instrução para a saida
    
	-- O padrão da instrução na entrada determina qual é a instrução
    i <= ADDU   when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000100001" else
         SUBU   when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000100011" else
         AAND   when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000100100" else
         OOR    when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000100101" else
         XXOR   when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000100110" else
         NNOR   when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000100111" else
         ORI    when CT_ir(31 downto 26)="001101" else
         LW     when CT_ir(31 downto 26)="100011" else
         SW     when CT_ir(31 downto 26)="101011" else

   -- ========================================
   -- === Instruções sendo acrescentadas =====
   -- ========================================

         SSLL   when CT_ir(31 downto 26)="000000" and CT_ir(5 downto 0)="000000" else
         ADDIU  when CT_ir(31 downto 26)="001001" else
         MUL    when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000011000" else
         JAL    when CT_ir(31 downto 26)="000011"  else
         J      when CT_ir(31 downto 26)="000010" else
         JR     when CT_ir(31 downto 26)="000000" and CT_ir(20 downto 0)="000000000000000001000" else
         LUI    when CT_ir(31 downto 26)="001111" else
         BGTZ   when CT_ir(31 downto 26)="000111" and CT_ir(5 downto 0)="00000" else

   -- ========================================
   -- === Final Instruções sendo acrescentadas =====
   -- ========================================

         invalid_instruction ; -- IMPORTANTE: condição "default" é invalid instruction;
        
    assert i /= invalid_instruction
          report "******************* INVALID INSTRUCTION *************"
          severity error;
                   
	-- Coloca os sinais de controle na saida

	-- Se a instrução for escrever na memória ou leitura o sinal é 1
	-- Por dedução o sinal é de liberação da memória
    CT_uins.ce    <= '1' when i=SW  or i=LW else '0';
    -- Senão for nenhum dos dois o sinal é 0

	-- Se a instrução for escrever na memória os dois sinais recebem 0
	-- Por dedução o sinal é escrita na memória
    CT_uins.rw    <= '0' when i=SW  else '1';

    CT_uins.wreg  <= '0' when i=SW  else '1';
    -- Senão a for escrever estes dois sinais recebem 1
	-- Por dedução o sinal é de liberação do bloco de registradores

end control_unit;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Topo da Hierarquia do Processador - instanciação dos Blocos de 
-- 		Dados e de Controle
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MI0.all;

entity MRstd is
    port( G_clock, G_reset:         in    std_logic; -- clock e reset
          G_ce, G_rw, G_bw:         out   std_logic; -- Controles da memória
          G_i_address, G_d_address: out   reg32; -- Possibilidade de ir para memória
          G_instruction:          	 in    reg32; -- Instrução para ser executada
          G_data:                   inout reg32);
end MRstd;

architecture MRstd of MRstd is
      signal IR: reg32; -- Sinal de 32 bits
      signal uins: microinstruction; -- Sinais de controle
 begin

     dp: entity work.datapath   

		-- clock, reset, instrução, controles, i_adress, instrução, d_adress, data
         port map( DP_ck=>G_clock, DP_rst=>G_reset, DP_IR_OUT=>IR, DP_uins=>uins, DP_i_address=>G_i_address, 
                   DP_instruction=>G_instruction, DP_d_address=>G_d_address,  DP_data=>G_data);

		-- clock, reset, instrução, controles 
     ct: entity work.control_unit port map( CT_ck=>G_clock, CT_rst=>G_reset, CT_IR=>IR, CT_uins=>uins);

	-- Controles da memória
     G_ce <= uins.ce; 
     G_rw <= uins.rw;
         
     G_bw <= '1';	-- Esta versão trabalha apenas em modo word (32 bits).
	 			-- Logo, este sinal é inútil aqui
     
end MRstd;
