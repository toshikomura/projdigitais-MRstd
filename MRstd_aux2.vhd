--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Implementa��o em hardware sintetiz�vel de uma organiza��o monociclo
--	do processador MIPS. Apenas 9 das instru��es do MIPS s�o aceitas
--	por esta organiza��o:
--	ADDU, SUBU, AAND, OOR, XXOR, NNOR, LW, SW e ORI	
--
--	Vers�o 	Inicial 	- Moraes 20/setembro/2006
--			Revis�o 	- Ney 07/maio/2008
--			Revis�o 	- Ney 09/maio/2008 - removido bug do ORI
--				ORI opera agora com extens�o de 0 e n�o extens�o de sinal
--			Revisado 	- Ney 24/outubro/2008 - Altera��o para eliminar
-- 						o registrador IR e tornar a MIPS_V0 realmente
--						monociclo.
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- package com tipos b�sicos
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;

-- Biblioteca pr�pia para ser usada no circuito
package p_MI0 is  
  
  subtype reg32 is std_logic_vector(31 downto 0);  
  -- tipo para os barramentos de 32 bits
  -- acrescentar lui
  type inst_type is (ADDU, SUBU, AAND, OOR, XXOR, NNOR, LW, SW, 
  	ORI, invalid_instruction, MUL, JAL, J, JR, BGTZ, ADDIU, LUI  );

-- Teoricamente, pois n�o fiz testes, as instru��es que j� foram implementadas foram 
-- MUL, LUI, ADDIU faltam J, JAL, JR e BGTZ
-- Observar a instru��o ORI para ver se ela deve ser re-implementada

  type microinstruction is record
 -- ce e rw s�o os controles da mem�ria
    ce:    std_logic; -- libera��o da mem�ria
    rw:    std_logic; -- escrita na mem�ria
    i:     inst_type; -- tipo de instru��o       
    wreg:  std_logic;       -- wreg diz se o banco de registradores
							-- deve ou n�o ser escrito
  end record;

    
end p_MI0;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Registrador gen�rico sens�vel � borda de subida do clock
-- com possibilidade de inicializa��o de valor
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_MI0.all;


-- Esta entidade � o reg
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

-- Entidade que define os 32 regs
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

        wen(i) <= '1' when i/=0 and adRD=i and wreg='1' else '0';
         
        rx: entity work.reg32_ce  
			port map(ck=>ck, rst=>rst, ce=>wen(i), D=>RD, Q=>reg(i));                   
        
    end generate g1;      

    R1 <= reg(CONV_INTEGER(AdRs));    -- sele��o do fonte 1  

    R2 <= reg(CONV_INTEGER(AdRt));    -- sele��o do fonte 2 
   
end reg_bank;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ALU - a opera��o depende somente da instru��o corrente que �
-- 	decodificada na Unidade de Controle
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_signed.all;
use work.p_MI0.all;

-- Entidade que faz as opera��es de soma, multiplica��o, etc
entity alu is
       port( alu_op1, alu_op2 : in  reg32;
	   		alu_outalu :   out reg32; 
	   		alu_zero : out std_logic;
             		alu_op_alu :   in  inst_type   
           );
end alu;

architecture alu_arq of alu is 
signal alu_int_alu	: reg32;
signal alu_bgtz : boolean;
begin
    alu_outalu <= alu_int_alu;
    alu_int_alu <=  
        alu_op1 - alu_op2      when  alu_op_alu=SUBU 				else
        alu_op1 and alu_op2    when  alu_op_alu=AAND				else 
        alu_op1 or  alu_op2    when  alu_op_alu=OOR  or alu_op_alu=ORI	else 
        alu_op1 xor alu_op2    when  alu_op_alu=XXOR              	else 
        alu_op1 nor alu_op2    when  alu_op_alu=NNOR             	else 
	   alu_op1 * alu_op2      when  alu_op_alu=MUL                   else
        alu_op1 + alu_op2;      --- default � a soma
	alu_bgtz <= alu_op1 > alu_op2 when  alu_op_alu=BGTZ;
	alu_zero <= '1' when alu_int_alu=x"00000000" else '0';
end alu_arq;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- MUX que difine qual valor vai para o pr�ximo PC
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; 
use work.p_MI0.all;

-- ADICIONADA
-- Entidade que serve para selecionar o pr�ximo PC
entity mux_pc is 
      port(  Mpc_ck, Mpc_rst : in std_logic;
		   Mpc_4, Mpc_j_jal, Mpc_jr, Mpc_bgtz : in reg32;
	        Mpc_seletor : in std_logic_vector(1 downto 0);
		   Mpc_next : out reg32
          );
end mux_pc;

architecture mux_pc_arq of mux_pc is 
begin 
	Mpc_next <= Mpc_4 when Mpc_seletor="00" else
			  Mpc_j_jal when Mpc_seletor="01" else
			  Mpc_jr when Mpc_seletor="10" else
			  Mpc_bgtz when Mpc_seletor="11";
end mux_pc_arq;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Descri��o do Bloco de Dados (Datapath)
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

-- ========================================
-- OBSERVAL��O IMPORTANTE 
-- Deve se observar nesta arquitetura aonde deve-se acrescentar o "process"
-- que impede que o circuito execute tudo em paralelo e execute em sequencia
-- isto foi analisado por�m tem que ser conferindo com bateria de testes
-- =======================================

architecture datapath_arq of datapath is
   signal DP_incpc, DP_pc, DP_result, DP_R1, DP_R2, DP_ext32,
 		DP_op2, DP_reg_dest, DP_pc_4, DP_pc_j_jal, DP_pc_save, DP_pc_jr,
	 	DP_pc_bgtz: reg32;
   signal DP_adD  : std_logic_vector(4 downto 0) ;      
   signal DP_seletor : std_logic_vector(1 downto 0);
   signal DP_instR, DP_instJ, DP_zero : std_logic ;       
begin           
        
   DP_instR <= '1' when DP_uins.i=ADDU or DP_uins.i=SUBU or 
		DP_uins.i=AAND or DP_uins.i=OOR or DP_uins.i=XXOR or DP_uins.i=NNOR or
	 	DP_uins.i=MUL or DP_uins.i=JR else '0';

   -- Jumps incodicionais
   DP_instJ <= '1' when DP_uins.i=JAL or DP_uins.i=J else '0';
   -- Se for JAL ent�o DP_pc_save � guardado no reg 31
   DP_pc_save <= Dp_pc + 4 when DP_uins.i=JAL else (others=>'Z');
   --======  Hardware para a busca de instru��es  =============================================

   -- O problema que deve ser resolvido aqui, � a quest�o do tipo de endere�o 
   DP_pc_4 <= DP_pc + 4;
   -- J e JAL s�o iguais com rela��o ao modo de conseguir o novo endere�o 
   -- A �nica diferen�a que em JAL o retorno � guardado em reg 31

   -- Define o valor do seletor 
   DP_seletor <= "01" when DP_uins.i=J or DP_uins.i=JAL else
		    "10" when DP_uins.i=JR else
		    "11" when DP_uins.i=BGTZ else
		    "00"; -- default para PC + 4

   -- Mux que vai determina qual o novo PC
   -- Eu acredito que o ck e rst s�o desness�rios, porem para n�o haver d�vidas tiramos depois
   mux1pc: entity work.mux_pc port map(Mpc_ck=>DP_ck, Mpc_rst=>DP_rst, 
		 Mpc_4=> DP_pc_4, Mpc_j_jal=> DP_pc_j_jal, Mpc_jr=>DP_pc_jr,
		 Mpc_bgtz=>DP_pc_bgtz, Mpc_seletor=> DP_seletor, Mpc_next=> DP_incpc);
   
   -- Determina quando atualiza o PC
   rpc: entity work.reg32_ce 
	   generic map(INIT_VALUE=>x"00400000")	-- ATEN��O a este VALOR!! 
	   										-- Ele depende do simulador!!
	   										-- Para o SPIM --> 	use x"00400000"
											-- Para o MARS -->	use x"0040002	0"
              port map(ck=>DP_ck, rst=>DP_rst, ce=>'1', 
			  D=>DP_incpc, Q=>DP_pc);
           
   DP_IR_OUT <= DP_instruction ;	
   -- IR_OUT � o sinal de sa�da do Bloco de Dados, que cont�m
   -- o c�digo da instru��o em execu��o no momento. � passado
   -- ao Bloco de Controle             
   DP_i_address <= DP_pc;
   
   --======== hardware do banco de registradores e extens�o de sinal ou de 0 ================
   
   DP_adD <= DP_instruction(15 downto 11) when DP_instR='1'  else
		   "11111" when DP_uins.i=JAL else -- em jal o reg 31 que vai ser usado 
             DP_instruction(20 downto 16) ;-- sen�o pode ser um ori ou lui           

  -- Processo que decide qual vai ser o dado a ser gravado em um reg
  process(DP_ck, DP_rst)
  begin            
   -- Se a instru��o for JAL
   if  DP_uins.i=JAL then
   	  DP_reg_dest <= DP_pc_save; 
   -- Se a instru��o for ORI ou LUI 
   elsif DP_uins.i=ORI or DP_uins.i=LUI then
   	  DP_reg_dest <= DP_instruction(15 downto 0) & "0000000000000000";
   -- Se a instru��o for ADDIU ou BGTZ
   elsif DP_uins.i=ADDIU or DP_uins.i=BGTZ then
   	  DP_reg_dest <= "0000000000000000" & DP_instruction(15 downto 0) ;
   -- Se a instru��o for LW
   elsif DP_uins.i=LW then
	  DP_reg_dest <=  DP_data; 
   -- Se a instru��o for do tipo R
   elsif DP_instR='1' then 
	  DP_reg_dest <= DP_result;
   end if;
  end process;  

   -- ck, rst, escritareg, reg1, reg2, reg3, dado, op1, op2
   REGS: entity work.reg_bank port map
	   (ck=>DP_ck, rst=>DP_rst, wreg=>DP_uins.wreg, AdRs=>DP_instruction(25 downto 21),
	   		AdRt=>DP_instruction(20 downto 16), adRD=>DP_adD, RD=>DP_reg_dest, R1=>DP_R1, R2=>DP_R2);
    
   -- Extens�o de 0 ou extens�o de sinal
   DP_ext32 <=	x"FFFF" & DP_instruction(15 downto 0) when (DP_instruction(15)='1' 
   and (DP_uins.i=LW or DP_uins.i=SW)) else
	   		-- LW and SW use signal extension, ORI uses 0-extension
			x"0000" & DP_instruction(15 downto 0);
	   		-- other instructions do not use this information,
			-- thus anything is good 0 or sign extension
    	
   --=========  hardware da ALU e em volta dela ==========================================
                          
   -- Se for uma instru��o do tipo R reg1, reg2, reg3
   -- Sen�o ao inv�s de mandar reg3 manda extende
   DP_op2 <= DP_R2 when DP_instR='1' else DP_ext32; 
                 
   -- Se for ADDIU mandar o imediato no lugar de reg3
   -- Sen�o permanece o op2
   DP_op2 <= DP_reg_dest when DP_uins.i=ADDIU else DP_op2;

   alu1: entity work.alu port map (alu_op1=>DP_R1, alu_op2=>DP_op2,
	   alu_outalu=>DP_result, alu_zero=>DP_zero, alu_op_alu=>DP_uins.i);
                                               
   -- operacao com a mem�ria de dados
     
   -- Se a instrucao for BGTZ coloca o resultado da ULA no sinal DP_BGTZ
   DP_pc_bgtz <= DP_result & "00" when DP_uins.i=BGTZ else (others=>'Z');

   -- Resultado da ULA coloca na saida d_adress
   DP_d_address <= DP_result;
       
   -- DATA pode ser R2 se for SW (escrita na mem�ria)
   DP_data <= DP_R2 when DP_uins.ce='1' and DP_uins.rw='0' else (others=>'Z');  

end datapath_arq;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--  Unidade de Controle - decodifica a instru��o e gera os sinais de controle
--		nesta implementa��o � um bloco puramente combinacional
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MI0.all;

-- Entidade que faz o controle da mem�ria RAM
entity control_unit is
	port(CT_ck, CT_rst: in std_logic; 	-- estes sinais s�o in�teis nesta vers�o da
									-- Unidade de Controle, pois ela � combinacional
         CT_uins :   out microinstruction; -- Controle
         CT_ir :     in reg32 
        );
end control_unit;
                   
architecture control_unit of control_unit is
  signal i : inst_type; -- i = tipo de instru��o como ADD
begin
    
    CT_uins.i <= i; -- coloca a instru��o para a saida
    
	-- O padr�o da instru��o na entrada determina qual � a instru��o
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
   -- === Instru��es sendo acrescentadas =====
   -- ========================================

	    -- Esta instru��o � uma pseudo instru��o do mips
	    -- A implementa��o utilizada foi feita da seguinte forma:
	    -- O padr�o � da pseudo instru��o utilizada foi a da instru��o MULT
	    -- E na ULA a multiplica��o � feita assim op1 * op2 => op3
         MUL    when CT_ir(31 downto 26)="000000" and CT_ir(10 downto 0)="00000011000" else
         JAL    when CT_ir(31 downto 26)="000011" else
         J      when CT_ir(31 downto 26)="000010" else
         JR     when CT_ir(31 downto 26)="000000" and CT_ir(20 downto 0)="000000000000000001000" else
         LUI    when CT_ir(31 downto 26)="001111" else
         BGTZ   when CT_ir(31 downto 26)="000111" and CT_ir(20 downto 16)="00000" else

   -- ========================================
   -- === Final Instru��es sendo acrescentadas =====
   -- ========================================

         invalid_instruction ; -- IMPORTANTE: condi��o "default" � invalid instruction;
        
    assert i /= invalid_instruction
          report "******************* INVALID INSTRUCTION *************"
          severity error;
                   
	-- Coloca os sinais de controle na saida

	-- Se a instru��o for escrever na mem�ria ou leitura o sinal � 1
	-- Por dedu��o o sinal � de libera��o da mem�ria
    CT_uins.ce    <= '1' when i=SW  or i=LW else '0';
    -- Sen�o for nenhum dos dois o sinal � 0

	-- Se a instru��o for escrever na mem�ria os dois sinais recebem 0
	-- Por dedu��o o sinal � escrita na mem�ria
    CT_uins.rw    <= '0' when i=SW  else '1';

    -- Sen�o a for escrever estes dois sinais recebem 1
	-- Por dedu��o o sinal � de libera��o do bloco de registradores para escrita 
    CT_uins.wreg  <= '0' when i=SW  else '1';

end control_unit;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Topo da Hierarquia do Processador - instancia��o dos Blocos de 
-- 		Dados e de Controle
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MI0.all;

-- Entidade que determina todo bloco do processador
entity MRstd is
    port( G_clock, G_reset:         in    std_logic; -- clock e reset
          G_ce, G_rw, G_bw:         out   std_logic; -- Controles da mem�ria
          G_i_address, G_d_address: out   reg32; -- Possibilidade de ir para mem�ria
          G_instruction:          	 in    reg32; -- Instru��o para ser executada
          G_data:                   inout reg32);
end MRstd;

architecture MRstd of MRstd is
      signal IR: reg32; -- Sinal de 32 bits
      signal uins: microinstruction; -- Sinais de controle
 begin

     dp: entity work.datapath   

		-- clock, reset, instru��o, controles, i_adress, instru��o, d_adress, data
         port map( DP_ck=>G_clock, DP_rst=>G_reset, DP_IR_OUT=>IR, DP_uins=>uins, DP_i_address=>G_i_address, 
                   DP_instruction=>G_instruction, DP_d_address=>G_d_address,  DP_data=>G_data);

		-- clock, reset, instru��o, controles 
     ct: entity work.control_unit port map( CT_ck=>G_clock, CT_rst=>G_reset, CT_IR=>IR, CT_uins=>uins);

	-- Controles da mem�ria
     G_ce <= uins.ce; 
     G_rw <= uins.rw;
         
     G_bw <= '1';	-- Esta vers�o trabalha apenas em modo word (32 bits).
	 			-- Logo, este sinal � in�til aqui
     
end MRstd;
