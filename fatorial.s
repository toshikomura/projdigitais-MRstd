# Exemplo da pagina A-26 (com simplificacoes)
# main()
# {
#   int x, y;
#   x = 3;
#   y = x + fat(5);
# }
#
# int fat(int n)
# {
#   if (n > 0)
#     return (n * fat(n - 1));
#   else
#     return (1);
# }

	.data
x:	.word 1
y:	.word 1
	
	.text
	.globl main
main:	la $sp, 0x7fff0000	# top of stack (para executar no xspim)
				# este valor deve ser ajustado para o topo da
				# memoria RAM declarada em MRstd_tb.vhd
				# x"10010800"
	subu  $sp, $sp, 4 	# stack frame of main is 4 bytes long
	sw  $ra, 0($sp)	 	# save return address

	# a rotina main invoca a funcao que calcula o fatorial e
	# passa o argumento (5).  Depois que fat() retorna, main()
	# escreve o resultado na variavel y. 

	la $t0, x
	li $s0, 3
	sw $s0, 0($t0)
	li $a0, 5		# coloca o argumento em $a0
	jal fat			# invoca fat()
	addu $s1, $v0, $s0
	la $t0, y
	sw $s1, 0($t0)

	# apos imprimir o fatorial, main() retorna; antes disso, main()
	# deve re-carregar seus registradores e reposicionar o stack
	# pointer.

	lw $ra, 0($sp)		# recarrega o SP
	addu $sp, $sp,4  	# reposiciona stack pointer

	# finalizacao para testes no xspim --remova para testar com VHDL
end:	li $v0, 10
	syscall

	# finalizacao para testes com VHDL - lasso infinito...
	#end:	b end

	# a estrutura de fat() e' similar aquela de main(): primeiro,
	# fat() aloca espaco na pilha e salva os registradores que vai
	# usar (callee-save).  Alem de $ra, fat() tambem salva $a0
	# que vai ser usado nas chamadas da recursao.
     
	.text
fat:    subu  $sp, $sp, 8 	# salva contexto na pilha
	sw $ra, 4($sp)
	sw $a0, 0($sp)		# salva parametro de entrada na pilha

	# fat() testa se o argumento e' maior que zero.  Se nao, a
	# funcao retorna 1.  Se o argumento e' maior que zero,
	# a funcao rotina chama fat() novamente para computar
	# fat(n-1).

	lw $2, 0($sp)
	bgtz $2, $L2      	# desvia se n > 0
	li $2, 1	       	# se n <= 0, retorna 1
	j $L1

$L2:
	lw $3, 0($sp)   	# n > 0
	subu $2, $3, 1     	# calcula n-1
	move $a0, $2      	# carrega parametro e executa fat(n-1)
	jal fat

	lw $3, 0($sp)           # carrega n e calcula fat(n-1) * n
	mul  $2, $2, $3

	# finalmente, fat() recarrega os registradores que salvou e
	# retorna o valor no registrador $v0.

$L1:
	lw  $a0, 0($sp)	        # recompoe contexto
	lw  $ra, 4($sp)	        # resultado esta em $2
	addu  $sp, $sp, 8
	jr  $ra	       	        # retorna
