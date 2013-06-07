all:
	ghdl -a --ieee=synopsys -fexplicit MRstd.vhd 
	ghdl -a --ieee=synopsys -fexplicit MRstd_tb.vhd 
	ghdl -e --ieee=synopsys -fexplicit MRstd_tb 
	./mrstd_tb --stop-time=1000ns --vcd=mrstd.vcd
	rm work-obj93.cf
	rm e~mrstd_tb.o
	gtkwave mrstd.vcd &

exec:
	gtkwave mrstd.vcd &

limpa:
	rm MRstd.o
	rm MRstd_tb.o 
