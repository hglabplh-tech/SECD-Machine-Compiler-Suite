# Makefile for the bcpl implementation of PAL

help:
	@echo
	@echo "make demo           Compile and run demo.pal"
	@echo "make syn            Compile and run syn.pal"
	@echo "make lextest        Compile and run lextest.pal"
	@echo "make syntest        Compile and run syntest.pal"
	@echo "make t1             Compile and run t1.pal"
	@echo "make test           Compile and run test.pal"
	@echo "make xref           Create xrefdata"
	@echo "make clean          Delete unneeded files"
	@echo

demo:
	cintsys -c pal70 -p -c -t demo.pal

t1:
	cintsys -c pal70 -p -c -t t1.pal

test:
	cintsys -c pal70 -l -p -c -t test.pal

lextest:
	cintsys -c pal70 -l lextest.pal

syntest:
	cintsys -c pal70 -p -c -t syntest.pal

syn:
	cintsys -c pal70 -p -c -t syn.pal

xref:
	cintsys -c sortxref rawxref to xrefdata
	rm rawxref

rawxref:
	cintsys -c c b pal xref >rawxref


clean:
	rm -f *~ pal junk*
