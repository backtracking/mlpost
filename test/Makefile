NAME:=test
test: test.dvi
	xdvi test.dvi

test.dvi: test.tex mpost
	latex $< && latex $<

mpost: test.mp
	mpost test.mp end

test.mp test.tex: ../graphs.opt
	../graphs.opt

clean: 
	rm -f test\.[0-9]*
	rm -f *.aux *.log $(NAME).tex $(NAME).dvi $(NAME).ps

graphs.opt:
	make -C ../ graphs.opt
