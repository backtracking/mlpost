mpost: test.mp
	mpost test.mp end
	make -C manual

latex: test.ps

clean: 
	rm -f test\.[0-9]*
	make -C manual clean
	rm -f test.tex *.log test.mpx *.ps *.aux *.dvi

include ../Makefile.rules
