mpost: testmanual.mp
	mpost testmanual.mp end
	make -C manual

manual: testmanual.ps

other: othergraphs.mp
	mpost othergraphs.mp end
	make -C othergraphs

tests: tests.mp
	mpost tests.mp end
	latex tests
	dvips tests.dvi -o tests.ps

othertex: othergraphs.ps

clean: 
	rm -f testmanual\.[0-9]* testmanual.mp
	make -C manual clean
	make -C othergraphs clean
	rm -f testmanual.tex *.log testmanual.mpx *.ps *.aux *.dvi
	rm -f othergraphs.*

include ../Makefile.rules
