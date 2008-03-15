manual: testmanual.mp
	mpost testmanual.mp end
	make -C manual
	latex testmanual.tex
	dvips testmanual.dvi -o testmanual.ps
	
other: othergraphs.mp
	mpost othergraphs.mp end
	make -C othergraphs
	latex othergraphs.tex
	dvips othergraphs.dvi -o othergraphs.ps

tests: tests.mp
	mpost tests.mp end
	latex tests
	dvips tests.dvi -o tests.ps

clean: 
	make -C manual clean
	make -C othergraphs clean
	rm -f testmanual.*
	rm -f tests.*
	rm -f othergraphs.*
