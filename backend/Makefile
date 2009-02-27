all: testdviinterp testdvi dvicairo tfm

testdviinterp:
	ocamlbuild testdviinterp.native
	ln -s _build/testdviinterp.native testdviinterp

testdvi:
	ocamlbuild testdvi.native
	ln -s _build/testdvi.native testdvi

dvicairo :
	ocamlbuild dvicairo.native
	ln -s _build/dvicairo.native dvicairo

tfm:
	ocamlbuild testtfm.native
	ln -s _build/testtfm.native tfm

test: testdvi
	./testdvi hello.dvi math.dvi jfla.dvi

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

.PHONY : tfm dvicairo testdvi testdviinterp

.SUFFIXES : .dvi .pdf
.dvi.pdf: dvicairo
	./dvicairo $< $*
	pdfjoin --outfile $@ $**.pdf

clean:
	ocamlbuild -clean