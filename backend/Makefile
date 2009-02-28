all: testdviinterp testdvi dvicairo tfm spline

testdviinterp:
	ocamlbuild -no-links testdviinterp.native
	ln -sf _build/testdviinterp.native testdviinterp

testdvi:
	ocamlbuild -no-links testdvi.native
	ln -sf _build/testdvi.native testdvi

dvicairo :
	ocamlbuild -no-links dvicairo.native
	ln -sf _build/dvicairo.native dvicairo

tfm:
	ocamlbuild -no-links testtfm.native
	ln -sf _build/testtfm.native tfm

spline:
	ocamlbuild -lflag "gtkInit.cmx" -no-links spline.native
	ln -sf _build/spline.native spline

test: testdvi
	./testdvi hello.dvi math.dvi jfla.dvi

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

.PHONY : tfm dvicairo testdvi testdviinterp spline

.SUFFIXES : .dvi .pdf
.dvi.pdf: dvicairo
	./dvicairo $< $@

clean:
	ocamlbuild -clean