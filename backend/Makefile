GENOPTIONS=-cflag -dtypes

all: testdviinterp testdvi testdvicairo tfm spline

testdviinterp:
	ocamlbuild $(GENOPTIONS) -no-links testdviinterp.native
	ln -sf _build/testdviinterp.native testdviinterp

testdvi:
	ocamlbuild $(GENOPTIONS) -no-links testdvi.native
	ln -sf _build/testdvi.native testdvi

testdvicairo :
	ocamlbuild $(GENOPTIONS) -lflag "gtkInit.cmx" -no-links testdvicairo.native
	ln -sf _build/testdvicairo.native dvicairo

tfm:
	ocamlbuild $(GENOPTIONS) -no-links testtfm.native
	ln -sf _build/testtfm.native tfm

spline:
	ocamlbuild $(GENOPTIONS) -lflag "gtkInit.cmx" -no-links spline.native
	ln -sf _build/spline.native spline

test: testdvi
	./testdvi hello.dvi math.dvi jfla.dvi

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

.PHONY : tfm dvicairo testdvi testdviinterp spline

.SUFFIXES : .dvi .pdf
.dvi.pdf: dvicairo
	./dvicairo --pdf $< $@

clean:
	ocamlbuild -clean