GENOPTIONS=-cflag -dtypes -j 2

all: testdviinterp testdvi testdvicairo tfm spline testspline_lib

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

testspline_lib:
	ocamlbuild $(GENOPTIONS) -no-links testspline_lib.native
	ln -sf _build/testspline_lib.native testspline_lib

test: testdvi
	./testdvi hello.dvi math.dvi jfla.dvi

testspline: testspline_lib
	./testspline_lib
	xpdf testspline_lib.pdf

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

.PHONY : tfm dvicairo testdvi testdviinterp spline testspline_lib

.SUFFIXES : .dvi .pdf
.dvi.pdf: dvicairo
	./dvicairo --pdf $< $@

clean:
	ocamlbuild -clean