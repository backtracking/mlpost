GENOPTIONS=-cflag -dtypes -j 2

all: mlcairo

testdviinterp:
	ocamlbuild $(GENOPTIONS) -no-links testdviinterp.native
	ln -sf _build/testdviinterp.native testdviinterp

testdvi:
	ocamlbuild $(GENOPTIONS) -no-links testdvi.native
	ln -sf _build/testdvi.native testdvi

testdvicairo :
	ocamlbuild $(GENOPTIONS) -no-links testdvicairo.native
	ln -sf _build/testdvicairo.native dvicairo

tfm:
	ocamlbuild $(GENOPTIONS) -no-links testtfm.native
	ln -sf _build/testtfm.native tfm

spline:
	ocamlbuild $(GENOPTIONS) -no-links spline.native
	ln -sf _build/spline.native spline

testspline_lib:
	ocamlbuild $(GENOPTIONS) -no-links testspline_lib.native
	ln -sf _build/testspline_lib.native testspline_lib

testwidth:
	ocamlbuild $(GENOPTIONS) -no-links testwidth.native
	ln -sf _build/testwidth.native testwidth

test: testdvi
	./testdvi hello.dvi math.dvi jfla.dvi

testspline: testspline_lib
	./testspline_lib
	xpdf testspline_lib.pdf

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

mlcairo :
	ocamlbuild $(GENOPTIONS) -I trunk -no-links mlcairo.native

.PHONY : tfm dvicairo testdvi testdviinterp spline testspline_lib testwidth mlcairo

.SUFFIXES : .dvi .pdf
.dvi.pdf: dvicairo
	./dvicairo --pdf $< $@

clean:
	ocamlbuild -clean