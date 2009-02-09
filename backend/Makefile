all: testdviinterp

testdviinterp: dviinterp.ml testdvi tfm
	ocamlfind ocamlopt dvi.cmx tfm.cmx \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg dviinterp.ml -dtypes
	ocamlfind ocamlopt dvi.cmx tfm.cmx dviinterp.cmx \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg testdviinterp.ml -dtypes -o testdviinterp

testdvi: dvi.ml
	ocamlfind ocamlopt -c -dtypes \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg dvi.ml
	ocamlfind ocamlopt dvi.cmx \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg testdvi.ml -dtypes -o testdvi

dvicairo : testdviinterp dvicairo.ml
	ocamlfind ocamlopt \
	-package bitstring,bitstring.syntax,lablgtk2,cairo,cairo.lablgtk2 \
	-linkpkg \
	gtkInit.cmx dvi.cmx tfm.cmx dviinterp.cmx \
	dvicairo.ml -dtypes -o dvicairo

tfm: tfm.ml
	ocamlfind ocamlopt -c -dtypes \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg tfm.ml -dtypes
	ocamlfind ocamlopt tfm.cmx \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg testtfm.ml -dtypes -o tfm


test: testdvi
	./testdvi hello.dvi math.dvi jfla.dvi

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

clean:
	rm -f dvicairo testdviinterp testdvi tfm *.cmi *.cmo *.cmx *.annot *~