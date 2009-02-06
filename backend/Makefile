all: dviinterp

dviinterp: dviinterp.ml dvi tfm
	ocamlfind ocamlc dvi.cmo tfm.cmo \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg dviinterp.ml -dtypes -o dviinterp

dvi: dvi.ml
	ocamlfind ocamlc \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg dvi.ml -dtypes -o dvi

tfm: tfm.ml
	ocamlfind ocamlc \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg tfm.ml -dtypes -o tfm

testdvi:
	./dvi hello.dvi math.dvi jfla.dvi

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

clean:
	rm -f dvi tfm *.cmi *.cmo *.annot *~