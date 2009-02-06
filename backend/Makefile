all: dviinterp

dviinterp: dviinterp.ml dvi tfm
	ocamlfind ocamlc dvi.cmo tfm.cmo \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg dviinterp.ml -dtypes -o dviinterp

dvi: dvi.ml
	ocamlfind ocamlc -c -dtypes \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg dvi.ml
	ocamlfind ocamlc dvi.cmo \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg testdvi.ml -dtypes -o dvi

tfm: tfm.ml
	ocamlfind ocamlc -c -dtypes \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg tfm.ml -dtypes
	ocamlfind ocamlc tfm.cmo \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg testtfm.ml -dtypes -o tfm


testdvi:
	./dvi hello.dvi math.dvi jfla.dvi

testtfm:
	./tfm cmr10.tfm cmmi10.tfm cmsy10.tfm

clean:
	rm -f dviinterp dvi tfm *.cmi *.cmo *.annot *~