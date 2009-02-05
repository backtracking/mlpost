all:
	ocamlfind ocamlc \
	-package bitstring,bitstring.syntax -syntax bitstring.syntax \
	-linkpkg dvi.ml -dtypes -o dvi

test:
	dvi hello.dvi math.dvi jfla.dvi

clean:
	rm -f dvi dvi.cmi dvi.cmo dvi.annot *~
