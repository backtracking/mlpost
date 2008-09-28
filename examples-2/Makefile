PNGFILES := f1.png f2.png
OUTFILES := $(PNGFILES:.png=.1)

all: $(PNGFILES) mydocgen.cmo
	ocamldoc -g mydocgen.cmo -I .. boxes.ml

mydocgen.cmo: mydocgen.ml
	ocamlc -I +ocamldoc -c mydocgen.ml

%.png: %.ps
	convert $*.ps $@ 

%.ps: %.1 all.template
	sed -e 's/all/$*/' all.template > $*.tex
	latex $*
	dvips -E $*.dvi -o

boxes.cmo:
	ocamlc -c ../mlpost.cma boxes.ml
# dangerous - might be called more than once or even at the same time
$(OUTFILES): boxes.ml
	../tool.opt -ccopt "-I ../ unix.cma" -v boxes.ml

clean:
	rm -f *.aux *.dvi *.ps *.1 *.log $(PNGFILES) *.mp *.mpx *.tex
	rm -f *html
	rm -f *cmo *cmi style.css
