BOXPNGFILES := f1.png f2.png traffic.png hierarchy.png
BOXOUTFILES := $(BOXPNGFILES:.png=.1)
PATHPNGFILES := path1.png path2.png
PATHOUTFILES := $(PATHPNGFILES:.png=.1)


all: $(BOXPNGFILES) $(PATHPNGFILES) parse
	caml2html -hc -ext "parse:./parse" boxes.ml paths.ml

%.png: %.ps
	convert $*.ps $@ 

%.ps: %.1 all.template
	sed -e 's/all/$*/' all.template > $*.tex
	latex $*
	dvips -E $*.dvi -o

# dangerous - might be called more than once or even at the same time
$(BOXOUTFILES): boxes.ml
	../tool.opt -ccopt "-I ../ unix.cma" -v $^

$(PATHOUTFILES): paths.ml
	../tool.opt -ccopt "-I ../ unix.cma" -v $^

parse.ml: parse.mll
	ocamllex parse.mll

parse: parse.ml
	ocamlopt.opt -o parse parse.ml

clean:
	rm -f *.aux *.dvi *.ps *.1 *.log $(PNGFILES) *.mp *.mpx *.tex
	rm -f boxes.ml.html paths.ml.html
	rm -f parse.ml *.cmx *.cmo *.cmi parse
