PNGFILES := f1.png f2.png
OUTFILES := $(PNGFILES:.png=.1)


all: $(PNGFILES)
	caml2html -hc boxes.ml

%.png: %.ps
	convert $*.ps $@ 

%.ps: %.1 all.template
	sed -e 's/all/$*/' all.template > $*.tex
	latex $*
	dvips -E $*.dvi -o

# dangerous - might be called more than once or even at the same time
$(OUTFILES): boxes.ml
	../tool.opt -ccopt "-I ../ unix.cma" -v $^

clean:
	rm -f *.aux *.dvi *.ps *.1 *.log $(PNGFILES) *.mp *.mpx *.tex *.html
