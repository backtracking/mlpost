MAX := 17
# all files that are generated from boxes.ml
BOXESPNG := $(foreach i,$(shell seq 1 8), boxes$(i).png)

# all files that are generated from paths.ml
PATHSPNG := $(foreach i,$(shell seq 1 17), paths$(i).png)

# all files that are generated from misc.ml
MISCPNG := $(foreach i,1 2 $(shell seq 4 9) $(shell seq 11 14), misc$(i).png)

# all files that are generated from tree.ml
TREEPNG := $(foreach i,$(shell seq 1 14), tree$(i).png)


# all files that are generated from label.ml
LABELPNG := $(foreach i,$(shell seq 1 2), label$(i).png)

# all files that are generated from automata.ml
AUTOMPNG := automata1.png automata2.png automata4.png

HISTPNG := hist1.png

RADARPNG := radar1.png radar2.png

REALPLOTPNG := real_plot1.png real_plot2.png

COLORPNG := color1.png color2.png color3.png color4.png

DOTPNG := dot1_dot.png dot2_dot.png

HTMLFILES := boxes.ml.html paths.ml.html misc.ml.html tree.ml.html \
             label.ml.html automata.ml.html hist.ml.html radar.ml.html\
	     real_plot.ml.html dot.ml.html color.ml.html

CAIRO = # -cairo

MLPOST:=mlpost -v -ps $(CAIRO)

all: $(BOXESPNG) $(PATHSPNG) $(MISCPNG) $(TREEPNG) $(LABELPNG) $(AUTOMPNG)\
	$(HISTPNG)  $(RADARPNG) $(REALPLOTPNG) $(COLORPNG)

contrib: $(DOTPNG)

html: $(HTMLFILES)
%.ml.html : %.ml parse
	caml2html -hc -ext "parse:./parse" $*.ml


%.png: %.ps
	convert -density 216 $*.ps  $@

%.ps: %.1 all.template
	sed -e 's/all/$*/' all.template > $*.tex
	latex $*
	dvips -E $*.dvi -o

%.pdf: %.mps all.template2
	sed -e 's/all/$*/' all.template2 > $*.tex
	pdflatex $*

%.mps: %.1
	cp $*.1 $*.mps

#automaton4.ps: automaton4.1
#	latex automaton4.tex
#	dvips -E automaton4.dvi -o

# dangerous - might be called more than once or even at the same time

ifeq "$(CAIRO)" "-cairo"
$(foreach i,$(shell seq 1 $(MAX)), %$(i)_dot.ps): %_dot.ml
	@echo MLPOST $^
	$(MLPOST) -contrib dot $^

$(foreach i,$(shell seq 1 $(MAX)), %$(i).ps) : %.ml
	$(MLPOST) $^

else
$(foreach i,$(shell seq 1 $(MAX)), %$(i)_dot.1): %_dot.ml
	@echo MLPOST $^
	$(MLPOST) -contrib dot $^

$(foreach i,$(shell seq 1 $(MAX)), %$(i).1) : %.ml
	$(MLPOST) $^
endif

parse.ml: parse.mll
	ocamllex parse.mll

parse: parse.ml
	ocamlopt.opt -o parse parse.ml

BOXTEXFILES:=$(BOXPNG:.png=.tex)
PATHTEXFILES:=$(PATHPNG:.png=.tex)
MISCTEXFILES:=$(MISCPNG:.png=.tex)
TREETEXFILES:=$(TREEPNG:.png=.tex)
LABELTEXFILES:=$(LABELPNG:.png=.tex)
clean:
	rm -f *.aux *.dvi *.ps *.1 *.log $(PNGFILES) *.mp *.mpx *.mps
	rm -f $(BOXTEXFILES) $(PATHTEXFILES) $(MISCTEXFILES) $(TREETEXFILES)
	rm -f $(LABELTEXFILES)
	rm -f automaton1.tex automaton2.tex
	# do not remove automaton4.tex - it's handmade
	rm -f boxes.ml.html paths.ml.html misc.ml.html tree.ml.html label.ml.html
	rm -f parse.ml *.cmx *.cmo *.cmi parse *.o
	rm -f *.png *.dummy *.dummy_dot


editor2 :
	ocamlbuild editor2.native

lattice_lablgtk : lattice_lablgtk.ml
	$(MLPOST) -contrib lablgtk lattice_lablgtk.ml
