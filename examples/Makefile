# all files that are generated from boxes.ml
BOXPNGFILES := simple.png f1.png f2.png traffic.png hierarchy.png custom.png
BOXOUTFILES := $(BOXPNGFILES:.png=.1)

# all files that are generated from paths.ml
PATHPNGFILES := path1.png path2.png path3.png path4.png path5.png \
		  path6.png path7.png path8.png path9.png path10.png\
		  path11.png path12.png path13.png path14.png path15.png\
		  cheno11.png cheno14.png
PATHOUTFILES := $(PATHPNGFILES:.png=.1)

# all files that are generated from misc.ml
MISCPNGFILES := bresenham.png cfg.png chess.png diag1.png diag2.png\
		drums.png escher.png other208.png proval.png\
		randtree.png rubik.png test.png lattice.png football.png
MISCOUTFILES := $(MISCPNGFILES:.png=.1)

# all files that are generated from tree.ml
TREEPNGFILES := tree1.png tree2.png tree3.png tree4.png tree5.png \
                tree6.png tree7.png tree8.png tree9.png tree10.png
TREEOUTFILES := $(TREEPNGFILES:.png=.1)

# all files that are generated from labels.ml
LABELPNGFILES := handbook3.png other27.png
LABELOUTFILES := $(LABELPNGFILES:.png=.1)

# all files that are generated from automata.ml
AUTOMPNGFILES := automaton1.png automaton2.png automaton4.png 
AUTOMOUTFILES := $(AUTOMPNGFILES:.png=.1)

HISTPNGFILES := hist1.png
HISTOUTFILES := $(HISTPNGFILES:.png=.1)

RADARPNGFILES := radar1.png radar2.png
RADAROUTFILES := $(RADARPNGFILES:.png=.1)

HTMLFILES := boxes.ml.html paths.ml.html misc.ml.html tree.ml.html \
             label.ml.html automata.ml.html hist.ml.html radar.ml.html

CAIRO := cairo_test.ml

MLPOST:=mlpost -v -ps

all: $(BOXPNGFILES) $(PATHPNGFILES) $(MISCPNGFILES) \
     $(TREEPNGFILES) $(LABELPNGFILES) $(AUTOMPNGFILES) $(HISTPNGFILES) \
     $(RADARPNGFILES)
html: $(HTMLFILES)
%.ml.html : %.ml parse
	caml2html -hc -ext "parse:./parse" $*.ml


%.png: %.ps
	convert $*.ps $@ 

%.ps: %.1 all.template
	sed -e 's/all/$*/' all.template > $*.tex
	latex $*
	dvips -E $*.dvi -o

automaton4.ps: automaton4.1 
	latex automaton4.tex
	dvips -E automaton4.dvi -o

# dangerous - might be called more than once or even at the same time
$(BOXOUTFILES): boxes.dummy
$(PATHOUTFILES): paths.dummy
$(MISCOUTFILES): misc.dummy
$(TREEOUTFILES): tree.dummy
$(LABELOUTFILES): label.dummy
$(AUTOMOUTFILES): automata.dummy
$(HISTOUTFILES): hist.dummy
$(RADAROUTFILES): radar.dummy


%.dummy: %.ml
	$(MLPOST) $^
	touch $@

parse.ml: parse.mll
	ocamllex parse.mll

parse: parse.ml
	ocamlopt.opt -o parse parse.ml

BOXTEXFILES:=$(BOXPNGFILES:.png=.tex)
PATHTEXFILES:=$(PATHPNGFILES:.png=.tex)
MISCTEXFILES:=$(MISCPNGFILES:.png=.tex)
TREETEXFILES:=$(TREEPNGFILES:.png=.tex)
LABELTEXFILES:=$(LABELPNGFILES:.png=.tex)
clean:
	rm -f *.aux *.dvi *.ps *.1 *.log $(PNGFILES) *.mp *.mpx
	rm -f $(BOXTEXFILES) $(PATHTEXFILES) $(MISCTEXFILES) $(TREETEXFILES)
	rm -f $(LABELTEXFILES)
	rm -f automaton1.tex automaton2.tex
	# do not remove automaton4.tex - it's handmade
	rm -f boxes.ml.html paths.ml.html misc.ml.html tree.ml.html label.ml.html
	rm -f parse.ml *.cmx *.cmo *.cmi parse *.o
	rm -f *.png *.dummy
