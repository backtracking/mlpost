FIGURES:= fibtree.pdf

$(FIGURES): figures.ml
	mlpost -cairo $<

all: slides.pdf

view: slides.pdf
	xpdf slides.pdf
slides.pdf: $(FIGURES)

%.pdf: %.tex
	pdflatex $<
