
all: main-pp.pdf

WWW=/users/demons/filliatr/WWW

%.pdf: %.tex biblio.bib figures.ml
	pdflatex $*.tex
	bibtex $*
	pdflatex $*.tex
	bibtex $*
	pdflatex $*.tex
	pdflatex $*.tex

%-pp.tex: pp.ml %.tex 
	ocaml $^ > $@

figures.mp: figures.ml
	../../tool.opt -ccopt "-I ../../" -pdf -latex main.tex $^

main-pp.dvi: main-pp.tex
	latex $^

pp.ml: pp.mll
	ocamllex pp.mll

print: main-pp.ps
	lpr $^

export: main-pp.ps 
	cp -f main-pp.ps mlpost.ps
	ps2pdf mlpost.ps
	cp -f mlpost.pdf $(WWW)/publis/

clean:
	rm -f *~ *.aux *.log *.bbl *.blg
	rm -f main-pp.dvi main-pp.ps figures.mp
	rm -f mpxerr.dvi mpxerr.tex main-pp.pdf

