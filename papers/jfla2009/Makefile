
all: main-pp.ps

WWW=/users/demons/filliatr/WWW

%.ps: %.tex biblio.bib
	latex $*.tex
	bibtex $*
	latex $*.tex
	bibtex $*
	latex $*.tex
	latex $*.tex
	dvips $*.dvi -o

%-pp.tex: pp.ml %.tex 
	ocaml $^ > $@

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
	rm -f *~ *.aux *.log *.bbl *.blg *-pp.*
	rm -f main-pp.dvi main-pp.ps

