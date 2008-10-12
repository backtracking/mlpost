
all: main-pp.pdf

WWW=/users/demons/filliatr/WWW

main-pp.pdf: main-pp.tex biblio.bib figures.mp figmp.mps yannick1.mps florence.mps
	pdflatex $<
	bibtex main-pp
	pdflatex $<
	bibtex main-pp
	pdflatex $<
	pdflatex $<

%-pp.tex: pp.ml %.tex 
	ocaml $^ > $@

figures.mp: figures.ml
	../../tool.opt -ccopt "-I ../../" -pdf -latex main.tex $^

figmp.mps: figmp.mp
	mpost figmp.mp end
	mv figmp.1 figmp.mps

yannick1.mps: yannick1.1
	cp $^ $@

florence.mps: florence.1
	cp $^ $@

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
	rm -f *~ *.aux *.log *.bbl *.blg *.mps
	rm -f main-pp.dvi main-pp.ps figures.mp
	rm -f mpxerr.dvi mpxerr.tex main-pp.pdf

