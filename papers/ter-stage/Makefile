
all: rapport.pdf

PDFLATEX:=pdflatex -interaction="errorstopmode"

rapport.pdf: rapport.tex figures.mp
	pdflatex $<
	pdflatex $<

slides.pdf: slides.tex figures.mp 
	pdflatex $<

wc:
	grep -iw frametitle slides.tex | wc -l

%.mp: %.ml
	../../tool.opt -ccopt "-I ../../" -pdf -latex rapport.tex $^


clean:
	rm -f *~ *.aux *.log *.bbl *.blg *.mps
	rm -f rapport.dvi rapport.ps figures.mp
	rm -f mpxerr.dvi mpxerr.tex rapport.pdf

