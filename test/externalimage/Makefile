all: powered-by-caml.png externalimage.ml externalimage.tex
	../../tool.opt -ccopt "-I ../../" -pdf externalimage.ml
	pdflatex externalimage.tex

clean:
	rm -f *.pdf *.aux *.log *.mp *.mps *.1
