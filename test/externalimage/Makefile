all: powered-by-caml.png externalimage.ml externalimage.tex
	mlpost -pdf externalimage.ml
	pdflatex externalimage.tex

clean:
	rm -f *.pdf *.aux *.log *.mp *.mps *.1