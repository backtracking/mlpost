pdf: slides.pdf

slides.pdf: cfg.mps proval.mps slides.tex
	pdflatex slides.tex

%.mps: %.ml
	mlpost -pdf -latex slides.tex $^

clean:
	rm -f *.toc *.aux *.pdf *.mp *.log *~ *.mp[sx] *.snm *.nav *.out