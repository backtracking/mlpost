pdf: slides.pdf

slides.pdf: cfg.mps proval.mps mpex.mps slides.tex
	pdflatex slides.tex

%.mps: %.ml
	mlpost -pdf -latex slides.tex $^

clean:
	rm -f *.toc *.aux *.pdf cfg.mp proval.mp *.log *~ *.mp[sx] *.snm *.nav *.out

%.mps: %.1
	mv $^ $@

%.1: %.mp
	mpost $^ end
