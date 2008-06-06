pdf: slides.pdf

slides.pdf: cfg.mps proval.mps slides.tex
	pdflatex slides.tex

%.mps: %.ml
	mlpost -pdf -latex slides.tex $^
