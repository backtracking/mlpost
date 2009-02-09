MLPOST:=mlpost -v
TARGETS:= circle.png rect.png ellipse.png round_rect.png patatoid.png tex.png\
          ctr.png north.png south.png west.png east.png north_west.png \
	  south_west.png north_east.png south_east.png

targets: $(TARGETS)
         

palette.ps: palette.1 
	latex palette.tex
	dvips -E palette.dvi -o

%.png: %.ps
	convert $*.ps $@ 

%.ps: %.1 all.template
	sed -e 's/all/$*/' all.template > $*.tex
	latex $*
	dvips -E $*.dvi -o

%.1: box.ml
	$(MLPOST) $^

clean:
	rm -f *.png *.tex *.dvi *.ps *.aux *.mp *.html *.mps *.1 *.log style.css *.mpx
