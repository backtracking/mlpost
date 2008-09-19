MLFILES := $(shell echo *.ml)
PNGFILES := $(MLFILES:.ml=.png)

all: $(PNGFILES)
	rm -f *.1 *.mp *_footer.ml
	echo > index.html
	for f in *.ml; do \
	  b=`basename $$f .ml`; \
	  echo '<img src="'$$b.png'">' >> index.html; \
	  caml2html $$f -o $$f.html; \
	  echo "<br>" >> index.html; \
	  echo '<a href="'$$f.html'">' >> index.html; \
	  wc -l $$f  >> index.html; \
	  echo '</a>' >> index.html; \
	  echo "<hr>" >> index.html; \
	done

$(PNGFILES): ../mlpost.cma

%_footer.ml: %.ml
	cp -f $^ $@
	echo 'let () = Metapost.emit "$*" fig' >> $@

%.1: %_footer.ml
	../tool.opt -ccopt "-I ../ unix.cma" -v $^

%.png: %.ps
	convert $*.ps $@ 

%.ps: %.1
	sed -e 's/all/$*/' all.template > $*.tex
	latex $*
	dvips -E $*.dvi -o

%: %.ps
	gv $^

clean:
	rm -f *.log *.aux *.mpx *.mp *.dvi *.png *.cmi *.cmo *.html *.tex *.ps
	rm -f *_footer\.*
