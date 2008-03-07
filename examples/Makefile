
all:
	rm -f *.1 *.mp *_footer.ml
	echo > index.html
	for f in *.ml; do \
	  b=`basename $$f .ml`; \
	  make $$b.png; \
	  echo '<img src="'$$b.png'">' >> index.html; \
	  caml2html $$f -o $$f.html; \
	  echo '<a href="'$$f.html'">source</a>' >> index.html; \
	  wc -l $$f  >> index.html; \
	  echo "<hr>" >> index.html; \
	done

%_footer.ml: %.ml
	cp -f $^ $@
	echo 'let mpfile = "'$*'.mp"' >> $@
	echo "let () = Command.generate_mp mpfile [1,fig]" >> $@

%.mp: %_footer.ml
	ocaml -I .. ../mlpost.cma $^

%.1: %.mp
	mpost $^ end

%.png: %.1
	sed -e 's/all/$*/' all.tex > $*.tex
	latex $*
	dvips -E $*.dvi
	convert $*.ps $@
	rm -f $*.tex $*.dvi $*.ps

