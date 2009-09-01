# misc
######
dep:
	$(OCAMLDEP) *.mli *.ml | ocamldot | dot -Tps | $(PSVIEWER) -

wc:
	ocamlwc *.ml* backend/*.ml* -p

# headers
#########

headers:
	rm -f mlpost.mli
	headache -c headache_config.txt -h header.txt \
	 *.in README.txt *.mli *.ml *.mll backend/*.ml backend/*.ml[iyl] 
	./config.status

# installation
##############

install: install-$(OCAMLBEST) install-bin


ifeq "$(OCAMLFIND)" "no"
install-byte:
	mkdir -p $(LIBDIR)
	cp -f $(BUILD)mlpost.cmi META $(BUILD)$(CMA) "$(LIBDIR)"

install-opt:
	mkdir -p $(LIBDIR)
	cp -f $(BUILD)mlpost.cmi META $(BUILD)$(CMA) "$(LIBDIR)"
	cp -f $(BUILD)$(CMXA) "$(LIBDIR)"
else
DESTDIR=-destdir $(LIBDIR:/mlpost=)

install-byte:
	ocamlfind remove $(DESTDIR) mlpost
	ocamlfind install $(DESTDIR) mlpost $(BUILD)mlpost.cmi META $(BUILD)$(CMA)

install-opt:
	ocamlfind remove $(DESTDIR) mlpost
	ocamlfind install $(DESTDIR) mlpost $(BUILD)mlpost$(LIBEXT) $(BUILD)mlpost.cmi META $(BUILD)$(CMXA) $(BUILD)$(CMA)
endif

install-bin:
	mkdir -p $(BINDIR)
	cp -f $(BUILD)tool.native $(BINDIR)/mlpost
	cp -f mlpost.1 $(MANDIR)/man1

uninstall:
	rm -f $(LIBDIR)/mlpost.cmo $(LIBDIR)/mlpost.cmi $(LIBDIR)/META 
	rm -f $(LIBDIR)/mlpost$(LIBEXT) $(LIBDIR)/mlpost.cmx $(LIBDIR)/$(CMA) $(LIBDIR)/$(CMXA)
	rm -f $(BINDIR)/mlpost
	rm -f $(MANDIR)/mlpost

# export
########

EXPORTDIR=$(NAME)-$(MLPOSTVERSION)
TAR=$(EXPORTDIR).tar

WWW = /users/www-perso/projets/mlpost
FTP = $(WWW)/download

FILES := $(wildcard *.ml) $(wildcard *.mli) $(wildcard *.mll) \
	 $(wildcard *.in) configure README.txt INSTALL LICENSE CHANGES FAQ \
	 mlpost.1 _tags *.mlpack mlpost.odocl
BACKENDFILES = backend/*ml backend/*mly backend/*mll backend/*mli backend/_tags
GENERATEDSOURCEFILES = mlpost.mli version.ml myocamlbuild.ml
GUIFILES = gui/*.mll gui/*.ml gui/_tags 
EXFILES = examples/Makefile examples/*.ml examples/all.template\
	  examples/index.html examples/parse.mll examples/README examples/automaton4.tex
CUSTOMDOCFILES = customdoc/all.template customdoc/img_doc.ml customdoc/img.ml \
		 customdoc/Makefile customdoc/_tags
LATEXFILES = latex/*sty latex/*tex latex/README

export: export-source export-www export-examples export-doc
	cp README.txt INSTALL LICENSE CHANGES FAQ $(FTP)

export-source: source
	cp export/$(TAR).gz $(FTP)

source: 
	mkdir -p export/$(EXPORTDIR)
	cp $(filter-out $(GENERATEDSOURCEFILES), $(FILES)) export/$(EXPORTDIR)
	mkdir -p export/$(EXPORTDIR)/backend
	cp $(BACKENDFILES) export/$(EXPORTDIR)/backend
	mkdir -p export/$(EXPORTDIR)/gui
	cp $(GUIFILES) export/$(EXPORTDIR)/gui
	mkdir -p export/$(EXPORTDIR)/examples
	cp $(EXFILES) export/$(EXPORTDIR)/examples
	mkdir -p export/$(EXPORTDIR)/customdoc
	cp $(CUSTOMDOCFILES) export/$(EXPORTDIR)/customdoc
	mkdir -p export/$(EXPORTDIR)/latex
	cp $(LATEXFILES) export/$(EXPORTDIR)/latex
	cd export ; tar cf $(TAR) $(EXPORTDIR) ; gzip -f --best $(TAR)


DOCFILES:=$(shell echo *.mli)
DOCFILES:=$(filter-out types.mli, $(DOCFILES))

export-doc: doc
	cp -r doc/*.html doc/img/*.png doc/style.css $(WWW)/doc

export-www: www/version.prehtml
	make -C www

www/version.prehtml: Makefile
	echo "<#def version>$(MLPOSTVERSION)</#def>" > www/version.prehtml

export-examples: 
	$(MAKEEXAMPLES)
	cp -f --parents examples/*.png examples/*.html $(WWW)

# Emacs tags
############

tags:
	find . -name "*.ml*" | sort -r | xargs \
	etags "--regex=/let[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/let[ \t]+rec[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/and[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/type[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/exception[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/val[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/module[ \t]+\([^ \t]+\)/\1/"

.PHONY: ocamlwizard
ocamlwizard:
	ocamlrun -bt ocamlwizard compile types.mli $(CMO:.cmo=.ml) mlpost.mli

# Makefile is rebuilt whenever Makefile.in or configure.in is modified
######################################################################

Makefile META version.ml mlpost.mli myocamlbuild.ml: Makefile.in ocamlbuild.Makefile.in META.in version.ml.in config.status myocamlbuild.ml.in mlpost.mli.in
	./config.status
	chmod a-w mlpost.mli myocamlbuild.ml META Makefile ocamlbuild.Makefile version.ml

config.status: configure
	./config.status --recheck

configure: configure.in
	autoconf 

