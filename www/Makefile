
WWW = /users/www-perso/projets/mlpost/

ALL =  	index.fr.html  index.en.html index.de.html \
         manual.fr.html manual.en.html manual.de.html

install: all
	cp -f --parents $(ALL) $(WWW)

all: $(ALL) 

# RULES

YAMLPP=yamlpp macros.prehtml version.prehtml

.SUFFIXES: .html .prehtml .fr.html .en.html

%.fr.html: %.prehtml
	$(YAMLPP) -l fr $< -o $@

%.en.html: %.prehtml
	$(YAMLPP) -l en $< -o $@

%.de.html: %.prehtml
	$(YAMLPP) -l de $< -o $@

