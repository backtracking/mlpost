BUILD=_build

# TO suppress
OCAMLBUILD=ocamlbuild
OCAMLFIND=ocamlfind

all :
	$(OCAMLBUILD) -I $(MLPOST_LIB) -tag dtypes  -no-links mlpost_dot.cma mlpost_dot.cmxa mlpost_dot.a dot.cmi -classic-display

install : 
	$(OCAMLFIND) remove mlpost_dot
	$(OCAMLFIND) install mlpost_dot $(BUILD)/mlpost_dot.cma $(BUILD)/mlpost_dot.cmxa $(BUILD)/mlpost_dot.a $(BUILD)/dot.cmi META

clean :
	$(OCAMLBUILD) -clean