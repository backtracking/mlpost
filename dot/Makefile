BUILD=_build

all :
	ocamlbuild -tag dtypes  -no-links mlpost_dot.cma mlpost_dot.cmxa mlpost_dot.a dot.cmi -classic-display

install : 
	ocamlfind remove mlpost_dot
	ocamlfind install mlpost_dot $(BUILD)/mlpost_dot.cma $(BUILD)/mlpost_dot.cmxa $(BUILD)/mlpost_dot.a $(BUILD)/dot.cmi META

clean :
	ocamlbuild -clean