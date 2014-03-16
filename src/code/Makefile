%.class: %.java
	javac $<

%.cmx: %.ml
	ocamlopt $<

%.cmi: %.mli
	ocamlopt $<

test_double.opt: test_double.cmx
	ocamlopt -o $@ $^
