PACKS=
OCAMLC=ocamlfind c $(PACKS)
OCAMLOPT=ocamlfind opt $(PACKS)

default: all

all: .depends mock_example.byte depinj.byte

depinj.byte: depinj.cmo
	$(OCAMLC) $^ -o $@

mock_example.byte: mock_example.cmo
	ocamlc $^ -o $@

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.depends:
	ocamldep *.ml *.mli > .depends

clean:
	rm -f mock_example depinj *.cm[iox]

include .depends
