PPX_MOCK=./ppx_mock.opt

all: $(PPX_MOCK)

%.byte: %.ml
	ocamlfind c -package compiler-libs.bytecomp -linkpkg $< -o $@

%.opt: %.ml
	ocamlfind opt -package compiler-libs.bytecomp -linkpkg $< -o $@

%.cmx: %.ml
	ocamlfind opt -c $<

.PHONY: test
test: test_simple.ml $(PPX_MOCK)
	ocamlfind opt -c -ppx $(PPX_MOCK) $<
