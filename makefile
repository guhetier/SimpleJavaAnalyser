
all: analyzer.native

analyzer.native: analyzer.ml
	ocamlbuild analyzer.native
