all: ocamlcat ocamlfold

ocamlcat: cat.ml
	ocamlc -o ocamlcat cat.ml

ocamlfold: fold.ml
	ocamlc -o ocamlfold fold.ml

test: ocamlcat ocamlfold
	./test.sh
