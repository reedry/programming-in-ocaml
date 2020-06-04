all: bin/ocamlcat bin/ocamlfold bin/ocamlwc

bin/ocamlcat: cat.ml
	ocamlc -o bin/ocamlcat cat.ml

bin/ocamlfold: fold.ml
	ocamlc -o bin/ocamlfold fold.ml

bin/ocamlwc: wc.ml
	ocamlc -o bin/ocamlwc wc.ml

test: bin/ocamlcat bin/ocamlfold bin/ocamlwc
	./test.sh
