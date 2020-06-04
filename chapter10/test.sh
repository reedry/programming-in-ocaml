#!/bin/bash

assert() {
  $1 >& left.tmp
  $2 >& right.tmp
  if diff left.tmp right.tmp; then
    echo "OK"
  else
    echo "failed"
  fi
}

assert "cat cat.ml" "./bin/ocamlcat cat.ml"
assert "cat -n cat.ml" "./bin/ocamlcat -n cat.ml"
assert "fold fold.ml" "./bin/ocamlfold fold.ml"
assert "fold --width 2 fold.ml" "./bin/ocamlfold --width 2 fold.ml"
assert "fold --width 37 fold.ml" "./bin/ocamlfold --width 37 fold.ml"
assert "wc wc.ml" "./bin/ocamlwc wc.ml"
assert "wc -c fold.ml" "./bin/ocamlwc -c fold.ml"
assert "wc -l cat.ml" "./bin/ocamlwc -l cat.ml"
assert "wc -w wc.ml" "./bin/ocamlwc -w wc.ml"
assert "wc -l -w fold.ml" "./bin/ocamlwc -l -w fold.ml"
assert "wc -c -l -w cat.ml" "./bin/ocamlwc -c -l -w cat.ml"
