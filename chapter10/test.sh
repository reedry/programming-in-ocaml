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

assert "cat cat.ml" "./ocamlcat cat.ml"
assert "cat -n cat.ml" "./ocamlcat -n cat.ml"
assert "fold fold.ml" "./ocamlfold fold.ml"
assert "fold --width 2 fold.ml" "./ocamlfold --width 2 fold.ml"
assert "fold --width 37 fold.ml" "./ocamlfold --width 37 fold.ml"
