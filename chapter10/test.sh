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
