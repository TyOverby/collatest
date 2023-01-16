#!/usr/bin/env bash 

opam switch create ./ ocaml-base-compiler.4.12.1
eval $(opam env)
opam install ocamlformat merlin core dune
