#!/bin/bash

#set -x

# configure, build and install via OASIS
oasis setup
ocaml setup.ml -uninstall
ocaml setup.ml -configure
ocaml setup.ml -build
#ocamldoc -html lib/interval_tree.mli # build doc
ocaml setup.ml -install
