#!/bin/sh

set -e

# Ollie Frolovs, 2013
# FIXME this script need a review, badly

# Built by example from: 
# http://nicolaspouillard.fr/ocamlbuild/ocamlbuild-user-guide.html

# FIXME automate targets?
# TODO .native assumed, but nice to have an option to build .byte

rule() {
  case $1 in
    clean)      corebuild -clean;;
    google_distance_matrix_query) corebuild -pkgs cohttp.lwt,yojson google_distance_matrix_query.native;;
    clusterise) corebuild clusterise.native;;
    dynamicpr)  corebuild dynamicpr.native;;
    all)     rule google_distance_matrix_query; rule clusterise; rule dynamicpr;;
    depend)  echo "not required";;
    *)       echo "unknown action $1";;
  esac;
}

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi
