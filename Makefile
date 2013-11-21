#
# Makefile for Short Modelling Project Yr 2
# Ollie Frolovs, 2013
#

# TODO this builds .native by default; option to build .byte?

# Let corebuild handle OCaml dependencies, it's good at that
.PHONY: all google_distance_matrix_query clusterise dynamicpr clean

all : google_distance_matrix_query clusterise dynamicpr

google_distance_matrix_query : 
	corebuild -pkgs cohttp.lwt,yojson google_distance_matrix_query.native

clusterise :
	corebuild clusterise.native

dynamicpr :
	corebuild dynamicpr.native

clean :
	corebuild -clean
