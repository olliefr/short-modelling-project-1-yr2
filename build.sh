#!/bin/sh

corebuild -pkgs cohttp.lwt,yojson google_distance_matrix_query.native
corebuild dynamicpr.native
corebuild clusterise.native
