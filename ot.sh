#!/bin/sh
set -e
omake src/top/boot_repl.cma
ocamlc -custom -verbose -I +compiler-libs -linkall ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma   src/clib/clib.a unix.cma boot/lm.cma src/top/boot_repl.cma topstart.cmo -o omaketop
######### don't forget custom #########################
