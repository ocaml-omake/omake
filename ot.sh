#!/bin/sh
/usr/local/bin/ocamlc -custom -verbose -I +compiler-libs -linkall ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma   src/clib/clib.a unix.cma boot/lm.cma src/top/boot_repl.cma topstart.cmo -o omaketop
######### don't forget custom #########################
