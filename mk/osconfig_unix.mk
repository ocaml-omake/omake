#
# System config
#
LN = ln -sf
RM = rm -f
DOT = ./
slash = /

win32 = unix
system = null

#
# C configuration
#
CC = cc
CFLAGS = -I"$(STDLIB)" -I"$(STDLIB)/caml" 
AR = ar cq
AROUT =
EXT_OBJ = .o
EXT_LIB = .a
EXE =
CCOMPTYPE = cc

OCAMLFLAGS = -w +a-4-32-30-42-40-41 -g $(OCAMLFLAGS_EXTRA)
THREADSLIB =
THREADSLIB_OPT =
PREFERRED = .byte
STDLIB := $(shell ocamlc -where)

.SUFFIXES: .mll .mly .mli .ml .c .cmi .cmo .cmx .cma .cmxa .o

.c.o:
	$(CC) $(CFLAGS) -c $*.c

#
# OCaml configuration
#
OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex.opt
OCAMLDEP = ocamldep.opt

.mly.ml:
	$(OCAMLYACC) $*.mly

.mly.mli:
	$(OCAMLYACC) $*.mly

.mll.ml:
	$(OCAMLLEX) $*.mll

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $*.mli

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $*.ml
#
# The version.txt file
#
version.txt:
	@echo 0.0.boot > $@
