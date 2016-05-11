#
# System config
#
LN = cp
RM = rm -f
DOT = ./
slash = /

win32 = win32
system = system

#
# OCaml configuration
#
OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex.opt
OCAMLDEP = ocamldep.opt

#
# C configuration
#
CC := $(sh -c "shell $(OCAMLC) -config | grep bytecomp_c_compiler | awk '{print $$2}'")
CFLAGS = -I"$(STDLIB)" -I"$(STDLIB)/caml" 
AR = ar cq
AROUT =
EXT_OBJ = .o
EXT_LIB = .a
EXE =
CCOMPTYPE = cc

OCAMLFLAGS = -thread -w +a-4-32-30-42-40-41 -g $(OCAMLFLAGS_EXTRA)
THREADSLIB = threads.cma
THREADSLIB_OPT = threads.cmxa
PREFERRED = .byte
STDLIB := $(shell $(OCAMLC) -where)

.SUFFIXES: .mll .mly .mli .ml .c .cmi .cmo .cmx .cma .cmxa .o

.c.o:
	$(CC) $(CFLAGS) -c $*.c

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
