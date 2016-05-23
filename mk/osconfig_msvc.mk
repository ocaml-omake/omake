#
# System config
#
LN = copy /Y
RM = del /F
DOT =
slash = \\

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
CC = cl
CFLAGS = /nologo /MT -I"$(STDLIB)" /DWIN32
#  /DFAM_ENABLED /DFAM_PSEUDO
AR = lib /nologo /debugtype:CV
AROUT = /out:
EXT_OBJ = .obj
EXT_LIB = .lib
EXE = .exe
CCOMPTYPE = msvc
STDLIB := $(shell $(OCAMLC) -where)

OCAMLFLAGS = -thread -w +a-4-32-30-42-40-41 -g $(OCAMLFLAGS_EXTRA)
THREADSLIB = threads.cma
THREADSLIB_OPT = threads.cmxa
PREFERRED = .byte

.SUFFIXES: .mll .mly .mli .ml .c .cmi .cmo .cmx .cma .cmxa .obj

.c.obj:
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


# Entry point
omake: omake.exe
	@echo
