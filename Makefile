OCAML = ocaml

.PHONY: all bootstrap force-bootstrap install default

#
# Bootstrap program is omake-boot
#
default: bootstrap

bootstrap:
	$(OCAML) build.ml -auto-bootstrap OCAML="$(OCAML)"

force-bootstrap:
	$(OCAML) build.ml -force-bootstrap OCAML="$(OCAML)"

all:
	$(OCAML) build.mk -build OCAML="$(OCAML)"

install: all
	$(OCAML) build.ml -install OCAML="$(OCAML)"

.PHONY: clean
clean:
	$(OCAML) build.ml -clean OCAML="$(OCAML)"
