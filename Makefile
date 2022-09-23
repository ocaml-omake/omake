OCAML = ocaml
OCAMLFLAGS = -safe-string -strict-sequence -I +unix -I +str

.PHONY: all bootstrap force-bootstrap install default clean doc package

#
# Bootstrap program is omake-boot
#
default: bootstrap

bootstrap:
	$(OCAML) $(OCAMLFLAGS) build.ml -auto-bootstrap OCAML="$(OCAML)"

force-bootstrap:
	$(OCAML) $(OCAMLFLAGS) build.ml -force-bootstrap OCAML="$(OCAML)"

all:
	$(OCAML) $(OCAMLFLAGS) build.ml -build OCAML="$(OCAML)"

install:
	$(OCAML) $(OCAMLFLAGS) build.ml -install OCAML="$(OCAML)"

clean:
	$(OCAML) $(OCAMLFLAGS) build.ml -clean OCAML="$(OCAML)"

doc:
	OMAKELIB=`pwd`/lib ./src/main/omake doc

# omake version is taken from the version.txt file!

package:
	OMAKELIB=`pwd`/lib ./src/main/omake clean-package
	OMAKELIB=`pwd`/lib ./src/main/omake package
