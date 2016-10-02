OCAML = ocaml

.PHONY: all bootstrap force-bootstrap install default clean doc package

#
# Bootstrap program is omake-boot
#
default: bootstrap

bootstrap:
	$(OCAML) build.ml -auto-bootstrap OCAML="$(OCAML)"

force-bootstrap:
	$(OCAML) build.ml -force-bootstrap OCAML="$(OCAML)"

all:
	$(OCAML) build.ml -build OCAML="$(OCAML)"

install: all
	$(OCAML) build.ml -install OCAML="$(OCAML)"

clean:
	$(OCAML) build.ml -clean OCAML="$(OCAML)"

doc:
	OMAKELIB=`pwd`/lib ./src/main/omake doc

# omake version is taken from the version.txt file!

package:
	OMAKELIB=`pwd`/lib ./src/main/omake clean-package
	OMAKELIB=`pwd`/lib ./src/main/omake package
