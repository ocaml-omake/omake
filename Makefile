#
# For bootstrapping
#
.PHONY: all bootstrap install default

#
# Bootstrap program is omake-boot
#
default:
	@echo "If you have already built omake, you should use it instead of make."
	@echo "If you need to bootstrap, use "
	@echo " - 'make bootstrap',"
	@echo "       to build the bootstrapping (feature-limited) OMake binary './omake-boot'."
	@echo " - 'make all',"
	@echo "       to bootstrap and then build everything"
	@echo " - 'make install',"
	@echo "       to bootstrap, build, and install everything"
	@exit 1

bootstrap: boot/Makefile
	@cd boot; $(MAKE) Makefile.dep; $(MAKE) omake
	@ln -sf boot/omake omake-boot

boot/Makefile: src/Makefile
	mkdir -p boot
	@touch boot/Makefile.dep
	@sleep 1
	ln -sf ../src/Makefile boot/Makefile

all: bootstrap
	touch .config
	OMAKEFLAGS= OMAKEPATH=lib ./omake-boot --dotomake .omake --force-dotomake -j2 main
	OMAKEFLAGS= OMAKEPATH=lib src/main/omake --dotomake .omake --force-dotomake -j2 all

install: all
	OMAKEFLAGS= OMAKEPATH=lib src/main/omake --dotomake .omake --force-dotomake -j2 install
