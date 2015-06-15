LN = ln -sf

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
	@$(LN) boot/omake omake-boot

bootstrap-mingw:
	@$(MAKE) boot/Makefile LN=cp
	@CC=`ocamlc -config | grep bytecomp_c_compiler | awk '{print $$2}'`; cd boot; $(MAKE) Makefile.dep LN=cp win32=win32 system=system; $(MAKE) omake.exe LN=cp OCAMLFLAGS_EXTRA=-thread THREADSLIB=threads.cma EXE=.exe "CC=$$CC" win32=win32 system=system
	@cp boot/omake omake-boot

boot/Makefile: src/Makefile
	mkdir -p boot
	@touch boot/Makefile.dep
	@sleep 1
	cd boot && $(LN) ../src/Makefile Makefile

all:
	touch .config
	OMAKEFLAGS= OMAKEPATH=lib ./omake-boot --dotomake .omake --force-dotomake  main
	OMAKEFLAGS= OMAKEPATH=lib src/main/omake --dotomake .omake --force-dotomake  all

install: all
	OMAKEFLAGS= OMAKEPATH=lib src/main/omake --dotomake .omake --force-dotomake  install
