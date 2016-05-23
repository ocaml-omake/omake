LN = ln -sf

#
# For bootstrapping
#
.PHONY: all bootstrap-byte bootstrap-opt bootstrap bootstrap-mingw install default
.PHONY: all-after-boot all-non-boot install-after-boot install-non-boot

#
# Bootstrap program is omake-boot
#
default:
	@if [ -f .config ]; then $(MAKE) all; else \
	   if [ -f mk/preconfig ]; then $(MAKE) bootstrap && $(MAKE) all; else \
	       echo "Unconfigured. Run the configure script first!" >&2; exit 1; \
	fi; fi

bootstrap-byte: boot/Makefile
	@cd boot; $(MAKE) Makefile.dep; $(MAKE) omake
	@$(LN) boot/omake omake-boot

bootstrap-opt: boot/Makefile
	@cd boot; $(MAKE) Makefile.dep; $(MAKE) omake PREFERRED=.opt
	@$(LN) boot/omake omake-boot

bootstrap:
	mkdir -p boot
	system=`ocamlc -config 2>/dev/null|grep '^system'|sed 's/system: //'`; \
	case "$system" in \
	  mingw|mingw64) \
	    $(MAKE) bootstrap-mingw ;; \
	  *) \
	    ocamlopt.opt -v 2>/dev/null && \
	       $(MAKE) bootstrap-opt || $(MAKE) bootstrap-byte ;; \
	esac

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
	@if [ -f ./omake-boot ]; then $(MAKE) all-after-boot; else $(MAKE) all-non-boot; fi

all-after-boot:
	touch .config
	OMAKEFLAGS= OMAKEPATH=lib ./omake-boot --dotomake .omake --force-dotomake  main
	OMAKEFLAGS= OMAKEPATH=lib src/main/omake --dotomake .omake --force-dotomake  all

all-non-boot:
	@echo "*********************************************"
	@echo "WARNING: No omake-boot, using omake from PATH"
	@echo "*********************************************"
	OMAKEFLAGS= OMAKEPATH=lib omake --dotomake .omake

install: all
	@if [ -f ./omake-boot ]; then $(MAKE) install-after-boot; else $(MAKE) install-non-boot; fi

install-after-boot:
	OMAKEFLAGS= OMAKEPATH=lib src/main/omake --dotomake .omake --force-dotomake  install

install-non-boot:
	OMAKEFLAGS= OMAKEPATH=lib omake --dotomake .omake --force-dotomake  install

.PHONY: clean
clean:
	rm -f .config
	rm -f boot/*
	find . -name "*.omc" | xargs rm -f
	rm -f src/*/*.cmi src/*/*.cmo src/*/*.cma
	rm -f src/*/*.cmx src/*/*.o src/*/*/.cmxa src/*/*.a
	rm -f src/env/omake_ast_parse.mly
	rm -f src/libmojave/lm_thread_core.ml
	rm -f src/libmojave/lm_thread_pool.ml
	rm -f src/magic/omake_magic.ml
	rm -f src/shell/omake_shell_sys.ml
	rm -f .omakedb .omakedb.lock
	@echo "Next 'make' will re-bootstrap. (This is not a full 'clean' rule.)"
