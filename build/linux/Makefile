SHELL		= /bin/sh

INSTALL_PROGRAM	= install
compress	= gzip

LISPFLAGS	= -build
LISP		= $(HOME)/bin/lw $(LISPFLAGS)

PREFIX		= /usr

# DESTDIR for local builds, ie. by rpmbuild etc.

bindir		= $(DESTDIR)$(PREFIX)/bin
libdir		= $(DESTDIR)$(PREFIX)/lib64/om-sharp
datadir		= $(DESTDIR)$(PREFIX)/share
omfontsdir	= $(DESTDIR)$(PREFIX)/share/fonts/om-sharp-fonts
omroot		= $(datadir)/$(APPNAME)
ICONDIR		= $(datadir)/icons/hicolor/64x64/apps


path_to_omroot	= ../../
thisfile	= $(lastword $(MAKEFILE_LIST))
thisdir		= $(dir $(thisfile))
builddir	= $(shell cd $(dir $(thisfile)) && cd $(path_to_omroot) && pwd)

BUILDROOT	= $(builddir)
APPNAME		= om-sharp
INSTALLRES	= $(BUILDROOT)/build/linux

app_name_version = om-sharp

faslname	= 64ufasl
rubbish		= *.$(faslname)

# rsyncflags	= -v -rlt -z
rsyncflags	= -v -rlt -O -z -C --include '*.so'
exclude_fasl	= --include 'dspec-database.$(faslname)' --exclude '$(rubbish)'
exclude_mac	= --exclude 'build/mac/' --exclude '*.finderinfo'
exclude_win	= --exclude 'build/win/'
exclude_libs	= --exclude 'resources/lib/'

checkname	= $(shell ls $(BUILDROOT) | grep "om-sharp_")
ifdef $(checkname)
releaseappname	= $(checkname)
else
releaseappname	= $(app_name_version)
endif




all:	compile


help:
	@echo BUILDROOT:	$(BUILDROOT)
	@echo releaseappname:	$(releaseappname)
	@echo targets: '(default=compile, all), compile, all (=compile), install, uninstall, clean, tardist, preparerpm, help'

compile: 
	cd $(BUILDROOT)
	@echo building $(releaseappname) in source tree: $(BUILDROOT)
	$(LISP) $(BUILDROOT)/build/deliver.lisp

install: $(BUILDROOT)/$(releaseappname)
	mkdir -p $(omroot)
	cd $(BUILDROOT) && rsync $(rsyncflags) $(exclude_fasl) $(exclude_libs) $(exclude_mac) $(exclude_win) --exclude 'Makefile' --exclude $(releaseappname) . $(omroot)
	mkdir -p $(libdir)
	cd $(BUILDROOT)/resources/lib/linux && rsync $(rsyncflags) . $(libdir)
	cd $(BUILDROOT) && $(INSTALL_PROGRAM) -D -m 0755 $(releaseappname) $(bindir)/$(releaseappname)
	mkdir -p $(datadir)/applications/
	cd $(INSTALLRES) && $(INSTALL_PROGRAM) -D -m 0644 OM-sharp.desktop $(datadir)/applications/
	mkdir -p $(ICONDIR)
	cd $(BUILDROOT)/resources/ && $(INSTALL_PROGRAM) -D -m 0644 om-sharp.png $(ICONDIR)
	mkdir -p $(omfontsdir)/
	cd $(BUILDROOT)/resources/fonts && rsync $(rsyncflags) *.otf $(omfontsdir)

uninstall:
	rm -rf $(omroot)
	rm -f $(bindir)/$(APPNAME)
	rm -f $(bindir)/$(releaseappname)
	rm -f $(datadir)/applications/OM-sharp.desktop
	rm -f $(ICONDIR)/om-sharp.png
	rm -rf $(omfontsdir)
	rm -rf $(libdir)

clean:
	cd $(BUILDROOT)/src && find . -name $(rubbish) -delete
	rm -f $(BUILDROOT)/$(releaseappname)

tardist: all
	cd $(BUILDROOT)/../ && \
	tar cvjf om-sharp.tar.bz2 \
	   --exclude=.git* \
	   --exclude=om-sharp/build/mac \
	   --exclude=om-sharp/build/win* \
	   --exclude=om-sharp/resources/lib/mac \
	   --exclude=om-sharp/resources/lib/win* \
	   ./om-sharp

preparerpm: tardist
	cd $(BUILDROOT)/../ && \
	mv om-sharp.tar.bz2 ~/rpmbuild/SOURCES
