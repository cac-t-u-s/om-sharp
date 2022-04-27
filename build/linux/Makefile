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


OMROOT		= ../../
thisfile	= $(lastword $(MAKEFILE_LIST))
thisdir		= $(dir $(thisfile))
builddir	= $(shell cd $(dir $(thisfile)) && cd $(OMROOT) && pwd)

BUILDROOT	= $(builddir)
APPNAME		= om-sharp
INSTALLRES	= $(BUILDROOT)/build/linux

OM_VERSION	= $(shell cat $(OMROOT)/VERSION)
OM_RELEASE	= 1
TARBALL		= $(shell cd $(OMROOT)/.. && pwd)/om-sharp-Linux-$(OM_VERSION)-$(OM_RELEASE).tar.bz2
RPMDIR		= $(HOME)/rpmbuild/SOURCES


app_name_version = om-sharp

faslname	= 64ufasl
rubbish		= *.$(faslname)

# include dspec database in install and tarball
dspec_database  = resources/dspec-database.$(faslname)
include_dspec	= --include '$(dspec_database)'

include_dirs	= --include 'slime/*'
include_slime_fasls = --include 'slime/fasl/*ufasl' --include 'slime/fasl/*/*ufasl'

# rsyncflags	= -v -rlt -z
rsyncflags	= -v -rlt -O -z -C --include '*.so'
exclude_fasl	= --exclude '$(rubbish)'

slime_dir	= ./om-sharp/src/lisp-externals/slime

exclude_mac	= --exclude 'build/mac/' --exclude '*.finderinfo' --exclude 'resources/lib/mac'
exclude_win	= --exclude 'build/win/' --exclude 'resources/lib/win*'
exclude_oses	= $(exclude_mac) $(exclude_win)

checkname	= $(shell ls $(BUILDROOT) | grep "om-sharp_")
ifdef $(checkname)
releaseappname	= $(checkname)
else
releaseappname	= $(app_name_version)
endif


all:	compile


help:
	@echo BUILDROOT:	$(BUILDROOT)
	@echo Makefile:		$(thisfile)
	@echo THISDIR:		$(thisdir)
	@echo BUILDROOT:	$(BUILDROOT)
	@echo buildname:	$(buildname)
	@echo IMAGENAME:	$(IMAGENAME)
	@echo releaseappname:	$(releaseappname)
	@echo targets: '(default=compile, all), compile, all (=compile), install, uninstall, clean, tardist, preparerpm, help'

compile: 
	cd $(BUILDROOT)
	@echo building $(releaseappname) in source tree: $(BUILDROOT)
	$(LISP) $(BUILDROOT)/build/deliver.lisp

install: $(BUILDROOT)/$(releaseappname)
	mkdir -p $(omroot)
	cd $(BUILDROOT) && \
			rsync $(rsyncflags) $(include_dspec) $(include_dirs) $(include_slime_fasls) \
			$(exclude_fasl) $(exclude_oses) --exclude 'Makefile' --exclude $(releaseappname) \
			. $(omroot)
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

# exclude various mac/win-related, fasls, but take care to include
# everything inside slime directory (including fasls):

tardist:
	$(echo bygger $(TARBALL) i $(BUILDROOT)../)
	cd $(BUILDROOT)/../ && \
	tar cvjf $(TARBALL) \
	   ./om-sharp/$(dspec_database) \
	   $(slime_dir) \
	   --exclude=.git* \
	   $(exclude_fasl) \
	   $(exclude_mac) \
	   $(exclude_win) \
	   ./om-sharp
	@echo tarball:	$(shell ls $(TARBALL))


preparerpm: $(TARBALL)
	cp -v $(TARBALL) $(RPMDIR)
