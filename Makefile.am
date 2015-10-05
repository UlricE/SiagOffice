
SUBDIRS = \
	cole \
	stocks \
	siod \
	common \
	xcommon \
	siag \
	xsiag \
	tsiag \
	pw \
	xpw \
	egon \
	xegon \
	plugins \
	xed \
	xfiler \
	o3read \
	gvu

DIST_SUBDIRS = $(SUBDIRS)

AM_CPPFLAGS = $(DEBUG) @X_CFLAGS@ -DXAWHDRS=@XAWHDRS@

doc_DATA = AUTHORS FILES NLS COPYING README

EXTRA_DIST = $(doc_DATA)

# This is an attempt to automate generation of binaries.

strip:
	@echo "Stripping binaries"
	find . -perm -100 -a -type f -exec strip {} \;

binaries:
	make clean; \
	./configure --with-stocks; \
	make; \
	rm -rf tmpinst; \
	make .binaries prefix=`cd . && pwd`/tmpinst/$(prefix); \
	rm -rf tmpinst;

.binaries:
	rm -rf $(prefix); \
	mkdir -p $(prefix); \
	make install-strip; \
	arch=`uname -s`-`uname -m`; \
	(cd tmpinst && tar cf - .) | gzip > siag-$${arch}-$(VERSION).tar.gz; \
	ldd $(bindir)/siag > siag-ldd

bzdist: dist
	gzip -cd siag-$(VERSION).tar.gz | bzip2 > siag-$(VERSION).tar.bz2

tgz: binaries
	(arch=`uname -s`-`uname -m`; \
	rm -rf builddir; mkdir builddir; \
	mkdir builddir/install; \
	cp doinst.sh builddir/install; \
	cd builddir; \
	tar zxf ../siag-$${arch}-$(VERSION).tar.gz; \
	tar zcf ../siag-$(VERSION).tgz .)

rpm: tgz
	alien -r siag-$(VERSION).tgz

