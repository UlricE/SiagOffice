
SUBDIRS = docs examples

noinst_LIBRARIES = libsiag.a

libsiag_a_SOURCES = railway.c matrix.c fileio.c cmds.c buffer.c mathwrap.c \
	fileio_abs.c fileio_siag.c fileio_txt.c \
	fileio_html.c fileio_scm.c fileio_sxc.c fileio_ps.c \
	fileio_xls.c fileio_wk1.c fileio_tbl.c fileio_latex.c \
	ci.c siodi.c guilei.c tcli.c pythoni.c rubyi.c position.c main.c \
	window.c globals.c stocks.c gmp.c ccmath.c \
	mathfunc.c roman.c db.c

noinst_HEADERS = calc.h fileio.h user_interface.h \
	mathfunc.h

INCLUDES = $(DEBUG) $(NDBM) $(CCMATH) -DSIAG

scmdir = $(datadir)/siag/siag

scm_DATA = 123.scm ccmath.scm data.scm functions.scm \
	external.load external.save \
	filemgr.scm find.scm keytable.scm mailto.scm menu.scm \
	plot.scm siag-http.scm siag-net.scm siag.scm sort.scm \
	splot.scm styles.scm usermgr.scm toolbar.scm trans.scm

EXTRA_DIST = $(scm_DATA)

install-data-local:
	@echo "Install support files for Siag"
	$(mkinstalldirs) $(DESTDIR)$(docdir)/siag
	cp $(srcdir)/data.scm $(DESTDIR)$(docdir)/siag

uninstall-local:
	@echo "Uninstalling support files for Siag"
	rm -rf $(DESTDIR)$(docdir)/siag

