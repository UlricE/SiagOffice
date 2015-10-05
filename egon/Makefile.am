
SUBDIRS = docs examples

bin_SCRIPTS = mgptotxt

noinst_LIBRARIES = libegon.a 

libegon_a_SOURCES = main.c railway.c matrix.c fileio.c cmds.c \
	buffer.c \
	fileio_egon.c fileio_c.c fileio_scm.c \
	fileio_html.c fileio_mgp.c fileio_ppt.c fileio_txt.c \
	siodi.c guilei.c tcli.c pythoni.c rubyi.c

noinst_HEADERS = egon.h user_interface.h

AM_CPPFLAGS = $(DEBUG) $(NDBM)

scmdir = $(datadir)/siag/egon
scm_DATA = animator.scm egon.scm menu.scm external.load external.save
EXTRA_DIST = $(bin_SCRIPTS) $(scm_DATA)

