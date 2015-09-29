
SUBDIRS = docs examples

noinst_LIBRARIES = libpw.a

libpw_a_SOURCES = railway.c matrix.c fileio.c cmds.c position.c \
	buffer.c main.c \
	fileio_pw.c fileio_txt.c fileio_html.c fileio_rtf.c \
	fileio_sxw.c \
	siodi.c guilei.c tcli.c pythoni.c rubyi.c

noinst_HEADERS = pw.h user_interface.h

INCLUDES = $(DEBUG) $(NDBM)

scmdir = $(datadir)/siag/pw

scm_DATA = external.load external.save \
	menu.scm pw.scm styles.scm
EXTRA_DIST = $(scm_DATA)

