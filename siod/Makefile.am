
SUBDIRS = docs

noinst_LIBRARIES = libsiod.a 

libsiod_a_SOURCES = md5.c regex.c sliba.c ss.c tar.c \
	ndbm.c slib.c slibu.c trace.c 

noinst_HEADERS = md5.h siod.h siodp.h ss.h silence.i

AM_CPPFLAGS = -Dunix -DSIAG $(DEBUG) $(NDBM)

CLEANFILES = siod

sioddir = $(datadir)/siag/siod

siod_DATA = siod.scm

EXTRA_DIST = $(siod_DATA)

siod:
	$(CC) -o siod siod.c libsiod.a -lm -lcrypt

