
SUBDIRS = docs bitmaps themes

bin_SCRIPTS = siagrun

noinst_LIBRARIES = libcommon.a 

libcommon_a_SOURCES = common.c ps.c o3read.c

noinst_HEADERS = common.h plugin.h ps.h o3read.h

INCLUDES = -DLIBDIR=\"$(libdir)/siag\" \
	-DDATADIR=\"$(datadir)/siag\" \
	-DDOCDIR=\"$(docdir)\" \
	-DVERSION_NO=\"@VERSION@\" \
	$(DEBUG)

CLEANFILES = rgb comments

commondir = $(datadir)/siag/common
common_DATA = common.scm position.scm tools.scm \
	dictionary.sv dictionary.es dictionary.de dictionary.fr \
	dictionary.it dictionary.nl \
	dictionary.no dictionary.hr dictionary.hu dictionary.da dictionary.pl \
	dictionary.gl dictionary.ru dictionary.cs dictionary.pt_BR

common_SCRIPTS = any2xpm

EXTRA_DIST = $(bin_SCRIPTS) $(common_DATA) $(common_SCRIPTS) rgb.c \
	newdict siag.magic siag.dfmext kdeinst

comments: comments.c
	$(CC) -o comments comments.c

