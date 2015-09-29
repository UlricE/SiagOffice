
noinst_LIBRARIES = libxcommon.a 

libxcommon_a_SOURCES = forminput.c embed.c xcommon.c plugin.c Ghostview.c

noinst_HEADERS = Ghostview.h GhostviewP.h \
	embed.h plugin.h xcommon.h \
	filesel-ad.h dialogs-ad.h xcommon-ad.h nws-ad.h

INCLUDES = $(DEBUG) @X_CFLAGS@

scmdir = $(datadir)/siag/xcommon
scm_DATA = StringDefs.scm form.scm
EXTRA_DIST = $(scm_DATA) ad2c

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(docdir)/xcommon
	cp $(srcdir)/form.scm $(DESTDIR)$(docdir)/xcommon

uninstall-local:
	rm -rf $(DESTDIR)$(docdir)/xcommon

drop: drop.c
	$(CC) -DTESTDROP -Wall -g -o drop drop.c -L/usr/X11/lib -lXt -lX11

