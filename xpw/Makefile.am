
bin_PROGRAMS = pw

pw_SOURCES = window.c xpw.c fileio_ps.c

noinst_HEADERS = app-defaults.h xpw.h

pw_LDFLAGS = -L../siod -L../common -L../xcommon -L../pw

pw_LDADD = -lpw -lxcommon -lcommon -lsiod \
	@LIBSYSDEP@ @INTERPRETERS@ \
	-lXpm @LIBT1@ @LIBT1X@ \
	@X_LIBS@ $(MY_X_LIBS) -lXt -lX11 \
	@X_PRE_LIBS@ @X_EXTRA_LIBS@

INCLUDES = $(DEBUG) $(NDBM) @X_CFLAGS@

EXTRA_DIST = Pw.ad PW.kdelnk x-pw.kdelnk

app-defaults.h: ../xcommon/ad2c Pw.ad
	sh ../xcommon/ad2c Pw.ad > app-defaults.h

# Add KDE entries for application and mimetypes

install-data-local:
	$(KDEINST) -a PW.kdelnk -m x-pw.kdelnk \
		-i ../common/bitmaps/pw.xpm

uninstall-local:
	$(KDEINST) -u -a PW.kdelnk -m x-pw.kdelnk \
		-i pw.xpm

