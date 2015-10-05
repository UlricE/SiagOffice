
bin_PROGRAMS = siag

siag_SOURCES = window.c xsiag.c

noinst_HEADERS = app-defaults.h xsiag.h

siag_LDFLAGS = -L../siod -L../common -L../xcommon -L../siag -L../cole -L../stocks

siag_LDADD = @LIBCCMATH@ -lsiag -lxcommon -lcommon -lcole -lstocks -lsiod \
	@LIBSYSDEP@ @INTERPRETERS@ \
	@LIBCCMATH@ -lXpm @LIBT1@ @LIBT1X@ \
	@X_LIBS@ $(MY_X_LIBS) -lXt -lX11 \
	@X_PRE_LIBS@ @X_EXTRA_LIBS@

AM_CPPFLAGS = $(DEBUG) $(NDBM) $(CCMATH) @X_CFLAGS@

EXTRA_DIST = Siag.ad Siag.kdelnk x-siag.kdelnk

app-defaults.h: ../xcommon/ad2c Siag.ad
	sh ../xcommon/ad2c Siag.ad > app-defaults.h

# Add KDE entries for application and mimetypes

install-data-local:
	$(KDEINST) -a Siag.kdelnk -m x-siag.kdelnk \
		-i ../common/bitmaps/siag.xpm

uninstall-local:
	$(KDEINST) -u -a Siag.kdelnk -m x-siag.kdelnk \
		-i siag.xpm

