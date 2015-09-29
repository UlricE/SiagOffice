
bin_PROGRAMS = egon

egon_SOURCES = window.c selection.c fileio_gif.c fileio_ps.c xegon.c

noinst_HEADERS = app-defaults.h xegon.h

egon_LDFLAGS = -L../siod -L../common -L../xcommon -L../egon -L../cole

egon_LDADD = -legon -lxcommon -lcommon -lcole -lsiod \
	@LIBSYSDEP@ @INTERPRETERS@ \
	-lXpm @LIBT1@ @LIBT1X@ \
	@X_LIBS@ $(MY_X_LIBS) -lXt -lX11 \
	@X_PRE_LIBS@ @X_EXTRA_LIBS@

INCLUDES = $(DEBUG) $(NDBM) @X_CFLAGS@

EXTRA_DIST = Egon.ad Egon.kdelnk x-egon.kdelnk

app-defaults.h: ../xcommon/ad2c Egon.ad
	sh ../xcommon/ad2c Egon.ad > app-defaults.h

# Add KDE entries for application and mimetypes

install-data-local:
	$(KDEINST) -a Egon.kdelnk -m x-egon.kdelnk \
		-i ../common/bitmaps/egon.xpm

uninstall-local:
	$(KDEINST) -u -a Egon.kdelnk -m x-egon.kdelnk \
		-i egon.xpm

