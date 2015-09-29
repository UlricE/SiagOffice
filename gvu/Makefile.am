
gvudocdir = $(docdir)/gvu

gvudoc_DATA = README gs.interface comments.doc gvu.html \
	copyright.html

bin_PROGRAMS = gvu

gvu_SOURCES = callbacks.c main.c misc.c

noinst_HEADERS = gvu-ad.h gv.h

man_MANS = gvu.1

gvu_LDFLAGS = -L../xcommon -L../common -L../XawM/.libs

gvu_LDADD = -lxcommon -lcommon \
	-lXpm $(LIBT1) $(LIBT1X) $(X_LIBS) $(MY_X_LIBS) \
	-lXt -lX11 $(X_PRE_LIBS) $(X_EXTRA_LIBS)

INCLUDES = $(DEBUG) $(X_CFLAGS)

EXTRA_DIST = Gvu.ad Gvu.kdelnk blank.xpm dot.xpm $(gvudoc_DATA) $(man_MANS)

gvu-ad.h: ../xcommon/ad2c Gvu.ad
	sh ../xcommon/ad2c Gvu.ad > gvu-ad.h

# Add KDE entries for application and mimetypes

install-data-local:
	$(KDEINST) -a Gvu.kdelnk -i ../common/bitmaps/gvu.xpm

uninstall-local:
	$(KDEINST) -u -a Gvu.kdelnk -i gvu.xpm

