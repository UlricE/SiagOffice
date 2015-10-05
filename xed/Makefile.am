
xedplusdocdir = $(docdir)/xedplus

xedplusdoc_DATA = xedplus.html README

bin_PROGRAMS = xedplus

xedplus_SOURCES = xedit.c jump.c edit.c file.c warning.c search.c \
	file_sel.c special.c commands.c dnd.c

noinst_HEADERS = app-defaults.h xedit.h

man_MANS = xedplus.1

xedplus_LDFLAGS = -L../xcommon -L../common -L../XawM/.libs

xedplus_LDADD = -lxcommon -lcommon \
	-lXpm $(LIBT1) $(LIBT1X) $(X_LIBS) $(MY_X_LIBS) \
	-lXt -lX11 $(X_PRE_LIBS) $(X_EXTRA_LIBS)

AM_CPPFLAGS = $(DEBUG) $(X_CFLAGS)

EXTRA_DIST = Xed.ad Xedplus.kdelnk $(xedplusdoc_DATA) $(man_MANS)

app-defaults.h: ../xcommon/ad2c Xed.ad
	sh ../xcommon/ad2c Xed.ad > app-defaults.h

# Add KDE entries for application and mimetypes

install-data-local:
	$(KDEINST) -a Xedplus.kdelnk -i ../common/bitmaps/xedplus.xpm

uninstall-local:
	$(KDEINST) -u -a Xedplus.kdelnk -i xedplus.xpm

