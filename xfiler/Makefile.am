
xfilerdir = $(datadir)/siag/xfiler

xfilerdocdir = $(docdir)/xfiler

xfilerdoc_DATA = xfiler.html README

SUBDIRS = pixmaps bitmaps

man_MANS = xfiler.1 runcmd.1

bin_PROGRAMS = xfiler runcmd

xfiler_SOURCES = Bitmaps.c Chmod.c Comms.c Confirm.c Delete.c Dirs.c Dnd.c \
	Errors.c Exec.c Fw.c FwActions.c FwCb.c Info.c Main.c Ops.c Popup.c \
	Utils.c magic.c \
	exec.c tycoon.c \
	regerror.c regexp.c regsub.c

xfiler_SCRIPTS = makeicons

runcmd_SOURCES = runcmd.c

noinst_HEADERS = Comms.h Files.h magic.h xfiler-ad.h \
	regexp.h regmagic.h \
	runcmd-ad.h blank.xpm dot.xpm

xfiler_LDFLAGS = -L../xcommon -L../common -L../XawM/.libs

xfiler_LDADD = -lxcommon -lcommon \
	-lXpm $(LIBT1) $(LIBT1X) $(X_LIBS) $(MY_X_LIBS) \
	-lXt -lX11  $(X_PRE_LIBS) $(X_EXTRA_LIBS)

runcmd_LDFLAGS = -L../xcommon -L../common -L../XawM/.libs

runcmd_LDADD = -lxcommon -lcommon -lxcommon -lcommon \
	-lXpm $(LIBT1) $(LIBT1X) $(X_LIBS) $(MY_X_LIBS) \
	-lXt -lX11  $(X_PRE_LIBS) $(X_EXTRA_LIBS)

INCLUDES = -I$(top_srcdir) -DXPM=1 $(X_CFLAGS) \
	-DROOTDROP_DEFAULT_ICON=\"linux_make.xpm\" \
	-DRCFILE=\"$(xfilerdir)/Filesrc\" \
	-DMAGICFILE=\"$(xfilerdir)/FilesMagic\" \
	-DPIXDIR=\"$(includedir)/pixmaps\"

xfiler_DATA = FilesMagic Filesrc

EXTRA_DIST = Xfiler.ad xfmtype.c $(xfilerdoc_DATA) $(xfiler_DATA) $(man_MANS) \
	$(xfiler_SCRIPTS) Runcmd.ad Xfiler.kdelnk

xfiler-ad.h: Xfiler.ad
	sh ../xcommon/ad2c Xfiler.ad > xfiler-ad.h

runcmd-ad.h: Runcmd.ad
	sh ../xcommon/ad2c Runcmd.ad > runcmd-ad.h

# Add KDE entries for application and mimetypes

install-data-local:
	$(KDEINST) -a Xfiler.kdelnk -i ../common/bitmaps/xfiler.xpm

uninstall-local:
	$(KDEINST) -u -a Xfiler.kdelnk -i xfiler.xpm

