
bindir = $(libdir)/siag/plugins

pluginsdocdir = $(docdir)/plugins

pluginsdoc_DATA = README

bin_PROGRAMS = dummy image hello plot text form clipart

dummy_SOURCES = dummy.c siodi.c
image_SOURCES = image.c
hello_SOURCES = hello.c
plot_SOURCES = plot.c
text_SOURCES = text.c
form_SOURCES = form.c
clipart_SOURCES = clipart.c

noinst_HEADERS = form-ad.h

man_MANS = dummy_plugin.1

scmdir = $(datadir)/siag/plugins

scm_DATA = dummy.scm plugin.scm

EXTRA_DIST = $(scm_DATA) $(pluginsdoc_DATA) $(man_MANS) Form.ad

INCLUDES = $(DEBUG) @X_CFLAGS@

dummy_LDFLAGS = -L../siod -L../xcommon -L../common -L../XawM/.libs
image_LDFLAGS = -L../siod -L../xcommon -L../common -L../XawM/.libs
hello_LDFLAGS = -L../siod -L../xcommon -L../common -L../XawM/.libs
plot_LDFLAGS = -L../siod -L../xcommon -L../common -L../XawM/.libs
text_LDFLAGS = -L../siod -L../xcommon -L../common -L../XawM/.libs
form_LDFLAGS = -L../siod -L../xcommon -L../common -L../XawM/.libs
clipart_LDFLAGS = -L../siod -L../xcommon -L../common -L../XawM/.libs

form-ad.h: ../xcommon/ad2c Form.ad
	sh ../xcommon/ad2c Form.ad > form-ad.h

LDADD = -lsiod -lxcommon -lcommon \
	@LIBSYSDEP@ -lXpm @LIBT1@ @LIBT1X@ \
	@X_LIBS@ $(MY_X_LIBS) -lXt -lX11 \
	@X_PRE_LIBS@ @X_EXTRA_LIBS@

