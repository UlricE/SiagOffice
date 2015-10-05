
bin_PROGRAMS = tsiag

tsiag_SOURCES = fonts.c window.c tsiag.c MwUtils.c

noinst_HEADERS = tsiag.h MwUtils.h

tsiag_LDFLAGS = -L../siod -L../common -L../siag -L../cole -L../stocks

tsiag_LDADD = @LIBCCMATH@ -lsiag -lcommon -lsiod -lcole -lstocks \
	@LIBCURSES@ \
	@LIBSYSDEP@ @INTERPRETERS@ \
	@LIBCCMATH@ @LIBT1@

AM_CPPFLAGS = $(DEBUG) $(NDBM) $(CCMATH)

