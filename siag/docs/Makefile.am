
siagdocdir = $(docdir)/siag

siagdoc_DATA = README c-expr.html commands.html concepts.html excel.html \
	fileformats.html form.html \
	gnuplot.html intro.html invocation.html keys.html \
	mouse.html scheme.html scrollbars.html siag.gif siag.html \
	siag-functions.html strings.html toolbar.html

man_MANS = siag.1

EXTRA_DIST = $(siagdoc_DATA) $(man_MANS)

autodocs:
	@echo generating html
	../../siod/siod keys.scm
	../../siod/siod menus.scm

