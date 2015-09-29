
egondocdir = $(docdir)/egon

egondoc_DATA = commands.html concepts.html egon.gif egon.html fileformats.html \
	intro.html invocation.html keys.html mouse.html scheme.html \
	scrollbars.html strings.html toolbar.html

man_MANS = egon.1

EXTRA_DIST = $(egondoc_DATA) $(man_MANS)

autodocs:
	@echo generating html
	../../siod/siod menus.scm

