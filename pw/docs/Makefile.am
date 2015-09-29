
pwdocdir = $(docdir)/pw

pwdoc_DATA = commands.html concepts.html fileformats.html intro.html \
	invocation.html keys.html mouse.html pw.gif pw.html scheme.html \
	scrollbars.html spell.html strings.html toolbar.html

man_MANS = pw.1

EXTRA_DIST = $(pwdoc_DATA) $(man_MANS)

autodocs:
	@echo generating html
	../../siod/siod menus.scm

