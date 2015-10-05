
SUBDIRS = kde kde2 gnome ms

xpmdir = $(datadir)/siag/common/bitmaps
xpm_DATA = blank.xpm bold.xpm borders.xpm copy.xpm copyright.xpm cut.xpm \
	egon.xpm fld_open.xpm fld_closed.xpm fld_up.xpm fld_new.xpm \
	save.xpm grid.xpm hcenter.xpm hfull.xpm hleft.xpm \
	hright.xpm info.xpm insert.xpm \
	italic.xpm lline.xpm new.xpm next.xpm none.xpm overwrite.xpm paste.xpm \
	play.xpm plotter.xpm preview.xpm previous.xpm printer.xpm pw.xpm \
	redo.xpm rline.xpm siag.xpm siagoffice.xpm \
	sigma.xpm sortaz.xpm sortza.xpm spell.xpm \
	stop.xpm uchar.xpm uline.xpm strike.xpm undo.xpm vbottom.xpm vtop.xpm \
	back.xpm cancel.xpm home.xpm quit.xpm reload.xpm search.xpm \
	xterm16.xpm viewmode.xpm sortmode.xpm table.xpm \
	xedplus.xpm netscape.xpm icons.xpm image.xpm ghost.xpm compress.xpm \
	fonts.xpm data.xpm unknown.xpm \
	center.xpm larger.xpm smaller.xpm portrait.xpm landscape.xpm \
	editor.xpm SO3.xpm book-closed.xpm book-open.xpm handlebg.xpm \
	xfiler.xpm gvu.xpm \
	security.xpm \
	navigator.xpm mail.xpm news.xpm addressbook.xpm composer.xpm

gifdir = $(docdir)/common/bitmaps
gif_DATA = bold.gif borders.gif copyright.gif egon_fg.gif fld_open.gif \
	save.gif grid.gif hcenter.gif hleft.gif hright.gif info.gif \
	italic.gif new.gif next.gif none.gif play.gif plotter.gif \
	preview.gif previous.gif printer.gif pw_fg.gif siag_fg.gif \
	sigma.gif stop.gif uchar.gif uline.gif vbottom.gif vtop.gif

EXTRA_DIST = $(xpm_DATA) $(gif_DATA) fromnone tonone xpm2gif rbm.xbm

gifs:
	@echo "Making gifs"
	for f in $(xpm_DATA); do ./xpm2gif $$f; done

