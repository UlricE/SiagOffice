"XedPlus.geometry:				600x400",
"XedPlus*topbox.xLayout:		100%",
"XedPlus*topbox.yLayout:		30 30 100% 30",
"XedPlus*Box.handle.height:	28",
"XedPlus*Box.handle.label:",
"XedPlus*menubar.shadowWidth:		1",
"XedPlus*menubar.gridy:			0",
"XedPlus*menubox.orientation:		horizontal",
"XedPlus*menubox.borderWidth:		0",
"XedPlus*menubox.NwsMenuButton.shadowWidth:	0",
"XedPlus*menubox.NwsMenuButton.internalHeight:	6",
"XedPlus*menubox.vSpace:			0",
"XedPlus*menubox.hSpace:			0",
"XedPlus*menubox.Command.borderWidth:	4",
"XedPlus*menubox.Command.borderColor:	grey",
"XedPlus*editmode.width:			120",
"XedPlus*toolbar.shadowWidth:		1",
"XedPlus*toolbar.gridy:			1",
"XedPlus*toolbox.orientation:		horizontal",
"XedPlus*toolbox.borderWidth:		0",
"XedPlus*toolbox.Toggle.shadowWidth:	0",
"XedPlus*toolbox.Command.shadowWidth:	0",
"XedPlus*toolbox.vSpace:			0",
"XedPlus*toolbox.hSpace:			0",
"XedPlus*vsep.width:			2",
"XedPlus*vsep.height:			24",
"XedPlus*vsep.borderWidth:		2",
"XedPlus*vsep.shadowWidth:		1",
"XedPlus*vsep.topShadowContrast:		-40",
"XedPlus*vsep.bottomShadowContrast:	-20",
"XedPlus*vsep.label:",
"XedPlus*editWindow.gridy:			2",
"XedPlus*editWindow*font:			fixed",
"XedPlus*statusbox.borderWidth:		0",
"XedPlus*statusbox.gridy:		3",
"XedPlus*statusbox.xLayout:		4 100% 4 100 4",
"XedPlus*statusbox.yLayout:		4 100% 4",
"XedPlus*labelWindow.gridx:		1",
"XedPlus*labelWindow.gridy:		1",
"XedPlus*labelWindow.shadowWidth:	1",
"XedPlus*labelWindow.justify:		left",
"XedPlus*dirty.gridx:			3",
"XedPlus*dirty.gridy:			1",
"XedPlus*dirty.shadowWidth:		1",
"XedPlus*dirty.font:			-*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-*",
"XedPlus*buttons*orientation:		horizontal",
"XedPlus*buttons*showGrip:		False",
"XedPlus*input:			        TRUE",
"XedPlus*editWindow.autoFill:		True",
"XedPlus*editWindow.scrollVertical:	Always",
"XedPlus.AutoIndent:			True",
"XedPlus*editWindow.wrap:		Never",
"XedPlus.PrintCommand:			lpr -p -T %t %f",
"XedPlus.Tabsize:			8",
"XedPlus.EnableBackups:		       	True",
"XedPlus.BackupNameSuffix:		.bak",
"XedPlus.MaxScrollbreak:			3",
"XedPlus*toolbar_command.translations:	#override		\\n\
	<Enter>:	siag-highlight(1)\\n\
	<Leave>:	siag-unhighlight(0)",
"XedPlus*value.translations:  #override \
	<Key>Escape: select-all() delete-selection()",
"XedPlus*tab_text.translations: #override \
	<Key>Escape: select-all() delete-selection() \\n \
	<Key>Return: no-op()",
"XedPlus*sed_text.translations: #override \
	<Key>Escape: select-all() delete-selection()",
"XedPlus*search_text.translations: #override \
	<Key>Escape: select-all() delete-selection()		",
"XedPlus*replace_text.translations: #override \
	<Key>Escape: select-all() delete-selection()",
"XedPlus*command_text.translations: #override \
	<Key>Escape: select-all() delete-selection()\\n \
	<Key>Return: no-op()",
"XedPlus*pipe_text.translations: #override \
	<Key>Escape: select-all() delete-selection()\\n \
	<Key>Return: no-op()",
"XedPlus*line_text.translations: #override \
	<Key>Escape: select-all() delete-selection()",
"XedPlus*commands: LatexPS	latex $stripped; dvips $stripped.dvi -o $stripped.ps; ghostview $stripped.ps &\\n\
LatexDVI	latex $stripped; xdvi $stripped.dvi &\\n\
Make	make\\n\
CC	cc $stripped.c -o $stripped\\n\
New Xedplus	1>/dev/null 2>/dev/null xedplus &\\n\
Manual	mantitle=`head $selection`;man $mantitle | sed \"s/.//g\"",
"XedPlus*pipes: Date	date",
"XedPlus.overwritetranslations:  #replace \
	Meta<Key>i: xedCallMenu(filemenu.insert) \\n \
	Meta<Key>o: xedCallMenu(filemenu.load) \\n \
	Meta<Key>s: xedCallMenu(filemenu.save) \\n \
	Meta<Key>e: xedCallMenu(filemenu.quit) \\n \
	Meta<Key>l: xedCallMenu(jumpmenu.line) \\n \
	Meta<Key>f: xedCallMenu(searchmenu.search) \\n \
	Meta<Key>x: kill-selection() \\n \
	<Key>L10:kill-selection() \\n \
	<Key>R7: beginning-of-file() \\n \
	<Key>R9: previous-page() \\n \
	<Key>R13: end-of-file() \\n \
	<Key>R15: next-page() \\n \
	:Ctrl<Key>Home: beginning-of-file() \\n \
	:<Key>Home: beginning-of-line() \\n \
	<Key>Prior: previous-page() \\n \
	:Ctrl<Key>End: end-of-file() \\n \
	:<Key>End: end-of-line() \\n \
	<Key>Next: next-page() \\n \
	Shift<Key>Up: previous-page() \\n \
	Shift<Key>Down: next-page() \\n \
	Shift<Key>Left: backward-word() \\n \
	Shift<Key>Right: forward-word() \\n \
	Ctrl<Key>Up: beginning-of-file() \\n \
	Ctrl<Key>Down: end-of-file() \\n \
	Ctrl<Key>Left: beginning-of-line() \\n \
	Ctrl<Key>Right: end-of-line() \\n \
	Meta<Key>m: xedCallMenu(paned.buttons.editmode) \\n \
Ctrl<Key>A:	beginning-of-line() \\n\
Ctrl<Key>B:	backward-character() \\n\
Ctrl<Key>C:	kill-selection() insert-selection(CLIPBOARD, CLIPBOARD) \\n\
Ctrl<Key>D:	delete-next-character() \\n\
Ctrl<Key>E:	end-of-line() \\n\
Ctrl<Key>F:	forward-character() \\n\
Ctrl<Key>G:     multiply(Reset) \\n\
Ctrl<Key>H:	delete-previous-character() \\n\
Ctrl<Key>J:	newline-and-indent() \\n\
Ctrl<Key>K:	kill-to-end-of-line() \\n\
Ctrl<Key>L:	redraw-display() \\n\
Ctrl<Key>M:	newline() \\n\
Ctrl<Key>N:	next-line() \\n\
Ctrl<Key>O:	newline-and-backup() \\n\
Ctrl<Key>P:	previous-line() \\n\
Ctrl<Key>Q:	xedCallMenu(filemenu.quit) \\n \
Ctrl<Key>R:	search(backward) \\n\
Ctrl<Key>S:	search(forward) \\n\
Ctrl<Key>T:     transpose-characters() \\n\
Ctrl<Key>U:	multiply(4) \\n\
Ctrl<Key>V:	insert-selection(CLIPBOARD, CLIPBOARD) \\n\
Ctrl<Key>W:	kill-selection() \\n\
Ctrl<Key>X:	kill-selection() \\n\
Ctrl<Key>Y:	insert-selection(SECONDARY) \\n\
Ctrl<Key>Z:	scroll-one-line-up() \\n\
Meta<Key>B:	backward-word() \\n\
Meta<Key>F:	forward-word() \\n\
Meta<Key>I:	insert-file() \\n\
Meta<Key>K:	kill-to-end-of-paragraph() \\n\
Meta<Key>Q:     form-paragraph() \\n\
Meta<Key>V:	previous-page() \\n\
Meta<Key>Y:	insert-selection(PRIMARY, CUT_BUFFER0) \\n\
Meta<Key>Z:	scroll-one-line-down() \\n\
:Meta<Key>d:	delete-next-word() \\n\
:Meta<Key>D:	kill-word() \\n\
:Meta<Key>h:	delete-previous-word() \\n\
:Meta<Key>H:	backward-kill-word() \\n\
:Meta<Key>less:	beginning-of-file() \\n\
:Meta<Key>greater:	end-of-file() \\n\
:Meta<Key>]:	forward-paragraph() \\n\
:Meta<Key>[:	backward-paragraph() \\n\
~Shift Meta<Key>Delete:		delete-previous-word() \\n\
 Shift Meta<Key>Delete:		backward-kill-word() \\n\
~Shift Meta<Key>BackSpace:	delete-previous-word() \\n\
 Shift Meta<Key>BackSpace:	backward-kill-word() \\n\
:Ctrl<Key>Home:	beginning-of-file() \\n\
<Key>Home:	beginning-of-line() \\n\
:<Key>KP_Home:	beginning-of-line() \\n\
:Ctrl<Key>End:	end-of-file() \\n\
<Key>End:	end-of-line() \\n\
:<Key>KP_End:	end-of-line() \\n\
<Key>Next:	next-page() \\n\
:<Key>KP_Next:	next-page() \\n\
<Key>Page_Down:	next-page() \\n\
<Key>Prior:	previous-page() \\n\
:<Key>KP_Prior: previous-page() \\n\
<Key>Page_Up:	previous-page() \\n\
:Ctrl<Key>Right:	forward-word() \\n\
<Key>Right:	forward-character() \\n\
:<Key>KP_Right: forward-character() \\n\
:Ctrl<Key>Left:	backward-word() \\n\
<Key>Left:	backward-character() \\n\
:<Key>KP_Left:	backward-character() \\n\
<Key>Down:	next-line() \\n\
:<Key>KP_Down:	next-line() \\n\
<Key>Up:	previous-line() \\n\
:<Key>KP_Up:	previous-line() \\n\
<Key>Delete:	delete-previous-character() \\n\
:<Key>KP_Delete: delete-previous-character() \\n\
<Key>BackSpace:	delete-previous-character() \\n\
<Key>Linefeed:	newline-and-indent() \\n\
<Key>Return:	newline() \\n\
:<Key>KP_Enter:	newline() \\n\
Ctrl<Key>backslash:	reconnect-im() \\n\
<Key>Kanji:	reconnect-im()\\n\
	<Key>:		xedskiplineend() delete-next-character() insert-char() \\n\
<EnterWindow>:	enter-window() \\n\
<LeaveWindow>:	leave-window() \\n\
<FocusIn>:	focus-in() \\n\
<FocusOut>:	focus-out() \\n\
<Btn1Down>:	select-start() \\n\
<Btn1Motion>:	extend-adjust() \\n\
<Btn1Up>:	extend-end(PRIMARY, CUT_BUFFER0) \\n\
<Btn2Down>:	insert-selection(PRIMARY, CUT_BUFFER0) \\n\
<Btn3Down>:	extend-start() \\n\
<Btn3Motion>:	extend-adjust() \\n\
<Btn3Up>:	extend-end(PRIMARY, CUT_BUFFER0)",
"XedPlus.inserttranslations:  #replace \
	Meta<Key>i: xedCallMenu(filemenu.insert) \\n \
	Meta<Key>o: xedCallMenu(filemenu.load) \\n \
	Meta<Key>s: xedCallMenu(filemenu.save) \\n \
	Meta<Key>e: xedCallMenu(filemenu.quit) \\n \
	Meta<Key>l: xedCallMenu(jumpmenu.line) \\n \
	Meta<Key>f: xedCallMenu(searchmenu.search) \\n \
	Meta<Key>x: kill-selection() \\n \
	<Key>L10:kill-selection() \\n \
	<Key>R7: beginning-of-file() \\n \
	<Key>R9: previous-page() \\n \
	<Key>R13: end-of-file() \\n \
	<Key>R15: next-page() \\n \
	:Ctrl<Key>Home: beginning-of-file() \\n \
	:<Key>Home: beginning-of-line() \\n \
	<Key>Prior: previous-page() \\n \
	:Ctrl<Key>End: end-of-file() \\n \
	:<Key>End: end-of-line() \\n \
	<Key>Next: next-page() \\n \
	Shift<Key>Up: previous-page() \\n \
	Shift<Key>Down: next-page() \\n \
	Shift<Key>Left: backward-word() \\n \
	Shift<Key>Right: forward-word() \\n \
	Ctrl<Key>Up: beginning-of-file() \\n \
	Ctrl<Key>Down: end-of-file() \\n \
	Ctrl<Key>Left: beginning-of-line() \\n \
	Ctrl<Key>Right: end-of-line() \\n \
	Meta<Key>m: xedCallMenu(paned.buttons.editmode) \\n \
Ctrl<Key>A:	beginning-of-line() \\n\
Ctrl<Key>B:	backward-character() \\n\
Ctrl<Key>C:	kill-selection() insert-selection(CLIPBOARD, CLIPBOARD) \\n\
Ctrl<Key>D:	delete-next-character() \\n\
Ctrl<Key>E:	end-of-line() \\n\
Ctrl<Key>F:	forward-character() \\n\
Ctrl<Key>G:     multiply(Reset) \\n\
Ctrl<Key>H:	delete-previous-character() \\n\
Ctrl<Key>J:	newline-and-indent() \\n\
Ctrl<Key>K:	kill-to-end-of-line() \\n\
Ctrl<Key>L:	redraw-display() \\n\
Ctrl<Key>M:	newline() \\n\
Ctrl<Key>N:	next-line() \\n\
Ctrl<Key>O:	newline-and-backup() \\n\
Ctrl<Key>P:	previous-line() \\n\
Ctrl<Key>Q:	xedCallMenu(filemenu.quit) \\n \
Ctrl<Key>R:	search(backward) \\n\
Ctrl<Key>S:	search(forward) \\n\
Ctrl<Key>T:     transpose-characters() \\n\
Ctrl<Key>U:	multiply(4) \\n\
Ctrl<Key>V:	insert-selection(CLIPBOARD, CLIPBOARD) \\n\
Ctrl<Key>W:	kill-selection() \\n\
Ctrl<Key>X:	kill-selection() \\n\
Ctrl<Key>Y:	insert-selection(SECONDARY) \\n\
Ctrl<Key>Z:	scroll-one-line-up() \\n\
Meta<Key>B:	backward-word() \\n\
Meta<Key>F:	forward-word() \\n\
Meta<Key>I:	insert-file() \\n\
Meta<Key>K:	kill-to-end-of-paragraph() \\n\
Meta<Key>Q:     form-paragraph() \\n\
Meta<Key>V:	previous-page() \\n\
Meta<Key>Y:	insert-selection(PRIMARY, CUT_BUFFER0) \\n\
Meta<Key>Z:	scroll-one-line-down() \\n\
:Meta<Key>d:	delete-next-word() \\n\
:Meta<Key>D:	kill-word() \\n\
:Meta<Key>h:	delete-previous-word() \\n\
:Meta<Key>H:	backward-kill-word() \\n\
:Meta<Key>less:	beginning-of-file() \\n\
:Meta<Key>greater:	end-of-file() \\n\
:Meta<Key>]:	forward-paragraph() \\n\
:Meta<Key>[:	backward-paragraph() \\n\
~Shift Meta<Key>Delete:		delete-previous-word() \\n\
 Shift Meta<Key>Delete:		backward-kill-word() \\n\
~Shift Meta<Key>BackSpace:	delete-previous-word() \\n\
 Shift Meta<Key>BackSpace:	backward-kill-word() \\n\
:Ctrl<Key>Home:	beginning-of-file() \\n\
<Key>Home:	beginning-of-line() \\n\
:<Key>KP_Home:	beginning-of-line() \\n\
:Ctrl<Key>End:	end-of-file() \\n\
<Key>End:	end-of-line() \\n\
:<Key>KP_End:	end-of-line() \\n\
<Key>Next:	next-page() \\n\
:<Key>KP_Next:	next-page() \\n\
<Key>Page_Down:	next-page() \\n\
<Key>Prior:	previous-page() \\n\
:<Key>KP_Prior: previous-page() \\n\
<Key>Page_Up:	previous-page() \\n\
:Ctrl<Key>Right:	forward-word() \\n\
<Key>Right:	forward-character() \\n\
:<Key>KP_Right: forward-character() \\n\
:Ctrl<Key>Left:	backward-word() \\n\
<Key>Left:	backward-character() \\n\
:<Key>KP_Left:	backward-character() \\n\
<Key>Down:	next-line() \\n\
:<Key>KP_Down:	next-line() \\n\
<Key>Up:	previous-line() \\n\
:<Key>KP_Up:	previous-line() \\n\
<Key>Delete:	delete-previous-character() \\n\
:<Key>KP_Delete: delete-previous-character() \\n\
<Key>BackSpace:	delete-previous-character() \\n\
<Key>Linefeed:	newline-and-indent() \\n\
<Key>Return:	newline() \\n\
:<Key>KP_Enter:	newline() \\n\
Ctrl<Key>backslash:	reconnect-im() \\n\
<Key>Kanji:	reconnect-im()\\n\
<Key>:		insert-char() \\n\
<EnterWindow>:	enter-window() \\n\
<LeaveWindow>:	leave-window() \\n\
<FocusIn>:	focus-in() \\n\
<FocusOut>:	focus-out() \\n\
<Btn1Down>:	select-start() \\n\
<Btn1Motion>:	extend-adjust() \\n\
<Btn1Up>:	extend-end(PRIMARY, CUT_BUFFER0) \\n\
<Btn2Down>:	insert-selection(PRIMARY, CUT_BUFFER0) \\n\
<Btn3Down>:	extend-start() \\n\
<Btn3Motion>:	extend-adjust() \\n\
<Btn3Up>:	extend-end(PRIMARY, CUT_BUFFER0)",
