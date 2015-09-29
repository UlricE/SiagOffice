"Pw.geometry:			640x420",
"Pw*combo_viewport.allowHoriz:	False",
"Pw*topbox.xLayout:	100%",
"Pw*topbox.yLayout:	30 30 30 100% 30",
"Pw*topbox*List*background:	white",
"Pw*textframe.topShadowContrast:	-40",
"Pw*textframe.bottomShadowContrast:	-20",
"Pw*format_command.shadowWidth:		1",
"Pw*format_command.borderWidth:		4",
"Pw*frame.shadowWidth:			1",
"Pw*menubar.shadowWidth:			1",
"Pw*menubar.gridy:			0",
"Pw*menubox.NwsMenuButton.shadowWidth:	0",
"Pw*menubox.NwsMenuButton.internalHeight:	6",
"Pw*menubox.orientation:			horizontal",
"Pw*menubox.borderWidth:			0",
"Pw*menubox.vSpace:			0",
"Pw*menubox.hSpace:			0",
"Pw*toolbar.shadowWidth:			1",
"Pw*toolbar.gridy:			1",
"Pw*toolbox.Toggle.shadowWidth:		0",
"Pw*toolbox.Command.shadowWidth:		0",
"Pw*toolbox.orientation:			horizontal",
"Pw*toolbox.borderWidth:			0",
"Pw*toolbox.vSpace:			0",
"Pw*toolbox.hSpace:			0",
"Pw*formatbar.shadowWidth:		1",
"Pw*formatbar.gridy:			2",
"Pw*formatbox.vSpace:			0",
"Pw*formatbox.hSpace:			0",
"Pw*formatbox.Toggle.shadowWidth:	0",
"Pw*formatbox.Command.shadowWidth:	0",
"Pw*formatbox.orientation:		horizontal",
"Pw*formatbox.borderWidth:		0",
"Pw*gridpane.borderWidth:		0",
"Pw*gridpane.gridy:			3",
"Pw*statusbox.borderWidth:		0",
"Pw*statusbox.gridy:			4",
"Pw*statusbox.xLayout:			4 100% 4 150 4 100 4",
"Pw*statusbox.yLayout:			4 100% 4",
"Pw*label1.shadowWidth:			1",
"Pw*label1.gridx:			3",
"Pw*label1.gridy:			1",
"Pw*label2.shadowWidth:			1",
"Pw*label2.gridx:			1",
"Pw*label2.gridy:			1",
"Pw*label3.shadowWidth:			1",
"Pw*label3.gridx:			5",
"Pw*label3.gridy:			1",
"Pw*viewport.xLayout:			25 25 50% 50% 17",
"Pw*viewport.yLayout:			25 100% 17",
"Pw*ruler.gridWidth:			4",
"Pw*ruler.borderWidth:			0",
"Pw*ruler.font:				-*-helvetica-medium-r-*-*-10-*-*-*-*-*-iso8859-*",
"Pw*grid.gridy:				1",
"Pw*grid.gridWidth:			4",
"Pw*grid.background:			white",
"Pw*grid.borderWidth:			0",
"Pw*grid.richtextDelay:			50",
"Pw*vscroll.gridx:			4",
"Pw*vscroll.gridHeight:			2",
"Pw*vscroll.orientation:			vertical",
"Pw*tabl.gridy:				2",
"Pw*tabl.label:				<<",
"Pw*tabr.label:				>>",
"Pw*tabr.gridx:				1",
"Pw*tabr.gridy:				2",
"Pw*tab.gridx:				2",
"Pw*tab.gridy:				2",
"Pw*hscroll.gridx:			3",
"Pw*hscroll.gridy:			2",
"Pw*hscroll.orientation:			horizontal",
"Pw*vsep.width:				2",
"Pw*vsep.height:				24",
"Pw*vsep.borderWidth:			2",
"Pw*vsep.borderColor:			grey",
"Pw*vsep.shadowWidth:			1",
"Pw*vsep.topShadowContrast:		-40",
"Pw*vsep.bottomShadowContrast:		-20",
"Pw*topbox.translations:		#override			\\n\
	<Key>F1:		siaghelp(pw.html)",
"Pw*toolbar_command.translations:	#override	\\n\
	<Enter>:	siag-highlight(1)\\n\
	<Leave>:	siag-unhighlight(0)",
"Pw*toolbar_toggle.translations:	#override	\\n\
	<Enter>:	siag-highlight(1 1)\\n\
	<Leave>:	siag-unhighlight(0 1)",
"Pw*grid.translations:		#override	\\n\
	:<Btn1Down>:			grid-button(point)		\\n\
	:<Btn1Motion>:			grid-button(adjust)		\\n\
	:<Btn2Down>:			grid-button(paste)		\\n\
	Shift<Btn3Down>:		grid-button(select)		\\n\
	:<Btn3Down>:			popup-shortcuts()		\\n\
	:Shift<Key>Left:		extend_left()			\\n\
	:<Key>Left:			backward_char()			\\n\
	:<Key>KP_Left:			backward_char()			\\n\
	:Shift<Key>Right:		extend_right()			\\n\
	:<Key>Right:			forward_char()			\\n\
	:<Key>KP_Right:			forward_char()			\\n\
	:Shift<Key>Down:		extend_down()			\\n\
	:<Key>Down:			next_line()			\\n\
	:<Key>KP_Down:			next_line()			\\n\
	:Shift<Key>Up:			extend_up()			\\n\
	:<Key>Up:			previous_line()			\\n\
	:<Key>KP_Up:			previous_line()			\\n\
	:<Key>Home:			execute(beginning-of-line)	\\n\
	:<Key>KP_Home:			execute(beginning-of-line)	\\n\
	:<Key>End:			execute(end-of-line)		\\n\
	:<Key>KP_End:			execute(end-of-line)		\\n\
	:Ctrl<Key>Home:			execute(beginning-of-buffer)	\\n\
	:Ctrl<Key>End:			execute(end-of-buffer)		\\n\
	:<Key>Page_Up:			execute(scroll-up)		\\n\
	:<Key>Page_Down:		execute(scroll-down)		\\n\
	:<Key>Delete:			execute(delete-char)		\\n\
	:<Key>BackSpace:		execute(delete-char-backward)	\\n\
	:<Key>Return:			execute(newline)		\\n\
	Meta<Key>x:			execute(execute-extended-command)\\n\
	:<Key>F1:			execute(help-contents)		\\n\
	:<Key>F4:			execute(more complicated test)	\\n\
	Ctrl<Key>b:			execute(toggle-format BOLD)	\\n\
	Ctrl<Key>c:			copy_to_clipboard()		\\n\
	Ctrl<Key>e:			execute(change-hadjust HADJ_CENTER)\\n\
	Ctrl<Key>f:			execute(search-forward)		\\n\
	Ctrl<Key>h:			execute(delete-char-backward)	\\n\
	Ctrl<Key>i:			execute(toggle-format ITALIC)	\\n\
	Ctrl<Key>j:			execute(change-hadjust HADJ_FULL)\\n\
	Ctrl<Key>k:			execute(kill-line)		\\n\
	Ctrl<Key>l:			execute(recenter)		\\n\
	Ctrl<Key>n:			execute(new-buffer)		\\n\
	Ctrl<Key>o:			execute(load-buffer)		\\n\
	Ctrl<Key>p:			execute(print)			\\n\
	Ctrl<Key>q:			execute(quit-program)		\\n\
	Ctrl<Key>r:			execute(change-hadjust HADJ_RIGHT)\\n\
	Ctrl<Key>s:			execute(save-buffer-as)		\\n\
	Ctrl<Key>t:			execute(add-sheet)		\\n\
	Ctrl<Key>u:			execute(toggle-format ULINE)	\\n\
	Ctrl<Key>v:			paste_from_clipboard()		\\n\
	Ctrl<Key>w:			execute(remove-sheet)		\\n\
	Ctrl<Key>x:			cut_to_clipboard()		\\n\
	Ctrl<Key>z:			execute(undo-restore)		\\n\
	<Key>:				self-insert-char()",
"Pw*plugin-parent.translations:\
	<Btn1Down>:			plugin-resize()	\\n\
	:<Motion>:			plugin-cursor()",
"Pw*font_pshell.x:			80",
"Pw*font_pshell.y:			80",
"Pw*font_okbutton.width:			80",
"Pw*font_cancelbutton.width:		80",
"Pw*alert*borderWidth:			0",
"Pw*alert*topbox.orientation:		vertical",
"Pw*alert*buttonbox.orientation:		horizontal",
"Pw*listshell*borderWidth:		0",
"Pw*listshell*topbox.orientation:	horizontal",
"Pw*listshell*listbox.orientation:	vertical",
"Pw*listshell*viewport.borderWidth:	1",
"Pw*listshell*buttonbox.orientation:	vertical",
"Pw*form_shell*NwsMenuButton.shadowWidth:	1",
