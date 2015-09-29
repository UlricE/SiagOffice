"Gvu.geometry:		600x420",
"Gvu*topbox.xLayout:	100%",
"Gvu*topbox.yLayout:	30 30 100% 30",
"Gvu*menubar.shadowWidth:		1",
"Gvu*menubar.gridy:			0",
"Gvu*menubox.orientation:		horizontal",
"Gvu*menubox.borderWidth:		0",
"Gvu*menubox.vSpace:			0",
"Gvu*menubox.hSpace:			0",
"Gvu*toolbar.shadowWidth:		1",
"Gvu*toolbar.gridy:			1",
"Gvu*toolbox.orientation:		horizontal",
"Gvu*toolbox.borderWidth:		0",
"Gvu*toolbox.Command.shadowWidth:	0",
"Gvu*toolbox.Toggle.shadowWidth:		0",
"Gvu*toolbox.vSpace:			0",
"Gvu*toolbox.hSpace:			0",
"Gvu*Box.handle.height:			28",
"Gvu*Box.handle.label:",
"Gvu*mainarea.gridy:			2",
"Gvu*mainarea.xLayout:			60 4 100%",
"Gvu*mainarea.yLayout:			4 100%",
"Gvu*toc.gridx:				0",
"Gvu*toc.gridy:				1",
"Gvu*pageview.gridx:			2",
"Gvu*pageview.gridy:			1",
"Gvu*status.gridy:			3",
"Gvu*status.shadowWidth:			2",
"Gvu*status.label:			0",
"Gvu*status.resize:			False",
"Gvu*status.xLayout:			4 100% 4 100 4",
"Gvu*status.yLayout:			4 100% 4",
"Gvu*label2.gridx:			1",
"Gvu*label2.gridy:			1",
"Gvu*label2.shadowWidth:			1",
"Gvu*locator.gridx:			3",
"Gvu*locator.gridy:			1",
"Gvu*locator.shadowWidth:		1",
"Gvu*locator.label:",
"Gvu*vsep.width:				2",
"Gvu*vsep.height:			24",
"Gvu*vsep.borderWidth:			2",
"Gvu*vsep.borderColor:			grey",
"Gvu*vsep.shadowWidth:			1",
"Gvu*vsep.topShadowContrast:		-40",
"Gvu*vsep.bottomShadowContrast:		-20",
"Gvu*toolbar_command.translations:	#override	\\n\
	<Enter>:		siag-highlight(1)\\n\
	<Leave>:		siag-unhighlight(0)",
"Gvu*input:			True",
"Gvu*allowShellResize:		True",
"Gvu*ghostviewButton.label:	Ghostview",
"Gvu*pageview.useBottom:		True",
"Gvu*pageview.useRight:		True",
"Gvu*copyright.width:		495",
"Gvu*copyright.height:		400",
"Gvu*SimpleMenu.translations:	#replace \\n\
    <EnterWindow>:	highlight() \\n\
    <LeaveWindow>:	unhighlight() \\n\
    <BtnMotion>:	highlight() \\n\
    <Btn2Up>:	MenuPopdown() GhostviewForce() notify() unhighlight() \\n\
    <BtnUp>:	MenuPopdown() GhostviewDefault() notify() unhighlight()",
"Gvu*okay.label:			Okay",
"Gvu*cancel.label:		Cancel",
"Gvu*dismiss.label:		Dismiss",
"Gvu*Ghostview.background:	white",
"Gvu.translations:		#replace \\n\
    <MapNotify>:		GhostviewCheckFile() \\n\
    <Message>WM_PROTOCOLS:	GhostviewDeleteWindow()",
"Gvu*TopLevelShell.translations:	#replace \\n\
    <Message>WM_PROTOCOLS:	GhostviewDismiss()",
"Gvu*TransientShell.translations:	#replace \\n\
    <Message>WM_PROTOCOLS:	GhostviewDismiss()",
"Gvu*zoom.form.translations:	#replace \\n\
    <Key>Q:			GhostviewDeleteZoom()",
"Gvu*zoom.translations:		#replace \\n\
    <Message>WM_PROTOCOLS:	GhostviewDeleteWindow()",
"Gvu*Ghostview.translations:	#replace \\n\
    <Message>:	message() \\n\
    <EnterWindow>:	notify(0) \\n\
    <LeaveWindow>:	GhostviewEraseLocator() \\n\
    <MotionNotify>:	notify(0) \\n\
    <Btn1Down>:	notify(180 180 200 200) \\n\
    <Btn2Down>:	notify(120 120 300 300) \\n\
    <Btn3Down>:	notify(90 90 400 400)",
"Gvu*toc.translations:	#replace \\n\
    <FocusIn>:	focus-in() \\n\
    <FocusOut>:	focus-out() \\n\
    <Btn1Down>:	select-start() \\n\
    <Btn1Motion>:	extend-adjust() \\n\
    <Btn1Up>:	extend-end(PRIMARY, CUT_BUFFER0) \\n\
    <Btn2Down>:	select-start() \\n\
    <Btn2Motion>:	extend-adjust() \\n\
    <Btn2Up>:	extend-end(PRIMARY, CUT_BUFFER0) GhostviewShow() \\n\
    <Btn3Down>:	extend-start() \\n\
    <Btn3Motion>:	extend-adjust() \\n\
    <Btn3Up>:	extend-end(PRIMARY, CUT_BUFFER0)",
"Gvu*topbox.translations:	#replace \\n\
    <Key>C:	GhostviewCenter() \\n\
    <Key>Q:	GhostviewQuit() \\n\
    <Key>O:	GhostviewOpen() \\n\
    <Key>R:	GhostviewReopen() \\n\
    <Key>S:	GhostviewSave() \\n\
    Shift<Key>P:	GhostviewPrintWhole() \\n\
    <Key>P:	GhostviewPrintMarked() \\n\
    <Key>BackSpace:	GhostviewPrevious() \\n\
    <Key>Delete:	GhostviewPrevious() \\n\
    <Key>B:	GhostviewPrevious() \\n\
    <Key>Prior:	GhostviewPrevious() \\n\
    <Key>space:	GhostviewNext() \\n\
    <Key>Return:	GhostviewNext() \\n\
    <Key>F:	GhostviewNext() \\n\
    <Key>Next:	GhostviewNext() \\n\
    <Key>Tab:	GhostviewNext() \\n\
    <Key>period:	GhostviewShow() \\n\
    Ctrl<Key>L:	GhostviewShow() \\n\
    <Key>M:	GhostviewMark() \\n\
    <Key>N:	GhostviewUnmark() \\n\
    <Key>0:	GhostviewSetMagstep(0) \\n\
    <Key>1:	GhostviewSetMagstep(1) \\n\
    <Key>2:	GhostviewSetMagstep(2) \\n\
    <Key>3:	GhostviewSetMagstep(3) \\n\
    <Key>4:	GhostviewSetMagstep(4) \\n\
    <Key>5:	GhostviewSetMagstep(5) \\n\
    <Key>+:	GhostviewIncreaseMagstep() \\n\
    <Key>-:	GhostviewDecreaseMagstep() \\n\
    <Key>U:	GhostviewScrollUp() \\n\
    <Key>D:	GhostviewScrollDown() \\n\
    <Key>K:	GhostviewScrollUp() \\n\
    <Key>J:	GhostviewScrollDown() \\n\
    <Key>H:	GhostviewScrollLeft() \\n\
    <Key>L:	GhostviewScrollRight() \\n\
    Shift<Key>Up:	GhostviewForce() GhostviewSetOrientation(portrait) \\n\
    Shift<Key>Right:	GhostviewForce() GhostviewSetOrientation(landscape) \\n\
    Shift<Key>Down: GhostviewForce() GhostviewSetOrientation(upside-down) \\n\
    Shift<Key>Left:	GhostviewForce() GhostviewSetOrientation(seascape) \\n\
    <Key>Up:	GhostviewScrollUp() \\n\
    <Key>Right:	GhostviewScrollRight() \\n\
    <Key>Down:	GhostviewScrollDown() \\n\
    <Key>Left:	GhostviewScrollLeft()",
