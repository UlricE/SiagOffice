! This file is theme.ms, a theme to mimic MS applications.

! Use MS toolbar icons
PIXPATH:		.../common/bitmaps/ms:.../common/bitmaps
XAWM_THEME:		ms

! Buttons are not animated
Siag*Command.translations:	#override	\n\
	<Enter>:
	<Leave>:
Siag*Repeater.translations:	#override	\n\
	<Enter>:
	<Leave>:
Siag*Toggle.translations:	#override	\n\
	<Enter>:
	<Leave>:

Pw*Command.translations:	#override	\n\
	<Enter>:
	<Leave>:
Pw*Repeater.translations:	#override	\n\
	<Enter>:
	<Leave>:
Pw*Toggle.translations:	#override	\n\
	<Enter>:
	<Leave>:

Siag*selectall.shadowWidth:		1
Siag*selectall.translations:	#override		\n\
	<Enter>:		siag-highlight(2)	\n\
	<Leave>:		siag-unhighlight(1)

*Menu.box_width:			2
*box_width:				1
*topShadowContrast:			40
*bottomShadowContrast:			80
*Scrollbar.topShadowContrast:		20
*Scrollbar.bottomShadowContrast:	40

! Flat buttons on the toolbar that pop out 1 pixel when mouse passes over
Siag*formatbox.Command.shadowWidth:	0
Siag*toolbox.Command.shadowWidth:	0
Siag*toolbox.Toggle.shadowWidth:	0
Siag*toolbar_toggle.translations:	#override \n\
	<Enter>:	siag-highlight(1 1)		\n\
	<Leave>:	siag-unhighlight(0 1)
Siag*toolbar_command.translations:	#override \n\
	<Enter>:	siag-highlight(1)		\n\
	<Leave>:	siag-unhighlight(0)

! Help menu button to the left
Siag*btnHelp.gravitation:		left

