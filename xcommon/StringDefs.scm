;;
;; StringDefs.scm
;;
;; Necessary definitions for interfacing with X

(define XtNaccelerators "accelerators")
(define XtNallowHoriz "allowHoriz")
(define XtNallowVert "allowVert")
(define XtNancestorSensitive "ancestorSensitive")
(define XtNbackground "background")
(define XtNbackgroundPixmap "backgroundPixmap")
(define XtNbitmap "bitmap")
(define XtNborderColor "borderColor")
(define XtNborder "borderColor")
(define XtNborderPixmap "borderPixmap")
(define XtNborderWidth "borderWidth")
(define XtNcallback "callback")
(define XtNchildren "children")
(define XtNcolormap "colormap")
(define XtNdepth "depth")
(define XtNdestroyCallback "destroyCallback")
(define XtNeditType "editType")
(define XtNfile "file")
(define XtNfont "font")
(define XtNforceBars "forceBars")
(define XtNforeground "foreground")
(define XtNfunction "function")
(define XtNheight "height")
(define XtNhighlight "highlight")
(define XtNhSpace "hSpace")
(define XtNindex "index")
(define XtNinitialResourcesPersistent "initialResourcesPersistent")
(define XtNinnerHeight "innerHeight")
(define XtNinnerWidth "innerWidth")
(define XtNinnerWindow "innerWindow")
(define XtNinsertPosition "insertPosition")
(define XtNinternalHeight "internalHeight")
(define XtNinternalWidth "internalWidth")
(define XtNjumpProc "jumpProc")
(define XtNjustify "justify")
(define XtNknobHeight "knobHeight")
(define XtNknobIndent "knobIndent")
(define XtNknobPixel "knobPixel")
(define XtNknobWidth "knobWidth")
(define XtNlabel "label")
(define XtNlength "length")
(define XtNlowerRight "lowerRight")
(define XtNmappedWhenManaged "mappedWhenManaged")
(define XtNmenuEntry "menuEntry")
(define XtNname "name")
(define XtNnotify "notify")
(define XtNnumChildren "numChildren")
(define XtNorientation "orientation")
(define XtNparameter "parameter")
(define XtNpixmap "pixmap")
(define XtNpopupCallback "popupCallback")
(define XtNpopdownCallback "popdownCallback")
(define XtNresize "resize")
(define XtNreverseVideo "reverseVideo")
(define XtNscreen "screen")
(define XtNscrollProc "scrollProc")
(define XtNscrollDCursor "scrollDCursor")
(define XtNscrollHCursor "scrollHCursor")
(define XtNscrollLCursor "scrollLCursor")
(define XtNscrollRCursor "scrollRCursor")
(define XtNscrollUCursor "scrollUCursor")
(define XtNscrollVCursor "scrollVCursor")
(define XtNselection "selection")
(define XtNselectionArray "selectionArray")
(define XtNsensitive "sensitive")
(define XtNshown "shown")
(define XtNspace "space")
(define XtNstring "string")
(define XtNtextOptions "textOptions")
(define XtNtextSink "textSink")
(define XtNtextSource "textSource")
(define XtNthickness "thickness")
(define XtNthumb "thumb")
(define XtNthumbProc "thumbProc")
(define XtNtop "top")
(define XtNtranslations "translations")
(define XtNunrealizeCallback "unrealizeCallback")
(define XtNupdate "update")
(define XtNuseBottom "useBottom")
(define XtNuseRight "useRight")
(define XtNvalue "value")
(define XtNvSpace "vSpace")
(define XtNwidth "width")
(define XtNwindow "window")
(define XtNx "x")
(define XtNy "y")
(define XtCAccelerators "Accelerators")
(define XtCBackground "Background")
(define XtCBitmap "Bitmap")
(define XtCBoolean "Boolean")
(define XtCBorderColor "BorderColor")
(define XtCBorderWidth "BorderWidth")
(define XtCCallback "Callback")
(define XtCColormap "Colormap")
(define XtCColor "Color")
(define XtCCursor "Cursor")
(define XtCDepth "Depth")
(define XtCEditType "EditType")
(define XtCEventBindings "EventBindings")
(define XtCFile "File")
(define XtCFont "Font")
(define XtCForeground "Foreground")
(define XtCFraction "Fraction")
(define XtCFunction "Function")
(define XtCHeight "Height")
(define XtCHSpace "HSpace")
(define XtCIndex "Index")
(define XtCInitialResourcesPersistent "InitialResourcesPersistent")
(define XtCInsertPosition "InsertPosition")
(define XtCInterval "Interval")
(define XtCJustify "Justify")
(define XtCKnobIndent "KnobIndent")
(define XtCKnobPixel "KnobPixel")
(define XtCLabel "Label")
(define XtCLength "Length")
(define XtCMappedWhenManaged "MappedWhenManaged")
(define XtCMargin "Margin")
(define XtCMenuEntry "MenuEntry")
(define XtCNotify "Notify")
(define XtCOrientation "Orientation")
(define XtCParameter "Parameter")
(define XtCPixmap "Pixmap")
(define XtCPosition "Position")
(define XtCReadOnly "ReadOnly")
(define XtCResize "Resize")
(define XtCReverseVideo "ReverseVideo")
(define XtCScreen "Screen")
(define XtCScrollProc "ScrollProc")
(define XtCScrollDCursor "ScrollDCursor")
(define XtCScrollHCursor "ScrollHCursor")
(define XtCScrollLCursor "ScrollLCursor")
(define XtCScrollRCursor "ScrollRCursor")
(define XtCScrollUCursor "ScrollUCursor")
(define XtCScrollVCursor "ScrollVCursor")
(define XtCSelection "Selection")
(define XtCSensitive "Sensitive")
(define XtCSelectionArray "SelectionArray")
(define XtCSpace "Space")
(define XtCString "String")
(define XtCTextOptions "TextOptions")
(define XtCTextPosition "TextPosition")
(define XtCTextSink "TextSink")
(define XtCTextSource "TextSource")
(define XtCThickness "Thickness")
(define XtCThumb "Thumb")
(define XtCTranslations "Translations")
(define XtCValue "Value")
(define XtCVSpace "VSpace")
(define XtCWidth "Width")
(define XtCWindow "Window")
(define XtCX "X")
(define XtCY "Y")
(define XtRAcceleratorTable "AcceleratorTable")
(define XtRAtom "Atom")
(define XtRBitmap "Bitmap")
(define XtRBool "Bool")
(define XtRBoolean "Boolean")
(define XtRCallback "Callback")
(define XtRCallProc "CallProc")
(define XtRCardinal "Cardinal")
(define XtRColor "Color")
(define XtRColormap "Colormap")
(define XtRCursor "Cursor")
(define XtRDimension "Dimension")
(define XtRDisplay "Display")
(define XtREditMode "EditMode")
(define XtREnum "Enum")
(define XtRFile "File")
(define XtRFloat "Float")
(define XtRFont "Font")
(define XtRFontStruct "FontStruct")
(define XtRFunction "Function")
(define XtRGeometry "Geometry")
(define XtRImmediate "Immediate")
(define XtRInitialState "InitialState")
(define XtRInt "Int")
(define XtRJustify "Justify")
(define XtRLongBoolean "Bool")
(define XtRObject "Object")
(define XtROrientation "Orientation")
(define XtRPixel "Pixel")
(define XtRPixmap "Pixmap")
(define XtRPointer "Pointer")
(define XtRPosition "Position")
(define XtRScreen "Screen")
(define XtRShort "Short")
(define XtRString "String")
(define XtRStringArray "StringArray")
(define XtRStringTable "StringTable")
(define XtRUnsignedChar "UnsignedChar")
(define XtRTranslationTable "TranslationTable")
(define XtRVisual "Visual")
(define XtRWidget "Widget")
(define XtRWidgetClass "WidgetClass")
(define XtRWidgetList "WidgetList")
(define XtRWindow "Window")
(define XtEoff "off")
(define XtEfalse "false")
(define XtEno "no")
(define XtEon "on")
(define XtEtrue "true")
(define XtEyes "yes")
(define XtEvertical "vertical")
(define XtEhorizontal "horizontal")
(define XtEtextRead "read")
(define XtEtextAppend "append")
(define XtEtextEdit "edit")
(define XtExtdefaultbackground "xtdefaultbackground")
(define XtExtdefaultforeground "xtdefaultforeground")
(define XtExtdefaultfont "xtdefaultfont")
(define XtNfontSet "fontSet")
(define XtRFontSet "FontSet")
(define XtCFontSet "FontSet")
(define XtRGravity "Gravity")
(define XtNcreateHook "createHook")
(define XtNchangeHook "changeHook")
(define XtNconfigureHook "configureHook")
(define XtNgeometryHook "geometryHook")
(define XtNdestroyHook "destroyHook")
(define XtNshells "shells")
(define XtNnumShells "numShells")
(define XtRCommandArgArray "CommandArgArray")
(define XtRDirectoryString "DirectoryString")
(define XtREnvironmentArray "EnvironmentArray")
(define XtRRestartStyle "RestartStyle")
(define XtRSmcConn "SmcConn")
(define XtHcreate "Xtcreate")
(define XtHsetValues "XtsetValues")
(define XtHmanageChildren "XtmanageChildren")
(define XtHunmanageChildren "XtunmanageChildren")
(define XtHmanageSet "XtmanageSet")
(define XtHunmanageSet "XtunmanageSet")
(define XtHrealizeWidget "XtrealizeWidget")
(define XtHunrealizeWidget "XtunrealizeWidget")
(define XtHaddCallback "XtaddCallback")
(define XtHaddCallbacks "XtaddCallbacks")
(define XtHremoveCallback "XtremoveCallback")
(define XtHremoveCallbacks "XtremoveCallbacks")
(define XtHremoveAllCallbacks "XtremoveAllCallbacks")
(define XtHaugmentTranslations "XtaugmentTranslations")
(define XtHoverrideTranslations "XtoverrideTranslations")
(define XtHuninstallTranslations "XtuninstallTranslations")
(define XtHsetKeyboardFocus "XtsetKeyboardFocus")
(define XtHsetWMColormapWindows "XtsetWMColormapWindows")
(define XtHsetMappedWhenManaged "XtsetMappedWhenManaged")
(define XtHmapWidget "XtmapWidget")
(define XtHunmapWidget "XtunmapWidget")
(define XtHpopup "Xtpopup")
(define XtHpopupSpringLoaded "XtpopupSpringLoaded")
(define XtHpopdown "Xtpopdown")
(define XtHconfigure "Xtconfigure")
(define XtHpreGeometry "XtpreGeometry")
(define XtHpostGeometry "XtpostGeometry")
(define XtHdestroy "Xtdestroy")

; And a few not in StringDefs.h
(define XtNshadowWidth "shadowWidth")
(define XtNstate "state")
