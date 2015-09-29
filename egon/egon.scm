;;
;; Runtime Library for Egon Animator
;; Copyright 1996-2000 Ulric Eriksson
;;

; Load the runtime library from SIOD
(require (string-append datadir "/siod/siod.scm"))

; Load Common
(require (string-append datadir "/common/common.scm"))

; Set up the menu
(define (create-menus)
  (require (string-append datadir "/egon/menu.scm")))

(define (init-windows)
  (require (string-append datadir "/plugins/plugin.scm")))

; X string definitions
(require (string-append datadir "/xcommon/StringDefs.scm"))

; Form interface
(require (string-append datadir "/xcommon/form.scm"))

; Animation interface
(require (string-append datadir "/egon/animator.scm"))

(define (quit-egon)
  (quit-program))

(define (new-buffer)
  (spawn "egon"))

(define (help-contents)
  (do-help "egon/egon.html"))

(define (recenter)
  (set-pr-scr))

(define *tooltip-mode* 2)

(define (preview)
  (let ((fn (string-append "/tmp/siagprint" (number->string (getpid)) ".ps")))
    (if (savematrix fn nil (psformat))
      (spawn (string-append viewer-command " " fn))
      (llpr "Can't make Postscript"))))

(define (print)
  (let ((fn (string-append "/tmp/siagprint" (number->string (getpid)) ".ps")))
    (if (savematrix fn nil (psformat))
      (spawn (string-append lpr-command " " fn))
      (llpr "Can't make Postscript"))))


(define FMT_SHIFT 16)
(define FMT_MASK (ash 15 FMT_SHIFT))

(define STY_DEFAULT 0)
(define STY_HEADER1 1)
(define STY_HEADER2 2)
(define STY_HEADER3 3)
(define STY_HEADER4 4)
(define STY_HEADER5 5)
(define STY_HEADER6 6)
(define STY_ADDRESS 7)
(define STY_OLIST 8)
(define STY_ULIST 9)
(define STY_PREFORMAT 10)
(define STY_USER1 11)
(define STY_USER2 12)
(define STY_USER3 13)
(define STY_USER4 14)
(define STY_USER5 15)
(define STY_EMBED STY_USER5)

(define HADJ_MASK (bit-or 4096 8192))
(define HADJ_LEFT 4096)
(define HADJ_CENTER 8192)
(define HADJ_RIGHT (bit-or 4096 8192))


;(define (change-font font mask)
;    (change-format font mask))

;(define (change-style style)
;  (set-style (position-row (get-point)) style))

;(define (change-adjust adj)
;  (set-adjust (position-row (get-point)) adj))

;(define (toggle-fontflag flag)
;  (let ((oldfmt (get-format nil (get-point))))
;    (if (= (bit-and oldfmt flag) flag)
;      (change-font 0 flag)
;      (change-font flag flag))))

(define (new-format name value)
  (let* ((fmt (decode-format (get-format)))
	 (field (assoc name fmt)))
    (set-cdr! field value)
    (set-format (encode-format fmt))))

(define (toggle-format name)
  (let* ((fmt (decode-format (get-format)))
	 (field (assoc name fmt))
	 (value (cdr field)))
    (new-format name (if (= value 0) 1 0))))

(define (form-ask-for-number prompt value)
  (eval (read-from-string (form-ask-for-str prompt value))))

;; TODO: make this interface nicer with better forms
(define (ins-object)
  (ani-object (form-ask-for-number "Object type:" "")))

(define (ins-tick)
  (ani-time (form-ask-for-number "Time:" "")))

(define (ins-property)
  (let* ((props (form-record "Name" "Value"))
	 (name (cdr (assoc "Name" props)))
	 (value (cdr (assoc "Value" props))))
    (ani-property
      (eval (read-from-string name))
      (eval (read-from-string value)))))

(define (ins-properties)
  (let* ((props (form-record "X" "Y" "Width" "Height" "Visible" "Text"))
	 (x (cdr (assoc "X" props)))
	 (y (cdr (assoc "Y" props)))
	 (width (cdr (assoc "Width" props)))
	 (height (cdr (assoc "Height" props)))
	 (visible (cdr (assoc "Visible" props)))
	 (text (cdr (assoc "Text" props))))
    (if (not (equal? x ""))
      (ani-property ANI_X (eval (read-from-string x))))
    (if (not (equal? y ""))
      (ani-property ANI_Y (eval (read-from-string y))))
    (if (not (equal? width ""))
      (ani-property ANI_WIDTH (eval (read-from-string width))))
    (if (not (equal? height ""))
      (ani-property ANI_HEIGHT (eval (read-from-string height))))
    (if (not (equal? visible ""))
      (ani-property ANI_VISIBLE (eval (read-from-string visible))))
    (if (not (equal? text ""))
      (ani-property ANI_TEXT text))))

(define (add-object type)
  (ani-object type)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 100 ANI_HEIGHT 100 ANI_VISIBLE 1
		ANI_COLOR BLACK ANI_FONT (+ SIZE_12 HELVETICA)))

(define (add-line)
  (add-object ANI_LINE))

(define (add-rectangle)
  (add-object ANI_RECTANGLE))

(define (add-arc)
  (add-object ANI_ARC))

(define (add-ellipse)
  (add-object ANI_ELLIPSE))

(define (add-image)
  (let ((fn (image-filename)))
    (if fn
      (begin
	(add-object ANI_PIXMAP)
	(ani-properties ANI_TEXT fn)))))

(define (add-string)
  (let ((fn (ask-for-str "String:" "")))
    (if fn
      (begin
  	(add-object ANI_STRING)
  	(ani-properties ANI_TEXT fn)))))

(define (add-point)
  (add-object ANI_POINT))

(define (add-filled-rectangle)
  (add-object ANI_FILLRECT))

(define (add-filled-arc)
  (add-object ANI_FILLARC))

(define (add-filled-ellipse)
  (add-object ANI_FILLELLIPSE))

(define (set-background)
  (ani-background (ask-for-file)))

(define (clear-background)
  (ani-background nil))

(define (set-bgrad)
  (ani-bgrad (ask-for-str "Gradient: " (get-bgrad))))

(define (clear-bgrad)
  (ani-bgrad nil))

(define (set-timing)
  (let* ((timing (form-record "Delta" "Duration"))
	 (delta (cdr (assoc "Delta" timing)))
	 (duration (cdr (assoc "Duration" timing))))
    (if (not (equal? delta ""))
      (ani-delta (eval (read-from-string delta))))
    (if (not (equal? duration ""))
      (ani-duration (eval (read-from-string duration))))))

(define (set-duration)
  (ani-duration (form-ask-for-number "Duration:" "")))

(define (set-delta)
  (ani-delta (form-ask-for-number "Delta:" "")))

(define (set-geometry)
  (let* ((geometry (form-record "Width" "Height"))
	 (width (cdr (assoc "Width" geometry)))
	 (height (cdr (assoc "Height" geometry))))
    (ani-geometry
      (eval (read-from-string width))
      (eval (read-from-string height)))))

(define (change-type)
  (set-type (form-ask-for-number "Type:" "")))

;(require (string-append datadir "/common/fonts.scm"))

(define (move-sheet-up)
  (let ((s (get-sheet)))
    (if (> s 0)
      (move-sheet s (- s 1)))))

(define (move-sheet-down)
  (let ((s (get-sheet)))
    (move-sheet s (+ s 1))))

(define (move-sheet-top)
  (let ((s (get-sheet)))
    (move-sheet s 0)))

(define (move-sheet-bottom)
  (let ((s (get-sheet)))
    (move-sheet s -1)))

(define (change-margins buf)
  (let* ((old (margin-get buf))
	 (tm (car old))
	 (bm (cadr old))
	 (lm (caddr old))
	 (rm (cadddr old))
	 (hm (car (cddddr old)))
	 (fm (cadr (cddddr old)))
	 (new nil))
    (form-begin)
    (input-field "Top:" 80 "top" 100 tm)
    (input-field "Bottom:" 80 "bottom" 100 bm)
    (input-field "Left:" 80 "left" 100 lm)
    (input-field "Right:" 80 "right" 100 rm)
    (input-field "Header:" 80 "header" 100 hm)
    (input-field "Footer:" 80 "footer" 100 fm)
    (form-okbutton "Ok")
    (form-cancelbutton "Cancel")
    (set! new (form-end))
    (set! lm (extract-number "left" new))
    (set! rm (extract-number "right" new))
    (set! tm (extract-number "top" new))
    (set! bm (extract-number "bottom" new))
    (set! hm (extract-number "header" new))
    (set! fm (extract-number "footer" new))
    (margin-set buf (list tm bm lm rm hm fm))))

(define paper-sizes
  '(("Letter" 612 792)
    ("LetterSmall" 612 792)
    ("Tabloid" 792 1224)
    ("Ledger" 1224 792)
    ("Legal" 612 1008)
    ("Statement" 396 612)
    ("Executive" 540 720)
    ("A3" 842 1190)
    ("A4" 595 842)
    ("A4Small" 595 842)
    ("A5" 420 595)
    ("B4" 729 1032)
    ("B5" 516 729)
    ("Folio" 612 936)
    ("Quarto" 610 780)
    ("10x14" 720 1008)))

(define (paper-add-size l)
  (if l (begin (form-menuentry (caar l))
		(paper-add-size (cdr l)))))

(define (change-paper-size buf)
  (let* ((old (paper-get buf))
	 (p (car old))
	 (w (cadr old))
	 (h (caddr old))
	 (o (cadddr old))
	 (new nil))
    (form-begin)
    (form-label "Size:")
    (form-properties XtNwidth 80)
    (form-menu "size")
    (paper-add-size paper-sizes)
    (form-properties XtNlabel p XtNwidth 150)
    (form-newline)
    (form-check "landscape" "Landscape")
    (form-properties XtNstate o)
    (form-newline)
    (form-okbutton "Ok")
    (form-cancelbutton "Cancel")
    (set! new (form-end))
    (set! p (extract-string "size" new))
    (set! w (cadr (assoc p paper-sizes)))
    (set! h (caddr (assoc p paper-sizes)))
    (set! o (extract-number "landscape" new))
    (paper-set buf (list p w h o))))

(define (change-headfoot buf)
  (let* ((old (headfoot-get buf))
	 (h (car old))
	 (f (cadr old))
	 (fp (caddr old))
	 (new nil))
    (form-begin)
    (input-field "Header:" 80 "header" 200 h)
    (input-field "Footer:" 80 "footer" 200 f)
    (form-check "first" "Header on first page")
    (form-properties XtNstate fp)
    (form-newline)
    (form-okbutton "Ok")
    (form-cancelbutton "Cancel")
    (set! new (form-end))
    (set! h (extract-string "header" new))
    (set! f (extract-string "footer" new))
    (set! fp (extract-number "first" new))
    (headfoot-set buf (list h f fp))))

(define (change-first-page buf)
  (let* ((old (first-page-get buf))
	 (new nil))
    (form-begin)
    (input-field "First page number:" 160 "first" 100 old)
    (form-okbutton "OK")
    (form-cancelbutton "Cancel")
    (set! new (form-end))
    (first-page-set buf (extract-number "first" new))))

(define (add-pseudo-menu name) nil)

(define (try-executable dirs fn)
  (cond ((null? dirs) nil)
	((stat (string-append (car dirs) "/" fn)) t)
	(t (try-executable (cdr dirs) fn))))

(define (executable-file fn)
  (let* ((path (getenv "PATH"))
	 (dirs (strbreakup path ":")))
    (try-executable dirs fn)))

(define (register-converters)
;  (if (executable-file "ppttohtml")
;    (register-converter "MS Powerpoint (*.ppt)"
;			"*.html" "ppttohtml %s > %s"
;			nil nil))
  (if (executable-file "ps2pdf")
    (register-converter "PDF saver (*.pdf)"
			nil nil
			"*.ps" "ps2pdf %s %s"))
  (if (executable-file "mgptotxt")
    (register-converter "Magic Point (*.mgp)"
			"*.txt" "mgptotxt %s > %s"
			nil nil)))

