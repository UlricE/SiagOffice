;
;; Runtime Library for Pathetic Writer
;; Copyright 1996-1999 Ulric Eriksson
;;

; Load the runtime library from SIOD
(require (string-append datadir "/siod/siod.scm"))

; Load Common
(require (string-append datadir "/common/common.scm"))
(require (string-append datadir "/common/position.scm"))

; Set up the menu
(define (create-menus)
  (load (string-append datadir "/pw/menu.scm")))

(define (init-windows)
  (require (string-append datadir "/plugins/plugin.scm")))

; X string definitions
(require (string-append datadir "/xcommon/StringDefs.scm"))

; Form interface
(require (string-append datadir "/xcommon/form.scm"))

(setq ROWS-MIN 1)
(setq COLS-MIN 0)

; Find a position not out of bounds
(define (safe-position row col)
  (if (< col COLS-MIN)
    (begin
      (set! row (- row 1))
      (set! col (col-last-used row)))
    (if (> col (col-last-used row))
      (begin
        (set! row (+ row 1))
        (set! col COLS-MIN))))
  (if (< row ROWS-MIN) (set! row ROWS-MIN))
  (if (> row (max-lines)) (set! row (max-lines)))
  (make-position row col))

(define (at-beginning-of-line p)
  (<= (position-col p) COLS-MIN))

(define (at-end-of-line p)
  (>= (position-col p) (col-last-used (position-row p))))

(define (at-beginning-of-paragraph p)
  (and (at-beginning-of-line p) (= (get-bop (position-row p)) 1)))

(define (at-end-of-paragraph p)
  (and (at-end-of-line p)
       (or (at-bottom-of-buffer p)
	   (= (get-bop (+ (position-row p) 1)) 1))))

(define (at-top-of-buffer p)
  (<= (position-row p) ROWS-MIN))

(define (at-bottom-of-buffer p)
  (>= (position-row p) (line-last-used)))

(define (at-beginning-of-buffer p)
  (and (at-top-of-buffer p) (at-beginning-of-line p)))

(define (at-end-of-buffer p)
  (and (at-bottom-of-buffer p) (at-end-of-line p)))

(define (find-beginning-of-line p)
  (make-position (position-row p) COLS-MIN))

(define (find-end-of-line p)
  (make-position (position-row p) (col-last-used (position-row p))))

(define (find-beginning-of-paragraph p)
  (let ((row (position-row p)))
    (if (at-beginning-of-paragraph (make-position row 0))
      (make-position row 0)
      (find-beginning-of-paragraph (make-position (- row 1) 0)))))

(define (find-end-of-paragraph p)
  (let ((row (position-row p))
	(col (col-last-used (position-row p))))
    (if (at-end-of-paragraph (make-position row col))
	(make-position row col)
	(find-end-of-paragraph (make-position (+ row 1) 0)))))

(define (find-beginning-of-buffer)
  (make-position ROWS-MIN COLS-MIN))

(define (find-end-of-buffer)
  (let ((row (line-last-used)))
    (make-position row (col-last-used row))))

(define (backward-char)
  (set-point (previous-col (get-point))))

(define (forward-char)
  (set-point (next-col (get-point))))

(define (previous-line)
  (let* ((p (get-point))
	 (r (- (position-row p) 1))
	 (lc (col-last-used r))
	 (c (position-col p)))
    (if (>= c lc) (set! c lc))
    (set-point (make-position r c))))
 
(define (next-line)
  (let* ((p (get-point))
	 (r (+ (position-row p) 1))
	 (lc (col-last-used r))
	 (c (position-col p)))
    (if (>= c lc) (set! c lc))
    (set-point (make-position r c))))

(define (beginning-of-line)
  (set-point (find-beginning-of-line (get-point))))

(define (end-of-line)
  (set-point (find-end-of-line (get-point))))

(define (beginning-of-paragraph)
  (set-point (find-beginning-of-paragraph (get-point))))

(define (end-of-paragraph)
  (set-point (find-end-of-paragraph (get-point))))

(define (select-all)
  (let ((p (get-point)))
    (set-mark (find-beginning-of-buffer))
    (set-point (find-end-of-buffer))
    (set-block)
    (set-point p)))

(define (quit-pw)
  (quit-program))

(define (new-buffer)
  (spawn "pw"))

(define (help-contents)
  (do-help "pw/pw.html"))

(define (newline)
  (let* ((style (get-style (position-row (get-point))))
	 (nextstyle (style-follower style)))
    (split-line (position-row (get-point))
		(position-col (get-point)))
    (forward-char)
    (set-bop (position-row (get-point)) 1)
    (set-point (get-point))	; to set format
    (if (not (= style nextstyle))
      (change-style nextstyle))
    (rebreak-line (position-row (get-point)))))

(define (recenter)
  (set-pr-scr))

(define (delete-char)
  (if (at-end-of-line (get-point))
    (join-lines (position-row (get-point)))
    (remove-text 1))
  (rebreak-line (position-row (get-point))))


(define (delete-char-backward)
  (if (at-beginning-of-buffer (get-point))
    (bell)
    (begin
      (backward-char)
      (delete-char))))

(define (kill-line)
  (let ((row (position-row (get-point)))
	(col (position-col (get-point))))
    (if (at-end-of-line (get-point))
      (join-lines row)
      (begin
	(split-line row col)
	(delete-lines (+ row 1) 1)))
    (rebreak-line (position-row (get-point)))
    (set-pr-scr)))

(define (delete-block)
  (let ((ur (position-row (get-blku)))
	(uc (position-col (get-blku)))
	(lr (position-row (get-blkl)))
	(lc (position-col (get-blkl))))
    (if (> ur 0)
      (begin
    (llpr "foo")
    	(split-line lr (+ lc 1))
    	(split-line ur uc)
    	(delete-lines (+ ur 1) (+ (- lr ur) 1))
    	(join-lines ur)
	(rebreak-line (position-row (get-point)))
    	(set-pr-scr)))))

(define *cut-buffer* nil)

(define (copy-block-to-string)
  (set! *cut-buffer* (pack-area (position-row (get-blku))
				(position-col (get-blku))
				(position-row (get-blkl))
				(position-col (get-blkl))))
  (buffer-changed nil)
  (set-pr-scr))

(define (cut-block-to-string)
  (copy-block-to-string)
  (delete-block))

(define (paste-block-from-string)
  (unpack-area *cut-buffer* (get-point-row) (get-point-col))
  (buffer-changed nil)
  (set-pr-scr))


(define (search-forward)
  (require (string-append datadir "/pw/find.scm"))
  (search-forward))

(define (search-backward)
  (require (string-append datadir "/pw/find.scm"))
  (search-backward))


(define STY_DEFAULT 0)
(define STY_HEADER1 1)
(define STY_HEADER2 2)
(define STY_HEADER3 3)
(define STY_HEADER4 4)
(define STY_HEADER5 5)
(define STY_HEADER6 6)
(define STY_ADDRESS 7)
(define STY_TITLE 8)
(define STY_ABSTRACT 9)
(define STY_PREFORMAT 10)
(define STY_USER1 11)
(define STY_USER2 12)
(define STY_USER3 13)
(define STY_USER4 14)
(define STY_USER5 15)
(define STY_EMBED STY_USER5)

(define (change-style style)
  (beginning-of-paragraph)
  (set-mark (find-end-of-paragraph (get-point)))
  (set-style (position-row (get-point)) style)
  (while (< (position-row (get-point)) (position-row (get-mark)))
	 (set-format nil
		     (position-row (get-mark))
		     (position-col (get-mark))
		     (get-format nil
				 (position-row (get-point))
				 (position-col (get-point))))
	 (set-mark (previous-col (get-mark))))
  (rebreak-line (position-row (get-point))))

(define (change-hadjust hadj)
  (let ((r (position-row (get-point)))
	(r1 (position-row (get-blku)))
	(r2 (position-row (get-blkl))))
    (if (or (< r r1) (> r r2))
      (set-hadjust r hadj)
      (while (<= r1 r2)
        (set-hadjust r1 hadj)
        (set! r1 (+ r1 1))))))

;  (set-hadjust (position-row (get-point)) hadj))

;(define (change-vadjust vadj)
;  (if (= vadj (bit-and (get-format nil (get-point)) VADJ_MASK))
;    (change-font VADJ_CENTER VADJ_MASK)
;    (change-font vadj VADJ_MASK)))

(define HADJ_MASK (bit-or 4096 8192))
(define HADJ_LEFT 0)
(define HADJ_FULL 4096)
(define HADJ_CENTER 8192)
(define HADJ_RIGHT (bit-or 4096 8192))
(define VADJ_MASK (bit-or 16384 32768))
(define VADJ_TOP 16384)
(define VADJ_CENTER 0)
(define VADJ_BOTTOM (+ 16384 32768))
(define BOLD "bold")
(define ITALIC "italic")
(define ULINE "uline")
(define STRIKE "strike")

(define (change-format buf r c name value)
  (let* ((fmt (decode-format (get-format buf r c)))
	 (field (assoc name fmt)))
    (set-cdr! field value)
    (set-format buf r c (encode-format fmt))))

(define (line-format buf r c1 c2 name value)
  (while (< c1 c2)
    (change-format buf r c1 name value)
    (set! c1 (+ c1 1))))

(define (area-format buf r1 c1 r2 c2 name value)
  (let ((r r1))
    (while (<= r r2)
      (line-format buf
		   r
		   (if (= r r1) c1 COLS-MIN)
		   (if (= r r2) (+ c2 1) (col-last-used r))
		   name
		   value)
      (set! r (+ r 1))))
  (set-pr-scr))

; If point within block, change a single property for every character in
; the block, then change current format.
; If point outside block, *only* change current format.
(define (new-format name value)
  (let ((r (position-row (get-point)))
	(c (position-col (get-point)))
	(r1 (position-row (get-blku)))
	(c1 (position-col (get-blku)))
	(r2 (position-row (get-blkl)))
	(c2 (position-col (get-blkl))))
    (if (and (or (>= r r1)
	         (and (= r r1)
		      (>= c c1)))
             (or (<= r r2)
	         (and (= r r2)
		      (<= c c2))))
      (area-format nil r1 c1 r2 c2 name value))
    (let* ((fmt (decode-format (get-format nil r c)))
	   (field (assoc name fmt)))
      (set-cdr! field value)
      (set-current-fmt (encode-format fmt))))
  (set-pr-scr))

; Interactively select value from a list
(define (new_attribute name)
  (let ((value (select-attribute name)))
    (if (not (null? value))
      (new-format name value))))

(define (toggle-format name)
  (let* ((r (get-point-row))
	 (c (get-point-col))
	 (fmt (decode-format (get-format nil r c)))
	 (field (assoc name fmt))
	 (value (cdr field)))
    (new-format name (if (= value 0) 1 0))))

(define (toggle-vadj newv)
  (let* ((r (get-point-row))
	 (c (get-point-col))
	 (fmt (decode-format (get-format nil r c)))
	 (field (assoc "vadj" fmt))
	 (oldv (cdr field))
	 (value (if (= oldv newv) VADJ_CENTER newv)))
    (new-format "vadj" value)))

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

(define (print-format)
  (let* ((props (form-record "Previewer" "Print Command"))
	 (viewcmd (cdr (assoc "Previewer" props)))
	 (printcmd (cdr (assoc "Print Command" props))))
    (if (not (equal? viewcmd ""))
      (set! viewer-command viewcmd))
    (if (not (equal? printcmd ""))
      (set! lpr-command printcmd))))

; load the fonts
;(require (string-append datadir "/common/fonts.scm"))

; and the styles
(define (register-style . ap)
  (register-style-wrapper ap))

(require (string-append datadir "/pw/styles.scm"))


(define (special-char c)
  (let ((text "x"))
    (aset text 0 c)
    (insert-text text)
    (forward-char)))

(define (ins-special-char)
  (special-char (string->number (form-ask-for-str "Code:" ""))))

; must check if already first because (- 0 1) would become -1 => last
(define (move-sheet-up)
  (let ((s (get-sheet)))
    (if (> s 0)
      (move-sheet s (- s 1)))))

; move-sheet checks new position, so we don't have to
(define (move-sheet-down)
  (let ((s (get-sheet)))
    (move-sheet s (+ s 1))))

; put first
(define (move-sheet-top)
  (let ((s (get-sheet)))
    (move-sheet s 0)))

; -1 means last
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

; Standard paper sizes

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
    (form-okbutton "OK")
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

(define (change-tab-distance buf)
  (let* ((old (tab-distance-get buf))
	 (new nil))
    (form-begin)
    (input-field "Tab distance:" 160 "distance" 100 old)
    (form-okbutton "OK")
    (form-cancelbutton "Cancel")
    (set! new (form-end))
    (tab-distance-set buf (extract-number "distance" new))))

; Used by menu.scm and docs/menus.scm
(define (add-pseudo-menu name) nil)

(define (goto-line)
  (set-point (make-position (string->number (form-ask-for-str "Line:" "")) 0)))

(define (try-executable dirs fn)
  (cond ((null? dirs) nil)
	((stat (string-append (car dirs) "/" fn)) t)
	(t (try-executable (cdr dirs) fn))))

(define (executable-file fn)
  (let* ((path (getenv "PATH"))
	 (dirs (strbreakup path ":")))
    (try-executable dirs fn)))

(define (register-converters)
; If wvHtml is available, use it for all Word versions
;  (if (executable-file "wvWare")
;    (register-converter "MS Word (*.doc)"
;			"*.html" "wvHtml -c iso-8859-15 %s > %s"
;			"*.html" "wvWare -c iso-8859-15 %s > %s 2> /dev/null"
;			nil nil)
; otherwise look for catdoc and mswordview
;    (begin
;      (if (executable-file "catdoc")
;        (register-converter "MS Word 2.0 - 95 (*.doc)"
;		            "*.txt" "catdoc %s > %s"
;		            nil nil))
;      (if (executable-file "mswordview")
;        (register-converter "MS Word 97 (*.doc)"
;                            "*.html" "mswordview -n %s -o %s"
;                            nil nil))))
  (register-converter "MS Word (*.doc)"
		      "*.html" (string-append "antiword -x db %s | "
					      libdir
					      "/o3read/dbtohtml | "
					      libdir
					      "/o3read/utf8tolatin1 > %s")
		      nil nil)
  (if (executable-file "wp2x")
    (register-converter "Word Perfect 5.1 (*.wp)"
			"*.html" "wp2x html.cfg %s > %s"
			nil nil))
  (if (executable-file "pptHtml")
    (register-converter "MS Powerpoint (*.ppt)"
			"*.html" "pptHtml %s > %s"
			nil nil))
  (if (executable-file "pdftohtml.bin")
    (register-converter "PDF loader (*.pdf)"
			"*.html" "pdftohtml.bin -i -noframes -stdout %s > %s"
			nil nil))
  (if (executable-file "ps2pdf")
    (register-converter "PDF saver (*.pdf)"
			nil nil
			"*.ps" "ps2pdf %s %s"))
  (if (executable-file "groff")
    (register-converter "Man page (*.[0-9])"
			"*.txt" "groff -Tascii -mandoc %s > %s"
			nil nil))
  (if (executable-file "lyx2html")
    (register-converter "LyX (*.lyx)"
			"*.html" "lyx2html %s %s"
			nil nil))
  (if (executable-file "psiconv")
    (register-converter "Psion (*.psi)"
			"*.html" "psiconv %s > %s"
			nil nil))
  (register-converter "Abi Word (*.abw)"
		      "*.html" "cp %s %s"
		      nil nil)
  (if (executable-file "wrd2html")
    (register-converter "Psion Word (*.wrd)"
			"*.html" "wrd2html %s > %s"
			nil nil))
  (if (executable-file "2rtf")
    (register-converter "AUIS ez (*.ez)"
			"*.rtf" "2rtf %s %s"
			nil nil)))

(define (zoom-adjust)
  (set-zoom (/ (string->number (ask-for-str "Zoom %:" "130")) 100)))

