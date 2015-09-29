;;
;; Runtime library for Siag
;; Copyright 1996-2000 Ulric Eriksson
;;

; Load the runtime library from Siod
(require (string-append datadir "/siod/siod.scm"))



; Load Common
(require (string-append datadir "/common/common.scm"))
(require (string-append datadir "/common/position.scm"))

; Set up the menu
(define (create-menus)
  (load (string-append datadir "/siag/trans.scm"))
  (load (string-append datadir "/siag/menu.scm"))
  (load (string-append datadir "/siag/toolbar.scm")))

; Set up the key bindings
(define (init-windows)
  (require (string-append datadir "/siag/keytable.scm"))
  (require (string-append datadir "/plugins/plugin.scm")))


; X string definitions
(require (string-append datadir "/xcommon/StringDefs.scm"))

; Form interface
(require (string-append datadir "/xcommon/form.scm"))

; Data interface
(require (string-append datadir "/siag/data.scm"))

(define EMPTY 0)
(define LABEL 1)
(define ERROR 2)
(define CONSTANT 3)
(define EXPRESSION 4)
(define STRING 5)
(define EMBED 6)
(define MNUMBER 7)
(define MTEXT 8)

(setq P-MIN (make-position 1 1))
(setq P-MAX (make-position 100000 100000))

;; Functions that do approximately the same as position.c
;;
;; The following functions are implemented in C:
;; get-point, set-point, get-mark, set-mark, get-blkl, set-blkl,
;; get-blku, set-blku, get-prot, set-prot
;; line-last-used, col-last-used, max-lines, max-columns

(define (find-beginning-of-buffer)
  (get-prot))

(define (find-end-of-buffer)
  (let ((row (line-last-used)))
    (make-position row (col-last-used row))))

; Find a position not out of bounds
(define (safe-position row col)
  (let* ((top (find-beginning-of-buffer))
         (top-r (position-row top))
         (top-c (position-col top)))
    (if (< row top-r) (set! row top-r))
    (if (> row (position-row P-MAX)) (set! row (position-row P-MAX)))
    (if (< col top-c) (set! col top-c))
    (if (> col (position-col P-MAX)) (set! col (position-col P-MAX))))
  (make-position row col))

;; And now: commands written in Scheme

(define (beginning-of-line)
  (set-point-col (position-col (find-beginning-of-buffer))))

(define (end-of-line)
  (set-point-col (col-last-used (get-point-row))))

(define (select-all)
  (let ((p (get-point)))
	(set-mark P-MIN)
	(set-point P-MAX)
	(set-block)
	(set-point p)))

(define (top-of-buffer)
  (set-point-row (position-row (find-beginning-of-buffer))))

(define (bottom-of-buffer)
  (set-point-row (line-last-used)))

(define (forward-cell)
  (set-point (next-col (get-point))))

(define (backward-cell)
  (set-point (previous-col (get-point))))

(define (next-line)
  (set-point (next-row (get-point))))

(define (previous-line)
  (set-point (previous-row (get-point))))

;;
;; A few shorthand functions for my convenience
;;

; These two are *variables* now, not functions
;(define (R) (row))
;(define (C) (col))
(define (@ r c) (get-cell r c))
(define (@$ r c) (get-string r c))
(define +$ string-append)
(define (?$ t s) (string-search t s))
(define (up$ s) (string-upcase s))
(define (down$ s) (string-downcase s))
;@href(x)
;@Returns the contents from the cell x positions to the right.
;@href(-2) returns the cell 2 positions to the left.
;@vref
(define (href x) (@ R (+ C x)))
;@vref(x)
;@Returns the contents from the cell x positions down.
;@vref(-2) returns the cell 2 positions up.
;@href
(define (vref x) (@ (+ R x) C))
(define (lastyear) (href -12))
(define (lastmonth) (href -1))

;@get_cell(row, column, [sheet])
;@Returns the value in the specified row and column. If a sheet is
; specified, it is used. Otherwise the current sheet is used.
;@get_cell(2, 3) returns the value in row 2, column 3 in the current sheet.
;
; get_cell(2, 3, "Sheet 2") returns the value in row 2, column 3
; in the sheet named "Sheet 2".
;
; get_cell(2, 3, "1998.siag:") returns the value in row 2, column 3
; in the first sheet of buffer "1998.siag".
;
; get_cell(2, 3, "1998.siag:January") returns the value in row 2,
; column 3 in the sheet named "January" in the buffer "1998.siag".
;@href, vref
(define (get_cell r c . s)
  (x-get-cell r c (car s)))

;;
;; Some more commands
;;

(define (quit-siag)
  (quit-program))

(define (new-buffer)
  (spawn "siag"))

(define (help-contents)
  (do-help "siag/siag.html"))

(define (get-cell-coords top point)
  (let ((top-row (position-row top))
	(top-col (position-col top))
	(cell-row (position-row point))
	(cell-col (position-col point))
	(cell-y 0)
	(cell-x 0))
    (while (< cell-row top-row)
      (set! cell-row (+ cell-row 1))
      (set! cell-y (- cell-y (get-cell-height cell-row))))
    (while (> cell-row top-row)
      (set! cell-row (- cell-row 1))
      (set! cell-y (+ cell-y (get-cell-height cell-row))))
    (while (< cell-col top-col)
      (set! cell-col (+ cell-col 1))
      (set! cell-x (- cell-x (get-cell-width cell-col))))
    (while (> cell-col top-col)
      (set! cell-col (- cell-col 1))
      (set! cell-x (+ cell-x (get-cell-width cell-col))))
    (list cell-x cell-y)))

; move point inside visible area
(define (fix-point width height)
  (let ((cur-x (car (get-cell-coords (get-top) (get-point))))
	(cur-y (cadr (get-cell-coords (get-top) (get-point))))
	(row (position-row (get-point)))
	(col (position-col (get-point))))
    (while (< cur-y 0)
      (set! cur-y (+ cur-y (get-cell-height row)))
      (set! row (+ row 1)))
    (while (> (+ cur-y (get-cell-height row)) height)
      (set! row (- row 1))
      (set! cur-y (- cur-y (get-cell-height row))))
    (while (< cur-x 0)
      (set! cur-x (+ cur-x (get-cell-width col)))
      (set! col (+ col 1)))
    (while (> (+ cur-x (get-cell-width col)) width)
      (set! col (- col 1))
      (set! cur-x (- cur-x (get-cell-width col))))
    (set-point (make-position row col))))

(define (scroll-down)
  (let ((width (caddr (get-geometry)))
	(height (cadddr (get-geometry)))
	(top (get-top))
	(new-y 0)
	(new-row 0))
    (set! new-y (- 20 height))
    (set! new-row (position-row top))
    (while (< new-y 20)
      (set! new-y (+ new-y 20))
      (set! new-row (- new-row 1)))
    (set-top (safe-position new-row (position-col top)))
    (fix-point width height)
    (set-pr-scr)))

(define (scroll-cell-down)
  (let ((width (caddr (get-geometry)))
	(height (cadr (cddr (get-geometry))))
	(top (get-top))
	(new-y 0)
	(new-row 0))
    (set! new-y -20)
    (set! new-row (position-row top))
    (while (< new-y 0)
      (set! new-y (+ new-y 20))
      (set! new-row (- new-row 1)))
    (set-top (safe-position new-row (position-col top)))
    (fix-point width height)
    (set-pr-scr)))

(define (scroll-up)
  (let ((width (caddr (get-geometry)))
	(height (cadr (cddr (get-geometry))))
	(top (get-top))
	(new-y 0)
	(new-row 0))
    (set! new-y (- height 20))
    (set! new-row (position-row top))
    (while (> new-y 0)
      (set! new-y (- new-y 20))
      (set! new-row (+ new-row 1)))
    (set-top (safe-position new-row (position-col top)))
    (fix-point width height)
    (set-pr-scr)))

(define (scroll-cell-up)
  (let ((width (caddr (get-geometry)))
	(height (cadr (cddr (get-geometry))))
	(top (get-top))
	(new-y 0)
	(new-row 0))
    (set! new-y 20)
    (set! new-row (position-row top))
    (while (> new-y 0)
      (set! new-y (- new-y 20))
      (set! new-row (+ new-row 1)))
    (set-top (safe-position new-row (position-col top)))
    (fix-point width height)
    (set-pr-scr)))

(define (scroll-left)
  (let ((width (caddr (get-geometry)))
	(height (cadr (cddr (get-geometry))))
	(top (get-top))
	(new-x 0)
	(new-col 0))
    (set! new-x (- 80 width))
    (set! new-col (position-col top))
    (while (< new-x 0)
      (set! new-x (+ new-x 80))
      (set! new-col (- new-col 1)))
    (set-top (safe-position (position-row top) new-col))
    (fix-point width height)
    (set-pr-scr)))

(define (scroll-cell-left)
  (let ((width (caddr (get-geometry)))
	(height (cadr (cddr (get-geometry))))
	(top (get-top))
	(new-x 0)
	(new-col 0))
    (set! new-x -80)
    (set! new-col (position-col top))
    (while (< new-x 0)
      (set! new-x (+ new-x 80))
      (set! new-col (- new-col 1)))
    (set-top (safe-position (position-row top) new-col))
    (fix-point width height)
    (set-pr-scr)))

(define (scroll-right)
  (let ((width (caddr (get-geometry)))
	(height (cadr (cddr (get-geometry))))
	(top (get-top))
	(new-x 0)
	(new-col 0))
    (set! new-x (- width 80))
    (set! new-col (position-col top))
    (while (> new-x 0)
      (set! new-x (- new-x 80))
      (set! new-col (+ new-col 1)))
    (set-top (safe-position (position-row top) new-col))
    (fix-point width height)
    (set-pr-scr)))

(define (scroll-cell-right)
  (let ((width (caddr (get-geometry)))
	(height (cadr (cddr (get-geometry))))
	(top (get-top))
	(new-x 0)
	(new-col 0))
    (set! new-x 80)
    (set! new-col (position-col top))
    (while (> new-x 0)
      (set! new-x (- new-x 80))
      (set! new-col (+ new-col 1)))
    (set-top (safe-position (position-row top) new-col))
    (fix-point width height)
    (set-pr-scr)))

(define data-type-list
  (list (cons EMPTY "EMPTY")
	(cons ERROR "ERROR")
	(cons LABEL "LABEL")
	(cons CONSTANT "CONSTANT")
	(cons EXPRESSION "EXPRESSION")
	(cons STRING "STRING")
	(cons EMBED "EMBED")
	(cons MNUMBER "MNUMBER")
	(cons MTEXT "MTEXT")))

(define (type->string type)
  (or
    (cdr (assoc type data-type-list))
    "ERROR"))

(define (what-cursor-position)
  (llpr
    (string-append "["
      (number->string (row) 10) ","
      (number->string (col) 10) "] is "
      (type->string (get-type nil (row) (col))) ": "
      (get-text (row) (col)))))

(define (insert-line)
  (downshift-matrix nil (position-row (get-point)))
  (buffer-changed nil)
  (set-pr-scr))

(define (remove-line)
  (upshift-matrix nil (position-row (get-point)))
  (buffer-changed nil)
  (set-pr-scr))

(define (insert-col)
  (rightshift-matrix nil (position-col (get-point)))
  (buffer-changed nil)
  (set-pr-scr))

(define (remove-col)
  (leftshift-matrix nil (position-col (get-point)))
  (buffer-changed nil)
  (set-pr-scr))

(define (delete-block)
  (let ((blku (get-blku))
	(blkl (get-blkl))
	(i 0)
	(j 0))
    (undo-save (position-row blku)
	       (position-col blku)
	       (position-row blkl)
	       (position-col blkl))
    (set! i (position-row blku))
    (while (and (<= i (position-row blkl)) (<= i (line-last-used)))
      (set! j (position-col blku))
      (while (and (<= j (position-col blkl)) (<= j (col-last-used i)))
	(if (not (null? (set-data nil "" 0 EMPTY (make-position i j))))
	  (buffer-changed nil))
	(set! j (+ j 1)))
      (set! i (+ i 1)))
    (calc-matrix nil)
    (set-pr-scr)))

(define *cut-buffer* nil)

(define (copy-block-to-string)
  (set! *cut-buffer* (pack-area (position-row (get-blku))
				(position-col (get-blku))
				(position-row (get-blkl))
				(position-col (get-blkl))))
  (buffer-changed nil)
  (calc-matrix nil)
  (set-pr-scr))

(define (cut-block-to-string)
  (copy-block-to-string)
  (delete-block))

(define (paste-block-from-string)
  (unpack-area *cut-buffer* (get-point-row) (get-point-col))
  (buffer-changed nil)
  (calc-matrix nil)
  (set-pr-scr))

(define (recalc-matrix)
  (llpr "calculating...")
  (calc-matrix nil)
  (set-pr-scr))

(define (recenter)
  (set-pr-scr))

(define (delete-cell)
  (if (not (null? (set-data nil "" 0 EMPTY (get-point))))
    (buffer-changed nil))
  (set-pr-scr))

(define (edit-label . args)
  (let* ((row (position-row (get-point)))
	 (col (position-col (get-point)))
	 (b (get-text row col)))
    (if args
      (set! b (edit-cell "Label:" (car args)))
      (set! b (edit-cell "Label:" b)))
    (if (not (null? b))
      (if (not (null? (set-data nil b 0 LABEL (get-point))))
	(begin
	  (calc-matrix nil)
	  (set-pr-scr)
	  (buffer-changed nil))
	(llpr "String pool full")))))

; The following function always inserts as error. This gets resolved
; during the first recalculation.
; This function superseded by interpreter-test (useless name)
;(define (edit-expression)
;  (let ((row (position-row (get-point)))
;	(col (position-col (get-point))))
;    (set! b (ask-for-str "Expression:" (get-text row col)))
;    (if (not (null? b))
;      (if (not (null? (set-data nil b 0 ERROR (get-point))))
;	(begin
;	  (calc-matrix nil)
;	  (set-pr-scr)
;	  (buffer-changed nil))
;	(llpr "String pool full")))))

;@r_sum(a, b, ...)
;@Returns the sum of all cells in the argument list. The arguments can
;be values, references or ranges.
;@r_sum(a1..c2, e5) returns the sum of all cells from a1 to c2 plus the
;value in e5.
;@r_max, r_min, r_avg
(define (r_sum . l)
  (if (null? l)
    0
    (if (eq (car l) 'RANGE)
      (+ (area-sum (cadr l)
		   (caddr l)
		   (cadddr l)
		   (cadddr (cdr l)))
	 (apply r_sum (cddddr (cdr l))))
      (+ (car l)
	 (apply r_sum (cdr l))))))

;@r_max(a, b, ...)
;@Returns the largest value of all cells in the argument list. The arguments can
;be values, references or ranges.
;@r_max(a1..c2, e5) returns the largest of all cells from a1 to c2 plus the
;value in e5.
;@r_sum, r_min, r_avg
(define (r_max . l)
  (if (null? l)
    -2000000000
    (if (eq (car l) 'RANGE)
      (max (area-max (cadr l)
		     (caddr l)
		     (cadddr l)
		     (cadddr (cdr l)))
	   (apply r_max (cddddr (cdr l))))
      (max (if (number? (car l)) (car l) 0)
	   (apply r_max (cdr l))))))

;@r_min(a, b, ...)
;@Returns the smallest value of all cells in the argument list.
;The arguments can be values, references or ranges.
;@r_min(a1..c2, e5) returns the smallest of all cells from a1 to c2 plus the
;value in e5.
;@r_max, r_sum, r_avg
(define (r_min . l)
  (if (null? l)
    2000000000
    (if (eq (car l) 'RANGE)
      (min (area-min (cadr l)
		     (caddr l)
		     (cadddr l)
		     (cadddr (cdr l)))
	   (apply r_min (cddddr (cdr l))))
      (min (car l)
	   (apply r_min (cdr l))))))


;@r_avg(a, b, ...)
;@Returns the average of all cells in the argument list. The arguments can
;be values, references or ranges.
;@r_avg(a1..c2, e5) returns the average of all cells from a1 to c2 plus the
;value in e5.
;@r_max, r_min, r_sum
; this is cheating
(define (r_avg . l)
  (apply area-avg (cdr l)))

(define (area-sum r1 c1 r2 c2)
  (let ((sum 0)
	(r r1))
    (while (<= r r2)
      (let ((c c1)
	    (val nil))
	(while (<= c c2)
	  (set! val (get-cell r c))
	  (if (number? val)
	    (set! sum (+ sum val)))
	  (set! c (+ c 1))))
      (set! r (+ r 1)))
    sum))

(define (area-max r1 c1 r2 c2)
  (let ((mmax nil)
	(r r1))
    (while (<= r r2)
      (let ((c c1)
	    (val nil))
	(while (<= c c2)
	  (set! val (get-cell r c))
	  (if (number? val)
	    (if (or (null? mmax) (> val mmax))
	      (set! mmax val)))
	  (set! c (+ c 1))))
      (set! r (+ r 1)))
    mmax))

(define (area-min r1 c1 r2 c2)
  (let ((mmin nil)
	(r r1))
    (while (<= r r2)
      (let ((c c1)
	    (val nil))
	(while (<= c c2)
	  (set! val (get-cell r c))
	  (if (number? val)
	    (if (or (null? mmin) (< val mmin))
	      (set! mmin val)))
	  (set! c (+ c 1))))
      (set! r (+ r 1)))
    mmin))

(define (area-avg r1 c1 r2 c2)
  (let ((sum 0)
	(count 0)
	(r r1))
    (while (<= r r2)
      (let ((c c1)
	    (val nil))
	(while (<= c c2)
	  (set! val (get-cell r c))
	  (if (number? val)
	    (begin
	      (set! sum (+ sum val))
	      (set! count (+ count 1))))
	  (set! c (+ c 1))))
      (set! r (+ r 1)))
    (/ sum count)))

(define (a1-digit d)
  (substring " ABCDEFGHIJKLMNOPQRSTUVWXYZ" d (+ d 1)))

(define (a1-coord c1)
  (let ((b "") (digit 0))
    (while (> c1 0)
      (set! digit (fmod c1 26))
      (if (eq? digit 0)
	(set! digit 26))
      (set! b (string-append (a1-digit digit) b))
      (set! c1 (- c1 digit))
      (set! c1 (trunc (/ c1 26))))
    b))

(define (make-reference r1 c1)
  (if (eq? (a1-refs-get) 0)
    (string-append "R" (number->string r1) "C" (number->string c1))
    (string-append (a1-coord c1) (number->string r1))))

(define (r_fcn fcn)
  (let ((r1 (position-row (get-blku)))
	(c1 (position-col (get-blku)))
	(r2 (position-row (get-blkl)))
	(c2 (position-col (get-blkl)))
	(b nil))
    (set! b (string-append
	      "("
	      fcn
	      " "
	      (make-reference r1 c1)
	      ".."
	      (make-reference r2 c2)
	      ")"))
    (set-data nil b 0 ERROR (get-point))
    (calc-matrix nil)
    (set-pr-scr)
    (buffer-changed nil)))

(define (block-fcn fcn)
  (let ((r1 (position-row (get-blku)))
	(c1 (position-col (get-blku)))
	(r2 (position-row (get-blkl)))
	(c2 (position-col (get-blkl)))
	(b nil))
    (set! b (string-append
	      "("
	      fcn
	      " "
	      (number->string r1)
	      " "
	      (number->string c1)
	      " "
	      (number->string r2)
	      " "
	      (number->string c2)
	      ")"))
    (set-data nil b 0 ERROR (get-point))
    (calc-matrix nil)
    (set-pr-scr)
    (buffer-changed nil)))

(define (block-sum)
  (r_fcn "r_sum"))

(define (block-min)
  (r_fcn "r_min"))

(define (block-max)
  (r_fcn "r_max"))

(define (block-avg)
  (r_fcn "r_avg"))


; Load the plotting functions
; Don't load until actually needed
(define (plot . args)
  (require (string-append datadir "/siag/plot.scm"))
  (apply plot args))

(define (plot-plugin . args)
  (require (string-append datadir "/siag/plot.scm"))
  (apply plot-plugin args))

(define (splot . args)
  (require (string-append datadir "/siag/splot.scm"))
  (apply splot args))

(define (plot-wizard)
  (require (string-append datadir "/siag/plot.scm"))
  (plot-wizard2))

(define (preview)
  (let ((fn (string-append "/tmp/siagprint" (number->string (getpid)) ".ps")))
    (if (savematrix fn nil (psformat))
      (spawn (string-append viewer-command " " fn))
      (llpr "Can't make postscript"))))

(define (print)
  (let ((fn (string-append "/tmp/siagprint" (number->string (getpid)) ".ps")))
    (if (savematrix fn nil (psformat))
      (spawn (string-append lpr-command " " fn))
      (llpr "Can't make postscript"))))

(define (print-format)
  (let* ((props (form-record "Previewer" "Print Command"))
	 (viewcmd (cdr (assoc "Previewer" props)))
	 (printcmd (cdr (assoc "Print Command" props))))
    (if (not (equal? viewcmd ""))
      (set! viewer-command viewcmd))
    (if (not (equal? printcmd ""))
      (set! lpr-command printcmd))))

; To consider: make sure that top is outside the protected area
; Also: change (set-point) et al to do this
(define (protect-cells)
  (set-prot (get-point))
  (set-pr-scr))

(define (remove-protection)
  (set-prot P-MIN)
  (set-pr-scr))

(define (ask-for-color)
  (form-begin)
  (form-label "Color:")
  (form-menu "color")
  (form-properties XtNwidth 100)
  (let ((c *colors*))
    (while c
      (form-menuentry (caar c))
      (set! c (cdr c))))
  (form-newline)
  (form-okbutton "Ok")
  (form-property XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-property XtNwidth 80)
  (cdr (assoc "color" (form-end))))


; A hack to allow simple hyperlinking.
; Calls execute on the text in the next cell.
(define (hyperlink)
  (execute (get-text (row) (+ (col) 1))))

(define (search-forward)
  (require (string-append datadir "/siag/find.scm"))
  (search-forward))

(define (search-backward)
  (require (string-append datadir "/siag/find.scm"))
  (search-backward))

(define FMT_SHIFT 16)
(define FMT_MASK (ash 15 FMT_SHIFT))
(define FMT_DEFAULT (ash 0 FMT_SHIFT))
(define FMT_INVISIBLE (ash 1 FMT_SHIFT))
(define FMT_INTEGER (ash 2 FMT_SHIFT))
(define FMT_SCIENTIFIC (ash 3 FMT_SHIFT))
(define FMT_FIXED (ash 4 FMT_SHIFT))
(define FMT_DATE (ash 5 FMT_SHIFT))
(define FMT_TIME (ash 6 FMT_SHIFT))
(define FMT_TIMEDIFF (ash 7 FMT_SHIFT))
(define FMT_PERCENT (ash 8 FMT_SHIFT))
(define FMT_HEX (ash 9 FMT_SHIFT))
(define FMT_CURRENCY (ash 10 FMT_SHIFT))
(define FMT_USER1 (ash 11 FMT_SHIFT))
(define FMT_USER2 (ash 12 FMT_SHIFT))
(define FMT_USER3 (ash 13 FMT_SHIFT))
(define FMT_USER4 (ash 14 FMT_SHIFT))
(define FMT_USER5 (ash 15 FMT_SHIFT))


(define HADJ_MASK (bit-or 4096 8192))
(define HADJ_LEFT 0)
(define HADJ_CENTER 8192)
(define HADJ_RIGHT (bit-or 4096 8192))

(define (change-format buf r c name value)
  (let* ((pos (make-position r c))
	 (fmt (decode-format (get-format buf pos)))
	 (field (assoc name fmt)))
    (set-cdr! field value)
    (set-format buf pos (encode-format fmt))))

(define (line-format buf r c1 c2 name value)
;  (set! c2 (min c2 (col-last-used r)))
  (set! c2 (min c2 (max (+ (col-last-used r) 1) c1)))
  (while (<= c1 c2)
    (change-format buf r c1 name value)
    (set! c1 (+ c1 1))))

(define (area-format buf r1 c1 r2 c2 name value)
;  (set! r2 (min r2 (line-last-used)))
  (set! r2 (min r2 (max (+ (line-last-used) 1) r1)))
  (while (<= r1 r2)
    (line-format buf r1 c1 c2 name value)
    (set! r1 (+ r1 1)))
  (set-pr-scr))

; Change a single attribute
(define (new-format name value)
  (let ((r (position-row (get-point)))
	(c (position-col (get-point)))
	(r1 (position-row (get-blku)))
	(c1 (position-col (get-blku)))
	(r2 (position-row (get-blkl)))
	(c2 (position-col (get-blkl))))
    (if (and (>= r r1) (<= r r2) (>= c c1) (<= c c2))
      (area-format nil r1 c1 r2 c2 name value)
      (area-format nil r c r c name value))))

; Interactively select value from a list
(define (new-attribute name)
  (let ((value (select-attribute name)))
    (if (not (null? value))
      (new-format name value))))

; Used for bold, italic and underline.
(define (toggle-format name)
  (let* ((fmt (decode-format (get-format nil (get-point))))
	 (field (assoc name fmt))
	 (value (cdr field)))
    (new-format name (if (eq? value 0) 1 0))))

(define (edit-siod)
  (interpreter-test nil 'SIOD))

(define (edit-guile)
  (interpreter-test nil 'Guile))

(define (edit-python)
  (interpreter-test nil 'Python))

(define (edit_ruby)
  (interpreter-test nil 'Ruby))

(define (edit-tcl)
  (interpreter-test nil 'Tcl))

(define (edit-c)
  (interpreter-test nil 'C))

; This is the interpreter that is bound to the '=' key.
;(define *standard-interpreter* 'SIOD)
(define *standard-interpreter* 'C)
(define *tooltip-mode* 2)

(define (edit-expression . args)
  (if args (interpreter-test nil *standard-interpreter* (car args))
    (interpreter-test nil *standard-interpreter* nil)))

; We don't know what the user wants. Start off by editing a formula.
; If that fails, convert to label.
; 000320: this function was causing random crashes. Had to go.
;(define (edit-unknown . args)
;  (let* ((p1 (get-point))
;	 (row (position-row p1))
;	 (col (position-col p1))
;	 (b (get-text row col)))
;    (if args (interpreter-test "Edit:" *standard-interpreter* (car args))
;      (interpreter-test "Edit:" *standard-interpreter* nil))
;    (if (equal? (get-type nil row col) ERROR)
;      (set-data nil (get-text row col) (get-format nil p1) LABEL p1))))

(define (change-interpreter)
  (set! *standard-interpreter*
	(ask-for-str "New expression interpreter:" *standard-interpreter*)))

(define (add-property)
  (let ((property (form-record "Key" "Value")))
    (put-property nil
		  (cdr (assoc "Key" property))
		  (cdr (assoc "Value" property)))))

; Load 1-2-3 compatibility functions
(require (string-append datadir "/siag/123.scm"))


(define (enter-date)
  (let* ((s (ask-for-str "Date:" ""))
	 (tim (strptime s "%x"))
	 (p (get-point))
	 (r (position-row p))
	 (c (position-col p))
	 (b (number->string (mktime tim) 10)))
    (set-data nil b 0 ERROR p)
    (change-format nil r c "style" "date")
    (calc-matrix nil)
    (set-pr-scr)
    (buffer-changed nil)))

(define (enter-time)
  (let* ((s (ask-for-str "Time:" ""))
	 (tim (strptime s "%X"))
	 (p (get-point))
	 (r (position-row p))
	 (c (position-col p))
	 (ti (append (list (assq 'sec tim) (assq 'min tim) (assq 'hour tim))
		     (cdddr (localtime 0))))
	 (b (number->string (mktime ti) 10)))
    (set-data nil b 0 ERROR p)
    (change-format nil r c "style" "time")
    (calc-matrix nil)
    (set-pr-scr)
    (buffer-changed nil)))

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

; Testing text plugin
; To use, create docment with a single text plugin, then enter the
; expression (text-plugin-test) into a cell
; Includes the necessary error checking
(define (text-plugin-test)
  (let ((ph 0) (p nil) (q nil))
    (plugin-write ph "EXEC !pwd\n")	; don't forget newline
    (set! q (plugin-read ph))		; this should be 200 \.\n
    (if (null? q)
      "No response from plugin"
      (if (substring-equal? "2" q 0 1)
        (begin
          (set! p (plugin-read ph))	; this should be current working dir
  	  (if (null? p)
	    "Empty reply from plugin"
            (begin
	      (set! q (plugin-read ph))	; this should be \.
              (if (null? q)
	        "Missing end marker from plugin"
	        p))))
	"Plugin command returned error"))))

(require (string-append datadir "/siag/sort.scm"))

; load the fonts
;(require (string-append datadir "/common/fonts.scm"))

; and the styles
(require (string-append datadir "/siag/styles.scm"))

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
	 (pr (respect-prot-get buf))
	 (new nil))
    (form-begin)
    (form-label "Size:")
    (form-properties XtNwidth 80)
    (form-menu "size")
    (paper-add-size paper-sizes)
    (form-properties XtNlabel p XtNwidth 100)
    (form-newline)
    (form-check "landscape" "Landscape")
    (form-properties XtNstate o)
    (form-newline)
    (form-check "protect" "Respect protection")
    (form-properties XtNstate pr)
    (form-newline)
    (form-okbutton "Ok")
    (form-cancelbutton "Cancel")
    (set! new (form-end))
    (set! p (extract-string "size" new))
    (set! w (cadr (assoc p paper-sizes)))
    (set! h (caddr (assoc p paper-sizes)))
    (set! o (extract-number "landscape" new))
    (set! pr (extract-number "protect" new))
    (paper-set buf (list p w h o))
    (respect-prot-set "" pr)))

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

; This is used by menu.scm and docs/menus.scm
(define (add_pseudo_menu name) nil)

;@totalheight(r1, r2)
;@ Return the total height (in pixels)
; of all cells from r1 up to and including r2
;@totalheight(3, 6)
;@totalwidth
(define (totalheight r1 r2)
  (let ((r r1)
	(h 0))
    (while (<= r r2)
      (set! h (+ h (get-cell-height r)))
      (set! r (+ r 1)))
    h))

;@totalwidth(c1, c2)
;@ Return the total width (in pixels)
; of all cells from c1 up to and including c2
;@totalwidth(3, 6)
;@totalheight
(define (totalwidth c1 c2)
  (let ((c c1)
	(w 0))
    (while (<= c c2)
      (set! w (+ w (get-cell-width c)))
      (set! c (+ c 1)))
    w))

(define (try-executable dirs fn)
  (cond ((null? dirs) nil)
	((stat (string-append (car dirs) "/" fn)) t)
	(t (try-executable (cdr dirs) fn))))

(define (executable-file fn)
  (let* ((path (getenv "PATH"))
	 (dirs (strbreakup path ":")))
    (try-executable dirs fn)))

; Use xlhtml if it is available, otherwise xls2csv if *it* is available
(define (register-converters)
;  (if (executable-file "xltohtml")
;    (register-converter "MS Excel (*.xls)"
;			"*.html" "xltohtml %s > %s"
;			nil nil)
;    (if (executable-file "xls2csv")
;      (register-converter "MS Excel (*.xls)"
;		          "*.csv" "xls2csv %s > %s"
;		          nil nil)))
  (if (executable-file "ps2pdf")
    (register-converter "PDF saver (*.pdf)"
			nil nil
			"*.ps" "ps2pdf %s %s"))
  (if (executable-file "tsv2spr")
    (register-converter "Psion Spread (*.spr)"
			"*.csv" "spr2tsv < %s > %s"
			"*.csv" "tsv2spr < %s > %s"))
  (if (executable-file "tsv2dbf")
    (register-converter "Psion Data (*.dbf)"
			"*.csv" "dbf2tsv < %s > %s"
			"*.csv" "tsv2dbf < %s > %s"))
  (if (executable-file "agn2tsv")
    (register-converter "Psion Agenda (*.agn)"
			"*.csv" "agn2tsv < %s > %s"
			nil nil))
  (if (executable-file "blaha")
    (register-converter "Blaha (*.blaha)"
			"*.txt" "blaha < %s > %s"
			"*.txt" "blaha < %s > %s")))

(define (zoom-adjust)
  (set-zoom (/ (string->number (ask-for-str "Zoom %:" "130")) 100)))

;@currency_rate(from, to)
;@Fetches currency exchange rates from Yahoo over the Internet.
;@currency_rate("SEK", "FRF") returns the value in French francs
; of one Swedish krona.
;@stock_price, euro
(define (currency_rate from to)
  (stock_price (string-append from to "=X")))


; Soo... here is a load of functions that come with SIOD. There is the
; siod.html file, but that obviously doesn't go into siag-functions.html
; automatically. This duplicates a lot of information from that file.

;@abs(x)
;@Returns the absolute numerical value of x.
;@abs(-3.14) returns 3.14.
;@fabs

;@acos(x)
;@Returns the inverse cosine of x.
;@
;@cos

;@ash(value, bits)
;@Arithmetic shift of value a given number of bits to the left (positive)
;or right (negative).
;@ash(1, 2) returns 4.
;@

;@asin(x)
;@Returns the inverse sin of x.
;@
;@sin

;@atan(x)
;@Returns the inverse tangent of x.
;@
;@tan

;@atan_2(x, y)
;@Returns the inverse tangent of x/y. This is the same function as atan2,
; but avoids being interpreted as an A1 style reference.
;@
;@tan, atan

;@atan2(x, y)
;@Returns the inverse tangent of x/y.
;@
;@tan, atan, atan_2

;@base64decode(x)
;@Given a string X in base64 representation returns a string
;with bytes computed using the base64 decoding algorithm.
;See <a href="http://info.internet.isi.edu/in-notes/rfc/files/rfc1521.txt">rfc1521.txt</a>.
;@
;@base64encode

;@base64encode(x)
;@Returns a string computed using the base64 encoding algorithm.
;@
;@base64decode

;@cos(x)
;@Returns the cosine where x is in units of radians.
;@
;@sin

;@crypt(key, salt)
;@A form of string hash.
;@
;@

;@define(variable, value)
;@A special form used to assign a value to a variable: 
;
;       define(variable, value)
;
;The variable can then be used in other places in the sheet.
;@Let's say that A1 contains the value 2 and B1 contains the value 3.
;
;define(foo, a1*b1) returns 6 and also defines the variable foo.
;
;foo returns 6 after the definition above.
;@

;@exp(x)
;@Computes the exponential function of x.
;@
;@

;@fmod(x)
;@Floating point mod.
;@
;@

;@getcwd()
;@Returns the current working directory.
;@
;@

;@getenv(name)
;@Returns the value of the environment variable named, or ().
;@
;@

;@getgid()
;@Returns the group id of the process.
;@
;@

;@getpgrp()
;@Returns the process group ID of the calling process.
;@
;@

;@getpid()
;@Returns the process ID of the calling process.
;@
;@

;@getppid()
;@Returns the parent process ID of the calling process.
;@
;@

;@getuid()
;@Returns the uid of the current process.
;@
;@

;@length()
;@Returns the length of an object which may be a string (acts like strlen)
;or a list, or an array.
;@
;@

;@log(x)
;@Computes the natural logarithm of x.
;@
;@

;@max(x1, x2, ...)
;@Returns the maximum of x1, x2, etc.
;@
;@r_max

;@min(x1, x2, ...)
;@Returns the numerical minimum of its arguments.
;@
;@r_min

;@pow(x, y)
;@Computes the result of x raised to the y power.
;@
;@

;@rand(modulus)
;@Computes a random number from 0 to modulus-1. Uses C library rand.
;@
;@random

;@random(modulus)
;@Computes a random number from 0 to modulus-1. Uses C library random.
;@
;@rand

;@realtime()
;@Returns a double precision floating point value representation
;of the current realtime number of seconds. Usually precise to about
;a thousandth of a second.
;@
;@

;@sin(x)
;@Computes the sine function of the angle x in radians.
;@
;@cos

;@sqrt(x)
;@Compute the square root of x.
;@
;@pow, pow2

;@strcmp(str1, str2)
;@Returns 0 if str1 and str2 are equal, or -1 if str1 is alphabetically
;less than str2 or 1 otherwise.
;@
;@

;@strcspn(str, indicators)
;@Returns the location of the first character in str which is
;found in the indicators set, returns the length of the string if none
;found.
;@
;@

;@strspn(str, indicators)
;@Returns the location of the first character in str which is not
;found in the indicators set, returns the length of the str if none found.
;@
;@

;@substring(str, start, end)
;@Returns a new string made up of the part of str begining at start
;and terminating at end. In other words, the new string has a length
;of end - start.
;@
;@

;@sxhash(data, modulus)
;@Computes a recursive hash of the data with respect to the specified
;modulus.
;@
;@

;@tan(x)
;@Computes the tagent of the angle x specified in radians.
;@
;@atan

;@trunc(x)
;@Returns the integer portion of x.
;@
;@floor, ceil

;@gethostname()
;@Returns the configured name of the host.
;@
;@

;@inet_addr(str)
;@Converts a "x.x.x.x" dotted notation string or a byte
;array into a number.
;@
;@

;@gethostid()
;@Returns a 32 bit number.
;@
;@

;@siod()
;@Many functions are only available by using the
;<a href="scheme.html">SIOD interface</a>.
;@
;@

(require (string-append datadir "/siag/functions.scm"))

