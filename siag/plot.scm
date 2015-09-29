;;
;; Use Gnuplot to plot diagrams
;;

; First the original plot.scm, plotting data by rows

(define (hplot-cell1 fp buf row col)
  (if (or (= (get-type buf row col) CONSTANT)
          (= (get-type buf row col) EXPRESSION))
    (writes fp (get-cell row col) "\n")
    (writes fp "\n")))

(define (hplot-cell fp buf row col rowx colx)
  (if (or (= (get-type buf row col) CONSTANT)
          (= (get-type buf row col) EXPRESSION))
    (writes fp (get-cell rowx colx) "\t" (get-cell row col) "\n")
    (writes fp "\n")))

; Check if the first row can be used for tics
; Yes, the heuristics are questionable
(define (is-htics buf row c1 c2)
  (let ((type (get-type buf row c1)))
    (and (<= c1 c2)
	 (or (= type LABEL)
	     (and (not (= type EXPRESSION))
		  (is-htics buf row (+ c1 1) c2))))))

; Check if the contents of the first column is likely to be titles
(define (is-htitles buf r1 r2 col)
  (let ((type (get-type buf r1 col)))
    (and (<= r1 r2)
	 (or (= type LABEL)
	     (and (not (= type EXPRESSION))
		  (is-htitles buf (+ r1 1) r2 col))))))

(define (hplot style ticks titles logx logy parametric width height)
  (let* ((has-tics (equal? ticks 1))
  	 (has-titles (equal? titles 1))
	 (param (equal? parametric 1))
	 (pid (number->string (getpid) 10))
    	 (r0 (position-row (get-blku)))
   	 (r1 r0)
   	 (r2 (position-row (get-blkl)))
   	 (c0 (position-col (get-blku)))
   	 (c1 c0)
   	 (c2 (position-col (get-blkl)))
	 (c 0)
	 (r 0)
	 (fn-cmd (string-append "/tmp/siagplot" pid ".cmd"))
	 (fn-output (string-append "/tmp/siagplot" pid ".ps"))
	 (fn-data "")
	 (fp-cmd (fopen fn-cmd "w"))
	 (fp-output nil)
	 (fp-data nil))
    (writes fp-cmd "# This file is used by Siag to control Gnuplot\n")
    (writes fp-cmd "set terminal postscript\n")
    (writes fp-cmd "set output \"" fn-output "\"\n")
    (writes fp-cmd "set data style " style "\n")
    (if has-titles (set! c1 (+ c1 1)))
    (if has-tics
      (begin
        (set! r1 (+ r1 1))
        (writes fp-cmd "set xtics (")
        (set! c c1)
        (while (<= c c2)
  	  (if (> c c1) (writes fp-cmd ", "))
	  (if (get-text r0 c)
	    (writes fp-cmd "\"" (get-string r0 c) "\" "))
	  (writes fp-cmd (- c c1))
	  (set! c (+ c 1)))
        (writes fp-cmd ")\n")))
    (writes fp-cmd "plot ")
    (if param
      (set! r (+ r1 1))
      (set! r r1))

    (while (<= r r2)
      (set! fn-data (string-append "/tmp/siagplot" pid "." (number->string r 10)))
      (set! fp-data (fopen fn-data "w"))
      (writes fp-cmd "\"" fn-data "\"")
      (if (and has-titles (get-text r c0))
	(writes fp-cmd " title \"" (get-string r c0) "\""))
      (if (< r r2)
	(writes fp-cmd ", "))
      (set! c c1)
      (while (<= c c2)
        (if param
	  (hplot-cell fp-data nil r c r1 c)
	  (hplot-cell1 fp-data nil r c))
	(set! c (+ c 1)))
      (fclose fp-data)
      (set! r (+ r 1)))
    (writes fp-cmd "\n")
    (fclose fp-cmd)
    (system "gnuplot " fn-cmd)
    (set! pid (spawn (string-append viewer-command " -landscape " fn-output)))))

; End of horizontal plot

; ----------------------

; And now, plotting by column

;; Modified by Rodrigo Alfonso Guzman (10-2-1999) (raguzman@impsat1.com.ar)
;; so the selection is done by columns instead of by rows.

(define (vplot-cell1 fp buf row col)
  (if (or (= (get-type buf row col) CONSTANT)
          (= (get-type buf row col) EXPRESSION))
    (writes fp (get-cell row col) "\n")
    (writes fp "\n")))

(define (vplot-cell fp buf row col rowx colx)
  (if (or (= (get-type buf row col) CONSTANT)
          (= (get-type buf row col) EXPRESSION))
    (writes fp (get-cell rowx colx) "\t" (get-cell row col) "\n" )
    (writes fp "\n")))

; Ver si la primera columna se puede usar para ticks.
; Check if the first row can be used for tics
; Yes, the heuristics are questionable
(define (is-vtics buf col r1 r2)
  (let ((type (get-type buf r1 col)))
    (and (<= r1 r2)
	 (or (= type LABEL)
	     (and (not (= type EXPRESSION))
		  (is-vtics buf col (+ r1 1) r2))))))

; ver si los contenidos de la primera fila son titulos.
; Check if the contents of the first column is likely to be titles
(define (is-vtitles buf c1 c2 row)
  (let ((type (get-type buf row c1)))
    (and (<= c1 c2)
	 (or (= type LABEL)
	     (and (not (= type EXPRESSION))
		  (is-vtitles buf (+ c1 1) c2 row))))))

(define (vplot style ticks titles logx logy parametric width height)
  (let* ((has-tics (equal? ticks 1))
  	 (has-titles (equal? titles 1))
	 (param (equal? parametric 1))
	 (pid (number->string (getpid) 10))
	 (r0 (position-row (get-blku)))
	 (r1 r0)
	 (r2 (position-row (get-blkl)))
	 (c0 (position-col (get-blku)))
	 (c1 c0)
	 (c2 (position-col (get-blkl)))
	 (c 0)
	 (r 0)
	 (fn-cmd (string-append "/tmp/siagplot" pid ".cmd"))
	 (fn-output (string-append "/tmp/siagplot" pid ".ps"))
	 (fn-data "")
	 (fp-cmd (fopen fn-cmd "w"))
	 (fp-output nil)
	 (fp-data nil))
    (writes fp-cmd "# This file is used by Siag to control Gnuplot\n")
    (writes fp-cmd "set terminal postscript\n")
    (writes fp-cmd "set output \"" fn-output "\"\n")
    (writes fp-cmd "set data style " style "\n")
    (if has-titles (set! r1 (+ r1 1)))
 
    (writes fp-cmd "plot ")

; Rodrigo A. Guzman. (raguzman@impsat1.com.ar)
; Codigo modificado para dibujar tablas del tipo x y1 y2 y3....
; el bloque seleccionado debe tener 2 o mas columnas.
; c1  es la columna que tiene los valores de X

; Modified by Ulric: use 'parametric' argument to determine this

    (if param
      (set! c (+ c1 1))    ; comienza con y1
      (set! c c1))         ; solo 1 vector.  

    (while (<= c c2)
      (set! fn-data (string-append "/tmp/siagplot" pid "." (number->string c 10)))
      (set! fp-data (fopen fn-data "w"))
      (writes fp-cmd "\"" fn-data "\"")
      (if (and has-titles (get-text r0 c))
	(writes fp-cmd " title \"" (get-string r0 c) "\""))
      (if (< c c2)
	(writes fp-cmd ", "))
      (set! r r1)
      (while (<= r r2)
	(if param
	  (vplot-cell fp-data nil r c r c1)
          (vplot-cell1 fp-data nil r c))
	(set! r (+ r 1)))
      (fclose fp-data)
      (set! c (+ c 1))) 
;-----------------------------------------------------------------

    (writes fp-cmd "\n")
    (fclose fp-cmd)
    (system "gnuplot " fn-cmd)
    (set! pid (spawn (string-append viewer-command " -landscape " fn-output)))))

; End of vplot.scm

; ----------------

; And now, glue it together

(define plot hplot)	; or vplot

; ----------------

; Plotting into a plugin

(define (plot-plugin style)
  (let* ((blku (get-blku))
	 (blkl (get-blkl))
	 (r 0)
	 (c 0)
	 (r1 (position-row blku))
	 (c1 (position-col blku))
	 (r2 (position-row blkl))
	 (c2 (position-col blkl))
	 (has-tics nil)
	 (xtics (if has-tics 1 0))
	 (has-titles nil)
	 (titles (if has-titles 1 0))
	 (horizontal 1)
	 (update 0)
  	 (pid (number->string (getpid) 10))
	 (fn (string-append "/tmp/siagplot" pid ".plot"))
	 (fp (fopen fn "w")))
    (writes fp "[Control]\n")
    (writes fp "range="
	       (number->string r1)
	       " "
	       (number->string c1)
	       " "
	       (number->string r2)
	       " "
	       (number->string c2)
	       "\n")
    (writes fp "style="
	       style
	       "\n")
    (writes fp "titles="
	       (number->string titles)
	       "\n")
    (writes fp "xtics="
	       (number->string xtics)
	       "\n")
    (writes fp "horizontal="
	       (number->string horizontal)
	       "\n")
    (writes fp "update="
	       (number->string update)
	       "\n")
    (writes fp "\n[Data]\n")
    (set! r r1)
    (while (<= r r2)
      (set! c c1)
      (while (<= c c2)
	(let ((type (get-type nil r c)))
      	  (cond
	    ((= type EXPRESSION)
	     (writes fp (number->string r)
			" "
			(number->string c)
			" "
			(number->string (get-cell r c))
			"\n"))
	    ((or (= type STRING) (= type LABEL))
	     (writes fp (number->string r)
		        " "
		        (number->string c)
		        " "
		        (get-string r c)
		        "\n"))
	    (t nil)))
	(set! c (+ c 1)))
      (set! r (+ r 1)))
    (fclose fp)
    (plugin-import fn)))


; Jetzt neu! Rather than letting the user select only style and
; orientation and then guessing everything else, guess first and
; then use a form to select the right parameters.

(define (plot-wizard3 config)
  (let* ((type (extract-string "type" config))
	 (orientation (extract-string "orientation" config))
	 (logx (extract-number "logx" config))
	 (logy (extract-number "logy" config))
	 (ticks (extract-number "ticks" config))
	 (titles (extract-number "titles" config))
	 (parametric (extract-number "parametric" config))
	 (width (extract-number "width" config))
	 (height (extract-number "height" config)))
    (if (equal? orientation "Rows")
      (hplot type ticks titles logx logy parametric width height)
      (vplot type ticks titles logx logy parametric width height))))

(define (plot-wizard2)
  (let* ((type "lines")
	 (ur (position-row (get-blku)))
	 (uc (position-col (get-blku)))
	 (lr (position-row (get-blkl)))
	 (lc (position-col (get-blkl)))
	 (nr (+ (- lr ur) 1))
	 (nc (+ (- lc uc) 1))
	 (orientation nil)
	 (logx 0)
	 (logy 0)
	 (ticks 0)
	 (titles 0)
	 (parametric 0)
	 (width 400)
	 (height 300))
    (if (> nr nc)
      (begin
        (set! orientation "Columns")
	(set! ticks (is-vtics nil uc ur lr))
	(set! titles (is-vtitles nil uc lc ur )))
      (begin
        (set! orientation "Rows")
	(set! ticks (is-htics nil ur uc lc))
	(set! titles (is-htitles nil ur lr uc))))
    (set! ticks (if ticks 1 0))
    (set! titles (if titles 1 0))
    (form-begin)
    (form-label "Type:")
    (form-properties XtNwidth 80)
    (form-menu "type")
    (form-menuentry "lines")
    (form-menuentry "points")
    (form-menuentry "linespoints")
    (form-menuentry "impulses")
    (form-menuentry "dots")
    (form-menuentry "steps")
    (form-menuentry "boxes")
    (form-properties XtNlabel "lines")
    (form-properties XtNwidth 100)
    (form-newline)
    (form-label "Orientation:")
    (form-properties XtNwidth 80)
    (form-menu "orientation")
    (form-menuentry "Columns")
    (form-menuentry "Rows")
    (form-properties XtNlabel orientation XtNwidth 100)
    (form-newline)
    (form-check "logx" "Logarithmic X axis")
    (form-properties XtNstate logx)
    (form-newline)
    (form-check "logy" "Logarithmic Y axis")
    (form-properties XtNstate logy)
    (form-newline)
    (form-check "ticks" "Has ticks")
    (form-properties XtNstate ticks)
    (form-newline)
    (form-check "titles" "Has titles")
    (form-properties XtNstate titles)
    (form-newline)
    (form-check "parametric" "Parametric")
    (form-properties XtNstate parametric)
    (form-newline)
    ; input-field is defined in siag.scm
    (input-field "Width:" 80 "width" 80 width)
    (input-field "Height:" 80 "height" 80 height)
    (form-okbutton "OK")
    (form-cancelbutton "Cancel")
;    (form-command "Help" "(do-help \"siag/gnuplot.html\")")
    (plot-wizard3 (form-end))))

