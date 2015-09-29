; Add a few things that make Scheme a little nicer
(define (caaaar l) (caar (caar l)))
(define (cdaaar l) (cdar (caar l)))
(define (cadaar l) (cadr (caar l)))
(define (cddaar l) (cddr (caar l)))
(define (caadar l) (caar (cdar l)))
(define (cdadar l) (cdar (cdar l)))
(define (caddar l) (cadr (cdar l)))
(define (cdddar l) (cddr (cdar l)))
(define (caaadr l) (caar (cadr l)))
(define (cdaadr l) (cdar (cadr l)))
(define (cadadr l) (cadr (cadr l)))
(define (cddadr l) (cddr (cadr l)))
(define (caaddr l) (caar (cddr l)))
(define (cdaddr l) (cdar (cddr l)))
(define (cadddr l) (cadr (cddr l)))
(define (cddddr l) (cddr (cddr l)))

(define (<= x y)
  (or (< x y) (= x y)))

(define (>= x y)
  (or (> x y) (= x y)))

; hard-coded defaults
(define viewer-command "gvu")
(define lpr-command "lpr")
(define editor-command "xedplus")
(define help-command "siaghelp")
(define filer-command "xfiler")
(define terminal-command "xterm -sb -sl 500 -j -ls -fn 7x14")
(define calculator-command "xcalc")

(define homedir (string-append (getenv "HOME") "/.siag"))

(if (not (string? homedir)) (set! homedir "/tmp"))

(if (not (stat homedir)) (mkdir homedir (string->number "700" 8)))

; Obsoleted by the new applications.sh file

(define appfile (string-append homedir "/applications.sh"))

(define (save-applications)
  (let ((fp (fopen appfile "w")))
    (writes fp
	    "# Auto generated file. Do not edit.\n"
	    "viewer=\"" viewer-command "\"\n"
	    "lpr=\"" lpr-command "\"\n"
	    "editor=\"" editor-command "\"\n"
	    "help=\"" help-command "\"\n"
	    "filer=\"" filer-command "\"\n"
	    "terminal=\"" terminal-command "\"\n"
	    "calculator=\"" calculator-command "\"\n")
    (fclose fp)))

(define (load-applications)
  (let ((fp (fopen appfile "r")))
; read line by line...
    (fclose fp)))

(if (stat appfile)
  (load-applications))

;    (require appfile)
;    (save-applications))

(define (set-applications viewer lpr editor help filer term calc)
  (if (string? viewer) (set! viewer-command viewer))
  (if (string? lpr) (set! lpr-command lpr))
  (if (string? editor) (set! editor-command editor))
  (if (string? help) (set! help-command help))
  (if (string? filer) (set! filer-command filer))
  (if (string? term) (set! terminal-command term))
  (if (string? calc) (set! calculator-command calc))
  (save-applications))

(define (set-environment int)
  (let ((new nil)
	(viewer viewer-command)
	(lpr lpr-command)
	(editor editor-command)
	(help help-command)
	(filer filer-command)
	(term terminal-command)
	(calc calculator-command))
    (cond
      ((equal? int "Defaults")
       (begin
	 (set! viewer "gvu")
	 (set! lpr "lpr")
	 (set! editor "xedplus")
	 (set! help "siaghelp")
	 (set! filer "xfiler")
	 (set! term "xterm")
	 (set! calc "xcalc")))
      ((equal? int "Gnome")
       (begin
	 (set! viewer "ggv")
	 (set! editor "gedit")
	 (set! help "gzilla")
	 (set! filer "gmc")))
      ((equal? int "KDE")
       (begin
	 (set! viewer "kghostview")
	 (set! editor "kedit")
	 (set! help "kdehelp")
	 (set! term "konsole")
	 (set! calc "kcalc")))
      ((equal? int "KDE2")
       (begin
	 (set! viewer "kghostview")
	 (set! editor "kedit")
	 (set! help "konqueror")
	 (set! term "konsole")
	 (set! calc "kcalc")))
      ((equal? int "CDE")
       (begin
	 (set! editor "dtpad")
	 (set! filer "dtfile")))
      (t nil))				; keep all the defaults
    (set-applications viewer lpr editor help filer term calc)))

(define (edit-applications)
  (let ((new nil)
	(viewer viewer-command)
	(lpr lpr-command)
	(editor editor-command)
	(help help-command)
	(filer filer-command)
	(term terminal-command)
	(calc calculator-command)
	(int "Custom"))
    (form-begin)
    (input-field "Previewer" 120 "viewer" 200 viewer)
    (input-field "Printer" 120 "lpr" 200 lpr)
    (input-field "Editor" 120 "editor" 200 editor)
    (input-field "Help browser" 120 "help" 200 help)
    (input-field "File manager" 120 "filer" 200 filer)
    (input-field "Terminal" 120 "term" 200 term)
    (input-field "Calculator" 120 "calc" 200 calc)
    (form-okbutton "OK")
    (form-cancelbutton "Cancel")
    (set! new (form-end))
    (set! viewer (extract-string "viewer" new))
    (set! lpr (extract-string "lpr" new))
    (set! editor (extract-string "editor" new))
    (set! help (extract-string "help" new))
    (set! filer (extract-string "filer" new))
    (set! term (extract-string "term" new))
    (set! calc (extract-string "calc" new))
    (set-applications viewer lpr editor help filer term calc)))

(define (exec-siod)
  (execute-interpreter-command 'SIOD))

(define (exec-c)
  (execute-interpreter-command 'C))

(define (exec-guile)
  (execute-interpreter-command 'Guile))

(define (exec-python)
  (execute-interpreter-command 'Python))

(define (exec-ruby)
  (execute-interpreter-command 'Ruby))

(define (exec-tcl)
  (execute-interpreter-command 'Tcl))

(define (execute-extended-command)
  (exec-siod))

(define (llpr x)
  (puts x))

(define (do-help helpfile)
;  (putenv (string-append "SIAGHELP=" help-command))
;  (spawn (string-append "siaghelp file:" docdir "/" helpfile)))
  (spawn (string-append "siagrun help file:" docdir "/" helpfile)))

(define (do-link url)
;  (putenv (string-append "SIAGHELP=" help-command))
;  (spawn (string-append help-command " " url)))
  (spawn (string-append "siagrun help " url)))

(define (help-for-help)
  (do-help "common/siaghelp.html"))

(define (help-search)
  (do-help "common/search.html"))

(define (help-copyright)
  (do-help "COPYING"))

(define (keyboard-quit)
  (llpr "Quit"))

(define (no-op)
  (llpr "This command does nothing"))

