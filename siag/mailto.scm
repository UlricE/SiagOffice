;;
;; Example for mailing from Siag
;; Assumes that sendmail is running on the local host
;;

(define (mailto)
  (let ((s (s-open (gethostname) "smtp")))
    (s-puts (string-append "helo " (gethostname) "\r\n") s)
    (s-force-output s)
    (writes nil (s-gets s))
    (s-puts (string-append "mail from: "
		(cdr (assoc 'name (getpwuid (getuid))))
		"@" (gethostname) "\r\n") s)
    (s-force-output s)
    (writes nil (s-gets s))
    (s-puts (string-append "rcpt to: " (form-ask-for-str "To:" "") "\r\n") s)
    (s-force-output s)
    (writes nil (s-gets s))
    (s-puts "data\r\n" s)
    (s-force-output s)
    (writes nil (s-gets s))
    (html-table s)
    (s-puts "\r\n.\r\n" s)
    (s-force-output s)
    (writes nil (s-gets s))
    (s-puts "quit\r\n" s)
    (s-force-output s)
    (writes nil (s-gets s))
    (s-close s)))

(define (html-table-cols a row col last)
  (cond ((<= col last)
	 (s-puts (string-append "  <TD>" (get-string row col) "</TD>\n") a)
	 (html-table-cols a row (+ col 1) last))))

(define (html-table-rows a row last)
  (cond ((<= row last)
	 (s-puts "\n<TR>\n" a)
	 (html-table-cols a row 1 (col-last-used row))
	 (s-puts "</TR>\n" a)
	 (html-table-rows a (+ row 1) last))))

(define (html-table a)
  (s-puts "<HTML>\n<HEAD><TITLE>HTML Table by Siag</TITLE></HEAD>\n" a)
  (s-puts "<BODY>\n<TABLE BORDER=1>\n" a)
  (html-table-rows a 1 (line-last-used))
  (s-puts "</TABLE>\n</BODY>\n</HTML>\n" a))

