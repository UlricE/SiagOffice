;;
;; Example for writing HTML table from Siag
;;

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

;; single-threaded http server for diagnostic purposes.

(define server-software "Siag/1.3")

(define (siag-http port)
  (let ((s (s-open "0.0.0.0" port 1)))
    (writes nil "*** listening ***\n")
    (*catch 'errobj
      (writes nil "*** accepted ***\n")
      (http-service-one (s-accept s)))
    (s-close s)))

(define siag_http siag-http)

(define (http-service-one a)
  (let ((line nil))
    (while (and (set! line (s-gets a))
		(not (or (equal? line "\r\n")
			 (equal? line "\n"))))
      (writes nil line))

    (s-puts (string-append
	     "HTTP/1.0 200 OK\r\n"
	     "Content-type: text/html\r\n"
	     "Server: " server-software "\r\n"
	     "Date: " (http-date (realtime)) "\r\n"
	     "\r\n")
	    a)
    (html-table a)
    (s-force-output a)
    (s-close a)
    (writes nil "*** Done ***\n")))

