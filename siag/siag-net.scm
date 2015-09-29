;;
;; Another simple networking example
;; This allows data exchange with external programs.
;;

(define (siag-net-get-cols a r c c2)
  (cond ((<= c c2)
	 (s-puts (string-append (get-string r c) "\n") a)
	 (siag-net-get-cols a r (+ c 1) c2))))

(define (siag-net-get-rows a r c1 r2 c2)
  (cond ((<= r r2)
	 (siag-net-get-cols a r c1 c2)
	 (siag-net-get-rows a (+ r 1) c1 r2 c2))))

;; GET r1 c1 r2 c2
;;	Returns contents of r1c1..r2c2 with one cell per line
(define (siag-net-get a r1 c1 r2 c2)
  (siag-net-get-rows a r1 c1 r2 c2))

(define (siag-net-put-cols a r c c2)
  (cond ((<= c c2)
	 (set-data nil (string-trim-right (s-gets a))
		       0
		       LABEL
		       (make-position r c))
	 (siag-net-put-cols a r (+ c 1) c2))))

(define (siag-net-put-rows a r c1 r2 c2)
  (cond ((<= r r2)
	 (siag-net-put-cols a r c1 c2)
	 (siag-net-put-rows a (+ r 1) c1 r2 c2))))

;; PUT r1 c1 r2 c2
;;	Reads one entry per line and inserts into r1c1..r2c2 as labels
(define (siag-net-put a r1 c1 r2 c2)
  (siag-net-put-rows a r1 c1 r2 c2)
  (s-force-output a))

;; EXEC command
;;	Reads one line and evaluates it
(define (siag-net-exec a cmd)
;  (eval (read-from-string cmd))
  (s-puts "OK\n" a)
  (s-force-output a))

(define (siag-net-one a)
  (let* ((line (string-upcase (string-trim (s-gets a))))
	 (llist (strbreakup line " "))
	 (another t))
    (cond
      ((null? llist)
       (s-puts "Huh?\n" a)
       (s-force-output a))
      ((equal? (car llist) "QUIT")
       (set! another nil))
      ((equal? (car llist) "PUT")
       (siag-net-put a
		     (string->number (cadr llist))
		     (string->number (caddr llist))
		     (string->number (cadddr llist))
		     (string->number (cadddr (cdr llist)))))
      ((equal? (car llist) "GET")
       (siag-net-get a
		     (string->number (cadr llist))
		     (string->number (caddr llist))
		     (string->number (cadddr llist))
		     (string->number (cadddr (cdr llist)))))
;; The Whopper Backdoor!
      ((equal? (car llist) "EXEC")
       (siag-net-exec (substring a 5 (string-length a))))
      (t
       (s-puts "Huh?\n" a)))
    (s-force-output a)
    (s-close a)
    another))


(define (siag-net port)
  (let ((s (s-open "0.0.0.0" port 1))
	(another t))
    (writes nil "*** listening ***\n")
    (while another
      (*catch 'errobj
        (writes nil "*** accepted ***\n")
        (set! another (siag-net-one (s-accept s)))))
    (s-close s)
    (writes nil "*** Done ***\n")))

(define siag_net siag-net)

(define (siag-net-put-block port)
  (let* ((host (form-ask-for-str "Host" ""))
	 (s (s-open host port 1)))
    (s-puts "put 1 1 2 2\r\n" s)
    (s-force-output s)
    (s-puts "qwe\r\n" s)
    (s-puts "rty\r\n" s)
    (s-puts "asd\r\n" s)
    (s-puts "fgh\r\n" s)
    (s-force-output s)
    (s-close s)))

;(define (siag-net-get-block port)
;  (let* ((host (form-ask-for-str "Host" ""))
;	 (s (s-open host port 1)))
;    (s-puts "put 1 1 2 2\r\n" s)
;    (s-force-output s)
;    (s-close s)))

(define (siag-net-close port)
  (let* ((host (form-ask-for-str "Host" ""))
	 (s (s-open host port 1)))
    (s-puts "quit\r\n" s)
    (s-force-output s)
    (s-close s)))

