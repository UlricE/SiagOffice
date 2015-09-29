; Search for string or value
;

(define (next-cell p)
  (let ((r (position-row p))
	(c (position-col p)))
    (cond ((< c (col-last-used r)) (make-position r (+ c 1)))
	  ((< r (line-last-used)) (make-position (+ r 1) 1))
	  (t nil))))

(define (previous-cell p)
  (let ((r (position-row p))
	(c (position-col p)))
    (cond ((> c 1) (make-position r (- c 1)))
	  ((> r 1) (make-position (- r 1) (col-last-used (- r 1))))
	  (t nil))))

; find string s starting at position p
;(define (find-string next p s)
;  (cond ((null? p) nil)
;	((equal? (get-string (position-row p) (position-col p)) s) p)
;	(t (find-string next (next p) s))))

(define (find-string next p s)
  (cond ((null? p) nil)
        ((string-search (string-upcase s)
			(string-upcase (get-string (position-row p)
						   (position-col p))))
	  p)
	(t (find-string next (next p) s))))

(define *search-string* "")

(define (search-forward)
  (let ((p (find-string next-cell
			(next-cell (get-point))
			(set! *search-string*
			  (ask-for-str "Search:" (or *search-string* ""))))))
    (if p (set-point p))))

(define (search-backward)
  (let ((p (find-string previous-cell
			(previous-cell (get-point))
			(set! *search-string*
			  (ask-for-str "Search:" (or *search-string* ""))))))
    (if p (set-point p))))

