(define (make-position row col)
  (list row col))

(define (position-row p)
  (car p))

(define (position-col p)
  (cadr p))

(define (get-point-row)
  (position-row (get-point)))

(define (get-point-col)
  (position-col (get-point)))

(define (set-point-row row)
  (set-point (safe-position row (get-point-col))))

(define (set-point-col col)
  (set-point (safe-position (get-point-row) col)))

(define (set-mark-command)
  (begin
    (set-mark (get-point))
    (llpr "Mark set")))

(define (exchange-point-and-mark)
  (let ((p (get-point)))
    (set-point (get-mark))
    (set-mark p)))

(define (beginning-of-buffer)
  (set-mark (get-point))
  (set-point (find-beginning-of-buffer)))

(define (end-of-buffer)
  (set-mark (get-point))
  (set-point (find-end-of-buffer)))

(define (previous-col p)
  (safe-position (position-row p) (- (position-col p) 1)))

(define (next-col p) 
  (safe-position (position-row p) (+ (position-col p) 1)))

(define (previous-row p)
  (safe-position (- (position-row p) 1) (position-col p)))

(define (next-row p)
  (safe-position (+ (position-row p) 1) (position-col p)))

