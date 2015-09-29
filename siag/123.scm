;;
;; Various compatibility functions for the Lotus 1-2-3 .wk1 format
;;

(define (@PI) 3.14159)
(define @FALSE 0)
(define @TRUE 1)
(define (@EQ x y)
  (if (= x y) 1 0))
(define (@NE x y)
  (if (= x y) 0 1))
(define (@LE x y)
  (if (<= x y) 1 0))
(define (@GE x y)
  (if (>= x y) 1 0))
(define (@LT x y)
  (if (< x y) 1 0))
(define (@GT x y)
  (if (> x y) 1 0))
(define (@NOT x)
  (if (= x 0) 1 0))

; @OR and @AND are actually used by ci.c, not the 123 loader
(define (@OR x y)
  (if (or (not (= x 0)) (not (= y 0))) 1 0))
(define (@AND x y)
  (if (and (not (= x 0)) (not (= y 0))) 1 0))

(define (@IF c x y)
  (if (= c 0) y x))
(define (@ROUND x y)
  (let ((p (pow 10 y)))
    (/ (floor (+ (* x p) 0.5)) p)))
(define @LOG10 log_10)

; now a bit of hoopla to count the number of numbers in an area
(define (count_numbers_cell r c)
  (if (= (get-type nil r c) EXPRESSION) 1 0))

(define (count_numbers_row r c1 c2)
  (if (> c1 c2)
    0
    (+ (count_numbers_cell r c1)
       (count_numbers_row r (+ c1 1) c2))))

(define (count_numbers_area r1 c1 r2 c2)
  (if (> r1 r2)
    0
    (+ (count_numbers_row r1 c1 c2)
       (count_numbers_area (+ r1 1) c1 r2 c2))))

(define (@COUNT . l)
  (if (null? l)
    0
    (if (eq (car l) 'RANGE)
      (+ (count_numbers_area (cadr l)
			     (caddr l)
			     (cadddr l)
			     (cadddr (cdr l)))
	 (apply @COUNT (cddddr (cdr l))))
      (+ 1
	 (apply @COUNT (cdr l))))))

; Usage: (@VLOOKUP r1c1 r1c1..r1c1 1)
; => (@VLOOKUP 1 'RANGE 1 1 1 1 1)
(define (find_vert value row row2 col)
  (if (> row row2)
    nil
    (if (eq (get-cell row col) value)
      row
      (find_vert value (+ row 1) row2 col))))

(define (@VLOOKUP value dummy r1 c1 r2 c2 offset)
  (let ((row (find_vert value r1 r2 c1)))
    (if (number? row)
      (get-cell row (+ c1 offset)))))

(define (find_horiz value row col col2)
  (if (> col col2)
    nil
    (if (eq (get-cell row col) value)
      col
      (find_vert value row (+ col 1) col2))))

(define (@HLOOKUP value dummy r1 c1 r2 c2 offset)
  (let ((col (find_horiz value r1 c1 c2)))
    (if (number? col)
      (get-cell (+ r1 offset) col))))

;;;
;;; Not really for wk1, but for cross-sheet references
;;;
(define (@XVALUE s r c)
  (x-get-cell r c s))


; also do @XHLOOKUP and @XVLOOKUP


; Helper function. Called from C to read SIOD expression
(define (scm->list r c)
  (pre->post (read-from-string (get-text r c))))

