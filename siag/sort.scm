; Sort a range
;

; swap-cells is implemented in C for efficiency

(define (swap-areas r1 c1 r2 c2 w h)
  (let ((r 0))
    (while (< r h)
      (let ((c 0))
	(while (< c w)
	  (swap-cells nil (+ r1 r) (+ c1 c) (+ r2 r) (+ c2 c))
	  (set! c (+ c 1))))
      (set! r (+ r 1))))
  (set-pr-scr)
  (buffer-changed nil))

; mode is null: sort as text
; mode is non-null: sort as numbers
; asc is null: sort in ascending order
; asc is non-null: sort in descending order

; compare-cells returns:
;	negative if (r1,c1) < (r2,c2)
;	zero if (r1,c1) == (r2,c2)
;	positive if (r1,c1) > (r2,c2)
(define (compare-cells r1 c1 r2 c2 mode)
  (if (null? mode)
    (let ((t1 (get-string r1 c1))
	  (t2 (get-string r2 c2)))
      (strcmp t1 t2))
    (let ((n1 (get-cell r1 c1))
	  (n2 (get-cell r2 c2)))
      (if (not (number? n1))
	(set! n1 0))
      (if (not (number? n2))
	(set! n2 0))
      (- n1 n2))))

; Using selection sort because swaps are expensive. This is no good
; for sorting huge ranges.
(define (sort-range-by-rows r1 c1 r2 c2 col op mode)
  (let ((i r1))
    (while (<= i r2)
      (let ((best i) (j (+ i 1)))
	(while (<= j r2)
	  (if (op (compare-cells best col j col mode) 0)
	    (set! best j))
	  (set! j (+ j 1)))
	(swap-areas i c1 best c1 (+ (- c2 c1) 1) 1))
      (set! i (+ i 1)))))

(define (sort-range-by-columns r1 c1 r2 c2 row op mode)
  (let ((i c1))
    (while (<= i c2)
      (let ((best i) (j (+ i 1)))
	(while (<= j c2)
	  (if (op (compare-cells row best row j mode) 0)
	    (set! best j))
	  (set! j (+ j 1)))
	(swap-areas r1 i r1 best 1 (+ (- r2 r1) 1)))
      (set! i (+ i 1)))))

(define (sort-range r1 c1 r2 c2 rc hor asc mode)
  (if (null? hor)
    (let* ((pc (get-point-col))	; sort by rows
	   (col (if (eq? rc 0)
		  (if (and (>= pc c1) (<= pc c2))
		    pc		; point in range: use column of point
		    c1)		; point out of range: use first column
		  rc)))		; use rc for column
      (sort-range-by-rows r1 c1 r2 c2 col (if (null? asc) > <) mode))
    (let* ((pr (get-point-row))	; sort by columns
	   (row (if (eq? rc 0)
		  (if (and (>= pr r1) (<= pr r2))
		    pr		; point in range: use row of point
		    r1)		; point out of range: use first row
		  rc)))		; use rc for row
      (sort-range-by-columns r1 c1 r2 c2 row (if (null? asc) > <)  mode))))

(define (sort-block rc hor asc mode)
  (let ((r1 (position-row (get-blku)))
	(c1 (position-col (get-blku)))
	(r2 (position-row (get-blkl)))
	(c2 (position-col (get-blkl))))
    (sort-range r1 c1 r2 c2 rc hor asc mode)))

