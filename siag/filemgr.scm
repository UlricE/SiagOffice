;;
;; Example code for file management from Siag
;;

(define (wipe-cells row col last)
  (cond ((<= col last)
	 (set-data nil "" 0 EMPTY (make-position row col))
	 (wipe-cells row (+ col 1) last))))

(define (wipe-rows row last)
  (cond ((<= row last)
	 (wipe-cells row (position-row P-MIN) (col-last-used row))
	 (wipe-rows (+ row 1) last))))

(define (wipe-sheet)
  (wipe-rows (position-row P-MIN) (line-last-used)))

(define (read-directory)
  (let ((ds (opendir (getcwd)))
	(fname ""))
    (wipe-sheet)
    (set-point P-MIN)
    (while (set! fname (readdir ds))
      (set-data nil fname 0 LABEL (get-point))
      (set-point-row (+ (get-point-row) 1)))
    (closedir ds)
    (set-pr-scr)))

(define (delete-file)
  (unlink (get-string (row) (col)))
  (read-directory))

(define (move-file)
  (rename (get-string (row) (col))
	  (form-ask-for-str "To:" ""))
  (read-directory))

(define (make-directory)
  (mkdir (form-ask-for-str "Name:" "")
	 (string->number "755" 8))
  (read-directory))

(define (delete-directory)
  (rmdir (get-string (row) (col)))
  (read-directory))

(define (change-directory)
  (chdir (get-string (row) (col)))
  (read-directory))

