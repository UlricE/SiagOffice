;;
;; data.scm
;;
;; A package to do simple "databasing" from Siag

; A database is a rectangular area in the matrix where the field names
; are the top row of the block. All data is stored as labels.

; When working on a record, it is represented by an associative list
; of the form ((fieldname . value) (fieldname . value) ...)

; Referring to this as a database is embarassing, but it is the term
; often used in DOS/Windows applications of this nature.


; (data-fieldnames)
; -> list of strings from the first row of the block
(define (data-fieldname-list row col)
  (cond
    ((> col (position-col (get-blkl))) nil)
    (t (cons (get-string row col)
	     (data-fieldname-list row (+ col 1))))))

(define (data-fieldnames)
  (data-fieldname-list (position-row (get-blku)) (position-col (get-blku))))


; (data-record-collect row)
; -> associative list of the form ((fieldname . value) (fieldname . value)...)

(define (data-collect-fields fieldnames row col)
  (if (null? fieldnames)
    nil
    (cons (cons (car fieldnames)
		(get-string row col))
	  (data-collect-fields (cdr fieldnames) row (+ col 1)))))

(define (data-record-collect row)
  (data-collect-fields (data-fieldnames) row (position-col (get-blku))))

; (data-record-store record row)
; Writes the values of a record into a row in the table

(define (data-store-fields record fieldnames row col)
  (if fieldnames
    (begin
      (set-data nil (cdr (assoc (car fieldnames) record))
		0 LABEL (make-position row col))
      (data-store-fields record (cdr fieldnames) row (+ col 1)))))

(define (data-record-store record row)
  (data-store-fields record (data-fieldnames) row (position-col (get-blku))))

; (data-record-form record lw tw)
; Displays a form with one line per field
(define (data-record-form record lw tw)
  (form-begin)
  (while record
    (form-label (caar record))
    (form-properties XtNwidth lw)
    (form-text (caar record))
    (form-properties XtNwidth tw XtNstring (cdar record))
    (form-newline)
    (set! record (cdr record)))
  (form-okbutton "OK")
  (form-properties XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-properties XtNwidth 80)
  (form-end))

; (data-record-edit row)
; Uses a form to read, edit and write back the results
(define (data-record-edit row)
  (data-record-store
    (data-record-form
      (data-record-collect row)
      100 300)
    row))

; Edit a record, move to the next line and edit again
(define (data-entry)
  (let* ((row (position-row (get-point)))
	 (record (data-record-form
		  (data-record-collect row)
		  100 300)))
    (data-record-store record row)
    (next-line)
    (if record
      (data-entry))))

