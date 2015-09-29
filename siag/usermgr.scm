;;
;; Example of user management from Siag
;;

; Beware of useless salt
(define (passwd clear)
  (let ((salt (number->string (rand 99))))
    (crypt clear salt)))

(define (edit-password)
  (set-data nil (passwd (form-ask-for-str "Password:" "")) 0 LABEL (get-point)))

