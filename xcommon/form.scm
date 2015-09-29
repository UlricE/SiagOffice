;;
;; form.scm
;;
;; Interface and support functions for forminput.c


; (form-ask-for-str prompt value)
; Does the same as the C implementation of ask-for-str
(define (form-ask-for-str prompt value)
  (form-begin)
  (form-label prompt)
  (form-newline)
  (form-text "text")
  (form-property XtNwidth 400)
  (form-property XtNstring value)
  (form-newline)
  (form-okbutton "OK")
  (form-property XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-property XtNwidth 80)
  (cdr (assoc "text" (form-end))))

; (form-properties property value property value ...)
; Set properties for the most recently added control
(define (form-properties . plist)
  (while plist
    (form-property (car plist) (cadr plist))
    (set! plist (cddr plist))))

; A simple example
(define (form-test)
  (form-begin)
  (form-label "Enter user data")
  (form-newline)
  (form-label "First name")
  (form-properties XtNwidth 100 XtNshadowWidth 1)
  (form-text "First name")
  (form-properties XtNwidth 200 XtNstring "Ulric")
  (form-newline)
  (form-label "Last name")
  (form-properties XtNwidth 100 XtNshadowWidth 1)
  (form-text "Last name")
  (form-properties XtNwidth 200 XtNstring "Eriksson")
  (form-newline)
  (form-label "Address")
  (form-properties XtNwidth 100 XtNshadowWidth 1)
  (form-text "Address")
  (form-properties XtNwidth 200 XtNstring "Balders Hage 27")
  (form-newline)
  (form-okbutton "OK")
  (form-property XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-property XtNwidth 80)
  (form-end))

; (form-withprompt prompt value width)
; A convenience function to create a label followed by a text field
(define (form-withprompt prompt value width)
  (form-label prompt)
  (form-properties XtNshadowWidth 1)
  (form-text prompt)
  (form-properties XtNwidth width XtNstring value))

; Create input field with a label and a text field with default value
(define (input-field label w1 text w2 default)
  (form-label label)
  (form-properties XtNwidth w1)
  (form-text text)
  (form-properties XtNwidth w2
		   XtNstring (if (number? default)
				 (number->string default)
				 default))
  (form-newline))

; s is a tag
; n is an associative list: ((tag . value) (tag . value) (tag . value))
; returns value as a number, converts string if necessary
(define (extract-number s n)
  (let ((x (cdr (assoc s n))))
    (if (number? x) x (string->number x))))

(define (extract-string s n)
  (let ((x (cdr (assoc s n))))
    (if (number? x) (number->string x) x)))

; Another example
(define (form-test2)
  (form-begin)
  (form-withprompt "Förnamn" "Ulric" 100)
  (form-withprompt "Efternamn" "Eriksson" 200)
  (form-newline)
  (form-withprompt "Gatuadress" "Balders Hage" 200)
  (form-withprompt "Nummer" "27" 100)
  (form-newline)
  (form-withprompt "Postnummer" "610 71" 100)
  (form-withprompt "Postadress" "Vagnhärad" 200)
  (form-newline)
  (form-withprompt "Land" "SWEDEN" 100)
  (form-newline)
  (form-okbutton "OK")
  (form-property XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-property XtNwidth 80)
  (form-end))

; (form-record name name ...)
; Creates a form with one label and one text field per line
(define (form-record . elist)
  (form-begin)
  (while elist
    (form-label (car elist))
    (form-properties XtNwidth 120 XtNshadowWidth 1)
    (form-text (car elist))
    (form-properties XtNwidth 300)
    (form-newline)
    (set! elist (cdr elist)))
  (form-okbutton "OK")
  (form-property XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-property XtNwidth 80)
  (form-end))

; Yet another example
(define (form-test3)
  (form-record "Förnamn" "Efternamn" "Gatuadress" "Nummer" "Postnummer" "Postadress"))

(define (form-test4)
  (form-begin)
  (form-menu "Choose colour")
  (form-property XtNwidth 120)
  (form-menuentry "Red")
  (form-menuentry "Orange")
  (form-menuentry "Green")
  (form-menuentry "Cyan")
  (form-menuentry "Black")
  (form-menuentry "White")
  (form-menuentry "Yellow")
  (form-newline)
  (form-okbutton "OK")
  (form-property XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-property XtNwidth 80)
  (form-end))

(define (form-test4)
  (form-begin)
  (form-label "Enter text")
  (form-newline)
  (form-text "test4")
  (form-properties)
  (form-newline)
  (form-okbutton "OK")
  (form-property XtNwidth 80)
  (form-cancelbutton "Cancel")
  (form-property XtNwidth 80)
  (form-end))

