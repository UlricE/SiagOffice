
;; supporting routines for the animator module

;; return values (generally unused by the Scheme interface)
(define ANI_OK 0)
(define ANI_ERR 1)

;; values for ani-ctl mode
(define ANI_STOP 1)
(define ANI_NEXT 2)
(define ANI_PREVIOUS 3)
(define ANI_CONTINUE 4)
(define ANI_QUIT 5)

;; the objects
(define ANI_NONE 0)
(define ANI_LINE 1)
(define ANI_RECTANGLE 2)
(define ANI_ARC 3)
(define ANI_ELLIPSE 4)
(define ANI_PIXMAP 5)
(define ANI_STRING 6)
(define ANI_POINT 7)
(define ANI_FILLRECT 8)
(define ANI_FILLARC 9)
(define ANI_FILLELLIPSE 10)

;; their properties
(define ANI_X 1)
(define ANI_Y 2)
(define ANI_WIDTH 3)
(define ANI_HEIGHT 4)
(define ANI_VISIBLE 5)
(define ANI_COLOR 6)
(define ANI_FONT 7)
(define ANI_TEXT 8)

;; Some trickery to let ani-properties handle the timer ticks as well
(define ANI_TIME -1)

;; Set multiple properties in one call
(define (ani-properties . plist)
  (while plist
    (if (eq (car plist) ANI_TIME)
      (ani-time (cadr plist))
      (ani-property (car plist) (cadr plist)))
    (set! plist (cddr plist))))

(define (ani-linetest)
  ;; A propeller!
  (ani-object ANI_LINE)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 400 ANI_HEIGHT 400)
  (ani-time 500)
  (ani-properties ANI_X 400 ANI_WIDTH -400)
  (ani-time 1000)
  (ani-properties ANI_Y 400 ANI_HEIGHT -400)
  (ani-time 1500)
  (ani-properties ANI_X 0 ANI_WIDTH 400)
  (ani-time 2000)
  (ani-properties ANI_Y 0 ANI_HEIGHT 400)

  ;; another line
  (ani-object ANI_LINE)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 400 ANI_HEIGHT 0)
  (ani-time 250)
  (ani-properties ANI_HEIGHT 400)
  (ani-time 500)
  (ani-properties ANI_WIDTH 0)
  (ani-time 750)
  (ani-properties ANI_X 400 ANI_WIDTH -400)
  (ani-time 1000)
  (ani-properties ANI_Y 400 ANI_HEIGHT 0)
  (ani-time 1250)
  (ani-properties ANI_HEIGHT -400)
  (ani-time 1500)
  (ani-properties ANI_WIDTH 0)
  (ani-time 1750)
  (ani-properties ANI_X 0 ANI_WIDTH 400)
  (ani-time 2000)
  (ani-properties ANI_Y 0 ANI_HEIGHT 0))

(define (ani-circletest)
  ;; a circle
  (ani-object ANI_FILLELLIPSE)
  (ani-properties ANI_COLOR YELLOW)
  (ani-properties ANI_X 200 ANI_Y 200 ANI_WIDTH 0 ANI_HEIGHT 0 ANI_VISIBLE 0)
  (ani-properties ANI_VISIBLE 1)
  (ani-time 2000)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 400 ANI_HEIGHT 400 ANI_VISIBLE 0)

  ;; another circle
  (ani-object ANI_FILLELLIPSE)
  (ani-properties ANI_COLOR GREEN)
  (ani-properties ANI_X 200 ANI_Y 200 ANI_WIDTH 0 ANI_HEIGHT 0 ANI_VISIBLE 0)
  (ani-time 500)
  (ani-properties ANI_VISIBLE 1)
  (ani-time 2500)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 400 ANI_HEIGHT 400 ANI_VISIBLE 0)

  ;; another one
  (ani-object ANI_FILLELLIPSE)
  (ani-properties ANI_COLOR YELLOW)
  (ani-properties ANI_X 200 ANI_Y 200 ANI_WIDTH 0 ANI_HEIGHT 0 ANI_VISIBLE 0)
  (ani-time 1000)
  (ani-properties ANI_VISIBLE 1)
  (ani-time 3000)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 400 ANI_HEIGHT 400 ANI_VISIBLE 0)
  ;; and another one
  (ani-object ANI_FILLELLIPSE)
  (ani-properties ANI_COLOR GREEN)
  (ani-properties ANI_X 200 ANI_Y 200 ANI_WIDTH 0 ANI_HEIGHT 0 ANI_VISIBLE 0)
  (ani-time 1500)
  (ani-properties ANI_VISIBLE 1)
  (ani-time 3500)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 400 ANI_HEIGHT 400 ANI_VISIBLE 0)
  ;; last circle
  (ani-object ANI_FILLELLIPSE)
  (ani-properties ANI_COLOR YELLOW)
  (ani-properties ANI_X 200 ANI_Y 200 ANI_WIDTH 0 ANI_HEIGHT 0 ANI_VISIBLE 0)
  (ani-time 2000)
  (ani-properties ANI_VISIBLE 1)
  (ani-time 4000)
  (ani-properties ANI_X 0 ANI_Y 0 ANI_WIDTH 400 ANI_HEIGHT 400 ANI_VISIBLE 0))

(define (ani-boxtest)
  ;; Rectangle moves left to right and down, changing shape as it goes
  (ani-object ANI_FILLRECT)
  (ani-properties ANI_COLOR BLUE)
  (ani-properties ANI_X 50 ANI_Y 100 ANI_WIDTH 100 ANI_HEIGHT 50)
  (ani-time 1000)
  (ani-properties ANI_X 300 ANI_Y 200 ANI_WIDTH 50 ANI_HEIGHT 150)

  ;; Another rectangle moves to upper left corner, grows then shrinks to nothing
  (ani-object ANI_FILLRECT)
  (ani-properties ANI_COLOR RED)
  (ani-properties ANI_X 400 ANI_Y 10 ANI_WIDTH 100 ANI_HEIGHT 100 ANI_VISIBLE 0)
  (ani-time 300)
  (ani-properties ANI_VISIBLE 1)
  (ani-time 700)
  (ani-properties ANI_X 0 ANI_Y 0)
  (ani-time 1400)
  (ani-properties ANI_WIDTH 300 ANI_HEIGHT 300)
  (ani-time 2000)
  (ani-properties ANI_WIDTH 0 ANI_HEIGHT 0 ANI_VISIBLE 0))

;; A simple example
(define (ani-test)
  (ani-begin)
  (ani-geometry 600 400)
  (ani-duration 2000)
  (ani-delta 100)
  (ani-boxtest)
  (ani-end))

;; Concentric circles
(define (ani-test2)
  (ani-begin)
  (ani-duration 4000)
  (ani-delta 100)
  (ani-geometry 400 400)
  (ani-circletest)
  (ani-end))

(define (ani-test3)
  (ani-begin)
  (ani-geometry 400 400)
  (ani-duration 2000)
  (ani-delta 100)
  (ani-linetest)
  (ani-end))

;; And now, the whole ensemble
(define (ani-test4)
  (ani-begin)
  (ani-geometry 400 400)
  (ani-duration 2000)
  (ani-delta 100)
  (ani-boxtest)
  (ani-linetest)
  (ani-circletest)
  (ani-end))

(define (ani-rectangle . script)
  (ani-object ANI_RECTANGLE)
  (apply ani-properties script))

(define (ani-line . script)
  (ani-object ANI_LINE)
  (apply ani-properties script))

(define (ani-arc . script)
  (ani-object ANI_ARC)
  (apply ani-properties script))

(define (ani-ellipse . script)
  (ani-object ANI_ELLIPSE)
  (apply ani-properties script))

(define (ani-test5a x1 y1 w1 h1 t1 x2 y2 w2 h2 t2)
  (ani-ellipse
    ANI_VISIBLE 0
    ANI_TIME t1
    ANI_X x1 ANI_Y y1 ANI_WIDTH w1 ANI_HEIGHT h1 ANI_VISIBLE 1
    ANI_TIME t2
    ANI_WIDTH w2 ANI_HEIGHT h2 ANI_VISIBLE 0))

(define (ani-test5)
  (ani-begin)
  (ani-geometry 600 400)
  (ani-duration 2000)
  (ani-delta 100)
  (let ((t 10))
    (while (< t 2000)
      (ani-test5a 0 0 0 0 t 0 0 600 400 (+ t 1000))
      (set! t (+ t 300))))
  (ani-end))

