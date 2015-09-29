;;;
;;; And now, more functions
;;;

;XXX Stuff I need to write:
; g_date_is_leap_year
; datetime_value_to_g
; g_date_day
; g_date_month
; g_date_year
; clean (or rather tr)
; num2str that can convert to any base including 2

; Support for financial functions.
(define (calculate_pvif rate nper)
  (pow (+ 1 rate) nper))

(define (calculate_fvifa rate nper)
  (/ (- (pow (+ 1 rate) nper) 1) rate))

(define (calculate_principal starting payment rate period)
  (/ (+ (* starting
	   (pow (+ 1 rate) period))
	(* payment
	   (- (pow (+ 1 rate) period) 1)))
     rate))

(define (calculate_pmt rate nper pv fv type)
  (let ((pvif (calculate_pvif rate nper))
	(fvifa (calculate_fvifa rate nper)))
    (/ (- (* (- pv) pvif) fv)
       (* (+ 1 (* rate type)) fvifa))))

(define (annual_year_basis value_date basis)
  (cond
    ((= basis 0) 360)
    ((= basis 1) (if (g_date_is_leap_year
		       (g_date_year
			 (datetime_value_to_g value_date)))
		   366
		   365))
    ((= basis 2) 360)
    ((= basis 3) 365)
    ((= basis 4) 360)
    (t -1)))

(define (days_monthly_basis issue_date maturity_date basis)
  (let* ((date_i (datetime_value_to_g issue_date))
	 (date_m (datetime_value_to_g maturity_date))
	 (issue_day (g_date_day date_i))
	 (issue_month (g_date_month date_i))
	 (issue_year (g_date_year date_i))
	 (maturity_day (g_date_day date_m))
	 (maturity_month (g_date_month date_m))
	 (maturity_year (g_date_year date_m))
	 (years (- maturity_year issue_year))
	 (months (+ (* 12 years) (- maturity_month issue_month)))
	 (days (- maturity_day issue_day))
	 (leap_year (g_date_is_leap_year issue_year)))
    (cond
      ((= basis 0)
	(if (and (= issue_month 2)
		 (not (= maturity_month 2))
		 (= issue_year maturity_year))
	  (if leap_year (+ (* months 30) days -1)
			(+ (* months 30) days -2))
	  (+ (* months 30) days)))
      ((or (= basis 1) (= basis 2) (= basis 3))
	(- (datetime_value_to_serial maturity_date)
	   (datetime_value_to_serial issue_date)))
      ((= basis 4)
	(+ (* months 30) days))
      (t -1))))


; Two functions to deal with conversion between numbers and strings.
(define (num2str x base width)
  (if (or (= base 8) (= base 10) (= base 16))
    (number->string x base width)
    (string-append "Base " (number->string base) " not supported")))

(define (str2num x base)
  (string->number x base))


; Dealing with "variadic" functions.

; Get everything from a row subject to some condition.
(define (collect_row r c1 c2 condition)
  (if (> c1 c2)
    nil
    (let ((v (get-cell r c1)))
      (if (apply condition v)
	(cons v (collect_row r (+ c1 1) c2 condition))
	(collect_row r (+ c1 1) c2 condition)))))

; Get everything from a range subject to some condition.
; Returns a list with a single level.
(define (collect_range r1 c1 r2 c2 condition)
  (if (> r1 r2)
    nil
    (append (collect_row r1 c1 c2 condition)
	    (collect_range (+ r1 1) c1 r2 c2 condition))))

; Used to get everything from an argument list, subject to some condition.
; If the argument list contains a range, the cells in the range are returned.
; This function returns a list with a single level.
(define (collect_args args condition)
  (if (null? args)
    nil
    (if (eq? (car args) 'RANGE)
      (append (collect_range (cadr args)
			     (caddr args)
			     (car (cdddr args))
			     (cadr (cdddr args))
			     condition)
	      (collect_args (cddr (cdddr args)) condition))
      (if (apply condition (car args))
	(cons (car args) (collect_args (cdr args) condition))
	(collect_args (cdr args) condition)))))

; Compare a value to a criteria given as a string, e.g. ">=28"
(define (check_crit v c)
  (cond
    ((< (length c) 2)
     (= v (string->number c)))
    ((equal? (substring c 0 2) "<=")
     (<= v (string->number (substring c 2))))
    ((equal? (substring c 0 2) ">=")
     (>= v (string->number (substring c 2))))
    ((equal? (substring c 0 2) "<>")
     (not (= v (string->number (substring c 2)))))
    ((equal? (substring c 0 1) "<")
     (< v (string->number (substring c 1))))
    ((equal? (substring c 0 1) ">")
     (> v (string->number (substring c 1))))
    ((equal? (substring c 0 1) "=")
     (= v (string->number (substring c 1))))
    (t
     (= v (string->number c)))))


;XXX;@accrint(issue, first_interest, settlement, rate, par, frequency[,basis])
;XXX;@Accrint calculates the accrued interest for a security that pays
; periodic interest. The rate is the annual rate of the security and
; par is the par value of the security. basis is the type of day
; counting system you want to use:
;
; 0 US 30/360
;
; 1 actual days/actual days
;
; 2 actual days/360
;
; 3 actual days/365
;
; 4 European 30/360
;
; If basis is omitted, US 30/360 is applied. If issue date or
; settlement date is not valid, accrint returns error. If rate
; or par is zero or negative, accrint returns error. If basis &lt;
; 0 or basis > 4, accrint returns error. If issue date is after
; settlement date or they are the same, accrint returns error.
;XXX;@
;XXX;@accrintm
(define (accrint issue first_interest settlement rate par frequency . b)
  (let* ((basis (if b (car b) 0))
	 (a (days_monthly_basis issue settlement basis))
	 (d (annual_year_basis issue basis)))
    (if (or (< a 0) (<= d 0) (<= par 0) (<= rate 0) (< basis 0) (> basis 4) (= freq 0))
      nil
      (let ((coefficient (/ (* par rate) freq))
	    (x (/ a d)))
	(* coefficient freq x)))))

;XXX;@accrintm(issue, maturity, rate[, par, basis])
;XXX;@Accrintm calculates and returns the accrued interest for a security
; from issue to maturity date. rate is the annual rate of the
; security and par is the par value of the security. If you omit par,
; accrintm applies $1,000 instead. basis is the type of day counting
; system you want to use:
;
; 0 US 30/360
;
; 1 actual days/actual days
;
; 2 actual days/360
;
; 3 actual days/365
;
; 4 European 30/360
;
; If basis is omitted, US 30/360 is applied. If issue date or
; maturity date is not valid, accrintm returns error. If rate or
; par is zero or negative, accrintm returns error. If basis &lt; 0
; or basis > 4, accrintm returns error. If issue date is after
; maturity date or they are the same, accrintm returns error.
;XXX;@
;XXX;@accrint
(define (accrintm issue maturity rate . b)
  (let* ((par (if (car b) (car b)))
	 (basis (if (cadr b) (cadr b)))
	 (a (days_monthly_basis issue maturity basis))
	 (d (annual_year_basis issue basis)))
    (if (or (< a 0) (<= d 0) (<= par 0) (<= rate 0) (< basis 0) (> basis 4))
      nil
      (* par rate (/ a d)))))

;@address(row_num, col_num[, abs_num, a1, text])
;@Address returns a cell address as text for specified row and column
; numbers.
;
; If abs_num is 1 or omitted, address returns absolute reference. If
; abs_num is 2, address returns absolute row and relative column. If
; abs_num is 3, address returns relative row and absolute column. If
; abs_num is 4, address returns relative reference. If abs_num is
; greater than 4, address returns error.
;
; a1 is a logical value that specifies the reference style. If a1 is
; TRUE or omitted, address returns an A1-style reference, i.e. $D$4.
; Otherwise address returns an R1C1-style reference, i.e. R4C4.
;
; text specifies the name of the worksheet to be used as the external
; reference.
;
; If row_num or col_num is less than one, address returns error.
;@
;@
(define (address row col . b)
  (let* ((abs_num (if (car b) (car b) 1))
	 (atmp (if (cadr b) (cadr b) 1))
	 (a1 (not (= atmp 0)))
	 (text (if (caddr b) (caddr b) "")))
    (cond
      ((= abs_num 1)
	(if a1
	    (string-append text
			   "$" (a1-coord col)
			   "$" (number->string row))
	    (string-append text
			   "R" (number->string row)
			   "C" (number->string col))))
      ((= abs_num 2)
	(if a1
	    (string-append text
			   (a1-coord col)
			   "$" (number->string row))
	    (string-append text
			   "R" (number->string row)
			   "C[" (number->string col) "]")))
      ((= abs_num 3)
	(if a1
	    (string-append text
			   "$" (a1-coord col)
			   (number->string row))
	    (string-append text
			   "R[" (number->string row) "]"
			   "C" (number->string col))))
      ((= abs_num 4)
	(if a1
	    (string-append text
			   (a1-coord col)
			   (number->string row))
	    (string-append text
			   "R[" (number->string row) "]"
			   "C[" (number->string col) "]")))
      (t nil))))

;;;@amordegrc(cost, purchase_date, first_period, salvage, period, rate, basis)
;;;@Returns the depreciation for each accounting period. Unimplemented.
;;;@
;;;@
;;(define (amordegrc colt purchase_date first_period salvage period rate basis)
;;  "Not implemented")

;;;@amorlinc(cost, purchase_date, first_period, salvage, period, rate, basis)
;;;@Returns the depreciation for each accounting period. Unimplemented.
;;;@
;;;@
;;(define (amorlinc colt purchase_date first_period salvage period rate basis)
;;  "Not implemented")

;@and(b1, b2, ...)
;@And implements the logical and function: the result is TRUE if all of
; the expression evaluates to TRUE, otherwise it returns FALSE.
;
; b1, trough bN are expressions that should evaluate to TRUE or FALSE.
; If an integer or floating point value is provided zero is considered
; FALSE and anything else is TRUE.
;
; If the values contain strings or empty cells those values are ignored.
; If no logical values are provided, then error is returned.
; Excel compatible. The name of the function is @and,
; since and is used by Scheme.
;@and(TRUE,TRUE) equals TRUE.
;
; and(TRUE,FALSE) equals FALSE.
;
; Let us assume that A1 holds number five and A2 number one. Then
;
; and(A1>3,A2&lt;2) equals TRUE.
;@or, not

(define (u_test2) "U_test2")

;@avedev(n1, n2, ...)
;@Avedev returns the average of the absolute deviations of a data set
; from their mean. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,   17.3, 21.3, 25.9, and 40.1. Then
;
; avedev(A1..A5) equals 7.84.
;@stdev
(define (avedev . n)
  (range_avedev (collect_args n (lambda x (number? x)))))

;@average(value1, value2, ...)
;@Average computes the average of all the values and cells referenced in
; the argument list. This is equivalent to the sum of the arguments
; divided by the count of the arguments. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; average(A1..A5) equals 23.2.
;@sum count
(define (average . n)
  (let* ((p (collect_args n (lambda x (number? x))))
	 (q p)
	 (s 0))
    (while q
      (set! s (+ s (car q)))
      (set! q (cdr q)))
    (/ s (length p))))

;@averagea(number1, number2, ...)
;@Averagea returns the average of the given arguments. Numbers, text and
; logical values are included in the calculation too. If the cell
; contains text or the argument evaluates to FALSE, it is counted as
; value zero (0). If the argument evaluates to TRUE, it is counted as
; one (1). Note that empty cells are not counted. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, 17.3, "missing", 25.9, and 40.1. Then
;
; averagea(A1..A5) equals 18.94.
;@average
(define (averagea . n)
  (let* ((p (collect_args n (lambda x (not (null? x)))))
	 (q p)
	 (s 0))
    (while q
      (if (number? (car q)) (set! s (+ s (car q))))
      (set! q (cdr q)))
    (/ s (length p))))


;@besseli(x, order)
;@Besseli returns the Neumann, Weber or Bessel function. x is
; where the function is evaluated. order is the order of the bessel
; function, if non-integer it is truncated.
;
; If x or order are not numeric an error is returned. If order &lt; 0 a
; error is returned. Excel compatible.
;@besseli(0.7,3) equals 0.007367374.
;@besselj besselk bessely
(define (besseli x order)
  (if (< order 0) nil (bessel_i x order 1)))

;@besselj(x, order)
;@Besselj returns the bessel function with x is where the
; function is evaluated. order is the order of the bessel function, if
; non-integer it is truncated.
;
; If x or order are not numeric an error is returned. If order &lt; 0 a
; error is returned. Excel compatible.
;@besselj(0.89,3) equals 0.013974004.
;@besselj besselk bessely
(define (besselj x order)
  (if (< order 0) nil (jn order x)))

;@besselk(x, order)
;@Besselk returns the Neumann, Weber or Bessel function. x is
; where the function is evaluated. order is the order of the bessel
; function, if non-integer it is truncated.
;
; If x or order are not numeric an error is returned. If order &lt; 0 a
; error is returned. Excel compatible.
;@besselk(3,9) equals 397.95880.
;@besseli besselj bessely
(define (besselk x order)
  (if (< order 0) nil (bessel_k x order 1)))

;@bessely(x, order)
;@Bessely returns the Neumann, Weber or Bessel function. x is
; where the function is evaluated. order is the order of the bessel
; function, if non-integer it is truncated.
;
; If x or order are not numeric an error is returned. If order &lt; 0 a
; error is returned. Excel compatible.
;@bessely(4,2) equals 0.215903595.
;@besseli besselj besselk
(define (bessely x order)
  (if (< order 0) nil (yn order x)))

;@betadist(x, alpha, beta[, a, b])
;@Betadist returns the cumulative beta distribution. a is the
; optional lower bound of x and b is the optional upper bound of x.
; If a is not given, betadist uses 0. If b is not given, betadist uses
; 1.
;
; If x &lt; a or x > b betadist returns error. If alpha &lt;= 0 or
; beta &lt;= 0, betadist returns error. If a >= b betadist returns
; error. Excel compatible.
;@betadist(0.12,2,3) equals 0.07319808.
;@betainv
(define (betadist x alpha beta . n)
  (let* ((a (if (car n) (car n) 0))
	 (b (if (cadr n) (cadr n) 1)))
    (if (or (< x a) (> x b) (>= a b) (<= alpha 0) (<= beta 0))
      nil
      (pbeta (/ (- x a) (- b a)) alpha beta))))

;@betainv(p, alpha, beta[, a, b])
;@Betainv returns the inverse of cumulative beta distribution.
; a is the optional lower bound of x and b is the optinal upper bound
; of x. If a is not given, betainv uses 0. If b is not given, betainv
; uses 1.
;
; If p &lt; 0 or p > 1, betainv returns error. If alpha &lt;= 0 or
; beta &lt;= 0, betainv returns error. If a >= b, betainv returns
; error. Excel compatible.
;@betainv(0.45,1.6,1) equals 0.607096629.
;@betadist
(define (betainv p alpha beta . n)
  (let* ((a (if (car n) (car n) 0))
	 (b (if (cadr n) (cadr n) 1)))
    (if (or (< p 0) (> p 1) (>= a b) (<= alpha 0) (<= beta 0))
      nil
      (+ (* (- b a) (qbeta p alpha beta)) a))))

;@bin2dec(x)
;@Bin2dec converts a binary number in string or number to its
; decimal equivalent. Excel compatible.
;@bin2dec(101) equals 5.
;@dec2bin bin2oct bin2hex
(define (bin2dec x)
  (str2num x 2))

;@bin2hex(number[, places])
;@Bin2hex converts a binary number to a hexadecimal number.
; places is an optional field, specifying to zero pad to that number of
; spaces.
;
; If places is too small or negative error is returned. This
; function is Excel compatible.
;@bin2hex(100111) equals 27.
;@hex2bin bin2oct bin2dec
(define (bin2hex n . p)
  (num2str (str2num n 2) 16 (car p)))

;@bin2oct(number[, places])
;@Bin2oct converts a binary number to an octal number. places
; is an optional field, specifying to zero pad to that number of spaces.
;
; If places is too small or negative error is returned. This
; function is Excel compatible.
;@bin2oct(110111) equals 67.
;@oct2bin bin2dec bin2hex
(define (bin2oct n . p)
  (num2str (str2num n 2) 8 (car p)))

;@binomdist(n, trials, p, cumulative)
;@Binomdist returns the binomial distribution. n is the number
; of successes, trials is the total number of independent trials, p is
; the probability of success in trials, and cumulative describes
; whether to return the sum of thebinomial function from 0 to n.
;
; If n or trials are non-integer they are truncated. If n &lt; 0 or
; trials &lt; 0 binomdist returns error. If n > trials binomdist
; returns error. If p &lt; 0 or p > 1 binomdist returns
; error. Excel compatible.
;@binomdist(3,5,0.8,0) equals 0.2048.
;@poisson
(define (binomdist x r p)
  (if (or (<= (+ x r -1) 0) (< p 0) (> p 1))
    nil
    (* (combin (+ x r -1) (- r 1)) (pow p r) (pow (- 1 p) x))))

;@ceiling(x, significance)
;@Ceiling rounds x up to the nearest multiple of
; significance.
;
; If x or significance is non-numeric ceiling returns error.
; If x and significance have different signs ceiling returns
; error. Excel compatible.
;@ceiling(2.43,1) equals 3.
;
; ceiling(123.123,3) equals 126.
;@ceil
(define (ceiling number significance)
  (let* ((s (if (>= number 0) 1 -1)))
    (if (or (= significance 0) (< (/ number s) 0))
      nil
      (* (ceil (/ number significance)) s))))

;XXX;@cell()
;XXX;@Cell returns information about the formatting, location, or contents
; of a cell.
;XXX;@
;XXX;@
;XXX(define cell what-cursor-position)

;@char(x)
;@Char returns the ASCII character represented by the number x.
;@char(65) equals A.
;@code
(define (char x)
  (let ((text "x"))
    (aset text 0 x)
    text))

;@chidist(x, dof)
;@Chidist returns the one-tailed probability of the chi-squared
; distribution. dof is the number of degrees of freedom.
;
; If dof is non-integer it is truncated. If dof &lt; 1, chidist returns
; error. Excel compatible.
;@chidist(5.3,2) equals 0.070651213.
;@chiinv chitest
(define (chidist x dof)
  (if (>= dof 1)
    (- 1 (pchisq x dof))))

;@chiinv(p, dof)
;@Chiinv returns the inverse of the one-tailed probability of
; the chi-squared distribution.
;
; If p &lt; 0 or p > 1 or dof &lt; 1, chiinv returns error. This
; function is Excel compatible.
;@chiinv(0.98,7) equals 1.564293004.
;@chidist chitest
(define (chiinv p dof)
  (if (and (>= p 0) (<= p 1) (>= dof 1))
    (qchisq (- 1 p) dof)))

;XXX;@chitest(actual_range, theoretical_range)
;XXX;@Chitest returns the test for independence of chi-squared
; distribution.
;
; actual_range is a range that contains the observed data points.
; theoretical_range is a range that contains the expected values of the
; data points. Excel compatible.
;XXX;@
;XXX;@chidist chiinv
(define (chitest actual_range theoretical_range)
  "Not implemented")

;@choose(index[, value1][, value2]...)
;@Choose returns the value of index index. index is rounded to an
; integer if it is not.
;
; If index &lt; 1 or index > number of values: returns error.
;@
;@if
(define (choose index . n)
  (let ((a (length n)))
    (if (or (< index 1) (> index a))
      nil
      (nth (- index 1) n))))

;XXX;@clean(string)
;XXX;@Clean cleans the string from any non-printable characters.
;XXX;@clean("one"\(7)) equals "one".
;XXX;@
;XXX(define (clean s)
  ;XXX"This function should be written in C")

;@code(char)
;@Code returns the ASCII number for the character char.
;@code("A") equals 65.
;@char
(define (code c)
  (aref c 0))

;XXX;@column([reference])
;XXX;@The column function returns an array of the column numbers taking a
; default argument of the containing cell position.
;
; If reference is neither an array nor a reference nor a range returns
; error.
;XXX;@
;XXX;@columns row rows
;XXX(define (column reference)
  ;XXX"Not implemented. We don't handle arrays yet")

;@columns(range)
;@The columns function returns the number of columns in area or array
; reference.
;
; If reference is neither an array nor a range returns
; error.
;@
;@column row rows
(define (columns dummy r1 c1 r2 c2)
  (if (eq? dummy 'RANGE)
    (- c2 c1 -1)))

;@combin(n, k)
;@Combin computes the number of combinations.
;
; Performing this function on a non-integer or a negative number returns
; an error. Also if n is less than k returns an error. This function
; is Excel compatible.
;@combin(8,6) equals 28.
;
; combin(6,2) equals 15.
;@
(define (combin n k)
  (if (or (< k 0) (< n k))
    nil
    (if (>= n 15)
      (floor (+ (exp (- (lgamma (+ n 1))
			(lgamma (+ k 1))
			(lgamma (- n k -1)))
	        0.5)))
      (/ (fact n)
	 (fact k)
	 (fact (- n k))))))

;@complex(real, im[, suffix])
;@Complex returns a complex number of the form x + yi. real is the real
; and im is the imaginary coefficient of the complex number. suffix is
; the suffix for the imaginary coefficient. If it is omitted, complex
; uses 'i' by default.
;
; If suffix is neither 'i' nor 'j', complex returns error. This
; function is Excel compatible.
;@complex(1,-1) equals 1-i.
;@
(define (complex real im . suffix)
  (imint2ext (cons real im)))

;@concatenate(string1[, string2...])
;@Concatenate returns up appended strings.
;@concatenate("aa","bb") equals "aabb".
;@left mid right
(define concatenate string-append)

;@confidence(x, stddev, size)
;@Confidence returns the confidence interval for a mean. x is
; the significance level, stddev is the standard deviation, and size
; is the size of the sample.
;
; If size is non-integer it is truncated. If size &lt; 0, confidence
; returns error. If size is 0, confidence returns error.
; Excel compatible.
;@confidence(0.05,1,33) equals 0.341185936.
;@average
(define (confidence x stddev size)
  (if (<= size 0)
    nil
    (* (- (qnorm (/ x 2) 0 1))
       (/ stddev (sqrt size)))))

;@convert(number, from_unit, to_unit)
;@Convert returns a conversion from one measurement system to another.
; For example, you can convert a weight in pounds to a weight in grams.
; number is the value you want to convert, from_unit specifies the
; unit of the number, and to_unit is the unit for the result.
;
; from_unit and to_unit can be any of the following:
;
; Weight and mass:
;
; 'g' Gram
;
; 'sg' Slug
;
; 'lbm' Pound
;
; 'u' U (atomic mass)
;
; 'ozm' Ounce
;
; Distance:
;
; 'm' Meter
;
; 'mi' Statute mile
;
; 'Nmi' Nautical mile
;
; 'in' Inch
;
; 'ft' Foot
;
; 'yd' Yard
;
; 'ang' Angstrom
;
; 'Pica' Pica
;
; Time:
;
; 'yr' Year
;
; 'day' Day
;
; 'hr' Hour
;
; 'mn' Minute
;
; 'sec' Second
;
; Pressure:
;
; 'Pa' Pascal
;
; 'atm' Atmosphere
;
; 'mmHg' mm of Mercury
;
; Force:
;
; 'N' Newton
;
; 'dyn' Dyne
;
; 'lbf' Pound force
;
; Energy:
;
; 'J' Joule
;
; 'e' Erg
;
; 'c' Thermodynamic calorie
;
; 'cal' IT calorie
;
; 'eV' Electron volt
;
; 'HPh' Horsepower-hour
;
; 'Wh' Watt-hour
;
; 'flb' Foot-pound
;
; 'BTU' BTU
;
; Power:
;
; 'HP' Horsepower
;
; 'W' Watt
;
; Magnetism:
;
; 'T' Tesla
;
; 'ga' Gauss
;
; Temperature:
;
; 'C' Degree Celsius
;
; 'F' Degree Fahrenheit
;
; 'K' Degree Kelvin
;
; Liquid measure:
;
; 'tsp' Teaspoon
;
; 'tbs' Tablespoon
;
; 'oz' Fluid ounce
;
; 'cup' Cup
;
; 'pt' Pint
;
; 'qt' Quart
;
; 'gal' Gallon
;
; 'l' Liter
;
; For metric units any of the following prefixes can be used:
;
; 'E' exa 1E+18
;
; 'P' peta 1E+15
;
; 'T' tera 1E+12
;
; 'G' giga 1E+09
;
; 'M' mega 1E+06
;
; 'k' kilo 1E+03
;
; 'h' hecto 1E+02
;
; 'e' dekao 1E+01
;
; 'd' deci 1E-01
;
; 'c' centi 1E-02
;
; 'm' milli 1E-03
;
; 'u' micro 1E-06
;
; 'n' nano 1E-09
;
; 'p' pico 1E-12
;
; 'f' femto 1E-15
;
; 'a' atto 1E-18
;
; If from_unit and to_unit are different types, CONVERT returns
; error. Excel compatible.
;@convert(3,"lbm","g") equals 1360.7769.
;
; convert(5.8,"m","in") equals 228.3465.
;
; convert(7.9,"cal","J") equals 33.07567.
;@
; Temperature constants
(define one_C_to_K      274.15)

(define weight_units
  '(("g"	1.0)
   ("sg"	0.00006852205001)
   ("lbm"	0.002204622915)
   ("u"		6.02217e+23)
   ("ozm"	0.035273972)))

(define distance_units
  '(("m"	1.0)
   ("mi"	0.0006213711922)
   ("Nmi"	0.0005399568035)
   ("in"	39.37007874)
   ("ft"	3.280839895)
   ("yd"	1.093613298)
   ("ang"	10000000000.0)
   ("Pica"	2834.645669)))

(define time_units
  '(("yr"	1.0)
   ("day"	365.25)
   ("hr"	8766)
   ("mn"	525960)
   ("sec"	31557600)))
	
(define pressure_units
  '(("Pa"	1.0)
   ("atm"	0.9869233e-5)
   ("mmHg"	0.00750061708)))

(define force_units
  '(("N"	1.0)
   ("dyn"	100000)
   ("lbf"	0.224808924)))

(define energy_units
  '(("J"	1.0)
   ("e"		9999995.193)
   ("c"		0.239006249)
   ("cal"	0.238846191)
   ("eV"	6.2146e+18)
   ("HPh"	3.72506e-7)
   ("Wh"	0.000277778)
   ("flb"	23.73042222)
   ("BTU"	0.000947815)))

(define power_units
  '(("HP"	1.0)
   ("W"		745.701)))

(define magnetism_units
  '(("T"	1.0)
   ("ga"	10000)))

(define liquid_units
  '(("tsp"	1.0)
   ("tbs"	0.33333333333)
   ("oz"	0.166666667)
   ("cup"	0.020833333)
   ("pt"	0.010416667)
   ("qt"	0.005208333)
   ("gal"	0.001302083)
   ("l"		0.004929994)))

(define unit_prefixes
  '(("E" 	1e+18)
   ("P" 	1e+15)
   ("T" 	1e+12)
   ("G" 	1e+9)
   ("M" 	1e+6)
   ("k" 	1000)
   ("h" 	100)
   ("e" 	10)
   ("d" 	0.1)
   ("c" 	0.01)
   ("m" 	0.001)
   ("u" 	1e-6)
   ("n" 	1e-9)
   ("p" 	1e-12)
   ("f" 	1e-15)
   ("a"		1e-18)))

; The way the tables are set up, larger units have smaller values.
; For example, a gallon is more than a litre. The conversion factor
; for going from gallon to litre is 0.0049/0.0013, i.e. the "to"
; unit divided by the "from" unit.
; Metric units have an optional prefix. In this case, the larger
; prefixes have larger values. But since a cm is smaller than a m,
; we should divide the conversion factor by the prefix value.
(define (get_constant_of_unit units prefixes name)
  (let ((v (assoc name units))
	(p (assoc (left name) prefixes)))
    (if v
      (cadr v)
      (if p
	  (if (set! v (assoc (substring name 1) units))
	    (/ (cadr v) (cadr p)))))))

(define (try_convert units prefixes from to n)
  (let ((from_c (get_constant_of_unit units prefixes from))
	(to_c (get_constant_of_unit units prefixes to)))
    (if (and from_c to_c)	; gotcha
      (/ (* n to_c) from_c))))

(define (convert n from to)
  (let (v)
    (cond
      ((and (equal? from "C") (equal? to "F"))
       (+ (* 1.8 n) 32))
      ((and (equal? from "F") (equal? to "C"))
       (/ (- n 32) 1.8))
      ((and (equal? from "F") (equal? to "F"))
       n)
      ((and (equal? from "F") (equal? to "K"))
       (+ (/ (- n 32) 1.8) one_C_to_K -1))
      ((and (equal? from "K") (equal? to "F"))
       (+ (* 1.8 (- n one_C_to_K -1)) 32))
      ((and (equal? from "C") (equal? to "K"))
       (+ n one_C_to_K -1))
      ((and (equal? from "K") (equal? to "C"))
       (- n one_C_to_K -1))
      ((set! v (try_convert weight_units unit_prefixes from to n))
       v)
      ((set! v (try_convert distance_units unit_prefixes from to n))
       v)
      ((set! v (try_convert time_units nil from to n))
       v)
      ((set! v (try_convert pressure_units unit_prefixes from to n))
       v)
      ((set! v (try_convert force_units unit_prefixes from to n))
       v)
      ((set! v (try_convert energy_units unit_prefixes from to n))
       v)
      ((set! v (try_convert power_units unit_prefixes from to n))
       v)
      ((set! v (try_convert magnetism_units unit_prefixes from to n))
       v)
      ((set! v (try_convert liquid_units unit_prefixes from to n))
       v)
      (t nil))))


;XXX;@correl(array1, array2)
;XXX;@Correl returns the correllation coefficient of two data sets.
;
; Strings and empty cells are simply ignored. This function is Excel
; compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; correl(A1..A5,B1..B5) equals 0.996124788.
;XXX;@covar fisher fisherinv
;XXX(define (correl ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  ;XXX(range_correl_pop (collect_range ar1 ac1 ar2 ac2 number?)
		    ;XXX(collect_range br1 bc1 br2 bc2 number?)))

;@count(b1, b2, ...)
;@Count returns the total number of integer or floating point arguments
; passed. Empty cells do not count. Strings and labels do not count.
; Complex numbers do not count.
; Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; count(A1..A5) equals 5.
;@average
(define (count . b)
  (length (collect_args b (lambda x (number? x)))))

;@counta(b1, b2, ...)
;@Counta returns the number of arguments passed not including empty
; cells. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, "missing", "missing", 25.9, and 40.1. Then
;
; counta(A1..A5) equals 5.
;@average count dcount dcounta product sum
(define (counta . b)
  (length (collect_args b (lambda x (not (null? x))))))

;@countblank(range)
;@Countblank returns the number of blank cells in a range. This
; function is Excel compatible.
;@
;@count
(define (countblank . b)
  (length (collect_args b (lambda x (null? x)))))

;@countif(range, criteria)
;@Countif counts the number of cells in the given range that
; meet the given criteria. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 23, 27,
; 28, 33, and 39. Then
;
; countif(A1..A5,"&lt;=28") equals 3.
;
; countif(A1..A5,"&lt;28") equals 2.
;
; countif(A1..A5,"28") equals 1.
;
; countif(A1..A5,">28") equals 2.
;@count sumif
(define (countif dummy r1 c1 r2 c2 condition)
  (let* ((p (collect_range r1 c1 r2 c2 (lambda x (number? x))))
	 (n 0))
    (while p
      (if (check_crit (car p) condition)
	(set! n (+ n 1)))
      (set! p (cdr p)))
    n))

;;;@coupdaybs(settlement, maturity, frequency[, basis])
;;;@Returns the number of days from the beginning of the coupon period to
;;; the settlement date.
;;;@
;;;@
;;(define (coupdaybs settlement maturity frequence . basis)
;;  "Not implemented")

;;;@coupdays(settlement, maturity, frequency[, basis])
;;;@Returns the number of days in the coupon period of the settlement
;;; date.
;;;@
;;;@
;;(define (coupdays settlement maturity frequency . basis)
;;  "Not implemented")

;;;@coupdaysnc(settlement, maturity, frequency[, basis])
;;;@Returns the number of days from the settlement date to the next coupon
;;; date.
;;;@
;;;@
;;(define (coupdaysnc settlement maturity frequency . basis)
;;  "Not implemented")

;;;@coupncd(settlement, maturity, frequency[, basis])
;;;@Returns the coupon date following settlement.
;;;@
;;;@
;;(define (coupncd settlement maturity frequency . basis)
;;  "Not implemented")

;;;@coupnum(settlement,maturity,frequency[,basis])
;;;@Returns the numbers of coupons to be paid between the settlement and
;;; maturity dates, rounded up.
;;;@
;;;@
;;(define (coupnum settlement maturity frequency . basis)
;;  "Not implemented")

;;;@couppcd(settlement,maturity,frequency[,basis])
;;;@Returns the coupon date preceeding settlement.
;;;@
;;;@
;;(define (couppcd settlement maturity frequency . basis)
;;  "Not implemented")

;XXX;@covar(array1,array2)
;XXX;@Covar returns the covariance of two data sets.
;
; Strings and empty cells are simply ignored. This function is Excel
; compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; covar(A1..A5,B1..B5) equals 65.858.
;XXX;@CORREL , FISHER , FISHERINV
(define (covar ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2 array2)
  (range_covar (collect_range ar1 ac1 ar2 ac2 number?)
	       (collect_range br1 bc1 br2 bc2 number?)))

;@critbinom(trials,p,alpha)
;@Critbinom returns the smallest value for which thecumulative
; is greater than or equal to a given value. n is the number of trials,
; p is the probability of success in trials, and alpha is the
; criterion value.
;
; If trials is a non-integer it is truncated. If trials &lt; 0, critbinom
; returns error. If p &lt; 0 or p > 1, critbinom returns
; error. If alpha &lt; 0 or alpha > 1, critbinom returns error. This
; function is Excel compatible.
;@critbinom(10,0.5,0.75) equals 6.
;@binomdist
(define (critbinom trials p alpha)
  (if (or (< trials 0) (< p 0) (> p 1) (< alpha 0) (> alpha 1))
    nil
    (qbinom alpha trials p)))


;;;@cumipmt(rate,nper,pv,start_period,end_period,type)
;;;@Returns the cumulative interest paid on a loan between start_period
;;; and end_period.
;;;@
;;;@
;;(define (cumipmt rate nper pv start_period end_period type)
;;  "Not implemented")

;;;@cumprinc(rate,nper,pv,start_period,end_period,type)
;;;@Returns the cumulative principal paid on a loan between start_period
;;; and end_period.
;;;@
;;;@
;;(define (cumprinc rate nper pv start_period end_period type)
;;  "Not implemented")

(define (days_of_year year)
  (cond
    ((< year 1753) (if (eq? (mod year 4) 0) 366 365))
    ((eq? (mod year 400) 0) 366)
    ((eq? (mod year 100) 0) 365)
    ((eq? (mod year 4) 0) 366)
    (t 365)))

(define (days_of_month month year)
  (cond
    ((eq? month 0) 31)
    ((eq? month 1) (if (eq? (days_of_year year) 366) 29 28))
    ((eq? month 2) 31)
    ((eq? month 3) 30)
    ((eq? month 4) 31)
    ((eq? month 5) 30)
    ((eq? month 6) 31)
    ((eq? month 7) 31)
    ((eq? month 8) 30)
    ((eq? month 9) 31)
    ((eq? month 10) 30)
    ((eq? month 11) 31)
    (t nil)))

;XXX;@date(year,month,day)
;XXX;@Computes the number of days since the 1st of january of 1970 (the date
;XXX; serial number) for the given year, month and day.
;XXX;
;XXX; The day might be negative (to count backwards) and it is relative to
;XXX; the previous month. The years should be at least 1900.
;XXX;@
;XXX;@today, now
;XXX(define (date year month day)
;XXX  "not implemented")

;@datevalue(date_str)
;@Datevalue returns the serial number of the date. date_str is the
; string that contains the date. For example, datevalue("1/1/1999")
; equals 36160.
;@
;@date
(define (datevalue date_str)
  (let ((a (strptime date_str "%D")))
    (if (null? a)
      nil
      (date (cdr (assq 'year a))
	    (cdr (assq 'mon a))
	    (cdr (assq 'mday a))))))


; Collecting databases
; This is almost equivalent to collecting ranges; in fact I'll use
; collect_range with a range only 1 column wide. The trick is to know
; which column.
; Then we use check_crit to filter out the cells we want.
; This is trickier than I thought. Each record (row) must be checked
; *in its entirety* against the criteria. Only if the whole record matches
; can we collect the specified field from the record. In other words,
; we can't use collect_range to get the fields.

; Check one record against one row of criteria. Return non-nil if
; *all* of the non-empty columns match.
; dr1 = header row from database
; dr2 = row of data in database being checked
; dc1, dc2 = first and last column in database
; cr1 = header rom from criteria
; cr2 = row in criteria being checked
; cc1 = first column in criteria
; cc2 = last column in criteria
; v = value from database
; r1 = first row of criteria
; r2 = last row of criteria
; c = column in criteria table
(define (database_match_row dr1 dc1 dr2 dc2 cr1 cc1 cr2 cc2)
  (or (> cc1 cc2)
      (and (let* ((f (get-text cr1 cc1))
		  (c (database_field dr1 dc1 dc2 f))
		  (v (get-cell dr2 c))
		  (crit (get-text cr2 cc1)))
	     (or (equal? crit "")
		 (check_crit v (get-text cr2 cc1))))
	   (database_match_row dr1 dc1 dr2 dc2 cr1 (+ cc1 1) cr2 cc2))))

;(define (database_match_row v r1 r2 c)
;  (or (> r1 r2)
;      (and (let* ((f (get-text cr1 c))
;		  (c (database_field ...
;      (and (check_crit v (get-text r1 c))
;	   (database_match_row v (+ r1 1) r2 c))))


; Check one record against the criteria.
; dr1 = the first row of the database (contains labels)
; dr = the row we are checking
; dc1, dc2 = the first and last column of the database
; cr1, cc1, cr2, cc2 = the criteria coordinates
; Returns non-nil if *any* of the rows match, nil otherwise
(define (database_match dr1 dr dc1 dc2 cr1 cc1 cr2 cc2)
  (and (< cr1 cr2)
       (or (database_match_row dr1 dc1 dr dc2 cr1 cc1 cr2 cc2)
	   (database_match dr1 dr dc1 dc2 cr1 cc1 (- cr2 1) cc2))))

(define (database_field r c1 c2 field)
  (cond
    ((> c1 c2) nil)
    ((number? field) (+ c1 field -1))
    ((not (string? field)) nil)
    ((equal? field (get-text r c1)) c1)
    (t (database_field r (+ c1 1) c2 field))))

(define (collect_database r1 c1 r2 c2 field cr1 cc1 cr2 cc2)
  (let ((c (database_field r1 c1 c2 field))
	(r r2)
	p)
    (while (> r r1)
      (if (database_match r1 r c1 c2 cr1 cc1 cr2 cc2)
	(set! p (cons (get-cell r c) p)))
      (set! r (- r 1)))
    p))

;@daverage(database,field,criteria)
;@Daverage returns the average of the values in a list or
; database that match conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; daverage(A1..C7, "Salary", A9..A11) equals 42296.3333.
;
; daverage(A1..C7, "Age", A9..A11) equals 39.
;
; daverage(A1..C7, "Salary", A9..B11) equals 40782.5.
;
; daverage(A1..C7, "Age", A9..B11) equals 36.
;@dcount
(define (daverage dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (apply averagea (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@day(serial_number)
;@Converts a serial number to a day.
;@
;@MONTH , TIME , NOW , YEAR
(define (day serial_number)
  (cdr (assoc 'mday (localtime serial_number))))

;XXX;@days360(date1,date2,method)
;XXX;@Returns the number of days from date1 to date2 following a 360-day
;XXX; calendar in which all months are assumed to have 30 days.
;XXX;
;XXX; If method is true, the European method will be used. In this case, if
;XXX; the day of the month is 31 it will be considered as 30.
;XXX;
;XXX; If method is false or omitted, the US method will be used. This is a
;XXX; somewhat complicated industry standard method.
;XXX;@
;XXX;@MONTH , TIME , NOW , YEAR
;XXX(define (days_360 date1 date2 method)
  ;XXX"Not implemented")

;XXX;@db(cost,salvage,life,period[,month])
;XXX;@DB calculates the depreciation of an asset for a given period using
;XXX; the fixed-declining balance method. cost is the initial value of the
;XXX; asset. salvage is the value after the depreciation. life is the
;XXX; number of periods overall. period is the period for which you want
;XXX; the depreciation to be calculated. month is the number of months in
;XXX; the first year of depreciation. If month is omitted, it is assumed to
;XXX; be 12.
;XXX;@
;XXX;@DDB , SLN , SYD
;XXX(define (db cost salvage life period . b)
  ;XXX"Not implemented")

(define (utest_2) "Ulric fibbar mera!")

;@dcount(database,field,criteria)
;@Dcount counts the cells that contain numbers in a database
; that match conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dcount(A1..C7, "Salary", A9..A11) equals 3.
;
; dcount(A1..C7, "Salary", A9..B11) equals 2.
;
; dcount(A1..C7, "Name", A9..B11) equals 0.
;@daverage
(define (dcount dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (length (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@dcounta(database,field,criteria)
;@Dcounta counts the cells that contain data in a database that
; match conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dcounta(A1..C7, "Salary", A9..A11) equals 3.
;
; dcounta(A1..C7, "Salary", A9..B11) equals 2.
;
; dcounta(A1..C7, "Name", A9..B11) equals 2.
;@dcount
(define (dcounta dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (length (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;XXX;@ddb(cost,salvage,life,period[,factor])
;XXX;@Ddb returns the depreciation of an asset for a given period using the
;XXX; double-declining balance method or some other similar method you
;XXX; specify. cost is the initial value of the asset, salvage is the
;XXX; value after the last period, life is the number of periods, period
;XXX; is the period for which you want the depreciation to be calculated,
;XXX; and factor is the factor at which the balance declines. If factor is
;XXX; omitted, it is assumed to be two (double-declining balance method).
;XXX;@
;XXX;@SLN , SYD
;XXX(define (ddb cost salvage life period . b)
  ;XXX"Not implemented")

;@dec2bin(number[,places])
;@Dec2bin converts a decimal number to a binary number. places
; is an optional field, specifying to zero pad to that number of spaces.
;
; If places is too small or negative error is returned. This
; function is Excel compatible.
;@dec2bin(42) equals 101010.
;@BIN2DEC , DEC2OCT , DEC2HEX
(define (dec2bin n . p)
  (num2str n 2 (car p)))

;@dec2hex(number[,places])
;@Dec2hex converts a decimal number to a hexadecimal number.
; places is an optional field, specifying to zero pad to that number of
; spaces.
;
; If places is too small or negative error is returned. This
; function is Excel compatible.
;@dec2hex(42) equals 2A.
;@HEX2DEC , DEC2BIN , DEC2OCT
(define (dec2hex n . p)
  (num2str n 16 (car p)))

;@dec2oct(number[,places])
;@Dec2oct converts a decimal number to an octal number. places
; is an optional field, specifying to zero pad to that number of spaces.
;
; If places is too small or negative error is returned. This
; function is Excel compatible.
;@dec2oct(42) equals 52.
;@OCT2DEC , DEC2BIN , DEC2HEX
(define (dec2oct n . p)
  (num2str n 8 (car p)))

;@degrees(x)
;@Degrees computes the number of degrees equivalent to x radians. This
; function is Excel compatible.
;@degrees(2.5) equals 143.2394.
;@RADIANS , PI
(define (degrees x)
  (* 180 (/ x *pi*)))

;@delta(x[,y])
;@Delta tests for numerical equivilance of two arguments
; returning 1 in equality y is optional, and defaults to 0.
;
; If either argument is non-numeric returns a error. This
; function is Excel compatible.
;@delta(42.99,43) equals 0.
;@EXACT , GESTEP
(define (delta x . b)
  (let ((y (if (car b) (car b) 0)))
    (if (= x y) 1 0)))

;@devsq(n1, n2, ...)
;@Devsq returns the sum of squares of deviations of a data set from the
; sample mean.
;
; Strings and empty cells are simply ignored. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; devsq(A1..A5) equals 470.56.
;@stdev
(define (devsq . b)
  (range_devsq (collect_args b (lambda x (number? x)))))

;@dget(database,field,criteria)
;@Dget returns a single value from a column that match
; conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dget(A1..C7, "Salary", A9..A10) equals 34323.
;
; dget(A1..C7, "Name", A9..A10) equals "Clark".
;
; If none of the items match the conditions, dget returns error.
; If more than one items match the conditions, dget returns error.
;@dcount
(define (dget dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (car (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;XXX;@disc(settlement,maturity,par,redemption[,basis])
;XXX;@Disc calculates and returns the discount rate for a sequrity. basis
; is the type of day counting system you want to use:
;
; 0 US 30/360
;
; 1 actual days/actual days
;
; 2 actual days/360
;
; 3 actual days/365
;
; 4 European 30/360
;
; If basis is omitted, US 30/360 is applied. If settlement date or
; maturity date is not valid, disc returns error. If basis &lt; 0
; or basis > 4, disc returns error. If settlement date is after
; maturity date or they are the same, disc returns error.
;XXX;@
;XXX;@
(define (disc settlement maturity par redemption . n)
  (let* ((basis (if (car n) (car n) 0))
	 (b (annual_year_basis disc basis))
	 (dsm (days_monthly_basis disc settlement basis)))
    (if (or (<= dsm 0) (<= b 0) (<= dsm 0) (< basis 0) (> basis 4) (= redemption 0))
      nil
      (* (/ (- redemption par) redemption) (/ b dsm)))))

;@dmax(database,field,criteria)
;@Dmax returns the largest number in a column that match
; conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dmax(A1..C7, "Salary", A9..A11) equals 47242.
;
; dmax(A1..C7, "Age", A9..A11) equals 45.
;
; dmax(A1..C7, "Age", A9..B11) equals 43.
;@dmin
(define (dmax dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (apply max (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@dmin(database,field,criteria)
;@Dmin returns the smallest number in a column that match
; conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dmin(A1..C7, "Salary", A9..B11) equals 34323.
;
; dmin(A1..C7, "Age", A9..B11) equals 29.
;@dmax
(define (dmin dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (apply min (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@dollar(num[,decimals])
;@Dollar returns num formatted as currency.
;@dollar(12345) equals "$12,345.00".
;@fixed text value
(define (dollar num . b)
  (let ((decimals (if (car b) (car b) 2)))
    (string-append "$" (number->string num))))

;XXX;@dollarde(fractional_dollar,fraction)
;XXX;@Dollarde converts a dollar price expressed as a fraction into a dollar
;XXX; price expressed as a decimal number.
;XXX;
;XXX; If fraction is non-integer it is truncated. If fraction &lt;= 0,
;XXX; dollarde returns error.
;XXX;@
;XXX;@dollarfr
;XXX(define (dollarde fractional_dollar fraction)
  ;XXX"Not implemented. What is this supposed to do?")

;XXX;@dollarfr(decimal_dollar,fraction)
;XXX;@Dollarfr converts a decimal dollar price into a dollar price expressed
;XXX; as a fraction.
;XXX;
;XXX; If fraction is non-integer it is truncated. If fraction &lt;= 0,
;XXX; dollarfr returns error.
;XXX;@
;XXX;@dollarde
;XXX(define (dollarfr decimal_dollar fraction)
  ;XXX"Not implemented. What is this supposed to do?")

;@dproduct(database,field,criteria)
;@Dproduct returns the product of numbers in a column that
; match conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dproduct(A1..C7, "Age", A9..B11) equals 1247.
;@dsum
(define (dproduct dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (apply product (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@dstdev(database,field,criteria)
;@Dstdev returns the estimate of the standard deviation of a
; population based on a sample. The populations consists of numbers that
; match conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dstdev(A1..C7, "Age", A9..B11) equals 9.89949.
;
; dstdev(A1..C7, "Salary", A9..B11) equals 9135.112506.
;@dstdevp
(define (dstdev dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (range_stddev_est (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@dstdevp(database,field,criteria)
;@Dstdevp returns the standard deviation of a population based
; on the entire populations. The populations consists of numbers that
; match conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dstdevp(A1..C7, "Age", A9..B11) equals 7.
;
; dstdevp(A1..C7, "Salary", A9..B11) equals 6459.5.
;@dstdev
(define (dstdevp dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (range_stddev_pop (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@dsum(database,field,criteria)
;@Dsum returns the sum of numbers in a column that match
; conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dsum(A1..C7, "Age", A9..B11) equals 72.
;
; dsum(A1..C7, "Salary", A9..B11) equals 81565.
;@dproduct
(define (dsum dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (apply sum (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@duration(rate,pv,fv)
;@Duration calculates number of periods needed for an investment to
; attain a desired value. This function is similar to fv and pv with a
; difference that we do not need give the direction of cash flows e.g.
; -100 for a cash outflow and +100 for a cash inflow.
;@
;@PPMT , PV , FV
(define (duration rate pv fv)
  (if (or (<= rate 0) (= fv 0) (= pv 0) (< (/ fv pv) 0))
    nil
    (/ (log (/ fv pv)) (log (+ 1 rate)))))

;@dvar(database,field,criteria)
;@Dvar returns the estimate of variance of a population based
; on a sample. The populations consists of numbers that match conditions
; specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dvar(A1..C7, "Age", A9..B11) equals 98.
;
; dvar(A1..C7, "Salary", A9..B11) equals 83450280.5.
;@dvarp
(define (dvar dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (range_var (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;@dvarp(database,field,criteria)
;@Dvarp returns the variance of a population based on the
; entire populations. The populations consists of numbers that match
; conditions specified.
;
; database is a range of cells in which rows of related information are
; records and columns of data are fields. The first row of a database
; contains labels for each column.
;
; field specifies which column is used in the function. If field is an
; integer, i.e. 2, the second column is used. Field can also be the
; label of a column. For example, ``Age'' refers to the column with the
; label ``Age'' in database range.
;
; criteria is the range of cells which contains the specified
; conditions. The first row of a criteria should contain the labels of
; the fields for which the criterias are for. Cells below the labels
; specify conditions, for example, ``>3'' or ``&lt;9''. Equality condition
; can be given simply by specifing a value, e.g. ``3'' or ``John''. Each
; row in criteria specifies a separate condition, i.e. if a row in
; database matches with one of the rows in criteria then that row is
; counted in (technically speaking boolean OR between the rows in
; criteria). If criteria specifies more than one columns then each of
; the conditions in these columns should be true that the row in
; database matches (again technically speaking boolean AND between the
; columns in each row in criteria).
;@Let us assume that the range A1..C7 contain the following values:
;
; Name Age Salary
;
; John 34 54342
;
; Bill 35 22343
;
; Clark 29 34323
;
; Bob 43 47242
;
; Susan 37 42932
;
; Jill 45 45324
;
; In addition, the cells A9..B11 contain the following values:
;
; Age Salary
;
; &lt;30
;
; >40 >46000
;
; dvarp(A1..C7, "Age", A9..B11) equals 49.
;
; dvarp(A1..C7, "Salary", A9..B11) equals 41725140.25.
;@dvar
(define (dvarp dummy r1 c1 r2 c2 field bd br1 bc1 br2 bc2)
  (range_varp (collect_database r1 c1 r2 c2 field br1 bc1 br2 bc2)))

;XXX;@edate(date,months)
;XXX;@Edate returns the serial number of the date that is the specified
;XXX; number of months before or after a given date. date is the serial
;XXX; number of the initial date and months is the number of months before
;XXX; (negative number) or after (positive number) the initial date.
;XXX;
;XXX; If months is not an integer, it is truncated.
;XXX;@
;XXX;@date
;XXX(define (edate date months)
  ;XXX"Not implemented")

;@effect(r,nper)
;@Effect calculates the effective interest rate from a given nominal
; rate.
;
; Effective interest rate is calculated using this formulae:
;
; r( 1 + ------ ) ^ nper - 1 nper
;
; where:
;
; r = nominal interest rate (stated in yearly terms)
;
; nper = number of periods used for compounding
;@For example credit cards will list an APR (annual percentage rate)
; which is a nominal interest rate.
;
; For example if you wanted to find out how much you are actually paying
; interest on your credit card that states an APR of 19% that is
; compounded monthly you would type in:
;
; effect(.19,12) and you would get .2075 or 20.75%. That is the
; effective percentage you will pay on your loan.
;@nominal
(define (effect rate nper)
  (if (or (< rate 0) (<= nper 0))
    nil
    (- (pow (+ 1 (/ rate nper)) nper) 1)))

;XXX;@eomonth(start_date,months)
;XXX;@Returns the last day of the month which is months from the
;XXX; start_date.
;XXX;
;XXX; Returns error if start_date or months are invalid.
;XXX;@
;XXX;@month
;XXX(define (eomonth date . b)
  ;XXX(let ((month (if (car b) (car b) 0)))
    ;XXX"Not implemented"))

;XXX;@error(text)
;XXX;@ERROR return the specified error
;XXX;@
;XXX;@iserror
; Scheme uses error

;XXX;@error.type()
;XXX;@ERROR.TYPE returns an error number corresponding to the given error
;XXX; value. The error numbers for error values are
;XXX;
;XXX; #DIV/0! 2
;XXX;
;XXX; #VALUE! 3
;XXX;
;XXX; #REF! 4
;XXX;
;XXX; #NAME! 5
;XXX;
;XXX; #NUM! 6
;XXX;
;XXX; #NA! 7
;XXX;
;XXX; Excel compatible.
;XXX;@ERROR.TYPE(NA()) equals 7.
;XXX;@iserror
;XXX(define (error_type n)
  ;XXX"Not implemented")

;@euro(currency)
;@Euro converts one Euro to a given national currency in the European
; monetary union. currency is one of the following:
;
; ATS (Austria)
;
; BEF (Belgium)
;
; DEM (Germany)
;
; ESP (Spain)
;
; FIM (Finland)
;
; FRF (France)
;
; IEP (Ireland)
;
; ITL (Italy)
;
; LUF (Luxemburg)
;
; NLG (Netherlands)
;
; PTE (Portugal)
;
; If the given currency is other than one of the above, EURO returns
; error.
;@euro("DEM") returns 1.95583.
;@currency_rate
(define euro_table
 '(("ATS"	13.7603)
   ("BEF"	40.3399)
   ("DEM"	1.95583)
   ("ESP"	166.386)
   ("FIM"	5.94573)
   ("FRF"	6.55957)
   ("IEP"	0.787564)
   ("ITL"	1936.27)
   ("LUX"	40.3399)
   ("NLG"	2.20371)
   ("PTE"	200.482)))

(define (euro c)
  (let ((a (assoc c euro_table)))
    (if a (cadr a) nil)))

;@even(number)
;@Even returns the number rounded up to the nearest even
; integer. Excel compatible.
;@even(5.4) equals 6.
;@odd
(define (even n)
  (let* ((sign (if (< n 0) -1 1))
	 (number (if (< n 0) (- n) n))
	 (ceiled (ceil number)))
    (if (= (fmod ceiled 2) 0)
      (if (> number ceiled)
	(* sign (+ ceiled 2))
	(* sign ceiled))
      (* sign (+ ceiled 1)))))

;@exact(string1, string2)
;@Exact returns true if string1 is exactly equal to string2 (this
; routine is case sensitive).
;@exact("key","key") equals TRUE.
;@len
(define (exact string1 string2)
  (if (equal? string1 string2) 1 0))

;@expondist(x,y,cumulative)
;@Expondist returns the exponential distribution. If the
; cumulative boolean is false it will return: y * exp (-y*x),
; otherwise it will return 1 - exp (-y*x).
;
; If x &lt; 0 or y &lt;= 0 this will return an error. This function is Excel
; compatible.
;@expondist(2,4,0) equals 0.001341851.
;@poisson
(define (expondist x y cuml)
  (if (or (< x 0) (<= y 0))
    nil
    (if (= cuml 0)
      (dexp x (/ 1 y))
      (pexp x (/ 1 y)))))

;XXX;@expression(cell)
;XXX;@Expression returns expression in cell as a string, orempty if the
;XXX; cell is not an expression.
;XXX;@in A1 expression(A2) equals 'expression(A3)'.
;XXX;
;XXX; in A2 expression(A3) equals empty.
;XXX;@text
;XXX(define (expression cell)
  ;XXX"Not implemented: references are not passed that way. Use get-cell")

;@fact(x)
;@Fact computes the factorial of x. ie, x! This function is Excel
; compatible.
;@fact(3) equals 6.
;
; fact(9) equals 362880.
;@
(define (fact x)
  (if (= x 1) 1 (* x (fact (- x 1)))))

;@factdouble(number)
;@FACTDOUBLE returns the double factorial of a number.
;
; If number is not an integer, it is truncated. If number is negative
; FACTDOUBLE returns error. Excel compatible.
;@FACTDOUBLE(5) equals 15.
;@fact
(define (factdouble x)
  (if (<= x 1) 1 (* x (factdouble (- x 2)))))

;@fdist(x, dof1, dof2)
;@FDIST returns the F probability distribution. dof1 is the
; numerator degrees of freedom and dof2 is the denominator degrees of
; freedom.
;
; If x &lt; 0 FDIST returns error. If dof1 &lt; 1 or dof2 &lt; 1, FDIST
; returns error. Excel compatible.
;@FDIST(2,5,5) equals 0.232511319.
;@finv
(define (fdist x dof1 dof2)
  (if (or (< x 0) (< dof1 1) (< dof2 1))
    nil
    (- 1 (pf x dof1 dof2))))

;XXX;@find(string1, string2[, start])
;XXX;@FIND returns position of string1 in string2 (case-sesitive),
;XXX; searching only from character start onwards (assumed 1 if omitted).
;XXX;@FIND("ac","Jack") equals 2.
;XXX;@exact len mid search
;XXX(define (find string1 string2 . b)
  ;XXX"Not implemented: oboe")

;@finv(p, dof1, dof2)
;@FINV returns the inverse of the F probability distribution.
;
; If p &lt; 0 or p > 1 FINV returns error. If dof1 &lt; 1 or dof2 &lt;
; 1 FINV returns error. Excel compatible.
;@FINV(0.2,2,4) equals 2.472135955.
;@fdist
(define (finv p dof1 dof2)
  (if (or (< p 0) (> p 1) (< dof1 1) (< dof2 1))
    nil
    (qf (- 1 p) dof1 dof2)))

;@fisher(x)
;@FISHER returns the Fisher transformation at x.
;
; If x is not-number FISHER returns error. If x &lt;= -1 or x >=
; 1 FISHER returns error. Excel compatible.
;@FISHER(0.332) equals 0.345074339.
;@skew
(define (fisher x)
  (if (or (<= x -1) (>= x 1))
    nil
    (* 0.5 (log (/ (+ 1 x) (- 1 x))))))

;@fisherinv(x)
;@FISHERINV returns the inverse of the Fisher transformation at
; x.
;
; If x is non-number FISHERINV returns error. This function is
; Excel compatible.
;@FISHERINV(2) equals 0.96402758.
;@fisher
(define (fisherinv x)
  (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1)))

;@fixed(num,[decimals, no_commas])
;@FIXED returns num as a formatted string with decimals numbers after
; the decimal point, omitting commas if requested by no_commas.
;@FIXED(1234.567,2) equals "1,234.57".
;@
(define (fixed num . b)
  (let ((decimal (if (car b) (car b) 2))
	(no_commas (if (cadr b) (cadr b))))
    (number->string num)))

;XXX;@forecast(x,known_y's,known_x's)
;XXX;@FORECAST estimates a future value according to existing
;XXX; values using simple linear regression. The estimated future value is a
;XXX; y-value for a given x-value (x).
;XXX;
;XXX; If known_x or known_y contains no data entries or different number
;XXX; of data entries, FORECAST returns error. If the variance of the
;XXX; known_x is zero, FORECAST returns error. This function is
;XXX; Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
;XXX; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
;XXX; 29.9, 33.5, and 42.7. Then
;XXX;
;XXX; FORECAST(7,A1..A5,B1..B5) equals -10.859397661.
;XXX;@intercept trend
;XXX(define (forecast x range1 range2)
  ;XXX"Not implemented")

;XXX;@frequency(data_array,bins_array)
;XXX;@FREQUENCY counts how often given values occur within a range
; of values. The results are given as an array.
;
; data_array is a data array for which you want to count the
; frequencies. bin_array is an array containing the intervals into
; which you want to group the values in data_array. If the bin_array is
; empty, FREQUENCY returns the number of data points in data_array.
; Excel compatible.
;XXX;@
;XXX;@
;XXX(define (frequency data_array bins_array)
  ;XXX"Not implemented")

;XXX;@ftest(array1,array2)
;XXX;@FTEST returns the one-tailed probability that the variances
; in the given two data sets are not significantly different. This
; function is Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; FTEST(A1..A5,B1..B5) equals 0.510815017.
;XXX;@fdist finv
;XXX(define (ftest array1 array2)
  ;XXX"Not implemented")

;@fv(rate,term,pmt,pv,type)
;@FV computes the future value of an investment. This is based on
; periodic, constant payments and a constant interest rate. The interest
; rate per period is rate, term is the number of periods in an
; annuity, pmt is the payment made each period, pv is the present
; value and type is when the payment is made. If type = 1 then the
; payment is made at the begining of the period. If type = 0 it is made
; at the end of each period.
;@
;@PV , PMT , PPMT
(define (fv rate nper pmt pv type)
  (let* ((pvif (calculate_pvif rate nper))
	 (fvifa (calculate_fvifa rate nper))
	 (type (if (= 0 type) 0 1)))
    (- (+ (* pv pvif)
	  (* pmt (+ 1 (* rate type)) fvifa)))))

;XXX;@fvschedule(principal,schedule)
;XXX;@FVSCHEDULE returns the future value of given initial value after
; applying a series of compound periodic interest rates. The argument
; principal is the present value; schedule is an array of interest
; rates to apply. The schedule argument must be a range of cells.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain interest rates
; 0.11, 0.13, 0.09, 0.17, and 0.03. Then
;
; FVSCHEDULE(3000,A1..A5) equals 4942.7911611.
;XXX;@PV , FV
;XXX(define (fvschedule principal schedule)
  ;XXX"Not implemented")

;@gammadist(x,alpha,beta,cum)
;@GAMMADIST returns the gamma distribution. If cum is TRUE,
; GAMMADIST returns the incomplete gamma function, otherwise it returns
; the probability mass function.
;
; If x &lt; 0 GAMMADIST returns error. If alpha &lt;= 0 or beta &lt;= 0,
; GAMMADIST returns error. Excel compatible.
;@GAMMADIST(1,2,3,0) equals 0.07961459.
;@gammainv
(define (gammadist x alpha beta cum)
  (if (or (< x 0) (<= alpha 0) (<= beta 0))
    nil
    (if (= cum 0)
      (dgamma x alpha beta)
      (pgamma x alpha beta))))

;@gammainv(p,alpha,beta)
;@GAMMAINV returns the inverse of the cumulative gamma
; distribution.
;
; If p &lt; 0 or p > 1 GAMMAINV returns error. If alpha &lt;= 0 or
; beta &lt;= 0 GAMMAINV returns error. This function is Excel
; compatible.
;@gammainv(0.34,2,4) equals 4.829093908.
;@gammadist
(define (gammainv p alpha beta)
  (if (or (< p 0) (> p 1) (<= alpha 0) (<= beta 0))
    nil
    (qgamma p alpha beta)))

;@gammaln(x)
;@gammaln returns the natural logarithm of the gamma function.
;
; If x is non-number then gammaln returns error. If x &lt;= 0
; then gammaln returns error. Excel compatible.
;@gammaln(23) equals 48.471181352.
;@poisson
(define (gammaln x)
  (if (<= x 0)
    nil
    (lgamma x)))

;@gcd(number1,number2,...)
;@gcd returns the greatest common divisor of given numbers.
;
; If any of the arguments is less than zero, gcd returns error. If
; any of the arguments is a non-integer, it is truncated. This function is
; Excel compatible.
;@gcd(470,770) equals 10.
;
; gcd(470,770,1495) equals 5.
;@lcm
; lives in mathfunc.c

;@geomean(b1, b2, ...)
;@GEOMEAN returns the geometric mean of the given arguments. This is
; equal to the Nth root of the product of the terms. This function is
; Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; GEOMEAN(A1..A5) equals 21.279182482.
;@AVERAGE , HARMEAN , MEDIAN , MODE , TRIMMEAN
(define (geomean . b)
  (range_geometric_mean (collect_args b (lambda x (number? x)))))

;@gestep(x[,y])
;@gestep test for if x is >= y, returning 1 if it is so, and
; 0 otherwise. y is optional, and defaults to 0.
;
; If either argument is non-numeric returns a error. This
; function is Excel compatible.
;@gestep(5,4) equals 1.
;@delta
(define (gestep x . b)
  (let ((y (if (car b) (car b) 0)))
    (if (>= x y) 1 0)))

;XXX;@getpivotdata(pivot_table,field_name)
;XXX;@GETPIVOTDATA fetches summary data from a pivot table.
; pivot_table is a cell range containing the pivot table. field_name
; is the name of the field of which you want the summary data.
;
; If the summary data is unavailable, GETPIVOTDATA returns error.
;XXX;@
;XXX;XXX;@
;XXX(define (getpivotdata pivot_table field_name)
  ;XXX"Not implemented")

;XXX;@growth(known_y's[,known_x's,new_x's,const])
;XXX;@GROWTH applies the ``least squares'' method to fit an
; exponential curve to your data and predicts the exponential growth by
; using this curve.
;
; If known_x's is omitted, an array {1, 2, 3, ...} is used. If new_x's
; is omitted, it is assumed to be the same as known_x's.
;
; GROWTH returns an array having one column and a row for each data
; point in new_x.
;
; If known_y's and known_x's have unequal number of data points,
; GROWTH returns error.
;
; If const is FALSE, the line will be forced to go through the origin,
; i.e., b will be zero. The default is TRUE.
;XXX;@
;XXX;@LOGEST , GROWTH , TREND
;XXX(define (growth known_y . b)
  ;XXX"Not implemented")

;@g_product(value1, value2, ...)
;@PRODUCT returns the product of all the values and cells referenced in
; the argument list. Empty cells are ignored and the empty product in 1.
;@G_PRODUCT(2,5,9) equals 90.
;@sum count
(define (g_product . b)
  (let* ((p (collect_args b (lambda x (number? x))))
	 (s 1))
    (while p
      (set! s (* (car p) s))
      (set! p (cdr p)))
    s))

;XXX;@harmean(b1, b2, ...)
;XXX;@HARMEAN returns the harmonic mean of the N data points. This function
; is Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; HARMEAN(A1..A5) equals 19.529814427.
;XXX;@AVERAGE , GEOMEAN , MEDIAN , MODE , TRIMMEAN
;XXX(define (harmean . b)
  ;XXX(range_harmonic_mean (collect_args b (lanbda x (number? x)))))

;@hex2bin(number[,places])
;@The HEX2BIN function converts a hexadecimal number to a binary number.
; places is an optional field, specifying to zero pad to that number of
; spaces.
;
; If places is too small or negative error is returned. This
; function is Excel compatible.
;@HEX2BIN("2A") equals 101010.
;@BIN2HEX , HEX2OCT , HEX2DEC
(define (hex2bin n . p)
  (num2str (str2num n 16) 2 (car p)))

;@hex2dec(x)
;@The HEX2DEC function converts a hexadecimal number to its decimal
; equivalent. Excel compatible.
;@HEX2DEC("2A") equals 42.
;@DEC2HEX , HEX2BIN , HEX2OCT
(define (hex2dec n)
  (str2num n 16))

;@hex2oct(number[,places])
;@The HEX2OCT function converts a hexadecimal number to an octal number.
; places is an optional field, specifying to zero pad to that number of
; spaces.
;
; If places is too small or negative error is returned. This
; function is Excel compatible.
;@HEX2OCT("2A") equals 52.
;@OCT2HEX , HEX2BIN , HEX2DEC
(define (hex2oct n . p)
  (num2str (str2num n 16) 8 (car p)))

;@hlookup(value,range,row[,approximate])
;@HLOOKUP finds the col in range that has a first row cell
; similar to value. If approximate is not true it finds the col with an
; exact equivilance. If approximate is true, then the values must be
; sorted in order of ascending value for correct function; in this case
; it finds the col with value less than value it returns the value in
; the col found at a 1 based offset in row rows into the range.
; Returns error if row &lt; 0. Returns error if row falls outside range.
;@
;@vlookup
(define hlookup @HLOOKUP)

;@hour(serial_number)
;@Converts a serial number to an hour. The hour is returned as an
; integer in the range 0 (12:00 A.M.) to 23 (11:00 P.M.).
;@
;@MINUTE , NOW , TIME , SECOND
(define (hour serial_number)
  (cdr (assoc 'hour (localtime serial_number))))

;@hyperlink(reference)
;@The HYPERLINK function currently returns its 2nd argument, or if that
; is omitted the 1st argument.
;@
;@
(define (hyperlink a . b)
  (if (car b) (car b) a))

;@hypgeomdist(x, n, M, N)
;@HYPGEOMDIST returns the hypergeometric distribution. x is
; the number of successes in the sample, n is the number of trials, M
; is the number of successes overall, and N is thepopulation size.
;
; If x,n,M or N is a non-integer it is truncated. If x,n,M or N
; &lt; 0 HYPGEOMDIST returns error. If x > M or n > N HYPGEOMDIST
; returns error. Excel compatible.
;@HYPGEOMDIST(1,2,3,10) equals 0.4666667.
;@binomdist poisson
(define (hypgeomdist x n M N)
  (if (or (< x 0) (< n 0) (< M 0) (< N 0) (> x M) (> n N))
    nil
    (/ (* (combin M x)
	  (combin (- N M) (- n x)))
       (combin N n))))

;XXX;@if(condition[,if-true,if-false])
;XXX;@Use the IF statement to evaluate conditionally other expressions IF
; evaluates condition. If condition returns a non-zero value the
; result of the IF expression is the if-true expression, otherwise IF
; evaluates to the value of if-false. If ommitted if-true defaults to
; TRUE and if-false to FALSE. Excel compatible.
;XXX;@IF(FALSE,TRUE,FALSE) equals FALSE.
;XXX;@
;XXX; Not implemented; clashes with Scheme if.

;@imabs(inumber)
;@IMABS returns the absolute value of a complex number. This function is
; Excel compatible.
;@IMABS("2-j") equals 2.23606798.
;@imaginary imreal
(define (imabs inumber)
  (let ((v (imext2int inumber)))
    (hypot (car v) (cdr v))))

;@imaginary(inumber)
;@IMAGINARY returns the imaginary coefficient of a complex number. This
; function is Excel compatible.
;@IMAGINARY("132-j") equals -1.
;@imreal
(define (imaginary inumber)
  (cdr (imext2int inumber)))

(define atan_2 atan2)

;@imargument(inumber)
;@IMARGUMENT returns the argument theta of a complex number. This
; function is Excel compatible.
;@IMARGUMENT("2-j") equals -0.463647609.
;@
(define (imargument inumber)
  (let* ((v (imext2int inumber))
	 (re (car v))
	 (im (cdr v)))
    (atan_2 im re)))

;@imconjugate(inumber)
;@IMCONJUGATE returns the complex conjugate of a complex number. This
; function is Excel compatible.
;@IMCONJUGATE("1-j") equals 1+j.
;@IMAGINARY , IMREAL
(define (imconjugate inumber)
  (let ((v (imext2int inumber)))
    (imint2ext (cons (car v) (- (cdr v))))))

;@imcos(inumber)
;@IMCOS returns the cosine of a complex number. This function is Excel
; compatible.
;@IMCOS("1+j") equals 0.833730-0.988898j.
;@IMSIN , IMTAN
(define (imcos inumber)
  (let* ((v (imext2int inumber))
	 (re (car v))
	 (im (cdr v)))
    (imint2ext (cons (* (cos re) (cosh im))
		     (* (- (sin re)) (sinh im))))))

;@imdiv(inumber,inumber)
;@IMDIV returns the quotient of two complex numbers. This function is
; Excel compatible.
;@IMDIV("2-j","2+j") equals 0.6-0.8j.
;@improduct
(define (imdiv i j)
  (let* ((v1 (imext2int i))
	 (v2 (imext2int j))
	 (r1 (car v1))
	 (i1 (cdr v1))
	 (r2 (car v2))
	 (i2 (cdr v2))
	 (a (+ (* r2 r2) (* i2 i2))))
    (imint2ext (cons (/ (+ (* r1 r2) (* i1 i2)) a)
		     (/ (- (* i1 r2) (* r1 i2)) a)))))

;@imexp(inumber)
;@IMEXP returns the exponential of a complex number. This function is
; Excel compatible.
;@IMEXP("2-j") equals 3.992324-6.217676j.
;@imln
(define (imexp inumber)
  (let* ((v (imext2int inumber))
	 (re (car v))
	 (im (cdr v)))
    (imint2ext (cons (* (exp re) (cos im))
		     (* (exp re) (sin im))))))

;@imln(inumber)
;@IMLN returns the natural logarithm of a complex number. (The result
; will have an imaginary part between -pi an +pi. The natural logarithm
; is not uniquely defined on complex numbers. You may need to add or
; subtract an even multiple of pi to the imaginary part.) This function
; is Excel compatible.
;@ IMLN("3-j") equals 1.15129-0.32175j.
;@IMEXP , IMLOG2 , IMLOG10
(define (imln inumber)
  (imint2ext (cons (log (imabs inumber))
		   (imargument inumber))))

;@imlog_10(inumber)
;@	IMLOG_10 returns the logarithm of a complex number in base 10. This
; function is Excel compatible.
;@	 IMLOG_10("3-j") equals 0.5-0.13973j.
;@IMLN , IMLOG_2
(define (imlog_10 inumber)
  (let* ((lg10 (log 10))
	 (re (log (imabs inumber)))
	 (im (imargument inumber)))
    (imint2ext (cons (/ re lg10) (/ im lg10)))))

;@imlog_2(inumber)
;@IMLOG_2 returns the logarithm of a complex number in base 2. This
; function is Excel compatible.
;@	IMLOG_2("3-j") equals 1.66096-0.46419j.
;@IMLN , IMLOG_10
(define (imlog_2 inumber)
  (let* ((lg2 (log 2))
	 (re (log (imabs inumber)))
	 (im (imargument inumber)))
    (imint2ext (cons (/ re lg2) (/ im lg2)))))

;@impower(inumber,number)
;@IMPOWER returns a complex number raised to a power. inumber is the
; complex number to be raised to a power and number is the power to
; which you want to raise the complex number. This function is Excel
; compatible.
;@IMPOWER("4-j",2) equals 15-8j.
;@imsqrt
(define (impower i j)
  (let* ((v1 (imext2int i))
	 (v2 (imext2int j))
	 (r1 (car v1))
	 (i1 (cdr v1))
	 (r2 (car v2))
	 (i2 (cdr v2)))
    (if (and (= i1 0) (= i2 0))
      (imint2ext (cons (pow r1 r2) 0))
      (imexp (improduct j (imln i))))))

;@improduct(inumber1[,inumber2,...])
;@IMPRODUCT returns the product of given complex numbers. This function
; is Excel compatible.
;@ IMPRODUCT("2-j","4-2j") equals 6-8j.
;@imdiv
(define (improduct . b)
  (let ((p (collect_args b (lambda x (not (null? x)))))
	(re 1)
	(im 0)
	v r1 i1 r2)
    (while p
      (set! v (imext2int (car p)))
      (set! p (cdr p))
      (set! r1 (car v))
      (set! i1 (cdr v))
      (set! r2 (- (* re r1) (* im i1)))
      (set! im (+ (* re i1) (* im r1)))
      (set! re r2))
    (imint2ext (cons re im))))

;@imreal(inumber)
;@IMREAL returns the real coefficient of a complex number. This function
; is Excel compatible.
;@imreal("132-j") equals 132.
;@imaginary
(define (imreal inumber)
  (let ((v (imext2int inumber)))
    (car v)))

;@imsin(inumber)
;@IMSIN returns the sine of a complex number. This function is Excel
; compatible.
;@IMSIN("1+j") equals 1.29846+0.63496j.
;@IMCOS , IMTAN
(define (imsin inumber)
  (let* ((v (imext2int inumber))
	 (re (car v))
	 (im (cdr v)))
    (imint2ext (cons (* (sin re) (cosh im))
		     (* (cos re) (sinh im))))))

;@imsqrt(inumber)
;@IMSQRT returns the square root of a complex number. This function is
; Excel compatible.
;@IMSQRT("1+j") equals 1.09868+0.4550899j.
;@impower
(define (imsqrt inumber)
  (impower inumber 0.5))

;@imsub(inumber,inumber)
;@IMSUB returns the difference of two complex numbers. This function is
; Excel compatible.
;@IMSUB("3-j","2+j") equals 1-2j.
;@imsum
(define (imsub i j)
  (let ((v1 (imext2int i))
	(v2 (imext2int j)))
    (imint2ext (cons (- (car v1) (car v2))
		     (- (cdr v1) (cdr v2))))))

;@imsum(inumber,inumber)
;@IMSUM returns the sum of two complex numbers. This function is Excel
; compatible.
;@IMSUM("2-4j","9-j") equals 11-5j.
;@imsub
(define (imsum i j)
  (let ((v1 (imext2int i))
	(v2 (imext2int j)))
    (imint2ext (cons (+ (car v1) (car v2))
		     (+ (cdr v1) (cdr v2))))))

;@imtan(inumber)
;@IMTAN returns the tangent of a complex number. This function is Excel
; compatible.
;@
;@imsin imcos
(define (imtan i)
  (imdiv (imsin i) (imcos i)))

;XXX;@index(reference,[row, col, area])
;XXX;@The INDEX function returns a reference to the cell at a offset into
; the reference specified by row, col.
;
; If things go wrong returns error
;XXX;@
;XXX;@
;XXX(define (index . b)
  ;XXX"Not implemented")

;XXX;@indirect(ref_text,[format])
;XXX;@INDIRECT returns the contents of the cell pointed to by the
; ref_text string. The string specifices a single cell reference the
; format of which is either A1 or R1C1 style. The style is set by the
; format boolean, which defaults to the former.
;
; If ref_text is not a valid reference returns error
;XXX;@
;XXX;@
;XXX(define (indirect ref_text . b)
  ;XXX"Not implemented")

;@info()
;@INFO returns information about the current operating environment. This
; function is Excel compatible.
;@
;@
(define (info)
  "Siag")

;@int(a)
;@INT rounds a now to the nearest integer where `nearest'
; implies being closer to zero. INT is equivalent to FLOOR(a) for a >=
; 0, and CEIL(a) for a &lt; 0. Excel compatible.
;@INT(7.2) equals 7.
;
; INT(-5.5) equals -6.
;@FLOOR , CEIL , ABS
(define (int a)
  (if (< a 0)
    (- (ceil (- a)))
    (floor a)))

;XXX;@intercept(known_y's,known_x's)
;XXX;@INTERCEPT calculates the point where the linear regression
; line intersects the y-axis.
;
; If known_x or known_y contains no data entries or different number
; of data entries, INTERCEPT returns error. If the variance of the
; known_x is zero, INTERCEPT returns error. This function is
; Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; INTERCEPT(A1..A5,B1..B5) equals -20.785117212.
;XXX;@FORECAST , TREND
(define (intercept ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  (range_intercept (collect_range ar1 ac1 ar2 ac2 number?)
		   (collect_range br1 bc1 br2 bc2 number?)))

;XXX;@intrate(settlement,maturity,investment,redemption[,basis])
;XXX;@ intrate calculates and returns the interest rate of a fully vested
; security. investment is the prize of the security paid at settlement
; date and redemption is the amount to be received at maturity date.
; basis is the type of day counting system you want to use:
;
; 0 US 30/360
;
; 1 actual days/actual days
;
; 2 actual days/360
;
; 3 actual days/365
;
; 4 European 30/360
;
; If basis is omitted, US 30/360 is applied. If settlement date or
; maturity date is not valid, intrate returns error. If basis &lt;
; 0 or basis > 4, intrate returns error. If settlement date is
; after maturity date or they are the same, intrate returns
; error.
;XXX;@If you had a bond with a settlement date of April 15, 2000, maturity
; date September 30, 2000, investment of $100,000, redemption value
; $103,525, using the actual/actual basis, the bond discount rate is:
;
; =intrate(36631, 36799, 100000, 103525, 1) which equals 0.0648 or 6.48%
;XXX;@RECEIVED , DATE
(define (intrate settlement maturity investment redemption . b)
  (let* ((basis (if (car b) (car b) 0))
	 (a (days_monthly_basis settlement maturity basis))
	 (d (annual_year_basis settlement basis)))
    (if (or (< basis 0) (> basis 4) (<= a 0) (<= d 0) (= investment 0))
      nil
      (* (/ (- redemption investment) investment)
	 (/ d a)))))

;XXX;@ipmt(rate,per,nper,pv[,fv[,type]])
;XXX;@IPMT calculates the amount of a payment of an annuity going towards
; interest.
;
; Formula for IPMT is:
;
; IPMT(PER) = -PRINCIPAL(PER-1) * INTEREST_RATE
;
; where:
;
; PRINCIPAL(PER-1) = amount of the remaining principal from last period.
;XXX;@
;XXX;@ppmt pv fv
(define (ipmt rate per nper pv . b)
  (let* ((fv (if (car b) (car b) 0))
	 (type (if (cadr b) (cadr b) 0))
	 (pmt (calculate_pmt rate nper pv fv type)))
    (- (* (calculate_principal pv pmt rate (- per 1)) rate))))

;XXX;@irr(values[,guess])
;XXX;@IRR calculates and returns the internal rate of return of an
; investment. This function is closely related to the net present value
; function (NPV). The IRR is the interest rate for a serie of cash flow
; where the net preset value is zero.
;
; values contains the serie of cash flow generated by the investment.
; The payments should occur at regular intervals. The optional guess is
; the initial value used in calculating the IRR. You do not have to use
; that, it is only provided for the Excel compatibility.
;
; Excel compatible.
;XXX;@Let us assume that the cells A1..A8 contain the numbers -32432, 5324,
; 7432, 9332, 12324, 4334, 1235, -3422. Then
;
; IRR(A1..A8) returns 0.04375.
;XXX;@FV , NPV , PV
;XXX(define (irr values . b)
  ;XXX"Not implemented")

;@isblank(exp)
;@ISBLANK returns TRUE if the value is blank. This function is Excel
; compatible.
;@
;@
(define (isblank exp)
  (if (null? exp) 1 0))

;XXX;@iserr(exp)
;XXX;@ISERR returns TRUE if the value is any error value except #N/A. This
; function is Excel compatible.
;XXX;@
;XXX;@iserror
;XXX(define (iserr exp)
  ;XXX"Not implemented")

;XXX;@iserror(exp)
;XXX;@ISERROR returns a TRUE value if the expression has an error
;
; Excel compatible.
;XXX;@
;XXX;@error
;XXX(define (iserror exp)
  ;XXX"Not implemented")

;@iseven(x)
;@ISEVEN returns TRUE if the number is even. This function is Excel
; compatible.
;@
;@isodd
(define (iseven x)
  (if (= (fmod (floor x) 2) 0) 1 0))

;@islogical(x)
;@ISLOGICAL returns TRUE if the value is a logical value. This function
; is Excel compatible.
;@
;@
(define (islogical x)
  (if (number? x) 1 0))

;@isna(x)
;@ISNA returns TRUE if the value is the #N/A error value. This function
; is Excel compatible.
;@
;@
(define (isna x)
  0)

;@isnontext(x)
;@ISNONTEXT Returns TRUE if the value is not text. This function is
; Excel compatible.
;@
;@istext
(define (isnontext x)
  (if (string? x) 0 1))

;@isnumber(x)
;@ISNUMBER returns TRUE if the value is a number. This function is Excel
; compatible.
;@
;@
(define (isnumber x)
  (if (number? x) 1 0))

;@isodd()
;@ISODD returns TRUE if the number is odd. This function is Excel
; compatible.
;@
;@iseven
(define (isodd x)
  (if (= (fmod (floor x) 2) 1) 1 0))

;@ispmt(rate,per,nper,pv)
;@ISPMT returns the interest paid on a given period.
;
; If per &lt; 1 or per > nper, ISPMT returns error.
;@
;@pv
(define (ispmt rate per nper pv)
  (let ((tmp (- (* pv rate))))
    (if (or (< per 1) (> per nper))
      nil
      (- tmp (* (/ tmp nper) per)))))

;XXX;@isref(x)
;XXX;@ISREF returns TRUE if the value is a reference. This function is Excel
; compatible.
;XXX;@
;XXX;@
;XXX(define (isref x)
  ;XXX"Not implemented")

;@istext()
;@ISTEXT returns TRUE if the value is text. This function is Excel
; compatible.
;@
;@isnontext
(define (istext x)
  (if (string? x) 1 0))

;@kurt(n1, n2, ...)
;@KURT returns an unbiased estimate of the kurtosis of a data set.
;
; Note, that this is only meaningful is the underlying distribution
; really has a fourth moment. The kurtosis is offset by three such that
; a normal distribution will have zero kurtosis.
;
; Strings and empty cells are simply ignored.
;
; If fewer than four numbers are given or all of them are equal KURT
; returns error. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; KURT(A1..A5) equals 1.234546305.
;@AVERAGE , VAR , SKEW , KURTP
(define (kurt . b)
  (range_kurtosis_m3_est (collect_args b (lambda x (number? x)))))

;@kurtp(n1, n2, ...)
;@KURTP returns the population kurtosis of a data set.
;
; Strings and empty cells are simply ignored.
;
; If fewer than two numbers are given or all of them are equal KURTP
; returns error.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; KURTP(A1..A5) equals -0.691363424.
;@AVERAGE , VARP , SKEWP , KURT
(define (kurtp . b)
  (range_kurtosis_m3_pop (collect_args b (lambda x (number? x)))))

;@large(n1, n2, ..., k)
;@LARGE returns the k-th largest value in a data set.
;
; If data set is empty LARGE returns error. If k &lt;= 0 or k is
; greater than the number of data items given LARGE returns error.
; Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; LARGE(A1..A5,2) equals 25.9.
;
; LARGE(A1..A5,4) equals 17.3.
;@PERCENTILE , PERCENTRANK , QUARTILE , SMALL
(define (large . b)
  (let* ((p (collect_args b (lambda x (number? x))))
	 (r (reverse p))
	 (n (car r))
	 (s (qsort (cdr r) >)))
    (if (>= n 1)
      (nth (- n 1) s))))

;@lcm(number1,number2,...)
;@LCM returns the least common multiple of integers. The least common
; multiple is the smallest positive number that is a multiple of all
; integer arguments given.
;
; If any of the arguments is less than one, LCM returns error.
; Excel compatible. Requires the GMP library.
;@LCM(2,13) equals 26.
;
; LCM(4,7,5) equals 140.
;@gcd
; no number => 1
; one number only => finished
; two numbers => 
(define (lcm_ p)
  (if (null p)
    1
    (string->number (mpz_lcm (car p)
			     (lcm_ (cdr p))))))

(define (lcm . b)
  (lcm_ (collect_args b (lambda x (number? x)))))

;@left(text[,num_chars])
;@LEFT returns the leftmost num_chars characters or the left character
; if num_chars is not specified.
;@LEFT("Directory",3) equals "Dir".
;@mid right
(define (left t . n)
  (substring t 0 (if n (car n) 1)))

;@len(string)
;@LEN returns the length in characters of the string string.
;@len("Helsinki") equals 8.
;@char code
(define len string-length)

;XXX;@linest(known_y's[,known_x's[,const[,stat]]])
;XXX;@LINEST calculates the ``least squares'' line that best fit to
; your data in known_y's. known_x's contains the corresponding x's
; where y=mx+b.
;
; If known_x's is omitted, an array {1, 2, 3, ...} is used. LINEST
; returns an array having two columns and one row. The slope (m) of the
; regression line y=mx+b is given in the first column and the
; y-intercept (b) in the second.
;
; If known_y's and known_x's have unequal number of data points,
; LINEST returns error.
;
; If const is FALSE, the line will be forced to go through the origin,
; i.e., b will be zero. The default is TRUE.
;
; If stat is TRUE, extra statistical information will be returned.
; Extra statistical information is written bellow the regression line
; coefficients in the result array. Extra statistical information
; consists of four rows of data. In the first row the standard error
; values for the coefficients m1, (m2, ...), b are represented. The
; second row contains the square of R and the standard error for the y
; estimate. The third row contains the F-observed value and the degrees
; of freedom. The last row contains the regression sum of squares and
; the residual sum of squares.
;
; The default of stat is FALSE.
;XXX;@
;XXX;@logest trend
;XXX(define (linest known_y . b)
  ;XXX"Not implemented")

;@ln(x)
;@LN returns the natural logarithm of x. If x &lt;= 0, LN returns
; error. Excel compatible.
;@LN(7) equals 1.94591.
;@EXP , LOG_2 , LOG_10
(define ln log)

;@log_2(x)
;@LOG_2 computes the base-2 logarithm of x. If x &lt;= 0, LOG_2 returns
; error. The name of this functions is log_2 rather than log2, otherwise Siag
; would interpret the name as a reference.
;@LOG_2(1024) equals 10.
;@EXP , LOG_10 , LOG
(define (log_2 x)
  (/ (log x) (log 2)))

;XXX;@logest(known_y's[,known_x's,const,stat])
;XXX;@The LOGEST function applies the ``least squares'' method to fit an
; exponential curve of the form y = b * m{1}^x{1} * m{2}^x{2}... to your
; data.
;
; If known_x's is omitted, an array {1, 2, 3, ...} is used. LOGEST
; returns an array { m{n},m{n-1}, ...,m{1},b }.
;
; If known_y's and known_x's have unequal number of data points,
; LOGEST returns error.
;
; If const is FALSE, the line will be forced to go through (0,1),i.e.,
; b will be one. The default is TRUE.
;
; If stat is TRUE, extra statistical information will be returned.
; Extra statistical information is written bellow the regression line
; coefficients in the result array. Extra statistical information
; consists of four rows of data. In the first row the standard error
; values for the coefficients m1, (m2, ...), b are represented. The
; second row contains the square of R and the standard error for the y
; estimate. The third row contains the F-observed value and the degrees
; of freedom. The last row contains the regression sum of squares and
; the residual sum of squares.
;
; The default of stat is FALSE.
;XXX;@
;XXX;@LINEST , GROWTH , TREND
;XXX(define (logest known_y . b)
  ;XXX"Not implemented")

;@loginv(p,mean,stdev)
;@LOGINV returns the inverse of the lognormal cumulative
; distribution. p is the given probability corresponding to the normal
; distribution, mean is the arithmetic mean of the distribution, and
; stdev is the standard deviation of the distribution.
;
; If p &lt; 0 or p > 1 or stdev &lt;= 0 LOGINV returns error. This
; function is Excel compatible.
;@LOGINV(0.5,2,3) equals 7.389056099.
;@EXP , LN , LOG , LOG10 , LOGNORMDIST
(define (loginv p mean stdev)
  (if (or (< p 0) (> p 1) (<= stdev 0))
    nil
    (qlnorm p mean stdev)))

;@lognormdist(x,mean,stdev)
;@lognormdist returns the lognormal distribution. x is the
; value for which you want the distribution, mean is the mean of the
; distribution, and stdev is the standard deviation of the
; distribution. Excel compatible.
;
; If stdev = 0 lognormdist returns error. If x &lt;= 0, mean &lt; 0
; or stdev &lt; 0 lognormdist returns error.
;@lognormdist(3,1,2) equals 0.519662338.
;@normdist
(define (lognormdist x mean stdev)
  (if (or (= stdev 0) (<= x 0) (< mean 0) (< stdev 0))
    nil
    (plnorm x mean stdev)))

;XXX;@lookup(value, vector1, vector2)
;XXX;@The LOOKUP function finds the row index of 'value' in vector1 and
; returns the contents of vector2 at that row index. If the area is
; longer than it is wide then the sense of the search is rotated.
; Alternatively a single array can be used.
;
; If LOOKUP can't find value it uses the next largest value less than
; value. The data must be sorted.
;
; If value is smaller than the first value it returns #N/A
;XXX;@
;XXX;@VLOOKUP , HLOOKUP
;XXX(define (lookup value vector1 vector2)
  ;XXX"Not implemented")

;@lower(text)
;@LOWER returns a lower-case version of the string in text.
;@LOWER("J. F. Kennedy") equals "j. f. kennedy".
;@upper
(define lower string-downcase)

;XXX;@match(seek,vector[,type])
;XXX;@The MATCH function finds the row index of seek in vector and returns
; it. If the area is longer than it is wide then the sense of the search
; is rotated. Alternatively a single array can be used.
;
; The type parameter, which defaults to +1, controls the search:
;
; If type = 1, finds largest value &lt;= seek.
;
; If type = 0, finds first value == seek.
;
; If type = -1, finds smallest value >= seek.
;
; For type 0, the data can be in any order. For types -1 and +1, the
; data must be sorted. (And in this case, MATCH uses a binary search to
; locate the index.)
;
; If seek could not be found, #N/A is returned.
;XXX;@
;XXX;@lookup
;XXX(define (match seek vector . b)
  ;XXX"Not implemented")

;@maxa(number1,number2,...)
;@MAXA returns the largest value of the given arguments. Numbers, text
; and logical values are included in the calculation too. If the cell
; contains text or the argument evaluates to FALSE, it is counted as
; value zero (0). If the argument evaluates to TRUE, it is counted as
; one (1). Note that empty cells are not counted. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, 17.3, "missing", 25.9, and 40.1. Then
;
; MINA(A1..A5) equals 0.
;@max mina
(define (maxa . b)
  (let* ((q (collect_args b (lambda x (not (null? x)))))
	 s v)
    (while q
      (set! v (if (number? (car q) (car q) 0)))
      (if (or (null? s) (< s v))
	(set! s v))
      (set! q (cdr q)))
    s))

;XXX;@mdeterm(matrix)
;XXX;@MDETERM returns the determinant of a given matrix.
;
; If the matrix does not contain equal number of columns and rows,
; MDETERM returns error. Excel compatible.
;XXX;@Let us assume that A1, ..., A4 contain numbers 2, 3, 7, and 3, B1,
; ..., B4 4, 2, 4, and 1, C1, ..., C4 9, 4, 3, and 2, and D1, ..., D4 7,
; 3, 6, and 5. Then
;
; MDETERM(A1..D4) equals 148.
;XXX;@mmult minverse
;XXX(define (mdeterm dummy r1 c1 r2 c2)
  ;XXX"Not implemented")

;;;@mduration(settlement,maturity,coupon,yield,frequency[,basis])
;;;@Returns the Macauley duration for a security with par value 100.
;;;@
;;;@
;;(define (mduration settlement maturity coupon yield frequency . b)
;;  (let* ((basis (if (car b) (car b) 0)))
;;    "Not implemented"))

;@median(n1, n2, ...)
;@MEDIAN returns the median of the given data set.
;
; Strings and empty cells are simply ignored. If even numbers are given
; MEDIAN returns the average of the two numbers in the middle. This
; function is Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; MEDIAN(A1..A5) equals 21.3.
;@AVERAGE , COUNT , COUNTA , DAVERAGE , MODE , SUM
(define (median . b)
  (let* ((p (collect_args b (lambda x (number? x))))
	 (s (qsort p <))
	 (n (length s))
	 (i (/ (- n 1) 2)))
    (/ (+ (nth (floor i) s) (nth (ceil i) s)) 2)))

;XXX;@mid(string, position, length)
;XXX;@MID returns a substring from string starting at position for length
; characters.
;XXX;@MID("testing",2,3) equals "est".
;XXX;@left right
;XXX(define (mid string position length)
  ;XXX"Not implemented; oboe")

;@mina(number1,number2,...)
;@MINA returns the smallest value of the given arguments. Numbers, text
; and logical values are included in the calculation too. If the cell
; contains text or the argument evaluates to FALSE, it is counted as
; value zero (0). If the argument evaluates to TRUE, it is counted as
; one (1). Note that empty cells are not counted. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, 17.3, "missing", 25.9, and 40.1. Then
;
; MAXA(A1..A5) equals 40.1.
;@min maxa
(define (mina . b)
  (let* ((q (collect_args b (lambda x (not (null? x)))))
	 s v)
    (while q
      (set! v (if (number? (car q) (car q) 0)))
      (if (or (null? s) (> s v))
	(set! s v))
      (set! q (cdr q)))
    s))

;@minute(serial_number)
;@Converts a serial number to a minute. The minute is returned as an
; integer in the range 0 to 59.
;@
;@HOUR , NOW , TIME , SECOND
(define (minute serial_number)
  (cdr (assoc 'min (localtime serial_number))))

;XXX;@minverse(matrix)
;XXX;@MINVERSE returns the inverse matrix of a given matrix.
;
; If the matrix cannot be inverted, MINVERSE returns error. If
; the matrix does not contain equal number of columns and rows,
; MINVERSE returns error. Excel compatible.
;XXX;@
;XXX;@mmult mdeterm
;XXX(define (minverse dummy r1 c1 r2 c2)
  ;XXX"Not implemented")

;XXX;@mirr(values,finance_rate,reinvest_rate)
;XXX;@MIRR returns the modified internal rate of return for a given
; periodic cash flow.
;XXX;@
;XXX;@npv
;XXX(define (mirr values frate rrate)
  ;XXX"Not implemented")


(define (get_value r c)
  (let ((v (get-cell r c)))
    (if (number? v) v 0)))

(define (scalprod r1 c1 r2 c2 n)
  (if (= 0 n)
    0
    (+ (* (get_value r1 c1) (get_value r2 c2))
       (scalprod r1 (+ c1 1) (+ r2 1) c2 (- n 1)))))

;@mmult(array1,array2)
;@MMULT returns the matrix product of two arrays. The result is
; an array with the same number of rows as array1 and the same number
; of columns as array2. Excel compatible.
;@
;@transpose minverse
(define (mmult dua r1a c1a r2a c2a dub r1b c1b r2b c2b)
  (if (and (eq? dua 'RANGE) (eq? dub 'RANGE)
	   (>= r2a r1a) (>= c2a c1a) (>= c2b c1b) (= (- c2a c1a) (- r2b r1b)))
    (let ((nra (- r2a r1a -1))
	  (nca (- c2a c1a -1))
	  (nrb (- r2b r1b -1))
	  (ncb (- c2b c1b -1))
	  (r0 (row))
	  (c0 (col))
	  (r 0)
	  (c 1))
      (while (< r nra)
        (while (< c ncb)
	  (set_array_value (+ r0 r)
		     (+ c0 c)
		     (scalprod (+ r1a r) c1a r1b (+ c1b c) nca))
	  (set! c (+ c 1)))
	(set! r (+ r 1))
	(set! c 0))
      (set-pr-scr)
      (recalc-matrix)
      (scalprod r1a c1a r1b c1b nca))))

;@mod(number,divisor)
;@MOD returns the remainder when divisor is divided into
; number. Excel compatible.
;
; MOD returns error if divisor is zero.
;@MOD(23,7) equals 2.
;@int floor ceil
(define mod fmod)

;XXX;@mode(n1, n2, ...)
;XXX@MODE returns the most common number of the data set. If the data set
; has many most common numbers MODE returns the first one of them.
;
; Strings and empty cells are simply ignored. If the data set does not
; contain any duplicates MODE returns error. This function is
; Excel compatible.
;XXX;@
;XXX;@average median
;XXX(define (mode . b)
  ;XXX"Not implemented")

;@month(serial_number)
;@Converts a serial number to a month.
;@
;@day time now year
(define (month serial_number)
  (cdr (assoc 'mon (localtime serial_number))))

;@mround(number,multiple)
;@MROUND rounds a given number to the desired multiple. number
; is the number you want rounded and multiple is the the multiple to
; which you want to round the number.
;
; If number and multiple have different sign, MROUND returns
; error. Excel compatible.
;@MROUND(1.7,0.2) equals 1.8.
;
; MROUND(321.123,0.12) equals 321.12.
;@ROUNDDOWN , ROUND , ROUNDUP
(define (mround n m)
  (let* ((acc 0.0000003)
	 (sign (if (< n 0) -1 1))
	 (number (* sign n))
	 (multiple (* sign m))
	 (mod (fmod number multiple))
	 (div (- number mod)))
    (* sign (+ div (if (>= (+ mod acc) (/ multiple 2)) multiple 0)))))

;XXX;@multinomial(value1, value2, ...)
;XXX;@MULTINOMIAL returns the ratio of the factorial of a sum of values to
; the product of factorials. Excel compatible.
;XXX;@MULTINOMIAL(2,3,4) equals 1260.
;XXX;@sum
;XXX(define (multinomial . b)
  ;XXX"Not implemented")

;@n(x)
;@N returns a value converted to a number. Strings containing text are
; converted to the zero value. Excel compatible.
;@
;@
(define (n x)
  (cond
    ((number? x) x)
    ((string? x) 0)
    (t nil)))

;XXX;@na()
;XXX;@NA returns the error value #N/A. Excel compatible.
;XXX;@
;XXX;@
;XXX(define (na)
  ;XXX"Not implemented")

;@negbinomdist(f,t,p)
;@negbinomdist returns the negative binomial distribution. f
; is the number of failures, t is the threshold number of successes,
; and p is the probability of a success.
;
; If f or t is a non-integer it is truncated. If (f + t -1) &lt;= 0
; negbinomdist returns error. If p &lt; 0 or p > 1 negbinomdist
; returns error. Excel compatible.
;@negbinomdist(2,5,0.55) equals 0.152872629.
;@BINOMDIST , COMBIN , FACT , HYPGEOMDIST , PERMUT
(define (negbinomdist f t p)
  (if (or (<= (+ x r -1) 0)
	  (< p 0)
	  (> p 1))
    nil
    (* (combin (+ x r -1) (- r 1))
       (pow p r)
       (pow (- 1 p) x))))

;XXX;@networkdays(start_date,end_date,holidays)
;XXX;@Returns the number of non-weekend non-holidays between start_date and
; end_date. Holidays optionaly supplied in holidays.
;
; Returns error if start_date or end_date are invalid.
;XXX;@
;XXX;@workday
;XXX(define (networkdays start_date end_date holidays)
  ;XXX"Not implemented")

;@nominal(rate, nper)
;@NOMINAL calculates the nominal interest rate from a given effective
; rate.
;
; Nominal interest rate is given by a formula:
;
; nper * (( 1 + r ) ^ (1 / nper) - 1 )
;
; where:
;
; r = effective interest rate
;
; nper = number of periods used for compounding
;@
;@effect
(define (nominal rate nper)
  (if (or (< rate 0) (<= nper 0))
    nil
    (* nper (- (pow (+ 1 rate) (/ 1 nper)) 1))))

;@normdist(x,mean,stdev,cumulative)
;@normdist returns the normal cumulative distribution. x is
; the value for which you want the distribution, mean is the mean of
; the distribution, stdev is the standard deviation.
;
; If stdev is 0 normdist returns error. This function is Excel
; compatible.
;@normdist(2,1,2,0) equals 0.176032663.
;@poisson
(define (normdist x mean stdev cuml)
  (if (<= stdev 0)
    nil
    (if (= cuml 1)
      (pnorm x mean stdev)
      (dnorm x mean stdev))))

;@norminv(p,mean,stdev)
;@norminv returns the inverse of the normal cumulative
; distribution. p is the given probability corresponding to the normal
; distribution, mean is the arithmetic mean of the distribution, and
; stdev is the standard deviation of the distribution.
;
; If p &lt; 0 or p > 1 or stdev &lt;= 0 norminv returns error. This
; function is Excel compatible.
;@norminv(0.76,2,3) equals 4.118907689.
;@NORMDIST , NORMSDIST , NORMSINV , STANDARDIZE , ZTEST
(define (norminv p mean stdev)
  (if (or (< p 0) (> p 1) (<= stdev 0))
    nil
    (qnorm p mean stdev)))

;@normsdist(x)
;@normsdist returns the standard normal cumulative
; distribution. x is the value for which you want the distribution.
; Excel compatible.
;@normsdist(2) equals 0.977249868.
;@normdist
(define (normsdist x)
  (pnorm x 0 1))

;@normsinv(p)
;@normsinv returns the inverse of the standard normal
; cumulative distribution. p is the given probability corresponding to
; the normal distribution. Excel compatible.
;
; If p &lt; 0 or p > 1 normsinv returns error.
;@normsinv(0.2) equals -0.841621234.
;@NORMDIST , NORMINV , NORMSDIST , STANDARDIZE , ZTEST
(define (normsinv p)
  (if (or (< p 0) (> p 1))
    nil
    (qnorm p 0 1)))

;@not(number)
;@NOT implements the logical NOT function: the result is TRUE if the
; number is zero; otherwise the result is FALSE.
;
; Excel compatible.
; The name of this function is @NOT, to avoid clash with Scheme.
;@NOT(0) equals TRUE.
;
; NOT(TRUE) equals FALSE.
;@and or
; lives in 123.scm

;@now()
;@Returns the serial number for the date and time at the time it is
; evaluated.
;
; Serial Numbers in Siag are represented as seconds from 01/01/1970.
;@
;@today now
(define (now)
  (realtime))

;@nper(rate, pmt, pv, fv, type)
;@NPER calculates number of periods of an investment based on periodic
; constant payments and a constant interest rate. The interest rate per
; period is rate, pmt is the payment made each period, pv is the
; present value, fv is the future value and type is when the payments
; are due. If type = 1, payments are due at the begining of the period,
; if type = 0, payments are due at the end of the period.
;@For example, if you deposit $10,000 in a savings account that earns an
; interest rate of 6%. To calculate how many years it will take to
; double your investment use NPER as follows:
;
; =NPER(0.06, 0, -10000, 20000,0)returns 11.895661046 which indicates
; that you can double your money just before the end of the 12th year.
;@ppmt pv fv
(define (nper rate pmt pv fv type)
  (let* ((tmp2 (* pmt (+ 1 (* rate type))))
	 (tmp (/ (- tmp2 (* fv rate)) (+ (* pv rate) tmp2))))
    (if (or (<= rate 0) (<= tmp 0))
      nil
      (/ (log tmp) (log (+ 1 rate))))))

;XXX;@npv(rate,v1,v2,...)
;XXX;@npv calculates the net present value of an investment generating
; peridic payments. rate is the periodic interest rate and v1, v2,
; ... are the periodic payments. If the schedule of the cash flows are
; not periodic use the XNPV function.
;XXX;@npv(0.17,-10000,3340,2941,2493,3233,1732,2932) equals 186.30673.
;XXX;@pv xnpv
;XXX(define (npv . b)
  ;XXX"Not implemented")

;;; This is SO BORING!

;@oct2bin(number[,places])
;@The OCT2BIN function converts an octal number to a binary number.
; places is an optional field, specifying to zero pad to that number of
; spaces. Excel compatible.
;
; If places is too small or negative error is returned.
;@OCT2BIN("213") equals 10001011.
;@bin2oct oct2dec oct2hex
(define (oct2bin n . p)
  (num2str (str2num n 8) 2 (car p)))

;@oct2dec(x)
;@OCT2DEC converts an octal number in a string or number to its
; decimal equivalent. Excel compatible.
;@OCT2DEC("124") equals 84.
;@DEC2OCT , OCT2BIN , OCT2HEX
(define (oct2dec x)
  (str2num x 8))

;@oct2hex(number[,places])
;@The OCT2HEX function converts an octal number to a hexadecimal number.
; places is an optional field, specifying to zero pad to that number of
; spaces. Excel compatible.
;
; If places is too small or negative error is returned.
;@OCT2HEX(132) equals 5A.
;@hex2oct oct2bin oct2dec
(define (oct2hex n . p)
  (num2str (str2num n 8) 16 (car p)))

;@odd(number)
;@ODD returns the number rounded up to the nearest odd
; integer. Excel compatible.
;@ODD(4.4) equals 5.
;@even
(define (odd n)
  (let* ((sign (if (< n 0) -1 1))
	 (number (* sign n))
	 (ceiled (ceil number)))
    (if (= (fmod ceiled 2) 1)
      (if (> number ceiled)
	(* sign (+ ceiled 2))
	(* sign ceiled))
      (* sign (+ ceiled 1)))))

;;;@oddfprice(settlement,maturity,issue,first_coupon,rate,yld,redemption,frequency, basis)
;;;@
;;;@
;;;@
;;(define (oddfprice settlement maturity issue first_c rate yld red freq)
;;  "Not implemented")

;;;@oddfyield(settlement,maturity,issue,first_coupon,rate,pr,redemption,frequency,basis)
;;;@
;;;@
;;;@
;;(define (oddfyield settlement maturity issue first_c rate pr red freq)
;;  "Not implemented")

;;;@oddlprice(settlement,maturity,last_interest,rate,yld,redemption,frequency,basis)
;;;@
;;;@
;;;@
;;(define (oddlprice settlement maturity last_int rate yld red freq basis)
;;  "Not implemented")

;;;@oddlyield(settlement,maturity,last_interest,rate,pr,redemption,frequency,basis)
;;;@
;;;@
;;;@
;;(define (oddlyield settlement maturity last_int rate pr red freq basis)
;;  "Not implemented")

;XXX;@offset(range,row,col,height,width)
;XXX;@The OFFSET function returns a cell range. The cell range starts at
; offset (col,row) from range, and is of height height and width
; width.
;
; If range is neither a reference nor a range returns error. If either
; height or width is omitted the height or width of the reference is
; used.
;XXX;@
;XXX;@column columns rows
;XXX(define (offset range row col height width)
  ;XXX"Not implemented")

;@or(b1, b2, ...)
;@OR implements the logical OR function: the result is TRUE if any of
; the values evaluated to TRUE.
;
; b1, trough bN are expressions that should evaluate to TRUE or FALSE.
; If an integer or floating point value is provided zero is considered
; FALSE and anything else is TRUE.
;
; If the values contain strings or empty cells those values are ignored.
; If no logical values are provided, then an error is returned.
; Excel compatible. The name of the function is @OR.
;@OR(TRUE,FALSE) equals TRUE.
;
; OR(3>4,4&lt;3) equals FALSE.
;@and not
; lives in 123.scm

;XXX;@pearson(array1,array2)
;XXX;@PEARSON returns the Pearson correllation coefficient of two data sets.
;
; Strings and empty cells are simply ignored. This function is Excel
; compatible.
;XXX;@
;XXX;@intercept linest rsq slope steyx
;XXX(define (pearson . b)
;XXX  (apply correl b))

;XXX;@percentile(array,k)
;XXX;@PERCENTILE returns the k-th percentile of the given data
; points.
;
; If array is empty, PERCENTILE returns error. If k &lt; 0 or k >
; 1, PERCENTILE returns error. Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; PERCENTILE(A1..A5,0.42) equals 20.02.
;XXX;@quartile
;XXX(define (percentile array k)
  ;XXX"Not implemented")

;XXX;@percentrank(array,x[,significance])
;XXX;@PERCENTRANK returns the rank of a data point in a data set.
; array is the range of numeric values, x is the data point which you
; want to rank, and the optional significance indentifies the number of
; significant digits for the returned value. If significance is
; omitted, PERCENTRANK uses three digits.
;
; If array contains not data points, PERCENTRANK returns error.
; If significance is less than one, PERCENTRANK returns error. If
; x does not match any of the values in array or x matches more than
; once, PERCENTRANK interpolates the returned value.
;XXX;@
;XXX;@LARGE , MAX , MEDIAN , MIN , PERCENTILE , QUARTILE , SMALL
;XXX(define (percentrank array x . b)
  ;XXX"Not implemented")

;@permut(n,k)
;@PERMUT returns the number of permutations. n is the number
; of objects, k is the number of objects in each permutation.
;
; If n = 0 PERMUT returns error. If n &lt; k PERMUT returns
; error. Excel compatible.
;@PERMUT(7,3) equals 210.
;@combin
(define (permut n k)
  (if (and (<= 0 k) (<= k n))
    (/ (fact n) (fact (- n k)))
    nil))

;@pi()
;@PI functions returns the value of Pi.
;
; This function is called with no arguments. Excel
; compatible.
;@PI() equals 3.141593.
;@sqrtpi
(define (pi)
  *pi*)

;@pmt(rate,nper,pv[,fv,type])
;@XXX: Below is a PV function description!PMT calculates the present
; value of an investment.
;@
;@ppmt pv fv
(define (pmt rate nper pv . b)
  (let ((fv (if (car b) (car b) 0))
	(type (if (car b) (car b) 0)))
    (calculate_pmt rate nper pv fv type)))

;@poisson(x,mean,cumulative)
;@poisson returns the Poisson distribution. x is the number of
; events, mean is the expected numeric value cumulative describes
; whether to return the sum of the poisson function from 0 to x.
;
; If x is a non-integer it is truncated. If x &lt;= 0 poisson returns
; error. If mean &lt;= 0 poisson returns the error.
; Excel compatible.
;@poisson(3,6,0) equals 0.089235078.
;@normdist weibull
(define (poisson x mean cuml)
  (if (or (<= x 0) (<= mean 0))
    nil
    (if (= cuml 1)
      (ppois x mean)
      (dpois x mean))))

;@power(x,y)
;@POWER returns the value of x raised to the power y.
; Excel compatible.
;@POWER(2,7) equals 128.
;
; POWER(3,3.141) equals 31.523749.
;@exp
(define power pow)

;@ppmt(rate,per,nper,pv[,fv,type])
;@PPMT calculates the amount of a payment of an annuity going towards
; principal.
;
; Formula for it is:
;
; PPMT(per) = PMT - IPMT(per)
;
; where:
;
; PMT = Payment received on annuity
;
; IPMT(per) = amount of interest for period per
;@
;@ipmt pv fv
(define (ppmt rate per nper pv . b)
  (let* ((fv (if (car b) (car b) 0))
	 (type (if (cadr b) (cadr b) 0))
	 (pmt (calculate_pmt rate nper pv fv type))
	 (ipmt (- (* (calculate_principal pv pmt rate (- per 1)) rate))))
    (- pmt ipmt)))

;XXX;@price(settle,mat,rate,yield,redemption_price,frequency,basis)
;XXX;@
;XXX;@
;XXX;@
;XXX(define (price settle mat rate yield redemption_price frequency basis)
  ;XXX"Not implemented")

;XXX;@pricedisc(settlement,maturity,discount,redemption[,basis])
;XXX;@PRICEDISC calculates and returns the price per $100 face value of a
; security bond. The security does not pay interest at maturity.
; discount is the rate for which the security is discounted.
; redemption is the amount to be received on maturity date. basis is
; the type of day counting system you want to use:
;
; 0 US 30/360
;
; 1 actual days/actual days
;
; 2 actual days/360
;
; 3 actual days/365
;
; 4 European 30/360
;
; If basis is omitted, US 30/360 is applied. If settlement date or
; maturity date is not valid, PRICEDISC returns error. If basis
; &lt; 0 or basis > 4, PRICEDISC returns error. If settlement date
; is after maturity date or they are the same, PRICEDISC returns
; error.
;XXX;@
;XXX;@pricemat
(define (pricedisc settlement maturity discount redemption . b)
  (let* ((basis (if (car b) (car b) 0))
	 (a (days_monthly_basis settlement maturity basis))
	 (d (annual_year_basis settlement basis)))
    (if (or (<= a 0) (<= d 0) (< basis 0) (> basis 4))
      nil
      (- redemption (/ (* discount redemption a) d)))))

;XXX;@pricemat(settlement,maturity,issue,rate,yield[,basis])
;XXX;@PRICEMAT calculates and returns the price per $100 face value of a
; security. The security pays interest at maturity. basis is the type
; of day counting system you want to use:
;
; 0 US 30/360
;
; 1 actual days/actual days
;
; 2 actual days/360
;
; 3 actual days/365
;
; 4 European 30/360
;
; If basis is omitted, US 30/360 is applied. If settlement date or
; maturity date is not valid, PRICEMAT returns error. If basis &lt;
; 0 or basis > 4, PRICEMAT returns error. If settlement date is
; after maturity date or they are the same, PRICEMAT returns
; error.
;XXX;@
;XXX;@pricedisc
(define (pricedisc settlement maturity issue rate yield . d)
  (let* ((basis (if (car d) (car d) 0))
	 (dsm (days_monthly_basis settlement maturity basis))
	 (dim (days_monthly_basis issue maturity basis))
	 (a (days_monthly_basis issue settlement basis))
	 (b (annual_year_basis settlement basis))
	 (n (+ 1 (* (/ dsm b) yield))))
    (if (or (<= a 0) (<= b 0) (<= dsm 0) (<= dim 0) (< basis 0) (> basis 4) (= n 0))
      nil
      (- (/ (+ 100 (* (/ dim b) discount 100)) n)
	 (* (/ a b) discount 100)))))

;XXX;@prob(range_x,prob_range,lower_limit[,upper_limit])
;XXX;@PROB returns the probability that values in a range or an
; array are between two limits. If upper_limit is not given, PROB
; returns the probability that values in x_range are equal to
; lower_limit.
;
; If the sum of the probabilities in prob_range is not equal to 1 PROB
; returns error. If any value in prob_range is &lt;=0 or > 1, PROB
; returns error. If x_range and prob_range contain a different
; number of data entries, PROB returns error. This function is
; Excel compatible.
;XXX;@
;XXX;@binomdist critbinom
;XXX(define (prob range_x prob_range lower_limit . b)
  ;XXX(let ((upper_limit (if (car b) (car b) lower_limit)))
    ;XXX"Not implemented"))

;@product(value1, value2, ...)
;@PRODUCT returns the product of all the values and cells referenced in
; the argument list. Excel compatible. In particular,
; this means that if all cells are empty, the result will be 0.
;@PRODUCT(2,5,9) equals 90.
;@sum count g_product
(define (product . b)
  (let* ((p (collect_args b (lambda x (number? x))))
	 (s (if (null? p) 0 1)))
    (while p
      (set! s (* (car p) s))
      (set! p (cdr p)))
    s))

;XXX;@proper(string)
;XXX;@PROPER returns string with initial of each word capitalised.
;XXX;@PROPER("j. f. kennedy") equals "J. F. Kennedy".
;XXX;@lower upper
;XXX(define (proper string)
  ;XXX"Not implemented")

;@pv(rate,nper,pmt,fv,type)
;@pv calculates the present value of an investment. rate is the
; periodic interest rate, nper is the number of periods used for
; compounding. pmt is the payment made each period, fv is the future
; value and type is when the payment is made. If type = 1 then the
; payment is made at the begining of the period. If type = 0 it is made
; at the end of each period.
;@
;@fv
(define (pv rate nper pmt fv type)
  (let* ((pvif (calculate_pvif rate nper))
	 (fvifa (calculate_fvifa rate nper)))
    (if (or (<= rate 0) (= pvif 0))
      nil
      (/ (- (- fv) (* pmt (+ 1 (* rate type)) fvifa)) pvif))))

;XXX;@quartile(array,quart)
;XXX;@QUARTILE returns the quartile of the given data points.
;
; If quart is equal to: QUARTILE returns:
;
; 0 the smallest value of array.
;
; 1 the first quartile
;
; 2 the second quartile
;
; 3 the third quartile
;
; 4 the largest value of array.
;
; If array is empty, QUARTILE returns error. If quart &lt; 0 or
; quart > 4, QUARTILE returns error. If quart is not an integer,
; it is truncated. Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; QUARTILE(A1..A5,1) equals 17.3.
;XXX;@LARGE , MAX , MEDIAN , MIN , PERCENTILE , SMALL
;XXX(define (quartile array quart)
  ;XXX"Not implemented")

;@quotient(num,den)
;@QUOTIENT returns the integer portion of a division. num is
; the divided and den is the divisor. This function is Excel
; compatible.
;@QUOTIENT(23,5) equals 4.
;@mod
(define (quotient num den)
  (floor (/ num den)))

;@radians(x)
;@RADIANS computes the number of radians equivalent to x degrees. This
; function is Excel compatible.
;@RADIANS(180) equals 3.14159.
;@pi degrees
(define (radians x)
  (* *pi* (/ x 180)))

;@randbetween(bottom,top)
;@RANDBETWEEN returns a random integer number between bottom
; and top.
;
; If bottom or top is non-integer, they are truncated. If bottom >
; top, RANDBETWEEN returns error. Excel compatible.
;@RANDBETWEEN(3,7).
;@rand
(define (randbetween bot tp)
  (let ((bottom (floor bot))
	(top (floor tp)))
    (+ bottom (floor (* (+ top 1 (- bottom)) (random_01))))))

;@randnegbinom(p,failures)
;@RANDNEGBINOM returns a negitive binomialy distributed random number.
;
; If p &lt; 0 or p > 1, RANDNEGBINOM returns error. If failures
; RANDNEGBINOM returns error.
;@RANDNEGBINOM(0.5,2).
;@rand randbetween
(define (randnegbinom p failures)
  (if (or (< p 0) (> p 1) (< failures 0))
    nil
    (random_negbinom p failures)))

;XXX;@rank(x,ref[,order])
;XXX;@RANK returns the rank of a number in a list of numbers. x is the
; number whose rank you want to find, ref is the list of numbers, and
; order specifies how to rank numbers. If order is 0, numbers are
; ranked in descending order, otherwise numbers are ranked in ascending
; order. Excel compatible.
;XXX;@
;XXX;@percentrank
;XXX(define (rank x ref . b)
  ;XXX"Not implemented")

;XXX;@rate(nper,pmt,pv[,fv,type,guess])
;XXX;@RATE calculates rate of an investment.
;XXX;@
;XXX;@pv fv
;XXX(define (rate nper pmt pv . b)
  ;XXX"Not implemented")

;XXX;@received(settlement,maturity,investment,rate[,basis])
;XXX;@RECEIVED calculates and returns the amount to be received at maturity
; date for a security bond. basis is the type of day counting system
; you want to use:
;
; 0 US 30/360
;
; 1 actual days/actual days
;
; 2 actual days/360
;
; 3 actual days/365
;
; 4 European 30/360
;
; If basis is omitted, US 30/360 is applied. If settlement date or
; maturity date is not valid, RECEIVED returns error. If basis &lt;
; 0 or basis > 4, RECEIVED returns error. If settlement date is
; after maturity date or they are the same, RECEIVED returns
; error.
;XXX;@
;XXX;@intrate
(define (received settlement maturity investment rate . b)
  (let* ((basis (if (car b) (car b) 0))
	 (a (days_monthly_basis settlement maturity basis))
	 (d (annual_year_basis settlement basis))
	 (n (- 1 (* discount (/ a d)))))
    (if (or (<= a 0) (<= d 0) (< basis 0) (> basis 4))
      nil
      (/ investment n))))

;XXX;@replace(old,start,num,new)
;XXX;@REPLACE returns old with new replacing num characters from start.
;XXX;@REPLACE("testing",2,3,"*****") equals "t*****ing".
;XXX;@MID , SEARCH , SUBSTITUTE , TRIM
;XXX(define (replace old start num new)
  ;XXX"Not implemented")

;@rept(string,num)
;@REPT returns num repetitions of string.
;@REPT(".",3) equals "...".
;@concatenate
(define (rept string num)
  (if (<= num 0) "" (string-append string (rept string (- num 1)))))

;XXX;@right(text[,num_chars])
;XXX;@RIGHT returns the rightmost num_chars characters or the right
; character if num_chars is not specified.
;XXX;@RIGHT("end") equals "d".
;
; RIGHT("end",2) equals "nd".
;XXX;@mid left
;XXX(define (right text . b)
  ;XXX"Not implemented; oboe")

;XXX;@roman(number[,type])
;XXX;@ROMAN returns an arabic number in the roman numeral style, as
; text. number is the number you want to convert and type is the type
; of roman numeral you want.
;
; If type is 0 or it is omitted, ROMAN returns classic roman numbers.
; Type 1 is more concise than classic type, type 2 is more concise than
; type 1, and type 3 is more concise than type 2. Type 4 is simplified
; type.
;
; If number is negative or greater than 3999, ROMAN returns
; error. Excel compatible.
;XXX;@ROMAN(999) equals CMXCIX.
;
; ROMAN(999,1) equals LMVLIV.
;
; ROMAN(999,2) equals XMIX.
;
; ROMAN(999,3) equals VMIV.
;
; ROMAN(999,4) equals IM.
;XXX;@
;XXX(define (roman number . b)
  ;XXX(let ((type (if (car b) (car b) 0)))
    ;XXX"Not implemented; write in C"))

;@round(number[,digits])
;@ROUND rounds a given number. number is the number you want
; rounded and digits is the number of digits to which you want to round
; that number.
;
; If digits is greater than zero, number is rounded to the given
; number of digits. If digits is zero or omitted, number is rounded to
; the nearest integer. If digits is less than zero, number is rounded
; to the left of the decimal point. Excel compatible.
;@ROUND(5.5) equals 6.
;
; ROUND(-3.3) equals -3.
;
; ROUND(1501.15,1) equals 1501.2.
;
; ROUND(1501.15,-2) equals 1500.0.
;@rounddown roundup
(define (round number . b)
  (let* ((digits (if (car b) (car b) 0))
	 (p10 (pow_10 digits)))
    (/ (floor (+ (* number p10) (if (>= number 0) 0.5 -0.5))) p10)))

;@rounddown(number[,digits])
;@ROUNDDOWN rounds a given number down, towards zero. number
; is the number you want rounded down and digits is the number of
; digits to which you want to round that number.
;
; If digits is greater than zero, number is rounded down to the given
; number of digits. If digits is zero or omitted, number is rounded
; down to the nearest integer. If digits is less than zero, number is
; rounded down to the left of the decimal point. This function is Excel
; compatible.
;@ROUNDDOWN(5.5) equals 5.
;
; ROUNDDOWN(-3.3) equals -4.
;
; ROUNDDOWN(1501.15,1) equals 1501.1.
;
; ROUNDDOWN(1501.15,-2) equals 1500.0.
;@round roundup
(define (rounddown number . b)
  (let* ((digits (if (car b) (car b) 0))
	 (p10 (pow_10 digits)))
    (if (< number 0)
      (- (/ (ceil (* (- number) p10)) p10))
      (/ (floor (* number p10)) p10))))

;@roundup(number[,digits])
;@ROUNDUP rounds a given number up, away from zero. number is
; the number you want rounded up and digits is the number of digits to
; which you want to round that number.
;
; If digits is greater than zero, number is rounded up to the given
; number of digits. If digits is zero or omitted, number is rounded up
; to the nearest integer. If digits is less than zero, number is
; rounded up to the left of the decimal point. This function is Excel
; compatible.
;@ROUNDUP(5.5) equals 6.
;
; ROUNDUP(-3.3) equals -3.
;
; ROUNDUP(1501.15,1) equals 1501.2.
;
; ROUNDUP(1501.15,-2) equals 1600.0.
;@round rounddown
(define (roundup number . b)
  (let* ((digits (if (car b) (car b) 0))
	 (p10 (pow_10 digits)))
    (if (< number 0)
      (- (/ (floor (* (- number) p10)) p10))
      (/ (ceil (* number p10)) p10))))


;XXX;@row([reference])
;XXX;@The ROW returns an array of the row numbers taking a default
; argument of the containing cell position.
;
; If reference is neither an array nor a reference nor a range returns
; error.
; This function name clashes with the existing row function in Siag.
; Not implemented.
;XXX;@
;XXX;@column columns rows

;@rows(range)
;@The ROWS function returns the number of rows in area or array
; reference.
;
; If reference is not an array nor a range returns
; error.
;@
;@column row rows
(define (rows dummy r1 c1 r2 c2)
  (if (eq? dummy 'RANGE)
    (- r2 r1 -1)))


;XXX;@rsq(array1,array2)
;XXX;@RSQ returns the square of the Pearson correllation coefficient of two
; data sets.
;
; Strings and empty cells are simply ignored. This function is Excel
; compatible.
;XXX;@
;XXX;@CORREL COVAR INTERCEPT LINEST LOGEST PEARSON SLOPE STEYX TREND
(define (rsq ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  (range_rsq (collect_range ar1 ac1 ar2 ac2 number?)
	     (collect_range br1 bc1 br2 bc2 number?)))

;@randbernoulli(p)
;@RandBernoulli returns a Bernoulli distributed random number.
;
; If p &lt; 0 or p > 1 RandBernoulli returns error.
;@RandBernoulli(0.5).
;@rand randbetween
(define (randbernoulli p)
  (if (or (< p 0) (> p 1))
    nil
    (random_bernoulli p)))

;@randbinom(p,trials)
;@RandBinom returns a binomialy distributed random number.
;
; If p &lt; 0 or p > 1 RandBinom returns error. If trials &lt; 0
; RandBinom returns error.
;@RandBinom(0.5,2).
;@rand randbetween
(define (randbinom p trials)
  (if (or (< p 0) (> p 1) (< trials 0))
    nil
    (random_binomial p trials)))

;@randexp(b)
;@RandExp returns a exponentially distributed random number.
;@RandExp(0.5).
;@rand randbetween
(define (randexp x)
  (random_exponential x))

;@randpoisson(lambda)
;@RandPoisson returns a poisson distributed random number.
;@RandPoisson(3).
;@rand randbetween
(define (randpoisson x)
  (if (< x 0)
    nil
    (random_poisson x)))

;XXX;@search(text,within[,start_num])
;XXX;@SEARCH returns the location of a character or text string within
; another string. text is the string or character to be searched.
; within is the string in which you want to search. start_num is the
; start position of the search in within. If start_num is omitted, it
; is assumed to be one. The search is not case sensitive.
;
; text can contain wildcard characters (*) and question marks (?) to
; control the search. A question mark matches with any character and
; wildcard matches with any string including empty string. If you want
; the actual wildcard or question mark to be searched, use tilde (~)
; before the character.
;
; If text is not found, SEARCH returns error. If start_num is
; less than one or it is greater than the length of within, SEARCH
; returns error.
;XXX;@SEARCH("c","Cancel") equals 1.
;
; SEARCH("c","Cancel",2) equals 4.
;XXX;@find
;XXX(define (search text within . b)
  ;XXX"Not implemented")

;@second(serial_number)
;@Converts a serial number to a second. The second is returned as an
; integer in the range 0 to 59.
;@
;@hour minute now time
(define (second serial_number)
  (cdr (assoc 'sec (localtime serial_number))))

;XXX;@selection(permit_intersection)
;XXX;@The SELECTION returns a list with the values in the current
; selection. This is usually used to implement on-the-fly computation of
; values. If permit_intersection is TRUE the user specifed selection
; ranges are returned, EVEN IF THEY OVERLAP. If permit_intersection is
; FALSE a distict set of regions is returned, however, there may be more
; of them than the user initially specified.
;XXX;@
;XXX;@
;XXX(define (selection permit_intersect)
  ;XXX"Not implemented")

;XXX;@seriessum(x,n,m,coefficients)
;XXX;@SERIESSUM returns the sum of a power series. x is the base
; of the power serie, n is the initial power to raise x, m is the
; increment to the power for each term in the series, and coefficients
; is the coefficents by which each successive power of x is multiplied.
; Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 1.23,
; 2.32, 2.98, 3.42, and 4.33. Then
;
; SERIESSUM(3,1,2.23,A1..A5) equals 251416.43018.
;XXX;@count sum
;XXX(define (seriessum x n m coefficients)
  ;XXX"Not implemented")

;@sign(number)
;@SIGN returns 1 if the number is positive, zero if the
; number is 0, and -1 if the number is negative. This function is
; Excel compatible.
;@SIGN(3) equals 1.
;
; SIGN(-3) equals -1.
;
; SIGN(0) equals 0.
;@
(define (sign x)
  (if (< x 0) -1 1))

;@skew(n1, n2, ...)
;@SKEW returns an unbiased estimate for skewness of a distribution.
;
; Note, that this is only meaningful is the underlying distribution
; really has a third moment. The skewness of a symmetric (e.g., normal)
; distribution is zero.
;
; Strings and empty cells are simply ignored.
;
; If less than three numbers are given, SKEW returns error. This
; function is Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; SKEW(A1..A5) equals 0.976798268.
;@AVERAGE , VAR , SKEWP , KURT
(define (skew . b)
  (range_skew_est (collect_args b (lambda x (number? x)))))

;@skewp(n1, n2, ...)
;@SKEWP returns the population skewness of a data set.
;
; Strings and empty cells are simply ignored.
;
; If less than two numbers are given, SKEWP returns error.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; SKEWP(A1..A5) equals 0.655256198.
;@AVERAGE , VARP , SKEW , KURTP
(define (skewp . b)
  (range_skew_pop (collect_args b (lambda x (number? x)))))

;@sln(cost,salvage_value,life)
;@The SLN function will determine the straight line depreciation of an
; asset for a single period. The amount you paid for the asset is the
; cost, salvage is the value of the asset at the end of its useful
; life, and life is the number of periods over which an the asset is
; depreciated. This method of deprecition devides the cost evenly over
; the life of an asset.
;
; The formula used for straight line depriciation is:
;
; Depriciation expense = ( cost - salvage_value ) / life
;
; cost = cost of an asset when acquired (market value). salvage_value
; = amount you get when asset sold at the end of the assets's useful
; life. life = anticipated life of an asset.
;@For example, lets suppose your company purchases a new machine for
; $10,000, which has a salvage value of $700 and will have a useful life
; of 10 years. The SLN yearly depreciation is computed as follows:
;
; =SLN(10000, 700, 10)
;
; This will return the yearly depreciation figure of $930.
;@syd
(define (sln cost salvage_value life)
  (if (<= life 0)
    nil
    (/ (- cost salvage_value) life)))

;XXX;@slope(known_y's,known_x's)
;XXX;@SLOPE returns the slope of the linear regression line. This function
; is Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; SLOPE(A1..A5,B1..B5) equals 1.417959936.
;XXX;@stdev stdevpa
(define (slope ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  (range_slope (collect_range ar1 ac1 ar2 ac2 number?)
	       (collect_range br1 bc1 br2 bc2 number?)))

;@small(n1, n2, ..., k)
;@SMALL returns the k-th smallest value in a data set.
;
; If data set is empty SMALL returns error. If k &lt;= 0 or k is
; greater than the number of data items given SMALL returns error.
; Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; SMALL(A1..A5,2) equals 17.3.
;
; SMALL(A1..A5,4) equals 25.9.
;@PERCENTILE , PERCENTRANK , QUARTILE , LARGE
(define (small . b)
  (let* ((p (collect_args b (lambda x (number? x))))
	 (r (reverse p))
	 (n (car r))
	 (s (qsort (cdr r) <)))
    (if (>= n 1)
      (nth (- n 1) s))))


;@sqrtpi(number)
;@SQRTPI returns the square root of a number multiplied by pi.
; Excel compatible.
;@SQRTPI(2) equals 2.506628275.
;@pi
(define (sqrtpi number)
  (sqrt (* number *pi*)))

;@standardize(x, mean, stddev)
;@STANDARDIZE returns a normalized value. x is the number to
; be normalized, mean is the mean of the distribution, stddev is the
; standard deviation of the distribution.
;
; If stddev is 0 STANDARDIZE returns error. This function is
; Excel compatible.
;@STANDARDIZE(3,2,4) equals 0.25.
;@average
(define (standardize x mean stddev)
  (if (<= stddev 0)
    nil
    (/ (- x mean) stddev)))

;@stdev(b1, b2, ...)
;@STDEV returns standard deviation of a set of numbers treating these
; numbers as members of a population. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; STDEV(A1..A5) equals 10.84619749.
;@AVERAGE , DSTDEV , DSTDEVP , STDEVA , STDEVPA , VAR
(define (stdev . b)
  (range_stddev_est (collect_args b (lambda x (number? x)))))

;@stdeva(number1,number2,...)
;@STDEVA returns the standard deviation based on a sample. Numbers, text
; and logical values are included in the calculation too. If the cell
; contains text or the argument evaluates to FALSE, it is counted as
; value zero (0). If the argument evaluates to TRUE, it is counted as
; one (1). Note that empty cells are not counted. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, 17.3, "missing", 25.9, and 40.1. Then
;
; STDEVA(A1..A5) equals 15.119953704.
;@STDEV , STDEVPA
(define (stdeva . b)
  (range_stddev_est (collect_args b (lambda x (number? x)))))

;@stdevp(b1, b2, ...)
;@STDEVP returns standard deviation of a set of numbers treating these
; numbers as members of a complete population. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; STDEVP(A1..A5) equals 9.701133954.
;@STDEV , STDEVA , STDEVPA
(define (stdevp . b)
  (range_stddev_pop (collect_args b (lambda x (number? x)))))

;@stdevpa(number1,number2,...)
;@STDEVPA returns the standard deviation based on the entire population.
; Numbers, text and logical values are included in the calculation too.
; If the cell contains text or the argument evaluates to FALSE, it is
; counted as value zero (0). If the argument evaluates to TRUE, it is
; counted as one (1). Note that empty cells are not counted. This
; function is Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, 17.3, "missing", 25.9, and 40.1. Then
;
; STDEVPA(A1..A5) equals 13.523697719.
;@STDEVA , STDEVP
(define (stdevpa . b)
  (range_stddev_pop (collect_args b (lambda x (number? x)))))

;XXX;@steyx(known_y's,known_x's)
;XXX;@STEYX returns the standard error of the predicted y-value for
; each x in the regression.
;
; If known_y's and known_x's are empty or have a different number of
; arguments then STEYX returns error. This function is Excel
; compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; STEYX(A1..A5,B1..B5) equals 1.101509979.
;XXX;@pearson rsq slope
;XXX(define (steyx known_y known_x)
  ;XXX"Not implemented")

;XXX;@substitute(text, old, new [,num])
;XXX;@SUBSTITUTE replaces old with new in text. Substitutions are only
; applied to instance num of old in text, otherwise every one is
; changed.
;XXX;@SUBSTITUTE("testing","test","wait") equals "waiting".
;XXX;@replace trim
;XXX(define (substitute text old new . b)
  ;XXX"Not implemented")

;XXX;@subtotal(function_nbr,ref1,ref2,...)
;XXX;@SUBTOTAL returns a subtotal of given list of arguments.
; function_nbr is the number that specifies which function to use in
; calculating the subtotal. The following functions are available:
;
; 1 AVERAGE
;
; 2 COUNT
;
; 3 COUNTA
;
; 4 MAX
;
; 5 MIN
;
; 6 PRODUCT
;
; 7 STDEV
;
; 8 STDEVP
;
; 9 SUM
;
; 10 VAR
;
; 11 VARP
;
; Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 23, 27,
; 28, 33, and 39. Then
;
; SUBTOTAL(1,A1..A5) equals 30.
;
; SUBTOTAL(6,A1..A5) equals 22378356.
;
; SUBTOTAL(7,A1..A5) equals 6.164414003.
;
; SUBTOTAL(9,A1..A5) equals 150.
;
; SUBTOTAL(11,A1..A5) equals 30.4.
;XXX;@ count sum
;XXX(define (subtotal function_nbr . b)
  ;XXX"Not implemented")

;@sum(value1, value2, ...)
;@SUM computes the sum of all the values and cells referenced in the
; argument list. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11, 15,
; 17, 21, and 43. Then
;
; SUM(A1..A5) equals 107.
;@AVERAGE , COUNT
(define (sum . b)
  (let* ((p (collect_args b (lambda x (number? x))))
	 (s 0))
    (while p
      (set! s (+ (car p) s))
      (set! p (cdr p)))
    s))

;@suma(value1, value2, ...)
;@SUMA computes the sum of all the values and cells referenced in the
; argument list. Numbers, text and logical values are included in the
; calculation too. If the cell contains text or the argument evaluates
; to FALSE, it is counted as value zero (0). If the argument evaluates
; to TRUE, it is counted as one (1). Since logical values are numbers
; in Siag, this function is identical to sum.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11, 15,
; 17, 21, and 43. Then
;
; SUMA(A1..A5) equals 107.
;@AVERAGE , SUM , COUNT
(define (suma . b)
  (let* ((p (collect_args b (lambda x (number? x))))
	 (s 0))
    (while p
      (set! s (+ (car p) s))
      (set! p (cdr p)))
    s))

;@sumif(range,criteria[,actual_range])
;@SUMIF sums the values in the given range that meet the given
; criteria. If actual_range is given, SUMIF sums the values in the
; actual_range whose corresponding components in range meet the given
; criteria. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 23, 27,
; 28, 33, and 39. Then
;
; SUMIF(A1..A5,"&lt;=28") equals 78.
;
; SUMIF(A1..A5,"&lt;28") equals 50.
;
; In addition, if the cells B1, B2, ..., B5 hold numbers 5, 3, 2, 6, and
; 7 then:
;
; SUMIF(A1..A5,"&lt;=27",B1..B5) equals 8.
;@countif sum
(define (sumif dummy r1 c1 r2 c2 criteria . b)
  (let* ((p (collect_range r1 c1 r2 c2 (lambda x (number? x))))
	 (q (if b
	      (collect_range (cadr b)
			     (caddr b)
			     (car (cdddr b))
			     (cadr (cdddr b))
			     (lambda x (number? x)))
	      p))
	 (n 0))
    (while p
      (if (check_crit (car p) criteria)
	(set! n (+ n (car q))))
      (set! q (cdr q))
      (set! p (cdr p)))
    n))

;@sumproduct(range1,range2,...)
;@SUMPRODUCT multiplies corresponding data entries in the given
; arrays or ranges, and then returns the sum of those products. If an
; array entry is not numeric, the value zero is used instead.
;
; If arrays or range arguments do not have the same dimentions,
; SUMPRODUCT returns error. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11, 15,
; 17, 21, and 43 and the cells B1, B2, ..., B5 hold numbers 13, 22, 31,
; 33, and 39. Then
;
; SUMPRODUCT(A1..A5,B1..B5) equals 3370.
;@sum product
(define (sumproduct ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  (let ((a (collect_range ar1 ac1 ar2 ac2 (lambda x t)))
	(b (collect_range br1 bc1 br2 bc2 (lambda x t)))
	(s 0))
    (while a
      (if (and (number? (car a)) (number? (car b)))
	(set! s (+ s (* (car a) (car b)))))
      (set! a (cdr a))
      (set! b (cdr b)))
    s))

;@sumsq(value1, value2, ...)
;@SUMSQ returns the sum of the squares of all the values and cells
; referenced in the argument list. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11, 15,
; 17, 21, and 43. Then
;
; SUMSQ(A1..A5) equals 2925.
;@sum count
(define (sumsq . b)
  (let ((p (collect_args b (lambda x (number? x))))
	(s 0))
    (while p
      (set! s (+ s (* (car p) (car p))))
      (set! p (cdr p)))
    s))

;@sumx2my2(array1,array2)
;@SUMX2MY2 returns the sum of the difference of squares of
; corresponding values in two arrays. array1 is the first array or
; range of data points and array2 is the second array or range of data
; points. The equation of SUMX2MY2 is SUM (x^2-y^2).
;
; Strings and empty cells are simply ignored.
;
; If array1 and array2 have different number of data points, SUMX2MY2
; returns error. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11, 15,
; 17, 21, and 43 and the cells B1, B2, ..., B5 hold numbers 13, 22, 31,
; 33, and 39. Then
;
; SUMX2MY2(A1..A5,B1..B5) equals -1299.
;@SUMSQ , SUMX2PY2
(define (sumx2my2 ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  (let ((a (collect_range ar1 ac1 ar2 ac2 (lambda x t)))
	(b (collect_range br1 bc1 br2 bc2 (lambda x t)))
	(s 0))
    (while a
      (if (and (number? (car a)) (number? (car b)))
	(set! s (+ s (- (* (car a) (car a)) (* (car b) (car b))))))
      (set! a (cdr a))
      (set! b (cdr b)))
    s))

;@sumx2py2(array1,array2)
;@SUMX2PY2 returns the sum of the sum of squares of
; corresponding values in two arrays. array1 is the first array or
; range of data points and array2 is the second array or range of data
; points. The equation of SUMX2PY2 is SUM (x^2+y^2).
;
; Strings and empty cells are simply ignored.
;
; If array1 and array2 have different number of data points, SUMX2PY2
; returns error. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11, 15,
; 17, 21, and 43 and the cells B1, B2, ..., B5 hold numbers 13, 22, 31,
; 33, and 39. Then
;
; SUMX2PY2(A1..A5,B1..B5) equals 7149.
;@SUMSQ , SUMX2MY2
(define (sumx2py2 ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  (let ((a (collect_range ar1 ac1 ar2 ac2 (lambda x t)))
	(b (collect_range br1 bc1 br2 bc2 (lambda x t)))
	(s 0))
    (while a
      (if (and (number? (car a)) (number? (car b)))
	(set! s (+ s (+ (* (car a) (car a)) (* (car b) (car b))))))
      (set! a (cdr a))
      (set! b (cdr b)))
    s))


;@sumxmy_2(array1,array2)
;@SUMXMY_2 returns the sum of squares of differences of
; corresponding values in two arrays. array1 is the first array or
; range of data points and array2 is the second array or range of data
; points. The equation of SUMXMY_2 is SUM (x-y)^2.
;
; Strings and empty cells are simply ignored.
;
; If array1 and array2 have different number of data points, SUMXMY2
; returns error. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11, 15,
; 17, 21, and 43 and the cells B1, B2, ..., B5 hold numbers 13, 22, 31,
; 33, and 39. Then
;
; SUMXMY_2(A1..A5,B1..B5) equals 409.
;@SUMSQ , SUMX2MY2 , SUMX2PY2
(define (sumxmy_2 ad ar1 ac1 ar2 ac2 bd br1 bc1 br2 bc2)
  (let ((a (collect_range ar1 ac1 ar2 ac2 (lambda x t)))
	(b (collect_range br1 bc1 br2 bc2 (lambda x t)))
	(s 0))
    (while a
      (if (and (number? (car a)) (number? (car b)))
	(set! s (+ s (* (- (car a) (car b)) (- (car a) (car b))))))
      (set! a (cdr a))
      (set! b (cdr b)))
    s))


;@syd(cost,salvage_value,life,period)
;@The SYD function calculates the sum-of-years digits depriciation for
; an asset based on its cost, salvage value, anticipated life and a
; particular period. This method accelerates the rate of the
; depreciation, so that more depreciation expense occurs in earlier
; periods than in later ones. The depreciable cost is the actual cost
; minus the salvage value. The useful life is the number of periods
; (typically years) over with the asset is depreciated.
;
; The Formula used for sum-of-years digits depriciation is:
;
; Depriciation expense = ( cost - salvage_value ) * (life - period +
; 1) * 2 / life * (life + 1).
;
; cost = cost of an asset when acquired (market value). salvage_value
; = amount you get when asset sold at the end of its useful life. life
; = anticipated life of an asset. period = period for which we need the
; expense.
;@For example say a company purchases a new computer for $5000 which has
; a salvage value of $200, and a useful life of three years. We would
; use the following to calculate the second year's depreciation using
; the SYD method:
;
; =SYD(5000, 200, 5, 2) which returns 1,280.00.
;@sln
(define (syd cost salvage_value life period)
  (if (<= life 0)
    nil
    (/ (* (- cost salvage_value) (- life period -1) 2)
       (* life (+ life 1)))))

;XXX;@t(value)
;XXX;@T returns value if and only if it is text, otherwise a blank string.
;XXX; Not implemented: we must not redefine t.
;XXX;@T("text") equals "text".
;XXX;
;XXX; T(64) returns an empty cell.
;XXX;@cell n value

;@tbilleq(settlement,maturity,discount)
;@TBILLEQ returns the bond-yield equivalent (BEY) for a
; treasury bill. TBILLEQ is equivalent to (365 * discount) / (360 -
; discount * DSM) where DSM is the days between settlement and
; maturity.
;
; If settlement is after maturity or the maturity is set to over one
; year later than the settlement, TBILLEQ returns error. If
; discount is negative, TBILLEQ returns error.
;@
;@TBILLPRICE , TBILLYIELD
(define (tbilleq settlement maturity discount)
  (let* ((dsm (- maturity settlement))
	 (divisor (- 360 (* discount dsm))))
    (if (or (> settlement maturity) (< discount 0) (> dsm 356) (= divisor 0))
      nil
      (/ (* 365 discount) divisor))))

;@tbillprice(settlement,maturity,discount)
;@TBILLPRICE returns the price per $100 value for a treasury
; bill where settlement is the settlement date and maturity is the
; maturity date of the bill. discount is the treasury bill's discount
; rate.
;
; If settlement is after maturity or the maturity is set to over one
; year later than the settlement, TBILLPRICE returns error. If
; discount is negative, TBILLPRICE returns error.
;@
;@TBILLEQ , TBILLYIELD
(define (tbillprice settlement maturity discount)
  (let* ((dsm (- maturity settlement))
	 (res (* 100 (- 1 (/ (* discount dsm) 360)))))
    (if (or (> settlement maturity) (< discount 0) (> dsm 356))
      nil
      res)))

;@tbillyield(settlement,maturity,pr)
;@TBILLYIELD returns the yield for a treasury bill. settlement
; is the settlement date and maturity is the maturity date of the bill.
; discount is the treasury bill's discount rate.
;
; If settlement is after maturity or the maturity is set to over one
; year later than the settlement, TBILLYIELD returns error. If
; pr is negative, TBILLYIELD returns error.
;@
;@TBILLEQ , TBILLPRICE
(define (tbillyield settlement maturity pr)
  (let* ((dsm (- maturity settlement))
	 (res (* (/ (- 100 pr) pr) (/ 360 dsm))))
    (if (or (<= pr 0) (<= dsm 0) (> dsm 356))
      nil
      res)))

;@tdist(x,dof,tails)
;@TDIST returns the Student's t-distribution. dof is the
; degree of freedom and tails is 1 or 2 depending on whether you want
; one-tailed or two-tailed distribution.
;
; If dof &lt; 1 TDIST returns error. If tails is neither 1 or 2
; TDIST returns error. Excel compatible.
;@TDIST(2,5,1) equals 0.050969739.
;@tinv ttest
(define (tdist x dof tails)
  (if (or (< dof 1) (not (or (= tails 1) (= tails 2))))
    nil
    (if (= tails 1)
      (- 1 (pt x dof))
      (* (- 1 (pt x dof))))))

;XXX;@text(value,format_text)
;XXX;@TEXT returns value as a string with the specified format.
;XXX;@TEXT(3.223,"$0.00") equals "$3.22".
;
; TEXT(date(1999,4,15),"mmmm, dd, yy") equals "April, 15, 99".
;XXX;@dollar
;XXX(define (text value format_text)
  ;XXX"Not implemented")

;@time(hours,minutes,seconds)
;@Returns a number representing the time of day.
;@
;@hour
(define (time hours minutes seconds)
  (+ (* 3600 hours) (* 60 minutes) seconds))

;@timevalue(timetext)
;@Returns a number representing the time of day, a number between 0
; and 86400.
;@
;@hour
(define (timevalue timetext)
  (let ((a (strptime timetext "%T")))
    (if (null? a) (set! a (strptime timetext "%R")))
    (if (null? a)
      nil
      (time (cdr (assq 'hour a))
	    (cdr (assq 'min a))
	    (cdr (assq 'sec a))))))

;@tinv(p,dof)
;@TINV returns the inverse of the two-tailed Student's
; t-distribution.
;
; If p &lt; 0 or p > 1 or dof &lt; 1 TINV returns error. This
; function is Excel compatible.
;@TINV(0.4,32) equals 0.852998454.
;@tdist ttest
(define (tinv p dof)
  (if (or (< p 0) (> p 1) (< dof 1))
    nil
    (qt (- 1 (/ p 2)) dof)))

;XXX;@today()
;XXX;@Returns the serial number for today (the number of seconds elapsed since
; the 1st of January of 1970).
;XXX;@
;XXX;@now
;XXX(define (today)
  ;XXX"Not implemented")

;@transpose(matrix)
;@TRANSPOSE returns the transpose of the input matrix.
;@
;@mmult
(define (transpose dummy r1 c1 r2 c2)
  (if (and (eq? dummy 'RANGE) (<= r1 r2) (<= c1 c2))
    (let ((nrows (- r2 r1 -1))
	  (ncols (- c2 c1 -1))
	  (r0 (row))
	  (c0 (col))
	  (r 0)
	  (c 1))
      (while (< r nrows)
        (while (< c ncols)
	  ; copy (r1+r,c1+c) to (r0+c,c0+r)
	  (set_array_value (+ r0 c) (+ c0 r) (get_value (+ r1 r) (+ c1 c)))
	  (set! c (+ c 1)))
        (set! r (+ r 1))
        (set! c 0))
      (set-pr-scr)
      (recalc-matrix)
      (get_value r1 c1))))

;XXX;@trend(known_y's[,known_x's],new_x's])
;XXX;@TREND estimates future values of a given data set using the
; ``least squares'' line that best fit to your data. known_y's is the
; y-values where y=mx+b and known_x's contains the corresponding
; x-values. new_x's contains the x-values for which you want to
; estimate the y-values.
;
; If known_x's is omitted, an array {1, 2, 3, ...} is used. If new_x's
; is omitted, it is assumed to be the same as known_x's. If known_y's
; and known_x's have unequal number of data points, TREND returns
; error.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; TREND(A1..A5,B1..B5) equals 156.52.
;XXX;@linest
;XXX(define (trend known_y . b)
  ;XXX"Not implemented")

;XXX;@trim(text)
;XXX;@TRIM returns text with only single spaces between words.
; Not implemented, there is already a trim in SIOD.
;XXX;@TRIM(" a bbb cc") equals "a bbb cc".
;XXX;@CLEAN , MID , REPLACE , SUBSTITUTE

;XXX;@trimmean(ref,percent)
;XXX;@TRIMMEAN returns the mean of the interior of a data set. ref is the
; list of numbers whose mean you want to calculate and percent is the
; percentage of number excluded from the mean. For example, if
; percent=0.2 and the data set contains 40 numbers, 8 numbers are
; trimmed from the data set (40 x 0.2), 4 from the top and 4 from the
; bottom of the set. Excel compatible.
;XXX;@
;XXX;@AVERAGE , GEOMEAN , HARMEAN , MEDIAN , MODE
;XXX(define (trimmean ref percent)
  ;XXX"Not implemented")

;XXX;@ttest(array1,array2,tails,type)
;XXX;@TTEST returns the probability of a Student's t-Test.
;
; array1 is the first data set and array2 is the second data set. If
; tails is one, TTEST uses the one-tailed distribution and if tails is
; two, TTEST uses the two-tailed distribution. type determines the kind
; of the test:
;
; 1 Paired test
;
; 2 Two-sample equal variance
;
; 3 Two-sample unequal variance
;
; If the data sets contain a different number of data points and the
; test is paired (type one), TTEST returns error. tails and
; type are truncated to integers. If tails is not one or two, TTEST
; returns error. If type is any other than one, two, or three,
; TTEST returns error. Excel compatible.
;XXX;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1, and the cells B1, B2, ... B5 23.2, 25.8,
; 29.9, 33.5, and 42.7. Then
;
; TTEST(A1..A5,B1..B5,1,1) equals 0.003127619.
;
; TTEST(A1..A5,B1..B5,2,1) equals 0.006255239.
;
; TTEST(A1..A5,B1..B5,1,2) equals 0.111804322.
;
; TTEST(A1..A5,B1..B5,1,3) equals 0.113821797.
;XXX;@fdist finv
;XXX(define (ttest array1 array2 tail2 type)
  ;XXX"Not implemented")

;XXX;@type()
;XXX;@TYPE returns a number indicating the data type of a value. This
; function is Excel compatible.
;XXX;@
;XXX;@
;XXX(define (type x)
  ;XXX"Not implemented")

;@upper(text)
;@UPPER returns a upper-case version of the string in text.
;@UPPER("canceled") equals "CANCELED".
;@lower
(define upper string-upcase)

;XXX;@value(text)
;XXX;@VALUE returns numeric value of text.
;XXX;@VALUE("$1,000") equals 1000.
;XXX;@dollar fixed text
;XXX(define (value text)
  ;XXX"Not implemented")

;@var(b1, b2, ...)
;@VAR estimates the variance of a sample of a population. To get the
; true variance of a complete population use VARP.
;
; (VAR is also known as the N-1-variance. Under reasonable conditions,
; it is the maximum-likelihood estimator for the true variance.)This
; function is Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; VAR(A1..A5) equals 117.64.
;@varp stdev
(define (var . b)
  (range_var_est (collect_args b (lambda x (number? x)))))

;@vara(number1,number2,...)
;@VARA returns the variance based on a sample. Numbers, text and logical
; values are included in the calculation too. If the cell contains text
; or the argument evaluates to FALSE, it is counted as value zero (0).
; If the argument evaluates to TRUE, it is counted as one (1). Note that
; empty cells are not counted. Excel compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, 17.3, "missing", 25.9, and 40.1. Then
;
; VARA(A1..A5) equals 228.613.
;@var varpa
(define (vara . b)
  (range_var_est (collect_args b (lambda x (number? x)))))

;@varp(b1, b2, ...)
;@VARP calculates the variance of a set of numbers where each number is
; a member of a population and the set is the entire population.
;
; (VARP is also known as the N-variance.)
;@Let us assume that the cells A1, A2, ..., A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; VARP(A1..A5) equals 94.112.
;@AVERAGE , DVAR , DVARP , STDEV , VAR
(define (varp . b)
  (range_var_pop (collect_args b (lambda x (number? x)))))

;@varpa(number1,number2,...)
;@VARPA returns the variance based on the entire population. Numbers,
; text and logical values are included in the calculation too. If the
; cell contains text or the argument evaluates to FALSE, it is counted
; as value zero (0). If the argument evaluates to TRUE, it is counted as
; one (1). Note that empty cells are not counted. This function is Excel
; compatible.
;@Let us assume that the cells A1, A2, ..., A5 contain numbers and
; strings 11.4, 17.3, "missing", 25.9, and 40.1. Then
;
; VARPA(A1..A5) equals 182.8904.
;@varp
(define (varpa . b)
  (range_var_pop (collect_args b (lambda x (number? x)))))

;;;@vdb(cost,salvage,life,start_period,end_period[,factor,switch])
;;;@VDB calculates the depreciation of an asset for a given period or
;;; partial period using the double-declining balance method.
;;;@
;;;@db
;;(define (vdb cost salvage life start_period end_period . b)
;;  "Not implemented")

;XXX;@vlookup(value,range,column[,approximate])
;XXX;@VLOOKUP finds the row in range that has a first column
; similar to value. If approximate is not true it finds the row with an
; exact equivilance. If approximate is true, then the values must be
; sorted in order of ascending value for correct function; in this case
; it finds the row with value less than value. It returns the value in
; the row found at a 1 based offset in column columns into the range.
;
; Returns error if column &lt; 0 or if column falls outside
; range.
;XXX;@
;XXX;@hlookup
(define vlookup @VLOOKUP)

;@weekday(serial_number)
;@Converts a serial number to a weekday. XXX: explain.
;@
;@month time now year
(define (weekday serial_number)
  (cdr (assoc 'wday (localtime serial_number))))

;(define (pweibull x a b)
;  (- 1 (exp (- (pow (/ x b) a)))))

;(define (dweibull x a b)
;  (* (/ a (pow b a))
;     (pow x (- a 1))
;     (exp (- (pow (/ x b) a)))))

;@weibull(x,alpha,beta,cumulative)
;@weibull returns the Weibull distribution. If the cumulative
; boolean is true it will return: 1 - exp (-(x/beta)^alpha),
; otherwise it will return (alpha/beta^alpha) * x^(alpha-1) *
; exp(-(x/beta^alpha)).
;
; If x &lt; 0 weibull returns error. If alpha &lt;= 0 or beta &lt;= 0
; weibull returns error. Excel compatible.
;@weibull(3,2,4,0) equals 0.213668559.
;@poisson
(define (weibull x alpha beta cuml)
  (if (or (< x 0) (<= alpha 0) (<= beta 0))
    nil
    (if (not (= cuml 0))
      (pweibull x alpha beta)
      (dweibull x alpha beta))))

;XXX;@workday(start_date,days,holidays)
;XXX;@Returns the day which is days working days from the start_date.
; Weekends and holidays optionaly supplied in holidays are respected.
;
; Returns error if start_date or days are invalid.
;XXX;@
;XXX;@networkdays
;XXX(define (workday start_date days holidays)
  ;XXX"Not implemented")

;XXX;@xirr(values,dates[,guess])
;XXX;@XIRR calculates and returns the internal rate of return of an
; investment that has not necessarily periodic payments. This function
; is closely related to the net present value function (NPV and XNPV).
; The XIRR is the interest rate for a serie of cash flow where the XNPV
; is zero.
;
; values contains the serie of cash flow generated by the investment.
; dates contains the dates of the payments. The first date describes
; the payment day of the initial payment and thus all the other dates
; should be after this date. The optional guess is the initial value
; used in calculating the XIRR. You do not have to use that, it is only
; provided for the Excel compatibility.
;
; Excel compatible.
;XXX;@Let us assume that the cells A1..A5 contain the numbers -6000, 2134,
; 1422, 1933, and 1422, and the cells B1..B5 contain the dates
; "1999-01-15", "1999-04-04", "1999-05-09", "2000-03-12", and
; "2000-05-1". Then
;
; XIRR(A1..A5,B1..B5) returns 0.224838.
;XXX;@irr xnpv
;XXX(define (xirr values dates . b)
  ;XXX"Not implemented")

;XXX;@xnpv(rate,values,dates)
;XXX;@XNPV calculates the net present value of an investment. The schedule
; of the cash flows is given in dates array. The first date indicates
; the beginning of the payment schedule. rate is the interest rate and
; values are the payments.
;
; If values and dates contain unequal number of values, XNPV returns
; the error.
;XXX;@
;XXX;@npv pv
;XXX(define (xnpv rate values dates)
  ;XXX"Not implemented")

;@year(serial_number)
;@Converts a serial number to a year.
;@
;@day month time now
(define (year serial_number)
  (cdr (assoc 'year (localtime serial_number))))

;;;@yield(settle,mat,rate,price,redemption_price,frequency,basis)
;;;@
;;;@
;;;@
;;(define (yield settle mat rate price redemption frequency basis)
;;  "Not implemented")

;;;@yielddisc(settlement,maturity,pr,redemption,basis)
;;;@
;;;@
;;;@
;;(define (yielddisc settlement maturity pr redemption basis)
;;  "Not implemented")

;;;@yieldmat(settlement,maturity,issue,rate,pr,basis)
;;;@
;;;@
;;;@
;;(define (yieldmat settlement maturity issue rate pr basis)
;;  "Not implemented")

;XXX;@ztest(ref,x)
;XXX;@ZTEST returns the two-tailed probability of a z-test.
;
; ref is the data set and x is the value to be tested.
;
; If ref contains less than two data items ZTEST returns error.
; Excel compatible.
;XXX@Let us assume that the cells A1, A2,... A5 contain numbers 11.4,
; 17.3, 21.3, 25.9, and 40.1. Then
;
; ZTEST(A1..A5,20) equals 0.254717826.
;XXX;@CONFIDENCE , NORMDIST , NORMINV , NORMSDIST , NORMSINV , STANDARDIZE
;XXX(define (ztest ref x)
  ;XXX"Not implemented")

(define (utest) "Ulric fibbar!")

