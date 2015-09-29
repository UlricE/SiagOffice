; Default styles are defined here.

(define STY_DEFAULT 0)
(define STY_INTEGER 1)
(define STY_FLOAT 2)
(define STY_TIME 3)
(define STY_INVISIBLE 4)
(define STY_PERCENT 5)

(register-style "Default" "%d" STY_DEFAULT)
(register-style "Invisible" "%d" STY_INVISIBLE)
(register-style "Integer" "%d" STY_INTEGER)
(register-style "Scientific" "%g" STY_FLOAT)
(register-style "Fixed" "%.2f" STY_FLOAT)
(register-style "Date" "%m/%d/%Y" STY_TIME)
(register-style "Time" "%H:%M:%S" STY_TIME)
(register-style "Time Difference" "%H:%M:%S" STY_TIME);
(register-style "Percent" "%.2f %%" STY_PERCENT)
(register-style "Hex" "%lX" STY_INTEGER)
(register-style "Currency" "$%.2f" STY_FLOAT)
(register-style "User 1" "%.2f" STY_FLOAT)
(register-style "User 2" "%.2f" STY_FLOAT)
(register-style "User 3" "%.2f" STY_FLOAT)
(register-style "User 4" "%.2f" STY_FLOAT)
(register-style "User 5" "%.2f" STY_FLOAT)
(register-style "Ulric Testar" "Ulric says %.2f" STY_FLOAT)

