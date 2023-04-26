(require "asserts")
(in-package :test-suite)

(assert-string= "" (format nil ""))

(assert-string= "-1" (format nil "~a" (- 1)))
(assert-string= "0" (format nil "~a" 0))
(assert-string= "1" (format nil "~a" 1))

(assert-string= "123" (format nil "123"))
(assert-string= "123" (format nil "~A" 123))
(assert-string= "123" (format nil "~A" "123"))

(assert-string= "~" (format nil "~~"))
(assert-string= "~~" (format nil "~~~~"))
(assert-string= "~~~" (format nil "~~~~~~"))

(assert-string= (make-string #\newline) (format nil "~%"))
(assert-string= (make-string #\newline #\newline) (format nil "~%~%"))

(assert-string= "#:test 123 TEST" (format nil "~a ~a ~a" (make-symbol "test") 123 'test))

(assert-string= "hello" (format nil "h~a~a~ao" #\e #\l #\l))

(assert-string= "NIL" (format nil "~a" '()))
(assert-string= "(1)" (format nil "~a" '(1)))
(assert-string= "(1 2)" (format nil "~a" '(1 2)))
(assert-string= "(1 2 3)" (format nil "~a" '(1 2 3)))
(assert-string= "(1 2 3 4)" (format nil "~a" '(1 2 3 4)))
(assert-string= "(1 2 3 4 5)" (format nil "~a" '(1 2 3 4 5)))

(assert-string= "(1)" (format nil "~a" '(1)))
(assert-string= "(1 . 2)" (format nil "~a" '(1 . 2)))
(assert-string= "(1 2 . 3)" (format nil "~a" '(1 2 . 3)))
(assert-string= "(1 2 3 . 4)" (format nil "~a" '(1 2 3 . 4)))
(assert-string= "(1 2 3 4 . 5)" (format nil "~a" '(1 2 3 4 . 5)))

(assert-string= "(1 (((NIL))) 2)" (format nil "~a" '(1 (((()))) 2)))

(assert-string= "15" (format nil "~a" 15))
(assert-string= "15" (format nil "~s" 15))
(assert-string= "15" (format nil "~d" 15))
(assert-string= "F" (format nil "~x" 15))
(assert-string= "17" (format nil "~o" 15))
(assert-string= "1111" (format nil "~b" 15))

(assert-string= "a" (format nil "~d" #\a))
(assert-string= "a" (format nil "~x" #\a))
(assert-string= "a" (format nil "~o" #\a))
(assert-string= "a" (format nil "~b" #\a))
(assert-string= "11111111111111111111111111111111111111111111111111111111111111"
                (format nil "~b" +most-positive-fixnum+))
(assert-string= "-100000000000000000000000000000000000000000000000000000000000000"
                (format nil "~b" +most-negative-fixnum+))
(assert-string= "-11111111111111111111111111111111111111111111111111111111111111"
                (format nil "~b" (+ 1 +most-negative-fixnum+)))

(assert-string= "3FFFFFFFFFFFFFFF" (format nil "~x" +most-positive-fixnum+))
(assert-string= "-4000000000000000" (format nil "~x" +most-negative-fixnum+))
(assert-string= "-3FFFFFFFFFFFFFFF" (format nil "~x" (+ 1 +most-negative-fixnum+)))

(assert-string= "377777777777777777777" (format nil "~o" +most-positive-fixnum+))
(assert-string= "-400000000000000000000" (format nil "~o" +most-negative-fixnum+))
(assert-string= "-377777777777777777777" (format nil "~o" (+ 1 +most-negative-fixnum+)))

(assert-string= "0" (format nil "~x" 0))
(assert-string= "1" (format nil "~x" 1))
(assert-string= "2" (format nil "~x" 2))
(assert-string= "3" (format nil "~x" 3))
(assert-string= "4" (format nil "~x" 4))
(assert-string= "5" (format nil "~x" 5))
(assert-string= "6" (format nil "~x" 6))
(assert-string= "7" (format nil "~x" 7))
(assert-string= "8" (format nil "~x" 8))
(assert-string= "9" (format nil "~x" 9))
(assert-string= "A" (format nil "~x" 10))
(assert-string= "B" (format nil "~x" 11))
(assert-string= "C" (format nil "~x" 12))
(assert-string= "D" (format nil "~x" 13))
(assert-string= "E" (format nil "~x" 14))
(assert-string= "F" (format nil "~x" 15))
(assert-string= "10" (format nil "~x" 16))
(assert-string= "11" (format nil "~x" 17))
(assert-string= "20" (format nil "~x" 32))
(assert-string= "21" (format nil "~x" 33))

(assert-string= "'FOO" (format nil "~s" ''foo))
(assert-string= "'FOO" (format nil "~a" ''foo))
(assert-string= "'(1 '2 '(3))" (format nil "~s" ''(1 '2 '(3))))
(assert-string= "'(1 '2 '(3))" (format nil "~a" ''(1 '2 '(3))))

(assert-string= "'(A B test 123)" (format nil "~a" ''(a b "test" 123)))
(assert-string= "'(A B \"test\" 123)" (format nil "~s" ''(a b "test" 123)))

(assert-string= "-3422861438051509793" (format nil "~s" (- 3422861438051509793)))
