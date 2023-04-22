(require "asserts")
(in-package :test-suite)

(let ((a 420) (b 69))
  (macrolet ((foo (x y) `(+ ,x ,y))
             (bar (x y) `(* ,x ,y)))
    (assert-= 70 (foo (bar 10 5) 20)) ;; (+ (* 10 5) 20)
    (assert-= (+ 420 69) (foo a b))))
