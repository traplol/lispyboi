(require "asserts")
(in-package :test-suite)

(defun test-func-a (a b)
  (* a b))

(defun test-func-b (a b)
  (+ a b))

(let ((a 10) (b 20))
  (assert-= (* a b) (funcall #'test-func-a a b))
  (assert-= (* a b) (test-func-a a b))
  (assert-= (+ a b) (funcall #'test-func-b a b))
  (assert-= (+ a b) (test-func-b a b)))

(lispyboi::%set-function 'test-func-b 'test-func-a)

(let ((a 10) (b 20))
  (assert-eq #'test-func-a #'test-func-b)
  (assert-= (test-func-a a b) (funcall #'test-func-b a b))
  (assert-= (test-func-a a b) (apply #'test-func-b (list a b)))
  (assert-= (+ a b) (test-func-b a b)))

