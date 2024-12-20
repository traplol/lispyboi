(in-package :lispyboi)
(provide "print-object")

(require "stream")
(require "math")

(defgeneric print-object (object stream)
  "Write a printable representation of OBJECT to STREAM.

OBJECT should be returned")

(defmethod print-object ((obj (simple-array * *)) stream)
  (output-stream-write-string stream "#(")
  (when (/= 0 (length obj))
    (dotimes (i (- (length obj) 1))
      (print-object (aref obj i) stream)
      (output-stream-write-char stream #\Space))
    (print-object (aref obj (- (length obj) 1)) stream))
  (output-stream-write-string stream ")")
  obj)

(defmethod print-object ((obj (simple-array bit *)) stream)
  (output-stream-write-string stream "#(BIT-VECTOR ")
  (dotimes (i (length obj))
    (output-stream-write-char stream (if (= 1 (aref obj (- (length obj) 1 i))) #\1 #\0)))
  (output-stream-write-string stream ")")
  obj)

(defmethod print-object ((o string) stream)
  (output-stream-write-char stream #\")
  (output-stream-write-string stream o)
  (output-stream-write-char stream #\")
  o)

(defmethod print-object ((o null) stream)
  (output-stream-write-string stream "NIL")
  nil)

(defmethod print-object ((c character) stream)
  (case c
    (#\Space (output-stream-write-string stream "#\\Space"))
    (#\Newline (output-stream-write-string stream "#\\Newline"))
    (#\Tab (output-stream-write-string stream "#\\Tab"))
    (#\Return (output-stream-write-string stream "#\\Return"))
    (t
     (output-stream-write-char stream #\#)
     (output-stream-write-char stream #\\)
     (output-stream-write-char stream c)))
  c)

(defmethod print-object ((o symbol) stream)
  (let ((pkg (symbol-package o)))
    (cond ((eq pkg (symbol-package :keyword))
           (output-stream-write-char stream #\:))
          ((null pkg)
           (output-stream-write-string stream "#:"))))
  (output-stream-write-string stream (symbol-name o))
  o)

(defmethod print-object ((n fixnum) stream)
  (let ((number n))
    (if (= number 0)
        (output-stream-write-char stream #\0)
        (progn
          (when (< number 0)
            (setf number (- number))
            (output-stream-write-char stream #\-))
          (let ((chars))
            (while (/= 0 number)
                   (push (code-char (+ (char-code #\0) (rem number 10))) chars)
                   (setf number (floor number 10)))
            (dolist (c chars)
              (output-stream-write-char stream c))))))
  n)

(defmethod print-object ((n float) stream)
  ;; FIXME: we just cheat and use the c++ implementation of float-string...
  (output-stream-write-string stream (kernel::%float-string n))
  n)

(defmethod print-object ((o cons) stream)
  (if (and (eq 'quote (car o))
           (consp (cdr o))
           (null (cddr o)))
      (progn
        (output-stream-write-char stream #\')
        (print-object (second o) stream))
      (progn
        (output-stream-write-char stream #\()
        (let ((cur o))
          (while (consp (cdr cur))
                 (print-object (car cur) stream)
                 (output-stream-write-char stream #\Space)
                 (setf cur (cdr cur)))
          (cond ((null (cdr cur))
                 (print-object (car cur) stream))
                (t
                 (print-object (car cur) stream)
                 (output-stream-write-string stream " . ")
                 (print-object (cdr cur) stream))))
        (output-stream-write-char stream #\))))
  o)

(defmethod print-object ((o function) stream)
  (let ((bits (kernel::%bits-of o))
        (n 0))
    (dotimes (i (length bits))
      (setf n (bit-shift n 1))
      (incf n (aref bits (- (length bits) i 1))))
    (format stream "#<FUNCTION ~X>" n))
  o)

(defmethod print-object ((o package) stream)
  (format stream "#<The PACKAGE ~S>" (package-name o))
  o)

(defmethod print-object ((o signal-context) stream)
  (format stream (kernel::%repr o)))

(export '(print-object))
