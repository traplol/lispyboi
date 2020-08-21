(in-package :lispyboi)
(provide "gc")

(defun gc-pause ()
  "Pause the garbage collector, stopping it from running automatically.
The garbage collect may still be manually run with GC-COLLECT."
  (kernel::%gc-pause))

(defun gc-paused ()
  "Returns T if the garbage collect is currently paused, otherwise NIL.
GC-PAUSED is a SETF-able place."
  (kernel::%gc-paused-p))

(defsetf gc-paused
    kernel::%gc-set-paused)

(defun gc-collect ()
  "Manually run the garbage collect and return the number of bytes freed."
  (kernel::%gc-collect))

(defun gc-total-consed ()
  "Returns the total number of bytes allocated. This value never decreases
as is represents the sum of all allocations."
  (kernel::%gc-get-consed))

(defun gc-total-freed ()
  "Returns the total number of bytes freed. This value never decreases
as is represents the sum of all frees."
  (kernel::%gc-get-freed))

(defun gc-time-spent ()
  "Returns the time spent, in microseconds, running the garbage collect."
  (kernel::%gc-get-time-spent-in-gc))

(defun gc-times-run ()
  "Returns the number of times the garbage collect has run."
  (kernel::%gc-get-times-gc-has-run))

(defun gc-collect-threshold ()
  "Returns the number of bytes needed to trigger an automatic garbage collection.
GC-COLLECT-THRESHOLD is a SETF-able place."
  (kernel::%gc-get-collect-threshold))

(defsetf gc-collect-threshold
    kernel::%gc-set-collect-threshold)

(export '(gc-pause
          gc-paused
          gc-collect
          gc-total-consed
          gc-total-freed
          gc-time-spent
          gc-times-run
          gc-collect-threshold))
