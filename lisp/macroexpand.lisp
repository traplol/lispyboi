(provide "macroexpand")

(in-package :lispyboi)


(let ((macro-table (make-hash-table)))
  (defun get-macro (macro-name)
    (gethash macro-name macro-table))

  (defun set-macro (macro-name function)
    (setf (gethash macro-name macro-table) function)))

(defun macroexpand (form)
  (if (atom form)
      form
      (case (car form)
        (quote
         form)
        (if
         `(if ,(macroexpand (second form))
              ,(macroexpand (third form))
              ,(macroexpand (fourth form))))
        (%define-macro
         `(%define-macro ,(second form) ,(third form)
                         ,@(map #'macroexpand (cdddr form))))
        (%lambda
         `(%lambda ,(second form)
                   ,@(map #'macroexpand (cddr form))))
        (%setq
         `(%setq ,(second form) ,(macroexpand (third form))))
        (otherwise
         (let ((macro (get-macro (first form))))
           (if macro
               (apply macro (cdr form))
               (kernel::%macro-expand form)))))))

(export '(macroexpand
          set-macro))
