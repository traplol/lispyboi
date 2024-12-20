(in-package :lispyboi)
(provide "defstruct")

(let ((struct-type-registry))
  (defun %defstruct (struct-name slot-descriptions)
    (let* ((struct-name-str (symbol-name struct-name))
           (ctor (intern (concatenate "MAKE-" struct-name-str)))
           (slot-names (map1 #'first slot-descriptions))
           (slot-initializers (map1 (lambda (desc) (list (first desc) (second desc)))
                                    slot-descriptions))
           (slot-indices (let ((i -1)) (map1 (lambda (e) (incf i)) slot-descriptions)))
           (getter-names (map1 (lambda (slot-name)
                                 (intern (concatenate struct-name-str "-" (symbol-name slot-name))))
                               slot-names))
           (setter-names (map1 (lambda (slot-name)
                                 (intern (concatenate struct-name-str "-SET-" (symbol-name slot-name))))
                               slot-names))
           (type (list struct-name :slot-names slot-names))
           (type-predicate (intern (concatenate struct-name-str
                                                (if (find-last-of struct-name-str #\-)
                                                    "-P"
                                                    "P")))))
      (push type struct-type-registry)
      `(progn
         (defun ,ctor (&key ,@slot-initializers)
           (kernel::%create-instance ',type ,@slot-names))
         (defun ,type-predicate (object)
           (eq ',struct-name (type-of object)))
         (defmethod print-object ((object ,struct-name) stream)
           (format stream
                   ,(concatenate "#S("
                                 (symbol-name struct-name)
                                 (apply #'concatenate
                                        (map1 (lambda (e) (concatenate " :" (symbol-name e) " ~S"))
                                              slot-names))
                                 ")")
                   ,@(map1 (lambda (e) (list e 'object))
                           getter-names))
           object)
         ,@(map (lambda (getter-name index)
                  `(defun ,getter-name (instance)
                     (if (eq ',struct-name (type-of instance))
                         (kernel::%get-slot instance ,index)
                         (signal 'type-error ',getter-name ',struct-name instance (type-of instance)))))
                getter-names
                slot-indices)
         ,@(map (lambda (setter-name index)
                  `(defun ,setter-name (instance value)
                     (if (eq ',struct-name (type-of instance))
                         (kernel::%set-slot instance ,index value)
                         (signal 'type-error ',setter-name ',struct-name instance (type-of instance)))))
                setter-names
                slot-indices)
         ,@(map (lambda (getter setter) (list 'defsetf getter setter))
                getter-names
                setter-names)
         ',struct-name)))

  (defun type-definition (type-name)
    (assoc type-name struct-type-registry)))

(defmacro defstruct (name &rest slot-descriptions)
  (%defstruct name slot-descriptions))

(defun slot-index (object slot-name)
  (index-of slot-name
            (second (member :slot-names
                            (rest (kernel::%structure-definition object))))))

(defun slot-value (object slot-name)
  (let ((index (slot-index object slot-name)))
    (if index
        (kernel::%get-slot object index)
        (signal 'slot-missing-error slot-name (type-of object)))))

(defun set-slot-value (object slot-name value)
  (let ((index (slot-index object slot-name)))
    (if index
        (kernel::%set-slot object index value)
        (signal 'slot-missing-error slot-name (type-of object)))))

(defsetf slot-value set-slot-value)

(defmacro with-slots (slots instance &body body)
  (let ((var (gensym)))
    `(let ((,var ,instance))
       (symbol-macrolet (,@(map1 (lambda (slot) `(,slot (slot-value ,var ',slot))) slots))
         ,@body))))

(export '(defstruct
          slot-value
          with-slots
          type-error
          slot-missing-error))
