(in-package :lispyboi)
(provide "ffi")

(setf *ffi-bytespecs* '(uint8 byte
                        uint16
                        uint32
                        uint64
                        char character string
                        short
                        int
                        long))

(setq *ffi-type-registry*
      (list
       '(uint8 1 nil nil) '(int8 1 nil nil)
       '(uint16 2 nil nil) '(int16 2 nil nil)
       '(uint32 4 nil nil) '(int32 4 nil nil)
       '(uint64 8 nil nil) '(int64 8 nil nil)
       '(char 1 nil nil)
       '(short 2 nil nil)
       '(int 4 nil nil)
       '(long 8 nil nil)
       '(float 4 nil nil)
       '(double 8 nil nil))
      "A list of (TYPE-NAME TYPE-SIZE DEFINITION STRUCT-OR-UNIONP)")

(defun ffi-get-type (type)
  (assoc type *ffi-type-registry*))

(defun %ffi-pointer-type-p (type)
  (let ((sym-name (symbol-name type)))
    (eql #\* (aref sym-name (- (length sym-name) 1)))))

(defun %ffi-struct-type-p (type)
  (let ((found (assoc type *ffi-type-registry*)))
    (when found
      (eq 'struct (fourth found)))))

(defun %ffi-scalar-type-p (type)
  (or (%ffi-pointer-type-p type)
      (member type '(uint8 int8
                     uint16 int16
                     uint32 int32
                     uint64 int64
                     char short int long
                     float double))))

(defun %ffi-align-up (offset amount)
  (if (eq 0 (rem offset amount))
      offset
      (* amount
         (+ 1 (/ offset amount)))))

(defun %ffi-pointer-align-up (offset)
  (%ffi-align-up offset (ffi-machine-pointer-size)))

(defun %ffi-struct-align-up (offset)
  (%ffi-align-up offset (ffi-machine-pointer-size)))

(defun %ffi-sizeof (type)
  (if (%ffi-pointer-type-p type)
      (ffi-machine-pointer-size)
      (let ((found-type (assoc type *ffi-type-registry*)))
        (if found-type
            (second found-type)
            (signal 'ffi-error "Type does not exist in *FFI-TYPE-REGISTRY*" type)))))

(defmacro ffi-sizeof (type)
  (if (consp type)
      `(%ffi-sizeof ,type)
      (%ffi-sizeof type)))

(defun ffi-field-offsets (type-obj)
  (third type-obj))

(defmacro ffi-offset-of (type field)
  (let* ((type-obj (ffi-field-offsets (if (symbolp type)
                                          (ffi-get-type type)
                                          type)))
         (found-field (assoc field type-obj)))
    (if found-field
        (third found-field)
        (signal 'ffi-error "Type does not have field" type field))))

(defmacro ffi-type-of (type field)
  (let ((type-obj (ffi-field-offsets
                   (if (symbolp type)
                       (ffi-get-type type)
                       type))))
    (list 'quote (second (assoc field type-obj)))))

(defun %ffi-getter-function (type-size)
  (case type-size
    (1 'ffi-ref-8)
    (2 'ffi-ref-16)
    (4 'ffi-ref-32)
    (8 'ffi-ref-64)
    (t 'ffi-ref)))

(defun %ffi-setter-function (type-size)
  (case type-size
    (1 'ffi-set-ref-8)
    (2 'ffi-set-ref-16)
    (4 'ffi-set-ref-32)
    (8 'ffi-set-ref-64)
    (t 'ffi-set-ref)))

(defun %ffi-define-struct-or-union (type-name struct-or-union field-names field-sizes field-types field-offsets total-size stride-alignment)
  (let ((type-obj (list type-name total-size field-offsets struct-or-union))
        (functions))
    (push type-obj *ffi-type-registry*)
    (map (lambda (name size type offset)
           (let ((offset (third offset))
                 (getter-name (intern (concatenate (symbol-name type-name) "." (symbol-name name))))
                 (setter-name (intern (concatenate (symbol-name type-name) ".SET-" (symbol-name name))))
                 (struct-type-p (ffi-field-offsets (ffi-get-type type))))
             (push 
              `(defun ,getter-name (,type-name)
                 ,(if struct-type-p
                      `(ffi-ref ,type-name ,offset)
                      `(,(%ffi-getter-function size) (ffi-ref ,type-name ,offset))))
              functions)
             (push
              `(defun ,setter-name (,type-name value)
                 ,(if struct-type-p
                      `(ffi-set-ref (ffi-ref ,type-name ,offset) value ,size)
                      `(,(%ffi-setter-function size) (ffi-ref ,type-name ,offset) value)))
              functions)
             (push `(defsetf ,getter-name ,setter-name) functions)))
         field-names
         field-sizes
         field-types
         field-offsets)
    (push `(defun ,(intern (concatenate "MAKE-" (symbol-name type-name))) ()
             (ffi-zero-alloc ,total-size))
          functions)
    (push 'progn functions)
    functions))

(defun %ffi-aggregate-stride-alignment-size (type)
  (let ((max-scalar-size 1))
    (dolist (e (ffi-field-offsets type))
      (if (%ffi-scalar-type-p (second e))
          ;; scalar type
          (when (> (%ffi-sizeof (second e)) max-scalar-size)
            (setf max-scalar-size (%ffi-sizeof (second e))))
          ;; aggregate type
          (let ((aggregate-size (%ffi-aggregate-stride-alignment-size (ffi-get-type (third e)))))
            (when (> aggregate-size max-scalar-size)
              (setf max-scalar-size aggregate-size)))))
    max-scalar-size))

(defun %ffi-stride-alignment-size (type-sizes)
  (let ((stride-alignment 1))
    (dolist (e type-sizes)
      (if (%ffi-scalar-type-p (car e))
          ;; scalar type
          (when (> (cdr e) stride-alignment)
            (setf stride-alignment (cdr e)))
          ;; aggregate type
          (let ((aggregate-size (%ffi-aggregate-stride-alignment-size (ffi-get-type (car e)))))
            (when (> aggregate-size stride-alignment)
              (setf stride-alignment aggregate-size)))))
    stride-alignment))

(defmacro ffi-defstruct (struct-name &body body)
  (let* ((field-names (map1 #'first body))
         (field-sizes (map1 (lambda (e) (%ffi-sizeof (second e))) body))
         (field-types (map1 #'second body))
         (field-arrays (map1 #'third body))
         (total-size 0)
         (last-field-size 0)
         (needs-pointer-align nil)
         (stride-alignment (%ffi-stride-alignment-size (map #'cons field-types field-sizes)))
         (field-offsets (let ((offset 0))
                          (map (lambda (field field-type size array-count)
                                 (when (%ffi-pointer-type-p field-type)
                                   (setf needs-pointer-align t))
                                 ;;(incf total-size size)
                                 (let ((new-offs (%ffi-align-up offset
                                                                (if (< size stride-alignment)
                                                                    size
                                                                    stride-alignment))))
                                   (setf total-size new-offs)
                                   (setf offset new-offs))
                                 (prog1 (list field field-type offset (or array-count 1))
                                   (setf last-field-size (* size (or array-count 1)))
                                   (when array-count
                                     ;;(format t "field-array: f=~a t=~a s=~a n=~a~%" field field-type size array-count)
                                     
                                     (incf offset (* size array-count)))
                                   (incf offset size)))
                               field-names
                               field-types
                               field-sizes
                               field-arrays))))
    (incf total-size last-field-size)
    (if needs-pointer-align
        (setf total-size (%ffi-align-up total-size (ffi-machine-pointer-size)))
        (setf total-size (%ffi-align-up total-size stride-alignment)))
    (when nil
      (format t "body ~a~%" body)
      (format t "struct ~a (~a) field offsets:~%" struct-name total-size)
      (dolist (e field-offsets)
        (format t "  ~a ~a @ ~a (~a)~%" (first e) (second e) (third e) (* (ffi-sizeof (second e))
                                                                          (fourth e)))))
    (%ffi-define-struct-or-union struct-name 'struct field-names field-sizes field-types field-offsets total-size stride-alignment)))

(defmacro ffi-defunion (union-name &body body)
  (let* ((field-names (map1 #'first body))
         (field-sizes (map1 (lambda (e) (%ffi-sizeof (second e))) body))
         (field-types (map1 #'second body))
         (total-size 0)
         (stride-alignment (%ffi-stride-alignment-size (map #'cons field-types field-sizes)))
         (field-offsets (let ((offset 0))
                          (map (lambda (field field-type size)
                                 (when (> size total-size)
                                   (setq total-size size))
                                 (prog1 (list field field-type 0)
                                   (incf offset size)))
                               field-names
                               field-types
                               field-sizes))))
    (when (eq stride-alignment (ffi-machine-pointer-size))
      (setf total-size (%ffi-align-up total-size stride-alignment)))
    (when nil
      (format t "union ~a (~a) field offsets:~%" union-name total-size)
      (dolist (e field-offsets)
        (format t "  ~a ~a @ ~a (~a)~%" (first e) (second e) (third e) (ffi-sizeof (second e)))))
    (%ffi-define-struct-or-union union-name 'union field-names field-sizes field-types field-offsets total-size 0)))

(defun ffi-get-symbol-or-signal (library-handle symbol-name)
  (let ((sym (ffi-get-symbol library-handle symbol-name)))
    (unless sym
      (signal 'ffi-symbol-not-found-error library-handle symbol-name))
    sym))

(defun ffi-nullptr-p (obj)
  (eq (ffi-nullptr) obj))

(defmacro with-ffi-array ((array-var bytespec buffer-expr) &body body)
  (let ((buffer (gensym))
        (size (gensym))
        (ref-function)
        (ref-offset-calc)
        (tmp-expr (gensym)))
    (if (member bytespec '(char character string))
        (progn
          `(let ((,tmp-expr ,buffer-expr))
             (when ,tmp-expr
               (destructuring-bind (,buffer ,size) ,tmp-expr
                 (unwind-protect (let ((,array-var (prog1 (ffi-coerce-string ,buffer ,size)
                                                     (ffi-free ,buffer))))
                                   ,@body))))))
        (progn
          (case bytespec
            (uint8
             (setf ref-function 'ffi-ref-8)
             (setf ref-offset-calc 'i))
            (uint16
             (setf ref-function 'ffi-ref-16)
             (setf ref-offset-calc '(* i 2)))
            (uint32
             (setf ref-function 'ffi-ref-32)
             (setf ref-offset-calc '(* i 4)))
            (uint64
             (setf ref-function 'ffi-ref-64)
             (setf ref-offset-calc '(* i 8)))
            (byte
             (setf ref-function 'ffi-ref-8)
             (setf ref-offset-calc 'i))
            (short
             (setf ref-function 'ffi-ref-16)
             (setf ref-offset-calc '(* i 2)))
            (int
             (setf ref-function 'ffi-ref-32)
             (setf ref-offset-calc '(* i 4)))
            (long
             (setf ref-function 'ffi-ref-64)
             (setf ref-offset-calc '(* i 8)))
            (t (signal 'bytespec-error "BYTESPEC must be one of" *ffi-bytespecs* bytespec)))
          `(let ((,tmp-expr ,buffer-expr))
             (when ,tmp-expr
               (destructuring-bind (,buffer ,size) ,tmp-expr
                 (let ((,array-var (make-array ,size)))
                   (dotimes (i ,size)
                     (setf (aref ,array-var i) (,ref-function (ffi-ref ,buffer ,ref-offset-calc))))
                   (ffi-free ,buffer)
                   ,@body))))))))

(defun ffi-open-or-signal (library-name)
  (let ((handle (ffi-open library-name)))
    (unless handle
      (signal 'ffi-library-not-found-error library-name))
    handle))

(defmacro ffi-with-symbols (library-name symbols &body body)
  (let ((lib-var (gensym)))
    `(let* ((,lib-var (ffi-open-or-signal ,library-name))
            ,@(map1 (lambda (e)
                      `(,(first e) (ffi-get-symbol-or-signal ,lib-var ,(second e))))
                    symbols))
       
       (macrolet (,@(map1 (lambda (e)
                            (let ((args '(unquote-splicing args))
                                  (return-type (third e)))
                              (cond ((eq 'void return-type)
                                     `(,(first e) (&rest args)
                                       `(ffi-call-void ,(first e) ,args)))
                                    ((member return-type '(char byte uint8 int8
                                                           short uint16 int16
                                                           int uint32 int32
                                                           long uint64 int64))
                                     `(,(first e) (&rest args)
                                       `(ffi-coerce-int (ffi-call ,(first e) ,args))))
                                    (t
                                     `(,(first e) (&rest args)
                                       `(ffi-call ,(first e) ,args))))

                              ))
                      symbols))
         ,@body)

       )))


(defmethod print-object ((o system-pointer) stream)
  (format stream "#<SYSTEM-POINTER 0x~X>" (ffi-coerce-fixnum o))
  o)

(export '(ffi-get-type
          ffi-sizeof
          ffi-offset-of
          ffi-type-of
          ffi-defstruct
          ffi-defunion
          ffi-get-symbol-or-signal
          ffi-nullptr-p
          with-ffi-array
          ffi-with-symbols

          ffi-machine-pointer-size
          ffi-open
          ffi-close
          ffi-get-symbol
          ffi-call
          ffi-nullptr
          ffi-alloc
          ffi-zero-alloc
          ffi-free
          ffi-marshal
          ffi-strlen
          ffi-coerce-fixnum
          ffi-coerce-int
          ffi-coerce-string
          ffi-ref
          ffi-ref-8
          ffi-ref-16
          ffi-ref-32
          ffi-ref-64
          ffi-set-ref
          ffi-set-ref-8
          ffi-set-ref-16
          ffi-set-ref-32
          ffi-set-ref-64

          bytespec-error
          ffi-library-not-found-error
          ffi-symbol-not-found-error

          uint8
          uint16
          uint32
          uint64
          int8
          int16
          int32
          int64
          char
          short
          int
          long))

(export *ffi-bytespecs*)
