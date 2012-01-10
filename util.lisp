(cl:in-package :srfi-29.internal)

(progn
  (setf (fdefinition 'eq?) #'eq)
  (setf (fdefinition 'integer?) #'integerp)
  (setf (fdefinition 'list?) #'listp)
  (setf (fdefinition 'negative?) #'minusp)
  (setf (fdefinition 'null?) #'null)
  (setf (fdefinition 'pair?) #'consp)
  (setf (fdefinition 'positive?) #'plusp)
  (setf (fdefinition 'zero?) #'zerop)
  (setf (fdefinition 'vector-length) #'length)
  (setf (fdefinition 'vector?) #'vectorp)
  (setf (fdefinition 'procedure?) #'functionp)
  (setf (fdefinition 'exact?) #'rationalp)
  (setf (fdefinition 'even?) #'evenp)
  (setf (fdefinition 'real?) #'realp)
  (setf (fdefinition 'newline) #'terpri)
  (setf (fdefinition 'display) #'princ)
  (setf (fdefinition 'char=?) #'char=)
  (setf (fdefinition 'char-numeric?) #'digit-char-p)
  )

(defmacro set! (var val)
  `(setq ,var ,val))

(declaim (inline list-tail vector-set! list-ref vector->list list->vector
                 quotient))
(defun quotient (x y)
  (values (truncate x y)))

(defun list-tail (list k)
  (nthcdr k list))

(defun list-ref (list k)
  (nth k list))

(defun vector-set! (vec index val)
  (setf (aref vec index) val))

(defun vector->list (vec)
  (coerce vec 'list))

(defun list->vector (list)
  (coerce list 'vector))

(defun to-proper-lambda-list (list)
  (typecase list
    (list (if (tailp () list)
              list
              (cl:let ((last (last list)))
                `(,@(butlast list)
                      ,(car last)
                    cl:&rest
                    ,(cdr last)))))
    (symbol `(cl:&rest ,list))))

(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body))

(defmacro letrec ((&rest binds) &body body)
  `(let (,@(mapcar (cl:lambda (x)
                     `(,(car x) #'values))
             binds))
     (declare (optimize (space 3)))
     (labels (,@(remove nil
                  (mapcar (cl:lambda (x &aux (name (car x)))
                            `(,name
                               (&rest args)
                               (apply ,name args)))
                          binds)))
       (declare (optimize (debug 0) (space 3)))
       (psetq ,@(apply #'append binds))
       ,@body)))

(defmacro define-function (name-args &body body)
  (if (consp name-args)
      (destructuring-bind (name . args)
                          name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (setf (fdefinition ',name-args)
               ,(car body)))))

(declaim (inline vector-ref))
(defun vector-ref (vec k)
  (svref vec k))

(declaim (inline modulo))
(defun modulo (x y)
  (mod x y))

(declaim (inline string->list))
(defun string->list (string)
  (coerce string 'list))

(defun string->number (string)
  (the number (values (read-from-string string))))

(defmacro functionlet ((&rest definitions) &body body)
  (do ((def definitions (cdr def))
       (ans '()
            (destructuring-bind (name fn)
                                (car def)
              (cons `(,name (&rest args)
                            (apply (the function ,fn) args))
                    ans))))
      ((endp def)
       `(labels (,@ans)
          (declare (optimize (debug 0)))
          ,@body))))

;;; eof
