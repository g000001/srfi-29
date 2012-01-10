;;;; srfi-29.asd

(cl:in-package :asdf)

(defsystem :srfi-29
  :serial t
  :depends-on (:fiveam
               :srfi-5
               :srfi-6
               :srfi-23)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-29")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-29))))
  (load-system :srfi-29)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-29.internal :srfi-29))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
