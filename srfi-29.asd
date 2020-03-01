;;;; srfi-29.asd

(cl:in-package :asdf)


(defsystem :srfi-29
  :version "20200302"
  :description "SRFI 29 for CL: Localization"
  :long-description "SRFI 29 for CL: Localization
https://srfi.schemers.org/srfi-29"
  :author "Scott G. Miller"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam
               :srfi-5
               :srfi-6
               :srfi-23
               :srfi-61)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-29")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-29))))
  (let ((name "https://github.com/g000001/srfi-29")
        (nickname :srfi-29))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-29))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-29#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-29)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
