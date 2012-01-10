;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-29
  (:use)
  (:export))

(defpackage :srfi-29.internal
  (:use :srfi-29 :cl :fiveam :srfi-6)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-5 :let)
  (:shadow :lambda :loop :format))
