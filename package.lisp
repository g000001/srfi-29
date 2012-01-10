;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-29
  (:use)
  (:export :current-language
           :current-country
           :current-locale-details
           :declare-bundle!
           :store-bundle
           :load-bundle!
           :localized-template
           :format))

(defpackage :srfi-29.internal
  (:use :srfi-29 :cl :fiveam :srfi-6)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-29 :format)
  (:shadow :lambda :loop))
