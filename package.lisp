;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-29"
  (:use)
  (:export
   current-language
   current-country
   current-locale-details
   declare-bundle!
   store-bundle
   load-bundle!
   localized-template
   format)
  (:size 9))


(in-package "https://github.com/g000001/srfi-29")


(cl:declaim
 (cl:ftype cl:function
           localized-template current-language format
           current-country declare-bundle! load-bundle!))


(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-29#internals"
  (:use
   "https://github.com/g000001/srfi-29"
   "https://github.com/g000001/srfi-23"
   "https://github.com/g000001/srfi-6"
   "https://github.com/g000001/srfi-61"
   cl
   fiveam )
  (:shadowing-import-from
   "https://github.com/g000001/srfi-23" error)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-61" cond)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-5" let)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-29" format)
  (:shadow lambda loop))


;;; *EOF*
