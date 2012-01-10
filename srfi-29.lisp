;;;; srfi-29.lisp

(cl:in-package :srfi-29.internal)
;; (in-readtable :srfi-29)

;;; "srfi-29" goes here. Hacks and glory await!

;; The association list in which bundles will be stored
(defvar *localization-bundles* '())

;; The current-language and current-country functions provided
;; here must be rewritten for each Scheme system to default to the
;; actual locale of the session
(define-function current-language
  (let ((current-language-value 'en))
    (lambda args
      (if (null? args)
          current-language-value
          (set! current-language-value (car args))))))

(define-function current-country
  (let ((current-country-value 'us))
    (lambda args
      (if (null? args)
          current-country-value
          (set! current-country-value (car args))))))

;; The load-bundle! and store-bundle! both return #f in this
;; reference implementation.  A compliant implementation need
;; not rewrite these procedures.
(define-function load-bundle!
  (lambda (bundle-specifier)
    (declare (ignore bundle-specifier))
    nil))

(define-function store-bundle!
  (lambda (bundle-specifier)
    (declare (ignore bundle-specifier))
    nil))

;; Declare a bundle of templates with a given bundle specifier
(define-function declare-bundle!
  (letrec ((remove-old-bundle
            (lambda (specifier bundle)
              (cond ((null? bundle) '())
                    ((equal (caar bundle) specifier)
                     (cdr bundle) )
                    (:else (cons (car bundle)
                                 (remove-old-bundle specifier
                                                    (cdr bundle) )))))))
    (lambda (bundle-specifier bundle-assoc-list)
      (declare (optimize (debug 0)))
      (set! *localization-bundles*
            (cons (cons bundle-specifier bundle-assoc-list)
                  (remove-old-bundle bundle-specifier
                                     *localization-bundles* ))))))

;;Retrieve a localized template given its package name and a template name
(define-function localized-template
  (letrec ((rdc
            (lambda (ls)
              (if (null? (cdr ls))
                  '()
                  (cons (car ls) (rdc (cdr ls))) )))
           (find-bundle
            (lambda (specifier template-name)
              (srfi:cond ((assoc specifier *localization-bundles* :test #'equal)
                          :=> #'values)
                         ((null? specifier) nil)
                         (:else (find-bundle (rdc specifier)
                                          template-name ))))))
    (lambda (package-name template-name)
      (declare (optimize (debug 0)))
      (let loop ((specifier (cons package-name
                                  (list (current-language)
                                        (current-country) ))))
           (and (not (null? specifier))
                (let ((bundle (find-bundle specifier template-name)))
                  (and bundle
                       (srfi:cond ((assoc template-name bundle :test #'eq)
                                   :=> #'cdr )
                                  ((null? (cdr specifier)) nil)
                                  (:else (loop (rdc specifier))) ))))))))

;;An SRFI-28 and SRFI-29 compliant version of format.  It requires
;;SRFI-23 for error reporting.
(define-function format
  (lambda (format-string . objects)
    (declare (optimize (debug 1)))
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects)
                 (object-override nil) )
           (declare (optimize (debug 0)))
           (cond ((null? format-list) (get-output-string buffer))
                 ((char=? (car format-list) #\~)
                  (cond ((null? (cdr format-list))
                         (error 'format "Incomplete escape sequence") )
                        ((char-numeric? (cadr format-list))
                         (let posloop ((fl (cddr format-list))
                                       (pos (string->number
                                             (string (cadr format-list)) )))
                              (declare (optimize (debug 0)))
                              (cond ((null? fl)
                                     (error 'format "Incomplete escape sequence") )
                                    ((and (eq? (car fl) '#\@)
                                          (null? (cdr fl)) )
                                     (error 'format "Incomplete escape sequence") )
                                    ((and (eq? (car fl) '#\@)
                                          (eq? (cadr fl) '#\*) )
                                     (loop (cddr fl) objects (list-ref objects pos)) )
                                    (:else
                                     (posloop (cdr fl)
                                              (+ (* 10 pos)
                                                 (string->number
                                                  (string (car fl)) )))))))
                        (:else
                         (case (cadr format-list)
                           ((#\a #\A)
                            (cond (object-override
                                   (progn
                                     (display object-override buffer)
                                     (loop (cddr format-list) objects nil) ))
                                  ((null? objects)
                                   (error 'format "No value for escape sequence") )
                                  (:else
                                   (progn
                                     (display (car objects) buffer)
                                     (loop (cddr format-list)
                                           (cdr objects) nil)))))
                           ((#\s #\S)
                            (cond (object-override
                                   (progn
                                     (display object-override buffer)
                                     (loop (cddr format-list) objects nil) ))
                                  ((null? objects)
                                   (error 'format "No value for escape sequence") )
                                  (:else
                                   (progn
                                     (write (car objects) :stream buffer)
                                     (loop (cddr format-list)
                                           (cdr objects) nil)))))
                           ((#\%)
                            (if object-override
                                (error 'format "Escape sequence following positional override does not require a value") )
                            (display #\newline buffer)
                            (loop (cddr format-list) objects nil) )
                           ((#\~)
                            (if object-override
                                (error 'format "Escape sequence following positional override does not require a value") )
                            (display #\~ buffer)
                            (loop (cddr format-list) objects nil) )
                           (oterwise
                            (error 'format "Unrecognized escape sequence") )))))
                 (:else (display (car format-list) buffer)
                        (loop (cdr format-list) objects nil) ))))))
