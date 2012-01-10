(cl:in-package :srfi-29.internal)

(def-suite srfi-29)

(in-suite srfi-29)

(let ((translations
       '(((en) . ((time . "Its ~a, ~a.")
                (goodbye . "Goodbye, ~a.")))
         ((fr) . ((time . "~1@*~a, c'est ~a.")
                (goodbye . "Au revoir, ~a."))))))
  (mapc (lambda (translation)
              (let ((bundle-name (cons 'hello-program (car translation))))
                (if (not (load-bundle! bundle-name))
                    (progn
                     (declare-bundle! bundle-name (cdr translation))
                     (store-bundle! bundle-name)))))
             translations))

(define-function localized-message
  (lambda (message-name . args)
    (apply #'format (cons (localized-template 'hello-program
                                              message-name)
                          args))))

(test en-fr
  (let ((loc (current-language)))
    (current-language 'en)
    (is (string= (with-output-to-string (*standard-output*)
                   (let ((myname "Fred"))
                     (display (localized-message 'time "12:00" myname))
                     (display #\newline)

                     (display (localized-message 'goodbye myname))
                     (display #\newline) ))
                 "Its 12:00, Fred.
Goodbye, Fred.
"))
    (current-language 'fr)
    (is (string= (with-output-to-string (*standard-output*)
                   (let ((myname "Fred"))
                     (display (localized-message 'time "12:00" myname))
                     (display #\newline)

                     (display (localized-message 'goodbye myname))
                     (display #\newline) ))
                 "Fred, c'est 12:00.
Au revoir, Fred.
"))
    (current-language loc)))

;; Displays (English):
;; Its 12:00, Fred.
;; Goodbye, Fred.
;;
;; French:
;; Fred, c'est 12:00.
;; Au revoir, Fred.

;;; eof
