;;; pseudo-print.lisp --- print lisp as pseudo-code

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; The `pseudo-print' function prints lisp code as pseudo code.

;;; Code:
(in-package :pseudo-print)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar indent-size 2 "Number of spaces for each indentation")

(defun pseudo-print (code &optional (stream nil) (indentation 0))
  ;; Look for something like bind for a pattern matching case statement.
  (flet ((p (word)
           (pprint-tab :line (* indentation indent-size) 0)
           (princ word) (princ " ")))
    (mapcar (lambda (el)
              (cond
                ((equal el :i+) (pprint-newline :mandatory) (incf indentation))
                ((equal el :i-) (pprint-newline :mandatory) (decf indentation))
                ((keywordp el)  (p el))
                (t (pseudo-print el stream indentation))))
            (match code
              ((list 'if cond then else)
               '(:if cond :then :i+ then :i- :else :i+ else :i- :endif))
              ((list 'if cond then)
               '(:if cond :then  :i+ then  :i- :endif))
              ((list 'when cond then)
               '(:when cond :then :i+ then :i- :endwhen))))))


