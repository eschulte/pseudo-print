;;; pseudo-print.lisp --- print lisp as pseudo-code

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; The `pseudo-print' function prints lisp code as pseudo code.

;;; Code:
(in-package :pseudo-print)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar pseudo-pprinters nil
  "List of pseudo-printer functions.")

(defun if-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (pprint-tab :line 1 2 s)
  (format s "If ~a Then" (second r))
  (pprint-newline :mandatory s)
  (pprint-tab :line 3 2 s)
  (pprint-linear s (third r))
  (pprint-newline :mandatory s)
  (pprint-tab :line 1 2 s)
  (format s "Else")
  (pprint-newline :mandatory s)
  (pprint-tab :line 3 2 s)
  (pprint-linear s (third r))
  (pprint-newline :mandatory s)
  (pprint-tab :line 1 2 s)
  (format s "EndIf")
  (pprint-newline :mandatory s))

(set-pprint-dispatch
 '(cons (and symbol (eql if)))
 (formatter "~/pseudo-print::if-print/"))

(defmacro with-pseudo-pprinter (&rest body)
  `(unwind-protect
        (progn
          
          ,@body)
     ;; restore original value of pprinter
     (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))))
