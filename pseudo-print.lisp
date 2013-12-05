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
  (format s "~:@_~0@TIf ~a Then~:@_" (second r))
  (format s "~2@T~W~:@_"(third r))
  (format s "~0@TElse~:@_")
  (format s "~2@T~W~:@_"(fourth r))
  (format s "~0@TEndIf~:@_"))

(defmacro with-pseudo-pprinter (&rest body)
  `(unwind-protect
        (progn
          (set-pprint-dispatch '(cons (and symbol (eql if)))
                               (formatter "~/pseudo-print::if-print/"))
          ,@body)
     ;; restore original value of pprinter
     (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))))
