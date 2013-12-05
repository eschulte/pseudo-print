;;; pseudo-print.lisp --- print lisp as pseudo-code

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Use `with-pseudo-printer' to print lisp code as pseudo code using
;; the normal printing facilities.  E.g.,
;;
;; > (with-pseudo-pprinter (format t "~W" '(if (> x 0) (+ 1 2 3) 3/4)))
;;
;; If (X > 0) Then
;;   (1 + 2 + 3)
;; Else
;;   3/4
;; EndIf

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
  (when (fourth r)
    (format s "~0@TElse~:@_")
    (format s "~2@T~W~:@_"(fourth r)))
  (format s "~0@TEndIf"))

(defun infix-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s (format nil "(~~{~~W~~^ ~W ~~})" (first r)) (cdr r)))

(defun defun-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:@_~0@TFunction: ~a(~{~a~^, ~})" (second r) (third r))
  (format s "~2I~{~W~^~:@_~}" (cdddr r)))

(defun setf-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~{~{~W <- ~W~}~^~:@_~}"
          (loop
             :for i :from 0 :by 2 :below (length (cdr r))
             :for j :from 1 :by 2 :below (length (cdr r))
             :collect (list (nth i (cdr r)) (nth j (cdr r))))))

(defmacro with-pseudo-pprinter (&rest body)
  (let ((orig-tab (gensym)))
    `(let ((,orig-tab (copy-pprint-dispatch *print-pprint-dispatch*)))
       (unwind-protect
            (progn
              (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))
              (set-pprint-dispatch '(cons (and symbol (eql if)))
                                   (formatter "~/pseudo-print::if-print/"))
              (set-pprint-dispatch '(cons (and symbol (member > < = + - * /)))
                                   (formatter "~/pseudo-print::infix-print/"))
              (set-pprint-dispatch '(cons (and symbol (eql defun)))
                                   (formatter "~/pseudo-print::defun-print/"))
              (set-pprint-dispatch '(cons (and symbol (eql setf)))
                                   (formatter "~/pseudo-print::setf-print/"))
              ,@body)
         ;; restore original value of pprinter
         (setq *print-pprint-dispatch* ,orig-tab)))))
