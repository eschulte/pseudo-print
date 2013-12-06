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

(defun if-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (if (fourth r)
      (format s "~:<If~; ~W Then~:@_~2I~W~-2I~:@_Else~0I~:@_~W~-2I~:@_~;EndIf~:>"
              (cdr r))
      (format s "~:<If~; ~W Then~:@_~2I~W~-2I~:@_~;EndIf~:>"
              (cdr r))))

(defun when-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<Wh~;en ~W Do~:@_~2I~W~-2I~:@_~;EndWhen~:>"
          (cdr r)))

(defun infix-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s (format nil "(~~{~~W~~^ ~W ~~})" (first r)) (cdr r)))

(defun defun-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:@_~0@TFunction: ~a(~{~a~^, ~})" (second r) (third r))
  (format s "~2I~:@_~{~W~^~:@_~}" (cdddr r)))

(defun set-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~{~{~W <- ~W~}~^~:@_~}"
          (loop
             :for i :from 0 :by 2 :below (length (cdr r))
             :for j :from 1 :by 2 :below (length (cdr r))
             :collect (list (nth i (cdr r)) (nth j (cdr r))))))

(defun do-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "Do~:@_")
  (let ((vars (second r))
        (until (first (third r)))
        (to-return (second (third r)))
        (body (fourth r)))
    (when vars
      (format s "TODO VARIABLE INITIALIZATION"))
    (format s "~2I~W" body)
    (when until
      (format s "~:@_Until ~W" until))
    (when to-return
      (format s "~:@_~W" to-return))))

(defvar pseudo-pprinters
  '((:if-print    (cons (and symbol (eql if))))
    (:when-print  (cons (and symbol (eql when))))
    (:set-print   (cons (and symbol (member set setq setf))))
    (:defun-print (cons (and symbol (eql defun))))
    (:infix-print (cons (and symbol (member > < = + - * /))))
    (:do-print    (cons (and symbol (eql do)))))
  "List of pseudo-printer functions.")

(defmacro with-pseudo-pprinter (&rest body)
  (let ((orig-tab (gensym)))
    `(let ((,orig-tab (copy-pprint-dispatch *print-pprint-dispatch*)))
       (unwind-protect
            (progn
              (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))
              ,@(mapcar
                 (lambda (pair)
                   `(set-pprint-dispatch
                     ',(second pair)
                     (formatter ,(format nil "~~/pseudo-print::~a/"
                                         (symbol-name (first pair))))))
                 pseudo-pprinters)
              ,@body)
         ;; restore original value of pprinter
         (setq *print-pprint-dispatch* ,orig-tab)))))
