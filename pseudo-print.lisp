;;; pseudo-print.lisp --- print lisp as pseudo-code

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Use `with-pseudo-printer' to print lisp code as pseudo code using
;; the normal printing facilities.  E.g.,
;;
;; PSEUDO-PRINT> (with-pseudo-pprinter
;;                   (pprint '(if (> a b) (+ a b c) 3/4)))
;;
;; If (A > B) Then
;;   (A + B + C)
;; Else
;;   3/4
;; EndIf
;;
;; PSEUDO-PRINT> (with-pseudo-pprinter
;;                   (pprint '(DEFUN EUCLIDS-GCD (A B)
;;                             (IF (= A 0)
;;                                 B
;;                                 (DO ()
;;                                     ((= B 0) A)
;;                                   (IF (> A B)
;;                                       (SETF A (- A B))
;;                                       (SETF B (- B A))))))))
;;
;; Function: EUCLIDS-GCD (A, B)
;;   If (A = 0) Then
;;     B
;;   Else
;;     Do
;;       If (A > B) Then
;;         A <- (A - B)
;;       Else
;;         B <- (B - A)
;;       EndIf
;;     Until (B = 0)
;;     A
;;   EndIf
;; EndFunction
;;
;; Customize using the `pseudo-pprinters' variable.
;;
;; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node253.html


;;; Code:
(in-package :pseudo-print)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defun if-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (if (fourth r)
      (format s "~:<If~; ~W Then~:@_~2I~W~-2I~
                    ~:@_Else~0I~:@_~W~-2I~:@_~;EndIf~:>"
              (cdr r))
      (format s "~:<If~; ~W Then~:@_~2I~W~-2I~:@_~;EndIf~:>"
              (cdr r))))

(defun when-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<Wh~;en ~W Do~:@_~2I~W~-2I~:@_~;EndWhen~:>"
          (cdr r)))

(defvar infix-translations '((equalp . "≡")
                             (equal . "≡")
                             (eql . "≡")
                             (eq . "≡")
                             (append . "++"))
  "Alist used to hold different names for infix functions.")

(defun infix-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s (format nil "~~{~~W~~^ ~a ~~}"
                    (or (cdr (assoc (first r) infix-translations))
                        (first r)))
          (cdr r)))

(defun defun-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<Fu~;nction: ~a (~{~a~^, ~})~:@_~2I~W~-2I~:@_~;EndFunction~:>"
          (cdr r)))

(defun set-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~{~{~W <- ~W~}~^~:@_~}"
          (loop
             :for i :from 0 :by 2 :below (length (cdr r))
             :for j :from 1 :by 2 :below (length (cdr r))
             :collect (list (nth i (cdr r)) (nth j (cdr r))))))

(defun do-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (let ((vars (second r))
        (until (first (third r)))
        (to-return (second (third r)))
        (body (fourth r)))
    (if until
        (format s "~:<Do~;~a ~:@_~2I~W~-2I~:@_Until ~W~;~:>"
                (list (if vars " TODO VARIABLE INITIALIZATION" "") body until))
        (format s "~:<Do~;~a ~:@_~2I~W~-2I~:@_~;EndDo~:>"
                (list (if vars " TODO VARIABLE INITIALIZATION" "") body)))
    (when to-return
      (format s "~:@_~W" to-return))))

(defun let-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<~;~W~^ ~:<~;~@{~:<~;~@{~W~^ <- ~_~}~;~:>~^, ~:_~}~; in~:>~
                ~1I~@{~^ ~_~W~}~;~:>"
          r))

(defun flet-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<~;let~^ ~:<~;~@{~:<~;~@{~W~^ <- ~_~}~;~:>~^, ~:_~}~; in~:>~
                ~1I~@{~^ ~_~W~}~;~:>"
          (cons (mapcar (lambda (f)
                          (list (car f) (append (list 'defun (car f)) (cdr f)))
                          ) (cadr r))
                (cddr r))))

(defun map-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (let ((collection-word (case (car r)
                           (mapc "Do")
                           (mapcar "Collect")
                           (mapcan "Append"))))
    (format s "~:<Fo~;r ~{~W~^, ~} in ~W ~a ~:_~
                      ~2I~{~W~^ ~_~} ~-2I~_~;EndFor~:>"
            (if (listp (second (second r)))
                (list (second (second r))
                      (third r)
                      collection-word
                      (cddr (second r)))
                (list (list 'element)
                      (third r)
                      collection-word
                      (list (list (second (second r)) 'element)))))))

(defun list-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "[~{~W~^, ~_~}]~_" (cdr r)))

(defun prog1-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<Re~;turn ~W After~:@_~2I~@{~W~^ ~_~}~-2I~@:_~;EndAfter~:>"
          (cdr r)))

(defun loop-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<Lo~;op ~2I~@{~W~^ ~_~}~-2I~@:_~;EndLoop~:>" (cdr r)))

(defun destructuring-bind-print (s r colon? atsign?)
  (declare (ignorable colon? atsign?))
  (format s "~:<~;Let~^ ~W <- ~W in~1I~@{~^ ~_~W~}~;~:>"
          (cdr r)))

(defvar pseudo-pprinters
  '((:if-print    (cons (and symbol (eql if))))
    (:when-print  (cons (and symbol (eql when))))
    (:set-print   (cons (and symbol (member set setq setf))))
    (:defun-print (cons (and symbol (eql defun))))
    (:infix-print
     (cons (and symbol (member > < >= <= = + - * /
                               append equalp equal eql eq))))
    (:do-print    (cons (and symbol (eql do))))
    (:let-print   (cons (and symbol (eql let))))
    (:flet-print  (cons (and symbol (eql flet))))
    (:map-print   (cons (and symbol (member mapc mapcar mapcan))))
    (:list-print  (cons (and symbol (eql list))))
    (:prog1-print (cons (and symbol (eql prog1))))
    (:loop-print  (cons (and symbol (eql loop))))
    (:destructuring-bind-print (cons (and symbol (eql destructuring-bind)))))
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
