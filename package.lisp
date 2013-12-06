(defpackage #:pseudo-print
  (:use :common-lisp :alexandria :curry-compose-reader-macros)
  (:export :with-pseudo-printer :pseudo-pprinters))
