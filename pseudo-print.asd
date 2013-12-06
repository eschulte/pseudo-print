(defsystem :pseudo-print
  :description "print lisp as pseudo-code"
  :version "0.0.0"
  :author ("Eric Schulte <schulte.eric@gmail.com>")
  :licence "GPL V3"
  :depends-on (alexandria curry-compose-reader-macros)
  :components
  ((:file "package") (:file "pseudo-print" :depends-on ("package"))))
