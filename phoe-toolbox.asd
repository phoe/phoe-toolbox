;;;; phoe-toolbox.asd

(asdf:defsystem #:phoe-toolbox
  :description "A personal utility library"
  :author "Michał \"phoe\" Herda <phoe@teknik.io>"
  :license "BSD 2-clause"
  :serial t
  :depends-on (#:alexandria
               #:trivial-indent
               #:closer-mop)
  :components ((:file "package")
               (:file "phoe-toolbox")))
