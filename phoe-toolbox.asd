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

(asdf:defsystem #:phoe-toolbox/bag
  :description "Simple data structure with random O(1) retrieval"
  :author "Michał \"phoe\" Herda <phoe@teknik.io>"
  :license "BSD 2-clause"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "bag")))
