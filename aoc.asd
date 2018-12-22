
(asdf:defsystem #:aoc
  :description "Describe cl-ivy here"
  :author "Clint Moore <clint@ivy.io>"
  :license "MIT"
  
  :depends-on (:alexandria
               :cl-ppcre
               :sketch
               :local-time
               :log4cl
               :lparallel
               :iterate
               :array-operations)
  
  :components ((:file "goldlists")
               (:file "aoc")))
