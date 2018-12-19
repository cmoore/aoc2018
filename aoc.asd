
(asdf:defsystem #:aoc
  :description "Describe cl-ivy here"
  :author "Clint Moore <clint@ivy.io>"
  :license "MIT"
  
  :depends-on (:alexandria
               :cl-ppcre
               :local-time
               :log4cl
               :array-operations)
  
  :components ((:file "aoc")))
