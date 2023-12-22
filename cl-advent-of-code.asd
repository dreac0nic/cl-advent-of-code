;;;; cl-advent-of-code.asd

(asdf:defsystem #:cl-advent-of-code
  :author "dreac0nic <spenser.m.bray@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:arrows #:dexador #:cl-cookie #:cl-ppcre #:lquery #:uiop)
  :serial t
  :components ((:file "package")
               (:file "cl-advent-of-code"))
  :description "A basic library for interacting with the advent of code API"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md")))
