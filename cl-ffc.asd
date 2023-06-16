(defpackage #:cl-ffc-system
  (:use :cl :asdf))

(in-package #:cl-ffc-system)

(defsystem cl-ffc
  :name "cl-ffc"
  :description ""
  :author "Nick Allen <nallen05@gmail.com>"
  :version "0.1"
  :depends-on (:cl-ppcre :trivial-utf-8 :cl-fad)
  :components ((:file "cl-ffc")))
