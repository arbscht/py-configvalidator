(cl:defpackage #:nz.geek.abhishek.py-configvalidator
  (:use #:cl #:asdf))

(in-package #:nz.geek.abhishek.py-configvalidator)

(defsystem py-configvalidator
    :name "py-configvalidator"
    :author "Abhishek Reddy"
    :version "0.1"
    :license "MIT"
    :description "A validation layer for py-configparser"
    :depends-on (#:py-configparser #:cl-utilities)
    :serial t
    :components ((:module "src"
                  :components ((:file "package")
                               (:file "validator")
                               (:file "config")))))
