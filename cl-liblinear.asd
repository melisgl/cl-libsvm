;;;; -*- mode: Lisp -*-

(cl:defpackage #:cl-liblinear.system
  (:use #:cl #:asdf))

(cl:in-package #:cl-liblinear.system)

(defsystem #:cl-liblinear
  :name "CL-LIBLINEAR"
  :description "CFFI wrapper for LIBLINEAR"
  :long-description "CFFI wrapper for LIBLINEAR, the machine learning library"
  :author "Gabor Melis"
  :version "0.0.1"
  :licence "MIT"
  :components ((:file "liblinear-package")
               (:file "liblinear"))
  :serial t
  :depends-on (cffi trivial-garbage))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-liblinear))))
  (funcall (intern (symbol-name '#:test)
                   (find-package '#:cl-liblinear))))
