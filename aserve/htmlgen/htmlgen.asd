;;; -*- mode: lisp -*-

(defpackage #:htmlgen-system
  (:use #:cl #:asdf))
(in-package #:htmlgen-system)

(defclass acl-file (cl-source-file) ())
(defmethod asdf::source-file-type ((c acl-file) (s module)) "cl")

(defsystem htmlgen
  :components ((:acl-file "htmlgen"))
  #-allegro :depends-on #-allegro (acl-compat))
