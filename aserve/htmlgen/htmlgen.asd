;;; -*- mode: lisp -*-

(defpackage #:htmlgen-system
  (:use #:cl #:asdf))
(in-package #:htmlgen-system)

(defclass acl-file (cl-source-file) ())
(defmethod asdf::source-file-type ((c acl-file) (s module)) "cl")

#+(or lispworks cmu mcl openmcl clisp sbcl)
(defsystem htmlgen
  :components ((:acl-file "htmlgen"))
  #-allegro :depends-on #-allegro (acl-compat)
  :perform (load-op :after (op htmlgen)
		    (pushnew :htmlgen cl:*features*))
  )

