;;; -*- mode: lisp -*-

(defpackage #:htmlgen-system
  (:use #:cl #:asdf))
(in-package #:htmlgen-system)

(defclass acl-file (cl-source-file) ())
(defmethod asdf::source-file-type ((c acl-file) (s module)) "cl")

#+(or lispworks cmu mcl openmcl)
(defsystem htmlgen
  :components ((:acl-file "htmlgen"))
  #-allegro :depends-on #-allegro (acl-compat)
  :perform (load-op :after (op htmlgen)
		    (pushnew :htmlgen cl:*features*))
  )

#+(or lispworks cmu mcl openmcl)
(when (ignore-errors (find-class 'load-compiled-op))
  (defmethod perform :after ((op load-compiled-op) (c (eql (find-system :htmlgen))))
    (pushnew :htmlgen cl:*features*)))
