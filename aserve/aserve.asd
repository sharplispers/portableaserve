;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ASERVE meant to replace
;;;; aserve-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:aserve-system
  (:use #:cl #:asdf))
(in-package #:aserve-system)

(defclass acl-file (cl-source-file) ())
(defmethod asdf:source-file-type ((c acl-file) (s module)) "cl")

#+(or lispworks cmu mcl openmcl clisp)
(defsystem aserve
  :components ((:acl-file "macs")
               (:acl-file "main"
                      :depends-on ("macs"))
               (:acl-file "headers"
                      :depends-on ("main"))
               (:acl-file "parse"
                      :depends-on ("main"))
               (:acl-file "decode"
                      :depends-on ("main"))
               (:acl-file "publish"
                      :depends-on ("main"))
               (:acl-file "authorize"
                      :depends-on ("main" "publish"))
               (:acl-file "log"
                      :depends-on ("main"))
               (:acl-file "client"
                      :depends-on ("main"))
               (:acl-file "proxy"
                      :depends-on ("main")))
  :depends-on (htmlgen acl-compat)
  :perform (load-op :after (op aserve)
		    (pushnew :aserve cl:*features*))
  )

;;; Logical pathname is needed by AllegroServe examples
#+(or lispworks cmu mcl openmcl clisp)
(setf (logical-pathname-translations "ASERVE")
      `(
        #+ignore                        ; Don't need this with asdf
        ("**;*.lisp.*" ;,(logical-pathname "**;*.cl.*")
	 ,(merge-pathnames 
	   (make-pathname :host (pathname-host *load-truename*)
                         :directory '(:relative "aserve" 
                                                 :wild-inferiors)
                         :name :wild
                         :type "cl"
                         :version :wild)
	   *load-truename*
         ))
        ("**;*.*.*" 
         ,(merge-pathnames 
           (make-pathname :host (pathname-host *load-truename*)
                          :directory '(:relative :wild-inferiors)
                          :name    :wild
                          :type    :wild
                          :version :wild
                          ;:case :common
                          )
           *load-truename*))))
#+cmu
(defun cl-user::init-aserve-cmu ()
  ;; this isn't strictly necessary, but scheduling feels very coarse
  ;; without startup-idle-and-top-level-loops, leading to answer delays
  ;; of about 1s per request.
  (unless (find-if
         #'(lambda (proc) (string= (mp:process-name proc) "Top Level Loop"))
         (mp:all-processes))
  (mp::startup-idle-and-top-level-loops)))


