;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ASERVE meant to replace
;;;; aserve-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:aserve-system
  (:use #:cl #:asdf))
(in-package #:aserve-system)

;;;; ignore warnings
;;;;
;;;; FIXME: should better fix warnings instead of ignoring them

(defclass legacy-acl-source-file (cl-source-file.cl)
    ()
  (:documentation
   "Common Lisp source code module with (non-style) warnings.
In contrast to CL-SOURCE-FILE, this class does not think that such warnings
indicate failure."))

(defmethod perform :around ((operation compile-op) (c legacy-acl-source-file))
  (handler-bind (((or style-warning warning) #'muffle-warning))
    (call-next-method)))

#-allegro
(defsystem aserve
    :name "AllegroServe (portable)"
    :author "John K. Foderaro"
    :version "1.2.50"
    :licence "LLGPL"
    :default-component-class cl-source-file.cl
    :components ((:file "packages")
                 (:file "macs" :depends-on ("packages"))
                 (:legacy-acl-source-file "main" :depends-on ("macs"))
                 (:file "headers" :depends-on ("main"))
                 (:legacy-acl-source-file "parse" :depends-on ("main"))
                 (:file "decode" :depends-on ("main"))
                 (:file "publish" :depends-on ("main"))
                 (:file "authorize" :depends-on ("main" "publish"))
                 (:file "log" :depends-on ("main"))
                 (:file "client" :depends-on ("main"))
                 (:file "proxy" :depends-on ("main" "headers"))
                 (:file "cgi" :depends-on ("main"))
                 (:file "playback" :depends-on ("main" "client")))
    :depends-on (htmlgen acl-compat)
    :perform (load-op :after (op aserve)
                      (pushnew :aserve cl:*features*))
    #+asdf3 :perform #+asdf3 (test-op (op c) (load-system :aserve-test :force t))
  )

#+allegro
(defsystem aserve
    :name "AllegroServe (portable)"
    :author "John K. Foderaro"
    :version "1.3.19"
    :licence "LLGPL"
    :default-component-class cl-source-file.cl
    :components ((:file "require-original-aserve")))


;;; Logical pathname is needed by AllegroServe examples
#+(or lispworks cmu mcl openmcl clisp sbcl)
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

(defsystem aserve-test
    :name "Tests for AllegroServe (portable)"
    :author "John K. Foderaro"
    :version "1.2.50"
    :licence "LLGPL"
    :default-component-class cl-source-file.cl
    :components ((:module "test"
                          :components ((:file "t-aserve"))))
    :depends-on (ptester))

#+cmu
(defun cl-user::init-aserve-cmu ()
  ;; this isn't strictly necessary, but scheduling feels very coarse
  ;; without startup-idle-and-top-level-loops, leading to answer delays
  ;; of about 1s per request.
  (unless (find-if
         #'(lambda (proc) (string= (mp:process-name proc) "Top Level Loop"))
         (mp:all-processes))
  (mp::startup-idle-and-top-level-loops)))


