;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ACL-COMPAT, meant to replace
;;;; acl-compat-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:acl-compat-system
  (:use #:cl #:asdf))
(in-package #:acl-compat-system)

;;;; load gray stream support

(defclass gray-streams (component) ())

(defmethod perform ((operation compile-op) (component gray-streams))
  ;; Debian cmucl distribution
  #+(and cmu common-lisp-controller (not gray-streams))
  (require :cmucl-graystream)
  ;; vanilla cmucl
  #+(and cmu (not common-lisp-controller) (not gray-streams))
  (progn (load "library:subsystems/gray-streams-library")
         (pushnew :gray-streams *features*))
  ;; LispWorks (it's already there)
  #+lispworks (lw:do-nothing))

(defmethod perform ((operation load-op) (component gray-streams))
  ;; Debian cmucl distribution
  #+(and cmu common-lisp-controller (not gray-streams))
  (require :cmucl-graystream)
  ;; vanilla cmucl
  #+(and cmu (not common-lisp-controller) (not gray-streams))
  (progn (load "library:subsystems/gray-streams-library")
         (pushnew :gray-streams *features*))
  ;; LispWorks it's already there
  #+lispworks (lw:do-nothing))

;;;; ignore warnings
;;;;
;;;; FIXME: should better fix warnings instead of ignoring them
;;;; FIXME: (perform legacy-cl-sourcefile) duplicates ASDF code

(defclass legacy-cl-source-file (cl-source-file)
    ()
  (:documentation
   "Common Lisp source code module with (non-style) warnings.
In contrast to CL-SOURCE-FILE, this class does not think that such warnings
indicate failure."))

(defmethod perform ((operation compile-op) (c legacy-cl-source-file))
  (let ((source-file (component-pathname c))
	(output-file (car (output-files operation c)))
	(warnings-p nil)
	(failure-p nil))
    (setf (asdf::component-property c 'last-compiled) nil)
    (handler-bind ((warning (lambda (c)
			      (declare (ignore c))
			      (setq warnings-p t)))
		   ;; _not_ (or error (and warning (not style-warning)))
		   (error (lambda (c)
			    (declare (ignore c))
			    (setq failure-p t))))
      (compile-file source-file
		    :output-file output-file))
    ;; rest of this method is as for CL-SOURCE-FILE
    (setf (asdf::component-property c 'last-compiled) (file-write-date output-file))
    (when warnings-p
      (case (asdf::operation-on-warnings operation)
	(:warn (warn "COMPILE-FILE warned while performing ~A on ~A"
		     c operation))
	(:error (error 'compile-warned :component c :operation operation))
	(:ignore nil)))
    (when failure-p
      (case (asdf::operation-on-failure operation)
	(:warn (warn "COMPILE-FILE failed while performing ~A on ~A"
		     c operation))
	(:error (error 'compile-failed :component c :operation operation))
	(:ignore nil)))))

;;;
;;; This is thought to reduce reader-conditionals in the system definition
;;;
(defclass unportable-cl-source-file (cl-source-file) ()
  (:documentation
   "This is for files which contain lisp-system dependent code. Until now those
are marked by a -system postfix but we could later change that to a directory per
lisp-system"))

(defun lisp-system-shortname ()
  #+acl :acl #+lispworks :lw #+cmu :cmu #+mcl :mcl #+openmcl :openmcl)

(defmethod component-pathname ((component unportable-cl-source-file))
  (let ((pathname (call-next-method)))
    (make-pathname :name (format nil "~A-~A" (pathname-name pathname) 
                                 (string-downcase (lisp-system-shortname)))
                   :defaults pathname)))

;;;; system

(defsystem acl-compat
  :components ((:gray-streams "vendor-gray-streams")
	       (:file "nregex")
	       (:file "acl-mp-package")
	       (:unportable-cl-source-file "acl-mp"
                      :depends-on ("acl-mp-package" "acl-socket"))
	       (:unportable-cl-source-file "acl-excl"
		      :depends-on ("gray-stream-package" "nregex"))
	       #+cmu(:file "cmu-read-sequence")
	       (:unportable-cl-source-file "acl-socket"
		      :depends-on ("acl-excl" "chunked-stream-mixin" 
                                              #+cmu "cmu-read-sequence"))
	       (:unportable-cl-source-file "acl-sys")
	       (:file "meta")
	       (:file "uri"
		      :depends-on ("meta"))
               (:file "gray-stream-package"
                :depends-on ("vendor-gray-streams"))
	       (:legacy-cl-source-file "chunked-stream-mixin"
		      :depends-on ("acl-excl" "gray-stream-package"))
               #+nil
               (:legacy-cl-source-file "md5")
               #+nil
               (:legacy-cl-source-file "acl-md5" :depends-on ("acl-excl" "md5"))))
