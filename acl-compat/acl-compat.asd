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

    ;; Debian cmucl distribution
    #+(and common-lisp-controller (not gray-streams))
    (require :cmucl-graystream)
    ;; vanilla cmucl
    #+(and (not common-lisp-controller) (not gray-streams))
    (progn (load "library:subsystems/gray-streams-library")
           (pushnew :gray-streams *features*))

(defmethod perform ((operation compile-op) (component gray-streams))
  ;; Debian cmucl distribution
  #+(and cmu common-lisp-controller (not gray-streams))
  (require :cmucl-graystream)
  ;; vanilla cmucl
  #+(and cmu (not common-lisp-controller) (not gray-streams))
  (progn (load "library:subsystems/gray-streams-library")
         (pushnew :gray-streams *features*)))


(defmethod perform ((operation load-op) (component gray-streams))
  ;; Debian cmucl distribution
  #+(and cmu common-lisp-controller (not gray-streams))
  (require :cmucl-graystream)
  ;; vanilla cmucl
  #+(and cmu (not common-lisp-controller) (not gray-streams))
  (progn (load "library:subsystems/gray-streams-library")
         (pushnew :gray-streams *features*)))

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

;;;; system

(defsystem acl-compat
  :components ((:gray-streams "vendor-gray-streams")
	       (:file "nregex")
	       (:file "acl-mp-package")
	       (:file "acl-mp-cmu"
		      :depends-on ("acl-mp-package"))
	       (:file "acl-excl-cmu"
		      :depends-on ("vendor-gray-streams" "nregex"))
	       (:file "cmu-read-sequence")
	       (:file "acl-socket-cmu"
		      :depends-on ("acl-excl-cmu" "cmu-read-sequence"))
	       (:file "acl-sys-cmu")
	       (:file "meta")
	       (:file "uri"
		      :depends-on ("meta"))
	       (:legacy-cl-source-file "chunked"
		      :depends-on ("acl-excl-cmu"))))
