(defpackage #:webactions-system
  (:use #:cl #:asdf))
(in-package #:webactions-system)

(defclass acl-file (cl-source-file) ())
(defmethod asdf:source-file-type ((c acl-file) (s module)) "cl")

(defclass legacy-acl-source-file (acl-file)
    ()
  (:documentation
   "Common Lisp source code module with (non-style) warnings.
In contrast to CL-SOURCE-FILE, this class does not think that such warnings
indicate failure."))

(defmethod perform ((operation compile-op) (c legacy-acl-source-file))
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

(defsystem webactions
    :components
  ((:acl-file "websession")
   (:acl-file "webact" :depends-on ("websession"))
   (:acl-file "clpage" :depends-on ("webact"))
   (:module :clpcode
	    :components
	    ((:acl-file "clp")
	     (:acl-file "http")
	     (:acl-file "time")
	     (:acl-file "wa"))
	    :depends-on ("clpage")))
  :depends-on (aserve acl-compat htmlgen))
