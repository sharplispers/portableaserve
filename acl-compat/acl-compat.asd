;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ACL-COMPAT, meant to replace
;;;; acl-compat-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:acl-compat-system
  (:use #:cl #:asdf))
(in-package #:acl-compat-system)

;;;; load gray stream support

;; Awaiting cmucl-graystreams to move to ASDF

#+common-lisp-controller (pushnew :cmucl-not-yet-asdf cl:*features*)

(defclass gray-streams (component) ())

(defmethod perform ((operation compile-op) (component gray-streams))
#+(and cmu (not graystreams) cmucl-not-yet-asdf) (require :cmucl-graystream)
  ;; vanilla cmucl
  #+(and cmu (not common-lisp-controller) (not gray-streams))
  (progn (load "library:subsystems/gray-streams-library")
         (pushnew :gray-streams *features*))
  ;; LispWorks (it's already there)
  #+lispworks (lw:do-nothing))

(defmethod perform ((operation load-op) (component gray-streams))
#+(and cmu (not graystreams) cmucl-not-yet-asdf) (require :cmucl-graystream)
  ;; vanilla cmucl
  #+(and cmu (not common-lisp-controller) (not gray-streams))
  (progn (load "library:subsystems/gray-streams-library")
         (pushnew :gray-streams *features*))
  ;; LispWorks it's already there
  #+lispworks (lw:do-nothing))

#+common-lisp-controller
(defmethod perform ((operation load-compiled-op) (component gray-streams))
#+(and cmu (not graystreams) cmucl-not-yet-asdf) (require :cmucl-graystream)
  ;; Nothing to do, uses ASDF :depends-on
)	


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
  #+acl :acl #+lispworks :lw #+cmu :cmu #+mcl :mcl #+scl :scl)

(defmethod component-pathname ((component unportable-cl-source-file))
  (let ((pathname (call-next-method)))
    (make-pathname :name (format nil "~A-~A" (pathname-name pathname) 
                                 (string-downcase (lisp-system-shortname)))
                   :defaults pathname)))

;;;; system


;standard MCL make-load-form is not ansi compliant because of CLIM
#+(and mcl (not openmcl)) (require :ansi-make-load-form)

#+(or lispworks cmu scl mcl openmcl)
(defsystem acl-compat
  :components ((:gray-streams "vendor-gray-streams")
	       (:file "nregex")
               (:file "packages" :depends-on ("nregex"))
	       #+mcl (:file "mcl-timers")
	       ;;(:file "acl-mp-package")
	       (:unportable-cl-source-file "acl-mp"
 		      :depends-on ("packages"
                                   ;"acl-mp-package"
				   #-mcl "acl-socket"
				   #+(and mcl openmcl) "acl-socket-openmcl"
				   #+(and mcl (not openmcl)) "acl-socket-mcl"
				   #+mcl "mcl-timers"))
	       (:unportable-cl-source-file "acl-excl"
		      :depends-on ("packages" #+nil "gray-stream-package" "nregex"))
               ;; Debian cmucl has gray stream support for
               ;; read-/write-sequence, cons.org cmucl has it
               ;; commented out in src/stream.lisp, so we leave the
               ;; hack in for the time being... (bug reported to
               ;; cmucl-imp, tho)
	       #+(and cmu (not common-lisp-controller))
               (:file "cmu-read-sequence")
	       #+(and mcl (not openmcl)) (:file "acl-socket-mcl"
                                                :depends-on ("packages"))
	       #+(and mcl openmcl) (:file "acl-socket-openmcl"
                                                :depends-on ("packages"))
	       #-mcl (:unportable-cl-source-file "acl-socket"
		      :depends-on ("packages" "acl-excl" "chunked-stream-mixin" 
                                   #+(and cmu (not common-lisp-controller))
                                   "cmu-read-sequence"))
	       (:unportable-cl-source-file "acl-sys" :depends-on ("packages"))
	       (:file "meta")
	       #+openmcl (:file "ansi-loop")
	       (:file "uri"
		      :depends-on ("meta" #+openmcl "ansi-loop"))
               ;;(:file "gray-stream-package"
               ;; :depends-on ("vendor-gray-streams"))
	       (:legacy-cl-source-file "chunked-stream-mixin"
		      :depends-on ("packages" "acl-excl" #+nil "gray-stream-package"))
               #+nil
               (:legacy-cl-source-file "md5")
               #+nil
	       (:legacy-cl-source-file "acl-md5" :depends-on ("acl-excl" "md5")))
  #+(and cmu common-lisp-controller (not gray-streams) (not cmucl-not-yet-asdf))
  :depends-on
  #+(and cmu common-lisp-controller (not gray-streams) (not cmucl-not-yet-asdf))
  (:cmucl-graystream)

  :perform (load-op :after (op acl-compat)
		    (pushnew :acl-compat cl:*features*))
  )

#+(or lispworks cmu mcl openmcl scl)
(when (ignore-errors (find-class 'load-compiled-op))
  (defmethod perform :after ((op load-compiled-op) (c (eql (find-system 'acl-compat))))
    (pushnew :acl-compat cl:*features*)))
