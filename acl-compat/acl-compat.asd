;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ACL-COMPAT, meant to replace
;;;; acl-compat-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:acl-compat-system
  (:use #:cl #:asdf))
(in-package #:acl-compat-system)

;;;; load gray stream support

(defclass library-component (component) ())

(defmethod asdf::input-files ((operation load-op) (component library-component))
  nil)

(defmethod asdf::output-files ((operation load-op) (component library-component))
  nil)

(defmethod asdf::operation-done-p ((operaton compile-op) (component library-component))
  "Always need to compile a library component"
  nil)

(defmethod asdf::operation-done-p ((operaton load-op) (component library-component))
  "Always need to load a library component"
  nil)


(defclass gray-streams (library-component) ())

(defmethod perform ((operation compile-op) (component gray-streams))
  ;; vanilla cmucl
  #+(and cmu (not common-lisp-controller) (not gray-streams))
  (progn (load "library:subsystems/gray-streams-library")
         (pushnew :gray-streams *features*)))

(defmethod perform ((operation load-op) (component gray-streams))
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

;;;
;;; This is thought to reduce reader-conditionals in the system definition
;;;
(defclass unportable-cl-source-file (cl-source-file) ()
  (:documentation
   "This is for files which contain lisp-system dependent code. Until now those
are marked by a -system postfix but we could later change that to a directory per
lisp-system"))

(defun lisp-system-shortname ()
  #+allegro :allegro #+lispworks :lispworks #+cmu :cmucl
  #+mcl :mcl #+clisp :clisp #+scl :scl #+sbcl :sbcl) ;mcl/openmcl use the same directory

(defmethod component-pathname ((component unportable-cl-source-file))
  (let ((pathname (call-next-method))
        (name (string-downcase (lisp-system-shortname))))
    (merge-pathnames
     (make-pathname :directory (list :relative name))
     pathname)))

;;;; system


;standard MCL make-load-form is not ansi compliant because of CLIM
#+(and mcl (not openmcl)) (require :ansi-make-load-form)

;want to include it with the rest - but I'm afraid ... maybe later
#+(or mcl openmcl)
(defsystem acl-compat
   :components ((:file "nregex")
                (:unportable-cl-source-file "mcl-timers")
                (:unportable-cl-source-file "acl-mp" :depends-on ("mcl-timers"))
                #-openmcl
                (:unportable-cl-source-file "acl-socket-mcl")
                #+(and (not openmcl) (not carbon-compat)) 
                (:unportable-cl-source-file "mcl-stream-fix" :depends-on ("acl-socket-mcl"))
                #+openmcl 
                (:unportable-cl-source-file "acl-socket-openmcl")
                (:unportable-cl-source-file "acl-excl" :depends-on ("nregex"))
                (:unportable-cl-source-file "acl-sys")
                (:file "meta")
                (:file "uri" :depends-on ("meta")))
   :perform (load-op :after (op acl-compat)
		     (pushnew :acl-compat cl:*features*)))


#+(or lispworks cmu scl sbcl clisp allegro)
(defsystem acl-compat
  :components ((:gray-streams "vendor-gray-streams")
	       (:file "nregex")
               (:file "packages" :depends-on ("nregex"))
               #-lispworks (:file "lw-buffering" :depends-on ("packages"))
	       (:unportable-cl-source-file "acl-mp"
 		      :depends-on ("packages" "acl-socket"
                                   ;"acl-mp-package"
                                   ))
	       (:unportable-cl-source-file "acl-excl"
		      :depends-on ("packages" "nregex"))
               ;; Hack for old versions of cmucl that did not
               ;; implement Gray stream handling in read-sequence,
               ;; write-sequence
	       (:unportable-cl-source-file "acl-socket"
		   :depends-on ("packages" "acl-excl"
					   #-(or mcl allegro)
                                           "chunked-stream-mixin"))
	       (:unportable-cl-source-file "acl-sys" :depends-on ("packages"))
	       (:file "meta")
	       (:file "uri" :depends-on ("meta"))
	       #-(or allegro mcl)
	       (:legacy-cl-source-file "chunked-stream-mixin"
		      :depends-on ("packages"
                                   "acl-excl"
                                   #-lispworks "lw-buffering"))
	       #+(and ssl-available (not (or allegro mcl cmu clisp)))
               (:file "acl-ssl" :depends-on ("acl-ssl-streams" "acl-socket"))
	       #+(and ssl-available (not (or allegro mcl cmu clisp)))
               (:file "acl-ssl-streams" :depends-on ("packages"))
               #+nil
               (:legacy-cl-source-file "md5")
               #+nil
	       (:legacy-cl-source-file "acl-md5" :depends-on ("acl-excl" "md5")))
  
  #+sbcl :depends-on
  #+sbcl (:sb-bsd-sockets)
  #+(and cmu common-lisp-controller) :depends-on
  #+(and cmu common-lisp-controller) (:cmucl-graystream)
  #+(and lispworks ssl-available) :depends-on
  #+(and lispworks ssl-available) (:cl-ssl)
  
  :perform (load-op :after (op acl-compat)
		    (pushnew :acl-compat cl:*features*))
  )
