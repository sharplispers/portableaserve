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

#+(and mcl (not openmcl)) (require :ansi-make-load-form)

(defsystem acl-compat
  :components
  (;; Implementation-specific files
   (:gray-streams "vendor-gray-streams")
   ;; nregex and packages
   (:file "nregex")
   (:file "packages" :depends-on ("nregex"))
   ;; Our stream class; support for buffering, chunking and (in the
   ;; future) unified stream exceptions
   #-(or lispworks (and mcl (not openmcl)))
   (:file "lw-buffering" :depends-on ("packages"))
   #-(or allegro (and mcl (not openmcl)))
   (:legacy-cl-source-file "chunked-stream-mixin"
                           :depends-on ("packages" "acl-excl"
                                                   #-lispworks "lw-buffering"))
   ;; Multiprocessing
   #+mcl (:unportable-cl-source-file "mcl-timers")
   (:unportable-cl-source-file "acl-mp"
                               :depends-on ("packages" #+mcl "mcl-timers"))
   ;; Sockets, networking; TODO: de-frob this a bit
   #-mcl
   (:unportable-cl-source-file
    "acl-socket" :depends-on ("packages" "acl-excl"
                                         #-mcl "chunked-stream-mixin"))
   #+(and mcl (not openmcl))
   (:unportable-cl-source-file "acl-socket-mcl" :depends-on ("packages"))
   #+(and mcl (not openmcl) (not carbon-compat)) 
   (:unportable-cl-source-file
    "mcl-stream-fix" :depends-on ("acl-socket-mcl"))
   #+(and mcl openmcl)
   (:unportable-cl-source-file
    "acl-socket-openmcl" :depends-on ("packages" "chunked-stream-mixin"))
   ;; Diverse macros, utility functions
   #-allegro (:file "acl-excl-common" :depends-on ("packages" "nregex"))
   (:unportable-cl-source-file "acl-excl" :depends-on
                               #-allegro ("acl-excl-common")
                               #+allegro ("packages" "nregex"))
   (:unportable-cl-source-file "acl-sys" :depends-on ("packages"))
   ;; URI objects, URI parsing
   (:file "meta")
   (:file "uri" :depends-on ("meta"))
   ;; SSL
   #+(and ssl-available (not (or allegro mcl cmu clisp)))
   (:file "acl-ssl" :depends-on ("acl-ssl-streams" "acl-socket"))
   #+(and ssl-available (not (or allegro mcl cmu clisp)))
   (:file "acl-ssl-streams" :depends-on ("packages"))
   #+nil
   (:legacy-cl-source-file "md5")
   #+nil
   (:legacy-cl-source-file "acl-md5" :depends-on ("acl-excl" "md5")))
  ;; Implementation-specific dependencies
  #+sbcl :depends-on
  #+sbcl (:sb-bsd-sockets :sb-posix)
  #+(and cmu common-lisp-controller) :depends-on
  #+(and cmu common-lisp-controller) (:cmucl-graystream)
  #+(and lispworks ssl-available) :depends-on
  #+(and lispworks ssl-available) (:cl-ssl)
  
  :perform (load-op :after (op acl-compat)
		    (pushnew :acl-compat cl:*features*))
  )
