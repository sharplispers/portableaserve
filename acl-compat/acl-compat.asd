;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ACL-COMPAT, meant to replace
;;;; acl-compat-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:acl-compat-system
  (:use #:cl #:asdf))
(in-package #:acl-compat-system)

;;;; gray stream support for cmucl: Debian/common-lisp-controller has
;;;; a `cmucl-graystream' system; if this is not found, we assume a
;;;; cmucl downloaded from cons.org, where Gray stream support resides
;;;; in the subsystems/ directory.

#+cmu
(progn

(defclass precompiled-file (static-file)
  ())

(defmethod perform ((operation load-op) (c precompiled-file))
  (load (component-pathname c)))

(defmethod operation-done-p ((operation load-op) (c precompiled-file))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (asdf:find-system :cmucl-graystream nil)
    (asdf:defsystem cmucl-graystream
        :pathname (make-pathname
                   :name nil :type nil :version nil
                   :defaults (truename "library:subsystems/gray-streams-library.x86f"))
      :components ((:precompiled-file "gray-streams-library.x86f")))))

;; KLUDGE: cmucl-graystream apparently isn't loaded before acl-compat
;; starts compiling, leading to an error in lw-buffering.  Investigate
;; that sometime, load unconditionally for now.
(asdf:operate 'asdf:load-op :cmucl-graystream)
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
  (
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
   (:file "meta")                       ; Still needed inside main.cl
   ;; SSL
   #+(and ssl-available (not (or allegro mcl clisp)))
   (:file "acl-ssl" :depends-on ("acl-ssl-streams" "acl-socket"))
   #+(and ssl-available (not (or allegro mcl clisp)))
   (:file "acl-ssl-streams" :depends-on ("packages")))
  ;; Dependencies
  :depends-on (:puri)
  ;; Implementation-specific dependencies
  #+sbcl :depends-on #+sbcl (:sb-bsd-sockets :sb-posix)
  #+cmu :depends-on #+cmu (:cmucl-graystream)
  #+(and (or cmu lispworks) ssl-available) :depends-on
  #+(and (or cmu lispworks) ssl-available) (:cl-ssl)
  
  :perform (load-op :after (op acl-compat)
		    (pushnew :acl-compat cl:*features*))
  )
