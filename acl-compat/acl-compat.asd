;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ACL-COMPAT, meant to replace
;;;; acl-compat-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :fiveam-asdf))

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

#-gray-streams
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (asdf:find-system :cmucl-graystream nil)
    (asdf:defsystem cmucl-graystream
        :pathname (make-pathname
                   :name nil :type nil :version nil
                   :defaults (truename "library:subsystems/gray-streams-library.x86f"))
      :components ((:precompiled-file "gray-streams-library.x86f")))))
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

(defmethod perform :around ((operation compile-op) (c legacy-cl-source-file))
  (handler-bind (((or style-warning warning) #'muffle-warning))
    (call-next-method)))

;;;
;;; This is thought to reduce reader-conditionals in the system definition
;;;
(defclass unportable-cl-source-file (cl-source-file) ()
  (:documentation
   "This is for files which contain lisp-system dependent code. Until now those
are marked by a -system postfix but we could later change that to a directory per
lisp-system"))

(defmethod perform ((op load-op) (c unportable-cl-source-file))
  (#+cmu ext:without-package-locks
   #-(or cmu) progn
     (call-next-method)))

(defmethod perform ((op compile-op) (c unportable-cl-source-file))
  (#+cmu ext:without-package-locks
   #-(or cmu) progn
     (call-next-method)))

(defmethod source-file-type ((c unportable-cl-source-file) (s module))
  "lisp")


(defun lisp-system-shortname ()
  #+allegro :allegro #+lispworks :lispworks #+cmu :cmucl
  #+(or mcl openmcl) :mcl #+clisp :clisp #+scl :scl #+sbcl :sbcl) ;mcl/openmcl use the same directory

(defmethod component-pathname ((component unportable-cl-source-file))
  (let ((pathname (call-next-method))
        (name (string-downcase (lisp-system-shortname))))
    (merge-pathnames
     (make-pathname :directory (list :relative name))
     pathname)))

;;;; system

#+(and mcl (not openmcl)) (require :ansi-make-load-form)

#-(or lispworks cmu sbcl mcl openmcl clisp allegro)
(error "The acl-compat library is not yet supported on this lisp implementation.")

(defsystem acl-compat
    :name "acl-compat"
    :author "The acl-compat team"
    :version "0.3.0"
    :description
    "A reimplementation of parts of the ACL API, mainly to get
    AllegroServe running on various machines, but might be useful
    in other projects as well."
    :properties
    ((("system" "author" "email") . "portableaserve-discuss@lists.sourceforge.net")
     (("albert" "presentation" "output-dir") . "docs/")
     (("albert" "presentation" "formats") . "docbook")
     (("albert" "docbook" "dtd") . "/Users/Shared/DocBook/lib/docbook/docbook-dtd-412/docbookx.dtd")
     (("albert" "docbook" "template") . "book"))
    :components
    (
     ;; packages
     (:file "packages")
     ;; Our stream class; support for buffering, chunking and (in the
     ;; future) unified stream exceptions
     #-(or lispworks (and mcl (not openmcl)))
     (:file "lw-buffering" :depends-on ("packages"))
     #-(or allegro (and mcl (not openmcl)))
     (:legacy-cl-source-file "chunked-stream-mixin"
                             :depends-on ("packages" "acl-excl"
                                                     #-lispworks "lw-buffering"))
     ;; Multi




processing
     (:file "mp-decls" (:depends-on "packages"))
     #+(or mcl openmcl) (:unportable-cl-source-file "mcl-timers")
     (:unportable-cl-source-file "acl-mp"
                                 :depends-on ("packages" "mp-decls" #+(or mcl openmcl) "mcl-timers"))
     ;; Sockets, networking; TODO: de-frob this a bit
     #+sbcl
     (:unportable-cl-source-file
      "socket-streams" :depends-on ("packages"))
     #-(or mcl openmcl)
     (:unportable-cl-source-file
      "acl-socket" :depends-on ("packages" "acl-excl"
                                           #+sbcl "socket-streams"
                                           #-(or allegro (and mcl (not openmcl))) "chunked-stream-mixin"))
     #+(and mcl (not openmcl))
     (:unportable-cl-source-file "acl-socket-mcl" :depends-on ("packages"))
     #+(and mcl (not openmcl) (not carbon-compat))
     (:unportable-cl-source-file
      "mcl-stream-fix" :depends-on ("acl-socket-mcl"))
     #+openmcl
     (:unportable-cl-source-file
      "acl-socket-openmcl" :depends-on ("packages" "chunked-stream-mixin"))
     ;; Diverse macros, utility functions
     #-allegro (:file "acl-excl-common" :depends-on ("packages"))
     #-allegro (:file "acl-sys-common" :depends-on ("packages"))
     (:unportable-cl-source-file "acl-excl" :depends-on
                                 #-allegro ("acl-excl-common")
                                 #+allegro ("packages"))
     (:unportable-cl-source-file "acl-sys" :depends-on ("packages"))
     ;; SSL
     #+(and ssl-available (not (or allegro mcl openmcl clisp)))
     (:file "acl-ssl" :depends-on ("acl-ssl-streams" "acl-socket"))
     #+(and ssl-available (not (or allegro mcl openmcl clisp)))
     (:file "acl-ssl-streams" :depends-on ("packages")))
    ;; Dependencies
    :depends-on (:puri
                 :cl-ppcre
                 :ironclad
                 :cl-fad
                 #+sbcl :sb-bsd-sockets
                 #+sbcl :sb-posix
                 #+(and cmu (not gray-streams)) :cmucl-graystream
                 #+(and (or cmu lispworks) ssl-available) :cl-ssl
                 )
    :perform (load-op :after (op acl-compat)
                      (pushnew :acl-compat cl:*features*)))

(defsystem acl-compat/acl-socket-tester
  :class fiveam-tester-system
  :depends-on ("acl-compat")
  :test-names ((acl-socket-tests . acl-socket-tests))
  :components ((:file "test-acl-socket")))
