;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-

(in-package "CL-USER")

#+lispworks
(progn
; Load logical host definitions
(load (merge-pathnames "logical-hostnames.lisp" *load-truename*))

; Load definition of CL-SSL system
;(load "cl-ssl:defsys.lisp")

; Compile and load CL-SSL system
;(scm:compile-system 'cl-ssl :load t) ;use :FORCE T if compilation troubles

; Load definition of ACL Compatibility system
(load "acl-compat:defsys.lisp")

; Compile and load ACL Compatibility system
(scm:compile-system 'acl-compat :load t :force t) ;use :FORCE T if compilation troubles

; Load definition of Aserve system
(load "aserve:defsys.lisp")

; Compile and load the ASERVE system
(scm:compile-system 'aserve :load t :force t) ;use :FORCE T if compilation troubles
)

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
;; We need gray-streams in the image.  The following works with Debian
;; packages of cmucl 2.5.1 + cmucl-source + common-lisp-controller.
;; Adapt as necessary.

(unless (member :gray-streams *features*)
  (mk:oos :cmucl-graystream :load))

;; this isn't strictly necessary, but scheduling feels very coarse
;; without startup-idle-and-top-level-loops, leading to answer delays
;; of about 1s per request.

(unless (find-if
         #'(lambda (proc) (string= (mp:process-name proc) "Top Level Loop"))
         (mp:all-processes))
  (mp::startup-idle-and-top-level-loops))


; Load logical host definitions
(load (merge-pathnames "logical-hostnames.lisp" *load-truename*))

; Load definition of ACL Compatibility system
(load "acl-compat:acl-compat.system")

; Compile and load ACL Compatibility system
(mk:oos "ACL-COMPAT" :load :compile-during-load t)

; Load definition of Aserve system
(load "aserve:aserve.system")

; Compile and load the ASERVE system
(mk:oos "ASERVE" :load :compile-during-load t)
))


; Test the installation

; Load example.lisp in the aserve directory.
(load "aserve:example.lisp")

; Select example package
(in-package :aserve-example)

;; This option enables extended debug message output
;(net.aserve::debug-on :info)

;; This option enables to enter the debugger if an
;; error occurs. (instead of simply logging and ignoring it)

;(net.aserve::debug-on :notrap)

; Start example server (in multiprocessing) on port 2001
(start-server :port 2001)
