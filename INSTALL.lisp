;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-

(in-package "CL-USER")

#+lispworks
(progn
  ;; Load logical host definitions
  (load (merge-pathnames "logical-hostnames.lisp" *load-truename*))

  ;; Load definition of CL-SSL system
  ;;(load "cl-ssl:defsys.lisp")

  ;; Compile and load CL-SSL system
  ;;(scm:compile-system 'cl-ssl :load t) ;use :FORCE T if compilation troubles

  ;; Load definition of ACL Compatibility system
  (load "acl-compat:defsys.lisp")

  ;; Compile and load ACL Compatibility system
  (scm:compile-system 'acl-compat :load t :force t) ;use :FORCE T if compilation troubles

  ;; Load definition of Aserve system
  (load "aserve:defsys.lisp")

  ;; Compile and load the ASERVE system
  (scm:compile-system 'aserve :load t :force t) ;use :FORCE T if compilation troubles
)


#+cmu
(progn
  ;; Load Gray-streams support
  (eval-when (:compile-toplevel :load-toplevel :execute)
    ;; Debian cmucl distribution
    #+(and common-lisp-controller (not gray-streams))
    (require :cmucl-graystream)
    ;; vanilla cmucl
    #+(and (not common-lisp-controller) (not gray-streams))
    (progn (load "library:subsystems/gray-streams-library")
           (pushnew :gray-streams *features*)))

  ;; Startup multiprocessing.
  ;;
  ;; this isn't strictly necessary, but scheduling feels very coarse
  ;; before the evaluation of (startup-idle-and-top-level-loops) --
  ;; answer delays of about 1s per http request.
  ;;
  ;; KLUDGE: startup-idle-and-top-level-loops can only be evaluated
  ;; once, so we look for something resembling an existing idle loop
  ;; before invoking it.
  #||
  #+mp
  (unless (find-if
           #'(lambda (proc) (string= (mp:process-name proc) "Idle Loop"))
           (mp:all-processes))
    (mp::startup-idle-and-top-level-loops))
  ||#
  ;; DOUBLE KLUDGE: The preceding (commented-out) form caused the
  ;; loading of INSTALL.lisp to abort silently (!), so we do the
  ;; following, pilfered from eclipse the window manager:
  #+mp
  (setf mp::*idle-process* mp::*initial-process*)

  ;; Load logical host definitions
  (load (merge-pathnames "logical-hostnames.lisp" *load-truename*))

  ;; Load definition of ACL Compatibility system
  (load "acl-compat:acl-compat.system")

  ;; Compile and load ACL Compatibility system
  (mk:oos "ACL-COMPAT" :load :compile-during-load t)

  ;; Load definition of Aserve system
  (load "aserve:aserve.system")

  ;; Compile and load the ASERVE system
  (mk:oos "ASERVE" :load :compile-during-load t)
  
  )

#||
;;; To test the installation, evaluate the following:

;;; Load example.lisp in the aserve directory.
(load "aserve:example.cl")

;;; Select example package
(in-package :aserve-example)

;;; This option enables extended debug message output
(net.aserve::debug-on :info)

;;; This option enables to enter the debugger if an error
;;; occurs. (instead of simply logging and ignoring it)
(net.aserve::debug-on :notrap)

;;; Start example server (in multiprocessing) on port 2001
(start-server :port 2001)
||#
