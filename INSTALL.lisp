;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-

(in-package "CL-USER")

#+(and (or lispworks cmu mcl) (not asdf))
(let ((asdf-pathname
       (merge-pathnames (make-pathname
                         :directory '(:relative "contrib")
                         :name "asdf"
                         :case :local)
                        *load-truename*)))
  (warn "~
Loading asdf from ~A ~
~:_(you might want to load asdf at startup ~
~:_and set up asdf:*central-registry* to point to your systems)" asdf-pathname)
  (load asdf-pathname))

#+(or lispworks cmu)
(progn
  (flet ((find-or-load-system (system path)
           (let ((path (merge-pathnames path *load-truename*)))
             (unless (asdf:find-system system nil)
               (warn "~
Cannot find ASDF system definition for ~A ~
~:_in asdf:*central-registry*, ~
~:_loading from file ~A. ~
~:_Hint: \"ln -sf ~A /path/to/your/systems\" ~
~:_to avoid this warning in the future."
                     system path (namestring path))
               (load path)))))
    (find-or-load-system :acl-compat
                         (make-pathname
                          :directory '(:relative "acl-compat")
                          :name "acl-compat" :type "asd" :case :local))
    (find-or-load-system :htmlgen
                         (make-pathname
                          :directory '(:relative "aserve" "htmlgen")
                          :name "htmlgen" :type "asd" :case :local))
    (find-or-load-system :aserve
                         (make-pathname
                          :directory '(:relative "aserve")
                          :name "aserve" :type "asd" :case :local)))
  ;; Compile and load the ASERVE system
  (asdf:operate 'asdf:load-op :acl-compat)
  (asdf:operate 'asdf:load-op :htmlgen)
  (asdf:operate 'asdf:load-op :aserve)

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
  #+(and cmu mp)
  (setf mp::*idle-process* mp::*initial-process*)

  )


#+sbcl
(require :asdf)

#+sbcl
(progn
  (flet ((find-or-load-system (system path)
           (let ((path (merge-pathnames path *load-truename*)))
             (unless (asdf:find-system system nil)
               (warn "~
                      Cannot find ASDF system definition for ~A ~
                      ~:_in asdf:*central-registry*, ~
                      ~:_loading from file ~A. ~
                      ~:_Hint: \"ln -sf ~A /path/to/your/systems\" ~
                      ~:_to avoid this warning in the future."
                     system path (namestring path))
               (load path)))))
    (find-or-load-system :acl-compat
                         (make-pathname
                          :directory '(:relative "acl-compat")
                          :name "acl-compat" :type "asd" :case :local))
    (find-or-load-system :htmlgen
                         (make-pathname
                          :directory '(:relative "aserve" "htmlgen")
                          :name "htmlgen" :type "asd" :case :local))
    (find-or-load-system :aserve
                         (make-pathname
                          :directory '(:relative "aserve")
                          :name "aserve" :type "asd" :case :local)))
  ;; Compile and load the ASERVE system
  (asdf:operate 'asdf:load-op :acl-compat)
  (asdf:operate 'asdf:load-op :htmlgen)
  (asdf:operate 'asdf:load-op :aserve))

#+mcl
(progn
  ; Load logical host definitions
  (load (merge-pathnames "logical-hostnames.lisp" *load-truename*))
    
  (unless (asdf:find-system :acl-compat nil)
    (load "acl-compat:acl-compat.asd"))
  
  (unless (asdf:find-system :htmlgen nil)
    (load "aserve:htmlgen;htmlgen.asd"))
  
  (unless (asdf:find-system :aserve nil)
    (load "aserve:aserve.asd"))
  
  ; Compile and load the ASERVE system - loads the other systems also
  (let ((ccl:*warn-if-redefine* nil))   ;defines a few vars in more than one file
    (asdf:operate 'asdf:load-op :aserve) )
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
