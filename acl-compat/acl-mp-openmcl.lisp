;;; This file implements the process functions for AllegroServe in MCL.
;;; Based on the the work done for cmucl and Lispworks.
;;;
;;; John DeSoi, Ph.D. desoi@users.sourceforge.net



(defpackage mp
  (:nicknames :acl-compat.mp :acl-mp)
  (:use :COMMON-LISP) 
  (:export
   "CURRENT-PROCESS"
   "INTERRRUPT-PROCESS"
   "MAKE-PROCESS"
   "MAKE-PROCESS-LOCK"
   "PROCESS-ADD-RUN-REASON"
   "PROCESS-KILL"
   "PROCESS-PROPERTY-LIST"
   "PROCESS-REVOKE-RUN-REASON"
   "PROCESS-RUN-FUNCTION"
   "WITH-PROCESS-LOCK"
   "WITH-TIMEOUT"
   "WITHOUT-SCHEDULING"
   ))


(in-package :mp)

(eval-when (:compile-toplevel :load-toplevel :execute)

; existing stuff from ccl we can reuse directly
(shadowing-import 
 '(ccl:*current-process*
   ccl::lock
   ccl:process-allow-schedule
   ccl:process-name
   ccl:process-preset
   ccl:process-run-reasons
   ccl:process-wait
   ccl:without-interrupts) 
 :mp)
)

(eval-when (:compile-toplevel :load-toplevel :execute)

(export 
 '(*current-process*
   lock
   process-allow-schedule
   process-name
   process-preset
   process-run-reasons
   process-wait
   without-interrupts) 
 :mp)
)

(eval-when (:compile-toplevel :load-toplevel :execute)
                 
(defmacro without-scheduling (&body forms)
  `(ccl:without-interrupts ,@forms))

#|
; more ideas stolen from acl-mp-lw.lisp
(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  (block timeout
    (let* ((process *current-process*)
           (timer (ccl:process-run-function "with-timeout-timer"
                                            #'(lambda () 
                                                (sleep seconds)
                                                (ccl:process-interrupt process
                                                                       #'(lambda ()
                                                                           (return-from timeout
                                                                             (funcall timeoutfn))))))))
      (unwind-protect (funcall bodyfn)
        (ccl:process-kill timer)))))

|#



(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  (block timeout
    (let* ((timer (ccl::make-timer-request
                    seconds
                    #'(lambda () (return-from timeout (funcall timeoutfn))))))
      (ccl::enqueue-timer-request timer)
      (unwind-protect (funcall bodyfn)
	(ccl::dequeue-timer-request timer)))))


(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate and evaluate TIMEOUT-FORMS."
  `(invoke-with-timeout ,seconds #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))


(defmacro process-revoke-run-reason (process reason)
  `(ccl:process-disable-run-reason ,process ,reason) )

(defmacro process-add-run-reason (process reason)
  `(ccl:process-enable-run-reason ,process ,reason) )


(defmacro make-process-lock (&key name)
  (if name
    `(ccl:make-lock ,name)
    `(ccl:make-lock)))

(defmacro with-process-lock ((lock &key norecursive) &body forms)
  (declare (ignore norecursive))
  `(ccl:with-lock-grabbed (,lock) ,@forms))


(defmacro process-kill (process)
  `(progn 
     (unless (ccl:process-active-p ,process) ;won't die unless enabled
       (ccl:process-reset-and-enable ,process) )
     (ccl:process-kill ,process)))

)

(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
(apply #'ccl:process-interrupt process function args))

(defun current-process ()
  "The current process."
  ccl:*current-process*)


;property list implementation from acl-mp-cmu.lisp
(defvar *process-plists* (make-hash-table :test #'eq)
  "maps processes to their plists.
See the functions process-plist, (setf process-plist).")

(defun process-property-list (process)
  (gethash process *process-plists*))

(defun (setf process-property-list) (new-value process)
  (setf (gethash process *process-plists*) new-value))

; from acl-mp-lw.lisp
(defun make-process (&key (name "Anonymous") reset-action run-reasons arrest-reasons (priority 0) quantum
                          resume-hook suspend-hook initial-bindings run-immediately)
  (declare (ignore priority quantum reset-action resume-hook suspend-hook run-immediately))
  (declare (ignore initial-bindings)) ;! need separate lexical bindings for each process?
  ;(let ((mp:*process-initial-bindings* initial-bindings))
    (ccl:make-process name :run-reasons run-reasons :arrest-reasons arrest-reasons))

(defun process-run-function (name-or-options preset-function &rest preset-arguments)
  (let ((process (ctypecase name-or-options
                   (string (mp:make-process :name name-or-options))
                   (list (apply #'mp:make-process name-or-options)))))
    (apply #'mp:process-preset process preset-function preset-arguments)
    (process-add-run-reason process :enable)
    process))