

(defpackage mp
  (:nicknames :acl-compat-mp :acl-mp)
  (:use :COMMON-LISP) 
  (:export
   "*CURRENT-PROCESS*"
   "CURRENT-PROCESS"
   "INTERRRUPT-PROCESS"
   "MAKE-PROCESS-LOCK"
   "PROCESS-ADD-RUN-REASON"
   "PROCESS-ALLOW-SCHEDULE"
   "PROCESS-KILL"
   "PROCESS-NAME"
   "PROCESS-REVOKE-RUN-REASON"
   "PROCESS-RUN-FUNCTION"
   "PROCESS-RUN-REASONS"
   "PROCESS-WAIT"
   "WITH-PROCESS-LOCK"
   "WITH-TIMEOUT"
   "WITHOUT-INTERRUPTS"
   "WITHOUT-SCHEDULING"
   ))

(in-package :mp)


(eval-when (:compile-toplevel :load-toplevel :execute)
  
(shadowing-import 'ccl:*current-process*)   ;need to refer to mp:*current-process*
  
;(export '*current-process*) 
  
(defmacro without-scheduling (&body forms)
  `(ccl:without-interrupts ,@forms))

(defmacro without-interrupts (&body forms)
  `(ccl:without-interrupts ,@forms))

(defmacro process-run-function (name-or-kwds function &rest args)
  (if function ; I don't get how this works; in some calls function is nil and then the real function is next
    `(ccl:process-run-function ,name-or-kwds ,function ,@args)
    `(ccl:process-run-function ,name-or-kwds ,(first args) ,@(rest args))))

(defmacro process-wait (whostate function &rest args)
  `(ccl:process-wait ,whostate ,function ,@args) )

(defmacro process-name (process)
  `(ccl:process-name ,process) )

;! from corman - need to determine how to do it right
(defmacro with-timeout ((seconds &body timeout-body) &body body)
	"Currently implemented in a manner that the timeout never occurs."
	(declare (ignore seconds timeout-body))
	`(progn
		,@body))

(defmacro process-kill (process)
  `(ccl:process-kill ,process) )


(defmacro process-allow-schedule ()
  `(ccl:process-allow-schedule) )


(defmacro process-run-reasons (process)
  `(ccl:process-run-reasons ,process) )

(defmacro process-revoke-run-reason (process reason)
  `(ccl:process-disable-run-reason ,process ,reason) )

(defmacro process-add-run-reason (process reason)
  `(ccl:process-enable-run-reason ,process ,reason) )


(defmacro make-process-lock (&key name)
  `(ccl:make-lock :name ,name) )

(defmacro with-process-lock ((lock &key norecursive) &body forms)
  (declare (ignore norecursive))
  `(ccl:with-lock-grabbed (,lock) ,@forms))


)

(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
(apply #'ccl:process-interrupt process function args))

(defun current-process ()
  "The current process."
  ccl:*current-process*)


;! need something for process-plist? currently those disabled for mcl

