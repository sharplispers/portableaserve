;; This package is designed for cmucl.  It implements ACL-style
;; multiprocessing on top of cmucl (basically, process run reasons and
;; some function renames).
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks.

(in-package :acl-compat-mp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import equivalent parts from the CMU MP package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(shadowing-import '(
                    mp:*current-process*
                    mp::process-preset
                    mp::process-reset
                    mp:process-interrupt
                    mp::process-name
                    mp::process-wait-function
                ;    mp:process-run-reasons
                ;    mp:process-arrest-reasons
                    mp:process-whostate
                ;    mp:without-interrupts
                    mp:process-wait
                    mp:with-timeout
		    mp:without-scheduling
                    ))

(export '(          *current-process*
                    process-preset
                    process-reset
                    process-interrupt
                    process-name
                    process-wait-function
                    process-whostate
                    process-wait
	            with-timeout
	            without-scheduling
                    ))

#|
(export 'process-allow-schedule)
(export 'make-process)
(export 'process-revoke-run-reason)
(export 'process-run-reasons)
(export 'process-plist)
(export 'process-add-run-reason)
(export 'process-run-function)
(export 'process-kill)
(export 'make-process-lock)
(export 'with-process-lock)
|#

(defun process-allow-schedule ()
  (mp:process-yield))

(defvar *process-plists* (make-hash-table :test #'eq)
  "maps processes to their plists.
See the functions process-plist, (setf process-plist).")

(defun process-property-list (process)
  (gethash process *process-plists*))

(defun (setf process-property-list) (new-value process)
  (setf (gethash process *process-plists*) new-value))

(defvar *process-run-reasons* (make-hash-table :test #'eq)
  "maps processes to their run-reasons.
See the functions process-run-reasons, (setf process-run-reasons),
process-add-run-reason, process-revoke-run-reason.")

(defun process-run-reasons (process)
  (gethash process *process-run-reasons*))

(defun (setf process-run-reasons) (new-value process)
  (mp:without-scheduling
   (prog1
       (setf (gethash process *process-run-reasons*) new-value)
     (if new-value
         (mp:enable-process process)
       (mp:disable-process process)))))

(defun process-revoke-run-reason (process object)
  (without-scheduling
   (setf (process-run-reasons process)
	 (remove object (process-run-reasons process))))
  (when (and (eq process mp:*current-process*))
    (mp:process-yield)))

(defun process-add-run-reason (process object)
  (setf (process-run-reasons process)
        (pushnew object (process-run-reasons process))))

;;;
;;; Note from JSC: PROCESS-RUN-FUNCTION should be changed to be more similar
;;;                to the ACL API.
;;;                I have at least removed the keywords argument here.
;;;                Initial-bindings should be included too (!)
;;;

(defun process-run-function (name-or-options preset-function &rest preset-arguments)
  (let ((process (ctypecase name-or-options
                   (string (make-process :name name-or-options))
                   (list (apply #'make-process name-or-options)))))
    (apply #'mp::process-preset process preset-function preset-arguments)
    process))

;;;
;;; Note from JSC: This need "initial-bindings" support too
;;;
(defun make-process (&key (name "Anonymous") reset-action run-reasons arrest-reasons (priority 0) quantum
                          resume-hook suspend-hook initial-bindings run-immediately)
  (declare (ignore priority quantum reset-action resume-hook suspend-hook run-immediately))
  (mp:make-process nil :name name))

(defun process-kill (process)
  (mp:destroy-process process))

;;;
;;; Note from JSC: with-gensyms is not needed here, is it?
;;;
(defmacro with-gensyms (syms &body body)
  "Bind symbols to gensyms.  First sym is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy) `(,sy (gensym ,(car syms)))) (cdr syms)))
    ,@body))


;;;
;;; Note from JSC: I think this comes from my more chaotic old version.
;;;                In ACL this function is simply called PROCESS-INTERRUPT
;;;
(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
  (mp:process-interrupt process #'(lambda () (apply function args))))

(defun make-process-lock (&key name)
  (mp:make-lock name))

(defmacro with-process-lock ((lock &key norecursive) &body forms)
  (declare (ignore norecursive))
  `(mp:with-lock-held (,lock) ,@forms))
