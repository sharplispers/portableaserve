;; This package is designed for cmucl.  It implements ACL-style
;; multiprocessing on top of cmucl (basically, process run reasons and
;; some function renames).
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks.

(in-package :mp)
(export 'process-allow-scheduling)
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

(defun process-allow-schedule ()
  (process-yield))

(defun process-allow-scheduling ()
  (process-yield))

(defvar *process-plists* (make-hash-table :test #'eq)
  "maps processes to their plists.
See the functions process-plist, (setf process-plist).")

(defun process-plist (process)
  (gethash process *process-plists*))

(defun (setf process-plist) (new-value process)
  (setf (gethash process *process-plists*) new-value))

(defvar *process-run-reasons* (make-hash-table :test #'eq)
  "maps processes to their run-reasons.
See the functions process-run-reasons, (setf process-run-reasons),
process-add-run-reason, process-revoke-run-reason.")

(defun process-run-reasons (process)
  (gethash process *process-run-reasons*))

(defun (setf process-run-reasons) (new-value process)
  (without-scheduling
   (prog1
       (setf (gethash process *process-run-reasons*) new-value)
     (if new-value
         (enable-process process)
       (disable-process process)))))

(defun process-revoke-run-reason (process object)
  (without-scheduling
   (setf (process-run-reasons process)
	 (remove object (process-run-reasons process))))
  (when (and (eq process mp:*current-process*))
    (process-allow-scheduling)))

(defun process-add-run-reason (process object)
  (setf (process-run-reasons process)
        (pushnew object (process-run-reasons process))))

(defun process-run-function (name keywords function &rest arguments)
  "Create a new process, passing it a function to run."
  (declare (ignore keywords))
  ;; processes seem to start when created; at least,
  ;; make-worker-thread sets process-run-reasons to nil explicitly
  (mp:make-process (lambda () (apply function arguments)) :name name))

(defun process-kill (process)
  (destroy-process process))

(defmacro with-gensyms (syms &body body)
  "Bind symbols to gensyms.  First sym is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy) `(,sy (gensym ,(car syms)))) (cdr syms)))
    ,@body))

(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
  (mp:process-interrupt process #'(lambda () (apply function args))))

(defun make-process-lock (&key name)
  (mp:make-lock name))

(defmacro with-process-lock ((lock &key norecursive) &body forms)
  (declare (ignore norecursive))
  `(mp:with-lock-held (,lock) ,@forms))
