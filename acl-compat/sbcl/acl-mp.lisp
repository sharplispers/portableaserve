;; Threading for sbcl, or stub functions for single-threaded sbcl.
;;
;; Written by Rudi Schlatte, intended to be distributed along with the
;; acl-compat library, under the same license as the rest of it.

;; Inspirations taken from Dan Barlow<dan@metacircles.com>'s work for
;; McCLIM; cut, pasted and mutilated with permission.

(in-package :acl-compat.mp)

#-sb-thread
(progn

(defparameter *current-process* nil)

(defparameter *all-processes* nil)

(macrolet ((def (name args)
             `(defun ,name ,args
                (declare (ignore ,@(remove-if
                                    (lambda (x)
                                      (member x '(&optional &rest &key &allow-other-keys &aux)))
                                    (mapcar (lambda (x) (if (consp x) (car x) x))
                                            args))))
                (error "~A: Calling a multiprocessing function on a single-threaded sbcl build"
                       ',name))))
  (def process-interrupt (process function))
  (defun process-name (process)         ; *x
    (declare (ignore process))
    "the only process")
  (def process-wait-function (process))
  (def process-whostate (process))
  (def process-wait (process))          ; *x
  (def process-allow-schedule ())       ; *x
  (def process-property-list (process)) ; *x
  (def (setf process-property-list) (new-value process))
  (def process-run-reasons (process))   ; *x
  (def (setf process-run-reasons) (new-value process))
  (def process-revoke-run-reason (process object)) ; *x
  (def process-add-run-reason (process object)) ; *x
  (def process-run-function (name-or-options preset-function
                             &rest preset-arguments)) ; *x
  (def process-preset (process preset-function &rest arguments)) ; *x
  (def process-initial-bindings (process))
  (def (setf process-initial-bindings) (bindings process))
  (def make-process (&key (name "Anonymous") reset-action run-reasons
                           arrest-reasons (priority 0) quantum resume-hook
                           suspend-hook initial-bindings run-immediately)) ; *x
  (def process-kill (process))          ; *x
  (def make-process-lock (&key name)))  ; *x

(defmacro with-process-lock ((lock &key norecursive timeout whostate) &body forms)
  (declare (ignore lock norecursive timeout whostate))
  `(progn ,@forms))                     ; *x

(defmacro without-scheduling (&body forms)
  `(progn ,@forms))                     ; *
)                                       ; #-sb-thread

;;; Same implementation for multi- and uni-thread
(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  (let ((c (gensym "TIMEOUT-")))
    `(handler-case
      (sb-ext:with-timeout ,seconds (progn ,@body))
      (sb-ext:timeout (,c) (declare (ignore ,c)) ,@timeout-forms))))


#+sb-thread
(progn
(defstruct (process
            (:constructor %make-process)
            (:predicate processp))
  name
  state
  whostate
  function                              ; function wot will be run
  arguments                             ; arguments to the function
  id                                    ; pid of unix thread or nil
  %queue                        ; lock for process structure mutators
  run-reasons                           ; primitive mailbox for IPC
  %block-queue                          ; queue for condition-wait
  initial-bindings                      ; special variable bindings
  property-list
  )

(defvar *current-process*
  ;; We don't fill in the process id, so the process compiling this
  ;; (the REPL, in most cases) can't be killed by accident. (loop for
  ;; p in (all-processes) do (kill-process p)), anyone?
  (%make-process :name "initial process" :function nil))

(defvar *all-processes* (list *current-process*))

(defvar *conditional-store-queue* (sb-thread:make-waitqueue))

(defmacro with-process-lock ((place &key timeout whostate norecursive)
			     &body body)
  (declare (ignore norecursive))
  (let ((old-whostate (gensym "OLD-WHOSTATE")))
    `(sb-thread:with-recursive-lock (,place)
      (let (,old-whostate)
	(unwind-protect
	     (progn
	       (when ,whostate
		 (setf ,old-whostate (process-whostate *current-process*))
		 (setf (process-whostate *current-process*) ,whostate))
	       ,@body)
	  (setf (process-whostate *current-process*) ,old-whostate))))))


(defun make-process  (&key (name "Anonymous") reset-action run-reasons
                           arrest-reasons (priority 0) quantum resume-hook
                           suspend-hook initial-bindings run-immediately)
  (declare (ignore reset-action arrest-reasons priority quantum resume-hook
                   suspend-hook run-immediately))
  (let ((p (%make-process :name name
                          :run-reasons run-reasons
                          :initial-bindings initial-bindings
                          :%queue (sb-thread:make-mutex :name (format nil "Internal lock for ~A" name))
                          :%block-queue (sb-thread:make-waitqueue :name (format nil "Blocking queue for ~A" name)))))
    (push p *all-processes*)
    p))

(defun restart-process (process)
  (labels ((boing ()
                  (let ((*current-process* process)
                        (bindings (process-initial-bindings process))
                        (function (process-function process))
                        (arguments (process-arguments process)))
                    (if bindings
                        (progv
                            (mapcar #'car bindings)
                            (mapcar #'(lambda (binding)
                                        (eval (cdr binding)))
                                    bindings)
                          (apply function arguments))
                      (apply function arguments)))))
    (when (process-id process) (sb-thread:destroy-thread process))
    ;; XXX handle run-reasons in some way?  Should a process continue
    ;; running if all run reasons are taken away before
    ;; restart-process is called?  (process-revoke-run-reason handles
    ;; this, so let's say (setf (process-run-reasons process) nil) is
    ;; not guaranteed to do the Right Thing.)
    (when (setf (process-id process) (sb-thread:make-thread #'boing))
      process)))

(defun process-preset (process function &rest arguments)
  "Set function of process and restart it if it was already running"
  (setf (process-function process) function
        (process-arguments process) arguments)
  (when (process-id process) (restart-process process)))

(defun process-run-function (name-or-options preset-function
                                             &rest preset-arguments)
  (let* ((make-process-args (etypecase name-or-options
                              (list name-or-options)
                              (string (list :name name-or-options))))
         (process (apply #'make-process make-process-args)))
    (apply #'process-preset process preset-function preset-arguments)
    (when (process-run-reasons process) (restart-process process))
    process))

(defun process-kill (process)
  (when (process-id process)
    (sb-thread:destroy-thread (process-id process))
    (setf (process-id process) nil))
  (setf *all-processes* (delete process *all-processes*)))

(defun current-process ()
  *current-process*)

(defun all-processes ()
  *all-processes*)

(defun process-wait (reason predicate &rest arguments)
  (let ((old-state (process-whostate *current-process*)))
    (unwind-protect
        (progn
          (setf old-state (process-whostate *current-process*)
                (process-whostate *current-process*) reason)
          (loop 
           (let ((it (apply predicate arguments)))
             (when it (return it)))
           (process-allow-schedule)))
      (setf (process-whostate *current-process*) old-state))))

(defun process-allow-schedule ()
  (sleep .01))

(defun process-revoke-run-reason (process object)
  (sb-thread:with-recursive-lock ((process-%queue process))
     (prog1
         (setf (process-run-reasons process)
               (delete object (process-run-reasons process)))
       (when (and (process-id process) (not (process-run-reasons process)))
         (disable-process process)))))

(defun process-add-run-reason (process object)
  (sb-thread:with-recursive-lock ((process-%queue process))
     (prog1
         (push object (process-run-reasons process))
       (if (process-id process)
           (enable-process process)
         (restart-process process)))))


(defun process-wait-with-timeout (reason timeout predicate)
  (let ((old-state (process-whostate *current-process*))
        (end-time (+ (get-universal-time) timeout)))
    (unwind-protect
        (progn
          (setf old-state (process-whostate *current-process*)
                (process-whostate *current-process*) reason)
          (loop 
           (let ((it (funcall predicate)))
             (when (or (> (get-universal-time) end-time) it)
               (return it)))
           (sleep .01)))
      (setf (process-whostate *current-process*) old-state))))

(defun process-interrupt (process function)
  (declare (ignore process function))
  (error "Sorry Dave, I'm afraid I can't do that"))

(defun disable-process (process)
  ;; TODO: set process-whostate
  ;; Can't figure out how to safely block a thread from a different one
  ;; and handle all the locking nastiness.  So punt for now.
  (if (eql (sb-thread:current-thread-id) (process-id process))
      ;; Keep waiting until we have a reason to run.  GC and other
      ;; things can break a wait prematurely.  Don't know if this is
      ;; expected or not.
      (do ()
          ((process-run-reasons process) nil)
        (sb-thread:condition-wait (process-%block-queue process)
                                  (process-%queue process)))
      (error "Can't safely disable-process from another thread")))

(defun enable-process (process)
  ;; TODO: set process-whostate
  (sb-thread:condition-notify (process-%block-queue process)))

;;; FIXME but, of course, we can't.  Fix whoever wants to use it,
;;; instead
(defmacro without-scheduling (&body body)
  `(progn ,@body))

;;; TODO: integrate with McCLIM / system-wide queue for such things
(defmacro atomic-incf (place)
  `(sb-thread::with-spinlock (*conditional-store-queue*)
                             (incf ,place)))

(defmacro atomic-decf (place)
  `(sb-thread::with-spinlock (*conditional-store-queue*)
                             (decf ,place)))

(defun make-process-lock (&key name)
  (sb-thread:make-mutex :name name))

(defun process-active-p (thread-id)
  "If a native thread exists, it is always active"
  (and
   (member thread-id
	   (let ((offset (* 4 sb-vm::thread-pid-slot)))
	     (sb-thread::mapcar-threads
	      #'(lambda (sap) (sb-sys:sap-ref-32 sap offset))))
	   :test 'eql)
   t))

)                                     ; #+sb-thread
