(in-package :mp)

(export 'without-scheduling)
(export 'process-allow-schedule)
(export 'make-process)
(export 'process-revoke-run-reason)
(export 'process-add-run-reason)
(export 'with-timeout)
(export 'make-process-lock)
(export 'with-process-lock)

(defmacro without-scheduling (&body forms)
  `(without-preemption ,@forms))

(defun process-allow-schedule ()
  (process-allow-scheduling))

(defun process-revoke-run-reason (process object)
  (without-scheduling
      (setf (process-run-reasons process)
              (remove object (process-run-reasons process))))
  (when (and (eq process mp:*current-process*)
               (not mp:*inhibit-scheduling-flag*))
      (process-allow-scheduling)))

(defun process-add-run-reason (process object)
  (setf (process-run-reasons process) (pushnew object (process-run-reasons process))))


(defmacro with-gensyms (syms &body body)
  "Bind symbols to gensyms.  First sym is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy) `(,sy (gensym ,(car syms)))) (cdr syms)))
    ,@body))

#|(defun with-timeout-f (timeout bodyf timeoutf)
  (block timeout
    (let ((done nil) (process (current-process)))
      (process-run-function (format nil "Timeout monitor for ~A" process) '()
                    (lambda ()
                      (sleep timeout)
                      (unless done
                        (interrupt-process
                         process (lambda ()
                                   (return-from timeout
                                     (funcall timeoutf)))))))
      (unwind-protect (funcall bodyf)
        (setf done t)))))
|#

(defun with-timeout-f (timeout bodyf timeoutf)
  (block timeout
    (let* ((process (current-process))
           (timer (make-timer #'(lambda () (interrupt-process process
                                                              #'(lambda ()
                                                                  (return-from timeout
                                                                    (funcall timeoutf))))))))
      (schedule-timer-relative timer timeout)
      (unwind-protect (funcall bodyf)
        (unschedule-timer timer)))))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  (with-gensyms ("WT-" bodyf timeoutf)
    `(flet ((,bodyf () ,@body)
            (,timeoutf () ,@timeout-forms))
      (with-timeout-f ,seconds #',bodyf #',timeoutf))))


(defun current-process ()
  "The current process."
mp:*current-process*)

(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
(apply #'mp:process-interrupt process function args))

(defun make-process-lock (&key name)
  (mp:make-lock :name name))

(defmacro with-process-lock ((lock &key norecursive) &body forms)
  (declare (ignore norecursive))
  `(mp:with-lock (,lock) ,@forms))

