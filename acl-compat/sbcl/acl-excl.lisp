;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
  (let ((mode (sb-posix:stat-mode (sb-posix:stat file-or-directory-name))))
    (cond
      ((sb-posix:s-isreg mode) :file)
      ((sb-posix:s-isdir mode) :directory)
      (t nil))))

(defmacro atomically (&body forms)
  `(acl-mp:without-scheduling ,@forms))

(defun filesys-inode (path)
  (sb-posix:stat-ino (sb-posix:lstat path)))

(defun cl-internal-real-time ()
  (round (/ (get-internal-real-time) internal-time-units-per-second)))

;;; FIXME: STREAM-ERROR is a more abstract class in SBCL and does not
;;; offer many of the slots provided by other implementations'
;;; STREAM-ERRORs. [2024/09/04:rpg]
(define-condition socket-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A socket error occured (stream=~S)."
                     ;;(stream-error-action condition)
                     ;;(stream-error-identifier condition)
                     ;; (stream-error-code condition)
                     (stream-error-stream condition))
             ;; (format stream "A socket error occured (action=~A identifier=~A code=~A stream=~S)."
             ;;         (stream-error-action condition)
             ;;         (stream-error-identifier condition)
             ;;         (stream-error-code condition)
             ;;         (stream-error-stream condition))
             )))

;! Need to figure out what to do here
(defun fasl-read (filename)
  (declare (ignore filename))
  (error "fasl-read not implemented for SBCL.") )

(defun fasl-write (data stream opt)
  (declare (ignore data stream opt))
  (error "fasl-write not implemented for SBCL.") )

(defun unix-signal (signal pid)
  (sb-unix:unix-kill pid signal))

(defun read-vector (&rest args)
  (apply #'sb-gray:stream-read-sequence args))

(defmacro schedule-finalization (object function)
  `(sb-ext:finalize ,object ,function))

;;; This type declaration isn't practically checkable, but it acts
;;; as a way to declare the programmer's intention.
(deftype alist ()
  'list)

(deftype if-exists ()
  '(member :error :overwrite :append :supersede nil))

(declaim (ftype (function ((or string vector) ;; command
                          &key (:input (or string pathname stream (eql :stream) null))
                          (:output (or string pathname stream (eql :stream) null))
                          (:error-output (or string pathname stream (eql :stream) null))
                          (:separate-streams boolean)
                          (:if-input-does-not-exist
                           (member :error :create nil))
                          (:if-output-exists if-exists)
                          (:if-error-output-exists if-exists)
                          (:wait boolean)
                          (:environment (or null alist))
                          (:show-window t))
                          t)
                run-shell-command))

(defun run-shell-command (command
                          &key input output error-output separate-streams
                          (if-input-does-not-exist :error) (if-output-exists :error)
                          (if-error-output-exists :error) (wait t) environment show-window)
  "
This function returns either one value when the wait argument is
true (it defaults to t), or three or four values when wait is
nil. When wait is nil, the number of values depends on the value of
the `separate-streams` argument.

When run-shell-command is called with the wait keyword argument is
true (or unspecified since the default is t), then the single return
value is the exit status of the spawned process.

The command argument can be a string containing a shell command (in
Unix) or a program (in Windows). On Unix only, command can be a
simple vector (element type t).

If command is a string, a shell is spawned which parses and executes the command.

If command is a simple vector (Unix only), its elements should be
coercible to strings. The zeroth element and the remainder of the
vector, converted to C strings, are passed as the two arguments to
execvp() (and thus bypassing the intermediate shell process despite
the name of this function). This is faster than spawning a shell and
it avoids processing of shell initialization files such as
.cshrc. Further, a shell will treat certain characters (such as
quotes, asterisks, question marks, semicolons, spaces) as
special. Such special treatment is a potential security risk if part
of the command string contains untrusted input.

The permissible values for IF-INPUT-DOES-NOT-EXIST are :error, :create and nil.

Permissible values for if-output-exists and if-error-output-exists are
:error, :overwrite, :append, :supersede and nil. The default is :error
in all cases.
"
  (declare (ignore show-window))
  (when (null if-input-does-not-exist)
    (error "SBCL does not support NIL value for IF-INPUT-DOES-NOT-EXIST"))
  (when (member if-output-exists '(nil :overwrite))
    (error "SBCL does not support ~S value for IF-OUTPUT-EXISTS" if-output-exists))
  (when (member if-error-output-exists '(nil :overwrite))
    (error "SBCL does not support ~S value for IF-ERROR-OUTPUT-EXISTS" if-error-output-exists))

  (let (program arguments force-shell)
    (etypecase command
      (string
       (if environment
           (setf program
                 (format nil "~:{~a=~a; ~}~a"
                         environment
                         command))
           (setf program command))
       (setf force-shell t))
      ;; Allegro docs specify the elements of the vector must be "coercible to strings"
      (vector (setf program (string (aref command 0))
                    arguments
                    (loop :for x :across (subseq command 1)
                          :collecting (string x)))))
    (if wait
        (progn
          (when (or (eq input :stream) (eq output :stream) (eq error-output :stream))
            (error "Value :STREAM not permitted for synchronous call in RUN-SHELL-COMMAND."))
          (synchronous-procedure-call program arguments force-shell
                                      :input input :output output :error-output error-output
                                      :if-input-does-not-exist if-input-does-not-exist
                                      :if-output-exists if-output-exists
                                      :if-error-output-exists if-error-output-exists))
        (asynchronous-procedure-call program arguments force-shell
                                     :input input :output output :error-output error-output
                                     :if-input-does-not-exist if-input-does-not-exist
                                     :if-output-exists if-output-exists
                                     :if-error-output-exists if-error-output-exists
                                     :separate-streams separate-streams))))

(declaim (ftype (function (string list boolean
                                  &key
                                  (:input (or null string pathname stream))
                                  (:output (or null string pathname stream))
                                  (:error-output (or null string pathname stream))
                                  (:if-output-exists (member :error :append :supersede))
                                  (:if-error-output-exists (member :error :append :supersede))
                                  (:if-input-does-not-exist (member :create :error)))
                          (values fixnum &optional))
                synchronous-procedure-call))
(defun synchronous-procedure-call (program arguments force-shell
                                   &key input output error-output
                                     if-input-does-not-exist if-output-exists
                                     if-error-output-exists)
  (macrolet ((translate-io-arg (arg)
               `(etypecase ,arg
                  (null
                   ;; inherit
                   :interactive)
                  ((or string pathname stream)
                   ,arg))))
    (let ((input-arg
            (translate-io-arg input))
          (output-arg
            (translate-io-arg output))
          (error-output-arg
            (translate-io-arg error-output)))
      (nth-value 2
                 (uiop:run-program (if arguments (cons program arguments)
                                       program)
                                   ;; return the exit code
                                   :ignore-error-status t
                                   :force-shell force-shell
                                   :if-input-does-not-exist if-input-does-not-exist
                                   :if-output-exists if-output-exists
                                   :if-error-output-exists if-error-output-exists
                                   :input input-arg
                                   :output output-arg
                                   :error-output error-output-arg)))))

(declaim (ftype (function (string list boolean
                                  &key
                                  (:input (or null string pathname (eql :stream)))
                                  (:output (or null string pathname (eql :stream)))
                                  (:error-output (or null string pathname (eql :stream)
                                                     (eql :output)))
                                  (:separate-streams boolean)
                                  (:if-output-exists (member :error :append :supersede))
                                  (:if-error-output-exists (member :error :append :supersede))
                                  (:if-input-does-not-exist (member :create :error)))
                          (values (or null stream) ; input stream
                                  (or null stream) ; output stream
                                  ;; if separate-streams, then error-output stream
                                  ;; else exit status
                                  (or null stream fixnum)
                                  ;; if separate-streams, then exit status
                                  ;; otherwise no value
                                  &optional fixnum))
                asynchronous-procedure-call))
(defun asynchronous-procedure-call (program arguments force-shell
                                    &key input output error-output separate-streams
                                      if-input-does-not-exist if-output-exists
                                      if-error-output-exists)
  ;;; UNHYGIENIC macro for decoding args per Allegro CL.
  (macrolet ((translate-io-arg (arg)
               `(etypecase ,arg
                  (null
                   ;; inherit
                   :interactive)
                  ((eql :stream)
                   ,(cond ((or (eq arg 'input)
                               (eq arg 'output))
                           :stream)
                          ((eq arg 'error-output)
                           `(if separate-streams
                               :stream
                               ;; else send to output
                               'output-arg))))
                  ((eql :output)
                   ,(if (eq arg 'error-output)
                        'output-arg
                        '(error ":OUTPUT is only valid for ERROR-OUTPUT.")))
                  ((or string pathname stream)
                   ,arg))))
    (let* ((input-arg
             (translate-io-arg input))
           (output-arg
             (translate-io-arg output))
           (error-output-arg
             (translate-io-arg error-output))
           (process-info
             (uiop:launch-program (if arguments (cons program arguments)
                                      program)
                                  ;; return the exit code
                                  :ignore-error-status t
                                  :force-shell force-shell
                                  :if-input-does-not-exist if-input-does-not-exist
                                  :if-output-exists if-output-exists
                                  :if-error-output-exists if-error-output-exists
                                  :input input-arg
                                  :output output-arg
                                  :error-output error-output-arg)))
      (if separate-streams
          (values (when (eq input :stream)
                    (uiop:process-info-input process-info))
                  (when (eq output :stream)
                    (uiop:process-info-output process-info))
                  (when (eq error-output :stream)
                    (uiop:process-info-error-output process-info))
                  (uiop:process-info-pid process-info))
          (values (when (eq input :stream)
                    (uiop:process-info-input process-info))
                  (when (eq output :stream)
                    (uiop:process-info-output process-info))
                  (uiop:process-info-pid process-info))))))
