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
