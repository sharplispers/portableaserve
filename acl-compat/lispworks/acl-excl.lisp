;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

#+obsolete
(defun stream-input-fn (stream)
  stream)

(defmethod stream-input-fn ((stream stream))
  stream)
	
(defun filesys-type (file-or-directory-name)
	(if (lw::file-directory-p file-or-directory-name)
		:directory
		(if (probe-file file-or-directory-name)
			:file
			nil)))

(defmacro atomically (&body forms)
  `(mp:without-preemption ,@forms))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))


#|
(defun run-shell-command ()
  (with-open-stream (s (open-pipe "/bin/sh"
                                  :direction :io
                                  :buffered nil))
    (loop for var in environment
          do (format stream "~A=~A~%" (car var) (cdr var)))
|#
  

(provide 'acl-excl)
