;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
  (if (eq :directory (sb-unix:unix-file-kind
                      (namestring file-or-directory-name)))
      :directory
      (if (probe-file file-or-directory-name)
          :file
          nil)))

(defmacro atomically (&body forms)
  `(acl-mp:without-scheduling ,@forms))

(defun unix-signal (signal pid)
  (declare (ignore signal pid))
  (error "unix-signal not implemented in acl-excl-sbcl.lisp"))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))
