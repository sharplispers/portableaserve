;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

(defun fixnump (x)
  (sys::fixnump x))

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
  ;; Taken from clocc's port library, with thanks to Sam Steingold
  (if (values
       (ignore-errors
         (#+lisp=cl ext:probe-directory #-lisp=cl lisp:probe-directory
                    file-or-directory-name)))
      :directory
      (if (probe-file file-or-directory-name)
          :file
          nil)))

(defmacro atomically (&body forms)
  ;; No multiprocessing here, move along...
  `(progn ,@forms))

(defun unix-signal (signal pid)
  (declare (ignore signal pid))
  (error "clisp unix-signal not implemented yet."))

(defmacro without-package-locks (&body forms)
  `(ext:without-package-lock ,(list-all-packages) ,@forms))

(defun fixnump (x)
  (sys::fixnump x))

