;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
       (if (eq :directory (unix:unix-file-kind
                           (namestring file-or-directory-name)))
           :directory
         (if (probe-file file-or-directory-name)
             :file
           nil)))

(defmacro atomically (&body forms)
  `(mp:without-scheduling ,@forms))

(defun unix-signal (signal pid)
  ;; fixxme: did I get the arglist right?  only invocation I have seen
  ;; is (excl::unix-signal 15 0) in net.aserve:start
  (unix:unix-kill pid signal))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))

