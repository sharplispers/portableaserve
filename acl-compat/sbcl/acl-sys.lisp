(in-package :acl-compat.system)

(defun command-line-arguments ()
  sb-ext:*posix-argv*)

(defun command-line-argument (n)
  (nth n sb-ext:*posix-argv*))
