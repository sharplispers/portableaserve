(in-package :acl-compat.system)

(defun command-line-arguments ()
  ext:*command-line-strings*)

(defun command-line-argument (n)
  (nth n ext:*command-line-strings*))
