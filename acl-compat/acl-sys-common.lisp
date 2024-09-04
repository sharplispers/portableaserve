(in-package :acl-compat.system)

#+(or sbcl clisp scl ccl openmcl)
(defun reap-os-subprocess (&key wait pid)
  (declare (ignore wait pid))
  nil)
