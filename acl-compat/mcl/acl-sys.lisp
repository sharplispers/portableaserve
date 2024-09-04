
(in-package :acl-compat.system)


(defun command-line-arguments ()
  #+openmcl (ccl::command-line-arguments)
  #-openmcl nil)

(defun command-line-argument (n)
  #+openmcl (nth n (command-line-arguments))
  #-openmcl nil)

#+nil
(export '(command-line-arguments command-line-argument reap-os-subprocess))
