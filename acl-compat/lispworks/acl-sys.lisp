(in-package :sys)
(let ((*handle-warn-on-redefinition* :warn))
;      (*packages-for-warn-on-redefinition* nil))

  (defun command-line-arguments ()
    system:*line-arguments-list*)
  
  (defun command-line-argument (n)
    (nth n system:*line-arguments-list*))
  
  (defun reap-os-subprocess (&key (wait nil))
    (declare (ignore wait))
    nil)

  (export 'command-line-arguments)
  (export 'command-line-argument)
  (export 'reap-os-subprocess))