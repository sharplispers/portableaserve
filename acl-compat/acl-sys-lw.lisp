(in-package :sys)

(ignore-errors
(export 'command-line-arguments)
(export 'command-line-argument)
(export 'reap-os-subprocess)

(defun command-line-arguments ()
  system:*line-arguments-list*)

(defun command-line-argument (n)
  (nth n system:*line-arguments-list*))

(defun reap-os-subprocess (&key (wait nil))
  (declare (ignore wait))
  nil)

)