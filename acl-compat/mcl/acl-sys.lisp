

(defpackage acl-compat.system
  (:nicknames :sys)
  (:use :common-lisp) 
  (:export
   "command-line-arguments"
   "command-line-argument"
   "reap-os-subprocess"
   ))

(in-package :acl-compat.system)


; none of this stuff now; maybe with OS X

(defun command-line-arguments ()
  nil)
  ;system:*line-arguments-list*)

(defun command-line-argument (n)
  (declare (ignore n))
  nil)
  ;(nth n system:*line-arguments-list*))

(defun reap-os-subprocess (&key (wait nil))
  (declare (ignore wait))
  nil)

(export '(command-line-arguments command-line-argument reap-os-subprocess))
