;; This package is designed for sbcl.  It will implement ACL-style
;; multiprocessing on top of sbcl, once it's there.  At the moment it
;; only hollers at the user when he tries to run multi-threaded
;; PortableAServe :)
;;
;; Written by Rudi Schlatte, intended to be distributed along with the
;; acl-compat library, under the same license as the rest of it.

(in-package :acl-compat.mp)

(defparameter *current-process* nil)

(macrolet ((def (name args)
             `(defun ,name ,args
                (declare (ignore ,@(remove-if
                                    (lambda (x)
                                      (member x '(&optional &rest &key &allow-other-keys &aux)))
                                    (mapcar (lambda (x) (if (consp x) (car x) x))
                                            args))))
                (error "~A: Multiprocessing is not implemented in sbcl at the moment."
                       ',name))))
  (def process-interrupt (process function))
  (def process-name (process))
  (def process-wait-function (process))
  (def process-whostate (process))
  (def process-wait (process))
  (def process-allow-schedule ())
  (def process-property-list (process))
  (def (setf process-property-list) (new-value process))
  (def process-run-reasons (process))
  (def (setf process-run-reasons) (new-value process))
  (def process-revoke-run-reason (process object))
  (def process-add-run-reason (process object))
  (def process-run-function (name-or-options preset-function
                             &rest preset-arguments))
  (def process-preset (process preset-function &rest arguments))
  (def process-initial-bindings (process))
  (def (setf process-initial-bindings) (bindings process))
  (def make-process (&key (name "Anonymous") reset-action run-reasons
                           arrest-reasons (priority 0) quantum resume-hook
                           suspend-hook initial-bindings run-immediately))
  (def process-kill (process))
  (def make-process-lock (&key name)))

(defmacro with-process-lock ((lock &key norecursive) &body forms)
  (declare (ignore lock norecursive))
  `(progn ,@forms))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  (declare (ignore seconds timeout-forms))
  `(progn ,@body))

(defmacro without-scheduling (&body forms)
  `(progn ,@forms))