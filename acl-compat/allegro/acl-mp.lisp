;;; This file implements the process functions for AllegroServe in MCL.


(defpackage :acl-compat-mp
  (:use :common-lisp)
  (:export
   #:process-interrrupt
   #:make-process
   #:make-process-lock
   #:process-add-run-reason
   #:process-kill
   #:process-property-list
   #:process-revoke-run-reason
   #:process-run-function
   #:with-process-lock
   #:with-timeout
   #:without-scheduling
   #:*current-process*
   #:lock
   #:process-allow-schedule
   #:process-name
   #:process-preset
   #:process-run-reasons
   #:process-wait
   #:without-interrupts
   #:process-active-p
   ))

(in-package :acl-compat-mp)

; existing stuff from ccl we can reuse directly
(shadowing-import 
 '(
   mp::process-interrrupt
   mp:make-process
   mp:make-process-lock
   mp:process-add-run-reason
   mp:process-kill
   mp:process-property-list
   mp:process-revoke-run-reason
   mp:process-run-function
   mp:with-process-lock
   mp:with-timeout
   mp:without-scheduling
   sys:*current-process*
   mp::lock
   mp:process-allow-schedule
   mp:process-name
   mp:process-preset
   mp:process-run-reasons
   mp:process-wait
   mp:process-active-p
   excl:without-interrupts)
 :acl-compat-mp)

