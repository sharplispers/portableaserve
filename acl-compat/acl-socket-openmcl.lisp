;;; OpenMCL layer for ACL sockets.
;;; Most everything is already there, just needs to be in the socket package.
;;;
;;; John DeSoi, Ph.D. desoi@users.sourceforget.net


(defpackage :acl-compat.socket
  (:nicknames :socket :acl-socket)
  (:use :common-lisp)
  (:export #:make-socket 
           #:accept-connection
           #:ipaddr-to-dotted 
           #:dotted-to-ipaddr 
           #:ipaddr-to-hostname 
           #:lookup-hostname
           #:remote-host 
           #:remote-port 
           #:local-host 
           #:local-port 
           #:socket-control
           ))


(in-package :socket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (shadowing-import
   '(ccl:make-socket
     ccl:accept-connection
     ccl:dotted-to-ipaddr 
     ccl:ipaddr-to-hostname
     ccl:lookup-hostname
     ccl:remote-host 
     ccl:remote-port 
     ccl:local-host 
     ccl:local-port socket-control)
   :socket)
  
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (export
   '(make-socket
     accept-connection
     ipaddr-to-dotted 
     dotted-to-ipaddr 
     ipaddr-to-hostname
     lookup-hostname
     remote-host 
     remote-port 
     local-host 
     local-port socket-control)
   :socket)
  
  )


; OpenMCL has a built-in ipaddr-to-dotted. But it appears that sometimes
; the log function is being called after the connection is closed and
; it causes nil to be passed to ipaddr-to-dotted. So we wrap ipaddr-to-dotten
; to ensure only non-nil values are passed.

(defun ipaddr-to-dotted (ipaddr &key values)
  (unless (null ipaddr)
    (ccl:ipaddr-to-dotted ipaddr :values values)))

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream))
  (warn "SOCKET-CONTROL function not implemented.")
  (when (or output-chunking output-chunking-eof input-chunking)
    (error "Chunking is not yet supported in OpenMCL Restart the server with chunking off.") ) )


(provide 'acl-socket)
