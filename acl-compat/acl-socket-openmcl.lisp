;;; OpenMCL layer for ACL sockets.
;;; Most everything is already there, just needs to be in the socket package.
;;;
;;; John DeSoi, Ph.D. desoi@mac.com


(defpackage socket
  (:use :common-lisp)
  (:export make-socket accept-connection
           ipaddr-to-dotted dotted-to-ipaddr ipaddr-to-hostname lookup-hostname
           remote-host remote-port local-host local-port socket-control
           ))

(in-package :socket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (shadowing-import
   '(ccl:make-socket
     ccl:accept-connection
     ccl:ipaddr-to-dotted 
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
; it causes nil to be passed to ipaddr-to-dotted. So until we have a fix,
; we just provide this to deal with nil values. The aserve log-request 
; method should really check to see that it has a good value.
;
; Second reason for this: ccl:ipaddr-to-dotted gives the wrong result.

(defun ccl:ipaddr-to-dotted (ipaddr &key values)
  (if (null ipaddr)
    (if values (values 0 0 0 0) "0.0.0.0")
    (let ((a (logand #xff (ash ipaddr -24)))
	  (b (logand #xff (ash ipaddr -16)))
	  (c (logand #xff (ash ipaddr -8)))
	  (d (logand #xff ipaddr)))
      (if values
	(values a b c d)
        (format nil "~d.~d.~d.~d" a b c d)))))

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream))
  (warn "SOCKET-CONTROL function not implemented.")
  (when (or output-chunking output-chunking-eof input-chunking)
    (error "Chunking is not yet supported in OpenMCL Restart the server with chunking off.") ) )


(provide 'acl-socket)
