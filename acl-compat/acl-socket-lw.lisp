;; This package is designed for LispWorks.  It implements the
;; ACL-style socket interface on top of LispWorks.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defpackage socket
  (:use comm cl)
  #+cl-ssl(:import-from :ssl "MAKE-SSL-CLIENT-STREAM" "MAKE-SSL-SERVER-STREAM")
  (:export make-socket accept-connection
   ipaddr-to-dotted dotted-to-ipaddr ipaddr-to-hostname lookup-hostname
   remote-host remote-port local-host local-port socket-control))

#+cl-ssl
(eval-when (:compile-toplevel :load-toplevel :execute)
(ssl-internal::initialize-ssl-library)
)

(in-package socket)

(defclass server-socket ()
  ((element-type :type (member signed-byte unsigned-byte base-char)
		 :initarg :element-type
		 :reader element-type)
   (port :type fixnum
	 :initarg :port
	 :reader port)
   (passive-socket :initarg :passive-socket
	   :reader passive-socket)))

(defmethod fd ((server-socket server-socket))
  42)

(defmethod print-object ((server-socket server-socket) stream)
  (print-unreadable-object (server-socket stream :type t :identity nil)
    (format stream "@~d on port ~d" (fd server-socket) (port server-socket))))

(defmethod accept-connection ((server-socket server-socket)
			      &key (wait t))
  (unless wait
    (cerror "Proceed anyway, and risk blocking." ()
	    "Nonclocking accept-connection not implemented."))
  (make-instance 'comm:socket-stream
		 :socket (comm::get-fd-from-socket (passive-socket server-socket))
		 :direction :io
		 :element-type (element-type server-socket)))

(defun %new-passive-socket (local-port)
  (multiple-value-bind (socket error-location error-code)
      (comm::create-tcp-socket-for-service local-port)
    (cond (socket socket)
	  (t (error "Passive socket creation failed: ~A (~A)" error-location
		    error-code)))))

(defun make-socket (&key (remote-host "localhost")
			 local-port
			 remote-port 
			 (connect :active)
			 (format :text)
			 &allow-other-keys)
  (check-type remote-host string)
  (let ((element-type (ecase format
			(:text 'base-char)
			(:binary 'signed-byte)
                        (:bivalent 'unsigned-byte))))
    (ecase connect 
      (:passive
       (make-instance 'server-socket
		      :port local-port
		      :passive-socket (%new-passive-socket local-port)
		      :element-type element-type))
      (:active
       (comm:open-tcp-stream remote-host remote-port
			     :direction :io
			     :element-type element-type)))))

(defmethod close ((server-socket server-socket) &key abort)
  (declare (ignore abort))
  (comm::close-socket (passive-socket server-socket)))

(declaim (ftype (function ((unsigned-byte 32)) (values simple-string))
		ipaddr-to-dotted))
(defun ipaddr-to-dotted (ipaddr &key values)
  (declare (type (unsigned-byte 32) ipaddr))
  (let ((a (logand #xff (ash ipaddr -24)))
	(b (logand #xff (ash ipaddr -16)))
	(c (logand #xff (ash ipaddr -8)))
	(d (logand #xff ipaddr)))
    (if values
	(values a b c d)
      (format nil "~d.~d.~d.~d" a b c d))))

(defun string-tokens (string)
  (labels ((get-token (str pos1 acc)
                      (let ((pos2 (position #\Space str :start pos1)))
                        (if (not pos2)
                            (nreverse acc)
                          (get-token str (1+ pos2) (cons (read-from-string (subseq str pos1 pos2))
                                                         acc))))))
(get-token (concatenate 'string string " ") 0 nil)))

(declaim (ftype (function (string &key (:errorp t))
                          (values (unsigned-byte 32)))
		dotted-to-ipaddr))
(defun dotted-to-ipaddr (dotted &key (errorp t))
  (declare (string dotted))
  (if errorp
      (let ((ll (string-tokens (substitute #\Space #\. dotted))))
	(+ (ash (first ll) 24) (ash (second ll) 16)
	   (ash (third ll) 8) (fourth ll)))
    (ignore-errors 
      (let ((ll (string-tokens (substitute #\Space #\. dotted))))
	(+ (ash (first ll) 24) (ash (second ll) 16)
	   (ash (third ll) 8) (fourth ll))))))

(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
  (declare (ignore ignore-cache))
  (multiple-value-bind (name)
      (comm:get-host-entry (ipaddr-to-dotted ipaddr) :fields '(:name))
    name))

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (multiple-value-bind (addr)
	  (comm:get-host-entry host :fields '(:address))
	addr)
    (dotted-to-ipaddr (ipaddr-to-dotted host))))

(defun remote-host (socket-stream)
   (comm:socket-stream-peer-address socket-stream))

(defun remote-port (socket-stream)
  (multiple-value-bind (host port)
      (comm:socket-stream-peer-address socket-stream)
    (declare (ignore host))
    port))

(defun local-host (socket-stream)
   (comm:socket-stream-address socket-stream))

(defun local-port (socket-stream)
  (if (typep socket-stream 'socket::server-socket)
      (port socket-stream)
    (multiple-value-bind (host port)
        (comm:socket-stream-address socket-stream)
      (declare (ignore host))
      port)))

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream output-chunking output-chunking-eof input-chunking))
  (warn "SOCKET-CONTROL function not implemented."))

(provide 'acl-socket)
