;; This package is designed for cmucl.  It implements the
;; ACL-style socket interface on top of cmucl.
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks and net.lisp in the port library of CLOCC.

(in-package #:acl-socket)

(defclass server-socket ()
  ((socket :initarg :socket :reader socket
           :initform (error "No value supplied for socket"))
   (element-type :type (member signed-byte unsigned-byte base-char)
		 :initarg :element-type
		 :reader element-type
                 :initform (error "No value supplied for element-type"))
   (port :type fixnum
	 :initarg :port
	 :reader port
         :initform (error "No value supplied for port"))
   (stream-type :type (member :text :binary :bivalent)
                :initarg :stream-type
                :reader stream-type
                :initform (error "No value supplied for stream-type"))))

(defmethod print-object ((socket server-socket) stream)
  (print-unreadable-object (socket stream :type t :identity nil)
    (format stream "listening on port ~d" (port socket))))

(defmethod accept-connection ((server-socket server-socket)
			      &key (wait t))
  "Return a bidirectional stream connected to socket."
  (if (sb-sys:wait-until-fd-usable (socket-file-descriptor (socket server-socket))
                                   :input (if (numberp wait) wait nil))
      (let* ((socket (socket-accept (socket server-socket)))
             (stream (socket-make-stream socket
                                         :input t :output t
                                        ; :buffering :none
                                         :element-type
                                         (element-type server-socket)
                                         :auto-close t)))
        (if (eq (stream-type server-socket) :bivalent)
            (excl::make-bivalent-stream stream)
            stream))
      nil))

(defun make-socket (&key (remote-host "localhost")
                    local-port
                    remote-port
                    (connect :active)
                    (format :text)
                    &allow-other-keys)
  "Return a stream connected to remote-host if connect is :active, or
something listening on local-port that can be fed to accept-connection
if connect is :passive.

This is an incomplete implementation of ACL's make-socket function!
It was written to provide the functionality necessary to port
AllegroServe.  Refer to
http://franz.com/support/documentation/6.1/doc/pages/operators/socket/make-socket.htm
to read about the missing parts."
  (check-type remote-host string)
  (let ((element-type (ecase format
			(:text 'base-char)
			(:binary 'signed-byte)
                        (:bivalent 'unsigned-byte)))
        (socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (ecase connect
      (:passive
       (setf (sockopt-reuse-address socket) t)
       (socket-bind socket #(0 0 0 0) local-port)
       (socket-listen socket 10)        ;Arbitrarily chosen backlog value
       (make-instance 'server-socket
                      :port local-port
                      :socket socket
                      :element-type element-type
                      :stream-type format))
      (:active
       (socket-connect socket (lookup-hostname remote-host) remote-port)
       (let ((stream (socket-make-stream socket :input t :output t
                                         :element-type element-type
                                        ; :buffering :none
                                           )))
           (if (eq :bivalent format)
               (excl::make-bivalent-stream stream)
               stream))))))

(defmethod close ((server server-socket) &key abort)
  "Kill a passive (listening) socket.  (Active sockets are actually
streams and handled by their close methods."
  (declare (ignore abort))
  (socket-close (socket server)))

(declaim (ftype (function ((unsigned-byte 32) &key (:values t))
                          (values simple-string))
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
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in IPADDR-TO-HOSTNAME not supported."))
  (host-ent-name (get-host-by-address (make-inet-address ipaddr))))

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (host-ent-address (get-host-by-name host))
      (dotted-to-ipaddr (ipaddr-to-dotted host))))

(defun remote-host (socket-stream)
  (warn "remote-host not implemented!")
  -1)

(defun remote-port (socket-stream)
  (warn "remote-port not implemented!")
  -1)

(defun local-host (socket-stream)
  (warn "local-host not implemented!")
  -1)

(defun local-port (socket-stream)
  (warn "local-port not implemented!")
  -1)

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream))
  (warn "SOCKET-CONTROL function not implemented.")
  (when (or output-chunking output-chunking-eof input-chunking)
    (error "Chunking is not yet supported in cmucl. Restart the server with chunking off.")))


(provide 'acl-socket)