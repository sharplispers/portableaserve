;; This package is designed for cmucl.  It implements the
;; ACL-style socket interface on top of cmucl.
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks and net.lisp in the port library of CLOCC.

(defpackage socket
  (:use "MP" "COMMON-LISP")
  (:export make-socket accept-connection
   ipaddr-to-dotted dotted-to-ipaddr ipaddr-to-hostname lookup-hostname
   remote-host remote-port local-host local-port socket-control))

(in-package socket)

(defclass socket ()
  ((fd :type fixnum
       :initarg :fd
       :reader fd)))

(defmethod print-object ((socket socket) stream)
  (print-unreadable-object (socket stream :type t :identity t)
    (format stream "@~d" (fd socket))))

(defclass server-socket (socket)
  ((element-type :type (member signed-byte unsigned-byte base-char)
		 :initarg :element-type
		 :reader element-type)
   (port :type fixnum
	 :initarg :port
	 :reader port)))

(defmethod print-object ((socket server-socket) stream)
  (print-unreadable-object (socket stream :type t :identity nil)
    (format stream "@~d on port ~d" (fd socket) (port socket))))

(defmethod accept-connection ((server-socket server-socket)
			      &key (wait t))
  "Return a bidirectional stream connected to socket, or nil if no
client wanted to initiate a connection and wait is nil."
  ;; fixxme: perhaps check whether we run multiprocessing and use
  ;; sys:wait-until-fd-usable instead of
  ;; mp:process-wait-until-fd-usable here?

  ;; api pipe fitting: wait t ==> timeout nil
  (when (mp:process-wait-until-fd-usable (fd server-socket) :input
                                         (if wait nil 0))
          (sys:make-fd-stream (ext:accept-tcp-connection (fd server-socket))
                              :input t :output t
                              :element-type (element-type server-socket)
                              :auto-close t)))

(defun make-socket (&key (remote-host "localhost")
			 local-port
			 remote-port 
			 (connect :active)
			 (format :text)
			 &allow-other-keys)
  "Return a stream connected to remote-host if connect is :active, or
something listening on local-port that can be fed to accept-connection
if connect is :passive."
  (check-type remote-host string)
  (let ((element-type (ecase format
			(:text 'base-char)
			(:binary 'signed-byte)
                        (:bivalent 'unsigned-byte))))
    (ecase connect 
      (:passive
         (make-instance 'server-socket
		        :port local-port
                        :fd (ext:create-inet-listener local-port)
                        :element-type element-type))
      (:active
       (sys:make-fd-stream (ext:connect-to-inet-socket remote-host remote-port)
                           :input t :output t :element-type element-type)))))
       
(defmethod close ((server server-socket) &key abort)
  "Kill a passive (listening) socket.  (Active sockets are actually
streams and handled by their close methods."
  (declare (ignore abort))
  (unix:unix-close (fd server)))

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
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in IPADDR-TO-HOSTNAME not supported."))
  (ext:host-entry-name (ext:lookup-host-entry ipaddr)))

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (car (ext:host-entry-addr-list (ext:lookup-host-entry host)))
      (dotted-to-ipaddr (ipaddr-to-dotted host))))

(defun remote-host (socket-stream)
  (let ((fd (sys:fd-stream-fd socket-stream)))
    (ext:get-peer-host-and-port fd)))

(defun remote-port (socket-stream)
  (let ((fd (sys:fd-stream-fd socket-stream)))
    (multiple-value-bind (host port)
        (ext:get-peer-host-and-port fd)
      (declare (ignore host))
      port)))

(defun local-host (socket-stream)
  (let ((fd (sys:fd-stream-fd socket-stream)))
    (ext:get-socket-host-and-port fd)))

(defun local-port (socket-stream)
    (let ((fd (sys:fd-stream-fd socket-stream)))
    (multiple-value-bind (host port)
        (ext:get-socket-host-and-port fd)
      (declare (ignore host))
      port)))

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream output-chunking output-chunking-eof input-chunking))
  (warn "SOCKET-CONTROL function not implemented."))

(provide 'acl-socket)
