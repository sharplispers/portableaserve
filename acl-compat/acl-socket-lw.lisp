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
	 :reader port)
   (proc :initarg :proc
         :reader server-socket-process)
   (mbox :initarg :mbox
         :reader server-socket-mbox)))

(defmethod print-object ((socket server-socket) stream)
  (print-unreadable-object (socket stream :type t :identity nil)
    (format stream "@~d on port ~d" (fd socket) (port socket))))

(defmethod accept-connection ((server-socket server-socket)
			      &key (wait t))
  (let ((mbox (server-socket-mbox server-socket)))
    (when (or wait (mp:mailbox-peek mbox))
      (make-instance
       'comm:socket-stream :direction :io
       :socket (mp:mailbox-read mbox)
       :element-type (element-type server-socket)))))

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
       (let* ((mbox (mp:make-mailbox :size 1))
              (proc (comm:start-up-server
                     :function (lambda (sock) 
                                 (mp:mailbox-send mbox sock)
                                 (mp:notice-fd sock))
                     :service local-port)))
         (make-instance 'server-socket
                        :mbox mbox
		        :port local-port
                        :proc proc
                        :fd (first (mp::process-wait-function-arguments proc))
                        :element-type element-type)))
      (:active
       (let ((stream (comm:open-tcp-stream remote-host remote-port
			                   :direction :io
			                   :element-type element-type)))
         (mp:notice-fd (comm:socket-stream-socket stream))
         stream)))))

(defmethod close ((server server-socket) &key abort)
  (declare (ignore abort))
  (mp:process-kill (server-socket-process server)))

(declaim (ftype (function ((unsigned-byte 32)) (values simple-string))
		ipaddr-to-dotted))
(defun ipaddr-to-dotted (ipaddr)
  (declare (type (unsigned-byte 32) ipaddr))
  (format nil "~d.~d.~d.~d"
	  (logand #xff (ash ipaddr -24)) (logand #xff (ash ipaddr -16))
	  (logand #xff (ash ipaddr -8)) (logand #xff ipaddr)))

(defun string-tokens (string)
  (labels ((get-token (str pos1 acc)
                      (let ((pos2 (position #\Space str :start pos1)))
                        (if (not pos2)
                            (nreverse acc)
                          (get-token str (1+ pos2) (cons (read-from-string (subseq str pos1 pos2))
                                                         acc))))))
(get-token (concatenate 'string string " ") 0 nil)))

(declaim (ftype (function (string) (values (unsigned-byte 32)))
		dotted-to-ipaddr))
(defun dotted-to-ipaddr (dotted)
  (declare (string dotted))
  (let ((ll (string-tokens (substitute #\Space #\. dotted))))
    (+ (ash (first ll) 24) (ash (second ll) 16)
       (ash (third ll) 8) (fourth ll))))

(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
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
    port))

(defun local-host (socket-stream)
   (comm:socket-stream-address socket-stream))

(defun local-port (socket-stream)
  (if (typep socket-stream 'socket::server-socket)
      (port socket-stream)
    (multiple-value-bind (host port)
        (comm:socket-stream-address socket-stream)
      port)))

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  (declare (ignore stream output-chunking output-chunking-eof input-chunking))
  (warn "SOCKET-CONTROL function not implemented."))

(provide 'acl-socket)
