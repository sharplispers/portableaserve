;; This package is designed for sbcl.  It implements the
;; ACL-style socket interface on top of sbcl.
;;
;; Written by Rudi Schlatte, based on the work done by Jochen Schmidt
;; for Lispworks and net.lisp in the port library of CLOCC.

(in-package #:acl-compat.socket)

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

(defmethod local-host ((socket server-socket))
  (vector-to-ipaddr #(127 0 0 1)))

(defmethod local-port ((socket server-socket))
  (port socket))

(defclass datagram-socket (server-socket)
  ())


(defmethod print-object ((socket server-socket) stream)
  (print-unreadable-object (socket stream :type t :identity nil)
    (format stream "listening on port ~d" (port socket))))

(defmethod print-object ((socket datagram-socket) stream)
  (print-unreadable-object (socket stream :type t :identity nil)
    (format stream "datagram socket listening on port ~d" (port socket))))


(defun filter-args (plist keys)
  "Filter out PLIST entries that do not appear in keys, and return the resulting filtered list."
  (loop :for (x y) :on plist :by #'cddr
        :when (member x keys :test #'eq)
          :append (list x y)))

(defun socket-make-stream (socket &rest args &key stream-type &allow-other-keys)
  (let ((stream (apply #'sb-bsd-sockets:socket-make-stream socket
                       (filter-args args '(:input :output :element-type
                                           :buffering :external-format
                                           :timeout
                                           :auto-close :serve-events)))))
    (make-instance (ecase stream-type
                     (:text 'socket-text-io-stream)
                     (:binary 'socket-binary-io-stream)
                     (:bivalent 'socket-bivalent-io-stream))
                   :stream stream :socket socket)))

(defgeneric accept-connection (socket &key wait))
(defmethod accept-connection ((server-socket server-socket)
                              &key (wait t))
  "Return a bidirectional stream connected to socket."
  (if (sb-sys:wait-until-fd-usable (socket-file-descriptor (socket server-socket))
                                   :input (if (numberp wait) wait nil))
      (let* ((socket (socket-accept (socket server-socket)))
             (stream (socket-make-stream socket
                                         :input t :output t
                                         :stream-type (stream-type server-socket)
                                         ;; :buffering :none
                                         :element-type
                                         (element-type server-socket)))
             (socket-stream (make-instance (ecase  (stream-type server-socket)
                                             (:bivalent
                                              'socket-bivalent-io-stream)
                                             (:text
                                              'socket-text-io-stream)
                                             (:binary 'socket-binary-io-stream))
                                           :stream stream
                                           :socket socket)))
        socket-stream)
      nil))

(defmethod receive-from ((socket datagram-socket) size &key buffer extract)
  (multiple-value-bind (rbuf len address port)
      (socket-receive (socket socket) buffer size)
    (declare (ignore port))
    (let ((buf
           (if (not extract)
               rbuf
             (subseq rbuf 0 len)))) ;; FIXME: am I right?
      (when buffer
          (replace buffer buf :end2 len))
      (values
       (if buffer buffer buf)
       len
       address))))

(defmethod send-to ((socket datagram-socket) buffer size &key remote-host remote-port)
  (let* ((rhost (typecase remote-host
                  (string (lookup-hostname remote-host))
                  (otherwise remote-host)))
         (s (socket socket))
         (stream (progn
                   (socket-connect s rhost remote-port)
                   (socket-make-stream s :input t :output t :buffering :none))))
    (write-sequence buffer stream)
    size))



(defun make-socket (&key
                    (type :stream)
                    (remote-host "localhost")
                    local-port
                    remote-port
                    (connect :active)
                    (format :text)
                    (reuse-address t)
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
                        (:text 'character)
                        (:binary 'signed-byte)
                        (:bivalent :default)))
        (socket
         (if (eq type :datagram)
             (progn
               (setf connect :passive-udp)
               (make-instance 'inet-socket :type :datagram :protocol :udp))
           (make-instance 'inet-socket :type :stream :protocol :tcp))))
    (ecase connect
      (:passive-udp
       (setf (sockopt-reuse-address socket) reuse-address)
       (if local-port
           (socket-bind socket #(0 0 0 0) local-port))
       (make-instance 'datagram-socket
                      :port (nth-value 1 (socket-name socket))
                      :socket socket
                      :element-type element-type
                      :stream-type format))
      (:passive
       (setf (sockopt-reuse-address socket) reuse-address)
       (when local-port
           (socket-bind socket #(0 0 0 0) local-port))
       (socket-listen socket 10)        ;Arbitrarily chosen backlog value
       ;; this still returns a socket, not a stream -- we get the
       ;; stream from accept
       (make-instance 'server-socket
                      :port (nth-value 1 (socket-name socket))
                      :socket socket
                      :element-type element-type
                      :stream-type format))
      (:active
       (socket-connect socket (lookup-hostname remote-host) remote-port)
       (let ((stream (sb-bsd-sockets:socket-make-stream
                      socket :input t
                             :output t
                             :element-type element-type)))
         ;; this stream should be bidirectional.
         (assert (and (input-stream-p stream) (output-stream-p stream)))
         (format t "~&ACL-SOCKET: Created stream for socket ~s.  Element type = ~a~%"
               socket
               (stream-element-type stream))
         (make-instance (ecase format
                          (:bivalent 'socket-bivalent-io-stream)
                          (:text 'socket-text-io-stream)
                          (:binary 'socket-binary-io-stream))
                        :stream stream :socket socket))))))

(defmethod close ((server server-socket) &key abort)
  "Kill a passive (listening) socket.  (Active sockets are actually
streams and handled by their close methods."
  (declare (ignore abort))
  (socket-close (socket server)))

#+ignore
(declaim (ftype (function ((unsigned-byte 32) &key (:values t))
                          (or (values fixnum fixnum fixnum fixnum)
                              (values simple-string)))
                ipaddr-to-dotted))
(defun ipaddr-to-dotted (ipaddr &key values)
  "Convert from 32-bit integer to dotted string."
  (declare (type (unsigned-byte 32) ipaddr))
  (let ((a (logand #xff (ash ipaddr -24)))
        (b (logand #xff (ash ipaddr -16)))
        (c (logand #xff (ash ipaddr -8)))
        (d (logand #xff ipaddr)))
    (if values
        (values a b c d)
      (format nil "~d.~d.~d.~d" a b c d))))

(defun ipaddr-to-vector (ipaddr)
  "Convert from 32-bit integer to a vector of octets."
  (declare (type (unsigned-byte 32) ipaddr))
  (let ((a (logand #xff (ash ipaddr -24)))
        (b (logand #xff (ash ipaddr -16)))
        (c (logand #xff (ash ipaddr -8)))
        (d (logand #xff ipaddr)))
    (make-array 4
                :element-type '(unsigned-byte 8)
                :initial-contents (list a b c d))))

(declaim (ftype (function (vector)
                          (values (unsigned-byte 32)))
                vector-to-ipaddr))
(defun vector-to-ipaddr (sensible-ipaddr)
  "Convert from 4-integer vector to 32-bit integer."
  (loop with result = 0
        for component across sensible-ipaddr
        do (setf result (+ (ash result 8) component))
        finally (return result)))

(defun string-tokens (string)
  (labels ((get-token (str pos1 acc)
                      (let ((pos2 (position #\Space str :start pos1)))
                        (if (not pos2)
                            (nreverse acc)
                          (get-token str (1+ pos2) (cons (read-from-string (subseq str pos1 pos2))
                                                         acc))))))
    (get-token (concatenate 'string string " ") 0 nil)))

(declaim (ftype (function (string &key (:errorp t))
                          (values (or null (unsigned-byte 32))
                                  (or condition fixnum)
                                  ))
                dotted-to-ipaddr))
(defun dotted-to-ipaddr (dotted &key (errorp t))
  "Convert from dotted string to 32-bit integer.  For partial dotted
notations (as in authorization patterns), return the bit width of the
dotted notation."
  (declare (string dotted))
  (if errorp
      (let ((ll (string-tokens (substitute #\Space #\. dotted))))
        (values
         (+ (ash (first ll) 24) (ash (or (second ll) 0) 16)
            (ash (or (third ll) 0) 8) (or (fourth ll) 0))
         (* (length ll) 8)))
    (ignore-errors
        (let ((ll (string-tokens (substitute #\Space #\. dotted))))
          (values
           (+ (ash (first ll) 24) (ash (or (second ll) 0) 16)
              (ash (or (third ll) 0) 8) (or (fourth ll) 0))
           (* (length ll) 8))))))

(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in IPADDR-TO-HOSTNAME not supported."))
  (host-ent-name (get-host-by-address (ipaddr-to-vector ipaddr))))

(defun lookup-hostname (host &key ignore-cache)
  (when ignore-cache
    (warn ":IGNORE-CACHE keyword in LOOKUP-HOSTNAME not supported."))
  (if (stringp host)
      (host-ent-address (get-host-by-name host))
      (dotted-to-ipaddr (ipaddr-to-dotted host))))

;; Now, throw chunking in the mix

(defclass chunked-stream (de.dataheaven.chunked-stream-mixin::chunked-stream-mixin
                          gray-stream::buffered-bivalent-stream)
  ((plist :initarg :plist :accessor stream-plist)))


;; (defun make-bivalent-stream (lisp-stream &key plist)
;;   (make-instance 'chunked-stream :lisp-stream lisp-stream :plist plist))

(defgeneric socket-control (stream &key output-chunking output-chunking-eof input-chunking)
  #+nil(:method ((stream de.dataheaven.chunked-stream-mixin::chunked-stream-mixin)
            &key (output-chunking nil oc-p) output-chunking-eof (input-chunking nil ic-p))
    (when oc-p
      (when output-chunking
        (de.dataheaven.chunked-stream-mixin::initialize-output-chunking stream))
      (setf (de.dataheaven.chunked-stream-mixin::output-chunking-p stream)
            output-chunking))
    (when output-chunking-eof
      (de.dataheaven.chunked-stream-mixin::disable-output-chunking stream))
    (when ic-p
      (when input-chunking
        (de.dataheaven.chunked-stream-mixin::initialize-input-chunking stream))
      (setf (de.dataheaven.chunked-stream-mixin::input-chunking-p stream)
            input-chunking)))
  (:method ((stream socket-stream)
            &key (output-chunking nil oc-p) output-chunking-eof (input-chunking nil ic-p))
    (declare (ignorable output-chunking-eof))
    (apply #'socket-control (socket-stream-stream stream)
           `(,@(when oc-p (list :output-chunking output-chunking))
             ,@(when ic-p (list :input-chunking input-chunking)))))
  (:method (stream  &key output-chunking output-chunking-eof input-chunking)
    (declare (ignore output-chunking output-chunking-eof input-chunking)
             (ignorable stream))
    (error "Output-chunking not available on stream ~s" stream)))
#|
(defmethod remote-host ((socket-stream chunked-stream))
  (let ((socket (getf (stream-plist socket-stream) :socket)))
    (vector-to-ipaddr (socket-peername socket))))

(defmethod remote-port ((socket-stream chunked-stream))
  (let ((socket (getf (stream-plist socket-stream) :socket)))
    (nth-value 1 (socket-peername socket))))

(defmethod local-host ((socket-stream chunked-stream))
  (let ((socket (getf (stream-plist socket-stream) :socket)))
    (if socket (vector-to-ipaddr (socket-name socket))
        (progn
          (warn "Socket not in plist of ~S -- could not get local host" socket-stream)
          0)))  )

(defmethod local-port ((socket-stream chunked-stream))
  (let ((socket (getf (stream-plist socket-stream) :socket)))
    (if socket (nth-value 1 (socket-name socket))
        (progn (warn "Socket not in plist of ~S -- could not get local port" socket-stream)
               0))))
|#







(provide 'acl-socket)
