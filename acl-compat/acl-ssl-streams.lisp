;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;;
;;; Filename:    gray-streams-integration.lisp
;;; Author:      Jochen Schmidt <jsc@dataheaven.de>
;;; Description: Integrate ssl-sockets with the lisp
;;;              stream system using gray-streams.
;;;              

(in-package :ssl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gray Streams integration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ssl-stream-mixin ()
  ((ssl-socket :accessor ssl-socket :initarg :ssl-socket)))

(defclass binary-ssl-stream 
          (ssl-stream-mixin
           trivial-gray-streams:fundamental-binary-input-stream
           trivial-gray-streams:fundamental-binary-output-stream)
  ())

(defclass character-ssl-stream
          (ssl-stream-mixin
           trivial-gray-streams:fundamental-character-input-stream
           trivial-gray-streams:fundamental-character-output-stream)
  ())

(defmethod stream-element-type ((socket-stream binary-ssl-stream))
  '(unsigned-byte 8))

(defmethod stream-element-type ((socket-stream character-ssl-stream))
  'character)

(defmethod trivial-gray-streams:stream-line-column ((socket-stream character-ssl-stream))
  nil)

(defmethod trivial-gray-streams:stream-line-column ((socket-stream binary-ssl-stream))
  nil)

(defmethod trivial-gray-streams:stream-listen ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (> (ssl-internal:ssl-pending (ssl-internal:ssl-socket-handle ssl-socket)) 0)))

(defmethod trivial-gray-streams:stream-read-byte ((socket-stream binary-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-byte ssl-socket)))

(defmethod trivial-gray-streams:stream-write-byte ((socket-stream binary-ssl-stream) byte)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-byte byte ssl-socket)))

#|
(defmethod trivial-gray-streams:stream-read-char ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-char ssl-socket)))

(defmethod trivial-gray-streams:stream-read-char ((socket-stream binary-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-char ssl-socket)))
|#

; Bivalent
(defmethod trivial-gray-streams:stream-read-char ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-char ssl-socket)))


(defmethod trivial-gray-streams:stream-read-char-no-hang ((socket-stream character-ssl-stream))
  (when (listen socket-stream)
    (with-slots (ssl-socket) socket-stream
      (ssl-internal:ssl-socket-read-char ssl-socket))))

#|
(defmethod trivial-gray-streams:stream-write-char ((socket-stream character-ssl-stream) char)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-char char ssl-socket)))

(defmethod trivial-gray-streams:stream-write-char ((socket-stream binary-ssl-stream) char)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-char char ssl-socket)))
|#

; Bivalent
(defmethod trivial-gray-streams:stream-write-char ((socket-stream ssl-stream-mixin) char)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-char char ssl-socket)))



; Bivalent
(defmethod trivial-gray-streams:stream-force-output ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod trivial-gray-streams:stream-finish-output ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod trivial-gray-streams:stream-clear-output ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::output-offset) ssl-socket
      (setf ssl-internal::output-offset 0))))

(defmethod trivial-gray-streams:stream-clear-input ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::input-avail ssl-internal::input-offset) ssl-socket
      (setf ssl-internal::input-avail 0)
      (setf ssl-internal::input-offset 0))))

(defmethod close ((socket-stream ssl-stream-mixin) &key abort)
  (with-slots (ssl-socket) socket-stream
    (unless abort
      (ssl-internal:flush-output-buffer ssl-socket))
    (ssl-internal:close-ssl-socket ssl-socket)))

#|
(defmethod trivial-gray-streams:stream-force-output ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod trivial-gray-streams:stream-finish-output ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod trivial-gray-streams:stream-clear-output ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::output-offset) ssl-socket
      (setf ssl-internal::output-offset 0))))

(defmethod trivial-gray-streams:stream-clear-input ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::input-avail ssl-internal::input-offset) ssl-socket
      (setf ssl-internal::input-avail 0)
      (setf ssl-internal::input-offset 0))))

(defmethod trivial-gray-streams:stream-read-sequence ((socket-stream character-ssl-stream) sequence start end &key &allow-other-keys)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    ;(format t "Read ~A chars from index ~A on.~%" chars start) (force-output t)
    (loop for i upfrom start
          repeat chars
          for char = (progn ;(format t "Read char on index ~A~%" i)
                       ;(force-output t)
                       (let ((c (trivial-gray-streams:stream-read-char socket-stream)))
                         ;(format t "The element read was ~A~%" c) 
			 c))
          if (eq char :eof) do (progn ;(format t "premature return on index ~A~%" i)
                                 ;(force-output t)
                                 (return-from trivial-gray-streams:stream-read-sequence i))
          do (setf (elt sequence i) char))
    ;(format t "Normal return on index ~A~%" (+ start chars)) (force-output t)
    (+ start chars)))

|#

;;
;; Why this argument ordering in CMUCL? LW has (stream sequence start end)
;; It would be interesting to know why it is a particular good idea to
;; reinvent APIs every second day in an incompatible way.... *grrr*
;;

#+cmu
(defmethod trivial-gray-streams:stream-read-sequence ((socket-stream character-ssl-stream) (sequence sequence) start end &key &allow-other-keys)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    (loop for i upfrom start
          repeat chars
          for char = (trivial-gray-streams:stream-read-char socket-stream)
          if (eq char :eof) do (return-from trivial-gray-streams:stream-read-sequence i)
          do (setf (elt sequence i) char))
    (+ start chars)))

#+cmu
(defmethod trivial-gray-streams:stream-read-sequence ((socket-stream binary-ssl-stream) (sequence sequence) start end &key &allow-other-keys)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    (loop for i upfrom start
          repeat chars
          for char = (trivial-gray-streams:stream-read-byte socket-stream)
          if (eq char :eof) do (return-from trivial-gray-streams:stream-read-sequence i)
          do (setf (elt sequence i) char))
    (+ start chars)))

#|
(defmethod trivial-gray-streams:stream-read-sequence ((socket-stream binary-ssl-stream) sequence start end &key &allow-other-keys)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    ;(format t "Read ~A chars from index ~A on.~%" chars start) (force-output t)
    (loop for i upfrom start
          repeat chars
          for char = (progn ;(format t "Read char on index ~A~%" i)
                       ;(force-output t)
                       (let ((c (trivial-gray-streams:stream-read-byte socket-stream)))
                         ;(format t "The element read was ~A~%" c) 
			 c))
          if (eq char :eof) do (progn ;(format t "premature return on index ~A~%" i)
                                 ;(force-output t)
                                 (return-from trivial-gray-streams:stream-read-sequence i))
          do (setf (elt sequence i) char))
    ;(format t "Normal return on index ~A~%" (+ start chars)) (force-output t)
    (+ start chars)))
|#

#| Alternative implementation?
(defmethod stream:stream-read-sequence ((socket-stream character-ssl-stream) sequence start end &key &allow-other-keys)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    (format t "Read ~A chars from index ~A on.~%" chars start) (force-output t)
    (loop for i upfrom start
          repeat chars
          for char = (progn (format t "Read char on index ~A~%" i)
                       (force-output t)
                       (let ((c (stream:stream-read-char socket-stream)))
                         (format t "The element read was ~A~%" c) c))
          if (eq char :eof) do (progn (format t "premature return on index ~A~%" i)
                                 (force-output t)
                                 (return-from stream:stream-read-sequence i))
          do (setf (elt sequence i) char))
    (format t "Normal return on index ~A~%" (+ start chars)) (force-output t)
    (+ start chars)))
|#

#|
(defmethod common-lisp:close ((socket-stream character-ssl-stream) &key abort)
  (with-slots (ssl-socket) socket-stream
    (unless abort
      (ssl-internal:flush-output-buffer ssl-socket))
    (ssl-internal:close-ssl-socket ssl-socket)))
|#

#+lispworks
(declaim (inline %reader-function-for-sequence))
#+lispworks
(defun %reader-function-for-sequence (sequence)
  (typecase sequence
    (string #'read-char)
    ((array unsigned-byte (*)) #'read-byte)
    ((array signed-byte (*)) #'read-byte)
    (otherwise #'read-byte)))

#+lispworks
(declaim (inline %writer-function-for-sequence))
#+lispworks
(defun %writer-function-for-sequence (sequence)
  (typecase sequence
    (string #'write-char)
    ((array unsigned-byte (*)) #'write-byte)
    ((array signed-byte (*)) #'write-byte)
    (otherwise #'write-byte)))

;; Bivalent socket support for READ-SEQUENCE / WRITE-SEQUENCE
#+lispworks
(defmethod trivial-gray-streams:stream-read-sequence ((stream ssl-stream-mixin) sequence start end &key &allow-other-keys)
  (stream::read-elements stream sequence start end (%reader-function-for-sequence sequence)))

#+lispworks
(defmethod trivial-gray-streams:stream-write-sequence ((stream ssl-stream-mixin) sequence start end &key &allow-other-keys)
  (stream::write-elements stream sequence start end (typecase sequence
                                                      (string t)
                                                      ((array unsigned-byte (*)) nil)
                                                      ((array signed-byte (*)) nil)
                                                      (otherwise nil))))

#+lispworks
(in-package :acl-socket)

#+lispworks
(defmethod remote-host ((socket ssl::ssl-stream-mixin))
  (comm:get-socket-peer-address (ssl-internal::ssl-socket-fd (ssl::ssl-socket socket))))

#+lispworks
(defmethod remote-port ((socket ssl::ssl-stream-mixin))
  (multiple-value-bind (host port)
      (comm:get-socket-peer-address (ssl-internal::ssl-socket-fd (ssl::ssl-socket socket)))
    (declare (ignore host))
    port))

#+lispworks
(defmethod local-host ((socket ssl::ssl-stream-mixin))
  (multiple-value-bind (host port)
      (comm:get-socket-address (ssl-internal::ssl-socket-fd (ssl::ssl-socket socket)))
    (declare (ignore port))
    host))

#+lispworks
(defmethod local-port ((socket ssl::ssl-stream-mixin))
  (multiple-value-bind (host port)
      (comm:get-socket-address (ssl-internal::ssl-socket-fd (ssl::ssl-socket socket)))
    (declare (ignore host))
    port))
    
