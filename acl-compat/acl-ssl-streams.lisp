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
  ((ssl-socket :initarg :ssl-socket)))

(defclass binary-ssl-stream 
          (ssl-stream-mixin
           gray-stream:fundamental-binary-input-stream
           gray-stream:fundamental-binary-output-stream)
  ())

(defclass character-ssl-stream
          (ssl-stream-mixin
           gray-stream:fundamental-character-input-stream
           gray-stream:fundamental-character-output-stream)
  ())

(defmethod gray-stream::stream-element-type ((socket-stream binary-ssl-stream))
  '(unsigned-byte 8))

(defmethod gray-stream::stream-element-type ((socket-stream character-ssl-stream))
  'character)

(defmethod gray-stream:stream-line-column ((socket-stream character-ssl-stream))
  nil)

(defmethod gray-stream:stream-listen ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (> (ssl-internal:ssl-pending (ssl-internal:ssl-socket-handle ssl-socket)) 0)))

(defmethod gray-stream:stream-read-byte ((socket-stream binary-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-byte ssl-socket)))

(defmethod gray-stream:stream-write-byte ((socket-stream binary-ssl-stream) byte)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-byte byte ssl-socket)))

#|
(defmethod gray-stream:stream-read-char ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-char ssl-socket)))

(defmethod gray-stream:stream-read-char ((socket-stream binary-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-char ssl-socket)))
|#

; Bivalent
(defmethod gray-stream:stream-read-char ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-read-char ssl-socket)))


(defmethod gray-stream:stream-read-char-no-hang ((socket-stream character-ssl-stream))
  (when (listen socket-stream)
    (with-slots (ssl-socket) socket-stream
      (ssl-internal:ssl-socket-read-char ssl-socket))))

#|
(defmethod gray-stream:stream-write-char ((socket-stream character-ssl-stream) char)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-char char ssl-socket)))

(defmethod gray-stream:stream-write-char ((socket-stream binary-ssl-stream) char)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-char char ssl-socket)))
|#

; Bivalent
(defmethod gray-stream:stream-write-char ((socket-stream ssl-stream-mixin) char)
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:ssl-socket-write-char char ssl-socket)))



; Bivalent
(defmethod gray-stream:stream-force-output ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod gray-stream:stream-finish-output ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod gray-stream:stream-clear-output ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::output-offset) ssl-socket
      (setf ssl-internal::output-offset 0))))

(defmethod gray-stream:stream-clear-input ((socket-stream ssl-stream-mixin))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::input-avail ssl-internal::input-offset) ssl-socket
      (setf ssl-internal::input-avail 0)
      (setf ssl-internal::input-offset 0))))

(defmethod common-lisp:close ((socket-stream ssl-stream-mixin) &key abort)
  (with-slots (ssl-socket) socket-stream
    (unless abort
      (ssl-internal:flush-output-buffer ssl-socket))
    (ssl-internal:close-ssl-socket ssl-socket)))

#|
(defmethod gray-stream:stream-force-output ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod gray-stream:stream-finish-output ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (ssl-internal:flush-output-buffer ssl-socket)))

(defmethod gray-stream:stream-clear-output ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::output-offset) ssl-socket
      (setf ssl-internal::output-offset 0))))

(defmethod gray-stream:stream-clear-input ((socket-stream character-ssl-stream))
  (with-slots (ssl-socket) socket-stream
    (with-slots (ssl-internal::input-avail ssl-internal::input-offset) ssl-socket
      (setf ssl-internal::input-avail 0)
      (setf ssl-internal::input-offset 0))))

(defmethod gray-stream:stream-read-sequence ((socket-stream character-ssl-stream) sequence start end)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    ;(format t "Read ~A chars from index ~A on.~%" chars start) (force-output t)
    (loop for i upfrom start
          repeat chars
          for char = (progn ;(format t "Read char on index ~A~%" i)
                       ;(force-output t)
                       (let ((c (gray-streams:stream-read-char socket-stream)))
                         ;(format t "The element read was ~A~%" c) 
			 c))
          if (eq char :eof) do (progn ;(format t "premature return on index ~A~%" i)
                                 ;(force-output t)
                                 (return-from gray-streams:stream-read-sequence i))
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
(defmethod gray-stream:stream-read-sequence ((sequence sequence) (socket-stream character-ssl-stream) &optional start end)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    (loop for i upfrom start
          repeat chars
          for char = (gray-stream:stream-read-char socket-stream)
          if (eq char :eof) do (return-from gray-stream:stream-read-sequence i)
          do (setf (elt sequence i) char))
    (+ start chars)))

#+cmu
(defmethod gray-stream:stream-read-sequence ((sequence sequence) (socket-stream binary-ssl-stream) &optional start end)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    (loop for i upfrom start
          repeat chars
          for char = (gray-stream:stream-read-byte socket-stream)
          if (eq char :eof) do (return-from gray-stream:stream-read-sequence i)
          do (setf (elt sequence i) char))
    (+ start chars)))

#|
(defmethod gray-stream:stream-read-sequence ((socket-stream binary-ssl-stream) sequence start end)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    ;(format t "Read ~A chars from index ~A on.~%" chars start) (force-output t)
    (loop for i upfrom start
          repeat chars
          for char = (progn ;(format t "Read char on index ~A~%" i)
                       ;(force-output t)
                       (let ((c (gray-streams:stream-read-byte socket-stream)))
                         ;(format t "The element read was ~A~%" c) 
			 c))
          if (eq char :eof) do (progn ;(format t "premature return on index ~A~%" i)
                                 ;(force-output t)
                                 (return-from gray-streams:stream-read-sequence i))
          do (setf (elt sequence i) char))
    ;(format t "Normal return on index ~A~%" (+ start chars)) (force-output t)
    (+ start chars)))
|#

#| Alternative implementation?
(defmethod stream:stream-read-sequence ((socket-stream character-ssl-stream) sequence start end)
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
