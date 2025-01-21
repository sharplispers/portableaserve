;;;----------------------------------------------------------------------
;;; SOCKET-STREAMS for SBCL
;;; These associate together a socket and a stream derived from that
;;; socket.
;;; These are used here because SBCL's SB-BSD-SOCKETS:SOCKET-MAKE-STREAM
;;; produces a stream that doesn't conveniently make socket information
;;; available.
;;;----------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Original code from McCLIM: encapsulating-streams.lisp
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
(in-package #:socket-streams)

(defclass socket-stream (sb-gray:fundamental-character-stream)
  ((stream :reader socket-stream-stream :initarg :stream)
   (socket :reader socket-stream-socket :initarg :socket)))

(defclass socket-input-stream (socket-stream sb-gray:fundamental-character-input-stream)
  ())

(defclass socket-output-stream (socket-stream
                                sb-gray:fundamental-character-output-stream)
  ())

(defclass socket-text-io-stream (socket-stream
                            sb-gray:fundamental-character-input-stream
                            sb-gray:fundamental-character-output-stream)
  ())

(defclass socket-binary-io-stream (socket-stream
                            sb-gray:fundamental-binary-input-stream
                            sb-gray:fundamental-binary-output-stream)
  ())


(defclass socket-bivalent-io-stream (socket-text-io-stream
                                     socket-binary-io-stream)
  ())



;;; Macro for defining methods that delegate to the encapsulated
;;; stream.  Call the delegated method directly if possible, otherwise
;;; collect up optional and rest args and apply.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-gf-lambda-list (ll)
  "Returns the required args, optional args, and presence of rest or key args"
  (let ((state 'required)
        (required-args nil)
        (optional-args nil)
        (rest-or-key nil))
    (loop for arg in ll
          do (cond ((member arg lambda-list-keywords)
                    (when (or (eq arg '&rest)
                              (eq arg '&key)
                              (eq arg '&allow-other-keys))
                      (setq rest-or-key t)
                      (loop-finish))
                    (setq state arg))
                   ((eq state 'required)
                    (push arg required-args))
                   ((eq state '&optional)
                    (push arg optional-args))
                   (t (error "How did I get in this lambda list state?~%~
                              state ~S lambda list ~S"
                             state ll))))
    (values (nreverse required-args) (nreverse optional-args) rest-or-key))))

(defmacro def-stream-method (name lambda-list)
  "stream is a required argument"
  (multiple-value-bind (required optional rest)
      (parse-gf-lambda-list lambda-list)
    (let* ((rest-arg (gensym "REST-ARG"))
           (supplied-vars (mapcar #'(lambda (var)
                                      (gensym (format nil "~A-SUPPLIED-P"
                                                      (symbol-name var))))
                                  optional))
           (ll `(,@required
                 ,@(and optional
                        `(&optional
                          ,@(mapcar #'(lambda (var supplied)
                                        `(,var nil ,supplied))
                                    optional
                                    supplied-vars)))

                 ,@(and rest
                        `(&rest ,rest-arg))))
           (required-params (mapcar #'(lambda (var)
                                        (if (consp var)
                                            (car var)
                                            var))
                                    required))
           (apply-list (gensym "APPLY-LIST"))
           (body (if (and (not optional) (not rest))
                     (if (symbolp name)
                         `(,name ,@required-params)
                         `(funcall #',name ,@required-params))
                     `(let ((,apply-list ,(and rest rest-arg)))
                        ,@(mapcar #'(lambda (var supplied)
                                      `(when ,supplied
                                         (push ,var ,apply-list)))
                                  (reverse optional)
                                  (reverse supplied-vars))
                        (apply #',name ,@required-params ,apply-list)))))
      `(defmethod ,name ,ll
         (let ((stream (slot-value stream 'stream)))
           ,body)))))

(defmacro def-socket-method (name lambda-list)
  "stream is a required argument; socket will be bound and
invocation delegated to the socket."
  (multiple-value-bind (required optional rest)
      (parse-gf-lambda-list lambda-list)
    (let* ((rest-arg (gensym "REST-ARG"))
           (supplied-vars (mapcar #'(lambda (var)
                                      (gensym (format nil "~A-SUPPLIED-P"
                                                      (symbol-name var))))
                                  optional))
           (ll `(,@required
                 ,@(and optional
                        `(&optional
                          ,@(mapcar #'(lambda (var supplied)
                                        `(,var nil ,supplied))
                                    optional
                                    supplied-vars)))

                 ,@(and rest
                        `(&rest ,rest-arg))))
           (required-params (mapcar #'(lambda (var)
                                        (if (consp var)
                                            (car var)
                                            var))
                                    required))
           (apply-list (gensym "APPLY-LIST"))
           (body (if (and (not optional) (not rest))
                     (if (symbolp name)
                         `(,name ,@required-params)
                         `(funcall #',name ,@required-params))
                     `(let ((,apply-list ,(and rest rest-arg)))
                        ,@(mapcar #'(lambda (var supplied)
                                      `(when ,supplied
                                         (push ,var ,apply-list)))
                                  (reverse optional)
                                  (reverse supplied-vars))
                        (apply #',name ,@required-params ,apply-list)))))
      `(defmethod ,name ,ll
         (let ((socket (slot-value stream 'socket)))
           ,(subst 'socket 'stream body))))))

;;; The basic input and output stream protocols, as specified by the Gray
;;; stream proposal in Chapter Common Lisp Streams .

;;; Not yet, apparently
#+nil
(def-stream-method streamp ((stream standard-encapsulating-stream)))

(def-stream-method input-stream-p
    ((stream socket-stream)))

(def-stream-method output-stream-p
    ((stream socket-stream)))

(def-stream-method stream-element-type
    ((stream socket-stream)))

(def-stream-method open-stream-p ((stream socket-stream)))

(def-stream-method close ((stream socket-stream)
                          &key abort))

(def-stream-method stream-pathname ((stream socket-stream)))

(def-stream-method stream-truename ((stream socket-stream)))

(def-stream-method stream-read-char ((stream socket-stream)))

(def-stream-method stream-unread-char ((stream socket-stream)
                                       character))

(def-stream-method stream-read-char-no-hang
    ((stream socket-stream)))

(def-stream-method stream-peek-char ((stream socket-stream)))

(def-stream-method stream-listen ((stream socket-stream)))

(def-stream-method stream-read-line ((stream socket-stream)))

(def-stream-method stream-clear-input ((stream socket-stream)))

(def-stream-method stream-write-char
    ((stream socket-stream)
     character))

(def-stream-method stream-line-column ((stream socket-stream)))

(def-stream-method stream-start-line-p
    ((stream socket-stream)))

(def-stream-method stream-write-string ((stream socket-stream) string &optional start end))

(defmethod stream-write-string :before  ((stream socket-stream) string &optional start end)
  (declare (ignore start end))
  (format t "~&STREAM-WRITE-STRING to SOCKET-STREAM.~%~tSTREAM: ~s~%~tINTERNAL STREAM: ~S~%~tSTRING: ~S~%" stream (socket-stream-stream stream) string))

(def-stream-method stream-write-sequence ((stream socket-stream) sequence &optional start end))

(defmethod stream-write-sequence :before  ((stream socket-stream) string &optional start end)
  (declare (ignore start end string))
  (format t "~&STREAM-WRITE-SEQENCE to SOCKET-STREAM.~%~tSTREAM: ~s~%~tINTERNAL STREAM: ~S~%" stream (socket-stream-stream stream)))


(def-stream-method stream-terpri ((stream socket-stream)))

(def-stream-method stream-fresh-line ((stream socket-stream)))

(def-stream-method stream-finish-output
    ((stream socket-stream)))

(def-stream-method stream-force-output
    ((stream socket-stream)))

(def-stream-method stream-clear-output
    ((stream socket-stream)))

(def-stream-method stream-advance-to-column
    ((stream socket-stream) column))

(def-stream-method stream-read-byte ((stream socket-stream)))

(def-stream-method stream-write-byte ((stream socket-stream) integer))

;;; STREAM-LINE-LENGTH is a CMUCL extension to Gray Streams which the
;;; pretty printer seems to use. There's a default method which works
;;; for most CLIM streams. For several dumb reasons it doesn't work on
;;; encapsulating streams.
#+CMU
(defmethod ext:stream-line-length ((stream socket-stream))
  nil)
#+SBCL
(defmethod sb-gray:stream-line-length ((stream socket-stream))
  nil)

;;; Socket methods
(defgeneric remote-host (socket-or-stream))
(defgeneric remote-port (socket-or-stream))
(defgeneric local-host (socket-or-stream))
(defgeneric local-port (socket-or-stream))

(defmethod remote-host ((socket sb-bsd-sockets:inet-socket))
  (let ((addr (socket-peername socket)))
    (format t "~&Trying to decode socket peername ~s~%" addr)
     (vector-to-ipaddr addr)))

(defmethod remote-port ((socket sb-bsd-sockets:inet-socket))
  (let ((raw-port
          (nth-value 1 (socket-peername socket))))
    (etypecase raw-port
      (fixnum raw-port)
      ;; should error if we can't read an integer
      (string (coerce (parse-integer raw-port :junk-allowed nil) 'fixnum)))))

(defmethod local-port ((socket sb-bsd-sockets:inet-socket))
  (let ((raw-port
          (nth-value 1 (sb-bsd-sockets:socket-name socket))))
    (etypecase raw-port
      (fixnum raw-port)
      ;; should error if we can't read an integer
      (string (coerce (parse-integer raw-port :junk-allowed nil) 'fixnum)))))

(def-socket-method remote-host ((stream socket-stream)))
(def-socket-method remote-port ((stream socket-stream)))
(def-socket-method local-host ((stream socket-stream)))
(def-socket-method local-port ((stream socket-stream)))


;;; WRAP FD STREAMS
(defmethod stream-read-char ((stream sb-sys:fd-stream))
  (read-char stream nil :eof))

(defmethod stream-unread-char ((stream sb-sys:fd-stream) character)
  (unread-char character stream)
  nil)

(defmethod stream-read-char-no-hang ((stream sb-sys:fd-stream))
  (read-char-no-hang stream))

(defmethod stream-peek-char ((stream sb-sys:fd-stream))
  (peek-char nil stream nil :eof))

(defmethod stream-listen ((stream sb-sys:fd-stream))
  (listen stream))

(defmethod stream-read-line ((stream sb-sys:fd-stream))
  (read-line stream nil t))

(defmethod stream-clear-input ((stream sb-sys:fd-stream))
  (clear-input stream))

(defmethod stream-write-char ((stream sb-sys:fd-stream) character)
  (write-char character stream))

(defmethod stream-line-column ((stream sb-sys:fd-stream))
  (declare (ignorable stream))
  nil)

(defmethod stream-start-line-p ((stream sb-sys:fd-stream))
  (declare (ignorable stream))
  nil)

(defmethod stream-write-string ((stream sb-sys:fd-stream) string &optional start end)
  (write-string string stream :start start :end end))

(defmethod stream-write-string :before ((stream sb-sys:fd-stream) string &optional start end)
  (declare (ignore start end))
  (format t "~&STREAM-WRITE-STRING to SB-SYS:FD-STREAM.~%~tSTREAM: ~s~%~tSTRING: ~S~%" stream string))

(defmethod stream-write-sequence ((stream sb-sys:fd-stream) string &optional start end)
  (write-sequence string stream :start start :end end))

(defmethod stream-terpri ((stream sb-sys:fd-stream))
  (terpri stream))

(defmethod stream-fresh-line ((stream sb-sys:fd-stream))
  (fresh-line stream))

(defmethod stream-finish-output  ((stream sb-sys:fd-stream))
  (finish-output stream))

(defmethod stream-force-output ((stream sb-sys:fd-stream))
  (force-output stream))

(defmethod stream-clear-output ((stream sb-sys:fd-stream))
  (clear-output stream))

(defmethod stream-advance-to-column ((stream sb-sys:fd-stream) column)
  (declare (ignorable stream) (ignore column))
  nil)

(defmethod stream-read-byte ((stream sb-sys:fd-stream))
  (read-byte stream nil :eof))

(defmethod stream-write-byte ((stream sb-sys:fd-stream) integer)
  (write-byte integer stream))
