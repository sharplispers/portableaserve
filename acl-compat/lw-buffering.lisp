;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LW Style Buffer Protocol for CMUCL           ;;;
;;; So far only 8bit byte and character IO works ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gray-stream)

(defvar *default-input-buffer-size* 8192)
(defvar *default-output-buffer-size* 8192)

(eval-when (compile load eval)
  (defstruct buffer-state 
    (input-buffer (make-array *default-input-buffer-size* :element-type '(unsigned-byte 8)))
    (input-index nil)
    (input-limit *default-input-buffer-size* :type fixnum)
    (output-buffer (make-array *default-output-buffer-size* :element-type '(unsigned-byte 8)))
    (output-index 0))
    (output-limit *default-output-buffer-size* :type fixnum)))

;; Can be used to implement resourcing of buffers later
(defun %allocate-buffer-state (&optional (input-limit *default-input-buffer-size*) (output-limit *default-output-buffer-size*))
  (make-buffer-state))

(defun %deallocate-buffer-state (state))
        
(defclass buffered-stream (fundamental-binary-input-stream fundamental-binary-output-stream)
  ((buffer-state :initform (%allocate-buffer-state))
   (lisp-stream :reader buffered-stream-lisp-stream
                :initarg :lisp-stream)))

(defgeneric STREAM-FLUSH-BUFFER (stream))
(defgeneric STREAM-FILL-BUFFER (stream))
(defgeneric STREAM-WRITE-BUFFER (stream buffer start end))

(defmacro with-stream-output-buffer ((buffer index limit) stream &body forms)
  (let ((state (gensym "BUFFER-STATE-")))
  `(let ((,state (slot-value ,stream 'buffer-state)))
     (symbol-macrolet ((,buffer ,(list 'buffer-state-output-buffer state))
                       (,index ,(list 'buffer-state-output-index state))
                       (,limit ,(list 'buffer-state-output-limit state)))
       ,@forms))))

(defmacro with-stream-input-buffer ((buffer index limit) stream &body forms)
  (let ((state (gensym "BUFFER-STATE-")))
  `(let ((,state (slot-value ,stream 'buffer-state)))
     (symbol-macrolet ((,buffer ,(list 'buffer-state-input-buffer state))
                       (,index ,(list 'buffer-state-input-index state))
                       (,limit ,(list 'buffer-state-input-limit state)))
       ,@forms))))

(defmethod stream-fill-buffer ((stream buffered-stream))
  (with-stream-input-buffer (buffer index limit) stream
    (let* ((read-bytes (system:read-n-bytes (buffered-stream-lisp-stream stream)
                                            buffer 0 limit nil)))
      (cond ((zerop read-bytes) nil)
            (t (setf limit read-bytes
                     index 0)
               t)))))

(defmethod stream-write-buffer ((stream buffered-stream) buffer start end)
  (let ((lisp-stream (buffered-stream-lisp-stream stream)))
    (write-sequence buffer lisp-stream :start start :end end)
    (force-output lisp-stream)))

(defmethod stream-flush-buffer ((stream buffered-stream))
  (with-stream-output-buffer (buffer index limit)
    (when (plusp index)
      (stream-write-buffer stream buffer index limit)
      (setf index 0))))

(defmethod stream-read-byte ((stream buffered-stream))
  (with-stream-input-buffer (buffer index limit) stream
     (unless (and index (<= index limit))
       (when (null (stream-fill-buffer stream))
	 (return-from stream-read-byte :eof)))
     (prog1 (aref buffer index)
       (incf index))))

(defmethod stream-read-char ((stream buffered-stream))
  (let ((byte (stream-read-byte stream)))
    (declare (type (unsigned-byte 8) byte))
    (if (eq byte :eof)
        :eof
      (code-char byte))))

(defmethod stream-write-byte ((stream buffered-stream) byte)
  (with-stream-output-buffer (buffer index limit) stream
    (unless (<= index limit)
      (stream-flush-buffer stream))
    (setf (aref buffer index) byte)
    (incf index)))

(defmethod stream-write-char ((stream buffered-stream) character)
  (stream-write-byte stream (char-code character)))

(defmethod stream-write-string ((stream buffered-stream) string &optional start end)
  (write-elements stream string start end #'stream-write-char))

(defmethod stream-write-sequence (sequence (stream buffered-stream) &optional start end)
  (write-elements stream sequence start end (%writer-function-for-sequence sequence)))

(defun read-elements (stream sequence start end reader-fn)
  (let* ((len (length sequence))
         (chars (- (min (or end len) len) start)))
    (loop for i upfrom start
          repeat chars
          for char = (funcall reader-fn socket-stream)
          if (eq char :eof) do (return-from read-elements i)
          do (setf (elt sequence i) char))
    (+ start chars)))

(defun write-elements (stream sequence start end writer-fn)
  (typecase sequence
    (array (loop for c across sequence do (funcall writer-fn stream c)))
    (otherwise (dolist (c sequence) (funcall writer-fn stream c)))))

(declaim (inline %reader-function-for-sequence))
(defun %reader-function-for-sequence (sequence)
  (typecase sequence
    (string #'read-char)
    ((array unsigned-byte (*)) #'read-byte)
    ((array signed-byte (*)) #'read-byte)
    (otherwise #'read-byte)))

(declaim (inline %writer-function-for-sequence))
(defun %writer-function-for-sequence (sequence)
  (typecase sequence
    (string #'write-char)
    ((array unsigned-byte (*)) #'write-byte)
    ((array signed-byte (*)) #'write-byte)
    (otherwise #'write-byte)))

(defmethod stream-read-sequence (sequence (stream buffered-stream) &optional start end)
  (read-elements stream sequence start end (%reader-function-for-sequence sequence)))

(defmethod stream-unread-char ((stream buffered-stream) character)
  (with-stream-input-buffer (buffer index limit) stream
      (let ((new-index (1- index)))
        (when (minusp new-index)
          (error "Cannot unread char ~A" character))
        (setf (aref buffer new-index) (char-code character)
              index new-index)))
  nil)

(defmethod stream-peek-char ((stream buffered-stream))
  (let ((char (stream-read-char stream)))
    (unless (eq char :eof)
      (stream-unread-char stream char))
    char))

(defmethod stream-read-line ((stream buffered-stream))
  (let ((res (make-string 80))
	(len 80)
	(index 0))
    (loop
     (let ((ch (stream-read-char stream)))
       (cond ((eq ch :eof)
	      (return (values (shrink-vector res index) t)))
	     (t
	      (when (char= ch #\newline)
		(return (values (shrink-vector res index) nil)))
	      (when (= index len)
		(setq len (* len 2))
		(let ((new (make-string len)))
		  (replace new res)
		  (setq res new)))
	      (setf (schar res index) ch)
	      (incf index)))))))

(defmethod stream-element-type ((stream buffered-stream))
  '(unsigned-byte 8))

(defmethod close ((stream buffered-stream) &key abort)
  (close (buffered-stream-lisp-stream stream) :abort abort))


