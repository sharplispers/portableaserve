
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :gray-stream)
    (defpackage :gray-stream
      (:import-from #+lispworks :stream #+cmu :lisp #+clisp :gray
       #:fundamental-binary-input-stream
       #:fundamental-binary-output-stream
       #:fundamental-character-input-stream
       #:fundamental-character-output-stream
       #-clisp #:stream-element-type
       #:stream-listen
       #:stream-read-byte
       #:stream-read-char
       #:stream-write-byte
       #:stream-write-char
       #:stream-read-char-no-hang
       #:stream-force-output
       #:stream-finish-output
       #:stream-clear-input
       #:stream-clear-output
       #:stream-line-column
       #-clisp #:stream-read-sequence
       #:stream-unread-char
       #:stream-read-line
       #-clisp #:stream-write-sequence
       #:stream-write-string
       #+lispworks #:stream-write-buffer
       #+lispworks #:stream-read-buffer
       #+lispworks #:stream-fill-buffer
       #+lispworks #:stream-flush-buffer
       #+lispworks #:with-stream-input-buffer
       #+lispworks #:with-stream-output-buffer)
      #+clisp(:import-from :common-lisp
                           #:stream-element-type)
      (:export
       #:fundamental-binary-input-stream
       #:fundamental-binary-output-stream
       #:fundamental-character-input-stream
       #:fundamental-character-output-stream
       #:stream-element-type
       #:stream-listen
       #:stream-read-byte
       #:stream-read-char
       #:stream-write-byte
       #:stream-write-char
       #:stream-read-char-no-hang
       #:stream-force-output
       #:stream-finish-output
       #:stream-clear-input
       #:stream-clear-output
       #:stream-line-column
       #:stream-read-sequence
       #:stream-unread-char
       #:stream-read-line
       #:stream-write-sequence
       #:stream-write-string
       #+(or lispworks cmu)
       #:stream-write-buffer
       #+(or lispworks cmu)
       #:stream-read-buffer
       #+(or lispworks cmu)
       #:stream-fill-buffer
       #+(or lispworks cmu)
       #:stream-flush-buffer
       #+(or lispworks cmu)
       #:with-stream-input-buffer
       #+(or lispworks cmu)
       #:with-stream-output-buffer))))

(in-package :gray-stream)
(defun buffer-ref (buffer index)
  #+lispworks (schar buffer index)
  #+cmu (aref buffer index))

(defun (setf buffer-ref) (new-value buffer index)
  #+cmu(setf (aref buffer index) (char-code new-value))
  #+lispworks (setf (schar buffer index) new-value))
