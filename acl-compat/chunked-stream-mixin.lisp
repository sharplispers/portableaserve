;;;;										;
;;;; (c) 2002 by Jochen Schmidt.
;;;;
;;;; File:            chunked-stream-mixin.lisp
;;;; Revision:        0.1
;;;; Description:     ACL style HTTP1.1 I/O chunking
;;;; Date:            08.04.2002
;;;; Authors:         Jochen Schmidt
;;;; Tel:             (+49 9 11) 47 20 603
;;;; Email:           jsc@dataheaven.de
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED "AS IS" AND THERE ARE NEITHER 
;;;; EXPRESSED NOR IMPLIED WARRANTIES -  THIS INCLUDES, BUT 
;;;; IS NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;;;; AND FITNESS FOR A PARTICULAR PURPOSE.IN NO WAY ARE THE
;;;; AUTHORS LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ;
;;;; LOSS OF USE, DATA, OR PROFITS ; OR BUSINESS INTERRUPTION)
;;;; 
;;;; For further details contact the authors of this software.
;;;;
;;;;  Jochen Schmidt        
;;;;  Zuckmantelstr. 11     
;;;;  91616 Neusitz         
;;;;  GERMANY               
;;;;
;;;; N�rnberg, 08.Apr.2002 Jochen Schmidt
;;;;

(in-package :cl-user)

(defpackage :de.dataheaven.chunked-stream-mixin
  (:use :common-lisp)
  (:export #:chunked-stream-mixin
           #:output-chunking-p #:input-chunking-p))

(in-package :de.dataheaven.chunked-stream-mixin)

(defclass chunked-stream-mixin ()
  ((output-chunking-p :initform nil :accessor output-chunking-p)
   (chunk-input-avail :initform nil)
   (real-input-limit :initform 0)))

(defmethod input-chunking-p ((stream chunked-stream-mixin))
  (not (null (slot-value stream 'chunk-input-avail))))

(defmethod (setf input-chunking-p) (new-value (stream chunked-stream-mixin))
  (setf (slot-value stream 'chunk-input-avail) (and new-value 0)))

(define-condition excl::socket-chunking-end-of-file (condition)
  ((excl::format-arguments :initform nil)
   (excl::format-control :initform "A chunking end of file occured")))


;;;;;;;;;;;;;;;;;;;;;;
;;; Input chunking ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Input chunking is not tested so far!

(defmethod initialize-input-chunking ((stream chunked-stream-mixin))
  "This method initializes input chunking. The real-input-limit is nil in the beginnings
   because it got not saved yet. Chunk-input-avail is obviously 0 because no chunk-data
   got read so far."
  (with-slots (real-input-limit chunk-input-avail) stream
    (setf real-input-limit nil
          chunk-input-avail 0)))

(defmethod gray-stream:stream-fill-buffer ((stream chunked-stream-mixin))
  "STREAM-FILL-BUFFER gets called when the input-buffer contains no more data (the index is
   bigger than the limit). We call out to the real buffer filling mechanism by calling the next
   specialized method. This method is responsible to update the buffer state in coordination
   with the chunk-header."
  (with-slots (chunk-input-avail real-input-limit) stream
    (gray-stream:with-stream-input-buffer (input-buffer input-index input-limit) stream
      (labels ((read-chunk-header ()
                 (let ((n 0))
                   (flet ((pop-char () (unless (< input-index input-limit) (call-next-method))
                            (gray-stream::buffer-ref input-buffer input-index)
                            (incf input-index)))
                     (when real-input-limit (setf input-limit real-input-limit))
                     (tagbody
                      initial-crlf (let ((char (pop-char)))
                                     (cond ((digit-char-p char 16) (decf input-index) (go chunk-size))
                                           ((eq #\Return char) 
                                            (if (eq (pop-char) #\Linefeed)
                                                (go chunk-size)
                                              (error "End of chunk-header corrupted: Expected Linefeed")))
                                           (t (error "End of chunk-header corrupted: Expected Carriage Return or a digit"))))
                      
                      chunk-size (let ((char (pop-char)))
                                   (cond ((digit-char-p char 16) (setf n (+ (* 16 n) (digit-char-p char 16)))
                                          (go chunk-size))
                                         (t (go skip-rest))))
                      
                      skip-rest (if (eq #\Return (pop-char))
                                    (go check-linefeed)
                                  (go skip-rest))
                      
                      check-linefeed (let ((char (pop-char)))
                                       (case char
                                         (#\Linefeed (go accept))
                                         (t (error "Chunkheader-end is corrupt: LF expected, ~A read." char))))
                      
                      accept)
                     
                     (if (zerop n)
                         (signal 'excl::socket-chunking-end-of-file :format-arguments stream)
                       (setf chunk-input-avail n)))))
               ;; Setup limits for chunking
               (update-limits ()
                 (let ((end-of-chunk (+ input-index chunk-input-avail)))
                   (setf chunk-input-avail
                         (cond ((< end-of-chunk input-limit)
                                (shiftf real-input-limit input-limit end-of-chunk) 0)
                               (t (setf real-input-limit nil)
                                  (- chunk-input-avail (- input-limit input-index))))))))

        (cond ((not (input-chunking-p stream)) (call-next-method))
              ((zerop chunk-input-avail) (when (read-chunk-header) (update-limits)))
              (t (when (call-next-method) (update-limits))))))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Output chunking ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; This constant is the amount of bytes the system reserves for the chunk-header
;; It is calculated as 4 bytes for the chunk-size in hexadecimal digits and a CR followed
;; by a LF
(defconstant +chunk-header-buffer-offset+ 6)

(defmethod initialize-output-chunking ((stream chunked-stream-mixin))
  "This method initializes output chunking. Actual contents in the output-buffer
   get flushed first. A chunk has a header at the start and a CRLF at the end.
   The header is the length of the (data) content in the chunk as a string in hexadecimal
   digits and a trailing CRLF before the real content begins. We assume that the content
   of a chunk is never bigger than #xFFFF and therefore reserve 6 bytes at the beginning
   of the buffer for the header. We reduce the buffer limit by 2 so that we have always
   room left in the buffer to attach a CRLF."
  (unless (output-chunking-p stream)
    (force-output stream)
    (gray-stream:with-stream-output-buffer (buffer index limit) stream
      (setf index +chunk-header-buffer-offset+)
      (setf (gray-stream::buffer-ref buffer (- +chunk-header-buffer-offset+ 2)) #\Return
            (gray-stream::buffer-ref buffer (1- +chunk-header-buffer-offset+)) #\Linefeed)
      (decf limit 2)
      (setf (output-chunking-p stream) t))))

(defmethod gray-stream:stream-flush-buffer ((stream chunked-stream-mixin))
  "When there is pending content in the output-buffer then compute the chunk-header and flush
   the buffer"
  (if (output-chunking-p stream)
      (gray-stream:with-stream-output-buffer (output-buffer output-index output-limit) stream
        (when (> output-index +chunk-header-buffer-offset+)
          (let* ((chunk-header (format nil "~X" (- output-index +chunk-header-buffer-offset+)))
                 (start (- +chunk-header-buffer-offset+ 2 (length chunk-header))))
            (loop for c across chunk-header
                  for i upfrom start
                  do (setf (gray-stream::buffer-ref output-buffer i) c))
            (setf (gray-stream::buffer-ref output-buffer output-index) #\Return
                  (gray-stream::buffer-ref output-buffer (1+ output-index)) #\Linefeed)
            (gray-stream:stream-write-buffer stream output-buffer start (+ output-index 2))
            (setf output-index +chunk-header-buffer-offset+))))
    (call-next-method)))


(defmethod close ((stream chunked-stream-mixin) &key abort)
  (unless abort
    (gray-stream:with-stream-output-buffer (output-buffer output-index output-limit) stream
                  (disable-output-chunking stream)))
    (call-next-method))


(defmethod disable-output-chunking ((stream chunked-stream-mixin))
  "When we disable chunking we first try to write out a last pending chunk and after that
   reset the buffer-state to normal mode. To end the game we write out a chunk-header with
   a chunk-size of zero to notify the peer that chunking ends."
  (when (output-chunking-p stream)
    (force-output stream)
    (gray-stream:with-stream-output-buffer (buffer index limit) stream
      (setf index 0)
      (incf limit 2))
    (setf (output-chunking-p stream) nil
          (input-chunking-p stream) nil)
    (format stream "0~A~A~A~A" #\Return #\Linefeed #\Return #\Linefeed)
    (force-output stream)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :de.dataheaven.chunked *features*))

(provide :de.dataheaven.chunked)

