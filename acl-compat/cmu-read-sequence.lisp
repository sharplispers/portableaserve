;;; cmu-read-sequence.lisp

;;; patch cmucl's read-sequence, write-sequence to know about
;;; gray-streams.
;;;
;;; Written by Rudi Schlatte
;;;
;;; This file was written for the Portable AllegroServe project and is
;;; intended to be distributed under the same license as the rest of
;;; AllegroServe.



(in-package "LISP")

(defvar *old-lisp-stream-read-sequence* #'read-sequence)

(fmakunbound 'read-sequence)

(defgeneric read-sequence (seq stream &key start end))

(defmethod read-sequence (seq (stream lisp-stream) &key (start 0) (end nil))
  (funcall *old-lisp-stream-read-sequence* seq stream :start start :end end))

(defmethod read-sequence (seq (stream fundamental-input-stream)
                          &key (start 0) (end nil))
  (stream-read-sequence stream seq start end))


(defvar *old-lisp-stream-write-sequence* #'write-sequence)

(fmakunbound 'write-sequence)

(defgeneric write-sequence (seq stream &key start end))

(defmethod write-sequence (seq (stream lisp-stream) &key (start 0) (end nil))
  (funcall *old-lisp-stream-write-sequence* seq stream :start start :end end))

(defmethod write-sequence (seq (stream fundamental-output-stream)
                           &key (start 0) (end nil))
  (stream-write-sequence stream seq start end))