;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
  (if (eq :directory (sb-unix:unix-file-kind
                      (namestring file-or-directory-name)))
      :directory
      (if (probe-file file-or-directory-name)
          :file
          nil)))

(defmacro atomically (&body forms)
  `(acl-mp:without-scheduling ,@forms))

(defun unix-signal (signal pid)
  (declare (ignore signal pid))
  (error "unix-signal not implemented in acl-excl-sbcl.lisp"))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))


(defun string-to-octets (string &key (null-terminate t) (start 0)
                         end mb-vector make-mb-vector?
                         (external-format :default))
  "This function returns a lisp-usb8-vector and the number of bytes copied."
  (declare (ignore external-format))
  ;; The end parameter is different in ACL's lambda list, but this
  ;; variant lets us give an argument :end nil explicitly, and the
  ;; right thing will happen
  (unless end (setf end (length string)))
  (let* ((octets-copied (if null-terminate 1 0))
         (needed-length (if null-terminate (1+ (- end start))
                            (- end start)))
         (mb-vector (cond
                      ((and mb-vector (>= (length mb-vector) needed-length))
                       mb-vector)
                      ((or (not mb-vector) make-mb-vector?)
                       (make-array (list needed-length)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0))
                      (t (error "Was given a vector of length ~A, ~
                                 but needed at least length ~A."
                                (length mb-vector) needed-length)))))
    (declare (type (simple-array (unsigned-byte 8) (*)) mb-vector)
             (type string string))
    (loop for from-index from start below end
          for to-index upfrom 0
          do (progn
               (setf (aref mb-vector to-index)
                     (char-code (aref string from-index)))
               (incf octets-copied)))
    (values mb-vector octets-copied)))


(defun write-vector (sequence stream &key start end endian-swap)
  (declare (ignore endian-swap))
  (check-type sequence (or string (array (unsigned-byte 8) 1)
                           (array (signed-byte 8) 1)))
  (write-sequence sequence stream :start start :end end))


(provide 'acl-excl)
