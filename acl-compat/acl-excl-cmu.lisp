;;;;
;;;; ACL-COMPAT - EXCL
;;;;
;;;; This is a modified version of Chris Doubles ACL excl wrapper library
;;;; As stated in the changelogs of his original this file includes the
;;;; IF* macro placed in the public domain by John Foderaro.
;;;; See: http://www.franz.com/~jkf/ifstar.txt
;;;;
;;;; Further modified by Rudi Schlatte for cmucl, taking Jochen
;;;; Schmidt's version for Xanalys Lispworks as base

;;;; This is the header of Chris Doubles original file. (but without Changelog)
;;;;
;;;; ACL excl wrapper library for Corman Lisp - Version 1.1
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;;
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source
;;;;    distribution.
;;;;
;;;; Notes
;;;; =====
;;;; A simple implementation of some of the EXCL package from Allegro
;;;; Common Lisp. Intended to be used for porting various ACL packages,
;;;; like AllegroServe.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author,
;;;; Christopher Double, at: chris@double.co.nz

(in-package :excl)

(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
	(state :init)
	(elseseen nil)
	(totalcol nil)
	(lookat nil nil)
	(col nil))
       ((null xx)
	(cond ((eq state :compl)
	       `(cond ,@totalcol))
	      (t (error "if*: illegal form ~s" args))))
       (cond ((and (symbolp (car xx))
		   (member (symbol-name (car xx))
			   if*-keyword-list
			   :test #'string-equal))
	      (setq lookat (symbol-name (car xx)))))

       (cond ((eq state :init)
	      (cond (lookat (cond ((string-equal lookat "thenret")
				   (setq col nil
					 state :then))
				  (t (error
				      "if*: bad keyword ~a" lookat))))
		    (t (setq state :col
			     col nil)
		       (push (car xx) col))))
	     ((eq state :col)
	      (cond (lookat
		     (cond ((string-equal lookat "else")
			    (cond (elseseen
				   (error
				    "if*: multiples elses")))
			    (setq elseseen t)
			    (setq state :init)
			    (push `(t ,@col) totalcol))
			   ((string-equal lookat "then")
			    (setq state :then))
			   (t (error "if*: bad keyword ~s"
					      lookat))))
		    (t (push (car xx) col))))
	     ((eq state :then)
	      (cond (lookat
		     (error
		      "if*: keyword ~s at the wrong place " (car xx)))
		    (t (setq state :compl)
		       (push `(,(car xx) ,@col) totalcol))))
	     ((eq state :compl)
	      (cond ((not (string-equal lookat "elseif"))
		     (error "if*: missing elseif clause ")))
	      (setq state :init)))))

(defvar *initial-terminal-io* *terminal-io*)
(defvar *cl-default-special-bindings* nil)

(defun filesys-size (stream)
	(file-length stream))

(defun filesys-write-date (stream)
	(file-write-date stream))

(defun stream-input-fn (stream)
  stream)

(defun match-regexp (pattern string &key (return :string))
  (let ((res (cond ((stringp pattern)
		    (regex pattern string))
		   ((functionp pattern) (funcall pattern string))
		   (t (error "Wrong type for pattern")))))
    (case return
      (:string
       (values-list (cons (not (null res))
                          res)))
      (:index (error "REGEXP: INDEX Not implemented"))
      (otherwise (not (null res))))))

(defun compile-regexp (regexp)
  (compile nil (regex-compile regexp)))

(defvar *current-case-mode* :case-insensitive-upper)

(defun intern* (s len package)
	(intern (string-upcase (subseq s 0 len)) package))

(defun filesys-type (file-or-directory-name)
       (if (eq :directory (unix:unix-file-kind
                           (namestring file-or-directory-name)))
           :directory
         (if (probe-file file-or-directory-name)
             :file
           nil)))

(defmacro errorset (form &optional (announce nil) (catch-breaks nil))
  "This macro is incomplete.  It was hacked to get AllegroServe
running, but the announce and catch-breaks arguments are ignored.  See
documentation at
http://franz.com/support/documentation/6.1/doc/pages/operators/excl/errorset.htm
An implementation of the catch-breaks argument will necessarily be
implementation-dependent, since Ansi does not allow any
program-controlled interception of a break."
  (declare (ignore announce catch-breaks))
  `(let* ((ok nil)
          (results (ignore-errors
                     (prog1 (multiple-value-list ,form)
                       (setq ok t)))))
     (if ok
         (apply #'values t results)
         nil)))

(defmacro atomically (&body forms)
  `(mp:without-scheduling ,@forms))

(defun unix-signal (signal pid)
  ;; fixxme: did I get the arglist right?  only invocation I have seen
  ;; is (excl::unix-signal 15 0) in net.aserve:start
  (unix:unix-kill pid signal))

(defmacro fast (&body forms)
  `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
	    ,@forms))

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
    (declare (type (simple-array (unsigned-byte 8) (*)) mb-vector))
    (loop for from-index from start below end
          for to-index upfrom 0
          do (progn
               (setf (aref mb-vector to-index)
                     (char-code (aref string from-index)))
               (incf octets-copied)))
    (values mb-vector octets-copied)))


(defun write-vector (sequence stream &key (start 0) end endian-swap)
  (declare (ignore endian-swap))
  (check-type sequence (or string (array (unsigned-byte 8) 1)
                           (array (signed-byte 8) 1)))
  (write-sequence sequence stream :start start :end end))


;;; Bivalent Gray streams

#||
(defmethod stream-read-sequence ((stream bivalent-input-stream)
                                 (seq vector) &optional start end)
  (unless start (setf start 0))
  (unless end (setf end (length seq)))
  (assert (<= end (length seq)))
  (if (subtypep (array-element-type seq) 'character)
      (loop for count upfrom start
            for i from start below end
            do (setf (aref seq i) (code-char (read-byte stream)))
            finally (return count))
      (read-sequence seq (lisp-stream stream)
                     :start start :end end)))

(defmethod stream-read-sequence ((stream bivalent-input-stream)
                                 (seq cons) &optional (start 0) end)
  (unless start (setf start 0))
  (unless end (setf end (length seq)))
  (let ((seq (nthcdr start seq)))
    (loop for count upfrom start
          for head on seq
          for i below (- end start)
          while head
          do (setf (car head) (read-byte stream))
          finally (return count))))

(defmethod stream-read-sequence ((stream bivalent-input-stream)
                                 (seq null) &optional (start 0) end)
  (declare (ignore end))
  start)





(defmethod stream-write-sequence ((stream bivalent-output-stream)
                                  (seq vector) &optional (start 0) end)
  (let ((length (length seq)))
    (unless end (setf end length))
    (assert (<= end length)))
  (unless start (setf start 0))
  (when (< end start)
    (cerror "Continue with switched start and end ~s <-> ~s"
            "Stream-write-sequence: start (~S) and end (~S) exchanged."
            start end seq)
    (rotatef start end))
  (cond
    ((subtypep (array-element-type seq) '(unsigned-byte 8))
     (write-sequence seq (lisp-stream stream) :start start :end end))
    ((subtypep (array-element-type seq) 'character)
     (loop for i from start below end
            do (stream-write-char stream (aref seq i))))
    ((subtypep (array-element-type seq) 'integer)
     (loop for i from start below end
           do (stream-write-byte stream (aref seq i)))))
  seq)

(defmethod stream-write-sequence ((stream bivalent-output-stream)
                                  (seq cons) &optional (start 0) end)
  (let ((length (length seq)))
    (unless end (setf end length))
    (assert (<= end length)))
  (unless start (setf start 0))
  (when (< end start)
    (cerror "Continue with switched start and end ~s <-> ~s"
            "Stream-write-sequence: start (~S) and end (~S) exchanged."
            start end seq)
    (rotatef start end))
  (let ((seq (nthcdr start seq)))
    (loop for element in seq
          for i below (- end start)
          while seq
          do (etypecase element
               (character (stream-write-char stream element))
               (integer (stream-write-byte stream element)))))
  seq)

(defmethod stream-write-sequence ((stream bivalent-output-stream)
                                  (seq null) &optional (start 0) end)
  (declare (ignore start end))
  seq)

;;; End bivalent Gray streams
||#

(provide 'acl-excl)
