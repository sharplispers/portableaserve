;;;;
;;;; ACL-COMPAT - EXCL
;;;;

;;;; Implementation-specific parts of acl-compat.excl (see
;;;; acl-excl-common.lisp)

(in-package :acl-compat.excl)

#-openmcl
(defun fixnump (x)
  (ccl::fixnump x))

#+openmcl
(defun filesys-inode (path)
  (let ((checked-path (probe-file path)))
    (cond (checked-path
	   (ccl:rlet ((lstat :stat))
		     (ccl:with-cstrs ((str (namestring checked-path)))
				     (#_stat str lstat))
		     (ccl:pref lstat :stat.st_ino)))
	  (t (error "path ~s does not exist" path)))))

(defun cl-internal-real-time ()
  (round (/ (get-internal-real-time) 1000)))

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
	(if (ccl:directory-pathname-p file-or-directory-name)
		:directory
		(if (probe-file file-or-directory-name)
			:file
			nil)))

(defmacro atomically (&body forms)
  `(ccl:without-interrupts ,@forms))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))

(define-condition stream-error (error)
  ((stream :initarg :stream
           :reader stream-error-stream)
   (action :initarg :action
           :initform nil
           :reader stream-error-action)
   (code :initarg :code
         :initform nil
         :reader stream-error-code)
   (identifier :initarg :identifier
               :initform nil
               :reader stream-error-identifier))
  (:report (lambda (condition stream)
             (format stream "A stream error occured (action=~A identifier=~A code=~A stream=~S)."
                     (stream-error-action condition)
                     (stream-error-identifier condition)
                     (stream-error-code condition)
                     (stream-error-stream condition)))))

(define-condition socket-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A socket error occured (action=~A identifier=~A code=~A stream=~S)."
                     (stream-error-action condition)
                     (stream-error-identifier condition)
                     (stream-error-code condition)
                     (stream-error-stream condition)))))



;! Need to figure out what to do here
(defun fasl-read (filename)
  (declare (ignore filename))
  (error "fasl-read not implemented for MCL.") )

(defun fasl-write (data stream opt)
  (declare (ignore data stream opt))
  (error "fasl-write not implemented for MCL.") )


(defmacro schedule-finalization (object function)
  `(ccl:terminate-when-unreachable ,object ,function))

(defun run-shell-command (program
                          &key input output error-output separate-streams
                          if-input-does-not-exist if-output-exists
                          if-error-output-exists wait environment show-window)
  (declare (ignore show-window))
  ;; KLUDGE: split borrowed from asdf, this shouldn't be done -- it
  ;; would be better to use split-sequence or define one ourselves ...
  ;; TODO: On Unix, acl also handles a vector of simple-strings as
  ;; value for program, with different semantics.
  (let* ((program-and-arguments
          (delete "" (asdf::split program) :test #'string=))
         (program (car program-and-arguments))
         (arguments (cdr program-and-arguments)))
   (when environment
     #-unix (error "Don't know how to run program in an environment.")
     (setf arguments (append
                      (list "-i")
                      (loop for (name . value) in environment
                         collecting (concatenate 'string name "=" value))
                      (list program)
                      arguments))
     (setf program "env"))
       
   (let* ((process (run-program program arguments
                                :input input
                                :if-input-does-not-exist
                                if-input-does-not-exist
                                :output output
                                :if-output-exists if-output-exists
                                :error error-output
                                :if-error-exists if-error-output-exists
                                :wait wait))
          (in-stream (external-process-input-stream process))
          (out-stream (external-process-output-stream process))
          (err-stream (external-process-error-stream process))
          (pid (external-process-id process)))
     (cond
       ;; one value: exit status
       (wait (nth-value 1 (external-process-status process)))
       ;; four values: i/o/e stream, pid
       (separate-streams
        (values (if (eql input :stream) in-stream nil)
                (if (eql output :stream) out-stream nil)
                (if (eql error-output :stream) err-stream nil)
                pid))
       ;; three values: normal stream, error stream, pid
       (t (let ((normal-stream
                 (cond ((and (eql input :stream) (eql output :stream))
                        (make-two-way-stream in-stream out-stream))
                       ((eql input :stream) in-stream)
                       ((eql output :stream) out-stream)
                       (t nil)))
                (error-stream (if (eql error-output :stream) err-stream nil)))
            (values normal-stream error-stream pid)))))))
