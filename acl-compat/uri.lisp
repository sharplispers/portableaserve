;;;;										;
;;;; (c) 2001 by Jochen Schmidt.
;;;;
;;;; File:            uri.lisp
;;;; Revision:        1.2.0
;;;; Description:     ACL NET.URI compatible URI implementation
;;;; Date:            01.07.2001
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
;;;; LOSS OF USE, DATA, OR PROFITS			; OR BUSINESS INTERRUPTION)
;;;; 
;;;; For further details contact the authors of this software.
;;;;
;;;;  Jochen Schmidt        
;;;;  Zuckmantelstr. 11     
;;;;  91616 Neusitz         
;;;;  GERMANY               
;;;;
;;;; Nürnberg, 01.Jul.2001 Jochen Schmidt
;;;;



(defpackage :net.uri
	(:use :common-lisp)
	(:export 
		#:uri
		#:uri-scheme
		#:uri-host
		#:uri-port
		#:uri-path
		#:uri-query
		#:uri-fragment
		#:uri-plist
		#:uri-authority
		#:render-uri
		#:parse-uri
		#:merge-uris
		#:enough-uri
		#:uri-parsed-path
		#:copy-uri
		))

(in-package :net.uri)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass uri ()
  ((scheme :accessor uri-scheme
           :initarg :scheme
           :initform nil)
   (host :accessor uri-host
         :initarg :host 
         :initform nil)
   (port :accessor uri-port
         :initarg :port
         :initform nil)
   (path :accessor uri-path
         :initarg :path
         :initform nil)
   (query :accessor uri-query
          :initarg :query
          :initform nil)
   (fragment :accessor uri-fragment
             :initarg :fragment
             :initform nil)
   (plist :accessor uri-plist
          :initarg :plist
          :initform nil)
   (string :accessor uri-string
	   :initarg :string
	   :initform nil)))
)

(defmethod uri ((object uri))
  object)

(defmethod uri ((object string))
  (parse-uri object))

(defmethod uri ((object t))
  (error "Object is not a URI."))

(defun uri-p (object)
  "Returns true if object is an instance of net.uri:uri
   or any of it's subclasses"
  (or (eq (type-of object)
          'net.uri:uri)
      (subtypep (type-of object)
                'net.uri:uri)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(meta:enable-meta-syntax)

(deftype alpha-char () '(and character (satisfies alpha-char-p)))
(deftype reserved-char () '(member #\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,))
(deftype alpha-num () '(and character (satisfies alphanumericp)))
(deftype mark-char () '(member #\- #\_ #\. #\! #\~ #\* #\' #\) #\())
(deftype unreserved-char () '(or alpha-num mark-char))
(deftype escaped-char () '(or alpha-num (member #\%)))
(deftype scheme-char () '(or alpha-num (member #\+ #\- #\.)))
(deftype authority-char () '(or unreserved-char escaped-char (member #\; #\: #\@ #\& #\= #\+ #\$ #\,)))
(deftype path-char () '(or authority-char (member #\/)))
(deftype uri-char () '(or unreserved-char reserved-char escaped-char)))

(defun %meta-parse-uri (str)
  (let (last-result)
  (meta:with-string-meta (buffer str)
     (labels ((make-result ()
                           (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t))
              (scheme (&aux (old-index meta::index) c
                            (result (make-result)))
                      (or (meta:match [@(alpha-char c) !(vector-push-extend c result)
                                       $[@(scheme-char c) !(vector-push-extend c result)]
                                       #\:  !(setf last-result result)])
                          (progn (setf meta::index old-index) nil)))
              (authority (&aux (old-index meta::index) c
                               (result (make-result)))
                         (or (meta:match [#\/ #\/ $[@(authority-char c) !(vector-push-extend c result)]
                                           !(setf last-result result)])
                             (progn (setf meta::index old-index) nil)))
              (path (&aux (old-index meta::index) c
                          (result (make-result)))
                    (or (meta:match [$[@(path-char c) !(vector-push-extend c result)]
                                      !(setf last-result result)])
                        (progn (setf meta::index old-index) nil)))
              (query/fragment (&aux (old-index meta::index) c
                           (result (make-result)))
                     (or (meta:match [$[@(uri-char c) !(vector-push-extend c result)]
                                       !(setf last-result result)])
                         (progn (setf meta::index old-index) nil)))
              (uri (&aux scheme authority path query fragment)
                      (and (meta:match [{[!(scheme) !(setf scheme last-result)] []}
                                       {[!(authority) !(setf authority last-result)] []}
                                       {[!(path) !(setf path last-result)] []}
                                       {[#\? !(query/fragment) !(setf query last-result)] []}
                                       {[#\# !(query/fragment) !(setf fragment last-result)] []}])
                           (values scheme authority path query fragment))))
       (uri)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(meta:disable-meta-syntax))

(defmethod uri-authority ((uri uri))
  "Construct the authority component out of the host and the
   port slot of an uri-object"
  (if (uri-port uri)
      (format nil "~A:~A" (uri-host uri) (uri-port uri))
    (uri-host uri)))

(defmethod (setf uri-authority) ((value string) (uri uri))
  "Construct the authority component out of the host and the
   port slot of an uri-object"
  (let ((colon-pos (position #\: value)))
    (if colon-pos
        (setf (uri-host uri) (subseq value 0 colon-pos)
              (uri-port uri) (subseq value (1+ colon-pos)))
      value)))

(defun render-uri (uri stream)
  "Print an URI-Object in normal URI-notation"
  (format stream 		
          (with-output-to-string (new-stream)
            (with-slots (scheme host port path query fragment) uri
              (when scheme 
                (format new-stream "~A:" (string-downcase (symbol-name scheme))))
              (when host
                (format new-stream "//~A" host))
              (when (and host port)
                (format new-stream ":~A" port))
              (when path
                      (format new-stream "~A" path))
              (when query
                (format new-stream "?~A" query))
              (when fragment
                (format new-stream "#~A" fragment))))))

(defmethod print-object ((uri uri) stream)
	(if *print-escape*
		(format stream "#<~a ~a>" 'uri (render-uri uri nil))
		(render-uri uri stream)))

(defun string-to-keyword (string)
  "Convert a string to an appropriate keyword"
  (when string
    (intern (string-upcase string) (find-package 'keyword))))
		
(defun parse-uri (name &key (class 'uri))
  "Parse an URI string and return a new URI Object initialized with
   the found components"
	(multiple-value-bind (scheme authority path query fragment)
            (%meta-parse-uri name)
          (let* ((pos (position #\: authority))
                 (host (subseq authority 0 pos))
                 (port (when (and pos (< (1+ pos) (length authority)))
                                  (subseq authority (1+ pos)))))
            (make-instance class
                           :scheme (string-to-keyword scheme)
                           :host host
                           :port port
                           :path (and path (plusp (length path))
                                      (coerce path 'simple-string))
                           :query (and query (plusp (length query))
                                       (coerce query 'simple-string))
                           :fragment (and fragment (plusp (length fragment))
                                          (coerce fragment 'simple-string))
			   :string name))))

(defmethod merge-uris (uri base &optional place)
  (declare (ignore place))
  (merge-uris (uri uri) (uri base)))

;;;
;; MERGE-URIS See http://RFC.net/rfc2396.html section 5.2
;;;
(defmethod merge-uris ((uri uri) (base-uri uri) &optional place)
  (declare (ignore place))
  ;; Step 2 Test for current document reference
  (when (and (null (uri-path uri))
             (null (uri-scheme uri))
             (null (uri-authority uri))
             (null (uri-query uri)))
    (return-from merge-uris (copy-uri base-uri :query nil :fragment (uri-fragment uri))))

  ;; Step 3 Test for absolute reference (TODO validating?)
  (when (uri-scheme uri)
    (return-from merge-uris (copy-uri uri)))
  (let ((scheme (uri-scheme base-uri))) ; inherit scheme

    ;; Step 4
    (when (uri-authority uri)
      (return-from merge-uris (copy-uri uri :scheme scheme)))
    (let ((host (uri-host base-uri))
          (port (uri-port base-uri))) ; inherit authority

      ;; Step 5
      (when (and (uri-path uri) (eql (char (uri-path uri) 0) #\/))
        (return-from merge-uris (copy-uri uri :scheme scheme :host host :port port)))

      ;; Step 6
      (%merge-relative-path-uris uri base-uri scheme host port))))

;;; We do some normalization to make the merging easier
(defun %canonicalize-path (opath)
  ;; A leading "" is only ok if it is the only element
  (let* ((path (if (and (cdr opath)
                        (equal (car opath) ""))
                   (cdr opath)
                 opath))
         (last-segment (first (last path))))
    ;; Remove trailing "." or ".."
    (cond ((equal last-segment ".")
           (nconc (nbutlast path) (list "")))
          ((equal last-segment "..")
           (nconc path (list "")))
          (t path))))

(defun %merge-relative-path-uris (uri base-uri scheme host port)
  ;; Step 6 a+b(+d)
  (let ((buffer (%canonicalize-path 
                 (append (rest (%parse-uri-path 
                                (subseq (uri-path base-uri)
                                        0 (or (position #\/ (uri-path base-uri) 
                                                        :from-end t) 0))))
                         (or (rest (uri-parsed-path uri)) '(""))))))
    
    ;; Step c) Remove all occurrences of "./" where . is a complete path segment
    (setf buffer (delete-if #'(lambda (segment) (and (stringp segment)
                                                     (string= "." segment)))
                            buffer))

    ;; Step e) Remove all occurrences of <segment>/../
    (loop for start-pos = (position-if #'(lambda (s) (not (equal s ".."))) buffer)
          for pos = (and start-pos (position-if #'(lambda (s) (equal s "..")) 
                                                buffer :start start-pos))
          while pos 
          do (unless (equal (elt buffer (1- pos)) "..")
               (setf buffer (delete-if (constantly t) buffer :count 2 :start (1- pos)))))
    (copy-uri uri 
              :scheme scheme 
              :host host 
              :port port
              :string nil
              :path (%render-parsed-path (cons :absolute buffer)))))

(defun %relative-path (target base)
  "Calculate the minimum relative path from target in relation to base"
  (let* ((level (or (mismatch base
                              target)
                    0))
             (rel-target (subseq target (1+ (position #\/ (subseq target 0 level) :from-end t))))
             (rel-to (subseq base level))
             (rel-to-levels (loop :for c :across rel-to
                                  :if (char= c #\/) :count c)))
        (if (plusp rel-to-levels)
            (loop :repeat rel-to-levels :do
              (setf rel-target (concatenate 'string "../" rel-target))
              :finally (return rel-target))
          (concatenate 'string "./" rel-target))))


(defmethod enough-uri (uri base)
  (enough-uri (uri uri) (uri base)))

(defmethod enough-uri ((uri uri) (base uri))
  (with-slots (scheme host port path query fragment) uri
      (if (or (string-not-equal host (uri-host base)) (not (equal port (uri-port base))))
          uri
        (make-instance  'uri 
                        :path (and path (%relative-path path (uri-path base)))
                        :query (uri-query uri)
                        :fragment (uri-fragment uri)
                        :plist (uri-plist uri)))))

(defun %render-parsed-path (path)
  "Convert a lisp-list notation parsed path to it's
   normal string representation"
  (with-output-to-string (s)
    (loop :initially (when (eq (first path)
                               :absolute)
                       (write-sequence "/" s))
          :for tail :on (rest path)
          :do (progn
                (if (consp (first tail))
                    (loop :for subtail :on (first tail)
                          :do
                          (format s "~A" (first subtail))
                          (unless (null (rest subtail))
                            (write-char #\; s)))
                  (format s "~A" (first tail)))
                (unless (null (rest tail))
                  (write-char #\/ s))))))
                  
(defun %parse-uri-path (path)
  "Convert the normal string representation of a path to a
   more Lispy list-notation"
  (let ((pathlength (length path)))
    (if (zerop pathlength)
        (list :absolute "")
      (loop :with token = nil
            :with pos1 :of-type fixnum = 0
            :with pos2 :of-type fixnum = 0
            :with result = (if (eql (elt path 0)
                                    #\/)
                               (progn
                                 (incf pos1)
                                 (incf pos2)
                                 (list :absolute))
                             (list :relative))
            :for c :across (subseq path pos1)
            :do (cond ((eql c #\;)
                       (setf token (cons (subseq path pos1 pos2) token)
                             pos1 (incf pos2)))
                      ((eql c #\/)
                       (if token
                           (setf token (cons (subseq path pos1 pos2) token)
                                 pos1 (incf pos2)
                                 result (cons (nreverse token)
                                              result)
                                 token nil)
                         (setf result (cons (subseq path pos1 pos2) result)
                               pos1 (incf pos2))))
                      (t (incf pos2)))
            :finally (when (> pos2 pos1)
                       (let ((s (subseq path pos1 pos2)))
                         (if token 
                             (push (nreverse (push s token)) result)
                           (push (subseq path pos1 pos2) result))))
            (return (nreverse (if (eql (char path (1- pathlength)) #\/)
                                  (cons "" result)
                                result)))))))

(defmethod uri-parsed-path (uri)
  (with-slots (path) uri
    (%parse-uri-path path)))

(defmethod (setf uri-parsed-path) (value uri)
  (setf (uri-path uri)
        (%render-parsed-path value)))

(defun copy-uri (uri &key place scheme host port path query fragment plist string)
  (when place
    (error "PLACE keyword of COPY-URI not implemented."))
  (make-instance 'uri
		 :scheme (or scheme (uri-scheme uri))
		 :host (or host (uri-host uri))
		 :port (or port (uri-port uri))
		 :path (or path (uri-path uri))
		 :query (or query (uri-query uri))
		 :fragment (or fragment (uri-fragment uri))
		 :plist (or plist (uri-plist uri))
		 :string (or string (uri-string uri))))

(provide 'uri)
