;; -*- mode: lisp; package: net.aserve -*-
;;
;; main.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;
;; $Id: main.cl,v 1.14 2002/02/22 15:28:52 desoi Exp $

;; Description:
;;   aserve's main loop

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(defpackage :net.aserve
  (:use :common-lisp :excl :net.html.generator :net.uri)
  (:export
   #:authorize
   #:authorizer
   #:base64-decode
   #:base64-encode
   #:compute-strategy
   #:computed-entity
   ;; don't export, these should be private
   ; #:debug-off		
   ; #:debug-on			
   #:denied-request
   #:enable-proxy
   #:failed-request
   #:form-urlencoded-to-query
   #:get-basic-authorization
   #:get-cookie-values
   #:get-multipart-header
   #:get-multipart-sequence
   #:get-request-body
   #:handle-request
   #:handle-uri		; add-on component..
   #:header-slot-value
   #:http-request  	; class
   #:locator		; class
   #:location-authorizer  ; class
   #:location-authorizer-patterns
   #:password-authorizer  ; class
   #:process-entity
   #:publish
   #:publish-file
   #:publish-directory
   #:query-to-form-urlencoded
   #:reply-header-slot-value 
   #:run-cgi-program
   #:set-basic-authorization
   #:standard-locator
   #:unpublish-locator
   #:vhost
   #:vhost-log-stream
   #:vhost-error-stream
   #:vhost-names

   #:request-method
   #:request-protocol

   #:request-protocol-string
   #:request-query
   #:request-query-value
   #:request-raw-request
   #:request-raw-uri
   #:request-socket
   #:request-uri
   #:request-wserver
   
   #:request-reply-code
   #:request-reply-date
   #:request-reply-content-length
   #:request-reply-content-type
   #:request-reply-plist
   #:request-reply-protocol-string
   #:request-reply-strategy
   #:request-reply-stream
   
   #:set-cookie-header
   #:shutdown
   #:split-into-words
   #:start
   #:uridecode-string
   #:uriencode-string
   #:unpublish
   #:url-argument
   #:url-argument-alist
   #:with-http-response
   #:with-http-body
   
   #:wserver
   #:wserver-default-vhost
   #:wserver-enable-chunking
   #:wserver-enable-keep-alive
   #:wserver-external-format
   #:wserver-filters
   #:wserver-locators
   #:wserver-io-timeout
   #:wserver-log-function
   #:wserver-log-stream
   #:wserver-response-timeout
   #:wserver-socket
   #:wserver-vhosts

   #:*aserve-version*
   #:*default-aserve-external-format*
   #:*http-io-timeout*
   #:*http-response-timeout*
   #:*mime-types*
   #:*response-accepted*
   #:*response-bad-request*
   #:*response-continue*
   #:*response-created*
   #:*response-found*
   #:*response-internal-server-error*
   #:*response-not-found*
   #:*response-not-modified*
   #:*response-ok*
   #:*response-moved-permanently*
   #:*response-see-other*
   #:*response-temporary-redirect*
   #:*response-unauthorized*
   #:*wserver*))

(in-package :net.aserve)

(defparameter *aserve-version* '(1 2 12 :b))

#+allegro
(eval-when (eval load)
    (require :sock)
    (require :process)
    #| #+(version>= 6) |# (require :acldns) ; not strictly required but this is preferred
)

#+lispworks
(import 'cl::fixnump)
#+cmu
(import 'ext:fixnump)
#+mcl
(import 'ccl:fixnump)

(provide :aserve)

(defparameter *aserve-version-string*
    ;; for when we need it in string format
    (format nil "~d.~d.~d.~a" 
	    (car *aserve-version*)
	    (cadr *aserve-version*)
	    (caddr *aserve-version*)
            (cadddr *aserve-version*)))
	    
;;;;;;;  debug support 

(defparameter *debug-all* nil)	; all of the debugging switches
(defparameter *debug-log* nil)  ; all debugging switches that write info
				; to the *debug-stream*
(defparameter *debug-current*  nil)	; current switches set

(defparameter *debug-stream* *initial-terminal-io*)

; set to true to automatically close sockets about to be gc'ed
; open sockets should never be subject to gc unless there's a bug
; in the code leading to leaks.
(defvar *watch-for-open-sockets* nil) 

(defmacro define-debug-kind (name class what)
  `(progn (ecase ,class
	    (:all (pushnew ,name *debug-all*))
	    (:log (pushnew ,name *debug-log*)
		  (pushnew ,name *debug-all*)))
	  (setf (get ,name 'debug-description) ,what)))

(define-debug-kind :notrap :all 
  "If set than errors in handlers cause a break loop to be entered")

(define-debug-kind :xmit   :log
  "If set then most of the traffic between clients and servers is also sent to the debug stream")

(define-debug-kind :info   :log
  "General information")

    

(defun debug-on (&rest args)
  ;; add the given debug kinds to the log list
  (if* (null args)
     then (note-debug-set)
     else (dolist (arg args)
	    (case arg
	      (:all (setq *debug-current* *debug-all*))
	      (:log (setq *debug-current*
		      (union *debug-current* *debug-log*)))
	      (t (pushnew arg *debug-current*))))))

(defun debug-off (&rest args)
  ;; turn off the debugging
  (if* (null args)
     then (note-debug-set)
     else (dolist (arg args)
	    (case arg
	      (:all (setq *debug-current* nil))
	      (:log (setq *debug-current*
		      (set-difference *debug-current* *debug-log*)))
	      (t (setq *debug-current* (remove arg *debug-current*)))))))

(defun note-debug-set ()
  ;; describe what debugging switches exist and if they are on
  ;; and off
  (dolist (kind *debug-all*)
    (format t "~7s ~4a  ~a~%" 
	    kind
	    (if* (member kind *debug-current*)
	       then "on"
	       else "off")
	    (get kind 'debug-description))))

	    

(defmacro debug-format (kind &rest args)
  ;; do the format to *debug-stream* if the kind of this info
  ;; is matched by the value of *debug-current*
  `(if* (member ,kind *debug-current* :test #'eq)
      then (write-sequence 
	    (concatenate 'string
	      (format nil "d> (~a): " (acl-mp:process-name acl-mp:*current-process*))
	      (format nil ,@args))
	    *debug-stream*)))


(defmacro format-dif (debug-key &rest args)
  ;; do the format and also do the same format to the 
  ;; debug stream if the given debug keyword is set
  ;; do the format and then send to *initial-terminal-io*
  `(progn (format ,@args)
	  (if* (member ,debug-key *debug-current* :test #'eq)
	     then ; do extra consing to ensure that it all be written out 
		  ; at once
		  (write-sequence
		   (concatenate 'string 
		     (format nil "x>(~a): " 
			     (acl-mp:process-name acl-mp:*current-process*))
		     (format nil ,@(cdr args)))
		   *debug-stream*))))

(defmacro if-debug-action (kind &rest body)
  ;; only do if the debug value is high enough
  `(progn (if* (member ,kind *debug-current* :test #'eq)
	     then ,@body)))

(defun check-for-open-socket-before-gc (socket)
  (if* (open-stream-p socket)
     then (logmess 
	   (format nil 
		   "socket ~s is open yet is about to be gc'ed. It will be closed" 
		   socket))
	  (ignore-errors (close socket))))


;;;;;;;;;;; end debug support ;;;;;;;;;;;;


;; foreign function imports
#+(and allegro unix)
(progn 
    (ff:def-foreign-call (setuid "setuid") ((x :int)) :returning :int)
    (ff:def-foreign-call (setgid "setgid") ((x :int)) :returning :int)
    (ff:def-foreign-call (getpid "getpid") (:void) :returning :int)
    (ff:def-foreign-call (unix-fork "fork") (:void) :returning :int)
    (ff:def-foreign-call (unix-kill "kill") ((pid :int) (sig :int))
      :returning :int)
    
)
#+(and lispworks unix)
(progn
  (fli:define-foreign-function (setuid "setuid" :source)
                               ((x :int))
                               :result-type :int
                               :language :ansi-c)
  (fli:define-foreign-function (setgid "setgid" :source)
                               ((x :int))
                               :result-type :int
                               :language :ansi-c)
  (fli:define-foreign-function (getpid "getpid" :source)
                               ()
                               :result-type :int
                               :language :ansi-c)
  (fli:define-foreign-function (unix-fork "fork" :source)
                               ()
                               :result-type :int
                               :language :ansi-c)
  (fli:define-foreign-function (unix-kill "kill" :source)
                               ((pid :int) (sig :int))
                               :result-type :int
                               :language :ansi-c)
    (fli:define-foreign-function (unix-signal "signal" :source)
                               ((sig :int) (hdl :int))
                               :result-type :void
                               :language :ansi-c))

#+(and cmu unix)
(progn
  (alien:def-alien-routine "setuid" integer (x integer))
  (alien:def-alien-routine "setgid" integer (x integer))
  (alien:def-alien-routine "getpid" integer)
  (alien:def-alien-routine ("fork" unix-fork) integer)
  (alien:def-alien-routine ("kill" unix-kill) integer (pid integer) (sig integer))
  (alien:def-alien-routine ("signal" unix-signal) c-call:void (sig integer) (hdl integer))
)


;; more specials
(defvar *max-socket-fd* 0) ; the maximum fd returned by accept-connection
(defvar *aserve-debug-stream* nil) ; stream to which to seen debug messages
(defvar *default-aserve-external-format* :latin1-base) 
(defvar *worker-request*)  ; set to current request object

(defvar *read-request-timeout* 20)
(defvar *read-request-body-timeout* 60)
(defvar *http-response-timeout* 
    #+io-timeout 300 ; 5 minutes for timeout if we support i/o timeouts
    #-io-timeout 120 ; 2 minutes if we use this for i/o timeouts too.
    )

; this is only useful on acl6.1 where we do timeout on I/O operations
(defvar *http-io-timeout* 60)

; usually set to the default server object created when aserve is loaded.
; users may wish to set or bind this variable to a different server
; object so it is the default for publish calls.
; Also bound to the current server object in accept threads, thus
; user response functions can use this to find the current wserver object.
(defvar *wserver*)   

; type of socket stream built.
; :hiper is possible in acl6
(defvar *socket-stream-type* 
;    #+(and allegro (version>= 6)) :hiper
;    #-(and allegro (version>= 6)) :stream
  #+allegro :hiper
  #-allegro :stream
)

;; specials from other files
(defvar *header-block-sresource*)
(defvar *header-index-sresource*)
(defvar *header-keyword-array*
    ;; indexed by header-number, holds the keyword naming this header
    )
    
(defvar *not-modified-entity*) ; used to send back not-modified message

	
;;;;;;;;;;;;;  end special vars


(defclass wserver ()
  ;; all the information contained in a web server
  (
   ;;
   ;;-- user visible slots --
   ;; (accessors exported)
   
   (socket 		;; listening socket 
    :initform nil
    :initarg :socket
    :accessor wserver-socket)
     
   (enable-keep-alive ;; do keep alive if it's possible
    :initform t
    :initarg :enable-keep-alive
    :accessor wserver-enable-keep-alive)
     
   (enable-chunking  ;; do chunking if it's possible
    :initform t
    :initarg :enable-chunking
    :accessor wserver-enable-chunking)
     
   (locators
    ;; list of locators objects in search order
    :initform (list (make-instance 'locator-exact
		      :name :exact)
		    (make-instance 'locator-prefix
		      :name :prefix)) 
    :accessor wserver-locators)

   (filters
    ;; if non nil is is a list of functions
    ;; of one arg (a request object) 
    ;; to be called before looking for a locator.  This function can
    ;; modify the request if it feels like it.
    :initform nil
    :accessor wserver-filters)
   
   (log-function
    ;; function to call after the request is done to 
    ;; do the logging
    :initarg :log-function
    :initform nil	; no logging initially
    :accessor wserver-log-function)
   
   (log-stream
    ;; place for log-function to store stream to log to if
    ;; it makes sense to do so
    :initarg :log-stream
    :initform  t 	; initially to *standard-output*
    :accessor wserver-log-stream)

   (accept-hook
    ;; if non-nil the function to call passing the socket about to be
    ;; processed by aserve, and charged with returning the socket to
    ;; process
    :initarg :accept-hook
    :initform nil
    :accessor wserver-accept-hook)

   (external-format
    ;; used to bind *default-aserve-external-format* in each thread
    :initarg :external-format
    :initform :latin1-base
    :accessor wserver-external-format)

   (vhosts
    ;; map names to vhost objects
    :initform (make-hash-table :test #'equalp)
    :accessor wserver-vhosts)
   
   (default-vhost
       ;; vhost representing situations with no virtual host
       :initarg :default-vhost
     :initform (make-instance 'vhost)
     :accessor wserver-default-vhost)

   (response-timeout
    ;; seconds a response is allowed to take before it gives up
    :initarg :response-timeout
    :initform *http-response-timeout*
    :accessor wserver-response-timeout)
   
   (io-timeout
    ;; seconds an I/O operation to an http client is allowed to take
    ;; before an error is signalled.  This is only effective on
    ;; acl6.1 or newer.
    :initarg :io-timeout
    :initform *http-io-timeout*
    :accessor wserver-io-timeout)

   
   ;;
   ;; -- internal slots --
   ;;
   
   (terminal-io  ;; stream active when we started server
    :initform *terminal-io*
    :initarg  :terminal-io
    :accessor wserver-terminal-io)
   
   (worker-threads  ;; list of threads that can handle http requests
    :initform nil
    :accessor wserver-worker-threads)
     
   (free-workers    ;; estimate of the number of workers that are idle
    :initform 0
    :accessor wserver-free-workers)
     
   (accept-thread   ;; thread accepting connetions and dispatching
    :initform nil
    :accessor wserver-accept-thread)

   (link-scan-threads  ;; threads scanning cached entries for links
    :initform nil
    :accessor wserver-link-scan-threads)
   
   (uri-scan-threads  ;; list of uri scanning processes
    :initform nil
    :accessor wserver-uri-scan-threads)
   
   (invalid-request
    ;; entity to invoke given a request that can't be
    ;; satisfied
    :initform nil  ; will build on demand if not present
    :accessor wserver-invalid-request)
   
   (denied-request
    ;; entity to invoke given a request that was denied
    :initform nil  ; will build on demand if not present
    :accessor wserver-denied-request)
   
   (ipaddrs
    ;; list of the ip addresses by which this wserver has been contacted
    :initform nil
    :accessor wserver-ipaddrs
    )
   
   (pcache
    ;; proxy cache
    :initform nil
    :accessor wserver-pcache)

   (shutdown-hooks
    ;; list of functions to call, passing this wserver object as an arg
    ;; when the server shuts down
    :initform nil
    :accessor wserver-shutdown-hooks)
   ))



(defmethod print-object ((wserver wserver) stream)
  (print-unreadable-object (wserver stream :type t :identity t)
    (format stream "port ~a" 
	    (let ((sock (wserver-socket wserver)))
	      (if* sock 
		 then (socket:local-port sock)
		 else "-no socket-")))))
     
     
;;;;; virtual host class
(defclass vhost ()
  ((log-stream :accessor vhost-log-stream
	       :initarg :log-stream
	       :initform *standard-output*)
   (error-stream :accessor vhost-error-stream
		 :initarg :error-stream
		 :initform *standard-output*)
   (names :accessor vhost-names
	  :initarg :names
	  :initform nil)))          


;;;;;; macros 

(defmacro with-http-response ((req ent
				&key timeout
				     (check-modified t)
				     (response '*response-ok*)
				     content-type
                                     format
				     )
			       &rest body)
  ;;
  ;; setup to response to an http request
  ;; do the checks that can shortciruit the request
  ;;
  (let ((g-req (gensym))
	(g-ent (gensym))
	(g-timeout (gensym))
        (g-format (gensym))
	(g-check-modified (gensym)))
    `(let* ((,g-req ,req)
	   (,g-ent ,ent)
           (,g-format ,format)
	   (,g-timeout ,(or timeout

                            `(or
                              (entity-timeout ,g-ent)
                              (wserver-response-timeout *wserver*))))
	   (,g-check-modified ,check-modified)
	   )
       (catch 'with-http-response
	 (compute-strategy ,g-req ,g-ent ,g-format)
	 (up-to-date-check ,g-check-modified ,g-req ,g-ent)
	 (acl-mp::with-timeout ((if* (and (fixnump ,g-timeout)  ; ok w-t
				      (> ,g-timeout 0))
			       then ,g-timeout
			       else 9999999)
			    (timedout-response ,g-req ,g-ent))
	   ,(if* response
	       then `(setf (request-reply-code ,g-req) ,response))
	   ,(if* content-type
	       then `(setf (request-reply-content-type ,g-req) ,content-type)
	       else `(setf (request-reply-content-type ,g-req) (content-type ,g-ent)))
	   ,@body
	   )))))


;#+(and allegro (version>= 6 0 pre-final 1))
#+allegro
(defun warn-if-crlf (external-format)
  (let ((ef (find-external-format external-format)))
    (if* (not (eq (crlf-base-ef ef) ef))
       then (warn "~
External-format `~s' passed to make-http-client-request filters line endings.
Problems with protocol may occur." (ef-name ef)))))

(defmacro with-http-body ((req ent
			   &key headers 
                                (external-format
                                 *default-aserve-external-format*))
			  &rest body)
  (declare (ignorable external-format))
  (let ((g-req (gensym))
	(g-ent (gensym))
	(g-headers (gensym))
	#+allegro(g-external-format (gensym))
	)
    `(let ((,g-req ,req)
	   (,g-ent ,ent)
	   (,g-headers ,headers)
;	   #+(and allegro (version>= 6 0 pre-final 1))
	   #+allegro
	   (,g-external-format (find-external-format ,external-format))
	   )
       (declare #+allegro (ignore-if-unused ,g-req ,g-ent ,g-external-format)
	        #-allegro (ignorable ,g-req ,g-ent))
       ,(if* body 
	   then `(compute-response-stream ,g-req ,g-ent))
       (if* ,g-headers
	  then (bulk-set-reply-headers ,g-req ,g-headers))
       (send-response-headers ,g-req ,g-ent :pre)
       (if* (not (member :omit-body (request-reply-strategy ,g-req)))
	  then (let ((*html-stream* (request-reply-stream ,g-req)))
;		 #+(and allegro (version>= 6 0 pre-final 1))
		 #+allegro
		 (if* (and (streamp *html-stream*)
			   (not (eq ,g-external-format
				    (stream-external-format *html-stream*))))
		    then (warn-if-crlf ,g-external-format)
			 (setf (stream-external-format *html-stream*)
			   ,g-external-format))
		 (progn ,@body)))
       
       (if* (member :keep-alive (request-reply-strategy ,g-req))
	  then ; force the body to be read so we can continue
	       (get-request-body ,g-req))
       (send-response-headers ,g-req ,g-ent :post))))
			  


; safe versions during multiprocessing

(defmacro atomic-incf (var)
  `(acl-mp:without-scheduling (incf ,var)))

(defmacro atomic-decf (var)
  `(acl-mp:without-scheduling (decf ,var)))


;;;;;;;;; end macros

	       
       


(eval-when (compile load eval)
  ;; these are the common headers and are stored in slots in 
  ;; the objects
  ;; the list consists of  ("name" . name)
  ;; where name is symbol naming the accessor function
  (defparameter *fast-headers*
      (let (res)
	(dolist (name '(:connection
			:date
			:transfer-encoding
			:accept
			:host
			:user-agent
			:content-length))
	  (push (list name   ;; keyword symbol name
		      (read-from-string (format nil "reply-~a" name)) ;; symbol name
		      ;; accessor name
		      (read-from-string 
			    (format nil "request-header-~a" name))) res))
	res))
  
  (defparameter *fast-reply-headers*
      ;; list of headers for the reply that at stored in slots of
      ;; the http request object
      (let (res)
	(dolist (name '(:date
			:content-type
			:content-length))
	  (push (list name   ;; string name
		      
		      ;; symbol naming slot
		      (read-from-string 
		       (concatenate 'string (symbol-name :reply-) 
				    (string name)))
		      
		      ;; accessor name
		      (read-from-string 
			    (format nil "request-reply-~a" name))) res))
	res))
  
  )

    
	


(defmacro header-slot-value (req name)
  ;; name is a keyword symbol naming the header value.
  ;; retrive the slot's value from the http-request req req.
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-headers* :test #'eq))
       then ; has a fast accesor
	    `(or (,(third ent) ,req)
		 (setf (,(third ent) ,req)
		    (header-buffer-req-header-value ,req ,name)
		       ))
       else ; must get it from the alist
	    `(header-slot-value-other ,req ,name))))

(defun header-slot-value-other (req name)
  ;; handle out of the the 'extra' headers
  (let ((ent (assoc name (request-headers req) :test #'eq)))
    (if* ent
       then (cdr ent)
       else (let ((ans (header-buffer-req-header-value req name)))
	      (push (cons name ans) (request-headers req))
	      ans))))
      



(defsetf header-slot-value (req name) (newval)
  ;; set the header value regardless of where it is stored
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-headers* :test #'eq))
       then `(setf (,(third ent) ,req) ,newval)
       else (let ((genvar (gensym))
		  (nreq (gensym)))
	      `(let* ((,nreq ,req)
		      (,genvar (assoc ,name (request-headers ,nreq) 
				      :test #'eq)))
		 (if* (null ,genvar)
		    then (push (setq ,genvar (cons ,name nil))
			       (request-headers ,nreq)))
		 (setf (cdr ,genvar) ,newval))))))

(defmacro reply-header-slot-value (req name)
  ;; name is a string naming the header value (all lower case)
  ;; retrive the slot's value from the http-request req req.
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-reply-headers* :test #'eq))
       then ; has a fast accesor
	    `(,(third ent) ,req)
       else ; must get it from the alist
	    `(cdr (assoc ,name (request-reply-headers ,req) :test #'eq)))))

(defsetf reply-header-slot-value (req name) (newval)
  ;; set the header value regardless of where it is stored
  (let (ent)
    (if* (stringp name)
       then (header-name-error name))
    (if* (setq ent (assoc name *fast-reply-headers* :test #'eq))
       then `(setf (,(third ent) ,req) ,newval)
       else (let ((genvar (gensym))
		  (nreq (gensym)))
	      `(let* ((,nreq ,req)
		      (,genvar (assoc ,name (request-reply-headers ,nreq) 
				      :test #'eq)))
		 (if* (null ,genvar)
		    then (push (setq ,genvar (cons ,name nil))
			       (request-reply-headers ,nreq)))
		 (setf (cdr ,genvar) ,newval))))))


(defun header-name-error (name)
  (error "You attempted to reference header ~s.  Headers are now named
by keyword symbols and not by strings"
	 name))

(defmacro header-slot-value-integer (req name)
  ;; if the header value exists and has an integer value
  ;; return two values: the value of the integer and t
  ;; else return nil
  
  `(header-decode-integer (header-slot-value ,req ,name)))



    


(defclass http-header-mixin ()
  ;; List of all the important headers we can see in any of the protocols.
  ;; 
  #.(let (res)
      ;; generate a list of slot descriptors for all of the 
      ;; fast header slots
      (dolist (head *fast-headers*)
	(push `(,(third head) :accessor ,(third head) 
			    :initform nil
			    :initarg
			    ,(intern (symbol-name (second head)) :keyword))
	      res))
      res))
   
	   
	   
   







(defclass http-request (http-header-mixin)
  ;;
  ;; incoming request and information about the reply we send to it
  ;;
  (
   ;;
   ;; -- external slots --
   ;;  (accessors exported)
   
   (method  ;; keyword giving the command in this request :get .. etc.
    :initarg :method
    :reader request-method)
   
   (uri  ;; uri object holding the current request with the scheme, host
         ;; and port filled in.
    :initarg :uri
    :accessor request-uri)

   (raw-uri  ;; uri object holding the actual uri from the command
    :initarg :raw-uri
    :accessor request-raw-uri)
   
   (protocol ;; symbol naming the http protocol  (e.g. :http/1.0)
    :initarg :protocol
    :reader request-protocol)
   
   (protocol-string ;; string naming the protcol requested
    :initarg :protocol-string
    :reader request-protocol-string)
   
   (socket ;; the socket we're communicating through
    :initarg :socket
    :reader request-socket)
   
   (wserver ;; wserver object for web server this request came to
    :initarg :wserver
    :reader request-wserver)
   
   (raw-request  ;; the actual command line from the browser
    :initarg :raw-request
    :reader request-raw-request)

   (vhost  ;; the virtual host to which this request is directed
    :initarg :vhost
    :initform (wserver-default-vhost *wserver*)
    :accessor request-vhost)
   
   ;;
   ;; -- internal slots --
   ;;
   
   (query-alist 
    ;; list of conses (name . value) for the query part of the 
    ;; current uri.  This slot is filled in when information
    ;; is first requested by  the  request-query function
    :initform :empty
    :accessor request-query-alist)
   
   
   (headers ;; alist of headers *not* stored in slots
    ;* use header-slot-value to retrieve header values 
    ;  rather than looking here since not all headers are stored 
    ;  here
    :initform nil
    :accessor request-headers)

   (header-block  ;; *header-block-sresource* object
    :initform nil
    :accessor request-header-block)
    
   (request-body 
    ;; if we've read the request body then this 
    ;; is the string holding it.
    :initform nil
    :accessor request-request-body)
   
   
   

   ;; response
   (reply-code   ;; one of the *response-xx* objects
    :initform nil
    :accessor request-reply-code)
   
   (reply-date
    :initform (get-universal-time)  ; when we're responding
    :reader request-reply-date)
   
   (reply-headers  ;; alist of headers to send out
    :initform nil
    :accessor request-reply-headers)
   
   (reply-content-type ;; mime type of the response
    :initform nil
    :accessor request-reply-content-type)
   
   (reply-stream   ;; stream to which to send response
    :initform nil
    :accessor request-reply-stream)
   
   (reply-content-length
    :initform nil  ;; nil means "i don't know"
    :accessor request-reply-content-length)
   
   (reply-strategy  ;; list of strategy objects
    :initform nil
    :accessor request-reply-strategy)
   
   (reply-plist    ;; general stuff in a property list form
    :initform nil
    :accessor request-reply-plist)

   (reply-protocol-sring
    ;; A web server announces the highest minor level of the 
    ;; major level of the protocol that was requested by the client.
    ;; Thus for now we're always http/1.1
    :initform "HTTP/1.1"
    :accessor request-reply-protocol-string)
   )
  
  
		
  )


(defstruct (response (:constructor make-resp (number desc)))
  number
  desc)

(defparameter *response-continue* (make-resp 100 "Continue"))
(defparameter *response-ok* (make-resp 200 "OK"))
(defparameter *response-created* (make-resp 201 "Created"))
(defparameter *response-accepted* (make-resp 202 "Accepted"))

(defparameter *response-moved-permanently* (make-resp 301 "Moved Permanently"))
(defparameter *response-found* (make-resp 302 "Found"))
(defparameter *response-see-other* (make-resp 303 "See Other"))
(defparameter *response-not-modified* (make-resp 304 "Not Modified"))
(defparameter *response-temporary-redirect* 
    (make-resp 307 "Temporary Redirect"))
(defparameter *response-bad-request* (make-resp 400 "Bad Request"))
(defparameter *response-unauthorized* (make-resp 401 "Unauthorized"))
(defparameter *response-not-found* (make-resp 404 "Not Found"))

(defparameter *response-internal-server-error*
    (make-resp 500 "Internal Server Error"))
(defparameter *response-not-implemented* (make-resp 501 "Not Implemented"))

(defparameter *responses*
    (list *response-continue*
	  *response-ok*
	  *response-created*
	  *response-accepted*
	  *response-moved-permanently*
	  *response-found*
	  *response-see-other*
	  *response-not-modified*
	  *response-temporary-redirect*
	  *response-bad-request*
	  *response-unauthorized*
	  *response-not-found*))

(defvar *crlf* (make-array 2 :element-type 'character :initial-contents
			   '(#\return #\linefeed)))

(defvar *thread-index*  0)      ; globalcounter to gen process names


				    
			      
(defun start (&key (port 80 port-p) 
		   (listeners 5)
		   (chunking t)
		   (keep-alive t)
		   (server *wserver*)
		   debug      ; set debug level
		   setuid
		   setgid
		   proxy
		   proxy-proxy ; who if anyone the proxy proxies to
		   cache       ; enable proxy cache
		   restore-cache ; restore a proxy cache
		   debug-stream  ; stream to which to send debug messages
		   accept-hook
		   ssl		 ; enable ssl
		   os-processes  ; to fork and run multiple instances
                   (external-format nil efp); to set external format
		   )
  ;; -exported-
  ;;
  ;; start the web server
  ;; return the server object
  #-unix
  (declare (ignore setuid setgid))
  
  (declare (ignore debug))  ; for now

  (if* debug-stream 
     then (setq *aserve-debug-stream* 
	    (if* (eq debug-stream t)
	       then *standard-output*
	       else debug-stream)))
  
  (if* (eq server :new)
     then (setq server (make-instance 'wserver)))

  (if* efp then (setf (wserver-external-format server) external-format))

  (if* ssl
     then (if* (pathnamep ssl)
	     then (setq ssl (namestring ssl)))
	  
	  (if* (not (stringp ssl))
	     then (error "The ssl argument should be a string or pathname holding the filename of the certificate and private key file"))
	  
	  (setq accept-hook 
	    #'(lambda (socket)
		(funcall 'socket::make-ssl-server-stream socket
			 :certificate ssl)))
	  (setq chunking nil) ; doesn't work well through ssl
	  (if* (not port-p)
	     then ; ssl defaults to port 443
		  (setq port 443)))
	    
  (if* accept-hook
     then (setf (wserver-accept-hook server) accept-hook))
  
  
  ; shut down existing server
  (shutdown :server server) 

  (if* proxy 
     then (enable-proxy :server server :proxy-proxy proxy-proxy))

  (if* (and (or restore-cache cache)
	    os-processes)
     then ; coordinating the cache between processes is something we're
	  ; not ready to do ... *yet*.
	  (error "Can't have caching and os-processes in the same server"))
  
  #-unix
  (if* os-processes
     then (error "os-processes supported on Unix only at this time"))
  
  
  (if* restore-cache
     then (restore-proxy-cache restore-cache :server server)
   elseif cache
     then ; cache argument can have many forms
	  (let ((memory-size #.(* 10 1024 1024)) ; default 10mb
		(disk-caches nil))
	    (if* (atom cache)
	       then (if* (integerp cache)
		       then (setq memory-size cache))
	       else (do ((info cache (cddr info)))
			((null info))
		      (case (car info)
			(:memory (setq memory-size (cadr info)))
			(:disk (let ((dsize (cadr info)))
				 (if* (atom dsize)
				    then (if* (not (integerp dsize))
					    then (setq dsize 
						   #.(* 10 1024 1024)))
					 
					 (push (cons nil dsize) disk-caches)
							     
				    else (push dsize disk-caches))))
			(t (error "unknown disk cache info specifier: ~s"
				  (car info))))))
		    
	    (create-proxy-cache :server server :size memory-size)
	    (dolist (disk-cache disk-caches)
	      (add-disk-cache :server server
			      :filename (car disk-cache)
			      :size (cadr disk-cache)))))
	    
  

  
  (let* ((main-socket (socket:make-socket :connect :passive
					  :local-port port
					  :reuse-address t
					  :format :bivalent
					  
					  :type 
					  *socket-stream-type*
					  ))
	 (is-a-child))

    #+unix
    (progn
      (if* (fixnump setgid) then (setgid setgid))
      (if* (fixnump setuid) then (setuid setuid)))
    
    (setf (wserver-socket server) main-socket)
    (setf (wserver-terminal-io server) *terminal-io*)
    (setf (wserver-enable-chunking server) chunking)
    (setf (wserver-enable-keep-alive server) keep-alive)

    #+unix
    (if* os-processes
       then ; create a number of processes, letting only the main
	    ; one keep access to the tty
	    (if* (not (and (integerp os-processes) 
			   (>= os-processes 1)))
	       then (error "os-processes should be an integer greater than zero"))
	    (let (children child)
	      (dotimes (i (1- os-processes))
		(if* (zerop (setq child (unix-fork)))
		   then ; we're a child, let the *lisp-listener* go 
			; catatonic
		        #+allegro
		        (excl::unix-signal 15 0) ; let term kill it
		        #-allegro 
		        (net.aserve::unix-signal 15 0)
			(setq is-a-child t 
			      children nil)
			(return) ; exit dotimes 
		   else (push child children)))
	      (if* children
		 then ; setup to kill children when main server 
		      ; shutdown
		      (push #'(lambda (wserver)
				(declare (ignore wserver))
				(dolist (proc children)
				  (unix-kill proc 15) ; 15 is sigterm
				  )
				; allow zombies to die
				(sleep 2)
				(loop (if* (null
					    (sys:reap-os-subprocess :wait nil))
					 then (return))))
			    (wserver-shutdown-hooks server)))))
    
    (let ((*wserver* server)) ; bind it too for privacy
      (if* (or (null listeners) (eq 0 listeners))
	 then (start-simple-server)
       elseif (and (fixnump listeners) (> listeners 0))
	 then (start-lisp-thread-server listeners)
	 else (error "listeners should be nil or a non-negative fixnum, not ~s"
		     listeners)))
    

    (if* is-a-child then (loop (sleep 10000)))
    
    server
    ))


(defun shutdown (&key (server *wserver*) save-cache)
  ;; shutdown the neo server
  ; first kill off old processes if any
  (let ((proc (wserver-accept-thread server)))
    (if* proc
       then ; we want this thread gone and the socket closed 
	    ; so that we can reopen it if we want to.
	    (acl-mp:process-kill proc)
	    (acl-mp:process-allow-schedule)
	    (let ((oldsock (wserver-socket server)))
	      (if* oldsock then (ignore-errors (close oldsock))))
	    (setf (wserver-accept-thread server) nil)))
  
  (dolist (th (wserver-worker-threads server))
    (acl-mp:process-kill th)
    (acl-mp:process-allow-schedule))
  
  (setf (wserver-worker-threads server) nil)
  
  (dolist (hook (wserver-shutdown-hooks server))
    (funcall hook server))
  
  (if* save-cache
     then (save-proxy-cache save-cache :server server)
     else (kill-proxy-cache :server server)))




(defun start-simple-server ()
  ;; do all the serving on the main thread so it's easier to
  ;; debug problems
  (let ((main-socket (wserver-socket *wserver*))
	(ipaddrs (wserver-ipaddrs *wserver*))
        (*default-aserve-external-format* (wserver-external-format *wserver*)))
    (unwind-protect
	(loop
	  (restart-case
	      (let ((sock (socket:accept-connection main-socket))
		    (localhost))
		(if* (not (member (setq localhost (socket:local-host sock))
				  ipaddrs))
		   then ; new ip address by which this machine is known
			(push localhost ipaddrs)
			(setf (wserver-ipaddrs *wserver*) ipaddrs))
                ;; cmucl: we use auto-finalization instead; see
		;; excl:accept-connection -- rudi
		#+allegro(if* *watch-for-open-sockets*
		   then (schedule-finalization 
			 sock 
			 #'check-for-open-socket-before-gc))

		#+io-timeout
		(socket:socket-control 
		 sock 
		 :read-timeout (wserver-io-timeout *wserver*)
		 :write-timeout (wserver-io-timeout *wserver*))

		(process-connection sock))
	    
	    (:loop ()  ; abort out of error without closing socket
	      nil)))
      (close main-socket))))

	
(defun start-lisp-thread-server (listeners)
  ;; start a server that consists of a set of lisp threads for
  ;; doing work and a lisp thread for accepting connections
  ;; and farming out the work
  
  ; create worker threads
  (setf (wserver-free-workers *wserver*) 0)
  (dotimes (i listeners) (make-worker-thread))
  
  
  ; create accept thread
  (setf (wserver-accept-thread *wserver*)

;	#+(or allegro lispworks)
	(acl-mp:process-run-function 
	 (list :name (format nil "aserve-accept-~d" (incf *thread-index*))
	       :initial-bindings
	       `((*wserver*  . ',*wserver*)
		 #+ignore (*debug-io* . ',(wserver-terminal-io *wserver*))
		 ,@excl:*cl-default-special-bindings*))
	 #'http-accept-thread)

#| NEW-MP
	#+cmu
	(mp:process-run-function (format nil "aserve-accept-~d" (incf *thread-index*))
				 nil #'http-accept-thread *wserver*)
|#
))

(defun make-worker-thread ()
#| NEW-MP
  #+cmu
  (mp:without-scheduling
   (let* ((name (format nil "~d-aserve-worker" (incf *thread-index*)))
	  (proc (mp:process-run-function name nil #'http-worker-thread *wserver*)))
     
     (setf (mp:process-run-reasons proc) nil)

     (push proc (wserver-worker-threads *wserver*))
     (atomic-incf (wserver-free-workers *wserver*))
     (setf (getf (mp:process-plist proc) 'short-name)
	   (format nil "w~d" *thread-index*))))
|#

;   #+(or allegro lispworks)
   (let* ((name (format nil "~d-aserve-worker" (incf *thread-index*)))
	  (proc (acl-mp:make-process :name name
				 :initial-bindings
				 `((*wserver*  . ',*wserver*)
				   #+ignore (*debug-io* . ',(wserver-terminal-io 
							     *wserver*))
				   ,@excl:*cl-default-special-bindings*)
				 )))

     (acl-mp:process-preset proc #'http-worker-thread)

     (push proc (wserver-worker-threads *wserver*))
     (atomic-incf (wserver-free-workers *wserver*))
     (setf (getf (acl-mp:process-property-list proc)  'short-name)
	   (format nil "w~d" *thread-index*))))


(defun http-worker-thread ()
  ;; made runnable when there is an socket on which work is to be done
  (let ((*print-level* 5)
        (*worker-request* nil)
	(*default-aserve-external-format* 
	 (wserver-external-format *wserver*))
        )
    ;; lots of circular data structures in the caching code.. we 
    ;; need to restrict the print level
    (loop

      (let ((sock (car (acl-mp:process-run-reasons acl-mp:*current-process*))))
	#-allegro
	(when (eq sock :kill) (return))
	(restart-case
	    (if* (not (member :notrap *debug-current* :test #'eq))
	       then (handler-case (process-connection sock)
		      (error (cond)
			(logmess 
                         (format nil "~agot error ~a~%" 
                                 (if* *worker-request*
                                      then (format
                                            nil
                                            "while processing command ~s~%"
                                            (request-raw-request
                                             *worker-request*))
                                      else "")
                                 cond
                                 ))))
	       else (process-connection sock))
	  (abandon ()
	      :report "Abandon this request and wait for the next one"
	    nil))
	(atomic-incf (wserver-free-workers *wserver*))
	(acl-mp:process-revoke-run-reason acl-mp:*current-process* sock))
    
      )))

(defun http-accept-thread ()
  ;; loop doing accepts and processing them
  ;; ignore sporatic errors but stop if we get a few consecutive ones
  ;; since that means things probably aren't going to get better.
  (let* ((error-count 0)
	 (workers nil)
         (server *wserver*)
	 (main-socket (wserver-socket server))
	 (ipaddrs (wserver-ipaddrs server)))
    (unwind-protect

	(loop
	  (handler-case
	      (let ((sock (socket:accept-connection main-socket))
		    (localhost))
		
		; optional.. useful if we find that sockets aren't being
		; closed
		; cmucl: we use auto-finalizatino instead; see
		; excl:accept-connection
		#+allegro
		(if* *watch-for-open-sockets*
		   then (schedule-finalization 
			 sock 
			 #'check-for-open-socket-before-gc))
		
		; track all the ipaddrs by which we're reachable
		(if* (not (member (setq localhost (socket:local-host sock))
				  ipaddrs))
		   then ; new ip address by which this machine is known
			(push localhost ipaddrs)
			(setf (wserver-ipaddrs *wserver*) ipaddrs))

                #+io-timeout
		(socket:socket-control 
		 sock 
		 :read-timeout (wserver-io-timeout *wserver*)
		 :write-timeout (wserver-io-timeout *wserver*))
		
		; another useful test to see if we're losing file
		; descriptors
		#+allegro
		(let ((fd (excl::stream-input-fn sock)))
		  (if* (> fd *max-socket-fd*)
		     then (setq *max-socket-fd* fd)
			  (logmess (format nil 
					   "Maximum socket file desciptor number is now ~d" fd))))
		
		
		(setq error-count 0) ; reset count
	
		; find a worker thread
		; keep track of the number of times around the loop looking
		; for one so we can handle cases where the workers are all busy
		(let ((looped 0))
		  (loop
		    (if* (null workers) 
		       then (case looped
			      (0 nil)
			      ((1 2 3) (logmess "all threads busy, pause")
				       (sleep 1))
			     
			      (4 (logmess "forced to create new thread")
				 (make-worker-thread))
		    
			      (5 (logmess "can't even create new thread, quitting")
				 (return-from http-accept-thread nil)))
			   
			    (setq workers (wserver-worker-threads server))
			    (incf looped))
		    (if* (null (acl-mp:process-run-reasons (car workers)))
		       then (atomic-decf (wserver-free-workers server))
			    (acl-mp:process-add-run-reason (car workers) sock)
			    (pop workers)
			    (return) ; satisfied
			    )
		    (pop workers))))
	  
	    (error (cond)
	      (logmess (format nil "accept: error ~s on accept ~a" 
			       error-count cond))
	      ;; we seem to get a string of connection reset by peers,
	      ;; perhaps due to connections that piled up before
	      ;; we started work. So we don't want to close down
	      ;; the accepting loop ever, thus we'll ignore the 
	      ;; code below.
	      #+ignore
	      (if* (> (incf error-count) 4)
		 then (logmess "accept: too many errors, bailing")
		      (return-from http-accept-thread nil)))))
      (ignore-errors (progn
		       (acl-mp:without-scheduling
			 (if* (eql (wserver-socket server) main-socket)
			    then (setf (wserver-socket server) nil)))
		       (close main-socket))))))
      
  
    
    

(defun start-cmd ()
  ;; start using the command line arguments
  (let ((port 8001))
    (do* ((args (cdr (sys:command-line-arguments)) (cdr args))
	  (arg (car args) (car args)))
	((null args))
      (if* (equal "-f" arg)
	 then (load (cadr args))
	      (pop args)
       elseif (equal "-p" arg)
	 then (setq port (read-from-string (cadr args)))
	      (pop args)
       elseif (equal "-I" arg)
	 then (pop args)
	 else (warn "unknown arg ~s" arg)))
    (dotimes (i 20)
      (handler-case (progn (start :port port) (loop (sleep 100000)))
	(error (cond)
	  (format t " got error ~a~%" cond)
	  (format t "restarting~%"))))))


(defun process-connection (sock)
  ;; read an http request from the socket and process
  ;; it.
  ;; If the response indicates 'keep alive' then loop around for
  ;; another request.
  ;; When this function returns the given socket has been closed.
  ;;
  
  ; run the accept hook on the socket if there is one
  (let ((ahook (wserver-accept-hook *wserver*)))
    (if* ahook then (setq sock (funcall ahook sock))))
  
	
  (unwind-protect
      (let ((req))
	;; get first command
	(loop
	  (acl-mp:with-timeout (*read-request-timeout* 
			    (debug-format :info "request timed out on read~%")
			    ; this is too common to log, it happens with
			    ; every keep alive socket when the user stops
			    ; clicking
			    ;;(log-timed-out-request-read sock)
			    (return-from process-connection nil))
	    (setq req (read-http-request sock)))
	  (if* (null req)
	     then ; end of file, means do nothing
		  ; (logmess "eof when reading request")
		  ; end this connection by closing socket
		  (return-from process-connection nil)
	     else ;; got a request
                  (setq *worker-request* req)

		  (handle-request req)		  
		  (force-output-noblock (request-socket req))
		  
		  (log-request req)
		  
                  (setq *worker-request* nil)
		  (free-req-header-block req)
		  
		  (let ((sock (request-socket req)))
		    (if* (member :keep-alive
				 (request-reply-strategy req)
				 :test #'eq)
		       then ; continue to use it
			    (debug-format :info "request over, keep socket alive~%")
			    (force-output-noblock sock)
		       else (return))))))
    ;; do it in two stages since each one could error and both have
    ;; to be attempted
    (ignore-errors (force-output-noblock sock))
    (ignore-errors (close sock :abort t))))

(defun force-output-noblock (stream)
  ;; do a force-output but don't get hung up if we get blocked on output
  ;; this happens enough with sockets that it's a real concern
  ; 30 seconds is enough time to wait
  (with-timeout-local (30) 
    (force-output stream)))

  

(defun read-http-request (sock)
  ;; read the request from the socket and return and http-request
  ;; object
  
  (let ((buffer (get-request-buffer))
	(req)
	(end)
	(raw-cmd))
    
    (unwind-protect
	(progn
	  (loop
	    ; loop until a non blank line is seen and is stored in
	    ; the buffer
	    ;
	    ; we handle the case of a blank line before the command
	    ; since the spec says that we should (even though we don't have to)
      
	    (multiple-value-setq (buffer end)
	      (read-sock-line sock buffer 0))
      
	    (if* (null end)
	       then ; eof or error before crlf
		    (return-from read-http-request nil))
      
	    
	    (debug-format  :info "got line of size ~d: " end)
	    (if-debug-action :info
			     (dotimes (i end) (write-char (schar buffer i) 
							  *initial-terminal-io*))
			     (terpri *initial-terminal-io*) (force-output *initial-terminal-io*))
      
	    (if* (not (eql 0 end))
	       then (return) ; out of loop
		    ))
	  
	  (setq raw-cmd (buffer-substr buffer 0 end))
	  
	  (multiple-value-bind (cmd uri protocol)
	      (parse-http-command buffer end)
	    (if* (or (null cmd) (null protocol))
	       then ; no valid command found
		    (return-from read-http-request nil))

	    (if* (null (net.uri:uri-path uri))
	       then (setf (net.uri:uri-path uri) "/"))
	    
	    (setq req (make-instance 'http-request
			:method cmd
			:uri (net.uri:copy-uri uri)
			:raw-uri uri
			:protocol protocol
			:protocol-string (case protocol
					   (:http/1.0 "HTTP/1.0")
					   (:http/1.1 "HTTP/1.1")
					   (:http/0.9 "HTTP/0.9"))
			:socket sock
			:wserver *wserver*
			:raw-request raw-cmd
			))
	    
	    
	    (if* (and (not (eq protocol :http/0.9))
		      #+ignore (null (read-request-headers req sock buffer))
		      (null (new-read-request-headers req sock))
		      )
	       then (debug-format :info "no headers, ignore~%")
		    (return-from read-http-request nil))
	    
	    ; insert the host name and port into the uri
	    (let ((host (header-slot-value req :host)))
	      (if* host
		 then (let ((colonpos (find-it #\: host 0 (length host)))
			    (uri (request-uri req))
			    (port))
			(if* colonpos
			   then ; host:port
				(setq 
				    port (string-to-number
					  host (1+ colonpos)
					  (length host))
				    host (buffer-substr host
							0 colonpos)))
			(if* (null (uri-host uri))
			   then (setf (uri-host uri) host)
				(if* port
				   then (setf (uri-port uri) port)))
			
			(setf (uri-scheme uri) :http)  ; always http
                        
                        ;; set virtual host in the request
			(let ((vhost 
			       (gethash host (wserver-vhosts *wserver*))))
			  (setf (request-vhost req)
			        (or vhost (wserver-default-vhost *wserver*))))

			))))
	  
	    
	  req  ; return req object
	  )
    
      ; cleanup forms
      (if* buffer then (free-request-buffer buffer)))))


				      
    
		    
      
   
    
    
    




(defvar *http-command-list*
    '(("GET " . :get)
      ("HEAD " . :head)
      ("POST " . :post)
      ("PUT "  . :put)
      ("OPTIONS " . :options)
      ("DELETE " .  :delete)
      ("TRACE "  .  :trace)
      ("CONNECT " . :connect)))
  

#+obsolete	    
(defmethod get-request-body ((req http-request))
  ;; return a string that holds the body of the http-request
  ;;  cache it for later too
  (or (request-request-body req)
      (setf (request-request-body req)
	(if* (member (request-method req) '(:put :post))
	   then (multiple-value-bind (length believe-it)
		    (header-slot-value-integer req :content-length)
		  (if* believe-it
		     then ; we know the length
  		          (prog1 (let ((ret (make-string length)))
				   (read-sequence-with-timeout 
				    ret length 
				    (request-socket req)
				    *read-request-body-timeout*)
				   ;; TODO: implement bivalent rational-read-sequence
                                   #+ignore
				   (loop :with result = (make-string length)
				         :for i :from 0 :below length
					 :do (setf (aref result i) (code-char (aref ret i)))
					 :finally (return result)))
			    
	    
			    ; netscape (at least) is buggy in that 
			    ; it sends a crlf after
			    ; the body.  We have to eat that crlf.  
			    ; We could check
			    ; which browser is calling us but it's 
			    ; not clear what
			    ; is the set of buggy browsers 
			    (let ((ch (read-char-no-hang (request-socket req)
							 nil nil)))
			      (if* (eq ch #\return)
				 then ; now look for linefeed
				      (setq ch (read-char-no-hang 
						(request-socket req) nil nil))
				      (if* (eq ch #\linefeed)
					 thenret 
					 else (unread-char 
					       ch (request-socket req)))
			       elseif ch
				 then (unread-char ch (request-socket req)))))
				      
				      
		     else ; no content length given
			  
			  (if* (equalp "keep-alive" 
				       (header-slot-value req :connection))
			     then ; must be no body
				  ""
			     else ; read until the end of file
				  (acl-mp:with-timeout 
				      (*read-request-body-timeout* 
				       nil)
				    (let ((ans (make-array 
						2048 
						:element-type 'character
						:fill-pointer 0))
					  (sock (request-socket req))
					  (ch))
				      (loop (if* (eq :eof 
						     (setq ch (read-char 
							       sock nil :eof)))
					       then (return  ans)
					       else (vector-push-extend ch ans))))))))
	   else "" ; no body
		))))

;; This is intented to be a bit more maintainable than the original
;; -- jsc 02.09.2001
(defmethod get-request-body ((req http-request))
  "Return and cache the body of the http-request as string"
  (labels ((accept-char (char stream)
	     (let ((c (read-char-no-hang stream nil nil)))
	       (cond ((eql c char))
		     (c (unread-char c stream)))))
	   
	   (read-body-with-content-length (length stream)
	     (prog1 (let ((ret (make-string length)))
		      (read-sequence-with-timeout 
		       ret length stream *read-request-body-timeout*))
		      
	       ;; Chop CRLF for buggy browsers like Netscape
		      (and (accept-char #\return stream)
			   (accept-char #\linefeed stream))))
	   
	   (read-body-without-content-length (stream)
	     (acl-mp:with-timeout (*read-request-body-timeout* nil)
	       (if (equalp "keep-alive" (header-slot-value req :connection))
		   "" ; no body
		   (loop with result = (make-array 2048 :element-type 'character :fill-pointer 0)
			 for c = (read-char stream nil :eof)
			 do (vector-push-extend c result)
			 finally (return result))))))

    (or (request-request-body req)
	(setf (request-request-body req)
	      (multiple-value-bind (length believe-it) 
		  (header-slot-value-integer req :content-length)
		(cond ((not (member (request-method req) '(:put :post))) "") ; no body
		      (believe-it (read-body-with-content-length length (request-socket req)))
		      (t (read-body-without-content-length (request-socket req)))))))))
		    

;; multipart code
;; used when enctype=multipart/form-data is used

; new version that uses binary mode to transfer data

(defstruct mp-info
  buffer	; usb8 buffer if we're active
  left		; bytes of content-length left to read
  state		; state where the buffer pointer is pointed
  cur		; current buffer pointer
  after		; after the boundary value
  end		; index after last byte in the buffer
  boundary	; boundary vector
  socket	; socket we're reading from
  )

(defmethod start-multipart-capture ((req http-request))
  ;; begin the grab of the body.
  ;; user doesn't have to call this, it's called automatically
  (let* ((ctype (header-slot-value req :content-type))
	 (parsed (and ctype (parse-header-value ctype)))
	 (boundary (and (equalp "multipart/form-data" (cadar parsed))
			(cdr (assoc "boundary" (cddar parsed) :test #'equal))))
	 (len (header-slot-value-integer req :content-length))
	 (mpbuffer))
    
    (if* (null boundary)
       then ; not in the right form, give up
	    (return-from start-multipart-capture nil))
    
    (setq mpbuffer (get-sresource *header-block-sresource*))
    
    (setf (aref mpbuffer 0) #.(char-code #\return))
    (setf (aref mpbuffer 1) #.(char-code #\linefeed))
    
    (setf (getf (request-reply-plist req) 'mp-info)
      (make-mp-info :buffer mpbuffer
		    :left  len
		    :state :start
		    :cur   0  ; we'll start the buffer with cr,lf
		    :end   2  ; since the first boundary may not have it
		    :socket (request-socket req)
		    
		    ;;  boundary is a case insensitive usb8 array
		    ;; <cr><lf>--boundary-downcased
		    ;;
		    :boundary 
		    (let ((array (make-array (+ 2 ; crlf
						2 ; --
						(length boundary))
					     :element-type '(unsigned-byte 8))))
		      (setf (aref array 0) #.(char-code #\return))
		      (setf (aref array 1) #.(char-code #\linefeed))
		      (setf (aref array 2) #.(char-code #\-))
		      (setf (aref array 3) #.(char-code #\-))
		      
		      (dotimes (i (length boundary))
			(setf (aref array (+ i 4))
			  (char-code (char-downcase (schar boundary i)))))
		      array)
		    ))))


(defparameter *crlf-crlf-usb8*
  ;; the correct way to end a block of headers
    (make-array 4 :element-type '(unsigned-byte 8)
		:initial-contents
		(list #.(char-code #\return)
		      #.(char-code #\linefeed)
		      #.(char-code #\return)
		      #.(char-code #\linefeed))))

(defparameter *lf-lf-usb8*
    ;; the incorrect way to end a block of headers but still found
    ;; in some Unix apps
    (make-array 2 :element-type '(unsigned-byte 8)
		:initial-contents
		(list #.(char-code #\linefeed)
		      #.(char-code #\linefeed))))

(defmethod get-multipart-header ((req http-request))
  ;; return an alist holding the header info for the next request.
  ;; or nil if this is the end of the line
  
  (let ((mp-info (getf (request-reply-plist req) 'mp-info)))
    
    (if* (null mp-info)
       then (start-multipart-capture req)
	    ; satisify normal requests for the body with an empty string
	    (setf (request-request-body req) "") 
	    (setq mp-info (getf (request-reply-plist req) 'mp-info)))
    
    (if* (null mp-info)
       then ; no headers left
	    (return-from get-multipart-header nil))
    
    (loop
      (case (mp-info-state mp-info)
	(:header (return)) ; ok
	((:body :start) 
	 (loop
	   (multiple-value-bind (pos state after)
	       (scan-forward mp-info)
	     (if* (eq state :partial)
		then (if* (not (shift-buffer-up-and-read mp-info))
			then ; no more data, bogus end though
			     (setf (mp-info-state mp-info) :last-boundary)
			     (return))
		else (setf (mp-info-cur mp-info) pos
			   (mp-info-state mp-info) state
			   (mp-info-after mp-info) after)
		     (return)))))
	(:boundary  (scan-forward mp-info))
	(:last-boundary ; no more space
	 ; free up the buffer
	 (free-sresource *header-block-sresource* (mp-info-buffer
						   mp-info))
	 (setf (mp-info-buffer mp-info) nil)
	 ;;
	 (return-from get-multipart-header nil))))
    
    ; in the header state, must find the end of the
    ; header block <cr><lf><cr><lf>
    (let* ((buffer (mp-info-buffer mp-info))
	   cur  
	   headers
	   endhead)
      
      (loop
	(setq cur (mp-info-cur mp-info))
	(setq endhead (search-usb8 buffer 
				   cur 
				   (mp-info-end mp-info)
				   *crlf-crlf-usb8*))
	(if* (integerp endhead) 
	   then (incf endhead 4) ; cr lf cr lf
		(return))
	
	(if* (not (shift-buffer-up-and-read mp-info))
	   then ; failed to find the end of the headers
		(error "end of headers not found, badly formed request")))
      
      ; found the end of the headers.
      (let ((ans (get-sresource *header-index-sresource*)))
	(parse-header-block-internal buffer cur endhead ans)
	(dotimes (i (length ans))
	  (dolist (ent (svref ans i))
	    (push (cons (svref *header-keyword-array* i)
			(parse-header-value
			 (buffer-subseq-to-string buffer (car ent) (cdr ent))))
		  headers)))
	(free-sresource *header-index-sresource* ans))
      
      (setf (mp-info-cur mp-info) endhead)
      (setf (mp-info-state mp-info) :body) ; pointing at the body
      
      headers)))

(defun search-usb8 (buffer start end pattern)
  ;; look for the pattern in the buffer
  (do* ((i start (1+ i))
	(patlen (length pattern))
	(realend (- end patlen -1)))
      ((>= i realend))
    (dotimes (j patlen (return-from search-usb8 i))
      (if* (not (eq (aref buffer (+ i j))  (aref pattern j)))
	 then ; no match
	      (return nil)))))
	      
       
  

(defun shift-buffer-up-and-read (mp-info)
  ;; bring bytes from cur to end to the top of the buffer.
  ;;
  ;; read in more bytes.
  ;;
  ;;
  ;; return true if more any bytes.  nil means that there is
  ;; no more data to read or no more space for data.
  ;;
  (let ((cur (mp-info-cur mp-info))
	(mpbuffer (mp-info-buffer mp-info))
	(left))
    
    (if* (zerop (setq left (mp-info-left mp-info)))
       then nil ; nothing to do, since no more data left
       else ; shift up buffer if needed
	    (if* (> cur 0)
	       then (do ((end (mp-info-end mp-info))
			 (i cur (1+ i)))
			((>= i end))
		      (setf (aref mpbuffer (- i cur)) (aref mpbuffer i)))
		    (setf (mp-info-cur mp-info) 0)
		    (decf (mp-info-end mp-info) cur))
    
	    (if* (eql (mp-info-end mp-info) (length mpbuffer))
	       then ; no room to store anything else
		    nil
	       else ; read as much as we acn
		    (let* ((end (mp-info-end mp-info))
			   (pos (rational-read-sequence mpbuffer 
					       (mp-info-socket mp-info)
					       :start end
					       :end (min
						     (length mpbuffer)
						     (+ end left)))))
		      (if* (<= pos end)
			 then ; no bytes read, eof
			      nil
			 else 
			      (if-debug-action 
			       :xmit
			       (format t "~%multipart read ~d bytes~%" 
				       (- pos end))
			       (do ((i end (1+ i)))
				   ((>= i pos))
				 (write-char (code-char (aref mpbuffer i))))
			       (format t "<end-of-read>~%")
			       (force-output))
				   
				      
			      (decf (mp-info-left mp-info) 
				    (- pos end))
			      (setf (mp-info-end mp-info) pos)
			      t ; read something in
			      ))))))



;; for debugging
#+ignore
(defun dump-mp-info (mp-info)
  (let ((buff (mp-info-buffer mp-info)))
    (format t "dump, state is ~s, cur = ~s, after is ~s, end is ~s~%"
	    (mp-info-state mp-info) (mp-info-cur mp-info) 
	    (mp-info-after mp-info)
	    (mp-info-end mp-info)
	    )
    (format t "buf:~%")
    
    (do ((v (mp-info-cur mp-info) (1+ v)))
	((>= v (mp-info-end mp-info)))
      (write-char (code-char (aref buff v))))
    (format t "<<end>>~%")
    ))
	 

(defun scan-forward (mp-info)
  ;; scan forward to the next interesting thing
  ;;
  ;; rfc2046 describes the multipart syntax
  ;;
  ;; If the current state is :boundary then we are being called 
  ;;   to locate the next header.  If we find it we set cur to point
  ;;   to it and set the state to :header.  If no more data is available
  ;;   (and this will never happen if the client works correctly) we
  ;;   set the state to :last-boundary
  ;;   nil is returned
  ;;
  ;; If the current state is :body or :start
  ;; we look forward to find the point a which the next :boundary
  ;; starts.  We give up at the end of the bytes read into the buffer  
  ;; we don't change cur or the state of the mp-info
  ;; We return two values:
  ;;  the point at which that transition occurs to boundary
  ;;  a symbol describing what we found
  ;;        :body,:state   - found nothing interesting, we're still in the same
  ;;	              state we started in (either :body or :state)
  ;;	    :boundary - we've identified a boundary between items
  ;;	    :last-boundary  - we've indentified the final boundary 
  ;;	    :partial - we've identified something that could be
  ;;		a boundary or last-boundary but don't have enough
  ;;		data to tell.
  ;(dump-mp-info mp-info)
  (case (mp-info-state mp-info)
    (:boundary
     ;; skip past boundary if it's in the buffer
     ;; this is called only in get-multipart-header
     (loop
       (let ((past (mp-info-after mp-info)))
	 (if* (< past (mp-info-end mp-info))
	    then ; inside the buffer
		 (setf (mp-info-cur mp-info) past)
		 (setf (mp-info-state mp-info) :header)
		 (return)
	    else (if* past
		    then ; adjust 'past' location to account for shifting
			 ; buffer contents up
			 (decf (mp-info-after mp-info)
			       (mp-info-cur mp-info)))
		 (if* (not (shift-buffer-up-and-read mp-info))
		    then ; no more data
			 (setf (mp-info-state mp-info) :last-boundary)
			 (setf (mp-info-after mp-info) (mp-info-cur mp-info))
			 (return))))))
    (:last-boundary nil)
    
    ((:body  :start)
     ;; looking for a boundary or partial boundary
     (let* ((cur (mp-info-cur mp-info))
	    (end (mp-info-end mp-info))
	    (boundary (mp-info-boundary mp-info))
	    (len-boundary (length boundary))
	    )
       (if* (eql cur end)
	  then (if* (not (shift-buffer-up-and-read mp-info))
		  then ; no more data available
		       (setf (mp-info-state mp-info) :last-boundary)
		       (return-from scan-forward (values 0 :last-boundary 0))))
       (setq cur (mp-info-cur mp-info)
	     end (mp-info-end mp-info))
       
       (do* ((i cur (1+ i))
	     (mpbuffer (mp-info-buffer mp-info)))
	   ((>= i end)
	    (values end (mp-info-state mp-info)))
	 
	 (let* ((ch (aref mpbuffer i)))
	   (if* (eq ch #.(char-code #\return))
	      then ; this could match
		   (do ((j i (1+ j))
			(ind 0 (1+ ind)))
		       
		       (nil)
		     (if* (>= ind len-boundary)
			then ; matched the whole boundary
			     ; may be followed by white space 
			     ; then crlf (for boundary)
			     ; or -- (for closing boundary)
			     
			     (do ((jj j (1+ jj)))
				 ((>= jj end)
				  ; can't tell yet
				  (return-from scan-forward
				    (values i :partial)))
			       
			       (if* (member (aref mpbuffer jj)
					    '(#.(char-code #\space)
					      #.(char-code #\tab)))
				  thenret ; pass over
				  else ; we need at least 2 chars in the buff
				       ; to see crlf or --
				       (if* (>= (1+ jj) end)
					  then ; only one char
					       (return-from scan-forward
						 (values i :partial)))
					       
				       (if* (and (eql (aref mpbuffer jj)
						      #.(char-code #\return))
						 (eql (aref mpbuffer (1+ jj))
						      #.(char-code #\newline)))
					  then (return-from scan-forward
						 (values i :boundary (+ jj 2)))
					elseif (and (eq (aref mpbuffer jj)
							#.(char-code #\-))
						    (eq (aref mpbuffer (1+ jj))
							#.(char-code #\-)))
					  then (return-from scan-forward
						 (values i :last-boundary (+ jj 2)))
					  else ; nothing we recognize
					       (return))))
				       
			     ; if here then doesn't match boundary
			     (return)
		      elseif (>= j end)
			then ; end of buffer before end of boundary
			     (return-from scan-forward
			       (values i :partial)))
		     
 		     ; boundary value is downcased so we must downcase 
 		     ; value in buffer.  we had to do the downcasing since
 		     ; I found cases where a case insensitive match had
 		     ; to be done
 		     (let ((bufch (aref mpbuffer j)))
 		       (if* (<= #.(char-code #\A) bufch #.(char-code #\Z))
 			  then (incf bufch #.(- (char-code #\a)
 						(char-code #\A))))
 		       (if* (not (eq bufch (aref boundary ind)))
			  then (return))))))))))) 


       
		     
		     
(defmethod get-multipart-sequence ((req http-request)
				   buffer
				   &key (start 0)
					(end (length buffer))
					(external-format 
                                         *default-aserve-external-format*
                                         ef-spec))
  ;; fill the buffer with the chunk of data.
  ;; start at 'start' and go no farther than (1- end) in the buffer
  ;; return the index of the first character not placed in the buffer.
  
  
  ;; Since external-format not used in all versions
  (declare (ignorable external-format ef-spec))

;  #-(and allegro (version>= 6 0 pre-final 1))
#|  #-(or allegro lispworks)
  (if* ef-spec
     then (warn "~
For this version of Lisp, external-format is ignored ~
in get-multipart-sequence"))|#

  (let* ((mp-info (getf (request-reply-plist req) 'mp-info))
	 mpbuffer 
	 cur
	 pos
	 kind
	 text-mode
	 after)

    (typecase buffer
      ((array (unsigned-byte 8) (*))
       )
      ((array character (*))
       (setq text-mode t))
      (t 
       (error 
	"This function only accepts (array (unsigned-byte 8)) or character arrays")))
    (if* (null mp-info)
       then (error "get-multipart-sequence called before get-multipart-header"))
    
    (setq mpbuffer (mp-info-buffer mp-info)
	  cur      (mp-info-cur mp-info))

    (loop
      (case (mp-info-state mp-info)
	((:header :boundary :last-boundary)
	 ; no data left
	 (return-from get-multipart-sequence nil))
	(:start
	 (error "get-multipart-sequence called before get-multipart-header"))
	((:body :partial)
	 (if* (eq (mp-info-state mp-info) :partial)
	    then ; this was set below. we will return the partial
		 ; at then end of the buffer
		 (setf (mp-info-state mp-info) :body)
		 (setq pos (mp-info-end mp-info))
	    else (multiple-value-setq (pos kind after) (scan-forward mp-info))
		 (setf (mp-info-after mp-info) after)
		 (setq cur (mp-info-cur mp-info)) ; scan-forward can change
		 )
	 (if* (> pos cur)
	    then ; got something to return
		 (let* ((tocopy (min (- end start) (- pos cur)))
			(items tocopy))
		   (if* text-mode
		      then 
			   ; here is where we should do
			   ; external format processing
#|;			   #+(and allegro (version>= 6 0 pre-final 1))
		           #+allegro
			   (multiple-value-setq (buffer items tocopy)
			     (octets-to-string
			      mpbuffer
			      :string buffer
			      :start cur
			      :end (+ cur tocopy)
			      :string-start start
			      :string-end (length buffer)
			      :external-format external-format
			      :truncate t)
;			   #-(and allegro (version>= 6 0 pre-final 1))
|#			   #-allegro
			   (dotimes (i tocopy)
			     (setf (aref buffer (+ start i))
			       (code-char (aref mpbuffer (+ cur i)))))
		      else 
			   (dotimes (i tocopy)
			     (setf (aref buffer (+ start i))
			       (aref mpbuffer (+ cur i)))))
		   (setf (mp-info-cur mp-info) (+ cur tocopy))
		   (return-from get-multipart-sequence (+ start items)))
	  elseif (eq kind :partial)
	    then  ; may be a boundary, can't tell
		 (if* (null (shift-buffer-up-and-read mp-info))
		    then ; no more data, partial will never match
			 ; so return the partial, this special
			 ; state is recognized in this routine
			 (setf (mp-info-state mp-info) :partial)
			 ; loop around
			 )
	  elseif (or (eq kind :boundary)
		     (eq kind :last-boundary))
	    then ; hit a boundary, nothing more to return
		 (setf (mp-info-state mp-info) kind
		       (mp-info-cur   mp-info) pos)
		 (return-from get-multipart-sequence nil)))))))

  



;; end multipart code






	      
(defun read-sequence-with-timeout (string length sock timeout)
  ;; read length bytes into sequence, timing out after timeout
  ;; seconds
  ;; return nil if things go wrong.
  (with-timeout-local (timeout nil)
    (let ((got 0))
      (loop
	(let ((this (rational-read-sequence string sock :start got)))
	  (if* (<= this 0)
	     then (return nil) ; eof too early
	     else (setq  got    this)
		  (if* (>= got length ) then (return string))))))))
		
    
      

(defun read-sock-line (sock buffer start)
  ;; read a line of data into the socket buffer, starting at start.
  ;; return  buffer and index after last character in buffer.
  ;; get bigger buffer if needed.
  ;; If problems occur free the passed in buffer and return nil.
  ;;
  
  (let ((max (length buffer))
	(prevch))
    (loop
      (let ((ch (read-byte sock nil :eof)))
	(if* (eq ch :eof)
	   then (debug-format :info "eof on socket~%")
		(free-request-buffer buffer)
		(return-from read-sock-line nil))

      	(setq ch (code-char ch))
	(if* (eq ch #\linefeed)
	   then (if* (eq prevch #\return)
		   then (decf start) ; back up to toss out return
			)
		(setf (schar buffer start) #\null) ; null terminate
		
		; debug output
		; dump out buffer
		(debug-format :info "read on socket: ")
		(if-debug-action :info
				 (dotimes (i start)
				   (write-char (schar buffer i) *initial-terminal-io*))
				 (terpri *initial-terminal-io*))
		;; end debug
			
		(return-from read-sock-line (values buffer start))
	   else ; store character
		(if* (>= start max)
		   then ; must grow the string
			(let ((new-buffer (get-request-buffer (+ max 1024))))
			  (if* (null new-buffer)
			     then ;; too large, give up
				  (free-request-buffer buffer)
				  (return-from read-sock-line nil)
			     else ; got it
				  (dotimes (i start)
				    (setf (schar new-buffer i) 
				      (schar buffer i)))
				  (setq max (length new-buffer))
				  (free-request-buffer buffer)
				  (setq buffer new-buffer))))
		;  buffer is big enough
		(setf (schar buffer start) ch)
		(incf start))
	
	(setq prevch ch)))))
      
		      
				  
(defmethod request-query ((req http-request) &key (post t) (uri t)
						  (external-format
						   *default-aserve-external-format*))
  ;; decode if necessary and return the alist holding the
  ;; args to this url.  In the alist items the value is the 
  ;; cdr of the alist item.
  ;;
  ;; If uri is true then we look for query information in the uri
  ;; (following a question mark)
  ;; If post is true and this is a post request then we look for
  ;; query information in the body of the query.
  ;; If both are true (and this is a post) then we look both places.
  ;;
  ;;
  (let ((alist (request-query-alist req))
	(signature (cons post uri)))
    
    (if* (not (eq alist :empty))
       then (let ((given-sig (getf (request-reply-plist req) 
				   'request-query-sig)))
	      (if* (equal given-sig signature)
		 then ; same args as before, cached value is legit
		      (return-from request-query alist))))
    
    (let (res)
      (if* uri
	 then (let ((arg (uri-query (request-uri req))))
		(if* arg
		   then (setq res (form-urlencoded-to-query
				   arg
				   :external-format external-format)))))
	      
      (if* post
	 then (if* (and (eq (request-method req) :post)
			(equal (header-slot-value req :content-type)
			    "application/x-www-form-urlencoded"))
		 then (setf res
			(append res
				(form-urlencoded-to-query
				 (get-request-body req)
				 :external-format external-format)))))
      (setf (getf (request-reply-plist req) 'request-query-sig)
	signature)
      (setf (request-query-alist req) res))))
			

(defun request-query-value (key req &key (post t) (uri t) (test #'equal))
  ;; access the value of the given key in the request's 
  ;; request query.  We do this so often that it's useful
  ;; to make this a function
  (cdr (assoc key (request-query req :post post :uri uri) :test test)))


	

(defun header-decode-integer (val)
  ;; if val is a string holding an integer return its value
  ;; and t,
  ;; else nil
  (if* val 
     then (let (ans)
	    (setq ans (string-to-number val 0 (length val)))
	    (if* (integerp ans)
	       then (values ans t)))))

#+allegro
(defun date-to-universal-time (date)
  ;; convert  a date string to lisp's universal time
  ;; we accept all 3 possible date formats

  (flet ((cvt (str start-end)
	   (let ((res 0))
	     (do ((i (car start-end) (1+ i))
		  (end (cdr start-end)))
		 ((>= i end) res)
	       (setq res 
		 (+ (* 10 res)
		    (- (char-code (schar str i)) #.(char-code #\0))))))))
    ;; check preferred type first (rfc1123 (formerly refc822)):
    ;;  	Sun, 06 Nov 1994 08:49:37 GMT
    (multiple-value-bind (ok whole
			  day
			  month
			  year
			  hour
			  minute
			  second)
	(match-regexp 
	 "[A-Za-z]+, \\([0-9]+\\) \\([A-Za-z]+\\) \\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) GMT"
	 date
	 :return :index)
      (declare (ignore whole))
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year)
		 0))))
    
    ;; now second best format (but used by Netscape sadly):
    ;;		Sunday, 06-Nov-94 08:49:37 GMT
    ;;
    (multiple-value-bind (ok whole
			  day
			  month
			  year
			  hour
			  minute
			  second)
	(match-regexp
	 
	 "[A-Za-z]+, \\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) GMT"
	 date
	 :return :index)
      
      (declare (ignore whole))
      
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year) ; cl does right thing with 2 digit dates
		 0))))
    
    
    ;; finally the third format, from unix's asctime
    ;;     Sun Nov  6 08:49:37 1994
    (multiple-value-bind (ok whole
			  month
			  day
			  hour
			  minute
			  second
			  year
			  )
	(match-regexp
	 
	 "[A-Za-z]+ \\([A-Za-z]+\\) +\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\)"
	 date
	 :return :index)
      
      (declare (ignore whole))
      
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year)
		 0))))
      
      
    ))

;;;
;;; DATE-TO-UNIVERSAL-TIME
;;; Rewritten using the META parser
;;; It reimplements the original function without MATCH-REGEXP
;;; which is not fully implemented in ACL-COMPAT
;;;
#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (meta:enable-meta-syntax)
(deftype alpha-char () '(and character (satisfies alpha-char-p)))
(deftype digit-char () '(and character (satisfies digit-char-p)))
)
#-allegro
(defun date-to-universal-time (date)
  ;; convert  a date string to lisp's universal time
  ;; we accept all 3 possible date formats
  
  ;; check preferred type first (rfc1123 (formerly refc822)):
  ;;    Sun, 06 Nov 1994 08:49:37 GMT
  ;; now second best format (but used by Netscape sadly):
  ;;    Sunday, 06-Nov-94 08:49:37 GMT
  ;; finally the third format, from unix's asctime
  ;;    Sun Nov  6 08:49:37 1994

  (let (last-result)
    (meta:with-string-meta (buffer date)
      (labels ((make-result ()
                            (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t))
               (skip-day-of-week (&aux c)
				 (declare (ignorable c))
                                 (meta:match [$[@(alpha-char c)] !(skip-delimiters)]))
               (skip-delimiters ()
                                (meta:match $[{#\: #\, #\space #\-}]))
               (word (&aux (old-index meta::index) c
                           (result (make-result)))
                     (or (meta:match [!(skip-delimiters) @(alpha-char c) !(vector-push-extend c result)
                                      $[@(alpha-char c) !(vector-push-extend c result)]
                                      !(setf last-result result)])
                         (progn (setf meta::index old-index) nil)))
               (integer (&aux (old-index meta::index) c
                              (result (make-result)))
                        (or (meta:match [!(skip-delimiters) @(digit-char c) !(vector-push-extend c result)
                                         $[@(digit-char c) !(vector-push-extend c result)]
                                         !(setf last-result (parse-integer result))])
                            (progn (setf meta::index old-index) nil)))
               (date (&aux day month year hours minutes seconds)
                     (and (meta:match [!(skip-day-of-week)
                                       {[!(word) !(setf month last-result)
                                         !(integer) !(setf day last-result)]
                                        [!(integer) !(setf day last-result)
                                         !(word) !(setf month last-result)]} 
                                       !(integer) !(setf year last-result) 
                                       !(integer) !(setf hours last-result) 
                                       !(integer) !(setf minutes last-result) 
                                       !(integer) !(setf seconds last-result)])
                         ; (values seconds minutes hours day month)
                          (encode-universal-time seconds minutes hours day
		                                 (net.aserve::compute-month (coerce month 'simple-string) 0)
		                                 year
		                                 0)
                          )))
              (date)))))

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
(meta:disable-meta-syntax))
    
    
    
    
	  
(defun compute-month (str start)
  ;; return the month number given a 3char rep of the string
  
  (case (schar str start)
    (#\A  
     (if* (eq (schar str (1+ start)) #\p)
	then 4 ; april
	else 8 ; august
	     ))
    (#\D 12) ; dec
    (#\F 2 ) ; feb
    (#\J
     (if* (eq (schar str (1+ start)) #\a)
	then 1 ; jan
      elseif (eq (schar str (+ 2 start)) #\l)
	then 7 ; july
	else 6 ; june
	     ))
    (#\M
     (if* (eq (schar str (+ 2 start)) #\r)
	then 3 ; march
	else 5 ; may
	     ))
    (#\N 11) ; nov
    (#\O 10)  ;oct
    (#\S 9) ; sept
    ))
    
     

(defun maybe-universal-time-to-date (ut-or-string &optional (time-zone 0))
  ;; given a ut or a string, only do the conversion on the string
  (if* (stringp ut-or-string) 
     then ut-or-string
     else (universal-time-to-date ut-or-string time-zone)))

(defparameter *saved-ut-to-date* nil)

(defun universal-time-to-date (ut &optional (time-zone 0))
  ;; convert a lisp universal time to rfc 1123 date
  ;;
  (let ((cval *saved-ut-to-date*))
    (if* (and (eql ut (caar cval))
	      (eql time-zone (cdar cval)))
       then ; turns out we often repeatedly ask for the same conversion
	    (cdr cval)
       else
	    (let ((*print-pretty* nil))
	      (multiple-value-bind
		  (sec min hour date month year day-of-week dsp tz)
		  (decode-universal-time ut time-zone)
		(declare (ignore tz dsp))
		(let ((ans
		       (format
			nil
			"~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d~@[ GMT~]"
			(svref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
			       day-of-week)
			date
			(svref '#(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
				  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
			       month)
			year
			hour
			min
			sec
			(= 0 time-zone))))
		  (setf *saved-ut-to-date* (cons (cons ut time-zone) ans))
		  ans))))))



;; ----- simple resource

(defstruct sresource
  data	 ; list of buffers
  create ; create new object for the buffer
  init	 ; optional - used to init buffers taken off the free list
  )

(defun create-sresource (&key create init)
  (make-sresource :create create :init init))

(defun get-sresource (sresource &optional size)
  ;; get a new resource. If size is given then ask for at least that
  ;; size
  (let (to-return)
    ;; force new ones to be allocated
    (acl-mp:without-scheduling 
      (let ((buffers (sresource-data sresource)))
	(if* size
	   then ; must get one of at least a certain size
		(dolist (buf buffers)
		  (if* (>= (length buf) size)
		     then (setf (sresource-data sresource)
			    (delete buf buffers :test #'eq))
			  (setq to-return buf)
			  (return)))
	    
		; none big enough
	      
	   else ; just get any buffer
		(if* buffers
		   then (setf (sresource-data sresource) (cdr buffers))
			(setq to-return (car buffers)))
		
		)))
  
    (if* to-return
       then ; found one to return, must init
	    
	    (let ((init (sresource-init sresource)))
	      (if* init
		 then (funcall init sresource to-return)))
	    to-return
       else ; none big enough, so get a new buffer.
	    (funcall (sresource-create sresource)
		     sresource
		     size))))
  
(defun free-sresource (sresource buffer)
  ;; return a resource to the pool
  ;; we silently ignore nil being passed in as a buffer
  (if* buffer 
     then (acl-mp:without-scheduling
	    ;; if debugging
	    (if* (member buffer (sresource-data sresource) :test #'eq)
	       then (error "freeing freed buffer"))
	    ;;
	    
	    (push buffer (sresource-data sresource)))))




;; ----- scratch buffer resource:

(defparameter *request-buffer-sresource* 
    (create-sresource 
     :create #'(lambda (sresource &optional size)
			  (declare (ignore sresource))
			  (make-array (or size 2048)
				      :element-type 'character))))

(defun get-request-buffer (&optional size)
  (get-sresource *request-buffer-sresource* size))

(defun free-request-buffer (buffer)
  (free-sresource *request-buffer-sresource* buffer))



	    
;;-----------------


(defun string-to-number (string &optional (start 0) (end (length string)))
  ;; convert the string into a number.
  ;; the number is integer base 10
  ;; this is faster than creating a string input stream and
  ;; doing a lisp read
  ;; string must be a simple string
  ;;
  ;; we allow whitespace before and after the number, anything else
  ;; will cause us to return 0
  ;;
  (let ((ans)
	(state :pre))
    (do ((i start)
	 (ch)
	 (digit))
	((>= i end)
	 (if* (member state '(:number :post) :test #'eq)
	    then ans
	    else nil))
      
      (setq ch (schar string i)
	    digit (- (char-code ch) #.(char-code #\0)))
      
      (case state
	(:pre (if* (member ch '(#\space #\tab #\newline #\return) :test #'eq)
		 then (incf i)
		 else (setq state :number-first)))
	(:number-first
	 (if* (<= 0 digit 9)
	    then (setq ans digit)
		 (incf i)
		 (setq state :number) ; seen a digit
	    else (return-from string-to-number nil) ; bogus
		 ))
	(:number
	 (if* (<= 0 digit 9)
	    then (setq ans (+ (* ans 10) digit))
		 (incf i)
	    else (setq state :post)))
	
	(:post 
	 (if* (member ch '(#\space #\tab #\newline #\return) :test #'eq)
	    then (incf i)
	    else (return-from string-to-number nil)))))))
	
(defun get-host-port (string &optional (port 80))
  ;; return the host and port from the string 
  ;; which should have the form: "www.foo.com" or "www.foo.com:9000"
  ;;
  ;; port is the default value for the port arg
  ;;
  ;; return two values:
  ;;	host	string
  ;;	port	integer
  ;; or nil if there host string is malformed.
  ;;
  (let ((parts (split-on-character string #\:)))
    (if* (null (cdr parts))
       then (values (car parts) port)
     elseif (null (cddr parts))
       then ; exactly two
	    (if* (equal "" (cadr parts))
	       then ; treat nothing after a colon like no colon present
		    (values (car parts) port)
	       else (setq port (string-to-number (cadr parts)))
		    (if* port
		       then (values (car parts) port))))))

	    
	
		
	
;;-------------------
;; authorization

(defmethod get-basic-authorization ((req http-request))
  ;; return the basic authorization information for this request, if any
  ;; 
  ;; basic authorization is used when a name/password dialog is
  ;; put up by the browser
  ;;
  ;; if authorization info in found this request, return two values
  ;;  name
  ;;  password
  ;;
  (let ((auth-value (header-slot-value req :authorization)))
    (if* auth-value
       then (let ((words (split-into-words auth-value)))
	      (if* (equalp (car words) "basic")
		 then (setq auth-value 
			(split-on-character (base64-decode (cadr words)) #\:))
		      (values-list auth-value))))))
		      
	      
(defmethod set-basic-authorization ((req http-request) realm)
  ;; declare that you want to get authentication information
  ;; for the given realm.
  ;; This must be called after with-http-response and before
  ;; with-http-body
  (setq realm (string realm))
  (setf (reply-header-slot-value req :www-authenticate)
    (format nil "Basic realm=~s" realm)))
    


;=======

(defun bulk-set-reply-headers (req headers)
  ;; given an alist list of headers to set, set the header info
  ;; in the correct place (given fast vrs slow headers)
  (let ((fast-headers *fast-reply-headers*)
	(current-headers (request-reply-headers req)))
    (dolist (header headers)
      (let ((this (car header))
	    (ent))
	(if* (setq ent (assoc this fast-headers :test #'eq))
	   then ; a fast one
		(setf (slot-value req (second ent)) (cdr header))
	   else ; a slow one
		(if* (null (setq ent (assoc this 
					    current-headers :test #'eq)))
		   then ; not present yet
			(push (setq ent (cons this nil)) current-headers))
		(setf (cdr ent) (cdr header)))))
    (setf (request-reply-headers req) current-headers)))
	

(defun code-to-response (code)
  ;; return response object for the given code
  (let ((obj (find code *responses* :key #'response-number)))
    (if* (null obj)
       then (push (setq obj (make-resp code "unknown code")) *responses*))
    obj))
  

