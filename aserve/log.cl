;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; log.cl
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
;; $Id: log.cl,v 1.1 2001/08/06 03:42:26 neonsquare Exp $

;; Description:
;;   iserve's logging

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(in-package :net.aserve)

(defvar *enable-logging* t) ; to turn on/off the standard logging method

(defmethod logmess (message)
  (multiple-value-bind (csec cmin chour cday cmonth cyear)
      (decode-universal-time (get-universal-time))
    (let ((str (format nil
		       "~a: ~2,'0d/~2,'0d/~2,'0d - ~2,'0d:~2,'0d:~2,'0d - ~a~%"
		       (mp:process-name mp:*current-process*)
		       cmonth cday (mod cyear 100)
		       chour cmin csec
		       message)))
      (write-sequence str (or *aserve-debug-stream* *initial-terminal-io*)))))

(defmethod brief-logmess (message)
  ;; omit process name and month, day, year
  (multiple-value-bind (csec cmin chour)
      (decode-universal-time (get-universal-time))
    (let ((str (format nil
		       "~2,'0d:~2,'0d:~2,'0d - ~a~%"
		       chour cmin csec
		       message)))
      (write-sequence str (or *aserve-debug-stream* *initial-terminal-io*)))))





(defun log-timed-out-request-read (socket)
  (logmess (format nil "No request read from address ~a" 
		   (socket::ipaddr-to-dotted (socket::remote-host socket)))))



(defmethod log-request ((req http-request))
  ;; after the request has been processed, write out log line
  (if* *enable-logging*
     then (let ((ipaddr (socket:remote-host (request-socket req)))
		(time   (request-reply-date req))
		(code   (let ((obj (request-reply-code req)))
			  (if* obj
			     then (response-number obj)
			     else 999)))
		(length  (or (request-reply-content-length req)
;			     #+(and allegro (version>= 6))
			     #+allegro
			     (excl::socket-bytes-written 
			      (request-socket req))))
	
		(stream (wserver-log-stream
			 (request-wserver req))))
    
	    (format stream
		    "~a - - [~a] ~s ~s ~s~%"
		    (socket:ipaddr-to-dotted ipaddr)
		    (maybe-universal-time-to-date time)
		    (request-raw-request req)
		    code
		    (or length -1)))))

	    	
    
    
(defun log-proxy (uri level action extra)
  ;; log information from the proxy module
  ;;
  (brief-logmess 
   (format nil "~a ~d ~a ~a~@[ ~s~]"
	   (or (getf #-allegro (mp:process-plist mp:*current-process*)
		     #+allegro (mp:process-property-list mp:*current-process*)
		     'short-name)
	       (mp:process-name mp:*current-process*))
	   level
	   action
	   (if* (stringp uri) 
	      then uri 
	      else (net.uri:render-uri uri nil))
	   extra))
  (force-output (or *aserve-debug-stream* *initial-terminal-io*)))

    
  

