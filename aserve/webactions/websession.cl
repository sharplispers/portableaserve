;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; websession.cl
;; session support for webactions
;;
;; copyright (c) 2003 Franz Inc, Oakland CA  - All rights reserved.
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
;; $Id: websession.cl,v 1.1 2003/12/02 14:36:33 rudi Exp $

(in-package :net.aserve)



(defclass websession-master ()
  ;; describes how a set of sessions is managed
  ((prefix :initarg :prefix
	   ;; string that preceeds all keys
	   :accessor sm-prefix)

   (suffix  :initarg :suffix
	    ;; number against which the counter will be xored
	    :accessor sm-suffix)
   
   (counter :initarg :counter
	    :initform 0
	    :accessor sm-counter)
   
   ;; how long a session will last if no reference made to it
   (lifetime :initarg :lifetime
	    :accessor sm-lifetime
	    :initform #.(* 5 60 60) ; five hours
	    )
   
   (cookie-name :initarg :cookie-name
		:initform "webaction"
		:reader sm-cookie-name)
   
   (websessions  :initform (make-hash-table :test #'equal)
	      :reader sm-websessions)))


(defclass websession ()
  ;; individual sessions
  (
   (key  :initarg :key
	 ;; the session key
	 :reader  websession-key)
   
   (lastref  :initform (excl::cl-internal-real-time)
	     :accessor websession-lastref)
   
   ; how we pass the session information
   ; :try-cookie - send via cookie and url rewiting
   ; :cookie - passwed as a cookie
   ; :url  - pass in url	
   (method :initarg :method
	   :initform nil
	   :accessor websession-method)
   
   ; a place for users to hang information onto the session object.
   (data   :initarg :data
	   :initform nil
	   :accessor websession-data)
   
   (variables :initform nil
	      :accessor websession-variables)))




(defmethod initialize-websession-master ((sm websession-master))
  ;; prepare the session master to emit keys
  
  ; randomize the random number generator
  (dotimes (i (logand (get-universal-time) #xfff)) (random 256))
  
  #+unix
  (dotimes (i (logand (excl::filesys-inode ".") #xfff)) (random 256))
  (dotimes (i (logand (get-universal-time) #xff)) (random 256))
  
  (let ((val 1))
    (dotimes (i 4)
      (setq val (+ (ash val 8) (random 255))))
    (setf (sm-prefix sm) (format nil "~x" val))

    (setq val 0)
    (dotimes (i 4)
      (setq val (+ (ash val 8) (random 255))))
    (setf (sm-suffix sm) val)
    
    (setf (sm-counter sm) (random 255)))
  
  )


(defmethod next-websession-id ((sm websession-master))
  (let ((counterval (incf (sm-counter sm))))
    (concatenate 'string (sm-prefix sm)
		 (format nil "~x" (random #xfffffff))
		 (format nil "~x" (logxor (sm-suffix sm) counterval)))))

    
  

(defvar *verify-reaper-started* 0)

(defmethod note-websession-referenced ((sess websession))
  (setf (websession-lastref sess) (excl::cl-internal-real-time))
  
  ; make sure we've got the reaper process running, but don't
  ; check too often since it's not necessary
  (if* (< (decf *verify-reaper-started*) 0)
     then (setq *verify-reaper-started* 30)
	  (ensure-webaction-cleanup-process)))



(defun websession-variable (websession name)
  (and websession
       (cdr (assoc name (websession-variables websession) :test #'equal))))

(defsetf  websession-variable .inv-websession-variable)

(defun .inv-websession-variable (websession name newvalue)
  (if* (null websession)
     then ; do nothing since there is no session
	  newvalue
     else 
	  (let ((ent (assoc name (websession-variables websession) 
			    :test #'equal)))
	    (if* ent
	       then (setf (cdr ent) newvalue)
	       else (setq ent (cons name newvalue))
		    (push ent (websession-variables websession)))
	    newvalue)))


      
(defun reap-unused-sessions (sm)
  (let ((now (excl::cl-internal-real-time))
	(lifetime (sm-lifetime sm))
	(toreap))
    (maphash #'(lambda (id websession)
		 (declare (ignore id))
		 (if* (> now
			 (+ (websession-lastref websession) lifetime))
		    then (push websession toreap)))
	     (sm-websessions sm))
  
    (dolist (websession toreap)
      (format t " flush session ~s~%" (websession-key websession))
      (force-output)
      (remhash (websession-key websession) (sm-websessions sm)))))

  
  
  
  
