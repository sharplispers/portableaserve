;;;; -*- mode: lisp -*-
;;;;
;;;; Package definitions for acl-compat.
;;;;
;;;; Package names follow their Allegro CL counterparts -- for an ACL
;;;; package foo, acl-compat defines a package acl-compat.foo
;;;;
;;;; Some packages have nicknames, which were used as package names by
;;;; previous versions of paserve and acl-compat.  The nicknames are
;;;; deprecated, but are kept for the benefit of people using
;;;; acl-compat in other projects.  New projects should use the
;;;; package names starting with "acl-compat.".
;;;;

;;; general
(defpackage :acl-compat.excl
  (:use #:common-lisp #:nregex
        #+cmu #:ext
        #+clisp #:ext
        #+sbcl #:sb-ext #+sbcl #:sb-gray
        )
  (:nicknames #-allegro #:excl)                   ; to be nuked later
  #+lispworks (:import-from :common-lisp #:fixnump)
  #+sbcl (:import-from :sb-int #:fixnump)
  (:export
   #:if*
   #:*initial-terminal-io*
   #:*cl-default-special-bindings*
   #:filesys-size
   #:filesys-write-date
   #:stream-input-fn
   #:match-regexp
   #:compile-regexp
   #:*current-case-mode*
   #:intern*
   #:filesys-type
   #:errorset
   #:atomically
   #:fast
   #:without-package-locks
   #:fixnump
   #+(or lispworks mcl) #:socket-error
   #+(or lispworks mcl) #:run-shell-command
   #+mcl #:fasl-read
   #+mcl #:fasl-write
   #+(or cmu scl) #:string-to-octets
   #+(or cmu scl) #:write-vector
   ))


;; general
(defpackage :acl-compat.mp
  (:use :common-lisp)
  (:nicknames :acl-mp :acl-compat-mp)
  (:export 
   #:*current-process*         ;*
   #:process-kill              ;*
   #:process-preset            ;*
   #:process-name              ;*

   #:process-wait-function
   #:process-run-reasons 
   #:process-arrest-reasons
   #:process-whostate
   #:without-interrupts
   #:process-wait
   #:process-enable
   #:process-disable
   #:process-reset
   #:process-interrupt

   #:process-run-function      ;*
   #:process-property-list     ;*
   #:without-scheduling        ;*
   #:process-allow-schedule    ;*
   #:make-process              ;*
   #:process-add-run-reason    ;*
   #:process-revoke-run-reason ;*
   #:process-add-arrest-reason    ;*
   #:process-revoke-arrest-reason ;*
   #:process-allow-schedule    ;*
   #:with-timeout              ;*
   #:make-process-lock         ;*
   #:with-process-lock         ;*

   #:current-process
   #:process-name-to-process
   #:process-wait-with-timeout
   #:wait-for-input-available
   ))

(defpackage :de.dataheaven.chunked-stream-mixin
  (:use :common-lisp)
  (:export #:chunked-stream-mixin
           #:output-chunking-p #:input-chunking-p))

;; general
(defpackage acl-compat.socket
  (:use #:common-lisp
        #+(or cmu lispworks scl) #:acl-mp
        #+(or lispworks cmu)#:excl
        #+clisp #:socket
        #+sbcl #:sockets
        #+(or lispworks cmu) #:de.dataheaven.chunked-stream-mixin
        )
  #+cl-ssl (:import-from :ssl #:MAKE-SSL-CLIENT-STREAM #:MAKE-SSL-SERVER-STREAM)
  #+lispworks (:shadow socket-stream stream-error)
  (:export
   #+(or lispworks cmu) #:socket
   #:make-socket
   #:accept-connection
   #:ipaddr-to-dotted
   #:dotted-to-ipaddr
   #:ipaddr-to-hostname
   #:lookup-hostname
   #:remote-host
   #:remote-port
   #:local-host
   #:local-port
   #:socket-control
   #+cl-ssl #:make-ssl-client-stream
   #+cl-ssl #:make-ssl-server-stream
   #+lispworks #:socket-os-fd
   )
  (:nicknames #-(or clisp allegro) socket #-allegro acl-socket))


(defpackage acl-compat.system
  #+nil (:nicknames :sys :system)
  (:use :common-lisp) 
  (:export
   #:command-line-arguments
   #:command-line-argument
   #:reap-os-subprocess
   ))


(defpackage :gray-stream
  (:use #:common-lisp)
  (:import-from #+lispworks :stream #+cmu :lisp #+clisp :gray
                #+(or mcl openmcl) :ccl #+allegro :excl #+sbcl :sb-gray
                #:fundamental-binary-input-stream
                #:fundamental-binary-output-stream
                #:fundamental-character-input-stream
                #:fundamental-character-output-stream
                #:stream-element-type
                #:stream-listen
                #:stream-read-byte
                #:stream-read-char
                #:stream-peek-char
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
   #-clisp #:stream-read-sequence
   #:stream-unread-char
   #:stream-read-line
   #-clisp #:stream-write-sequence
   #:stream-write-string
   #:stream-write-buffer
   #:stream-read-buffer
   #:stream-fill-buffer
   #:stream-flush-buffer
   #:with-stream-input-buffer
   #:with-stream-output-buffer))
