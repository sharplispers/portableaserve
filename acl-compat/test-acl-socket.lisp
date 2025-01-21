;;; Unit tests for the ACL-SOCKET compatibility package.

;;;---------------------------------------------------------------------------
;;; These tests all seem to assume that *something* is listening on
;;; port 2500, but when I try to run these, I don't have anything listening
;;; there.  Judging from the "helo" command, it looks like this was expecting
;;; TELNET-compatible service, but no docs to explain what would be there.

;;; I have updated the tests to use a publicly-available echo server,
;;; tcpbin.com
;;;---------------------------------------------------------------------------


(in-package cl-user)

(defpackage acl-socket-tests
  (:use :common-lisp :acl-socket :fiveam))

(in-package :acl-socket-tests)

(def-suite* acl-socket-tests)

(defparameter +tcpbin-host+ "tcpbin.com")
(defconstant +tcpbin-port+ 4242)

(test test1
  (let ((stream (make-socket :connect :active :remote-host +tcpbin-host+ :remote-port +tcpbin-port+)))
    (unwind-protect
     (progn
       (is-true stream)
       (format stream "helo foo")
       (write-char #\Return stream)
       (write-char #\Linefeed stream)
       (finish-output stream)
       (let ((read
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (read-line stream))))
         (format t "~&Response to \"helo\":~%~t~a~%" read)
         (is (equal "helo foo" read))))
     (close stream))))


;;;---------------------------------------------------------------------------
;;; Chunking does not work (yet ?)
;;;---------------------------------------------------------------------------

#|
(test test2
  (let ((stream (make-socket :connect :active :remote-host +tcpbin-host+ :remote-port +tcpbin-port+)))
    (is-true stream)
    (socket-control stream :output-chunking t)
    (read-line stream)
    (format stream "helo foo")
    (write-char #\Return stream)
    (write-char #\Linefeed stream)
    (finish-output stream)
    (read-line stream)
    (close stream)))

(test test3
  (let ((stream (make-socket :connect :active :remote-host +tcpbin-host+ :remote-port +tcpbin-port+)))
    (is-true stream)
    (socket-control stream :input-chunking t)
    (prog1
        (read-line stream)
      (close stream))))

(test test4
  (let ((stream (make-socket :connect :active :remote-host +tcpbin-host+ :remote-port +tcpbin-port+)))
    (is-true stream)
    (socket-control stream :input-chunking t)
    (format t "File number 1: ")
    #1=(handler-case
	   (loop
	    for char = (read-char stream nil stream)
	    until (eq char stream)
	    do (write-char char))
	 (excl::socket-chunking-end-of-file (e) (socket-control stream :input-chunking t)))
    (format t "~%File number 2: ")
    #1#
    (terpri)
    (values)))

|#

#+sbcl
(test filter-args
     (is (equalp '(:input t) (acl-compat.socket::filter-args '(:input t :ooble nil) '(:input)))))
