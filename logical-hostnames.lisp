;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-

(in-package "CL-USER")

(setf (logical-pathname-translations "ACL-COMPAT")
      `(("*.*.*" 
         ;; e.g., "/home/jsc/lisp/src/aserve/acl-compat-lw/" 
         ,(merge-pathnames 
           (make-pathname :host (pathname-host *load-truename*)
                          :directory '(:relative "acl-compat")
                          :name    :wild
                          :type    :wild
                          :version :wild
;                          :case :common
)
           *load-truename*))))

#+lispworks
(setf (logical-pathname-translations "CL-SSL")
      `(("*.*.*" 
         ;; e.g., "/home/jsc/lisp/src/aserve/acl-compat-lw/cl-ssl/" 
         ,(merge-pathnames 
           (make-pathname :host (pathname-host *load-truename*)
                          :directory '(:relative "acl-compat-lw" "cl-ssl")
                          :name    :wild
                          :type    :wild
                          :version :wild
;                          :case :common
)
           *load-truename*))))


(setf (logical-pathname-translations "ASERVE")
      `(("**;*.lisp.*" ;,(logical-pathname "**;*.cl.*")
         ,(make-pathname :host (pathname-host *load-truename*)
                         :directory '(:relative "aserve" 
                                                 :wild-inferiors)
                         :name :wild
                         :type "cl"
                         :version :wild)
         )
        ("**;*.*.*" 
         ;; e.g., "/home/jsc/lisp/src/aserve/aserve-lw/**/"
         ,(merge-pathnames 
           (make-pathname :host (pathname-host *load-truename*)
                          :directory '(:relative "aserve" 
                                                 :wild-inferiors)
                          :name    :wild
                          :type    :wild
                          :version :wild
                          ;:case :common
                          )
           *load-truename*))))
