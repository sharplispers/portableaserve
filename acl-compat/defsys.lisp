(in-package "CL-USER")

(defsystem "ACL-COMPAT"
  (:default-pathname "ACL-COMPAT:")
  :members
  ("nregex"
   "acl-mp-lw"
   "acl-socket-lw"
   "acl-excl-lw"
   "acl-sys-lw"
   "meta"
   "uri"
   "acl-sys-lw")

  :rules
  ((:in-order-to :compile "acl-excl-lw"
    (:caused-by (:compile "nregex"))
    (:requires (:load "nregex")))
 
  (:in-order-to :compile "uri"
    (:caused-by (:compile "meta"))
     (:requires (:load "meta")))))
