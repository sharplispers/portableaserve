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
   "chunked")

  :rules
  ((:in-order-to :compile "acl-excl-lw"
    (:caused-by (:compile "nregex"))
    (:requires (:load "nregex")))

   (:in-order-to :load "acl-excl-lw"
    (:requires (:load "nregex")))

   (:in-order-to :compile "acl-socket-lw"
    (:requires (:load "chunked")))

   (:in-order-to :compile "chunked"
    (:requires (:load "acl-excl-lw")))
 
  (:in-order-to :compile "uri"
    (:caused-by (:compile "meta"))
     (:requires (:load "meta")))))
