(in-package "CL-USER")

(defsystem "ASERVE"
  (:default-pathname "ASERVE:")
  :members
   ("./htmlgen/htmlgen"
    "macs"
    "main"
    "headers"
    "parse"
    "decode"
    "publish"
    "authorize"
    "log"
    "client"
    "proxy"
    )
  :rules
   ((:in-order-to :compile "main"
    (:caused-by (:compile "macs"))
    (:requires (:load "macs")))
   (:in-order-to :compile "headers"
    (:caused-by (:compile "main"))
    (:requires (:load "main")))
   (:in-order-to :compile "parse"
    (:caused-by (:compile "main"))
    (:requires (:load "main")))
   (:in-order-to :compile "decode"
    (:caused-by (:compile "main"))
    (:requires (:load "main")))
   (:in-order-to :compile "publish"
    (:caused-by (:compile "main"))
    (:requires (:load "htmlgen")))
   (:in-order-to :compile "authorize"
    (:caused-by (:compile "main"))
    (:requires (:load "main")))
   (:in-order-to :compile "log"
    (:caused-by (:compile "main"))
    (:requires (:load "main")))
   (:in-order-to :compile "client"
    (:caused-by (:compile "main"))
    (:requires (:load "main")))
   (:in-order-to :compile "proxy"
    (:caused-by (:compile "main"))
    (:requires (:load "main")))))
