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
   ((:in-order-to :compile "macs"
		  (:caused-by (:compile "htmlgen"))
		  (:requires (:load "htmlgen")))
    (:in-order-to :compile "main"
		  (:caused-by (:compile "macs"))
		  (:requires (:load "macs")))
    (:in-order-to :compile "headers"
		  (:caused-by (:compile "main"))
		  (:requires (:load "main")))
    (:in-order-to :compile "parse"
		  (:caused-by (:compile "headers"))
		  (:requires (:load "headers")))
    (:in-order-to :compile "decode"
		  (:caused-by (:compile "parse"))
		  (:requires (:load "parse")))
    (:in-order-to :compile "publish"
		  (:caused-by (:compile "decode"))
		  (:requires (:load "decode")))
    (:in-order-to :compile "authorize"
		  (:caused-by (:compile "publish"))
		  (:requires (:load "publish")))
    (:in-order-to :compile "log"
		  (:caused-by (:compile "authorize"))
		  (:requires (:load "authorize")))
    (:in-order-to :compile "client"
		  (:caused-by (:compile "log"))
		  (:requires (:load "log")))
    (:in-order-to :compile "proxy"
		  (:caused-by (:compile "client"))
		  (:requires (:load "client")))))
