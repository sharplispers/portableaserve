;;; Load Allegro's packaged aserve via asdf.

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve))

