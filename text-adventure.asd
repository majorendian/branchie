(defpackage text-adventure-system
  (:use :cl :asdf))

(in-package :text-adventure-system)
;(ql:quickload "ltk")
;(ql:quickload "ltk-mw")
;(ql:quickload :harmony-simple)
;(use-package :ltk)

(defsystem "text-adventure"
  :description "Text adventure game engine"
  :depends-on (:ltk :ltk-mw :harmony-simple :harmony-pulse :cl-conspack)
  :version "0.1"
  :author "Ernest Deák <gordon.zar@gmail.com>"
  :license "BSD 2-Clause License"
  :components ((:file "text-adventure-ext-classes")
               (:file "text-adventure" :depends-on ("text-adventure-ext-classes"))
               (:file "text-adventure-ext-systems" :depends-on ("text-adventure-ext-classes" "text-adventure"))))
