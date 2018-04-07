(defpackage branchie-system 
  (:use :cl :asdf))

(in-package :branchie-system)
;(ql:quickload "ltk")
;(ql:quickload "ltk-mw")
;(ql:quickload :harmony-simple)
;(use-package :ltk)

(defsystem "branchie"
  :description "Text adventure game engine"
  :depends-on (:ltk :ltk-mw :harmony-simple :harmony-pulse :cl-conspack)
  :version "0.1"
  :author "Ernest Deák <gordon.zar@gmail.com>"
  :license "BSD 2-Clause License"
  :components ((:file "branchie-classes")
               (:file "branchie-core")
               (:file "branchie-utils")
               (:file "branchie-sound")
               (:file "branchie" :depends-on ("branchie-classes" "branchie-ui" "branchie-utils"))
               (:file "branchie-systems" :depends-on ("branchie-classes" "branchie"))
               (:file "branchie-ui")))
