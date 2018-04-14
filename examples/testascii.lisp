(require :branchie)

(defpackage :testascii
  (:use :cl :branchie-core :branchie-eta))

(in-package :testascii)

(defparameter *startbranch* (br "Welcome to an ascii map"))

(defun run-game ()
  (eta-loop *startbranch*))
