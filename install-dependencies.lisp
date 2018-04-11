#!/usr/local/bin/sbcl --script
(write-line "Loading runtime configuration...")
(load "~/.sbclrc")
(if (find-package :ql)
  (progn
    (write-line "Installing dependencies...")
    (ql:quickload "cl-conspack")
    (ql:quickload "harmony-simple")
    (ql:quickload "harmony-pulse")
    (write-line "Finished."))
  (progn
    (write-line "Please install quicklisp first before running this script.")))
