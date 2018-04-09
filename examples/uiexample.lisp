(require :branchie)
(defpackage :uiexample
  (:use :cl :branchie-tiny-ui :branchie-core))

(in-package :uiexample)

(defparameter *root-branch* (br "Hello world!" :name 'helloworld :code (lambda (b inp)
                                                                         (declare (ignore b) (ignore inp))
                                                                         nil)))

(defun main ()
  (GUI 960 480 :key-handler (lambda (key)
                              (if (string= (string key) "RETURN")
                                (set-textbox-text (get-textbox) (branch-text *root-branch*))
                                (set-textbox-text (get-textbox) (format nil "You pressed the ~a key" key))))))
