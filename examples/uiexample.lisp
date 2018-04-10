(require :branchie)
(defpackage :uiexample
  (:use :cl :branchie-tiny-ui :branchie-core))

(in-package :uiexample)

(defparameter *root-branch* (br "Hello world!" :name 'helloworld :code (lambda (b inp)
                                                                         (declare (ignore b) (ignore inp))
                                                                         nil)))

(defun main ()
  (GUI 960 480 
       :on-init (lambda ()
                  (set-textarea-text (get-textarea) "Press any key."))
       :on-key (lambda (key)
                 (cond
                   ((string= key "ESCAPE") (quit-gui))
                   ((string= key "RETURN") 
                    (set-textarea-text (get-textarea) (branch-text *root-branch*)))
                   (t 
                    (set-textarea-text (get-textarea) (format nil "You pressed the ~a key" key)))))))
(main)
