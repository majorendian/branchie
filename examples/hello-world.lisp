(require :branchie)
(defpackage :helloworld
  (:use :cl :branchie-core))

(in-package :helloworld)

(br "Press enter to exit." :name 'quit-branch :code #'quit)

(term-loop (br "Hello world!"
               :name 'start-branch
               :options (list
                          (list "Hello program!" (br "You said hello to the program."
                                                     :code (lambda (current_branch userinput)
                                                             (declare (ignore current_branch) (ignore userinput))
                                                             nil)))
                          (list "Exit" 'quit-branch))))
