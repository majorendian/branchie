(require :branchie)

(defpackage :bc-example-1
  (:use :cl :branchie-core))

(in-package :bc-example-1)

(defun testgame ()
  (macrolet ((with-default-options (&rest l)
               `(list
                  ,@l
                  (list "Main menu" 'start-branch)
                  (list "Quit" 'quit-branch))))
    (term-loop (br "Once upon a time, there was a terminal on a computer screen."
                      :name 'start-branch
                      :options (list 
                                 (list "Interact"
                                       (br "You interacted with the terminal. A small game came out of it. How magical!"
                                           :options (list
                                                      (list "Play the game"
                                                            (br "You are playing it. This is the game."
                                                                :options (with-default-options
                                                                           (list "Try something else"
                                                                                 (br (format nil "You press some buttons on the keyboard.~%Nothing else much happens.")
                                                                                     :options (with-default-options))))))
                                                      (list "Quit" 'quit-branch))))
                                 (list "Type something"
                                       (br "Start typing and then press enter."
                                           :code (lambda (b input)
                                                   (setf (branch-text b) (format nil "You typed:~S" input)))
                                           :options (with-default-options)))
                                 (list "Quit" 
                                       (br (format nil "Quiting...~%Press enter to exit the program")
                                           :name 'quit-branch
                                           :code #'quit)))))))
