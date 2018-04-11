(require :branchie)
(defpackage :textadventure-example
  (:use :cl :branchie-ta :branchie-core :branchie-utils))

(in-package :textadventure-example)

(defparameter *adventure-branch*
  (br (join-nl "Welcome to yet another test game."
               "To play the game, use your keypad or enter a number to select an option.")
      :name 'start-branch
      :options (list
                 (list "First option" (br ""
                                          :code (lambda (b inp)
                                                  (setf (branch-text b) "This text has been set by the :code of the branch."))
                                          :options (list
                                                     (list "Back" 'start-branch))))
                 (list "Second option" (br "You selected the second option."
                                           :code (lambda (b inp)
                                                   (declare (ignore inp))
                                                   (setf (branch-options b)
                                                         (list
                                                           (list "Back (set by :code of branch)" 'start-branch))))))
                 (list "Third option" (br "Third option selected"
                                          :code (lambda (b inp)
                                                  (declare (ignore inp))
                                                  (if (get-glob-var 'option-three-visited)
                                                    (setf (branch-text b) "You have already checked this option.")
                                                    (set-glob-var 'option-three-visited t)))
                                          :options (list
                                                     (list "Back" 'start-branch))))
                 (list "Fourth option" (br "Fourth option selected"
                                           :code (lambda (b inp)
                                                   (if (get-glob-var 'option-three-visited)
                                                     (setf (branch-text b) "Check option three again if you haven't double-checked.")))
                                           :options (list
                                                      (list "Back" 'start-branch))))
                 (list "Fifth option" (br "Just a plain old normal branch."
                                          :options (list
                                                     (list "Back" 'start-branch))))
                 (list "Sixth option" (br "Press enter to advance the text"
                                          :next (br "You reached this branch without any options. Press enter to go back to the start"
                                                    :next 'start-branch)))
                 (list "Seventh option" (br "Press enter to go back to the start."
                                            :next 'start-branch)))))

(ta-loop 960 480 *adventure-branch*)
