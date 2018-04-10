(require :branchie)
(defpackage :raw-ui-helloworld
  (:use :cl 
        :branchie-tiny-ui
        :branchie-core
        :branchie-utils
        :ltk))

(in-package :raw-ui-helloworld)

(br "" :name 'quit-program :code (lambda (b inp) 
                                   (declare (ignore b) (ignore inp))
                                   (format t "Quiting~%")
                                   (quit-gui)))
(defparameter *root-branch* (br "Hello world!"
                                :name 'helloworld 
                                :code (lambda (b inp)
                                        (declare (ignore b) (ignore inp))
                                        )
                                :options (list
                                           (list "Hello program!" (br "You greeted the program."
                                                                      :options (list
                                                                                 (list "Quit" 'quit-program))))
                                           (list "Goodbye program!" (br "Goodbye world!"
                                                                        :options (list
                                                                                   (list "Quit" 'quit-program)))))))


(defun display-branch-options (canvas branch)
  (draw-text canvas 20 100 (join #\Newline (loop for i = 1 then (+ i 1)
                                                 for option-text in (map 'list (lambda (pair) (first pair)) (branch-options branch))
                                                 collect (format nil "~a] ~a" i option-text)))))

(defparameter *somex* 0)
(defparameter *somey* 200)

(defun main ()
  (defparameter *current-branch* *root-branch*)
  (gui 960 480 
       :on-init (lambda ()
                  (set-textarea-text (get-textarea) (branch-text *root-branch*)) 
                  (display-branch-options (get-canvas) *current-branch*)
                  )
       :on-update (lambda ()
                    ;(format t "updating~%")
                    (cond
                      ((> *somex* 350) (setf *somey* 200))
                      ((> *somex* 300) (setf *somey* (- *somey* 3)))
                      ((> *somex* 200) (setf *somey* (+ 3 *somey*)))
                      ((> *somex* 150) (setf *somey* (- *somey* 3)))
                      ((> *somex* 100) (setf *somey* 200)))
                    (format-wish "~a create text ~a ~a -fill white -text l"
                                 (widget-path (get-canvas))
                                 *somex* *somey*)
                    (setf *somex* (+ 1 *somex*))
                    )
       :on-key (lambda (key)
                      (cond
                        ((string= key "ESCAPE") (quit-gui))
                        (t 
                         (let* ((selection (first (member (string key) (list "1" "2" "3" "4" "5" "6" "7" "8" "9") :test #'string=)))
                                (next_branch (when selection
                                               (process-selection *current-branch* (- (parse-integer selection) 1)))))
                           (when next_branch
                             (progn
                               (funcall (branch-code next_branch) next_branch key)
                               (clear-canvas (get-canvas))
                               (draw-textarea (get-canvas) (get-textarea))
                               (set-textarea-text (get-textarea) (branch-text next_branch) nil 50)
                               (display-branch-options (get-canvas) next_branch)
                               (setf *current-branch* next_branch))))
                         )))
       :update-interval 0.005))

(main)
