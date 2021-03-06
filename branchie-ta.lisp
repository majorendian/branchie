(defpackage :branchie-ta
  (:use :cl 
        :branchie-core
        :branchie-tiny-ui
        :branchie-utils
        :ltk)
  (:export ta-loop
           *text-speed*
           *process-input-key*
           image-canvas))
(in-package :branchie-ta)

(defparameter *text-speed* 100)
(defparameter *process-input-key* t)

(defun init-keymap ()
  (let ((keymap (make-hash-table)))
    (setf (gethash (intern "KP_1") keymap) 0)
    (setf (gethash (intern "KP_2") keymap) 1)
    (setf (gethash (intern "KP_3") keymap) 2)
    (setf (gethash (intern "KP_4") keymap) 3)
    (setf (gethash (intern "KP_5") keymap) 4)
    (setf (gethash (intern "KP_6") keymap) 5)
    (setf (gethash (intern "KP_7") keymap) 6)
    (setf (gethash (intern "KP_8") keymap) 7)
    (setf (gethash (intern "KP_9") keymap) 8)
    (setf (gethash 1 keymap) 0)
    (setf (gethash 2 keymap) 1)
    (setf (gethash 3 keymap) 2)
    (setf (gethash 4 keymap) 3)
    (setf (gethash 5 keymap) 4)
    (setf (gethash 6 keymap) 5)
    (setf (gethash 7 keymap) 6)
    (setf (gethash 8 keymap) 7)
    (setf (gethash 9 keymap) 8)
    keymap))

(defun lookup-key (keymap keystring)
  (let ((r (gethash (intern (string keystring)) keymap)))
    (if r
      r
      (gethash (parse-integer keystring :junk-allowed t) keymap))))

(defun display-branch-options (branch)
  (draw-text (get-canvas)
             (floor (/ (parse-integer (cget (get-canvas) :width)) 8))
             (floor (/ (parse-integer (cget (get-canvas) :height)) 2))
             (join #\Newline (loop for i = 1 then (+ i 1)
                                   for option-text in (map 'list (lambda (pair) (first pair)) (branch-options branch))
                                   collect (format nil "~a] ~a" i option-text)))))


(defun display-branch-options-bottom (branch)
  (draw-text (get-canvas)
             (floor (/ (parse-integer (cget (get-canvas) :width)) 2))
             (floor (/ (parse-integer (cget (get-canvas) :height)) 1.05))
             (join "   " (loop for i = 1 then (+ i 1)
                                   for option-text in (map 'list (lambda (pair) (first pair)) (branch-options branch))
                                   collect (format nil "~a] ~a" i option-text)))
             :anchor :center))

(defun display-branch-options-right (branch)
  (draw-text (get-canvas)
             (floor (/ (parse-integer (cget (get-canvas) :width)) 1.40))
             (floor (/ (parse-integer (cget (get-canvas) :height)) 7))
             (join #\Newline (loop for i = 1 then (+ i 1)
                                   for option-text in (map 'list (lambda (pair) (first pair)) (branch-options branch))
                                   collect (format nil "~a] ~a" i option-text)))
             :anchor :nw))

(defun display-branch-text (cur_b)
  (set-textarea-text (get-textarea) (branch-text cur_b) nil *text-speed*))


(defun init-elements ()
  (clear-canvas (get-canvas))
  (draw-textarea (get-canvas) (get-textarea)
                 (floor (/ (parse-integer (cget (get-canvas) :width)) 25))
                 (floor (/ (parse-integer (cget (get-canvas) :height)) 10))
                 :anchor :nw)
  (configure (textbox (get-textarea))
             :width (floor (/ (parse-integer (cget (get-canvas) :width)) 15))
             :height (floor (/ (parse-integer (cget (get-canvas) :height)) 25))))

(defun display-branch (abranch key)
  (format-wish "~a delete text" (widget-path (get-canvas)))
  (funcall (branch-code abranch) abranch key)
  (display-branch-text abranch)
  (display-branch-options-right abranch))

(defun handle-key (key keymap active_branch 
                       &key
                       (on-branch-jump (lambda (b) (declare (ignore b)))))
  (let ((next_branch (process-selection active_branch (lookup-key keymap key))))
    (if next_branch
      (progn
        (display-branch next_branch key)
        (funcall on-branch-jump next_branch)
        (setf active_branch next_branch))
      (if (and
            (branch-next active_branch)
            (or (string= "RETURN" key) (string= "KP_ENTER" key)))
        (progn
          (format t "branch-next: ~S~%" (branch-next active_branch))
          (cond
            ((symbolp (branch-next active_branch)) (setf next_branch (lookup-branch-name (branch-next active_branch))))
            ((typep (branch-next active_branch) 'branch) (setf next_branch (branch-next active_branch)))
            (t (error "Invalid type for :next keyword")))
          (display-branch next_branch key)
          (funcall on-branch-jump next_branch)
          next_branch)
        active_branch))))

(defun ta-loop (width height start_branch &key
                      (on-pre-init (lambda ()))
                      (on-post-init (lambda ()))
                      (on-update nil)
                      (on-key (lambda (keystring) (declare (ignore keystring))))
                      (on-branch-jump (lambda (b) (declare (ignore b))))
                      (text-speed 100))
  (let ((active_branch start_branch)
        (keymap (init-keymap)))
    (setf *text-speed* text-speed)
    (gui width height
         :on-init (lambda ()
                    (funcall on-pre-init)
                    (init-elements)
                    (funcall on-post-init)
                    (funcall (branch-code active_branch) active_branch "") 
                    (display-branch-text active_branch)
                    (display-branch-options-right active_branch))
         :on-update on-update
         :on-key (lambda (key)
                   (funcall on-key key)
                   (when *process-input-key*
                     (setf active_branch (handle-key key keymap active_branch)))))))
