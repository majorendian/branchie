
(defpackage :branchie-core
  (:use :cl :cl-conspack)
  (:export branch
           branch-options
           branch-text
           branch-code
           branch-name
           br
           process-branch
           process-selection
           advance-branch
           term-loop
           quit
           set-glob-var
           get-glob-var
           save-game-state
           load-game-state
           ))

(in-package :branchie-core)

(defparameter *debug* t)
(defun format-dbg (stdout format_string &rest r)
  (when *debug*
    (apply #'format stdout format_string r)))

(defparameter *playing* t)
(defun quit (&optional b input)
  (declare (ignore b) (ignore input))
  nil)

(defparameter *game-state* (make-hash-table))

(defun set-glob-var (varname value)
  "Set a global variable with name VARNAME and value VALUE."
  (setf (gethash varname *game-state*) value))

(defun get-glob-var (varname)
  "Get the value of a global variable with VARNAME."
  (gethash varname *game-state*))

(defun save-game-state (cur_b &optional (save_name *gametitle*))
  ;(setf (gethash 'current-branch *global-game-state*) branch_name)
  (with-open-file (str (savefile save_name)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))

    (write-sequence (conspack:encode *game-state*) str)))

(defun load-game-state (&optional (save_name *gametitle*))
  (with-open-file (str (savefile save_name)
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (let ((save_state_array (make-array (file-length str) :element-type '(unsigned-byte 8))))
      (read-sequence save_state_array str)
      (let ((loaded_game_state_hash (conspack:decode save_state_array)))
        (setf *game-state* (copy-hash-table loaded_game_state_hash))
        ))))

;The branch table is responsible for keeping track of branches
;for cross-referencing
(defparameter *branch-table* (make-hash-table))

(defclass branch ()
  ((options :accessor branch-options :initarg :options :initform (list))
   (text :accessor branch-text :initarg :text :initform "...")
   (code :accessor branch-code :initarg :code :initform (lambda (b input) (declare (ignore b) (ignore input)) t))
   (name :accessor branch-name :initarg :name :initform 'default-branch-name)))

(defun br (text &key 
                (options nil) 
                (name 'default-branch-name) 
                (code (lambda (b input) (declare (ignore b) (ignore input)) t)))
  (let ((b (make-instance 'branch
                          :text text 
                          :options options
                          :name name
                          :code code)))
    (setf (gethash name *branch-table*) b)))

(defun print-branch-to-terminal (b)
  (format t "============================~%~a~%============================~%" (branch-text b)))

(defun print-branch-options-to-terminal (b)
  (loop for opt-pair in (branch-options b)
        for i = 0 then (+ i 1)
        do (format t "~a] ~a~%" i (first opt-pair)))
  (format t "> ")
  (force-output))

(defmethod process-selection ((cur_b branch) (input_string string))
  (cond
    ((string= "" input_string) cur_b)
    (t (handler-case
         (let* ((choice_index (parse-integer input_string))
                (selected_branch (second (nth choice_index (branch-options cur_b)))))
           (format-dbg t "selected:~S~%" selected_branch)
           (cond
             ((symbolp selected_branch) (gethash selected_branch *branch-table*))
             ((typep selected_branch 'branch) selected_branch)))
         (sb-int:simple-parse-error () cur_b)))))

(defmethod process-selection ((cur_b branch) (selection fixnum))
  (process-selection cur_b (write-to-string selection)))

(defmethod process-selection ((cur_b branch) anything_else)
  nil)

(defun advance-branch (cur_b selection)
  (funcall (branch-code cur_b) cur_b selection)
  (process-selection cur_b selection))

(defun process-branch (active_branch &key 
                                (get-user-input #'read-line) 
                                (render-branch #'print-branch-to-terminal)
                                (render-options #'print-branch-options-to-terminal))

  (format-dbg t "b:~S~%" (branch-name active_branch))
  (funcall render-branch active_branch)
  (funcall render-options active_branch)
  (let ((userinput (funcall get-user-input)))
    (if (funcall (branch-code active_branch) active_branch userinput)
      (process-selection active_branch userinput)
      nil)))

(defun term-loop (active-branch)
  (loop while active-branch
        do (progn
             (setf active-branch (process-branch active-branch)))))

