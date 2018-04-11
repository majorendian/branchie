(defpackage :branchie
  (:use :cl :ltk :ltk-mw 
            :branchie-classes
            :branchie-ui
            :branchie-utils
            :branchie-sound)
  (:export br 
           opt 
           choices 
           branch
           opt-pair
           create-branch
           make-opt-pair 
           start-game
           available-options-of

           set-branch-text 
           get-branch-text
           set-branch-var 
           get-branch-var
           set-branch-actors
           get-branch-actors
           reset-game-state
           save-game-state
           load-game-state
           savefile
           
           set-branch-next
           set-branch-choices 
           add-branch-opt
           remove-nth-branch-opt 
           set-branch-opt
           set-glob-var get-glob-var 
           action
           switch-root-branch
           clear-branch-stack
           reset-all-branches
           goto
           root-branch
           set-gold-theme
           set-dark-theme
           ;helpfull functions
           join
           join-nl
           ;other configurations
           set-debug
           ;aux
           text-entry-autocomplete
           )
  (:shadow withdraw)
  )

(in-package :branchie)
(use-package :ltk) ;need this here so that all the symbols get imported. Otherwise we have for some reason name conflicts

#+:linux (asdf:load-system :harmony-pulse)

;(setf ltk:*wish-pathname* "/home/tino/Lab/Binaries/IronTcl/bin/wine64-wish.sh")


(defstruct opt-pair 
  (opt-text nil) ;choice text to matcha against when processing user input
  (opt-branch)) ;actual branch in question associated with the option text

(defstruct branch 
  (name nil) ;branch id
  (next nil)
  (text nil) ;text to display on the branch
  (text-options nil) ;options for the branch, created by macro "choices"
  (br-func nil) ;function to execute when entering the branch
  (images (list)) ;image associated with this branch
  )

(defparameter *start-branch* nil) ;starting branch for "/reset"
(defun root-branch () *start-branch*)
(defparameter *start-branch-deep-copy* nil)
(defparameter *branch-stack* (list))
(defparameter *branch-state-refs* (make-hash-table))
;The *branch-state-refs* variable is used for storing
;references to branches. These are then used during save/load
;operation to restore the previous state of the referenced branches
(defmethod branch-ref-add ((b branch))
  (setf (gethash (slot-value b 'name) *branch-state-refs*) b))

(defmethod branch-ref ((branch_name symbol))
  (gethash branch_name *branch-state-refs*))

(defmethod load-branch-from-ref ((branch_name symbol)))

(defparameter *commands-for-autocomplete* (list))
;(defparameter *current-branch-image* nil) ;last image drawn on the screen for a redraw option
;(defparameter *current-branch-imagepath* nil)
(defparameter *text-adventure-debug* nil)
(defun set-debug (v)
  (setf *text-adventure-debug* v))


;global table of branches
(defparameter *branch-table* (make-hash-table))
;The branch table is a hash table with names of branches as keys
;branches are created during their visitation due to lazy evaluation
;this means that even though a branch might be defined in the
;script, it might not yet be present in the branch table
;This is because the branches get deeply nested so the
;inner branches wouldn't be able to reference the outer containing branch.
;COMPILE-GAME-TREE is used to evaluate all branches currently accessible.
;Any (BR ...) statement adds the branch to the branch table.

(defmethod register-branch ((b branch))
  (setf (gethash (slot-value b 'name) *branch-table*) b))

(defmethod retrieve-branch (symb)
  (gethash symb *branch-table*))

(define-condition branch-not-found-error (error)
  ((message 
     :initarg :message 
     :accessor bnf-error-message 
     :initform "The requested branch was not found")
   (bname
     :initarg branch-name
     :accessor bnf-error-branch-name
     :initform nil
     :documentation "The name of the branch")))

(defun branch-not-found-error (branch_name)
  (error 'branch-not-found-error
         :message (format nil "The requested branch was not found:~a" branch_name)
         :bname branch_name))

;--- Configurable variables ---


(defparameter *gametitle* "untitled")

;------------------------------

;--- helper functions ---------
(defparameter *branch-label-counter* -1)
(defun generate-branch-label ()
  (setf *branch-label-counter* (+ 1 *branch-label-counter*))
  (format nil "BR~a" *branch-label-counter*))

;-------------------------------

;==== Global game state table ====
(defparameter *global-game-state* (make-hash-table))
;The table is used in conjunction with the game script related functions
;this is to simplify the save/load mechanism
;conspack:encode and conspack:decode are used to save/load the this variable
;Scripts that define and store custom calsses into this table must call
;(conspack:defencoding some-class
;   slot-1 slot-2 slot-4)
;in order for them to be passed to conspack:encode/conspack:decode

;==== Game script related functions ====
;These functions are exported and used in game scripts
;for control such as setting variables, branch texts and options, etc.

(defun set-branch-var (branch varname value)
  (declare (type branch branch))
  "Set a variable for the specified BRANCH.
   VARNAME is the name of the variable with value VALUE."
  (setf (gethash (intern (format nil "~a_var_~a" (slot-value branch 'name) varname)) *global-game-state*) value))

(defun get-branch-var (branch varname)
  (declare (type branch branch))
  "Get the value of a BRANCH variable specified by VARNAME."
  (gethash (intern (format nil "~a_var_~a" (slot-value branch 'name) varname)) *global-game-state*))

(defun set-glob-var (varname value)
  "Set a global variable with name VARNAME and value VALUE."
  (setf (gethash varname *global-game-state*) value))

(defun get-glob-var (varname)
  "Get the value of a global variable with VARNAME."
  (gethash varname *global-game-state*))

(defun reset-game-state ()
  (setf *global-game-state* (make-hash-table)))

;==================================================


(defun br (text &rest r)
  (declare (type (or list string function) text))
  "Create a new branch with the specified TEXT and a list of option in R.
   Accepts a key argument :ACTION followed by a LAMBDA"
  ;Defines a branch with optional keywords such as
  ; :action (lambda) - runs a function when entering the branch
  (setq r (nconc r nil)) ;makes sure we don't have to type 'nil' at the end of each (br ...) that has no choices
  ;FIXME:
  ;^ The above solution still doesn't work when :action is specified, it takes the function and assumes it is a list
  ;(format t "current form:~S~%" r)
  (let ((_actionf nil)
        (_choices nil)
        (_branch_name nil)
        (_images nil)
        (_next nil)
        (_skip_register nil)
        (i 0))
    (loop for e in r
          ;Old-fashioned index-style loop
          do (progn
               (cond
                 ((eql e :action) (setq _actionf (nth (+ 1 i) r)))
                 ((eql e :name) (setq _branch_name (nth (+ 1 i) r)))
                 ((eql e :images) (setq _images (nth (+ 1 i) r)))
                 ((eql e :next) (setq _next (nth (+ 1 i) r)))
                 ((eql e :skip-register) (setq _skip_register (nth (+ 1 i) r)))
                 ((functionp e) (continue))
                 ((and (listp e) (typep (first e) 'opt-pair)) (setf _choices (macroexpand e))))
               (setq i (+ 1 i))
               ))
    (let ((b
             (make-branch 
               :name (if (not _branch_name) (read-from-string (generate-branch-label)) _branch_name)
               :text (lambda () text)
               ;The IF condition fixes an issue of a required terminating NIL when no choices are provided or only keyword args are supplied
               :text-options (if (functionp _choices) nil _choices) 
               :br-func _actionf
               :images _images
               :next _next ;_next should be a symbol referencing another branch
               )))
      (if (not _skip_register)
        (progn
          (format t "Registering branch:~S~%" (slot-value b 'name))
          (register-branch b)))
       b)))

(defmacro action (&optional arglist &body body)
  (format t "arglist:~S~%" arglist)
  `(lambda (&optional ,@(cond
                          ((not arglist) (list '__cur_b '__userinput))
                          ((= 1 (length arglist)) (list (first arglist) '__userinput))
                          (arglist arglist)))
     ,@(cond 
         ((not arglist) (list `(declare (ignore __cur_b) (ignore __userinput))))
         ((= 1 (length arglist)) (list `(declare (ignore __userinput)))))
     ,@body))

(defmethod br-text ((b branch))
  "Lazy evaluation so that the text can change based on variables within it."
  (funcall (slot-value b 'text)))

(defmethod br-next ((b branch))
  (slot-value b 'next))

(defmethod br-images ((b branch))
  (slot-value b 'images))

(defmethod br-name ((b branch))
  (slot-value b 'name))

;--- opt related ---

(defmacro opt (text branch)
  `(make-opt-pair :opt-text ,text :opt-branch ,branch))

(defun get-branch-of-opt-pair (opt)
  (slot-value opt 'opt-branch))

(defmacro choices (&rest r)
  "Define a list of choices. Simple expands to the LIST function.
   This macro is purely for syntactic sugar."
  `(list ,@r))

;-------------------

(defun set-branch-choices (branch options)
  (declare (type branch branch) (type list options))
  "Set the list of choices in BRANCH. OPTIONS is a list of OPT-PAIRs"
  (setf (slot-value branch 'text-options) options))

(defun set-branch-text (branch text)
  (declare (type branch branch) (type string text))
  "Set the TEXT of the BRANCH."
  (setf (slot-value branch 'text) (lambda () text)))

(defun set-branch-next (branch next)
  (declare (type branch branch))
  "Set the NEXT branch in sequence of the BRANCH."
  (setf (slot-value branch 'next) next))

(defun get-branch-text (branch)
  (declare (type branch branch))
  (br-text branch))

(defun get-branch-actors (branch)
  (declare (type branch branch))
  (slot-value branch 'characters))

(defun set-branch-actors (branch l_o_l)
  "Set the character list which is a list of lists.
   The nested lists are pairs of GAME-CHARACTER and :ANIMATION-SPECIFIER"
  (declare (type branch branch))
  (setf (slot-value branch 'characters) l_o_l))

(defun add-branch-opt (branch option)
  (declare (type branch branch) (type opt-pair option))
  "Add an extra option to BRANCH. OPTION is of type OPT-PAIR
   usualy defined with the macro OPT."
  (push option (slot-value branch 'text-options)))

(defun set-branch-opt (branch choices)
  (declare (type branch branch) (type list choices))
  "Set the list of choices in BRANCH to CHOICES.
   CHOICES is a list of OPT-PAIRs."
  (setf (slot-value branch 'text-options) choices))

;from stackoverflow:
;https://stackoverflow.com/questions/29721699/common-lisp-function-that-deletes-the-element-at-the-n-th-position-of-each-sub-l
;Author: Sylwester
;Profile: https://stackoverflow.com/users/1565698/sylwester
;-----------------------------------------------

(defun remove-nth-branch-opt (n branch)
  (declare (type fixnum n) (type branch branch))
  "Remove the N-th option from the possible options in BRANCH."
  (setf (slot-value branch 'text-options) (remove-nth n (slot-value branch 'text-options))))

;===============================================

(defun search-branch (opts opt_txt_str)
  (declare (type list opts) (type string opt_txt_str))
  "Searches OPTS which is a list of OPT-PAIRs
   for and option that is equal to the text in OPT_TXT_STR"
  (let ((retval nil))
    (if opts
      (progn
        ;(format t "HERE:~S~%" (slot-value (first opts) 'opt-text))
        (if (string-equal (slot-value (first opts) 'opt-text) opt_txt_str)
          (progn
            (setf retval (first opts))
            retval)
          ;else, keep walking the branch
          (search-branch (rest opts) opt_txt_str)))
      retval)))



(defun get-branch-by-name-from-table (branch_name &rest reduntant)
  "Use SEARCH-BRANCH-IN-LIST to lookup a cached branch in the *branch-table*"
  (let ((b (retrieve-branch branch_name)))
    (if b
      b
      (branch-not-found-error branch_name))))

(defun compare-branch-names (b1 b2)
  (string= (string-downcase (format nil "~a" (slot-value b1 'name)))
           (string-downcase (format nil "~a" (slot-value b2 'name)))))


;--- from stackoverflow: https://stackoverflow.com/questions/26045442/copy-hash-table-in-lisp
;Name has been modified
(defun copy-hash-table (table)
  (declare (type hash-table table))
  "Creates a copy of the hash table TABLE"
  (let ((new-table (make-hash-table
                     :test (hash-table-test table)
                     :size (hash-table-size table))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) value))
             table)
    new-table))
;--------------------------------------------------------------------------------------------


(defun deep-copy-branch (branch)
  (declare (type branch branch))
  "Makes a deep copy of the branch BRANCH.
   NOTE: If a branch contains an option that references the branch in which the option
   is defined, it enters an inifite recursion loop. So be careful with that."
  (labels ((clone-choices (l)
             (declare (type list l))
             (if l
               (cons 
                 (opt (slot-value (first l) 'opt-text) (deep-copy-branch (get-branch-of-opt-pair (first l))))
                 (clone-choices (rest l))))))
    (make-branch 
      :name (slot-value branch 'name)
      :text (slot-value branch 'text)
      :text-options (clone-choices (slot-value branch 'text-options))
      :br-func (slot-value branch 'br-func)
      :images (slot-value branch 'images)
      )))


(defun clone-choices (l) ;traverse and copy choices along with their respective branches
;Copied into deep-copy-branch because of circular dependency
  (declare (type list l))
  "Recursively makes a deep copy of the list of options L.
   The options are OPT-PAIRs."
  (if l
    (cons (opt (slot-value (first l) 'opt-text) (deep-copy-branch (get-branch-of-opt-pair (first l)))) (clone-choices (rest l)))))

(defun available-options-of (br)
  (declare (type branch br))
  "Returns a list of options as strings that are available
   for branch BR + options that are available for all branches that
   need to be visible to the player."
  (nconc (loop for opt in (slot-value br 'text-options)
        collect (slot-value opt 'opt-text))))

;==== Stylization ====

(defun core-commands (userinput command-lambda-pairs &key main)
  (declare (type string userinput) (type list command-lambda-pairs) (type function main))
  "Logic for handling built in commands. If command is not found, the MAIN function is executed
   which can be anything."
  ;update the *commands-for-autocomplete* so that we dont have to update it manualy every time
  (setf *commands-for-autocomplete* (loop for (cmd func) in command-lambda-pairs
                                          collect (if *text-adventure-debug*
                                                    cmd
                                                    (return ""))))
  (let ((retval nil))
    (loop for (cmd func) in command-lambda-pairs
          do (when (string-equal cmd userinput)
               (setq retval (funcall func)))
          )
    (if retval
      retval
      (funcall main))))


(defun trigger-branch-action (br &optional userinput)
  (declare (type branch br))
  "Runs the function specified by :ACTION in the
   branch BR.
   Internally it calls the BR-FUNC slot of the branch.
   Returns the same branch as passed in the argument BR"
  (when (slot-value br 'br-func)
    (if userinput
      (funcall (slot-value br 'br-func) br userinput)
      (funcall (slot-value br 'br-func) br)))
  br)

(defun clear-branch-stack ()
  (setf *branch-stack* (list)))


(defun _print_hash (hash)
  (maphash (lambda (k v) (format t "k:~S v:~S~%" k v)) hash))

;=== core branch switching and saving/loading ===
(defun savefile (&optional (save_name *gametitle*))
  (uiop:merge-pathnames* (concatenate 'string save_name ".save")))

(defun save-game-state (cur_b &optional (save_name *gametitle*))
  ;(setf (gethash 'current-branch *global-game-state*) branch_name)
  (with-open-file (str (savefile save_name)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))

    (set-glob-var 'current-branch (br-name cur_b))
    (set-glob-var 'current-images (br-images cur_b))
    (_print_hash *global-game-state*)
    (write-sequence (conspack:encode *global-game-state*) str)))

(defun load-game-state (&optional (save_name *gametitle*))
  (with-open-file (str (savefile save_name)
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (let ((save_state_array (make-array (file-length str) :element-type '(unsigned-byte 8))))
      (read-sequence save_state_array str)
      (let ((loaded_game_state_hash (conspack:decode save_state_array)))
        (setf *global-game-state* (copy-hash-table loaded_game_state_hash))
        (_print_hash *global-game-state*)))))

(defun advance-branch-main (cur_b userinput)
  (let ((found_opt_br (search-branch (slot-value cur_b 'text-options) userinput)))
    (if found_opt_br
      (let ((branch (get-branch-of-opt-pair found_opt_br)))
        (when (not (eq (last *branch-stack*) cur_b))
          (push cur_b *branch-stack*))
        (format t "Starting branch action~%")
        (when (or (stringp branch) (symbolp branch))
          (progn
            (format t "Branch is a reference:~S~%" branch)
            (setq branch (get-branch-by-name-from-table (intern (string-upcase branch))))))
        (branch-ref-add branch)
        (trigger-branch-action branch)
        branch) 
      ;else if branch not found
      (progn
        (format t "Option not found:~S~%" userinput)
        (trigger-branch-action cur_b userinput)
        (if (br-next cur_b)
          (if (symbolp (br-next cur_b))
            (br-next (get-branch-by-name-from-table (br-next cur_b)))
            (br-next cur_b))
          cur_b))
      )))

(defun advance-branch-back ()
  (if *branch-stack*
    (let ((last_branch (pop *branch-stack*)))
      (trigger-branch-action last_branch)
      last_branch)
    (progn
      (trigger-branch-action *start-branch*)
      *start-branch*
      )))

(defun advance-branch (cur_b entry-widget text-widget display)
  (declare (type entry entry-widget) (type scrolled-text text-widget) (type branch cur_b))
  "The 'main game loop' so to speak. Most of the decision making and input processing
   occurs here."
  (let* ((userinput (text entry-widget))
         (cmd (split-string userinput)))
    ;cmd = list of strings. the fist string is the command, the rest argumensts
    (core-commands (first cmd) ;user input
                   (list ;list of commands
                     (list "/help"      (lambda ()
                                          (show-help)
                                          cur_b
                                          ))
                     (list "/exit"      (lambda () (stop-music) (exit-wish)))
                     (list "/quit"      (lambda () (stop-music) (exit-wish)))
                     (list "/restart" (lambda ()
                                        (progn
                                          ;NOTE: This function heavily alters some data due to the nature of the command
                                          ;(setf *start-branch* (deep-copy-branch *start-branch-deep-copy*))
                                          (reset-game-state)
                                          (setf *branch-stack* (list))
                                          (setf cur_b *start-branch*)
                                          cur_b)))
                     (list "/toggle_text_sfx" (lambda () 
                                                (setf *text-sfx-enabled* (not *text-sfx-enabled*))
                                                (setf (text entry-widget) "")  
                                                cur_b))
                     (list "/toggle_music" (lambda ()
                                             (setf *music-enabled* (not *music-enabled*))
                                             (setf (text entry-widget) "")
                                             (if *music-enabled*
                                               (play-music *current-bg-music*)
                                               (stop-music))
                                             cur_b))
                     (list "/wm_mode_fullscreen" (lambda ()
                                                   (send-wish "wm attributes . -fullscreen 1")
                                                   (setf (text entry-widget) "")  
                                                   cur_b))
                     (list "/wm_mode_windowed" (lambda ()
                                                 (send-wish "wm attributes . -fullscreen 0")
                                                 (setf (text entry-widget) "")  
                                                 cur_b))
                     (list "/dbg_goto" (lambda ()
                                         (let ((found_branch (get-branch-by-name-from-table (intern (string-upcase (second cmd))) *start-branch*)))
                                           (if found_branch
                                             (progn
                                               (trigger-branch-action found_branch)
                                               found_branch)
                                             cur_b))
                                         ))
                     (list "/save" (lambda ()
                                     (let ((save_br (br "Saved" :skip-register t)))
                                       (format t "Saving on branch:~S~%" (slot-value cur_b 'name)) 
                                       (save-game-state cur_b)
                                       (set-branch-next save_br (slot-value cur_b 'name))
                                       save_br)))
                     (list "/load"  (lambda ()
                                      (load-game-state)
                                      (br "Game state loaded"
                                          :skip-register t
                                          :next (get-glob-var 'current-branch)
                                          :image (read-from-string (get-glob-var 'current-image)))))
                     (list "/dbg_eval" (lambda ()
                                         ;we split the userinput on spaces so we join it in on spaces
                                         (message-box (eval (read (make-string-input-stream (join " " (rest cmd))))) "EVAL" "ok" "info")
                                         cur_b))
                     (list "" (lambda ()
                                (if (not (available-options-of cur_b))
                                  (if (not (br-next cur_b))
                                    ;vvv --- copied from "back" lambda
                                    (advance-branch-back)
                                    ;else, the branch has a :next, return it
                                    (if (symbolp (br-next cur_b))
                                      (trigger-branch-action (get-branch-by-name-from-table (br-next cur_b)))
                                      (trigger-branch-action (br-next cur_b)))))))

                     (list "/back" #'advance-branch-back))
                   :main (lambda () (advance-branch-main cur_b userinput)))))

;===========================================================


(defun switch-root-branch (new-root-branch)
  (declare (type branch new-root-branch))
  (setf *start-branch* new-root-branch)
  (setf *start-branch-deep-copy* (deep-copy-branch *start-branch*)))

(defun reset-all-branches ()
  "Resets everything. Intended for game restarts and game overs."
  (setf *start-branch* (deep-copy-branch *start-branch-deep-copy*)))

(defun text-entry-autocomplete (options input-str)
  (declare (type list options) (type string input-str))
  "Option autocompletion for the entry input field.
   Autocompletes/Cycles through options of BRANCH given the string INPUT-STR"
  (let* ((i 0)
         (next-opt-flag nil)
         (all_options (concatenate 'list options *commands-for-autocomplete*))
         (result
           (loop for option-text in all_options
                 do (progn
                      (setq i (+ i 1))
                      (cond
                        ((string= (string-downcase option-text) (string-downcase input-str))
                         (progn
                           ;(format t "text equals option:~A=~A~%" option-text input-str)
                           (if (string= option-text (car (last all_options))) ;if this is the last element
                             (return (first all_options)))
                           (setq next-opt-flag t)
                           (return (nth i all_options))))
                        ((handler-case
                           (string= (string-downcase input-str) (string-downcase option-text) :end1 (length input-str) :end2 (length input-str))
                           (sb-kernel:bounding-indices-bad-error () nil))
                         (progn
                           (return option-text)))
                        ((not (string= (string-downcase input-str) (string-downcase option-text)))
                         (progn
                           (if next-opt-flag
                             (return option-text)
                             (continue))))
                        (t (return input-str))) 
                      )
                 )))
    (if result
      result
      input-str))
  )



(defparameter *branch-display-mutex* (sb-thread:make-mutex :name "branch-display-mutex"))

(defun update-main-display (d br)
  (display-render-stack d)
  (display-draw-branch-options d (join " | " (available-options-of br))))

(defun update-panel-display (d br)
  (display-clear d)
  ;(display-draw-image d #p"data/img/tree_ascii.gif" (/ (display-width d) 2) (/ (display-height d) 2))
  )

;--- Text related widget functions ---


(defun create-redraw-thread (redraw-func gbranch &key name)
  (sb-thread:make-thread (lambda (wish-stream outputstream gb)
                           (setq *wish* wish-stream)
                           (setq *standard-output* outputstream)
                           ;(setq game-branch gbranch)
                           (handler-case
                             (loop
                               do (progn
                                    (sb-thread:with-mutex (*branch-display-mutex*)
                                      (when (not *wish*)
                                        (progn
                                          (return)))
                                      (when (not (funcall redraw-func))
                                        (return)))))
                             (type-error (e) (progn
                                               (format t "Error in redraw thread. Aborting thread. ~S~%" e)
                                               (sb-thread:release-mutex *branch-display-mutex*)
                                               (error e)
                                               (abort)))
                             (simple-error (e) (progn
                                                 (sb-thread:release-mutex *branch-display-mutex*)
                                                 (error e)))))
                         :arguments (list *wish* *standard-output* gbranch)
                         :name name))



(defun start-game (game_title game_branch &key (theme-func (lambda (&optional arg))))
  (when (or *music-enabled* *sfx-enabled* *text-sfx-enabled*)
    #+:linux (harmony-simple:initialize :output-spec '(harmony-pulse:pulse-drain))
    #-:linux (harmony-simple:initialize))
  
  (setf *start-branch* game_branch)
  (setf *gametitle* game_title)
  (save-game-state game_branch "newgame")
  (setf *branch-label-counter* -1)
  (setf branchie-classes::*display-image-cache* (make-hash-table)) ;prevents weird caching issues while in the REPL
  (handler-case
    (progn
      (when (and *default-bg-music* *music-enabled*)
        (progn
          (format t "Playing music:~S~%" *default-bg-music*)
          (play-music *default-bg-music*)))
      (main-window game_title game_branch theme-func))
    (condition (e)
               (progn
                 (format t "An unexpected condition occured:~%")
                 (stop-music)
                 (error e)
                 )))
  (stop-music))
