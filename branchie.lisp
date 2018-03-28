(defpackage :branchie
  (:use :cl :ltk :ltk-mw :harmony-simple
            :branchie-classes)
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
           play-music
           play-sfx
           stop-music
           set-default-bg-music
           root-branch
           set-gold-theme
           set-dark-theme
           quit-game
           ;helpfull functions
           join
           join-nl
           ;other configurations
           set-character-sfx
           set-bg-music
           set-text-draw-speed
           set-debug
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
  (bshadow nil) ;boolean value to check if this is a shadow branch or a regular branch
  (text nil) ;text to display on the branch
  (text-options nil) ;options for the branch, created by macro "choices"
  (hint nil) ;a hint for the player if available so that they can figure out things if they are stuck
  (br-func nil) ;function to execute when entering the branch
  (image nil) ;image associated with this branch
  (characters (list))
  (animation nil)
  (variables nil)) ;a hashmap of branch-bound variables (like leaves)

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

(defparameter *game-music-source* nil) ;populated on first play

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

(defparameter *window-icon* "data/img/icon.gif")
(defparameter *header-image* "data/img/header.gif")

(defparameter *character-sfx-file* #p"data/sound/char-sfx-1.mp3")
(defmethod set-character-sfx ((soundfile_mp3 pathname))
  (setf *character-sfx-file* soundfile_mp3))
(defparameter *default-bg-music* #p"data/sound/Seedling.mp3") ;Seedling.mp3 has been created by Walid Feghali
(defmethod set-bg-music ((soundfile_mp3 pathname))
  (setf *default-bg-music* soundfile_mp3))

(defparameter *gametitle* "untitled")

;------------------------------
(defparameter *current-bg-music* nil)

;--- In game settings ---

(defparameter *text-draw-speed* 20)
(defmethod set-text-draw-speed ((tds fixnum))
  (setf *text-draw-speed* tds))
(defparameter *text-sfx-enabled* t)
(defparameter *music-enabled* t)
(defparameter *sfx-enabled* t)


;--- helper functions ---------
(defparameter *branch-label-counter* -1)
(defun generate-branch-label ()
  (setf *branch-label-counter* (+ 1 *branch-label-counter*))
  (format nil "BR~a" *branch-label-counter*))

(defun join (sep l)
  (declare (type list l))
  "Joins a list of strings together with a separator recursively"
  (when (characterp sep)
    (setq sep (format nil "~a" sep)))

  (when (not sep)
    (setq sep ",")) ;set default value for separator if not defined
  
  (when (not (cdr l))
    (setq sep "")) ;remove separator if this is the end of the list
  (if l
    (concatenate 'string (car l) sep (join sep (cdr l)))
    ""))

(defmacro join-nl (&rest l)
  `(join #\Newline (list ,@l)))

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
        (_image nil)
        (_characters nil)
        (_next nil)
        (_skip_register nil)
        (_animation nil)
        (i 0))
    (loop for e in r
          ;Old-fashioned index-style loop
          do (progn
               (cond
                 ((eql e :action) (setq _actionf (nth (+ 1 i) r)))
                 ((eql e :name) (setq _branch_name (nth (+ 1 i) r)))
                 ((eql e :image) (setq _image (nth (+ 1 i) r)))
                 ((eql e :characters) (setq _characters (nth (+ 1 i) r)))
                 ((eql e :next) (setq _next (nth (+ 1 i) r)))
                 ((eql e :skip-register) (setq _skip_register (nth (+ 1 i) r)))
                 ((eql e :animation ) (setq _animation (nth (+ 1 i) r)))
                 ((functionp e) (continue))
                 ((and (listp e) (typep (first e) 'opt-pair)) (setf _choices (macroexpand e))))
               (setq i (+ 1 i))
               (format t "Characters:~S~%" _characters)))
    (let ((b
             (make-branch 
               :name (if (not _branch_name) (read-from-string (generate-branch-label)) _branch_name)
               :text (lambda () text)
               ;The IF condition fixes an issue of a required terminating NIL when no choices are provided or only keyword args are supplied
               :text-options (if (functionp _choices) nil _choices) 
               :br-func _actionf
               :hint nil
               :variables (make-hash-table)
               :characters (if (listp _characters) _characters (list _characters))
               :image _image
               :bshadow nil
               :animation _animation
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

(defmethod br-animation ((b branch))
  (slot-value b 'animation))

(defmethod br-next ((b branch))
  (slot-value b 'next))

(defmethod br-image ((b branch))
  (slot-value b 'image))

(defmethod br-characters ((b branch))
  (slot-value b 'characters))

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
(defun remove-nth (n l)
  (declare (type fixnum n) (type list l))
  "Remove N-th element from list L by copying
   everything except the N-th element."
  (nconc (subseq l 0 n) (nthcdr (1+ n) l)))
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

(defun compile-game-tree (start_br)
  "Evaluates all branches recursively"
  (labels ((traverse-options (optlist)
             (if optlist
               (progn
                 (format t "Compiling:~S~%" (first optlist))
                 ;this gets/evaluates the opt-branch portion of the opt-pair
                 (let ((compiled_branch (get-branch-of-opt-pair (first optlist))))
                   (when (or (stringp compiled_branch) (symbolp compiled_branch))
                     (return-from traverse-options nil))
                   (if (or
                         (compare-branch-names *start-branch* compiled_branch)
                         (compare-branch-names start_br compiled_branch)
                         )
                     (return-from traverse-options nil)
                     (progn
                       (traverse-options (rest optlist))
                       (compile-game-tree compiled_branch)))) 
                 )))) 
    (if (slot-value start_br 'text-options)
      (progn
        (traverse-options (slot-value start_br 'text-options))
        ))))

(defmacro goto (branch_name)
  `(get-branch-by-name-from-table ,branch_name))

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
      :hint (slot-value branch 'hint)
      :image (slot-value branch 'image)
      :characters (slot-value branch 'characters)
      :variables (copy-hash-table (slot-value branch 'variables)))))


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
(defun setup-window-icon ()
  (handler-case
    (let ((game-window-icon (make-image)))
      (image-load game-window-icon *window-icon*)
      (format-wish "wm iconphoto ~A -default ~A" (widget-path *tk*) (widget-path game-window-icon)))
    (condition () (progn (format t "Tk/Tcl wish server not running.") (abort))))) 

(defun setup-font ()
  "Sets up fonts for the current window"
  (send-wish "font create myDefaultFont -family \"Monospace\" -size 11")
  (send-wish "option add *font myDefaultFont")
  )

(defun set-dark-theme (text-widget)
  (declare (type (or nil text) text-widget))
  "Sets the widget colors to a more darker theme."
  (handler-case (progn
                  (send-wish "ttk::style configure TFrame -background black")
                  (send-wish ". configure -background black")
                  (send-wish "ttk::style configure TLabel -background black")
                  (send-wish "ttk::style configure TLabel -foreground lightgrey")
                  (send-wish "ttk::style map TLabel -background [list disabled darkgrey readonly white]")
                  (send-wish "ttk::style map TLabel -foreground [list disabled darkgrey readonly white]")
                  ;(send-wish "ttk::style configure TScrollbar -background black")
                  ;(send-wish "ttk::style configure TScrollbar -throughcolor black")
                  ;(send-wish "ttk::style configure TScrollbar -arrowcolor grey")
                  ;(send-wish "ttk::style configure TScrollbar -bordercolor grey")
                  ;(send-wish "ttk::style configure TScrollbar -darkcolor black")
                  ;(send-wish "ttk::style configure TScrollbar -lightcolor white")
                  (send-wish "ttk::style configure TSeparator -background black")
                  ;    (send-wish "ttk::style map TScrollbar -background [list active white disabled black]")
                  ;    (send-wish "ttk::style map TScrollbar -foreground [list active white disabled black]")
                  ;    (send-wish "ttk::style map TScrollbar -arrowblack [list active white disabled black]")
                  (send-wish "ttk::style configure TEntry -background black")
                  (send-wish "ttk::style configure TEntry -foreground black")
                  (send-wish "ttk::style configure TEntry -selectforeground lightgrey")
                  (send-wish "ttk::style configure TEntry -selectbackground grey")
                  (send-wish "ttk::style configure TEntry -fieldbackground darkgrey")
                  (send-wish "ttk::style map TEntry -background [list disabled darkgrey readonly darkgrey]")
                  (send-wish "ttk::style map TEntry -foreground [list disabled darkgrey readonly darkgrey]")
                  (send-wish "ttk::style map TEntry -fieldbackground [list disabled darkgrey readonly darkgrey]")
                  (when text-widget
                    (progn
                      (configure text-widget :background "black" :foreground "white")))
                  )
    (condition (c) (progn
                     (format t "An error occured while setting theme.~%Error:~S~%Tk/Tcl wish server is probably not running~%Aborting~%" c)
                     (abort)))))


(defun set-gold-theme (&optional text-widget)
  "Sets the widgets colors to a golden theme."
  (handler-case (progn
                  (send-wish "ttk::style configure TFrame -background #ADAD52")
                  (send-wish ". configure -background #ADAD52")
                  (send-wish "ttk::style configure TLabel -background #ADAD52")
                  (send-wish "ttk::style configure TLabel -foreground black")
                  (send-wish "ttk::style map TLabel -background [list disabled #ADAD52 readonly #4D4D3D]")
                  (send-wish "ttk::style map TLabel -foreground [list disabled #ADAD52 readonly #4D4D3D]")
                  (send-wish "ttk::style configure TScrollbar -background #D9D473")
                  (send-wish "ttk::style configure TScrollbar -throughcolor #ADAD52")
                  (send-wish "ttk::style configure TScrollbar -arrowcolor #ADAD52")
                  (send-wish "ttk::style configure TScrollbar -bordercolor #ADAD52")
                  (send-wish "ttk::style configure TScrollbar -darkcolor #ADAD52")
                  (send-wish "ttk::style configure TScrollbar -lightcolor #ADAD52")
                  (send-wish "ttk::style configure TSeparator -background #ADAD52")
                  (send-wish "ttk::style map TScrollbar -background [list active #FAF6AE disabled #D9D473]")
                  (send-wish "ttk::style map TScrollbar -foreground [list active #ADAD52 disabled #D9D473]")
                  (send-wish "ttk::style map TScrollbar -arrowblack [list active #ADAD52 disabled #D9D473]")
                  (send-wish "ttk::style configure TEntry -background #4D4D3D")
                  (send-wish "ttk::style configure TEntry -foreground #4D4D3D")
                  (send-wish "ttk::style configure TEntry -selectforeground #4D4D3D")
                  (send-wish "ttk::style configure TEntry -selectbackground #D9D473")
                  (send-wish "ttk::style configure TEntry -fieldbackground #D9D473")
                  (send-wish "ttk::style map TEntry -background [list disabled darkgrey readonly darkgrey]")
                  (send-wish "ttk::style map TEntry -foreground [list disabled darkgrey readonly darkgrey]")
                  (send-wish "ttk::style map TEntry -fieldbackground [list disabled darkgrey readonly darkgrey]")
                  (when text-widget
                    (progn
                      (configure text-widget :background "#D9D473" :foreground "#4D4D3D")))
                  )
    (condition (c) (progn
                     (format t "An error occured while setting theme.~%Error:~S~%Tk/Tcl wish server is probably not running~%Aborting~%" c)
                     (abort)))))


;===================================================================================

(defun show-help () 
  "Show a help window containing instructions on how to play the game."
  ;TODO: make a show-hint function simmilar to this function which will show hints (if any) for the current branch
  ;for the player to proceed.
  (progn
    (let* ((tl (make-instance 'toplevel))
           (f (make-instance 'frame
                             :master tl))
           (l (make-instance 'label
                             :master f))
           )
      (pack f :padx 0 :pady 0)
      (pack l :side :top :padx 7 :pady 7 :fill :x)
      (configure f
                 :borderwidth 20)
      (setf (text l) (join (format nil "~%")
                           (list
                             "To play the game, type your response into the input field then press ENTER."
                             "You can use TAB to autocomplete your response."
                             "You can also use TAB to cycle through all the options."
                             "======================== Commands ========================="
                             "Type '/exit' or '/quit' to close the program."
                             "Type '/back' to go back 1 screen."
                             "Type '/toggle_txt_sfx' to toggle text sound effects on/off"
                             "Type '/toggle_music' to turn music on/off"
                             "Type '/wm_mode_fullscreen' to go into fullscreen mode"
                             "Type '/wm_mode_windowed' to go into windowed mode"
                             "==========================================================="
                             "Press ESC to close any window.")
                           ))
      (wm-title tl "Help")
      (bind tl "<Key-Escape>" (lambda (evt) (declare (ignore evt)) (destroy tl))))))

(defun really-quit? ()
  (let* ((tl (make-instance 'toplevel))
         (f (make-instance 'frame
                           :master tl))
         (l (make-instance 'label
                           :master f))
         )
    (pack f :padx 0 :pady 0)
    (pack l :side :top :padx 7 :pady 7 :fill :x)
    (configure f
               :borderwidth 20)
    (setf (text l) (join (format nil "~%")
                         (list
                           "Really quit?"
                           "Press ESC to close the window."
                           "Press ENTER to quit."
                           )
                         ))
    (wm-title tl "Really quit?")
    (bind tl "<Key-Escape>" (lambda (evt) (declare (ignore evt)) (destroy tl)))
    (bind tl "<Key-Return>" (lambda (evt) (declare (ignore evt)) (exit-wish)))))

(defun quit-game ()
  (if *wish*
    (exit-wish)))

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

(defun play-character-sfx ()
  (let ((sound_source (harmony-simple:play *character-sfx-file* :master)))
    (setf (harmony-simple:volume sound_source) 0.05)
    ))

(defun play-sfx (sfx)
  (declare (type pathname sfx))
  (when *sfx-enabled*
    (let ((sound_source (harmony-simple:play sfx :master :loop nil)))
      (setf (harmony-simple:volume sound_source) 0.05)
      )))

(defun play-music (filepath)
  (declare (type pathname filepath))
  "FILEPATH is a path to a file in the form #p\"filepath\"
  Stops music on *GAME-MUSIC-SOURCE* and plays the music given in FILEPATH."
  (when *game-music-source*
    (harmony-simple:stop *game-music-source*))
  (when *music-enabled*
    (setf *game-music-source* (harmony-simple:play filepath :music :loop t))
    (setf (harmony-simple:volume *game-music-source*) 0.5))
  (setf *current-bg-music* filepath))

(defun set-default-bg-music (filepath)
  "Sets the default background music specified by FILEPATH
   for the gmaescript"
  (declare (type pathname filepath))
  (setf *default-bg-music* filepath))

(defun stop-music ()
  "Stop all music on *GAME-MUSIC-SOURCE*"
  (when *game-music-source*
    (progn
      (handler-case
        (harmony-simple:stop *game-music-source*)
        (harmony-pulse:pulse-error () (abort))))))

(defun set-textbox-text (tbox str &optional skip-ahead) ;used in #'advance-branch to set the text of the scrollable text area
  (declare (type scrolled-text tbox) (type string str) (type (or nil t) skip-ahead))
  "Animate the display of text in the given textbox."
  (configure (textbox tbox) :state :normal)
  (setf (text tbox) "")
  (if (not skip-ahead)
    (loop for c in (coerce str 'list) do
          (progn
            ;(when (string-equal "~" c) (setq c (format nil "~%"))) ;if there is a "~" in the string, treat it as a new line
            ;play soundeffect for each letter excluding whitespace
            (when *text-sfx-enabled* 
              (if (not (string= c " ")) (play-character-sfx)))
            (configure (textbox tbox) :state :normal)
            (append-text tbox c)
            (configure (textbox tbox) :state :disabled) 
            (see (textbox tbox) :end) ;TODO: Here we should move the scrollbar down automatically. If not then whatever.
            (let ((ev (read-event :blocking nil)))
              (if (string-equal (write-to-string (nth 5 ev)) "RETURN")
                (progn
                  (configure (textbox tbox) :state :normal)
                  (setf (text tbox) str)
                  (configure (textbox tbox) :state :disabled)
                  (return))
                (progn 
                  (sleep (/ 1 *text-draw-speed*))
                  )
                ))
            ))
    ;else skip ahead
    (setf (text tbox) str))
  (configure (textbox tbox) :state :disabled))

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

(defun split-string (str &optional (c #\Space))
  (if (position c str)
    (cons (subseq str 0 (position c str)) (split-string (subseq str (+ 1 (position c str)))))
    (cons str nil)))


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

    (set-glob-var 'current-branch (slot-value cur_b 'name))
    (set-glob-var 'current-image (write-to-string (slot-value cur_b 'image)))
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

(defun text-entry-autocomplete (branch input-str)
  (declare (type branch branch) (type string input-str))
  "Option autocompletion for the entry input field.
   Autocompletes/Cycles through options of BRANCH given the string INPUT-STR"
  (let* ((i 0)
         (next-opt-flag nil)
         (all_options (concatenate 'list (available-options-of branch) *commands-for-autocomplete*))
         (result
           (loop for option-text in all_options
                 do (progn
                      (setq i (+ i 1))
                      (cond
                        ((string= (string-downcase option-text) (string-downcase input-str))
                         (progn
                           (format t "text equals option:~A=~A~%" option-text input-str)
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



;====== Graphic related =====================================================================================================
;classes and methods for the main drawing canvas
(defparameter *branch-display-mutex* (sb-thread:make-mutex :name "branch-display-mutex"))
;========= Image cache ===============
(defparameter *display-image-cache* nil)
(defmethod image-cache-symbol-from ((filepath pathname))
  (let ((convstring (string-upcase (format nil "CACHED_~A" (slot-value filepath 'namestring))) ))
    (format t "Resulting conversion string:~S~%" convstring)
    (intern convstring)))

(defmethod image-cache-symbol-from ((filepath string))
  (image-cache-symbol-from (uiop:parse-native-namestring filepath)))

(defmethod lookup-image ((filepath pathname))
  ;(_print_hash *display-image-cache*)
  (gethash (image-cache-symbol-from filepath) *display-image-cache*))

(defmethod lookup-image ((filepath string))
  (lookup-image (uiop:parse-native-namestring filepath)))

(defmethod image-cache ((imgpath pathname))
  (let ((existing_image (lookup-image imgpath)))
    (if existing_image
      (progn
   ;     (format t "Image exists. Retriving from cache.~%")
        existing_image)
      (progn
    ;    (format t "Image doesn't exists. Setting into cache.~%")
        (setq existing_image (make-instance 'game-image :image-path imgpath :image (make-image)))
        (image-load (gi-image existing_image) imgpath)
        (setf (gethash (image-cache-symbol-from imgpath) *display-image-cache*) existing_image)
        existing_image))))

(defmethod image-cache ((gi game-image))
  (format t "display cahce:~%")
  (_print_hash *display-image-cache*)
  (let ((existing (lookup-image (gi-image-path gi))))
    (if existing
      (progn
        (format t "fetching game-image from cache: ~S~%" (gi-image-path existing))
        existing)
      (progn
        (format t "loading game-image x:~S~%" (gi-image-path gi))
        (setf (gi-image gi) (make-image))
        (image-load (gi-image gi) (gi-image-path gi))
        (setf (gethash (image-cache-symbol-from (gi-image-path gi)) *display-image-cache*) gi) 
        gi))))

(defmethod image-cache ((gc game-character))
  (image-cache (gc-gi gc)))

(defmethod image-cache ((imgpath string))
  (image-cache (uiop:parse-native-namestring imgpath)))

(defmethod image-cache-preload-gc ()
  (maphash (lambda (v gc)
             (when (and (typep gc 'game-character) (gc-image-preload gc))
               (image-cache (gi-image-path (gc-gi gc))))) *global-game-state*))

;=====================================
(defclass display-point ()
  ((x :accessor display-point-x :initarg :x)
   (y :accessor display-point-y :initarg :y)))

(defmethod display-point-set-x ((dp display-point) val)
  (setf (display-point-x dp) val))

(defmethod display-point-set-y ((dp display-point) val)
  (setf (display-point-y dp) val))

(defmethod display-point-op-x ((dp display-point) (f function) val)
  "Call function F with arguments X of the point and VAL"
  (funcall f (display-point-x dp) val))

(defmethod display-point-op-y ((dp display-point) (f function) val)
  "Call function F with arguments Y of the point and VAL"
  (funcall f (display-point-y dp) val))

(defmethod print-object ((dp display-point) any)
  (format any "display-point x:~d y:~d" (display-point-x dp) (display-point-y dp)))

;----------------------------------------------------------------------

(defclass display ()
  ((canvas :reader display-canvas :initarg :canvas)
   (width :reader display-width :initarg :width :initform nil)
   (height :reader display-height :initarg :height :initform nil)
   ;internal attributes
   (text-position :accessor display-text-position :initarg :text-position :initform (make-instance 'display-point :x 0 :y 0))
   (image :accessor display-image :initarg :image :initform (make-image))
   (imagepath :accessor display-imagepath :initarg :imagepath :initform nil)
   (characters :accessor display-characters :initarg :characters :initform (list))
   (options-frame-container :accessor display-opt-frame :initarg :opt-frame :initform nil)
   (instance-variables :accessor display-iv :initarg :iv :initform (make-hash-table))
   ))

(defmethod display-set-var ((d display) (varname symbol) val)
  (setf (gethash varname (display-iv d)) val))

(defmethod display-get-var ((d display) (varname symbol))
  (gethash varname (display-iv d)))

(defmethod init-display ((c canvas))
  (configure c
             :bd 0
             :highlightthickness 0
             :relief :ridge)
  (let ((dinstance (make-instance 'display 
                                  :canvas c 
                                  :width (parse-integer (cget c :width)) 
                                  :height (parse-integer (cget c :height))
                                  :opt-frame (make-instance 'frame :master c))))
    (display-set-var dinstance 'aux-point (make-instance 'display-point :x 0 :y 0))
    dinstance))

(defmethod display-center-text-x ((d display))
  (setf (display-point-x (display-text-position d)) (floor (/ (display-width d) 2)))
  (format t "text-position x: ~A~%" (display-point-x (display-text-position d))))

(defmethod display-center-text-y ((d display))
  (setf (display-point-y (display-text-position d)) (floor (/ (display-height d) 2)))
  )

(defmethod display-draw-text ((d display) (text string))
  (format-wish "~A create text ~A ~A -font myDefaultFont -text ~S"
               (widget-path (display-canvas d))
               (display-point-x (display-text-position d))
               (display-point-y (display-text-position d))
               text))

(defmethod display-draw-text-frame ((d display) (f frame))
  "Draws a frame widget near the bottom of the canvas display.
   This depends on the widgets size so adjustments might be necessary.
   D is the main game display canvas.
   F is a frame expected to contain some sort of text.
   Frames are positioned from top-left corner as opposed to
   just text which is positioned from the center."
  (create-window (display-canvas d)
                 ;(floor (/ (display-point-x (display-text-position d)) 2))
                 20
                 (floor (/ (display-point-y (display-text-position d)) 1.05))
                 f))

(defmethod display-clear ((d display))
  (clear (display-canvas d))
  (format-wish "~a create rectangle ~a ~a ~a ~a -fill ~a" (widget-path (display-canvas d))
               0 0 (display-width d) (display-height d)
               "black"))

(defmethod display-draw-branch-options ((d display) (game-branch branch))
  "Draw a black stripe rectangle and then in the center draw the text options"
  (format-wish "~a create rectangle ~a ~a ~a ~a -fill ~a" (widget-path (display-canvas d))
               0
               (floor (/ (display-point-y (display-text-position d)) 1.11))
               (display-width d)
               (+ 23 (floor (/ (display-point-y (display-text-position d)) 1.05)))
               "white") 
  (format-wish "~a create rectangle ~a ~a ~a ~a -fill ~a" (widget-path (display-canvas d))
               0
               (floor (/ (display-point-y (display-text-position d)) 1.10))
               (display-width d)
               (+ 20 (floor (/ (display-point-y (display-text-position d)) 1.05)))
               "black") 
  (format-wish "~a create text ~a ~a -fill white -font myDefaultFont -text ~s" (widget-path (display-canvas d))
               (display-point-x (display-text-position d))
               (floor (/ (display-point-y (display-text-position d)) 1.05))
               (join " | " (available-options-of game-branch))
               ))

(defmethod display-draw-image ((d display) imagepath x y) ;imagepath can be nil
  ;NOTE: make sure we don't unnecessarily load the same image multiple times
  (labels ((wish-draw-image ()
             (format-wish "~a create image ~a ~a -image ~a" 
                          (widget-path (display-canvas d)) 
                          ;divide the width and height by 2 because the image is being drawn from the center not from top-left
 ;                         (/ (display-width d) 2) 
 ;                         (/ (display-height d) 2)
                          x
                          y
                          (widget-path (display-image d)))
             ))
    (if imagepath
      (progn
        (cond
          ((or (null (display-imagepath d)) (not (uiop:pathname-equal imagepath (display-imagepath d))) )
           (progn
             (format t "Loading image: ~S~%" imagepath)
             (image-load (display-image d) imagepath)
             (setf (display-imagepath d) imagepath)
             (wish-draw-image)))
          ((uiop:pathname-equal imagepath (display-imagepath d)) (wish-draw-image))
          (t nil))
        )
      ;esle if imgpath evals to false
      (if (and (display-imagepath d) (display-image d))
        (wish-draw-image))) ;if we have the imagepath and image defined
    ;(create-image (display-canvas d) 0 0 :image *current-branch-image*)
    )
  )

(defmethod display-redraw-current-image ((d display))
  (display-draw-image d (display-imagepath d) (/ (display-width d) 2) (/ (display-height d) 2)))

(defmethod display-draw-image-slide-in ((d display) imagepath)
  (loop for i from -320 to 320
        do (progn
             (display-clear d)
             (display-draw-image d imagepath 
                                 i
                                 (/ (display-height d) 2)
                                 )
             (sleep 0.001))))

(defmethod display-wish-draw-image ((d display) (imgpath string) &optional x y)
  (let ((cached_image (image-cache imgpath)))
    (format-wish "~a create image ~a ~a -image ~a" 
                (widget-path (display-canvas d)) 
                (if x
                  x
                  (gi-posx cached_image))
                (if y
                  y
                  (gi-posy cached_image))
                (widget-path (gi-image (image-cache imgpath))))))

(defmethod display-wish-draw-image ((d display) (imgpath pathname) &optional x y)
  (display-wish-draw-image d (namestring imgpath) x y))

(defmethod display-wish-draw-image ((d display) (gi game-image) &optional x y)
  (display-wish-draw-image d (gi-image-path (image-cache gi)) x y))

(defmethod display-wish-draw-image ((d display) (gc game-character) &optional x y)
  (display-wish-draw-image d (gc-gi gc) x y))

(defmethod display-draw-image-shake-x ((d display) imagepath)
  (let* ((img (image-cache imagepath))
         (start_x (gi-posx img))
         (x start_x)
         (end_x (+ 50 start_x))
         (c 0)
         (timestep 0.001)
         (end_c 200)
         (flipdir nil))
    (loop
      do (progn
           (if (> c end_c)
             (return)
             (progn
               (cond
                 ((>= x end_x) (setq flipdir t))
                 ((<= x start_x) (setq flipdir nil)))
               (if flipdir
                 (setq x (- x 1))
                 (setq x (+ 1 x)))
               (setq c (+ c 1))
              ; (format t "flipdir:~S x:~S~%" flipdir x)
               (display-clear d)
               (display-redraw-current-image d) ;redraw background image
               (display-wish-draw-image d imagepath x)))
           (sleep timestep)))))

(defmethod display-draw-image-shake-x ((d display) (gi game-image) )
  (image-cache gi)
  (display-draw-image-shake-x d (gi-image-path gi)))

(defmethod display-draw-image-shake-x ((d display) (gc game-character))
  (display-draw-image-shake-x d (gc-gi gc)))

(defmethod display-center-x ((d display))
  (floor (/ (display-width d) 2)))

(defmethod display-center-y ((d display))
  (floor (/ (display-height d) 2)))

(defmethod display-draw-characters ((d display) (b branch))
  (labels ((animate-character (pair)
             (let ((gc (first pair))
                   (anim (second pair)))
               (format t "Animating ~S with ~S~%" (gc-name gc) anim)   
               (cond
                 ((null anim) (display-wish-draw-image d gc))
                 ((eql anim :shake) (display-draw-image-shake-x d gc))
                 ((eql anim :attack) (display-wish-draw-image d gc))
                 ((eql anim :overlay) (display-wish-draw-image d gc (display-center-x d) (display-center-y d)))
                 )
               )))
    (map 'nil #'animate-character (slot-value b 'characters))))

(defmethod display-effect-silly ((d display))
  (format t "~S~%" (display-get-var d 'aux-point))
  (format-wish "~a create rectangle ~a ~a ~a ~a -fill ~a" (widget-path (display-canvas d))
               0 0 
               (display-width d)
               (display-point-set-y (display-get-var d 'aux-point) (+ 50 (display-point-y (display-get-var d 'aux-point))))
               "white")
  (when (> (display-point-y (display-get-var d 'aux-point)) (display-height d))
    (display-point-set-y (display-get-var d 'aux-point) 0)))

(defun update-main-display (d br)
  ;clear the screen
  (display-clear d)
  ;check for animation parameters
  ;animate main branch image accordingly
  (cond 
    ((eql :slide (br-animation br)) (display-draw-image-slide-in d (br-image br)))
    ((null (br-animation br)) (display-draw-image d (br-image br) (/ (display-width d) 2) (/ (display-height d) 2))))
  (display-draw-characters d br)
  ;(display-effect-silly d)
  (display-draw-branch-options d br))

(defun update-panel-display (d br)
  (display-clear d)
  (display-draw-image d #p"data/img/tree_ascii.gif" (/ (display-width d) 2) (/ (display-height d) 2)))

;--- Text related widget functions ---

(defun update-fields (branch entry-widget text-widget &key skip-ahead)
  "Update the text widgets.
   Sets the text of the main dialog box and resets the input field."
  ;;reset every field
  (setf (text entry-widget) "") ;also clear the entry input field
  (set-textbox-text text-widget (br-text branch) skip-ahead) 
  branch)

;=======================================================================================================================

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
                                               ;(error e)
                                               (sb-thread:release-mutex *branch-display-mutex*)
                                               (abort)))
                             (simple-error (e) (progn
                                                 (sb-thread:release-mutex *branch-display-mutex*)
                                                 (error e)))))
                         :arguments (list *wish* *standard-output* gbranch)
                         :name name))

;main setup for main graphic display
(defmethod setup-main-display ((d display))
  (setf (display-text-position d) (make-instance 'display-point :x 0 :y (- (display-height d) 30)))
  (display-center-text-x d))

;Main window of the game
(defun main-window (game_title game-branch &optional (theme-function #'set-gold-theme))
  (declare (type string game_title) (type branch game-branch))
  "The main window containing the main widgets and bindings"
  (with-ltk ()
    ;initialize image cache
    (wm-title *tk* game_title)
    (setf *display-image-cache* (make-hash-table))
    ;preload flagged images
    (maxsize *tk* 960 540)
    (minsize *tk* 960 540)
    (setup-font)
    (setup-window-icon)
    (let* ((top_frame (make-instance 'frame
                                     :master *tk*))
           (f (make-instance 'frame
                             :master *tk*))
           (sf (make-instance 'frame
                              :master f
                              ))
           (right-side-frame (make-instance 'frame
                                            :master *tk*))
           (side-panel-canvas (make-instance 'canvas
                                             :master right-side-frame
                                             :width 300
                                             :height 480))
           (right-gfxscreen (init-display side-panel-canvas))
           (main-canvas (make-instance 'canvas ;This is the game display for images and other kinds of texts
                                       :master f
                                       :width 640
                                       :height 360))

           ;main graphics screen
           (gfxscreen (init-display main-canvas))
           (txt (make-instance 'scrolled-text
                               :master sf))
           (txt-input (make-instance 'entry
                                     :master f))
           ;---these 2 functions are run within other threads--- 
           (gfxscreen-redraw-func (lambda () 
                                    (image-cache-preload-gc) ;TODO: preload only once
                                    (update-main-display gfxscreen game-branch)  nil))
           (right-gfxscreen-redraw-func (lambda ()
                                          (update-panel-display right-gfxscreen game-branch)  nil))
           ;-----------------------------------------------------
           (confirm-input-f (lambda (&optional evt)
                              (declare (ignore evt))
                              (let ((prev_branch game-branch))
                                (setf game-branch (advance-branch game-branch txt-input txt gfxscreen)) ;most logic processing happens here
                                (when (not (eq prev_branch game-branch))
                                  (sb-thread:join-thread
                                    (create-redraw-thread (lambda ()
                                                            (funcall gfxscreen-redraw-func)
                                                            (funcall right-gfxscreen-redraw-func)
                                                            nil) game-branch))
                                  (update-fields game-branch txt-input txt)))
                              ))
           (spacing 4))
      (pack top_frame :side :top :padx spacing :pady spacing)
      (pack f :padx spacing :pady spacing :side :left)
      ;right side panel
      (pack right-side-frame :side :left :padx spacing :pady spacing)
      (configure right-side-frame
                 :relief :ridge
                 :borderwidth 5)
      (pack side-panel-canvas)
      ;---------------
      ;This sets up the main display redraw loop
      (pack main-canvas :side :top)
      (setup-main-display gfxscreen)

      ;Asynchronly draw the display
      (funcall right-gfxscreen-redraw-func)
      ;We need to use a mutex here otherwise the WISH server might crash
      ;due to multiple async calls
      (sb-thread:with-mutex (*branch-display-mutex*)
        (pack sf :expand t :side :top :padx spacing :pady spacing)
        (pack txt :side :top :pady spacing :padx spacing)
        (grid-forget (hscroll txt)) ;remove horizontal scroll from text area. we dont need it
        (pack txt-input :side :top :pady spacing :padx 10)
        (configure f
                   :borderwidth spacing
                   :relief :groove)
        (configure (textbox txt) 
                   :height 4
                   :width 73
                   :wrap :word)
        (configure sf
                   :relief :sunken
                   :borderwidth 5)
        (configure sf
                   :height 50
                   :width 100)
        (configure txt-input
                   :width 70)
        (focus txt-input)
        (funcall theme-function (textbox txt))
        ;--- ENTER binding for text entry input ---
        (bind txt-input "<Key-Return>" confirm-input-f)
        ;--- TAB binding for autocomplete ---
        (bind txt-input "<Key-Tab>" (lambda (evt)
                                      (declare (ignore evt))
                                      (setf (text txt-input) (text-entry-autocomplete game-branch (text txt-input)))
                                      ;clear selection and move cursor to the end of the string
                                      (format-wish "~a selection clear" (widget-path txt-input))
                                      (format-wish "~a icursor end" (widget-path txt-input))
                                      ))
        (bind txt-input "<Control-BackSpace>" (lambda (evt)
                                                (declare (ignore evt))
                                                ;for simplicity sake, this just removes all text from the input field
                                                (setf (text txt-input) "")))
        (bind *tk* "<Key-Escape>" (lambda (evt) (declare (ignore evt)) (really-quit?)))
        (bind *tk* "<Key-F1>" (lambda (evt) (declare (ignore evt)) (show-help)))
        (set-textbox-text txt (br-text game-branch) t)
        (advance-branch game-branch txt-input txt gfxscreen)
        (trigger-branch-action game-branch)
        (on-close *tk* (lambda () (stop-music) (exit-wish))))
      (create-redraw-thread gfxscreen-redraw-func nil :name "main-display-thread")
      )))



(defun start-game (game_title game_branch &key (theme-func #'set-dark-theme))
  #+:linux (harmony-simple:initialize :output-spec '(harmony-pulse:pulse-drain))
  #-:linux (harmony-simple:initialize)
  
  (setf *start-branch* game_branch)
  (setf *gametitle* game_title)
  (save-game-state game_branch "newgame")
  (setf *branch-label-counter* -1)
  (handler-case
    (progn
      (when *default-bg-music*
        (progn
          (format t "Playing music:~S~%" *default-bg-music*)
          (play-music *default-bg-music*)))
      (main-window game_title game_branch theme-func))
    (condition (e)
               (progn
                 (format t "An unexpected condition occured:~%")
                 (stop-music)
                 (describe e))))
  (stop-music))
