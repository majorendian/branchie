
(defpackage :branchie-ui
  (:use :cl :ltk :ltk-mw
        :branchie-classes
        :branchie-utils)
  (:export main-window
           set-dark-theme
           set-gold-theme
           set-text-draw-speed
           quit-game
           show-help))
;========================= GUI =======================

(in-package :branchie-ui)

(defparameter *window-icon* "data/img/icon.gif")

(defparameter *text-draw-speed* 20)
(defmethod set-text-draw-speed ((tds fixnum))
  (setf *text-draw-speed* tds))

(defun setup-window-icon (iconpath)
  (handler-case
    (let ((game-window-icon (make-image)))
      (image-load game-window-icon iconpath)
      (format-wish "wm iconphoto ~A -default ~A" (widget-path *tk*) (widget-path game-window-icon)))
    (type-error (e) (progn (format t "Tk/Tcl wish server not running.") (abort))))) 

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
                             "Type '/back' to go back 1 screen"
                             "Type '/toggle_txt_sfx' to toggle text sound effects on/off"
                             "Type '/toggle_music' to turn music on/off"
                             "Type '/wm_mode_fullscreen' to go into fullscreen mode"
                             "Type '/wm_mode_windowed' to go into windowed mode"
                             "Type '/save' to save the game at any time"
                             "Type '/load' to load the game at any time"
                             "==========================================================="
                             "There may be exceptions as to where you can save/load the game."
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
            (see (textbox tbox) :end) 
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


(defun update-fields (text entry-widget text-widget &key skip-ahead)
  "Update the text widgets.
   Sets the text of the main dialog box and resets the input field."
  ;;reset every field
  (setf (text entry-widget) "") ;also clear the entry input field
  (set-textbox-text text-widget text skip-ahead))

(defun setup-right-side-frame (right-side-frame)
  (configure right-side-frame
             :relief :sunken
             :borderwidth 5))

(defun setup-frame (f)
  (configure f
             :borderwidth 4
             :relief :groove))

(defun setup-scrolled-frame (sf)
  (configure sf
             :relief :sunken
             :borderwidth 5
             :height 50
             :width 100))

(defun setup-textbox (txt)
  (configure (textbox txt) 
             :height 4
             :width 73
             :wrap :word))

(defun setup-text-input (txt-input)
  (configure txt-input
             :width 70)
  (focus txt-input))

(defun confirm-input (game-branch)
  (let ((prev_branch game-branch))
    (setf game-branch (advance-branch game-branch txt-input txt gfxscreen)) ;most logic processing happens here
    (when (not (eq prev_branch game-branch))
      (sb-thread:join-thread
        (create-redraw-thread (lambda ()
                                (funcall gfxscreen-redraw-func)
                                (funcall right-gfxscreen-redraw-func)
                                nil) game-branch))
      (update-fields game-branch txt-input txt))))

(defun default-game-function (a_branch a_string)
  (format t "Got branch:~S~%Got String:~S~%" a_branch a_string)
  a_branch)

(defun default-update-display-function (a_display game-branch))

(defun default-update-side-panel-function (a_display game-branch))

;Main window of the game
(defun main-window (game_title game-branch &optional
                               (theme-function #'set-gold-theme) 
                               (game-function #'default-game-function)
                               (update-main-display-function #'default-update-main-display)
                               (update-side-panel-function #'default-update-side-panel-function)
                               )
  (declare (type string game_title))
  "The main window containing the main widgets and bindings"
  (with-ltk ()
    ;initialize image cache
    (wm-title *tk* game_title)
    ;(setf *display-image-cache* (make-hash-table))
    (maxsize *tk* 960 540)
    (minsize *tk* 960 540)
    (setup-font)
    (setup-window-icon *window-icon*)
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
                                    ;(when *global-game-state*
                                    ; (image-cache-preload-gc *global-game-state*))
                                    (update-main-display-function gfxscreen game-branch)
                                    nil))
           (right-gfxscreen-redraw-func (lambda ()
                                          ;(update-panel-display right-gfxscreen game-branch)
                                          nil))
           ;-----------------------------------------------------
           (confirm-input-f (lambda (&optional evt)
                              (declare (ignore evt))
                              (setf game-branch (game-function game-branch))
                              ))
           (spacing 4))
      (pack top_frame :side :top :padx spacing :pady spacing)
      (pack f :padx spacing :pady spacing :side :left)
      ;right side panel
      (pack right-side-frame :side :left :padx spacing :pady spacing)
      (setup-right-side-frame right-side-frame)
      (pack side-panel-canvas)
      ;---------------
      (pack main-canvas :side :top)
      (funcall right-gfxscreen-redraw-func)
      ;We need to use a mutex here otherwise the WISH server might crash
      ;due to multiple async calls
      (sb-thread:with-mutex (*display-mutex*)
        (pack sf :expand t :side :top :padx spacing :pady spacing)
        (pack txt :side :top :pady spacing :padx spacing)
        (grid-forget (hscroll txt)) ;remove horizontal scroll from text area. we dont need it
        (pack txt-input :side :top :pady spacing :padx 10)
        (setup-frame f)
        (setup-textbox txt)
        (setup-scrolled-frame sf)
        (setup-text-input txt-input)
        (funcall theme-function (textbox txt))
        ;--- ENTER binding for text entry input ---
        (bind txt-input "<Key-Return>" confirm-input-f)
        ;--- TAB binding for autocomplete ---
        (bind txt-input "<Key-Tab>" (lambda (evt)
                                      (declare (ignore evt))
                                      ;(setf (text txt-input) (text-entry-autocomplete (available-options-of game-branch) (text txt-input)))
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
        ;(set-textbox-text txt (br-text game-branch) t)
        ;(advance-branch game-branch txt-input txt gfxscreen)
        ;(trigger-branch-action game-branch)
        (on-close *tk* (lambda () (exit-wish))))
      ;(create-redraw-thread gfxscreen-redraw-func nil :name "main-display-thread")
      )))

;====================================================================================
