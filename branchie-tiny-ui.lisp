(defpackage :branchie-tiny-ui
  (:use :cl :branchie-core :ltk)
  (:export gui
           quit-gui
           set-textarea-text
           get-textarea
           get-canvas
           clear-canvas
           draw-text
           draw-textarea
           ))

(in-package :branchie-tiny-ui)

(defun clear-canvas (canvas)
  (clear canvas)
  (format-wish "~a create rectangle 0 0 ~a ~a -fill black"
               (widget-path canvas)
               (parse-integer (cget canvas :width)) (parse-integer (cget canvas :height))
               ))

(defun draw-text (canvas posx posy text &key (border-func (lambda (x y) (declare (ignore x) (ignore y)))) (anchor :nw))
  (funcall border-func posx posy)
  (format-wish "set _canvas_text \"~a\"~%
               ~a create text ~a ~a -anchor ~a -font \"Monospace\" -tags text -fill white -text $_canvas_text"
               text
               (widget-path canvas)
               posx posy
               (string-downcase anchor )
               ))

(defun setup-textarea (master rows columns)
  (let ((textarea (make-instance 'scrolled-text :master master)))
    (configure (textbox textarea)
               :height rows
               :width columns
               :wrap :word
               :state :disabled)
    (grid-forget (hscroll textarea))
    (grid-forget (vscroll textarea))
    textarea))

(defun set-textarea-text (tbox str &optional skip-ahead (draw-speed 100)) 
  (declare (type scrolled-text tbox) (type string str) (type (or nil t) skip-ahead))
  "Animate the display of text in the given textbox."
  (configure (textbox tbox) :state :normal)
  (setf (text tbox) "")
  (if (not skip-ahead)
    (loop for c in (coerce str 'list) do
          (progn
            ;play soundeffect for each letter excluding whitespace
            ;(when *text-sfx-enabled* 
              ;(if (not (string= c " ")) (play-character-sfx)))
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
                  (sleep (/ 1 draw-speed))
                  )
                ))
            ))
    ;else skip ahead
    (setf (text tbox) str))
  (configure (textbox tbox) :state :disabled))

(defun setup-canvas (master width height)
  (let* ((main-canvas (make-instance 'canvas
                                     :master master
                                     :width width 
                                     :height height
                                     :highlightthickness 0
                                     :borderwidth 0)))
    (pack main-canvas :side :top :fill :y :expand t)
    main-canvas))

(defun setup-font ()
  "Sets up fonts for the current window"
  (send-wish "font create myDefaultFont -family \"Monospace\" -size 11")
  (send-wish "option add *font myDefaultFont"))

(defun stylize-ui (text-widget)
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
                     (abort)))) () )

(defun quit-gui ()
  (loop for atimer in (sb-ext:list-all-timers)
        do (sb-ext:unschedule-timer atimer))
  (loop for athread in (sb-thread:list-all-threads)
        do (sb-thread:terminate-thread athread))
  (exit-wish))

(defun get-textarea () nil)
(defun get-canvas () nil)
(defun draw-textarea (main-canvas textarea &optional x y &key anchor)
  (if (and x y) 
    (create-window main-canvas x y textarea :anchor anchor)
    (create-window main-canvas
                   (/ (parse-integer (cget main-canvas :width)) 2)
                   (parse-integer (cget main-canvas :height)) textarea :anchor :s)))

(defun GUI (width height &key
                  (on-key (lambda (keychar) (format t "~a pressed~%" keychar)))
                  (on-init (lambda ()))
                  (on-update nil)
                  (update-interval 1)
                  )
  (with-ltk ()
    (minsize *tk* width height)
    (setup-font)
    (let* ((outside-frame (make-instance 'frame :master *tk*))
           (main-canvas (setup-canvas outside-frame width height))
           (main-text (setup-textarea main-canvas 
                                      (floor (/ (parse-integer (cget main-canvas :height)) 100))
                                      (floor (/ (parse-integer (cget main-canvas :width)) 10)))))
      (stylize-ui (textbox main-text))
      (clear-canvas main-canvas)
      (pack outside-frame :fill :x :expand t)

      (bind *tk* "<Key>" (lambda (evt)
                                (funcall on-key (format nil "~a" (event-char evt)))))

      ;functions that expose the gui to the user
      ;-----------------------------------------
      (defun get-textarea ()
        main-text)
      (defun get-canvas ()
        main-canvas)

      (draw-textarea main-canvas main-text)
      ;----------------------------------------.

      ;call the init callback
      (funcall on-init)
      ;start off the 1 second update function
      (when on-update
        (sb-ext:schedule-timer (sb-ext:make-timer (lambda ()
                                                    (sb-sys:with-interrupts
                                                      (funcall on-update))) :name "on-update-timer")
                               0
                               :repeat-interval update-interval))
      (on-close *tk* (lambda () (quit-gui)))
      )))
