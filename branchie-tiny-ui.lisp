(defpackage :branchie-tiny-ui
  (:use :cl :branchie-core :ltk)
  (:export GUI
           set-textbox-text
           get-textbox))

(in-package :branchie-tiny-ui)

(defun clear-canvas (canvas)
  (clear canvas)
  (format-wish "~a create rectangle 0 0 ~a ~a -fill black"
               (widget-path canvas)
               (parse-integer (cget canvas :width)) (parse-integer (cget canvas :height))
               ))

(defun draw-text (canvas posx posy text &optional (border-func (lambda (x y) (declare (ignore x) (ignore y)))))
  (funcall border-func posx posy)
  (format-wish "set _canvas_text \"~a\"~%
               ~a create text ~a ~a -anchor nw -font \"Monospace\" -fill white -text $_canvas_text"
               text
               (widget-path canvas)
               posx posy
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

(defun set-textbox-text (tbox str &optional skip-ahead (draw-speed 100)) 
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

(defun GUI (width height &key
                  (key-handler (lambda (keychar) (format t "~a pressed~%" keychar)))
                  (redraw-func (lambda (canv textarea) (declare (ignore canv) (ignore textarea)))))
  (with-ltk ()
    (minsize *tk* width height)
    (setup-font)
    (let* (
           (outside-frame (make-instance 'frame :master *tk*))
           (main-canvas (setup-canvas outside-frame width height))
           (main-text (setup-textarea main-canvas 4 100)))
      (stylize-ui (textbox main-text))
      (clear-canvas main-canvas)
      (create-window main-canvas
                     (/ (parse-integer (cget main-canvas :width)) 2)
                     (parse-integer (cget main-canvas :height)) main-text :anchor :s)
      (pack outside-frame :fill :x :expand t)
      (bind *tk* "<Key>" (lambda (evt)
                           (declare (ignore evt))
                           (funcall key-handler (event-char evt))))
      (defun get-textbox ()
        main-text)
      )))
