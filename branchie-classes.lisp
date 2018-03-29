(defpackage :branchie-classes
  (:use :cl :ltk)
  (:export
    game-image
    gi-posx
    gi-posy
    gi-image
    gi-image-path
    gi-preload

    game-character
    gc-name
    gc-image
    gc-hp
    gc-say
    gc-att
    gc-def
    gc-dex
    gc-posx
    gc-posy
    gc-change-name
    gc-take-damage
    gc-gi
    gc-image-preload
    
    create-character
    quest
    quest-log
    new-quest
    add-to-quest-log
    find-quest

    display-point
    display-point-x
    display-point-y
    display-point-set-x
    display-point-set-y

    init-display
    display
    display-canvas
    display-height
    display-width
    display-text-position
    display-image
    display-imagepath
    display-clear
    display-center-x
    display-center-text-x
    display-center-y
    display-center-text-y
    display-draw-branch-options
    display-draw-characters
    display-draw-image
    display-handle-animation

    image-cache-preload-gc
    ))

(in-package :branchie-classes)

(defclass game-image ()
  ((posx :accessor gi-posx :initarg :x :initform 0)
   (posy :accessor gi-posy :initarg :y :initform 0)
   (image :accessor gi-image :initarg :image :initform nil)
   (image-path :accessor gi-image-path :initarg :image-path :initform nil)
   (preload :reader gi-preload :initarg :preload :initform nil)))

;after instance has been created, modify suplied arguments
(defmethod initialize-instance :after ((gi game-image)
                                        &key ((:image img)))
  ;(format t "instance of game-image initialized with image:~S~%" img)
  )

; character helper classes
(defclass game-character (game-image)
  ((name :accessor gc-name :initarg :name :initform "UNNAMED")
   (image :accessor gc-image :initarg :imagepath :initform nil)
   (image-preload :accessor gc-image-preload :initarg :preload :initform nil)
   (game-image :accessor gc-gi :initarg :game-image :initform nil)
   (hp :accessor gc-hp :initarg :hp :initform 100)
   (att :accessor gc-att :initarg :att :initform 1)
   (dex :accessor gc-dex :initarg :dex :initform 1)
   (def :accessor gc-def :initarg :def :initform 1)
   (posx :reader gc-posx :initarg :x :initform 0)
   (posy :reader gc-posy :initarg :y :initform 0)))

(defmethod initialize-instance :after ((gc game-character) &rest r)
  (setf (gc-gi gc) (make-instance 'game-image
                                  :x (gc-posx gc) :y (gc-posy gc)
                                  :image-path (gc-image gc)
                                  :preload (gc-image-preload gc)))
  (format t "image of game image:~S~%" (gi-image-path (gc-gi gc))))

(defmethod gc-change-name ((gc game-character) (name string))
  (setf (gc-name gc) name))

(defmethod gc-say ((gc game-character) (str string))
  (format nil "~a: ~a" (gc-name gc) str))

(defmethod create-character ((character-name string))
  (make-instance 'game-character :name character-name))

;this specific method is used mainly for battle systems
;not every character is an enemy but we want the player
;to have the freedom to engage in battle with most if not all
;in case they decide to be mean
(defmethod gc-take-damage ((gc game-character) dmg)
  (setf (gc-hp gc) (- (gc-hp gc) dmg)))

(defmethod gc-change-name ((gc game-character) (name string))
  (setf (gc-name gc) name))


;pack definition for game image
(conspack:defencoding game-image
  posx posy image image-path)

;pack definition for game character
(conspack:defencoding game-character
  name image game-image hp att dex def posx posy)
;----

; quest helper classes
(defclass quest ()
  ((status :accessor quest-status :initarg :qstatus :initform nil)
   (name :accessor quest-name :initarg :name)))

(defclass quest-log ()
  ((quests :accessor quest-log-quests :initarg :quests :initform (list))))

(defmethod new-quest ((qname string))
  (make-instance 'quest :name qname))

(defmethod add-to-quest-log ((qlog quest-log) (q quest))
  (push q (quest-log-quests qlog)))

(defun find-quest-helper (qlog_list qname)
  (if qlog_list
    (if (string= (quest-name (first qlog_list)) qname)
      (first qlog_list)
      (find-quest-helper (rest qlog_list) qname))))

(defmethod find-quest ((qlog quest-log) (qname string))
  (find-quest-helper (quest-log-quests qlog) qname))


;========= Image cache ===============
(defparameter *display-image-cache* (make-hash-table))
(defmethod image-cache-symbol-from ((filepath pathname))
  (when (not (namestring filepath))
    (error "Filepath cannot be NIL"))
  (let ((convstring (string-upcase (format nil "CACHED_~A" (namestring filepath))) ))
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
  (format t "display cachee:~%")
  (maphash (lambda (k v) (format t "k:~S v:~S~%" k v)) *display-image-cache*)
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

(defmethod image-cache-preload-gc (game-state-hash)
  (maphash (lambda (v gc)
             (when (and (typep gc 'game-character) (gc-image-preload gc))
               (image-cache (gi-image-path (gc-gi gc))))) game-state-hash))

;=====================================


; === graphic related class ===
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

(defmethod display-draw-branch-options ((d display) (options string))
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
               options
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

(defmethod display-handle-animation ((d display) animation imgpath )
  (cond 
    ((eql :slide animation) (display-draw-image-slide-in d imgpath))
    ((null animation) (display-draw-image d imgpath (/ (display-width d) 2) (/ (display-height d) 2)))))

(defmethod display-draw-characters ((d display) (actors list))
  (labels ((animate-character (pair)
             (let ((gc (first pair))
                   (anim (second pair)))
               (format t "Animating ~S with ~S~%" (gc-name gc) anim)   
               (cond
                 ((null anim) (display-wish-draw-image d gc))
                 ((eql anim :shake) (display-draw-image-shake-x d gc))
                 ((eql anim :attack) (display-wish-draw-image d gc))
                 ((eql anim :overlay) (display-wish-draw-image d gc (display-center-x d) (display-center-y d)))
                 ((eql anim :red-flash) (display-effect-silly d))
                 )
               )))
    (map 'nil #'animate-character actors)))

(defmethod display-effect-silly ((d display))
  (loop
    do (progn
         (display-clear d)
         (display-redraw-current-image d)
         (format-wish "~a create rectangle ~a ~a ~a ~a -fill ~a" (widget-path (display-canvas d))
                      0
                      (- (display-point-y (display-get-var d 'aux-point)) 100)
                      (display-width d)
                      (display-point-set-y (display-get-var d 'aux-point) (+ 50 (display-point-y (display-get-var d 'aux-point))))
                      "red")
         (when (> (display-point-y (display-get-var d 'aux-point)) (+ 200 (display-height d)))
           (progn
             (display-point-set-y (display-get-var d 'aux-point) 0)
             (return)))
         (sleep 0.02))))
