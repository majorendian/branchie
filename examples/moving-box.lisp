(require :branchie)
(defpackage :moving-box
  (:use :cl :branchie-tiny-ui :ltk))

(in-package :moving-box)

(defclass square ()
  ((width :accessor square-width :initarg :width)
   (height :accessor square-height :initarg :height)
   (x :accessor square-x :initarg :x)
   (y :accessor square-y :initarg :y)
   (tag :accessor square-tag :initarg :tag)))

(defparameter *box* (make-instance 'square
                                   :x 50
                                   :y 50
                                   :width 100
                                   :height 100
                                   :tag "box"))

(defparameter *keydown-w* nil)
(defparameter *keydown-a* nil)
(defparameter *keydown-s* nil)
(defparameter *keydown-d* nil)

(defun main ()
  (GUI 960 480
       :on-init (lambda ()
                  (set-textarea-text (get-textarea) "Move the box around with WASD")
                  (format-wish "~a create rectangle ~a ~a ~a ~a -fill white -tags ~a" (widget-path (get-canvas))
                               (square-x *box*) (square-y *box*)
                               (square-width *box*) (square-height *box*)
                               (square-tag *box*)
                               ))
       :on-update (lambda ()
                    (let ((mx 0)
                          (my 0))
                      (when *keydown-w*
                        (setq my -5)
                        )
                      (when *keydown-a*
                        (setq mx -5)
                        )
                      (when *keydown-s*
                        (setq my 5)
                        )
                      (when *keydown-d*
                        (setq mx 5)
                        )
                      (format-wish "~a move ~a ~a ~a" (widget-path (get-canvas))
                                   (square-tag *box*)
                                   mx my))
                    ;    (format t "~a~%" (canvas-bbox (get-canvas) "box"))
                    (setf *keydown-w* nil)
                    (setf *keydown-a* nil)
                    (setf *keydown-s* nil)
                    (setf *keydown-d* nil)
                )
       :on-key (lambda (key)
                 (cond
                   ((string= key "W") (setf *keydown-w* t))
                   ((string= key "A") (setf *keydown-a* t))
                   ((string= key "S") (setf *keydown-s* t))
                   ((string= key "D") (setf *keydown-d* t))))
       :update-interval 0.01))

(main)
