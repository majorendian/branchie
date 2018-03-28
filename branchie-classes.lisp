(defpackage :branchie-classes
  (:use :cl)
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

