
(defpackage :branchie-sound
  (:use :cl :harmony-simple)
  (:export play-character-sfx
           play-sfx
           play-music
           set-character-sfx
           set-default-bg-music
           set-bg-music
           stop-music
           *text-sfx-enabled*
           *music-enabled*
           *sfx-enabled*
           *default-bg-music*
           ))

(in-package :branchie-sound)

(defparameter *game-music-source* nil) ;populated on first play

(defparameter *character-sfx-file* #p"data/sound/char-sfx-1.mp3")
(defmethod set-character-sfx ((soundfile_mp3 pathname))
  (setf *character-sfx-file* soundfile_mp3))
(defparameter *default-bg-music* #p"data/sound/Seedling.mp3") ;Seedling.mp3 has been created by Walid Feghali
(defmethod set-bg-music ((soundfile_mp3 pathname))
  (setf *default-bg-music* soundfile_mp3))

(defparameter *current-bg-music* nil)

;--- In game settings ---

(defparameter *text-sfx-enabled* nil)
(defparameter *music-enabled* nil)
(defparameter *sfx-enabled* nil)

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
 
