(defpackage :branchie-systems
  (:use :cl :branchie-classes
            :branchie)
  (:export
    enemy-battle-simple
    ))

(in-package :branchie-systems)

;NOTE: When making branch systems
;keep in mind that only variables in *global-game-state* hash are stored
;that is, variables set with the function (set-glob-var 'symbol value)
;their active values (current game/save) can only be retrieved with (get-glob-var 'symbol)
(defmethod enemy-battle-simple ((player_sym symbol) 
                                (enemy_sym symbol) 
                                (branch_name symbol) 
                                &key 
                                victory-text 
                                victory-branch
                                bg-image
                                game-over-branch)
  (flet ((player () (get-glob-var player_sym))
         (enemy () (get-glob-var enemy_sym)))
    (let ((def_boost 0))
      (labels ((player-battle-turn-attack (b)
                 (format t "enemy image is:~S~%" (gi-image-path (gc-gi (enemy))))
                 (let ((roll (random (gc-att (player))))) 
                   (if (< (gc-def (enemy)) roll)
                     (progn
                       (gc-take-damage (enemy) (- roll (gc-def (enemy))))
                       (play-sfx #p"data/sound/sfx_damage.mp3")
                       (set-branch-actors b (list
                                              (list (enemy) :shake)))
                       (if (<= (gc-hp (enemy)) 0)
                         (progn
                           (set-branch-text b victory-text)
                           (set-branch-next b victory-branch))
                         (progn
                           (set-branch-text b (format nil "~a took ~a points of damage.~%It has ~a HP left." 
                                                      (gc-name (enemy))
                                                      (- roll (gc-def (enemy))) (gc-hp (enemy))))
                           (set-branch-next b 'enemy-battle-turn-branch)))
                       )
                     (progn
                       (set-branch-actors b (list
                                              (list (enemy) nil)))
                       (set-branch-text b "You didn't get through it's defenses.")))))
               (enemy-battle-turn (b)
                 (format t "in enemy-battle-turn~%")
                 (set-branch-actors b (list 
                                        (list (enemy) nil)))
                 (let ((decide (random 2)))
                   (cond
                     ((= decide 0)
                      (progn
                        (set-branch-text b (format nil "The ~a does nothing." (gc-name (enemy))))
                        ))
                     ((= decide 1) 
                      (let ((roll (random (gc-att (enemy)))))
                        (if (< (+ (gc-def (player)) def_boost) roll)
                          (progn
                            (let ((dmg (- roll (+ (gc-def (player)) def_boost))))
                              (gc-take-damage (player) dmg)
                              (play-sfx #p"data/sound/sfx_damage2.mp3")
                              (set-branch-actors b (list
                                                     (list (enemy) :red-flash)))
                              (set-branch-text b (format nil "The ~a attacks. You took ~a points of damage.~%You have ~a of HP left" 
                                                         (gc-name (enemy))
                                                         dmg
                                                         (gc-hp (player))))
                              ))
                          (set-branch-text b (format nil "The ~a attacks you but can't get through your defenses."
                                                     (gc-name (enemy))))))))
                   ;else if players hp is 0 or less
                   )
                 ))

        (br "Make your move"
            :name branch_name
            :image bg-image
            :characters (list
                          (list (enemy) nil))
            :action (action (b)
                      (if (<= (gc-hp (player)) 0)
                        (progn
                          (set-branch-text b (format nil "The ~a has defeated you." (gc-name (enemy))))
                          (set-branch-next b game-over-branch))))
            (choices
              (opt "Attack" (br "" :action (action (b)
                                             (setq def_boost 0)
                                             (player-battle-turn-attack b))
                                :next (br "" :action #'enemy-battle-turn
                                          :name 'enemy-battle-turn-branch
                                          :characters (list
                                                        (list (enemy) nil))
                                          :next branch_name)))
              (opt "Guard" (br ""
                               :action (action (b)
                                         (setq def_boost 10)
                                         (set-branch-text b (join-nl "You guard. Defense increased for 1 turn."
                                                                     (format nil "Your defense increased from ~a to ~a." (gc-def (player)) (+ (gc-def (player)) def_boost))))
                                         )
                               :characters (list
                                             (list (enemy) nil))
                               :next (br "" :action #'enemy-battle-turn
                                         :characters (list
                                                       (list (enemy) nil))
                                         :next branch_name)))))))))

