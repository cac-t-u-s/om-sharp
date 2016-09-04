(in-package :om)

(defmethod general-play ((player omplayer))
  (let ((start-t (or (car (play-interval player)) 0))
        (end-t (or (cadr (play-interval player )) 3600000)))
    (cond ((equal (state player) :play)
           ;;; prolonge la durée de vie du player
           (setf (stop-time player) (max (stop-time player) end-t)))
        
          (t 
           (setf (stop-time player) end-t)
           (when (callback-process player)
             (om-kill-process (callback-process player)))
           (when (scheduling-process player)
             (om-kill-process (scheduling-process player)))
           
           (when (callback-fun player)
             (om-with-priority 10
               (setf (callback-process player)
                     (om-run-process "editor player callback"
                                     #'(lambda ()
                                         (loop 
                                          (funcall (callback-fun player) (caller player) (get-player-time player))
                                          (sleep (callback-tick player))
                                          ))))))
           (when (loop-play player) 
             (mapcar #'(lambda (pl) (player-set-loop pl start-t end-t)) 
                     (engines player)))
           ;(mapcar #'player-start (engines player) 
           ;        (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
           (setf (state player) :play
                 (start-time player) start-t
                 (ref-clock-time player) (clock-time))))))