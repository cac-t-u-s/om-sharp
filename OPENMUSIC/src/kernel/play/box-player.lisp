(in-package :om)

;;;=================================
;;; GENERAL PLAYER: USED IN PATCH EDITORS
;;;=================================

(defvar *general-player* nil)
(defvar *play-boxes* nil)

(defun init-om-player ()
  (setf *general-player*
        (make-player :reactive-player;:dynamic-scheduler  
                     :run-callback 'play-editor-callback
                     :callback-tick 50
                     :stop-callback 'stop-editor-callback)))

(defun abort-om-player ()
  (destroy-player *general-player*)
  (setf *general-player* nil))

;(abort-om-player)
;(init-om-player)
(add-om-init-fun 'init-om-player)


(defmethod get-obj-to-play ((self ombox)) (play-obj-from-value (car (value self)) self))
(defmethod play-obj-from-value (value box) value)
(defmethod play-box? ((self t)) nil)
(defmethod play-box? ((self OMBoxEditCall)) t)
(defmethod play-obj? ((self t)) nil)
(defmethod get-obj-dur ((self t)) nil)
(defmethod get-obj-dur ((self null)) nil)

(defmethod additional-player-params ((self omboxeditcall))
  (list :port (get-edit-param self :outport)
        :approx (get-edit-param self :approx)))

#|
(defun box-player-stop (caller)
  (declare (ignore caller))
  (mapcar #'(lambda (box)
              (setf (play-state box) nil)
              (if (frame box)
                  (om-invalidate-view (frame box))))
          *play-boxes*)
  (setf *play-boxes* nil))

(defun box-player-callback (caller time)
  (declare (ignore caller))
  (handler-bind ((error #'(lambda (e) 
                            (print (format nil "~A" e))
                            (om-kill-process (callback-process *general-player*))
                            (abort e))))
    (mapc #'(lambda (box) (draw-cursor-at box time)) *play-boxes*)
    ))
|#

(defun box-player-stop (caller)
  (when caller
    (setf (play-state caller) nil)
    (when (frame caller) 
      (setf (box-play-time (frame caller)) 0)
      (om-invalidate-view (frame caller)))))
  
(defun box-player-callback (caller time)
  (handler-bind ((error #'(lambda (e)
                            (print (format nil "~A" e))
                            ;(om-kill-process (callback-process *general-player*))
                            (abort e))))
    (draw-cursor-at caller time)))


(defmethod play-editor-callback ((self t) time) nil)
(defmethod stop-editor-callback ((self t)) (box-player-stop self))

(defmethod play-editor-callback ((self OMBox) time)
  (box-player-callback self time))

(defun draw-cursor-at (box time)
  (let* ((frame (frame box)))
    (setf (box-play-time frame) time)  ; (- time (play-state box))))
    (om-invalidate-view frame)))


(defmethod play-boxes ((boxlist list))
  (let ((list2play (remove-if-not 'play-box? boxlist)))
    (mapcar #'(lambda (box)
                (when (play-obj? (car (value box)))
                  (player-play-object *general-player* (get-obj-to-play box) box)
                  (setf (play-state box) t)
                  (push box *play-boxes*)
                  (om-invalidate-view (frame box))))
            list2play)
    (when *play-boxes*
      ;(player-set-time-interval *general-player* 0 (+ now (loop for box in list2play maximize (get-obj-dur (get-obj-to-play box)))))
      ;;; ça fait rien (avec p-obj-player)
      (player-start *general-player*)
      )))

; (print (get-edit-param box 'player))
(defmethod stop-boxes ((boxlist list))
  (mapc #'(lambda (box)
            (when (play-obj? (car (value box)))
              (player-stop-object *general-player* (car (value box)))
              ;;; ABORT THE OBJECT !!
              (setf (play-state box) nil)
              (setf *play-boxes* (remove box *play-boxes*))
              (om-invalidate-view (frame box))
              ))
        boxlist)
  (unless *play-boxes* (player-stop *general-player*)))


(defmethod play/stop-boxes ((boxlist list))
  (let ((play-boxes (remove-if-not 'play-box? boxlist)))
    (if (find-if 'play-state play-boxes)
        ;;; stop all
        (mapc #'(lambda (box) 
                  (player-stop-object *general-player* (get-obj-to-play box))
                  (setf (play-state box) nil)
                  (om-invalidate-view (frame box)))
              play-boxes)
      ;;; start all
      (mapc #'(lambda (box) 
                (player-play-object *general-player* (get-obj-to-play box) box)
                (setf (play-state box) t)
                (om-invalidate-view (frame box)))
            play-boxes))))

(defmethod stop-all-boxes ()
  (stop-boxes *play-boxes*))




    