;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson, D. Bouche
;============================================================================

(in-package :om)

;;;=================================
;;; GENERAL PLAYER: USED IN PATCH EDITORS
;;;=================================

(defvar *general-player* nil)
(defvar *play-boxes* nil)

(defun init-om-player ()
  (setf *general-player*
        (make-player :reactive-player 
                     :run-callback 'play-editor-callback
                     :callback-tick 50
                     :stop-callback 'stop-editor-callback)))


;(register *general-player*)

(defun abort-om-player ()
  (destroy-player *general-player*)
  (setf *general-player* nil))

;(abort-om-player)
;(init-om-player)

(add-om-init-fun 'init-om-player)


(defmethod get-obj-to-play ((self ombox)) 
  (play-obj-from-value (car (value self)) self))

(defmethod play-obj-from-value (val box) val)


;;; when the value of a box is another box...
(defmethod play-obj-from-value ((val ombox) box) 
  (play-obj-from-value (car (value val)) val))

(defmethod play-box? ((self t)) nil)
(defmethod play-box? ((self OMBoxEditCall)) t)
(defmethod play-obj? ((self t)) nil)
(defmethod get-obj-dur ((self t)) nil)
(defmethod get-obj-dur ((self null)) nil)

(defmethod get-obj-dur ((self ombox)) 
  (get-obj-dur (get-obj-to-play self)))

(defmethod additional-player-params ((self omboxeditcall))
  (list :port (get-edit-param self :outport)
        :approx (get-edit-param self :approx)))




(defmethod play-box-callback ((self OMBox) time)
  (handler-bind ((error #'(lambda (e)
                            (print (format nil "~A" e))
                            ;(om-kill-process (callback-process *general-player*))
                            (abort e))))
    (let* ((frame (frame self)))
      (set-box-play-time frame time)  ; (- time (play-state box))))
      (om-invalidate-view frame))
    ))


(defmethod start-box-callback ((self OMBox))
  (setf (play-state self) t)
  (when (frame self) 
    (om-invalidate-view (frame self))))

(defmethod stop-box-callback ((self OMBox))
  (setf (play-state self) nil)
  (when (frame self) 
      (set-box-play-time (frame self) 0)
      (om-invalidate-view (frame self))))




;;; called from OM / user
(defun box-player-start (box)
  (when box
    (start-box-callback box)
    (when (editor box) (start-editor-callback (editor box)))
    ))

(defun box-player-stop (box)
  (when box
    (stop-box-callback box)
    (when (editor box) (stop-editor-callback (editor box)))
    ))


;;; called by the player
(defmethod play-editor-callback ((self OMBox) time)
  (play-box-callback self time)
  (when (editor self) (play-editor-callback (editor self) time)))

(defmethod stop-editor-callback ((self OMBox)) 
  (box-player-stop self)
  (when (editor self) (stop-editor-callback (editor self))))





;;; called from OM action
(defmethod play-boxes ((boxlist list))
  (let ((list2play (remove-if-not 'play-box? boxlist)))
    (mapcar #'(lambda (box)
                (when (play-obj? (get-obj-to-play box))
                  (player-play-object *general-player* (get-obj-to-play box) box)
                  (box-player-start box)
                  (push box *play-boxes*)
                  ))
            list2play)
    (when *play-boxes*
      ;(player-set-time-interval *general-player* 0 (+ now (loop for box in list2play maximize (get-obj-dur (get-obj-to-play box)))))
      ;;; ça fait rien (avec p-obj-player)
      (player-start *general-player*)
      )))

(defmethod stop-boxes ((boxlist list))
  (mapc #'(lambda (box)
            (when (play-obj? (get-obj-to-play box))
              (player-stop-object *general-player* (get-obj-to-play box))
              ;;; ABORT THE OBJECT !!
              (box-player-stop box)
              (setf *play-boxes* (remove box *play-boxes*))
              ))
        boxlist)
  (unless *play-boxes* (player-stop *general-player*)))


(defmethod play/stop-boxes ((boxlist list))
  (let ((play-boxes (remove-if-not 'play-box? boxlist)))
    (if (find-if 'play-state play-boxes)
        ;;; stop all
        (mapc #'(lambda (box) 
                  (player-stop-object *general-player* (get-obj-to-play box))
                  (box-player-stop box)
                  )
              play-boxes)
      ;;; start all
      (mapc #'(lambda (box) 
                (player-play-object *general-player* (get-obj-to-play box) box)
                (box-player-start box)
                )
            play-boxes))))

(defmethod stop-all-boxes ()
  (stop-boxes *play-boxes*))


