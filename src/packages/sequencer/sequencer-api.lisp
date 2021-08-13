;============================================================================
; om#: visual programming language for computer-assisted music composition
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
; File author: D. Bouche
;============================================================================
; SEQUENCER EDIT API
;=========================================================================

(in-package :om)

;;;=========================================
;;; PLAYER
;;;=========================================

(defmethod* m-play ((self OMSequencer) &optional trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "anything")
  :doc "Play sequencer"
  :icon 'm-play
  (declare (ignore trigger))
  (if (eq (state self) :pause)
      (player-continue-object (player (editor self)) self)
    (player-play-object (player (editor self)) self (editor self))))

(defmethod* m-pause ((self OMSequencer) &optional trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "anything")
  :doc "Pause sequencer"
  :icon 'm-pause
  (declare (ignore trigger))
  (player-pause-object (player (editor self)) self))

(defmethod* m-stop ((self OMSequencer) &optional trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "anything")
  :doc "Stop sequencer"
  :icon 'm-stop
  (declare (ignore trigger))
  (player-stop-object (player (editor self)) self))

(defmethod* m-loop ((self OMSequencer) t1 t2 &optional trigger)
  :initvals '(nil nil nil nil)
  :indoc '("sequencer" "start-time (nil or ms)" "end-time (nil or ms)" "anything")
  :doc "Loop sequencer"
  :icon 'm-loop
  (declare (ignore trigger))
  (editor-set-interval (editor self) `(,t1 ,t2)) )

(defmethod* m-set-time ((self OMSequencer) time &optional trigger)
  :initvals '(nil nil nil)
  :indoc '("sequencer" "time (ms)" "anything")
  :doc "Set sequencer time"
  :icon 'm-set-time
  (declare (ignore trigger))
  (if (integerp time)
      (set-object-current-time self (max 0 (round time)))))

(defmethod m-get-time ((self OMSequencer))
  (get-obj-time self))


;;;=========================================
;;; OBJECTS
;;;=========================================

(defmethod insert-object ((self OMSequencer) (object t) &key (time 0) (track 1) (pre-delay 0))
  (declare (ignore pre-delay))
  (let ((b (omng-make-new-boxcall (class-of object) (omp (or time 0) 0))))
    (setf (value b) `(,object))
    (set-display b :mini-view)
    (if (add-box-in-track self b track) t)))

(defmethod insert-object ((self OMSequencer) (object ompatchinternal) &key (time 0) (track 1) (pre-delay 0))
  (let ((b (omng-make-new-boxcall object (omp (or time 0) 0))))
    (set-reactive b t)
    (setf (pre-delay b) pre-delay)
    (set-display b :mini-view)
    (if (add-box-in-track self b track) t)))

(defmethod remove-object ((self OMSequencer) object &key multiple-instances)
  (let ((box (find object (boxes self) :key 'get-box-value)))
    (if box (omng-remove-element self box))
    (if multiple-instances
        (loop while (setq box (find object (boxes self) :key 'get-box-value))
              do
              (omng-remove-element self box)))))

(defmethod move-object ((self OMSequencer) object &key multiple-instances (delta-t 0) (delta-y 0) track)
  (if (and multiple-instances (listp delta-t) (listp track))
      (let ((boxes (loop for box in (sort (copy-list (boxes self)) '< :key 'get-box-onset)
                         when (and (get-box-value box) (eq (get-box-value box) object))
                         collect box)))
        (mapcar #'(lambda (box dt dy trk)
                    (set-box-onset box (max 0 (+ (get-box-onset box) dt)))
                    (setf (box-y box) (+ (box-y box) dy))
                    (if trk
                        (setf (group-id box) trk)))
                boxes
                delta-t
                delta-y
                track))
    (let ((box (find object (boxes self) :key 'get-box-value)))
      (set-box-onset box (max 0 (+ (get-box-onset box) delta-t)))
      (setf (box-y box) (+ (box-y box) delta-y))
      (if track
          (setf (group-id box) track)))))

(defmethod clear ((self OMSequencer) &optional track)
  (loop for box in (get-all-boxes self :track track)
        do
        (omng-remove-element self box)
        (delete-box-frame (frame box)) ;;; removes the view
        (omng-delete box) ;;; deals with contents/references
        ))


(defmethod s-add ((self OMSequencer) (object t) &key (time 0) (track 1) (pre-delay 0) trigger)
  (declare (ignore trigger))
  (insert-object self object :time time :track track :pre-delay pre-delay))

(defmethod s-remove ((self OMSequencer) object &key multiple-instances trigger)
  (declare (ignore trigger))
  (remove-object self object :multiple-instances multiple-instances))

(defmethod s-move ((self OMSequencer) object &key multiple-instances (delta-t 0) (delta-y 0) track trigger)
  (declare (ignore trigger))
  (move-object self object :multiple-instances multiple-instances :delta-t delta-t :delta-y delta-y :track track))

(defmethod s-clear ((self OMSequencer) &key (track nil))
  (clear seq track))
