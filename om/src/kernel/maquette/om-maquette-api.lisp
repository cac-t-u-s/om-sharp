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
; File author: D. Bouche
;============================================================================
; MAQUETTE EDIT API
;=========================================================================

(in-package :om)

;;;=========================================
;;; VISIBLE API
;;;=========================================
(defmethod* m-play ((self ommaquette) &optional trigger)
   :initvals '(nil nil)
   :indoc '("maquette" "anything")
   :doc "Play maquette"
   :icon 'm-play
   (declare (ignore trigger))
   (if (eq (state self) :pause)
       (player-continue-object (player (editor self)) self)
     (player-play-object (player (editor self)) self (editor self))))

(defmethod* m-pause ((self ommaquette) &optional trigger)
   :initvals '(nil nil)
   :indoc '("maquette" "anything")
   :doc "Pause maquette"
   :icon 'm-pause
   (declare (ignore trigger))
   (player-pause-object (player (editor self)) self))

(defmethod* m-stop ((self ommaquette) &optional trigger)
   :initvals '(nil nil)
   :indoc '("maquette" "anything")
   :doc "Stop maquette"
   :icon 'm-stop
   (declare (ignore trigger))
   (player-stop-object (player (editor self)) self))

(defmethod* m-loop ((self ommaquette) t1 t2 &optional trigger)
   :initvals '(nil nil nil nil)
   :indoc '("maquette" "start-time (nil or ms)" "end-time (nil or ms)" "anything")
   :doc "Loop maquette"
   :icon 'm-loop
   (declare (ignore trigger))
   (editor-set-interval (editor self) `(,t1 ,t2)) )

(defmethod* m-set-time ((self OMMaquette) time &optional trigger)
   :initvals '(nil nil nil)
   :indoc '("maquette" "time (ms)" "anything")
   :doc "Set maquette time"
   :icon 'm-set-time
   (declare (ignore trigger))
   (if (integerp time)
       (set-object-time self (max 0 (round time)))))

(defmethod* m-without-exec ((self OMMaquette) &optional trigger)
   :initvals '(nil nil)
   :indoc '("maquette" "anything")
   :doc "Mute maquette actions (computations only)"
   :icon 'm-without-exec
   (declare (ignore trigger))
    (with-schedulable-object self (setf (no-exec self) t)))

(defmethod* m-with-exec ((self OMMaquette) &optional trigger)
   :initvals '(nil nil)
   :indoc '("maquette" "anything")
   :doc "Mute maquette actions (computations only)"
   :icon 'm-with-exec
   (declare (ignore trigger))
   (with-schedulable-object self (setf (no-exec self) nil)))




(defmethod m-add ((self ommaquette) (object t) &key (time 0) (track 1) (pre-delay 0) trigger)
  (declare (ignore trigger))
  (insert-object self object :time time :track track :pre-delay pre-delay))

(defmethod m-remove ((self ommaquette) object &key multiple-instances trigger)
  (declare (ignore trigger))
  (remove-object self object :multiple-instances multiple-instances))

(defmethod m-move ((self ommaquette) object &key multiple-instances (delta-t 0) (delta-y 0) track trigger)
  (declare (ignore trigger))
  (move-object self object :multiple-instances multiple-instances :delta-t delta-t :delta-y delta-y :track track))

(defmethod m-get-time ((self OMMaquette))
  (get-obj-time self))

(defmethod m-objects ((self OMMaquette) &key (sorted t) (track nil))
  (if track 
      (get-track-objects self track :sorted sorted)
    (get-all-objects self :sorted sorted)))

(defmethod m-flush ((self ommaquette) &key (track nil))
  (loop for box in (if track (get-track-boxes self track) (get-all-boxes self))
        do (omng-remove-element self box)))



(defmethod set-patch-inputs ((self OMPatchInternal) (defvals list))
  (loop for inp in (get-inputs self)
        for val in defvals
        do (setf (defval inp) val))
  (compile-patch self)
  self)

;;;=========================================
;;; HIDDEN API
;;;=========================================

(defmethod insert-object ((self OMMaquette) (object t) &key (time 0) (track 1) (pre-delay 0) trigger)
  (declare (ignore trigger))
  (declare (ignore pre-delay))
  (let ((b (omng-make-new-boxcall (class-of object) (omp (or time 0) 0))))
    (setf (value b) `(,object))
    (set-display b :mini-view)
    (if (add-box-in-track self b track) t)))

(defmethod insert-object ((self OMMaquette) (object ompatchinternal) &key (time 0) (track 1) (pre-delay 0) trigger)
  (declare (ignore trigger))
  (let ((b (omng-make-new-boxcall object (omp (or time 0) 0))))
    (set-reactive b t)
    (setf (pre-delay b) pre-delay)
    (set-display b :mini-view)
    (if (add-box-in-track self b track) t)))



(defmethod remove-object ((self ommaquette) object &key multiple-instances trigger)
  (declare (ignore trigger))
  (let ((box (find object (boxes self) :key 'get-box-value)))
    (if box (omng-remove-element self box))
    (if multiple-instances
        (loop while (setq box (find object (boxes self) :key 'get-box-value))
              do
              (omng-remove-element self box)))))

(defmethod move-object ((self ommaquette) object &key multiple-instances (delta-t 0) (delta-y 0) track trigger)
  (declare (ignore trigger))
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



(defmethod prune ((maquette ommaquette) (object t) t1-ms t2-ms)
  (prune-object object t1-ms t2-ms)
  (loop for box in (boxes maquette)
        when (eq (get-box-value box) object)
        do (prune-box box t1-ms t2-ms)))



;;;=========================================
;;;TODO
;;;=========================================


(defmethod connect-objects-in-maquette ((self ommaquette) object1 object2 out1 in2 &key (trigger nil))
  (declare (ignore trigger))
  nil)

(defmethod disconnect-objects-in-maquette ((self ommaquette) object1 object2 out1 in2 &key (trigger nil))
  (declare (ignore trigger))
  nil)


;;;=================ALEX

(defun freq-cycle (freqlist address port counter)
  (osc-send (list address (print (nth (mod counter (length freqlist)) freqlist))) "127.0.0.1" port))

(defmethod flush-maquette-track ((self ommaquette) tracknum)
  (loop for box in (get-track-boxes self tracknum)
        do
        (omng-remove-element self box))
  (build-editor-window (editor self)))

(defmethod flush-maquette ((self ommaquette))
  (loop for box in (get-all-boxes self)
        do
        (omng-remove-element self box)))
  

