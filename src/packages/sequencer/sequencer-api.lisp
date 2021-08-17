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

(defmethod* s-play ((self OMSequencer) &optional trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "anything")
  :doc "Play <self> (a sequencer).

<trigger> allows connecting anything else to be evaluated at the same time."
  :icon 's-play
  (declare (ignore trigger))
  (when (editor self)
    (if (eq (state self) :pause)
        (player-continue-object (player (editor self)) self)
      (player-play-object (player (editor self)) self (editor self)))
    (button-select (play-button (editor self)))
    (button-unselect (pause-button (editor self)))
    t))

(defmethod* s-pause ((self OMSequencer) &optional trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "anything")
  :doc "Pause  <self> (a sequencer).

<trigger> allows connecting anything else to be evaluated at the same time."
  :icon 's-pause
  (declare (ignore trigger))
  (when (editor self)
    (player-pause-object (player (editor self)) self)
    (button-select (pause-button (editor self)))
    (button-unselect (play-button (editor self)))
    t))

(defmethod* s-stop ((self OMSequencer) &optional trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "anything")
  :doc "Stop  <self> (a sequencer).

<trigger> allows connecting anything else to be evaluated at the same time."
  :icon 's-stop
  (declare (ignore trigger))
  (when (editor self)
    (player-stop-object (player (editor self)) self)
    (button-unselect (play-button (editor self)))
    (button-unselect (pause-button (editor self)))
    t))

(defmethod* s-loop ((self OMSequencer) t1 t2 &optional trigger)
  :initvals '(nil nil nil nil)
  :indoc '("sequencer" "start-time (nil or ms)" "end-time (nil or ms)" "anything")
  :doc "Set the sequencer (<self>) loop interval from <t1> to <t2>. Set the loop on if either <t1> or <t2> is not null.

<trigger> allows connecting anything else to be evaluated at the same time."
  :icon 's-loop
  (declare (ignore trigger))
  (let* ((editor (editor self))
         (begin (or t1 0))
         (end (or t2 begin))
         (loop-on (and (or t1 t2) t)))
    (when editor
      (editor-set-interval (editor self) (list begin end))
      (editor-set-loop (editor self) loop-on)
      (if loop-on
          (button-select (repeat-button (editor self)))
        (button-unselect (repeat-button (editor self))))
      t)))

(defmethod* s-set-time ((self OMSequencer) (time integer) &optional trigger)
  :initvals '(nil nil nil)
  :indoc '("sequencer" "time (ms)" "anything")
  :doc "Set current play-time of <self> (a sequencer) to <time>.

<trigger> allows connecting anything else to be evaluated at the same time."
  :icon 's-time
  (declare (ignore trigger))
  (set-object-current-time self (max 0 (round time)))
  (reset-boxes self)
  time)

(defmethod* s-get-time ((self OMSequencer))
  :indoc '("sequencer")
  :doc "Get current play-time of <self> (a sequencer)."
  :icon 's-time
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


(defmethod* s-add ((seq OMSequencer) (object t) &key (time 0) (track 1) (pre-delay 0) trigger)
  :initvals '(nil nil 0 1 0 nil)
  :indoc '("sequencer" "a playable object" "onset (ms)" "track number" "pre-delay for reactive eval" "anything")
  :doc "Adds a box in the sequencer <seq>, track <track>, at time <time> containing <object> (a playable musical object).

<pre-delay> sets this property for advanced computation when the box is reactive.

<trigger> allows connecting anything else to be evaluated at the same time."
  (declare (ignore trigger))
  (insert-object seq object :time time :track track :pre-delay pre-delay))

(defmethod* s-remove ((seq OMSequencer) object &key multiple-instances trigger)
  :initvals '(nil nil nil nil)
  :indoc '("sequencer" "a playable object from the sequencer" "remove all instances" "anything")
  :doc "Removes <object> from the sequencer <seq>, track <track>, at time <time> containing <object> (a playable musical object).

If <multiple-instances> is not NIL, will try to eliminate all possible instances of <object> found in <seq>.

<trigger> allows connecting anything else to be evaluated at the same time."
  (declare (ignore trigger))
  (remove-object seq object :multiple-instances multiple-instances))

(defmethod* s-move ((seq OMSequencer) object &key multiple-instances (delta-t 0) (delta-y 0) track trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "a playable object from the sequencer" "move all instances" "time shift (ms)" "verticql shift" "track number" "anything")
  :doc "Moves <object> (an object from the sequencer <seq>):
- in time (with a shift of <delta-t>
- vertically (maquette view only) with a shift of <delta-y>
- to another track <track>

If <multiple-instances> is not NIL, will try to eliminate all possible instances of <object> found in <seq>.

<trigger> allows connecting anything else to be evaluated at the same time."
  (declare (ignore trigger))
  (move-object seq object :multiple-instances multiple-instances :delta-t delta-t :delta-y delta-y :track track))

(defmethod* s-clear ((seq OMSequencer) &key (track nil) trigger)
  :initvals '(nil nil)
  :indoc '("sequencer" "track number")
  :doc "Removes all boxes in <seq>, or in <track> if a track number is given."
  (declare (ignore trigger))
  (clear seq track))
