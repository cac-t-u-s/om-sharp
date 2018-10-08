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
; File authors: J. Bresson, D. Bouche
;============================================================================

;=========================================================================
; MAQUETTE 2.0 !
;=========================================================================

(in-package :om)

;;===========================================================================
;;;OMMaquette = OM Sequencer
;;===========================================================================
;; Structure
(defclass OMMaquette (OMPatch schedulable-object timed-object) 
  ((ctrlpatch :accessor ctrlpatch :initform nil :initarg :ctrlpatch)
   (range :accessor range :initform '(:x1 0 :x2 20000 :y1 0 :y2 100) :initarg :range)
   ;;;Scheduler slot (t to only compute)
   (no-exec :accessor no-exec :initform nil :initarg :no-exec))
  (:metaclass omstandardclass))

(defclass OMMaquetteFile (OMPersistantObject OMMaquette) ()
  (:metaclass omstandardclass))

(defclass OMMaquetteInternal (OMMaquette) ()
  (:metaclass omstandardclass))

; (class-precedence-list (find-class 'ommaquettefile))

;;;===========================

(defmethod obj-file-extension ((self OMMaquette)) "omaq")
(defmethod get-object-type-name ((self OMMaquette)) "Maquette")
(defmethod object-doctype ((self OMMaquette)) :maquette)

(defmethod make-new-om-doc ((type (eql :maquette)) name)
  (make-instance 'OMMaquetteFile :name name))

(defmethod unregister-document ((self OMMaquette))
  (player-stop-object *general-player* self)
  (call-next-method))

(defmethod play-obj? ((self OMMaquette)) t)

(defmethod n-tracks ((self OMMaquette)) (n-tracks (editor self)))

(defmethod n-current-active-tracks ((self OMMaquette))
  (length (loop for box in (get-all-boxes self)
                if (box-being-rendered? self box)
                collect
                box)))

  
;;;===============================
;;; MAQUETTE CONTENTS (BOXES)
;;;===============================

(defmethod get-obj-dur ((self OMMaquette))
  (loop for tb in (get-all-boxes self) maximize (get-box-end-date tb)))

(defmethod get-all-boxes ((self OMMaquette) &key (sorted nil))
  (if sorted (sort (copy-list (boxes self)) '< :key 'get-box-onset) (copy-list (boxes self))))

(defmethod get-all-objects ((self OMMaquette) &key (sorted nil))
  (mapcar 'get-box-value (remove-if #'(lambda (obj) (eq (type-of obj) 'omlispfboxcall))
                                    (get-all-boxes self :sorted sorted))))

(defmethod get-box-onset ((self OMBox)) (box-x self))
(defmethod set-box-onset ((self OMBox) o) 
  (setf (box-x self) o)
  (when (get-box-value self) (set-object-onset (get-box-value self) o)))

;(defmethod get-box-onset ((self omboxpatch)) (box-x self))
;(defmethod set-box-onset ((self omboxpatch) o) 
;  (setf (box-x self) o)
;  (when (get-box-value self) (set-object-onset (get-box-value self) o)))

(defparameter *temporalbox-def-w* 1000)
(defmethod get-box-duration ((self OMBox)) (box-w self))
(defmethod set-box-duration ((self OMBox) d) 
  (setf (box-w self) (or d *temporalbox-def-w*))
  (when (get-box-value self) (set-object-interval (get-box-value self) (list 0 (box-w self))))
  (box-w self))

(defmethod get-box-end-date ((self OMBox)) (+ (get-box-onset self) (get-box-duration self)))
(defmethod get-box-time-span ((self OMBox)) (list (get-box-onset self) (get-box-end-date self)))

;;; the box has changed, it must be updated (depending on its context)
;;; for convenience we do not allow the box being smaller than 100ms
(defmethod contextual-update ((self OMBox) (container ommaquette))
  (set-object-onset (get-box-value self) (box-x self))
  (let ((duration (get-obj-dur (get-box-value self))))
    (when duration 
      ;;; (max 100 (or (get-obj-dur (get-box-value self)) *temporalbox-def-w*))
      (set-box-duration self duration)))
  (let ((view (get-view-from-mode (editor container))))
    (if (listp view)
        (om-invalidate-view (nth (1- (group-id self)) view))
        (om-invalidate-view view))))

;;;=========================================
;;; EVALUATION
;;;=========================================

;;; NOT GOOD !!! NEED TO EVAL JUST TERMINAL BOXES  
(defmethod eval-maquette ((maq OMMaquette))
  (loop for box in (get-all-boxes maq)
        ;when (not (all-reactive-p box))
        do
        (progn
          (eval-box box)
          (reset-cache-display box)
          (contextual-update box maq)
          ))
  ; (set-meta-inputs (ctrlpatch maq) (car (references-to maq)) maq)
  (mapcar 'eval-box (get-boxes-of-type (ctrlpatch maq) 'omoutbox))
  
  (clear-ev-once maq)
  ;(compile-patch (ctrlpatch maq))
  ;(apply (intern (string (compiled-fun-name (ctrlpatch maq))) :om) `(,maq))
  )


;;===========================================================================
;; Scheduling
;;===========================================================================

;;; called after user modifications o check if resecheduling is required
(defmethod box-being-rendered? ((self OMMaquette) (tb OMBox))
  (let ((current-time (get-obj-time self)))
    (and (> (get-box-end-date tb) current-time)
         (< (get-box-onset tb) current-time))))

(defmethod box-cross-interval ((tb OMBox) interval)
  (not (or (< (get-box-end-date tb) (car interval))
           (>= (get-box-onset tb) (cadr interval)))))

(defmethod get-computation-list-for-play ((self OMMaquette) &optional interval)
  (loop for box in (get-all-boxes self :sorted t)
        append
        (let ((b box))
          (if (and (all-reactive-p box)
                   (or (not interval)
                       (and interval
                            (in-interval (- (get-box-onset box) (pre-delay box)) interval :exclude-high-bound t)))
                   (not (ready b)))
              (progn
                (setf (ready b) t)
                (list (list (- (get-box-onset box) (pre-delay box))
                            (get-box-onset box)
                            #'(lambda ()
                                (with-schedulable-object self
                                                         (eval-box b))
                                (clear-ev-once b)
                                (reset-cache-display b)
                                (set-display b :value)
                                (contextual-update b self)))))))))

(defmethod get-action-list-for-play ((self OMMaquette) time-interval &optional parent)
  (sort 
   (if (not (no-exec self))
       (loop for box in (get-all-boxes self :sorted t)
             when (box-cross-interval box time-interval)
             when (not (and (all-reactive-p box) (not (ready box))))
             when (group-id box) ;;; only boxes in tracks are played
             append
             (let ((interval-in-object (list
                                        (max (- (car time-interval) (get-box-onset box)) 0)
                                        (min (- (cadr time-interval) (get-box-onset box)) (get-box-duration box)))))
               (mapcar 
                #'(lambda (b) (incf (car b) (get-box-onset box)) b)
                (get-action-list-for-play  
                 (play-obj-from-value (get-box-value box) box)
                 interval-in-object self)))))
   '< :key 'car))

(defmethod set-object-time ((self ommaquette) time)
  (set-time-callback self time)
  (call-next-method))

(defmethod set-time-callback ((self ommaquette) time)
  (let ((interval (or (interval self) (list time *positive-infinity*))))
    (loop for box in (get-all-boxes self :sorted t)
          when (get-box-value box)
          do
          (if (box-cross-interval box interval)
              (progn
                (set-object-interval (get-box-value box) (list (- time (get-box-onset box)) (cadr interval)))
                (if (in-interval time (list (get-box-onset box) (get-box-end-date box)))
                    (progn 
                      (set-object-time (get-box-value box) (- (car interval) (get-box-onset box)))
                      (set-time-callback (get-box-value box) (- time (get-box-onset box))))
                  (player-stop-object *general-player* (get-box-value box))))
            (player-stop-object *general-player* (get-box-value box))))))

#|
(with-schedulable-object 
   maq
   ;;;Move the object
   (setf (onset tb) (max (+ (onset tb) dx) 0))
   (if (box-being-rendered? maq tb)
       ;;;If the object was and is still being rendered, just set its time
       (set-object-time (object-from-box tb) (- (get-obj-time maq) (onset tb)))
     ;;;If the object was being rendered and is not anymore, stop- it
     (player-stop-object (player (editor maq)) (object-from-box tb))))
|#

(defmethod player-play-object ((self scheduler) (object ommaquette) caller &key parent interval)
  ;;;Ajouter ici la task begin : (mp:mailbox-send (taskqueue *engine*) *taskbegin*)
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object ommaquette))
  ;;;Pause all boxes under the maquette cursor (that is being rendered).
  ;;;Note : useful only for objects triggered by the maquette (hierarchical).
  ;(loop for box in (get-all-boxes object)
  ;      when (box-being-rendered? object box) 
  ;      do (player-pause-object self (get-box-value box)))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object ommaquette))
  ;;;Continue all boxes under the maquette cursor (that is being rendered).
  ;;;Note : useful only for objects triggered by the maquette (hierarchical).
  ;(loop for box in (get-all-boxes object)
  ;      when (box-being-rendered? object box) 
  ;      do (player-continue-object self (get-box-value box)))
  (call-next-method))

(defmethod player-stop-object ((self scheduler) (object ommaquette))
  ;;;Stop all boxes under the maquette cursor (that is being rendered).
  ;;;Note : useful only for objects triggered by the maquette (hierarchical).
  (loop for box in (get-all-boxes object)
        do 
        (player-stop-object self (get-box-value box)))

  ;;;Ajouter ici la task end : (mp:mailbox-send (taskqueue *engine*) *taskend*)
  
  (call-next-method))


;;;===============================
;;; MODIFY/UPDATE MAQUETTE CONTENTS 
;;;===============================

;(defmethod allowed-element ((self OMMaquette) (elem t)) (call-next-method))
(defmethod allowed-element ((self OMMaquette) (elem timed-object)) t)

(defmethod omNG-add-element ((self OMMaquette) (elem OMBox))
  (set-box-duration elem (or (get-obj-dur (get-box-value elem)) 
                             (box-w elem)))
  ;; (unless (group-id elem) (setf (group-id elem) 1))   
  (with-schedulable-object self (call-next-method)))

;; evaluate the patch before ?
(defmethod omNG-add-element ((maq OMMaquette) (tb OMBoxPatch))
  ;(omng-box-value tb 0)
  (call-next-method))

(defmethod omNG-add-element ((maq OMMaquette) (tb OMBoxEditCall))
  (setf (lock-state tb) :locked)
  (call-next-method))

(defmethod omng-remove-element ((maq OMMaquette) (tb OMBox))
  ;;;If the box is under the maquette cursor (that is being rendered), stop it.
  ;;;Note : useful only for objects triggered by the maquette (hierarchical).
  (if (box-being-rendered? maq tb) (player-stop-object *general-player* (get-box-value tb)))
  ;;;Perform the remove operation asking the scheduler for a replan.
  (with-schedulable-object maq (call-next-method)))

(defmethod move-box-in-maquette ((maq OMMaquette) (tb OMBox) &key (dx 0) (dy 0))
  (with-schedulable-object
   maq 
   ;;; this is +/- like move-box (with set-box-onset)
   (set-box-onset tb (max (+ (get-box-onset tb) dx) 0))
   (setf (box-y tb) (+ (box-y tb) dy))
   
   (when (frame tb) (update-frame-to-box-position tb))

   (when (container tb)
     (report-modifications (editor (container tb))))
   
   (update-connections tb)
   
   ;;; maquette-specific
   (if (and (get-box-value tb) (eq (get-object-state (get-box-value tb)) :play))
       (let ((ti (get-obj-time maq)))
         (if (in-interval ti (list (get-box-onset tb) (get-box-end-date tb)))
             (set-object-time (get-box-value tb) (- ti (get-box-onset tb)))
           (player-stop-object *general-player* (get-box-value tb)))))
   
   ))


;;;======================================
;;;SIMULATE TRACKS USING BOXES' GROUP-ID
;;;======================================

(defmethod get-track-boxes ((self OMMaquette) tracknum &key (sorted nil))
  (remove-if-not #'(lambda (id) (and (numberp id) (= id tracknum))) 
                 (get-all-boxes self :sorted sorted)
                 :key 'group-id))

(defmethod get-track-objects ((self OMMaquette) tracknum  &key (sorted nil))
  (mapcar 'get-box-value (get-track-boxes self tracknum :sorted sorted)))

(defmethod add-box-in-track ((maquette OMMaquette) (tb OMBox) tracknum)
  (setf (group-id tb) tracknum)
  (let* ((yrange (- (getf (range maquette) :y2) (getf (range maquette) :y1)))
         (trackw (round yrange (n-tracks maquette)))
         (y (+ (getf (range maquette) :y1) 
               (if tracknum (* (- (n-tracks maquette) (1- tracknum)) trackw) (round yrange 2)))))
    (setf (box-y tb) y 
          (box-h tb) (- (round yrange 10)))
  (omNG-add-element maquette tb)
  (om-invalidate-view (nth (1- tracknum) (get-g-component (editor maquette) :track-views)))))

#| 
(with-schedulable-object 
   maquette
   (when (< tracknum (length (tracks maquette)))
     (setf (nth tracknum (tracks maq))
           (sort (cons tb (nth tracknum (tracks maquette))) '< :key 'onset))))
|#

(defmethod set-track-gain ((self OMMaquette) tracknum gain)
  (loop for obj in (get-track-objects self tracknum)
        do (set-object-gain obj gain)))

(defmethod set-object-gain ((self t) gain)
  (print (format nil "No redefinition of set-object-gain found for ~A" (type-of self))))

(defmethod set-track-pan ((self OMMaquette) tracknum pan)
  (loop for obj in (get-track-objects self tracknum)
        do (set-object-pan obj pan)))

(defmethod set-object-pan ((self t) gain)
  (print (format nil "No redefinition of set-object-pan found for ~A" (type-of self))))



;;;=========================================
;;; TIME MARKERS METHODS
;;;=========================================

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-time-markers ((self ommaquette))
  "returns a list of time markers"
  (flat (loop for box in (boxes self)
              collect
              (get-time-markers box))))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-elements-for-marker ((self ommaquette) marker)
  "returns a list of elements matching the marker if not elements, it retuns "
  (loop for box in (boxes self) 
        collect
        (list box (get-elements-for-marker box marker))))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod translate-elements-from-time-marker ((self ommaquette) elems dt)
  "translates elements from a time marker with dt"
  (loop for elem in elems
        do
        (when (not (member nil (cdr elem)))
          (temporal-translate-points (car elem) (cdr elem) dt))))

  
