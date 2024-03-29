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
; File authors: J. Bresson, D. Bouche
;============================================================================

;=========================================================================
; OMSequencer = MAQUETTE 2.0 !
;=========================================================================

(in-package :om)

(defclass OMSequencer (OMPatch schedulable-object timed-object)
  ((ctrlpatch :accessor ctrlpatch :initform nil :initarg :ctrlpatch)
   (range :accessor range :initform '(:x1 0 :x2 20000 :y1 0 :y2 100) :initarg :range)
   (view-mode :accessor view-mode :initarg :view-mode :initform :tracks)     ;;; :tracks or :maquette
   (control-patch-visible-p :accessor control-patch-visible-p :initarg :control-patch-visible-p :initform nil))
  (:metaclass omstandardclass))

(defclass OMSequencerFile (OMPersistantObject OMSequencer) ()
  (:default-initargs :icon :sequencer-file)
  (:metaclass omstandardclass))

(add-om-doctype :sequencer "oseq" "Sequencer")

(defclass OMSequencerInternal (OMSequencer) ()
  (:default-initargs :icon :sequencer)
  (:metaclass omstandardclass))


;;;===========================

(defmethod get-object-type-name ((self OMSequencer)) "Sequencer")
(defmethod object-doctype ((self OMSequencer)) :sequencer)

(defmethod make-new-om-doc ((type (eql :sequencer)) name)
  (make-instance 'OMSequencerFile :name name))


;; For conversions
(defmethod internalized-type ((self OMSequencerFile)) 'OMSequencerInternal)
(defmethod externalized-type ((self OMSequencer)) 'OMSequencerFile)
(defmethod externalized-icon ((self OMSequencer)) :sequencer-file)


(defmethod type-check ((type (eql :sequencer)) obj)
  (let ((seq (ensure-type obj 'OMSequencer)))
    (when seq
      (change-class seq 'OMSequencerFile)
      (setf (icon seq) :sequencer-file))
    seq))

(defmethod unregister-document ((self OMSequencer))
  (player-stop-object *general-player* self)
  (call-next-method))

(defmethod play-obj? ((self OMSequencer)) t)

(defmethod n-tracks ((self OMSequencer)) (n-tracks (editor self)))

(defmethod n-current-active-tracks ((self OMSequencer))
  (length (loop for box in (get-all-boxes self)
                if (box-being-rendered? self box)
                collect
                box)))

(defmethod copy-contents ((from OMSequencer) (to OMSequencer))
  (let ((rep (call-next-method)))
    (set-control-patch rep (om-copy (ctrlpatch from)))
    (setf (range rep) (range from))
    rep))


;;;===============================
;;; SEQUENCER CONTENTS (BOXES)
;;;===============================

(defmethod get-obj-dur ((self OMSequencer))
  (loop for tb in (get-all-boxes self) maximize (get-box-end-date tb)))


(defmethod get-all-boxes ((self OMSequencer) &key (sorted nil) (track nil))
  (let* ((boxes (get-boxes self)))

    (when track
      (setf boxes
            (remove-if-not #'(lambda (id) (and (numberp id) (= id track)))
                           boxes
                           :key 'group-id)))

    (when sorted
      (setf boxes (sort boxes '< :key 'get-box-onset)))

    boxes))


(defmethod get-box-onset ((self OMBox)) (box-x self))
(defmethod set-box-onset ((self OMBox) o)
  (setf (box-x self) o)
  (when (get-box-value self)
    (set-object-onset (get-box-value self) o)))

;(defmethod get-box-onset ((self omboxpatch)) (box-x self))
;(defmethod set-box-onset ((self omboxpatch) o)
;  (setf (box-x self) o)
;  (when (get-box-value self) (set-object-onset (get-box-value self) o)))

(defparameter *temporalbox-def-w* 1000)
(defmethod get-box-duration ((self OMBox)) (box-w self))
(defmethod set-box-duration ((self OMBox) d)
  (setf (box-w self) (or d *temporalbox-def-w*))
  (when (get-box-value self)
    (set-object-interval (get-box-value self) (list 0 (box-w self))))
  (box-w self))

(defmethod get-box-end-date ((self OMBox)) (+ (get-box-onset self) (get-box-duration self)))
(defmethod get-box-time-span ((self OMBox)) (list (get-box-onset self) (get-box-end-date self)))

;;; the box has changed, it must be updated (depending on its context)
;;; for convenience we do not allow the box being smaller than 100ms
(defmethod contextual-update ((self OMBox) (container OMSequencer))
  (let ((object (get-box-value self)))
    (set-object-onset object (box-x self))
    (let ((duration (and (play-obj? object) (get-obj-dur object))))
      (when duration
        ;;; (max 100 (or (get-obj-dur (get-box-value self)) *temporalbox-def-w*))
        (set-box-duration self duration)))
    (when (editor container)
      (let ((view (get-view-from-mode (editor container))))
        (if (listp view)
            ;;; tracks mode
            (when (group-id self)
              (om-invalidate-view (nth (1- (group-id self)) view)))
          ;;; maquette mode
          (update-temporalboxes view)
          )))))

;;;=========================================
;;; EVALUATION
;;;=========================================

;;; NOT GOOD !!! NEED TO EVAL JUST TERMINAL BOXES
(defmethod eval-sequencer ((seq OMSequencer) &optional (with-control-patch t) (from-editor nil))

  (when from-editor
    (setf *current-eval-panel* (get-editor-view-for-action (editor seq))))

  (loop for box in (get-all-boxes seq)
        when (not (find-if #'connections (outputs box)))
        do
        (progn
          (eval-box box)
          (reset-cache-display box)
          (contextual-update box seq)
          ))

  ; (set-meta-inputs (ctrlpatch seq) (car (references-to seq)) seq)

  (when (and from-editor (ctrlpatch seq) (editor (ctrlpatch seq)))
    (setf *current-eval-panel* (editor-view (ctrlpatch seq))))

  (when with-control-patch
    (mapcar 'eval-box (get-boxes-of-type (ctrlpatch seq) 'omoutbox)))

  (clear-ev-once seq)
  (clear-ev-once (ctrlpatch seq))

  (when from-editor
    (setf *current-eval-panel* nil))

  ;(compile-patch (ctrlpatch seq))
  ;(apply (intern (string (compiled-fun-name (ctrlpatch seq))) :om) `(,seq))
  )

;;===========================================================================
;; Scheduling
;;===========================================================================

;;; called after user modifications o check if resecheduling is required
(defmethod box-being-rendered? ((self OMSequencer) (tb OMBox))
  (let ((current-time (get-obj-time self)))
    (and (> (get-box-end-date tb) current-time)
         (< (get-box-onset tb) current-time))))

(defmethod box-cross-interval ((tb OMBox) interval)
  (not (or (< (get-box-end-date tb) (car interval))
           (>= (get-box-onset tb) (cadr interval)))))

(defmethod get-computation-list-for-play ((self OMSequencer) &optional interval)
  (loop for box in (get-all-boxes self :sorted t)
        append
        (let ((b box))
          (when (and (find-if 'reactive (outputs box))
                     (or (not interval)
                         (in-interval (- (get-box-onset box) (pre-delay box)) interval :exclude-high-bound t))
                     (not (ready b)) ;; avoids computing it several times
                     )
            (setf (ready b) t)
            (list (list (- (get-box-onset box) (pre-delay box))
                        (get-box-onset box)
                        #'(lambda ()
                            ;; (with-schedulable-object self (eval-box b))
                            ;; with-schedulable-object has undesired effects when used in a loop
                            (eval-box b)
                            (clear-ev-once b)
                            (reset-cache-display b)
                            (set-display b :value)
                            (contextual-update b self)
                            )))
            ))))


(defmethod reset-box ((self OMBox)) nil)
(defmethod reset-box ((self OMBoxAbstraction)) (setf (ready self) nil))

(defmethod reset-boxes ((self OMSequencer))
  (loop for tb in (get-all-boxes self)
        do (reset-box tb)))


;;; from scheduler functions
(defmethod reset-I :before ((self OMSequencer) &optional date)
  (reset-boxes self))


(defmethod get-action-list-for-play ((self OMSequencer) time-interval &optional parent)
  (sort
   (loop for box in (get-all-boxes self :sorted t)
         when (box-cross-interval box time-interval)
         ;; if it's a reactive box it must be "ready" (= computed)
         ;; when (not (and (find-if 'reactive (outputs box)) (not (ready box))))
         when (group-id box) ;;; only boxes in tracks are played
         append
         (let ((interval-in-object (list
                                    (max (- (car time-interval) (get-box-onset box)) 0)
                                    (min (- (cadr time-interval) (get-box-onset box)) (get-box-duration box)))))
           (mapcar
            #'(lambda (b) (incf (car b) (get-box-onset box)) b)
            (get-action-list-for-play
             (play-obj-from-value (get-box-value box) box)
             interval-in-object self))))
   '< :key 'car))

(defmethod set-object-current-time ((self OMSequencer) time)
  (set-time-callback self time)
  (call-next-method))


(defmethod set-time-callback ((self t) time) nil)

(defmethod set-time-callback ((self OMSequencer) time)
  (let ((interval (or (interval self) (list time *positive-infinity*))))
    (loop for box in (get-all-boxes self :sorted t)
          do (let ((object (get-box-value box)))
               (if (box-cross-interval box interval)
                   (progn
                     (set-object-interval object (list (- time (get-box-onset box)) (cadr interval)))
                     (if (in-interval time (list (get-box-onset box) (get-box-end-date box)))
                         (progn
                           (set-object-current-time object (- (car interval) (get-box-onset box)))
                           (set-time-callback object (- time (get-box-onset box))))
                       (player-stop-object *general-player* object)))
                 (player-stop-object *general-player* object))))))

#|
(with-schedulable-object
   seq
   ;;;Move the object
   (setf (onset tb) (max (+ (onset tb) dx) 0))
   (if (box-being-rendered? seq tb)
       ;;;If the object was and is still being rendered, just set its time
       (set-object-current-time (object-from-box tb) (- (get-obj-time seq) (onset tb)))
     ;;;If the object was being rendered and is not anymore, stop- it
     (player-stop-object (player (editor seq)) (object-from-box tb))))
|#

(defmethod player-play-object ((self scheduler) (object OMSequencer) caller &key parent interval)
  ;;;Ajouter ici la task begin : (mp:mailbox-send (taskqueue *engine*) *taskbegin*)
  (declare (ignore parent interval))
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object OMSequencer))
  ;;;Pause all boxes under the sequencer cursor (that is being rendered).
  ;;;Note : useful only for objects triggered by the sequencer (hierarchical).
  ;(loop for box in (get-all-boxes object)
  ;      when (box-being-rendered? object box)
  ;      do (player-pause-object self (get-box-value box)))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object OMSequencer))
  ;;;Continue all boxes under the sequencer cursor (that is being rendered).
  ;;;Note : useful only for objects triggered by the sequencer (hierarchical).
  ;(loop for box in (get-all-boxes object)
  ;      when (box-being-rendered? object box)
  ;      do (player-continue-object self (get-box-value box)))
  (call-next-method))

(defmethod player-stop-object ((self scheduler) (object OMSequencer))
  ;;;Stop all boxes under the sequencer cursor (that is being rendered).
  ;;;Note : useful only for objects triggered by the sequencer (hierarchical).
  (loop for box in (get-all-boxes object)
        do
        (player-stop-object self (get-box-value box)))

  ;;;Ajouter ici la task end : (mp:mailbox-send (taskqueue *engine*) *taskend*)

  (call-next-method))


;;;===============================
;;; MODIFY/UPDATE SEQUENCER CONTENTS
;;;===============================

(defmethod allowed-element ((self OMSequencer) (elem time-sequence)) t)

(defmethod allowed-element ((self OMSequencer) (elem timed-object))
  (plusp (get-obj-dur elem))) ;; no object that have no duration => must be put in containers

(defmethod allowed-element ((self OMSequencer) (elem OMBoxEditCall))
  (allowed-element self (get-box-value elem)))

(defmethod allowed-element ((self OMSequencer) (elem OMComment)) nil)


(defmethod omNG-add-element ((self OMSequencer) (elem OMBox))
  (set-box-duration elem (or (and (play-obj? (get-box-value elem))
                                  (get-obj-dur (get-box-value elem)))
                             (box-w elem)))
  ;; (unless (group-id elem) (setf (group-id elem) 1))
  (with-schedulable-object self (call-next-method)))

;; evaluate the patch before ?
(defmethod omNG-add-element ((seq OMSequencer) (tb OMBoxPatch))
  ;(omng-box-value tb 0)
  (call-next-method))

(defmethod omNG-add-element ((seq OMSequencer) (tb OMBoxEditCall))
  (setf (lock-state tb) :locked)
  (call-next-method))

(defmethod omng-remove-element ((seq OMSequencer) (tb OMBox))
  ;;;If the box is under the sequencer cursor (that is being rendered), stop it.
  ;;;Note : useful only for objects triggered by the sequencer (hierarchical).
  (when (box-being-rendered? seq tb)
    (player-stop-object *general-player* (get-box-value tb)))
  ;;;Perform the remove operation asking the scheduler for a replan.
  (with-schedulable-object seq (call-next-method)))

(defmethod move-box-in-sequencer ((seq OMSequencer) (tb OMBox) &key (dx 0) (dy 0))
  ;;; this is +/- like move-box (with set-box-onset)
  (set-box-onset tb (max (+ (get-box-onset tb) dx) 0))
  (setf (box-y tb) (+ (box-y tb) dy))

  (when (frame tb) (update-frame-to-box-position tb))

  (when (container tb)
    (report-modifications (editor (container tb))))

  (update-connections tb)

  ;;; specific
  (if (and (get-box-value tb) (eq (get-object-state (get-box-value tb)) :play))
      (let ((ti (get-obj-time seq)))
        (if (in-interval ti (list (get-box-onset tb) (get-box-end-date tb)))
            (set-object-current-time (get-box-value tb) (- ti (get-box-onset tb)))
          (player-stop-object *general-player* (get-box-value tb)))))
  )


;;;======================================
;;;SIMULATE TRACKS USING BOXES' GROUP-ID
;;;======================================

(defmethod add-box-in-track ((seq OMSequencer) (tb OMBox) tracknum)
  (setf (group-id tb) tracknum)
  (let* ((yrange (- (getf (range seq) :y2) (getf (range seq) :y1)))
         (trackw (round yrange (n-tracks seq)))
         (y (+ (getf (range seq) :y1)
               (if tracknum (* (- (n-tracks seq) (1- tracknum)) trackw) (round yrange 2)))))
    (setf (box-y tb) y
          (box-h tb) (round yrange 10))
    (omNG-add-element seq tb)
    (om-invalidate-view (nth (1- tracknum) (get-g-component (editor seq) :track-views)))))


(defmethod* get-objects ((self OMSequencer) &key (sorted nil) (track nil))
  :initvals '(nil nil nil)
  :indoc '("a sequencer" "sort the boxes by onset?" "select a specific track (number)")
  :doc "Returns the objects/values of the boxes of <self>, or from the track <track> if specified, sorted by onset if <sorted>."
  (mapcar
   #'get-box-value
   (remove-if-not #'group-id (get-all-boxes self :sorted sorted :track track))))


;;;=========================================
;;; TIME MARKERS METHODS
;;;=========================================

;;; Note: this is not used as long as sequencers are
;;; not embedded in other sequencers

(defmethod get-time-markers ((self OMSequencer))
  (loop for box in (boxes self)
        append (get-time-markers box)))

(defmethod get-time-markers ((self OMBox))
  (when (and (subtypep (type-of (get-box-value self)) 'time-sequence)
             (show-markers self))
    (get-time-markers (get-box-value self))))

(defmethod get-elements-for-marker ((self OMSequencer) marker)
  (loop for box in (boxes self)
        collect
        (list box (get-elements-for-marker box marker))))

(defmethod get-elements-for-marker ((self OMBox) marker)
  (when (get-box-value self)
    (get-elements-for-marker (get-box-value self) marker)))

(defmethod translate-elements-from-time-marker ((self OMSequencer) elems dt)
  (loop for elem in elems
        do (when (not (member nil (cdr elem)))
             (temporal-translate-points (car elem) (cdr elem) dt))))


(defmethod set-property ((self OMBox) (prop-id (eql :show-markers)) val)
  (call-next-method)
  ;;; in order to update the rulers
  (when (editor (container self))
    (editor-invalidate-views (editor (container self)))))


;;;=================================
;;; PERSISTENCE / OM-SAVE
;;;=================================

(defmethod save-patch-contents ((self OMSequencer) &optional (box-values nil))
  (append
   (call-next-method self t)
   `((:range ,(range self))
     (:control-patch ,(omng-save (ctrlpatch self)))
     (:loop-interval ,(interval self))
     (:loop-on ,(looper self))
     (:view-mode ,(view-mode self))
     (:control-patch-visible-p ,(control-patch-visible-p self))
     )))


(defmethod load-patch-contents ((patch OMSequencer) data)
  (let ((seq (call-next-method))
        (patch (find-value-in-kv-list data :control-patch))
        (range (find-value-in-kv-list data :range))
        (interval (find-value-in-kv-list data :loop-interval))
        (loop-on (find-value-in-kv-list data :loop-on))
        (view-mode (find-value-in-kv-list data :view-mode))
        (control-patch-visible-p (find-value-in-kv-list data :control-patch-visible-p)))
    (when patch (set-control-patch seq (omng-load patch)))
    (when range (setf (range seq) range))
    (when interval (setf (interval seq) interval))
    (setf (looper seq) loop-on)
    (when view-mode (setf (view-mode seq) view-mode))
    (setf (control-patch-visible-p seq) control-patch-visible-p)
    seq))

(defmethod om-load-from-id ((id (eql :sequencer)) data)
  (let ((seq (make-instance 'OMSequencerInternal
                            :name (find-value-in-kv-list data :name))))
    (load-patch-contents seq data)
    seq))



(defmethod omng-save-relative ((self OMSequencerFile) ref-path)
  `(:sequencer-from-file
    ,(if (mypathname self)
         (omng-save (relative-pathname (mypathname self) ref-path))
       (omng-save (pathname (name self))))))


(defmethod om-load-from-id ((id (eql :sequencer-from-file)) data)

  (let* ((path (omng-load (car data)))
         (checked-path (and (pathname-directory path)  ;; normal case
                            (check-path-using-search-path path)))
         (seq
          (if checked-path

              (load-doc-from-file checked-path :sequencer)

            ;;; no pathname-directory can occur while loading old patch abstractions from OM6
            ;;; in this case we look for a not-yet-save file with same name in registered documents
            (let ((registered-entry (find (pathname-name path) *open-documents*
                                          :test 'string-equal
                                          :key #'(lambda (entry) (name (doc-entry-doc entry))))))
              (when registered-entry
                (doc-entry-doc registered-entry)))
            )))

    (unless seq
      (om-beep-msg "SEQUENCER FILE NOT FOUND: ~S !" path)
      (setf seq (make-instance'OMSequencerFile :name (pathname-name path)))
      (setf (mypathname seq) path))

    seq))
