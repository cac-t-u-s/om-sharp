;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
; File author: J. Bresson
;============================================================================

;;;===========================
;;; DRAW SCORE OBJECT BOXES
;;;===========================

(in-package :om)

;; we need MultiCacheBoxEditCall as some score-object (POLY/MULTI-SEQ) 
;; are subclasses of COLLECTION
(defclass ScoreBoxEditCall (MultiCacheBoxEditCall) 
  ((fontsize :accessor fontsize)))

(defmethod special-box-type ((class-name (eql 'score-element))) 'ScoreBoxEditCall)

(defmethod default-size ((self ScoreBoxEditCall)) 
  (score-object-default-box-size (get-box-value self)))

(defmethod score-object-default-box-size ((self t)) (omp 80 50))
(defmethod score-object-default-box-size ((self chord)) (omp 80 80))
(defmethod score-object-default-box-size ((self note)) (omp 80 80))

(add-preference-section :appearance "Score Boxes")
(add-preference :appearance :score-font "Score font size" '(10 12 14 16 18 20 24 28 32) 18)

(defmethod get-box-fontsize ((self ScoreBoxEditCall)) 
  (or (fontsize self) 
      (get-pref-value :appearance :score-font)))

(defmethod set-box-fontsize ((self ScoreBoxEditCall) size) 
  (setf (fontsize self) size))

  
(defmethod display-modes-for-object ((self score-element))
  '(:mini-view :text :hidden))

(defmethod additional-box-attributes ((self score-element)) 
  `((:font-size "a font size for score display" nil) 
    (:staff "default staff configuration" 
     ,(loop for s in *score-staff-options* collect (list (string-upcase s) s)))
    (:scale "default scale" 
     ,(loop for s in *all-scales* collect (list (string (car s)) (car s))))
    ))

(defmethod miniview-time-to-pixel-proportional ((object score-element) box view time)

  (let* ((fontsize (get-box-fontsize box))
         (unit (font-size-to-unit fontsize))
         (shift-x-pix (* (score-mini-view-left-shift-in-units box) unit)))
    (+ shift-x-pix ;; left margin
        *miniview-x-margin*
        (* (- (w view) (* *miniview-x-margin* 2) shift-x-pix)   
           (/ time (if (plusp (get-obj-dur object)) (get-obj-dur object) 1000))))
    ))

(defmethod miniview-time-to-pixel-rhythmic ((object score-element) box view time)

  (let* ((fontsize (get-box-fontsize box))
         (unit (font-size-to-unit fontsize))
         (shift-x-pix (* (score-mini-view-left-shift-in-units box) unit))
         (time-map (get-edit-param box :time-map)))
    
    (+ shift-x-pix
       (* unit (score-time-to-units time-map time)))
    ))

;;; all objects (except voice/poly) on a box
(defmethod miniview-time-to-pixel ((object score-element) box (view omboxframe) time) 
  (miniview-time-to-pixel-proportional object box view time))

;;; voice on a box
(defmethod miniview-time-to-pixel ((object voice) box (view omboxframe) time)
  (if (equal (display box) :mini-view)
      (miniview-time-to-pixel-rhythmic object box view time)
    (miniview-time-to-pixel-proportional object box view time)))

(defmethod miniview-time-to-pixel ((object poly) box (view omboxframe) time)
  (if (equal (display box) :mini-view)
      (miniview-time-to-pixel-rhythmic object box view time)
    (miniview-time-to-pixel-proportional object box view time)))

(defmethod miniview-time-to-pixel ((object multi-seq) box (view omboxframe) time)
  (miniview-time-to-pixel-proportional object box view time))


;;; an objects in the maquette tracks...
(defmethod miniview-time-to-pixel ((object score-element) box (view sequencer-track-view) time)
  (let ((tt (if (listp time) (car time) time))) ;;; can happen that time is a list (see draw-measure)
    (- (time-to-pixel view (+ (box-x box) tt)) 
       (time-to-pixel view (box-x box)))
    ))

(defun score-mini-view-left-shift-in-units (box)
  (if (equal (get-edit-param box :staff) :empty)
      1 5))


(defmethod compute-font-size ((self score-element) (box OMBox) h y)
  
  (let* ((font-size (get-pref-value :appearance :score-font))
         (unit (font-size-to-unit font-size))
         (y-in-units (/ y unit)))

    (when (> (num-voices self) 0)

      (let* ((n-lines-max (+ (loop for staff in (list! (get-edit-param box :staff))
                                   maximize 
                                   (let ((staff-lines (apply 'append (mapcar 'staff-lines (staff-split staff)))))
                                     (- (car (last staff-lines)) (car staff-lines))))  ;;; range of the staff lines 
                             8)) ;; + margin
             (h-per-voice (/ h (num-voices self)))
             (draw-box-h (* n-lines-max unit)))
                  
        (if (> draw-box-h h-per-voice)
          ;;; there's no space: reduce font ?
          (setf unit (- unit (/ (- draw-box-h h-per-voice) n-lines-max))
                font-size (unit-to-font-size unit))
           ;;; there's space: draw more in the middle
           (setf y-in-units (+ y-in-units (/ (round (- h-per-voice draw-box-h) 2) unit))))
        ))
    
    (values font-size y-in-units)))

  
  
(defmethod draw-mini-view ((self score-element) (box OMBox) x y w h &optional time)
  
  (om-draw-rect x y w h :fill t :color (om-def-color :white))
  
  (multiple-value-bind (fontsize y-u)
      (compute-font-size self box h y)

    (set-box-fontsize box fontsize)
 
  (om-with-fg-color (om-make-color 0.0 0.2 0.2)
    (score-object-mini-view self box x y y-u w h))
  
  ))



;;; draw on a collection box...
(defmethod collection-draw-mini-view ((type score-element) list box x y w h time)
  (let ((voice-h (if list (/ h (length list)) h)))
    (loop for voice in list
          for i from 0
          do (let ((yy (+ y 4 (* i voice-h))))
               (om-draw-rect x yy w (- voice-h 2) :fill t :color (om-def-color :white))
               (score-object-mini-view voice box x yy 0 w voice-h)
               ))
    ))


;;; Hacks for patch-boxes / collection boxes etc.

(defmethod get-edit-param ((self OMBoxAbstraction) param)
  (case param
    (:staff :g)
    (otherwise nil)))

(defmethod get-box-fontsize ((self OMBox)) (get-pref-value :appearance :score-font))
(defmethod set-box-fontsize ((self OMBox) size) nil)


;;; what do we do with other objects ?
(defmethod score-object-mini-view ((self t) box x-pix y-pix y-u w h) t)

;;;===========================
;;; NOTE
;;;===========================

;;; note has no editor (at the moment) so the font-size is not used
(defmethod additional-box-attributes ((self note)) 
  `((:staff "default staff configuration" 
     ,(loop for s in *score-staff-options* collect (list (string-upcase s) s)))
    (:scale "default scale" 
     ,(loop for s in *all-scales* collect (list (string (car s)) (car s))))
    ))

(defmethod score-object-mini-view ((self note) box x-pix y-pix y-u w h)
  
  (let ((staff (get-edit-param box :staff))
        (scale (get-edit-param box :scale))
        (font-size (get-box-fontsize box))
        (in-sequencer? (typep (frame box) 'sequencer-track-view)))
    
    (draw-staff x-pix y-pix y-u w h font-size staff :margin-l 1 :margin-r 1 
                :keys (not in-sequencer?))

    (draw-chord self
                0 
                2 y-u 
                0 y-pix w h 
                font-size 
                :scale scale :staff staff
                :stem NIL
                :time-function #'(lambda (time) (declare (ignore time)) (/ w 2))
                )
    ))


;;; super-hack to edit the note as a slider
(defmethod editable-on-click ((self ScoreBoxEditCall)) 
  (special-box-click-action (get-box-value self) self)
  nil)

(defmethod special-box-click-action ((self t) box) nil)

(defmethod special-box-click-action ((self note) box) 
  
  (let* ((frame (frame box))
         (h (- (h frame) (* 2 *miniview-y-margin*)))
         (position (om-mouse-position frame))
         (y-position (- (om-point-y position) *miniview-y-margin*)))
    
    (when (or (om-action-key-down)
              (and (om-view-container frame) 
                   (container-frames-locked (om-view-container frame))))
      
      (multiple-value-bind (fontsize y-u)
          (compute-font-size self box h *miniview-y-margin*)
      
        (let* (; (fontsize (get-box-fontsize box))
             (unit (font-size-to-unit fontsize))
             (staff (get-edit-param box :staff))
             (scale (get-the-scale (get-edit-param box :scale)))
             (shift (+ (calculate-staff-line-shift staff) y-u))
             (click-y-in-units (- shift (/ y-position unit)))
             (clicked-pitch (line-to-pitch click-y-in-units scale)))

          (setf (midic self) clicked-pitch)
          
          (om-init-temp-graphics-motion 
           frame position nil 
           :motion #'(lambda (view pos)
                       (let* ((y-position (- (om-point-y pos) *miniview-y-margin*))
                              (click-y-in-units (- shift (/ y-position unit)))
                              (clicked-pitch (line-to-pitch click-y-in-units scale)))
                         (unless (equal clicked-pitch (midic self))
                           (setf (midic self) clicked-pitch)
                           (self-notify box)
                           (om-invalidate-view view)
                     ))))
          
          (self-notify box)
          (update-after-eval box)
          
          )))))


;;;===========================
;;; CHORD
;;;===========================
(defmethod score-object-mini-view ((self chord) box x-pix y-pix y-u w h)
  
  (let ((staff (get-edit-param box :staff))
        (scale (get-edit-param box :scale))
        (in-sequencer? (typep (frame box) 'sequencer-track-view))
        (font-size (get-box-fontsize box)))
    
    (draw-staff x-pix y-pix y-u w h font-size staff :margin-l 1 :margin-r 1 
                :keys (not in-sequencer?))

    (when (notes self)
      (draw-chord self 0 0 y-u x-pix y-pix w h font-size 
                  :scale scale :staff staff
                  :time-function #'(lambda (time) (declare (ignore time)) (/ w 2))
                  ))
    ))


;;;===========================
;;; CHORD-SEQ
;;;===========================
(defmethod score-object-mini-view ((self chord-seq) box x-pix y-pix y-u w h)
  
  (let* ((staff (get-edit-param box :staff))
         (scale (get-edit-param box :scale))
         (offsets (get-edit-param box :offsets))
         (font-size (get-box-fontsize box))
         (in-sequencer? (typep (frame box) 'sequencer-track-view)))
        
    (when (listp staff)
      (setf staff (or (nth (position self (get-voices (get-box-value box))) staff)
                      (car staff))))
    
    (draw-staff x-pix y-pix y-u w h font-size staff :margin-l 0 :margin-r 0 :keys (not in-sequencer?))

    (loop for chord in (chords self) do
          (draw-chord chord
                      (date chord)
                      0 y-u  
                      x-pix 
                      y-pix 
                      w h
                      font-size :scale scale :staff staff
                      :time-function #'(lambda (time) (miniview-time-to-pixel (get-box-value box) box (frame box) time))
                      :offsets offsets
                      )
          )
    ))


;;;===========================
;;; VOICE
;;;===========================

(defmethod score-object-mini-view ((self voice) box x-pix y-pix shift-y w h)
  
  (let* ((staff (get-edit-param box :staff))
         (font-size (get-box-fontsize box))
         (unit (font-size-to-unit font-size))
         (x-u (/ x-pix unit))
         (y-u (/ y-pix unit))
         (shift-x x-u) ; (+ (score-mini-view-left-shift-in-units box) x-u))
         (frame (frame box))
         (max-w (w frame))
         
         (in-sequencer? (typep frame 'sequencer-track-view))
         )
    
    (if (> h 5)
        (progn 
          (draw-staff x-pix y-pix shift-y w h font-size staff 
                      :margin-l 0 :margin-r 0 :keys (not in-sequencer?))
    
          (draw-tempo self (+ x-pix 4) (+ y-pix (* unit (+ shift-y 2))) font-size)

          (loop with on-screen = t 
                with prev-signature = nil
                for m in (inside self)
                for i from 1
                while on-screen
                do 
                (setf on-screen (< (miniview-time-to-pixel (get-box-value box) box frame 
                                                           (beat-to-time (symbolic-date m) (tempo self)))
                                   max-w))
                ;;; we draw the measure if it begins on-screen...
                (when on-screen
                  (draw-measure m (tempo self) box (frame box) 
                                :position i
                                :with-signature (and (not in-sequencer?)
                                                     (not (equal (car (tree m)) prev-signature)))
                                :staff staff
                                :x-shift shift-x
                                :y-shift (+ shift-y y-u) 
                                :font-size font-size 
                                :time-function #'(lambda (time) (miniview-time-to-pixel (get-box-value box) box frame time))
                                ))
          
                ;;; if the end is off-screen we notify it with a little gray area at the end
                (when (and (equal self (car (value box)))  ;; don't do it on poly boxes
                           (> (time-to-pixel frame (beat-to-time (+ (symbolic-date m) (symbolic-dur m)) (tempo self))) max-w))
                  (om-draw-rect (- (w frame) 20) 0 20 (h frame) :fill t :color (om-make-color .8 .8 .8 .5))
                  (om-draw-string (- (w frame) 16) (- (h frame) 12) "..."))
                (setf prev-signature (car (tree m)))
                ))
    
      (om-draw-line x-pix y-pix (+ w x-pix) y-pix :style '(1 2)))
    ))


;;;===========================
;;; POLY
;;;===========================

(defmethod score-object-mini-view ((self multi-seq) box x-pix y-pix y-u w h)
  (let ((voice-h (if (obj-list self) (/ h (num-voices self)) h)))
    (loop for voice in (obj-list self)
          for i from 0 do
          (score-object-mini-view voice box x-pix (+ y-pix (* i voice-h)) 0 w voice-h))
    ))


(defmethod score-object-mini-view ((self poly) box x-pix y-pix y-u w h)
  (call-next-method) 
  (let ((frame (frame box)))
    (when (> (time-to-pixel frame (get-obj-dur self)) (w frame))
      (om-draw-rect (- (w frame) 20) 0 20 (h frame) :fill t :color (om-make-color .8 .8 .8 .5))
      (om-draw-string (- (w frame) 16) (- (h frame) 12) "..."))))


