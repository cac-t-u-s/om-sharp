;============================================================================
; o7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed; in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================


(in-package :om)


;;;===================================================
;;; SCORE IS JUST A SPECIAL KIND OF DATA-STREAM
;;;===================================================

(defclass* score (data-stream)
  ((contents :accessor contents :initarg :contents :initform '() :documentation "any kinds of score-object")
   (tempo-curve :accessor tempo-curve :initform nil :documentation "any kinds of score-object"))
  (:default-initargs :default-frame-type 'chord))

(defmethod data-stream-frames-slot ((self score)) 'contents)

(defmethod om-init-instance ((self score) &optional initargs)
  (call-next-method)
  (setf (tempo-curve self) (make-tempo-curve self))
  (setf (contents self) (sort (slot-value self 'contents) '< :key 'date))
  self)

(defmethod initialize-instance :after ((self score) &rest initargs)
  (setf (autostop self) t) ;;; ??? wtf 
  )

;;; redefined from time-sequence
(defmethod time-sequence-default-duration ((self score)) 1000)
       
(defmethod additional-class-attributes ((self score)) '(tempo-curve))

;;;======================================
;;; BOX
;;;======================================
(defmethod display-modes-for-object ((self score))
  '(:hidden :text :mini-view))


(defmethod draw-mini-view ((self score) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (multiple-value-bind (fx ox) 
        (conversion-factor-and-offset 0 (get-obj-dur self) (- w 60) 30)
      (multiple-value-bind (fy oy) 
          (conversion-factor-and-offset 10000 4000 (- h 20) (+ y 10))
        (loop for line in '(6400 6800 7200 7600 8000) do 
              (om-draw-line 10 (+ oy (* fy line)) (- w 10) (+ oy (* fy line))))
        
        (loop for n in (contents self) do
              (om-draw-ellipse (+ ox (* fx (date n))) (+ oy (* fy (car (lmidic n))))
                              4 3 :fill t)
              (om-draw-line (+ ox (* fx (date n)) 4) (+ oy (* fy (car (lmidic n))))
                            (+ ox (* fx (date n)) 4) (+ oy (* fy (car (lmidic n))) -20))
                            ))
      )
    t))
   

;;;======================================
;;; EDITOR
;;;======================================

(defclass score-editor (multi-view-editor data-stream-editor) 
  ((tempo-editor :accessor tempo-editor :initform nil)
   (beats :accessor beats :initform nil)))

(defmethod get-editor-class ((self score)) 'score-editor)

(defclass score-panel (stream-panel multi-view-editor-view) ())
(defmethod editor-view-class ((self score-editor)) 'score-panel)

(defmethod frame-display-modes-for-object ((self score-editor) (object score)) nil)
(defmethod y-range-for-object ((self score)) '(15000 2000))

(defmethod om-draw-contents ((self score-panel))
  (let* ((editor (editor self))
         (score (if (multi-display-p editor)
                     (nth (stream-id self) (multi-obj-list editor))
                   (object-value editor))))
     
    (om-with-fg-color (om-def-color :dark-blue)
      (om-with-line-size 2
      (loop for beat in (beats editor) do
            
           (om-draw-line (x-to-pix self (car (list! beat))) 0 (x-to-pix self (car (list! beat))) (h self))
           (om-with-line '(2 2)
             (loop for sub in (cdr beat) do 
                   (om-draw-line (x-to-pix self sub) 0 (x-to-pix self sub) (h self))))
           ))
      )
    (loop for line in '(6400 6800 7200 7600 8000) do 
          (om-draw-line 0 (y-to-pix self line) (w self) (y-to-pix self line)))
    (call-next-method)))


(defmethod get-frame-area ((frame chord) editor)
  (let ((panel (get-g-component editor :main-panel)))
    (values (x-to-pix panel (date frame))
            (- (y-to-pix panel (get-frame-attribute frame :posy editor)) 4)
            6
            4  ;; downwards
            )))

(defmethod set-frame-attributes-from-editor ((f chord) editor) 
 (declare (ignore editor))
 (setf (getf (attributes f) :color) (om-def-color :black)
       (getf (attributes f) :posy) (car (lmidic f))
       (getf (attributes f) :sizey) 500))

(defmethod draw-data-frame ((frame chord) editor i &optional (active nil))
  (let* ((panel (get-g-component editor :main-panel)))
    (multiple-value-bind (x y w h)
        (get-frame-area frame editor)
      (om-with-fg-color (if (and active (find i (selection editor))) 
                            (om-make-color-alpha (om-def-color :dark-red) 0.5)
                          (getf (attributes frame) :color (om-def-color :light-gray)))
        (om-draw-ellipse (+ x (round w 2)) y w h :fill t))
      (om-with-font 
       (om-def-font :font1 :size 8)
       (om-draw-string x (+ y 30) (format nil "b=~A" (symbolic-date frame)))
       (om-draw-string x (+ y 40) (format nil "d=~A" (symbolic-dur frame)))
       (when (find i (selection editor))
         (om-draw-string (- x 5) 20 (number-to-string (date frame))))
       ))))


;;;============
;;; TEMPO
;;;============


(defun make-tempo-curve (score)
  (let ((tc (or (tempo-curve score)
                (make-instance 'automation :x-points '(0) :y-points '(60) :decimals 0))))
    (unless (find 0 (x-points tc))
      (insert-point tc (make-automation-point :x 0 :y 60) 0))
    (setf (ap-lock (car (point-list tc))) t)
    tc))

(defclass tempo-editor (automation-editor) ())
(defclass tempor-editor-view (automation-panel multi-view-editor-view) ())
(defmethod get-curve-panel-class ((self tempo-editor)) 'tempor-editor-view)

(defmethod update-to-editor ((editor score-editor) (from tempo-editor))
  (report-modifications editor))

(defmethod update-to-editor ((editor score-editor) (from ombox))
  (when (tempo-editor editor)
    (setf (contents (object (tempo-editor editor)))
          (tempo-curve (object-value editor)))
    (update-to-editor (tempo-editor editor) editor))
  (update-tempo editor)
  (call-next-method))

(defmethod report-modifications  ((self tempo-editor)) 
  (when (container-editor self) (update-tempo (container-editor self)))
  (call-next-method))

;;; not always / everywhere necessary...
(defmethod report-modifications  ((self score-editor)) 
  (update-beat-values self)
  (call-next-method))
  
(defun make-tempo-editor (editor)
  (let ((tempo-editor (make-instance 'tempo-editor 
                                      :object (make-instance 'omabstractcontainer :contents (tempo-curve (object-value editor)))
                                      :container-editor editor
                                      :decimals 0)))
    (set-g-component tempo-editor :main-layout (om-make-layout 'om-row-layout))
    (setf (tempo-editor editor) tempo-editor)
    tempo-editor))

(defun show-tempo-editor (editor)
  (let* ((tempo-editor (tempo-editor editor))
         (tempo-view (om-make-view (get-curve-panel-class tempo-editor) ; :size (omp 50 100) 
                                   :direct-draw t :bg-color (om-def-color :white) :scrollbars nil
                                   :scale-fact 1
                                   :editor tempo-editor))
         (x-ruler (get-g-component editor :x-ruler)))
    (set-g-component tempo-editor :main-panel tempo-view)
    (setf (x-ruler tempo-view) x-ruler)
    (setf (y-ruler tempo-view) (om-make-view 'y-ruler-view :related-views (list tempo-view) 
                                             :size (omp 28 nil)
                                             :decimals 0 
                                             :vmin 0 :vmax 300))
    (setf (related-views x-ruler) (cons tempo-view (related-views x-ruler)))
    (setf (x1 tempo-view) (v1 x-ruler) (x2 tempo-view) (v2 x-ruler))
    (set-ruler-range (y-ruler tempo-view) 40 180)
    (set-shift-and-factor tempo-view)
    (apply 'om-remove-subviews (cons tempo-view (get-g-component tempo-editor :mode-buttons)))
    (om-add-subviews (get-g-component tempo-editor :main-layout) 
                     (y-ruler tempo-view) ;(om-make-view 'om-view :size (omp 28 nil) )
                     tempo-view)
    (om-set-layout-ratios (get-g-component tempo-editor :main-layout) '(nil 100)) 
    tempo-editor))
   

(defun hide-tempo-editor (editor)
  (let* ((tempo-editor (tempo-editor editor))
         (tempo-view (get-g-component tempo-editor :main-panel)))
    (setf (related-views (get-g-component editor :x-ruler))
          (remove tempo-view (related-views (get-g-component editor :x-ruler))))
    (om-remove-all-subviews (get-g-component tempo-editor :main-layout))))

(defun make-tempo-check-box (editor)
  (om-make-di 'om-check-box :text "tempo" :size (omp 65 24) :font (om-def-font :font1)
              :checked-p nil
              :enable (tempo-editor editor)
              :di-action #'(lambda (item) 
                             (if (om-checked-p item) 
                                 (show-tempo-editor editor)
                               (hide-tempo-editor editor))
                             (om-set-layout-ratios 
                              (main-view editor)
                              (if (om-checked-p item)
                                  '(0.77 0.2 0.02) '(0.97 0.01 0.02)
                                ))
                             ;(om-update-layout (main-view editor))
                             )))

(defmethod find-clicked-point-or-curve ((editor tempo-editor) (object automation) position)
  (let ((selection (call-next-method)))
    (if (equal selection T) (setf selection NIL))
    selection))

(defmethod position-display ((editor tempo-editor) pos-pix)
  (let* ((decimals (decimals editor))
         (point (omp (pix-to-x (get-g-component editor :main-panel) (om-point-x pos-pix))
                     (pix-to-y (get-g-component editor :main-panel) (om-point-y pos-pix)))))
    (om-show-tooltip (get-g-component editor :main-panel)
                     (format nil "~D" (round (om-point-y point)))
                     nil 0)
    ))

(defmethod move-editor-selection ((self tempo-editor) &key (dx 0) (dy 0))
  (call-next-method)
  (update-tempo (container-editor self)))

;;;============
;;; MAIN
;;;============

(defmethod init-editor ((editor score-editor))
  (call-next-method)
  (make-tempo-editor editor))
       
(defmethod make-editor-window-contents ((editor score-editor))
  (om-make-layout 
   'om-column-layout 
   :ratios '(0.97 0.01 0.02)
   :subviews (list 
              ;;; first group with the 'main' editor:
               (om-make-layout 
                'om-row-layout :ratios '(nil 100) :subviews 
                (list (om-make-view 'om-view :size (omp 28 nil))
                      (om-make-layout 'om-column-layout :align :right
                              :subviews (list 
                                         (make-control-bar editor)
                                         (get-g-component editor :main-panel) 
                                         (get-g-component editor :x-ruler))
                                :delta 2
                                :ratios '(0.01 0.98 0.01))))
              ;;; the tempo editor
              (get-g-component (tempo-editor editor) :main-layout)
              ;;; the timeline editor:
              (get-g-component (timeline-editor editor) :main-panel)
              ;;; the bottom control bar:
              (om-make-layout 'om-row-layout 
                              :size (omp nil 40) 
                              :subviews (list (make-display-modes-menu editor) nil 
                                              (make-tempo-check-box editor)
                                              (make-timeline-check-box editor)))
              ))
  )


(defmethod update-beat-values ((self score-editor))
  ;(print "update-beats")
  (let ((score (object-value self)))
    (loop for chords on (contents score)
          do (let* ((c (car chords))
                   (c-next (cadr chords))
                   (b (get-beat-at-time (date c) (tempo-curve score)))
                   (b-next (and c-next (get-beat-at-time (date c-next) (tempo-curve score)))))
               (setf (symbolic-date c) b)
               (setf (symbolic-dur c) (if (and c-next b-next) (- b-next b) 1)) 
               ))))

(defmethod update-tempo ((self score-editor))
  ;(print "update-tempo")
  (let* ((score (object-value self))  
         (dur (get-obj-dur score))
         (curr-t 0) (subdiv 4))
    (setf (beats self)
          (remove nil
                  (loop for b = 0 then (+ b 1)
                        while (and curr-t (< curr-t dur))
                        collect (remove nil 
                                        (loop for s = 0 then (+ s (/ 1 subdiv))
                                              while (and curr-t (< curr-t dur) (< s 1))
                                              do (setf curr-t (get-time-of-beat (float (+ b s)) (tempo-curve score)))
                                              collect curr-t)))))
    (update-beat-values self)
    (om-invalidate-view (get-g-component self :main-panel))
    ))
               
                    
;;;================
;;; LIB-T CONNECTION


(defun format-tempo-curve-as-osc-messages (tempo-curve)
  (list 
   (osc-msg "/xs" (ms->sec (x-points tempo-curve)))
   (osc-msg "/ys" (om/ (y-points tempo-curve) 60.0))
   (osc-msg "/tempo_phase" "lambda(t, map( lambda(i, if(t < /xs[[i+1]] & t >= /xs[[i]], progn( /x1 = /xs[[i]], /x2 = /xs[[i + 1]], /y1 = /ys[[i]], /y2 = /ys[[i + 1]], /m = (/y2 - /y1) / (/x2 - /x1), /b = -1 * (/m * /x1 - /y1), /_tempo = /m * t + /b, /_phase = (((/m * pow(t, 2.0)) / 2.0) + (/b * t)), /segment = i) ) ), aseq(0, length(/xs) - 2)))")
   (osc-msg "/expr" "/c1 = [], /c2 = [], map(lambda(i, /tempo_phase(/xs[[i]]), /c1 = [/c1, /_phase]), aseq(0, length(/xs) - 1)),map( lambda(i, /tempo_phase(/xs[[i]] - .0000000000001), /tmp = /_phase, /tempo_phase(/xs[[i - 1]]), /c2 = [/c2, /tmp - /_phase], delete(/tmp)), aseq(1, length(/xs) - 1)), /c2 = cumsum([0, /c2]), if(bound(/time), progn( /tempo_phase(/time), /tempo = /_tempo, /phase = /_phase - /c1[[/segment]] + /c2[[/segment]]), if(bound(/phase), progn( map( lambda(t, if(!bound(/time), progn(/tempo_phase(t), if((/_phase - /c1[[/segment]] + /c2[[/segment]]) >= /phase, /time = t)))), aseq(/xs[[0]], /xs[[length(/xs) - 1]], 0.01)))))")))
   
   
(defun get-beat-at-time (date tempo-curve) 
  (let* ((b1 (make-o.bundle 
             (make-instance 
              'osc-bundle 
              :messages (cons 
                         (osc-msg "/time" (ms->sec date))
                         (format-tempo-curve-as-osc-messages tempo-curve)))))
         (b2 (libt-call b1))
         (phase (cadr (find "/phase" (get-messages b2) :key 'car :test 'string-equal))))
    (and phase (coerce phase 'single-float))))

  
(defun get-time-of-beat (b tempo-curve)
  (let* ((b1 (make-o.bundle 
             (make-instance 
              'osc-bundle 
              :messages (cons 
                         (osc-msg "/phase" b)
                         (format-tempo-curve-as-osc-messages tempo-curve)))))
         (b2 (libt-call b1))
         (time (cadr (find "/time" (get-messages b2) :key 'car :test 'string-equal))))
    (and time (sec->ms time))))





#|
;;;======================================
;;; PLAY
;;;======================================
(defmethod get-action-list-for-play ((object score) interval &optional parent)
  (sort 
   (mapcan #'(lambda (n)
               (remove nil (list 
                            (if (in-interval (midinote-onset n) interval :exclude-high-bound t) 
                                (list (midinote-onset n)
                                      #'(lambda (note) (om-midi::midi-send-evt 
                                                        (om-midi:make-midi-evt 
                                                         :type :keyOn
                                                         :chan (or (midinote-channel note) 1) :port 0
                                                         :fields (list (midinote-pitch note) (midinote-vel note)))))
                                      (list n)))

                            (if (in-interval (midinote-end n) interval :exclude-high-bound t)
                                (list (midinote-end n)
                                      #'(lambda (note) (om-midi::midi-send-evt 
                                                        (om-midi:make-midi-evt 
                                                         :type :keyOff
                                                         :chan (or (midinote-channel note) 1) :port 0
                                                         :fields (list (midinote-pitch note) 0))))
                                      (list n)))
                      
                            )))
           
           (remove-if #'(lambda (note) (or (< (midinote-end note) (car interval))
                                           (> (midinote-onset note) (cadr interval))))
                      (sort (midi-notes object) '< :key 'midinote-onset)))
   '< :key 'car))


;;; not good
(defmethod player-stop-object ((self scheduler) (object score))
  (call-next-method)
  (om-midi::midi-stop))
|#

