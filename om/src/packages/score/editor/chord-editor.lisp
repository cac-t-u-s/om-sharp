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
; File author: J. Bresson
;============================================================================


(in-package :om)


;;;======================================================================== 
;;; CHORD EDITOR
;;;========================================================================


(defclass chord-editor (score-editor) ())
(defmethod object-has-editor ((self chord)) t)
(defmethod get-editor-class ((self chord)) 'chord-editor)


(defmethod make-editor-window-contents ((editor chord-editor))
  
  (let* ((panel (om-make-view 'score-view 
                              :size (omp 50 100) 
                              :direct-draw t 
                              :bg-color (om-def-color :white) 
                              :scrollbars nil
                              :editor editor))
         
         (bottom-area (make-score-display-params-controls editor)))

    (set-g-component editor :main-panel panel)
    (om-make-layout 'om-row-layout :ratios '(99 1) 
                    :subviews 
                    (list 
                     (om-make-layout 'om-column-layout 
                                     :ratios '(99 1)
                                     :subviews (list panel bottom-area))
                     (call-next-method)))
    ))



(defmethod update-to-editor ((editor chord-editor) (from t))
  (call-next-method)
  (editor-invalidate-views editor))

(defmethod editor-invalidate-views ((self chord-editor))
  (call-next-method)
  (om-invalidate-view (get-g-component self :main-panel)))


;;; called at add-click
(defmethod get-chord-from-editor-click ((self chord-editor) position) 
  (declare (ignore position))
  (object-value self))


(defmethod score-editor-delete ((self chord-editor) element) 
  (let ((c (object-value self)))
    (if (equal element c)
        (setf (notes c) nil)
      (setf (notes c) (remove element (notes c))))))

 
(defmethod move-editor-selection ((self chord-editor) &key (dx 0) (dy 0))
  
  (declare (ignore dx))

  (let* ((chord (object-value self))
         (notes (if (find chord (selection self))
                    (notes chord)
                  (selection self))))

    (unless (zerop dy)
      (loop for n in notes do
            (setf (midic n) (+ (midic n) (* dy 100)))))
    ))


(defmethod score-editor-change-selection-durs ((self chord-editor) delta) 
  (let* ((chord (object-value self))
         (notes (if (find chord (selection self))
                    (notes chord)
                  (selection self))))
    
    (loop for n in notes
          do (setf (dur n) (max (abs delta) (round (+ (dur n) delta)))))
    ))


;;; SPECIAL/SIMPLE CASE FOR CHORD-EDITOR
(defmethod draw-score-object-in-editor-view ((editor chord-editor) view unit)

  (let ((chord (object-value editor)))
    
    (setf 
     (b-box chord)
     (draw-chord chord
                 0 
                 0 (editor-get-edit-param editor :y-shift)
                 0 0 
                 (w view) (h view) 
                 (editor-get-edit-param editor :font-size) 
                 :staff (editor-get-edit-param editor :staff)
                 :draw-chans (editor-get-edit-param editor :channel-display)
                 :draw-vels (editor-get-edit-param editor :velocity-display)
                 :draw-ports (editor-get-edit-param editor :port-display)
                 :draw-durs (editor-get-edit-param editor :duration-display)
                 :draw-dur-ruler t
                 :selection (if (find chord (selection editor)) T 
                              (selection editor))

                 :time-function #'(lambda (time) 
                                    (+ (/ (w view) 2) 
                                       (if (notes chord)
                                           (* (/ (- (w view) 80) 2) 
                                              (/ time (list-max (mapcar #'dur (notes chord)))))
                                         0))
                                    )
                 :build-b-boxes t
                 ))
    
    ; (draw-b-box chord)
    ))
