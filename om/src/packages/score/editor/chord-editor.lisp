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
  
  (let* ((object (object-value editor))
         (panel (om-make-view 'score-view 
                              :size (omp 50 100) 
                              :direct-draw t 
                              :bg-color (om-def-color :white) 
                              :scrollbars nil
                              :editor editor))
         
         (bottom-area (make-score-control-panel editor)))

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

;;; SPECIAL/SIMPLE CASE FOR CHORD-EDITOR
(defmethod draw-score-object-in-editor-view ((editor chord-editor) view unit)

  (let ((chord (object-value editor))
        (middle-in-units (/ (w view) 2 unit)))
    
    (setf 
     (b-box chord)
     (draw-chord (notes chord) middle-in-units 0 
                 (w view) (h view) 
                 (editor-get-edit-param editor :font-size) 
                 :staff (editor-get-edit-param editor :staff)
                 :draw-chans (editor-get-edit-param editor :channel-display)
                 :draw-vels (editor-get-edit-param editor :velocity-display)
                 :draw-ports (editor-get-edit-param editor :port-display)
                 :draw-durs (editor-get-edit-param editor :duration-display)
                 :selection (if (find chord (selection editor)) T 
                              (selection editor))
                 :build-b-boxes t
                 ))
    
    ; (draw-b-box chord)
    ))
