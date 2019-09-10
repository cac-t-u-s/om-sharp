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
;;; MULTI-SEQ EDITOR
;;;========================================================================

;;; The editor will inherit from dta-stream-editor, althouhg MULTI-SEQ in NOT a data-stream...
(defclass multi-seq-editor (chord-seq-editor) ())
(defmethod get-editor-class ((self multi-seq)) 'multi-seq-editor)

(defmethod object-default-edition-params ((self multi-seq))
  (append (call-next-method)
          '((:grid nil))))

;;; From DATA-STREAM-EDITOR...
(defmethod y-range-for-object ((self multi-seq)) '(-100 100))   ;;; not used anyway


;;;=========================
;;; Multi-STAFF
;;;=========================

(defvar *default-inter-staff* 8)

(defmethod accum-y-shift-list ((editor multi-seq-editor))
  (let* ((y-shift (editor-get-edit-param editor :y-shift))
         (y-shift-list (list! y-shift))
         (staff (editor-get-edit-param editor :staff))
         (accum-y-list ()))
    
    (loop with tmp-y = 0 
          for i from 0 to (1- (length (obj-list (object-value editor)))) 
          do 
          (setf tmp-y (+ tmp-y (or (nth i y-shift-list) *default-inter-staff*)))
          (push tmp-y accum-y-list) 
          (setf tmp-y (+ tmp-y (- (staff-higher-line staff) (staff-lower-line staff))))
          )
    (reverse accum-y-list)
    ))


;; returns the y1-y2 pairs for all staffs
(defun make-staff-y-map (editor)
  
  (let* ((unit (font-size-to-unit (editor-get-edit-param editor :font-size)))
         (staff (editor-get-edit-param editor :staff)))
  
  (loop for ys in (accum-y-shift-list editor)
        collect (staff-y-range staff ys unit))
  ))

;;;=========================
;;; LEFT VIEW (KEYS etc.)
;;;=========================

(defclass poly-left-score-view (left-score-view) ())
(defmethod left-score-view-class ((self multi-seq-editor)) 'poly-left-score-view)

;;; !!!!
(defmethod om-view-click-handler ((self poly-left-score-view) position)
  
  (let* ((editor (editor self))
         (staff-y-map (make-staff-y-map editor))
         (n-voices (length staff-y-map))
         (selected-staff (position-if #'(lambda (range)
                                          (and (>= (om-point-y position) (car range))
                                               (<= (om-point-y position) (cadr range))))
                                      staff-y-map)))
    
    (if selected-staff
        
        (let* ((unit (font-size-to-unit (editor-get-edit-param editor :font-size))))
               
          (set-selection editor (nth selected-staff (obj-list (object-value editor))))
          
          (om-init-temp-graphics-motion 
           self position nil :min-move 1
           :motion #'(lambda (view pos)
                       (declare (ignore view))
                       
                       (when (and (> (om-point-y pos) 10)
                                  (< (om-point-y pos) (- (h self) 10)))
                                  
                         (let ((y-diff-in-units (/ (- (om-point-y pos) (om-point-y position)) unit))
                               (previous-y-list (list! (editor-get-edit-param editor :y-shift)))
                               (new-list (make-list n-voices)))
                           
                           (loop for i from 0 to (1- n-voices)
                                 do (setf (nth i new-list) (nth i previous-y-list)))
                           
                           (setf (nth selected-staff new-list) 
                                 (max 0 (+ (nth selected-staff new-list) y-diff-in-units)))
                           
                           (editor-set-edit-param editor :y-shift new-list)
                           (om-invalidate-view self)
                           (om-invalidate-view (main-view editor)))
                         )))
          )
      
      (set-selection editor nil))
    
    (om-invalidate-view self)
    (om-invalidate-view (main-view editor))
    ))


;;;=========================
;;; DISPLAY
;;;=========================

(defmethod draw-staff-in-editor-view ((editor multi-seq-editor) (self score-view))
  (let* ((fontsize (editor-get-edit-param editor :font-size))
         (y-shift (editor-get-edit-param editor :y-shift))
         (y-shift-list (list! y-shift))
         (staff (editor-get-edit-param editor :staff)))
    
    (loop with tmp-y = 0 
          for i from 0 to (1- (length (obj-list (object-value editor)))) 
          do 
          (setf tmp-y (+ tmp-y (or (and (> (length y-shift-list) i)
                                        (nth i y-shift-list))
                                   *default-inter-staff*)))
          (draw-staff 0 0 
                      tmp-y
                      (w self) (h self) 
                      fontsize 
                      staff
                      :margin-l (margin-l self) 
                      :margin-r (margin-r self)
                      :keys (keys self))
          (setf tmp-y (+ tmp-y (- (staff-higher-line staff) (staff-lower-line staff))))
          )))


(defmethod draw-sequence ((object multi-seq) editor view unit &optional (voice-num 0))
  (let* ((y-shift (editor-get-edit-param editor :y-shift))
         (y-shift-list (list! y-shift))
         (staff (editor-get-edit-param editor :staff)))
    (loop with tmp-y = 0  
          for voice in (obj-list object) 
          for i from 0 do 
          (setf tmp-y (+ tmp-y (or (and (> (length y-shift-list) i)
                                        (nth i y-shift-list))
                                   *default-inter-staff*)))
          (draw-sequence voice editor view unit tmp-y)
          (setf tmp-y (+ tmp-y (- (staff-higher-line staff) (staff-lower-line staff))))
    )))

;;;=========================
;;; ACTIONS
;;;=========================

;;; remove chord
(defmethod remove-from-obj ((self multi-seq) (item chord))
  (let ((cseq (find-if #'(lambda (cseq)
                           (find item (data-stream-get-frames cseq)))
                       (obj-list self) 
                       )))
    (when cseq
      (time-sequence-remove-timed-item cseq item))
    ))

;;; remove note
(defmethod remove-from-obj ((self multi-seq) (item note))
  (loop for cseq in (obj-list self)
        while (not (remove-from-obj cseq item))))


;;; add chord/notes
(defmethod get-chord-seq-at-pos ((self multi-seq-editor) position)
  (object-value self))



