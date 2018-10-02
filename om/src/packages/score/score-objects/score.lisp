;============================================================================
; o7: visual programming language for computer-aided music composition
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


;;;===================================================
;;; SCORE IS A LIST OF PARALLEL VOICES 
;;; Equivalent to former POLY / MULTI-SEQ + MIXED-TYPES
;;;===================================================

(defclass* score (score-object container) ())


;;;======================================
;;; BOX
;;;======================================

   
#|
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
            (- (y-to-pix panel (get-frame-posy frame)) 4)
            6
            4  ;; downwards
            )))


(defmethod get-frame-color ((self chord)) (om-def-color :black))
(defmethod get-frame-posy ((self chord)) (car (lmidic self)))
(defmethod get-frame-sizey ((self chord)) 500)

(defmethod draw-data-frame ((frame chord) editor i &optional (active nil))
  (let* ((panel (get-g-component editor :main-panel)))
    (multiple-value-bind (x y w h)
        (get-frame-area frame editor)
      (om-with-fg-color (if (and active (find i (selection editor))) 
                            (om-make-color-alpha (om-def-color :dark-red) 0.5)
                          (get-frame-color frame))
        (om-draw-ellipse (+ x (round w 2)) (+ y (round h 2)) w h :fill t)
        (om-draw-line (+ x w 3) (+ y 3) (+ x w 3) (- y 26))
        )
      (om-with-font 
       (om-def-font :font1 :size 8)
       (when (symbolic-date frame)
         (om-draw-string x (+ y 30) (format nil "b=~A" (symbolic-date frame)))
         (om-draw-string x (+ y 40) (format nil "d=~A" (symbolic-dur frame))))
       (when (find i (selection editor))
         (om-draw-string (- x 5) 20 (number-to-string (date frame))))
       ))))





(defmethod make-editor-window-contents ((editor score-editor))
  (om-make-layout 
   'om-column-layout 
   :ratios '(0.97 0.01 0.02 .02)
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
                                     :ratios '(0.01 0.98 0.01)))
                     )
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
              )))


|#


