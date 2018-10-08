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

;=========================================================================
;COMMENTS 
;=========================================================================

(in-package :om)

(defclass OMComment (OMBox) ())

(add-preference-section :appearance "Comments" "Default values for comments with unspecified or disabled attributes")
(add-preference :appearance :comment-fgcolor "Text color" :color (om-def-color :black))
(add-preference :appearance :comment-bgcolor "Background color" :color-a (om-def-color :transparent))
(add-preference :appearance :comment-border "Border" (make-number-in-range :min 0 :max 4 :decimals 1) 1)
(add-preference :appearance :comment-roundness "Corner roundness" (make-number-in-range :min 0 :max 20) 0)
(add-preference :appearance :comment-font "Font" :font (om-def-font :font1 :style '(:italic)))
(add-preference :appearance :comment-align "Text align" '(:left :center :right) :left)


 ;;; id text type slot-name defaly
(defmethod get-properties-list ((self OMComment))
  `(("Appearance" ;;; category
     (:fgcolor "Text color" :color-or-nil text-color (:appearance :comment-fgcolor))
     (:bgcolor "Background color" :color-or-nil color (:appearance :comment-bgcolor))
     (:border "Border" ,(make-number-or-nil :min 0 :max 4 :decimals 1) border (:appearance :comment-border))
     (:roundness "Corner" ,(make-number-or-nil :min 0 :max 20) roundness (:appearance :comment-roundness))
     (:text-font "Text font" :font-or-nil text-font (:appearance :comment-font))
     (:align "Text align" (:left :center :right :default) text-align (:appearance :comment-align))
     )))

(defmethod box-draw-color ((box OMComment)) 
  (if (and (color box) (color-? (color box)))
      (color-color (color box))
    (get-pref-value :appearance :comment-bgcolor)))

(defmethod box-draw-text-color ((box OMComment)) 
  (if (and (text-color box) (color-? (text-color box)))
      (color-color (text-color box))
    (get-pref-value :appearance :comment-fgcolor)))

(defmethod box-draw-text-align ((box OMComment)) 
  (or (text-align box))
      (get-pref-value :appearance :comment-align))

(defmethod box-draw-font ((box OMComment)) 
  (if (font-? (text-font box))
      (font-font (text-font box))
    (get-pref-value :appearance :comment-font)))

(defmethod box-draw-border ((box OMComment)) 
  (if (number-? (border box))
      (number-number (border box))
    (get-pref-value :appearance :comment-border)))

(defmethod box-draw-roundness ((box OMComment)) 
  (if (number-? (roundness box))
      (number-number (roundness box))
    (get-pref-value :appearance :comment-roundness)))



(defmethod omNG-make-new-comment (text pos)
  (let* ((comment-lines (om-text-to-lines text))
         (longest-line (reduce #'(lambda (s1 s2) (if (> (length s1) (length s2)) s1 s2)) comment-lines)))
    (multiple-value-bind (w h)
        (om-string-size longest-line (get-pref-value :appearance :comment-font))
    
      (let ((newcomment (make-instance 'OMComment 
                                       :icon-pos nil 
                                       :box-x (om-point-x pos) :box-y (om-point-y pos)
                                       :box-w (+ 4 w) :box-h (+ 12 (* h (length comment-lines)))
                                     )))
      (setf (value newcomment) text)
      newcomment))))

(defmethod default-size ((self OMComment))
  (let* ((comment-lines (om-text-to-lines (value self)))
         (longest-line (reduce #'(lambda (s1 s2) (if (> (length s1) (length s2)) s1 s2)) comment-lines)))
    (multiple-value-bind (w h)
        (om-string-size longest-line (box-draw-font self))
      (omp (+ 12 w) (+ 8 (* h (length comment-lines)))))))

(defmethod maximum-size ((self OMComment)) nil)

(defmethod om-copy ((self OMComment)) 
  (let ((newbox (call-next-method)))
    (setf (value newbox) (value self))
    newbox))

(defmethod get-box-value ((self OMComment)) nil)

(defmethod eval-box ((self OMComment)) nil)

;(defun str-without-nl (str)
;  (map 'string  #'(lambda (x) (if (equal x #\Newline) #\$ x)) str))
;(defun str-with-nl (str)
;  (map 'string  #'(lambda (x) (if (equal x #\$) #\Newline  x)) str))
;(defmethod save-reference ((self OMBoxcomment))
;  (str-without-nl (str-check (reference self))))


(defclass CommentFrame (OMBoxFrame) ())
(defmethod get-box-frame-class ((self OMComment)) 'CommentFrame)

(defmethod make-frame-from-callobj ((self OMComment))
  (let ((view (om-make-graphic-object 'CommentFrame 
                :position (omp (box-x self) (box-y self))
                :size (omp (box-w self) (box-h self))
                :help "comment"
                :font (font-font (text-font self))
                :object self)))
    (setf (frame self) view)
    (set-frame-areas view)
    view))


(defmethod clickable-box ((self CommentFrame)) 
  (let ((container-view (om-view-container self)))
    (and (editor container-view)
         (not (edit-lock (editor container-view))))))

(defmethod set-frame-areas ((self CommentFrame))
  (setf (areas self) 
        (append (resize-areas self)
                (info-area self))))


;; not used
(defmethod display-text-and-area ((self CommentFrame))
  (let ((font (or (font-font (text-font (object self))) (om-get-font self)))
        (lines (om-text-to-lines (value (object self)))))
    
    (multiple-value-bind (w h) (om-string-size (car lines) font)
      (loop for l in (cdr lines) do (setf w (max w (om-string-size l font))))
      
      (values (value (object self)) 3 8 w (* h (length lines)))
      ))
  )

(defmethod boxframe-draw-contents ((self CommentFrame) (box OMComment))
  (let ((color (box-draw-color box))
        (font (box-draw-font box))
        (round (box-draw-roundness box))
        (text (value box)))
    
    (om-with-fg-color (om-def-color :dark-gray)
   
      ;;; interior
      (unless (om-color-null-p color)
        (if (plusp round)
            (om-draw-rounded-rect 0 0 (w self) (h self) 
                                  :fill t :color color 
                                  :round (min (round (h self) 2) round))
          (om-draw-rect 0 0 (w self) (h self) 
                        :fill t :color color :angles :round)
          ))
    
      (when (selected box)
        (if (plusp round)
              (om-draw-rounded-rect 0 0 (w self) (h self) :fill t 
                                    :color (om-make-color-alpha (om-def-color :gray) 0.3) 
                                    :round (min (round (h self) 2) round))
            (om-draw-rect 0 0 (w self) (h self) 
                          :fill t 
                          :color (om-make-color-alpha (om-def-color :gray) 0.3) 
                          :angles :round)
            ))

      ;;; text
      (when text
        (om-with-clip-rect self 0 0 (w self) (h self) 
          (multiple-value-bind (w h)
              (om-string-size "A" font)
            (om-with-fg-color (box-draw-text-color box)
              (om-with-font
               font
               (om-draw-string 4 ; (max 2 x)
                               h ;(+ y (om-font-size (or font (om-get-font self)))) 
                               text :selected nil 
                               :wrap (max 10 (- (w self) 8))
                               :align (box-draw-text-align box))
             )))))
      
     ;;; border
     (when (and (box-draw-border box) (plusp (box-draw-border box)))
         (om-with-line '(2 2)
           (draw-border box 0 0 (w self) (h self))))
     
     ))
  
  ;;; resize etc. (do nothing)
  (mapcar #'(lambda (a) (om-draw-area a)) (areas self))
   
  )


(defmethod enter-new-comment ((self om-view) position)
  (let ((textinput 
         (om-make-di 'om-text-edit-view
                     :text "type your comment here"
                     :fg-color (om-def-color :gray)
                     :di-action #'(lambda (item) 
                                    (let ((text (om-dialog-item-text item)))
                                      (om-end-text-edit item)
                                      (om-remove-subviews self item)
                                      (unless (or (string-equal text "type your comment here")
                                                  (<= (length text) 0))
                                        (new-comment-in-patch-editor self text position))
                                      (om-set-focus self)
                                      ))
                     :begin-edit-action #'(lambda (item) (om-set-fg-color item (om-def-color :dark-gray)))
                     ;:edit-action #'(lambda (item) (print "edit"))
                     :font (om-def-font :font1)
                     :size (om-make-point 140 60)
                     :position position
                     )))
    (om-add-subviews self textinput)
    (om-set-text-focus textinput t)
    t))

(defmethod new-comment-in-patch-editor ((self patch-editor-view) text pos)
  (let ((comment (omNG-make-new-comment text pos)))
    (add-box-in-patch-editor comment self)))

(defmethod om-view-doubleclick-handler ((self CommentFrame) position)
   (unless (edit-lock (editor (om-view-container self)))
     (let* ((box (object self))
            (container-view (om-view-container self))
            (textinput (om-make-di 'om-text-edit-view
                                   :text (value box)
                                   :focus t
                                   :di-action #'(lambda (item) 
                                                  (let ((newtext (om-dialog-item-text item)))
                                                (om-end-text-edit item)
                                                (om-remove-subviews container-view item)
                                                (set-value box newtext)
                                                (om-set-focus container-view)
                                                ))
                                   :font (font-font (text-font box))
                                   :size (om-add-points (omp (box-w box) (box-h box)) (omp 4 4))
                                   :position (omp (box-x box) (box-y box))
                                   )))
       (om-add-subviews container-view textinput)
       (om-set-text-focus textinput t))))


    
