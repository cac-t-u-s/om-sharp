;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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
(add-preference :appearance :comment-border "Border" (make-number-in-range :min 0 :max 4 :decimals 1) 0)
(add-preference :appearance :comment-roundness "Corner roundness" (make-number-in-range :min 0 :max 20) 0)

;;; for comments we specify directly a font size: the platform-specifics apply at drawing
(add-preference :appearance :comment-font "Font" :font (om-def-font :font1 :size 12))

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

(defmethod consolidate-appearance ((self OMComment))
  (set-property self :fgcolor (make-color-or-nil :t-or-nil t :color (box-draw-text-color self)))
  (set-property self :bgcolor (make-color-or-nil :t-or-nil t :color (box-draw-color self)))
  (set-property self :border (make-number-or-nil :t-or-nil t :number (box-draw-border self)))
  (set-property self :roundness (make-number-or-nil :t-or-nil t :number (box-draw-roundness self)))
  (set-property self :text-font (make-font-or-nil :t-or-nil t :font (box-draw-font self)))
  (set-property self :align (box-draw-text-align self)))


(defmethod object-name-in-inspector ((self OMComment)) "COMMENT")
(defmethod get-documentation ((self OMComment))
  "Comments are simple textual elements documenting patches.")

(defmethod box-draw-color ((box OMComment))
  (if (and (color box) (color-? (color box)))
      (color-color (color box))
    (get-pref-value :appearance :comment-bgcolor)))

(defmethod box-draw-text-color ((box OMComment))
  (if (and (text-color box) (color-? (text-color box)))
      (color-color (text-color box))
    (get-pref-value :appearance :comment-fgcolor)))

(defmethod box-draw-text-align ((box OMComment))
  (or (text-align box)
      (get-pref-value :appearance :comment-align)))

(defmethod box-draw-font ((box OMComment))
  (let ((font (if (font-? (text-font box))
                  (font-font (text-font box))
                (get-pref-value :appearance :comment-font))))
    #+macosx font
    #-macosx (om-make-font (om-font-face font) (* (om-font-size font) .75) :style (om-font-style font))
    ))

(defmethod box-draw-border ((box OMComment))
  (if (number-? (border box))
      (number-number (border box))
    (get-pref-value :appearance :comment-border)))

(defmethod box-draw-roundness ((box OMComment))
  (if (number-? (roundness box))
      (number-number (roundness box))
    (get-pref-value :appearance :comment-roundness)))


(defmethod special-box-p ((name (eql 'comment))) t)

(defmethod omNG-make-special-box ((reference (eql 'comment)) pos &optional init-args)
  (let ((text (if init-args (format nil "~{~A ~}" (list! init-args))
                "enter your comment here...")))
    (omNG-make-new-comment text pos)))

(defparameter *comment-box-margin* 4)

(defmethod omNG-make-new-comment (text pos)
  (let* ((comment-lines (om-text-to-lines text))
         (longest-line (reduce #'(lambda (s1 s2) (if (> (length s1) (length s2)) s1 s2)) comment-lines)))
    (multiple-value-bind (w h)
        (om-string-size longest-line (get-pref-value :appearance :comment-font))

      (let ((newcomment (make-instance 'OMComment
                                       :icon-pos nil
                                       :box-x (om-point-x pos) :box-y (om-point-y pos)
                                       :box-w (+ w (* 2 *comment-box-margin*) 1)
                                       :box-h (+ (* h (length comment-lines)) (* 2 *comment-box-margin*))
                                       )))
        (setf (value newcomment) text)
        newcomment))))

(defmethod initialize-box-value ((self OMComment) &optional value) nil)

(defmethod default-size ((self OMComment))
  (let* ((comment-lines (om-text-to-lines (value self)))
         (longest-line (reduce #'(lambda (s1 s2) (if (> (length s1) (length s2)) s1 s2)) comment-lines)))
    (multiple-value-bind (w h)
        (om-string-size longest-line (box-draw-font self))
      (omp (+ w (* 2 *comment-box-margin*) 1)
           (+ (* h (length comment-lines)) (* 2 *comment-box-margin*))))))

(defmethod maximum-size ((self OMComment)) nil)

(defmethod om-copy ((self OMComment))
  (let ((newbox (call-next-method)))
    (setf (value newbox) (value self))
    newbox))

(defmethod get-box-value ((self OMComment)) nil)
(defmethod eval-box ((self OMComment)) nil)
(defmethod get-obj-to-play ((self OMComment)) nil)

;(defun str-without-nl (str)
;  (map 'string  #'(lambda (x) (if (equal x #\Newline) #\$ x)) str))
;(defun str-with-nl (str)
;  (map 'string  #'(lambda (x) (if (equal x #\$) #\Newline  x)) str))
;(defmethod save-reference ((self OMBoxcomment))
;  (str-without-nl (str-check (reference self))))


(defclass CommentFrame (OMBoxFrame) ())
(defmethod get-box-frame-class ((self OMComment)) 'CommentFrame)

(defmethod make-frame-from-callobj ((self OMComment))
  (let ((view (om-make-graphic-object
               'CommentFrame
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
        ;(append (info-area self)
        (resize-areas self)
        ))


;; not used
(defmethod display-text-and-area ((self CommentFrame))
  (let ((font (or (font-font (text-font (object self))) (om-get-font self)))
        (lines (om-text-to-lines (value (object self)))))

    (multiple-value-bind (w h) (om-string-size (car lines) font)
      (loop for l in (cdr lines) do (setf w (max w (om-string-size l font))))

      (values (value (object self)) *comment-box-margin* *comment-box-margin*
              w (* h (length lines)))
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

            (declare (ignore w))
            (om-with-fg-color (box-draw-text-color box)
              (om-with-font
               font
               (om-draw-string *comment-box-margin*
                               h
                               text :selected nil
                               :wrap (- (w self) (* *comment-box-margin* 2))
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
                     :border t
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

                                                   (let ((new-size (fit-comment-size box (omp (box-w box) (box-h box)) t)))
                                                     (omng-resize box new-size)
                                                     (reset-frame-size self))
                                                   ))
                                  :font (font-font (text-font box))
                                  :border t
                                  :size (om-add-points (omp (box-w box) (box-h box)) (omp 4 4))
                                  :position (omp (box-x box) (box-y box))
                                  )))
      (om-add-subviews container-view textinput)
      (om-set-text-focus textinput t))))


(defmethod fit-comment-size ((box OMComment) size &optional fit)
  (let* ((font (box-draw-font box))
         (contrained-width (max
                            (om-point-x (minimum-size box))
                            (om-point-x size)
                            (+ (apply 'max (mapcar #'(lambda (str)
                                                       (om-string-size str font))
                                                   (mapcan 'string-to-list (string-lines-to-list (value box)))))
                               (* *comment-box-margin* 2)))))
    (om-make-point
     contrained-width
     (max (if fit 0 (om-point-y size))
          (+
           (* (length (om-string-wrap (value box) (- contrained-width (* *comment-box-margin* 2)) font))
              (cadr (multiple-value-list (om-string-size "Dummy" font))))
           (* *comment-box-margin* 2)
           ))
     )))

(defmethod resize-frame-size ((self resize-area) (frame CommentFrame) size)
  (fit-comment-size (object frame) (call-next-method) nil))

(defmethod resize-frame-size ((self h-resize-area) (frame CommentFrame) size)
  (fit-comment-size (object frame) (call-next-method) t))

(defmethod resize-frame-size ((self v-resize-area) (frame CommentFrame) size)
  (fit-comment-size (object frame) (call-next-method) nil))


(defmethod fit-box-and-frame :after ((box OMComment) (frame CommentFrame))
  (let ((new-size (fit-comment-size box (omp (box-w box) (box-h box)) t)))
    (omng-resize box new-size)
    (reset-frame-size frame)))
