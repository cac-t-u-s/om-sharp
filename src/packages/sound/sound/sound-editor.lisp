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

;====================
; SOUND EDITOR
;====================

(in-package :om)


(defclass sound-editor (data-stream-editor)
  ((cache-display-list :accessor cache-display-list :initform nil)))

(defclass sound-panel (stream-panel) ())
(defmethod get-editor-class ((self sound)) 'sound-editor)
(defmethod editor-view-class ((self sound-editor)) 'sound-panel)

(defmethod window-title-for-object ((self sound))
  (string+ "SOUND - "
           (if (file-pathname self)
               (namestring (file-pathname self))
             "Temp Buffer")))

(defmethod frame-display-modes-for-object ((self sound-editor) (object sound)) '(:lines))

(defmethod make-editor-controls ((editor sound-editor))
  (let ((sound (object-value editor)))
    (om-make-layout
     'om-row-layout
     :align :center
     :subviews
     (list
      (om-make-di 'om-simple-text :text "Gain"
                  :size (omp 40 20)
                  :font (om-def-font :font1))
      (om-make-view
       'om-view :size (om-make-point 28 20)
       :subviews
       (list (om-make-graphic-object 'numbox
                                     :value (gain sound)
                                     :bg-color (om-def-color :white)
                                     :border t
                                     :decimals 2
                                     :size (om-make-point 36 18)
                                     :font (om-def-font :font1)
                                     :min-val 0.0 :max-val 10.0
                                     :change-fun #'(lambda (item)
                                                     (set-gain sound (get-value item)))
                                     :after-fun #'(lambda (item)
                                                    (declare (ignore item))
                                                    (report-modifications editor)))
             ))))))


(defmethod editor-view-after-init-space ((self sound)) 0)

(defmethod default-editor-min-x-range ((self sound-editor)) 0)

(defmethod om-draw-contents ((self sound-panel))

  (when (get-pref-value :appearance :waveform-bg)
    (om-draw-rect 0 0 (w self) (h self) :color (get-pref-value :appearance :waveform-bg) :fill t))

  (let* ((editor (editor self))
         (sound (if (multi-display-p editor)
                    (nth (stream-id self) (multi-obj-list editor))
                  (object-value editor))))
    (if (or (buffer sound) (and (access-from-file sound) (valid-pathname-p (file-pathname sound))))
        (draw-sound-waveform sound editor self (x1 self) (x2 self) (stream-id self))
      ;;; no sound
      (om-with-fg-color (om-def-color :light-gray)
        (om-with-font (om-make-font "Arial" (round (h self) 4) :style '(:bold))
                      (om-draw-string 10 (+ (round (h self) 2) (round (h self) 8)) "No sound loaded..")
                      )))

    ; will draw the markers
    (call-next-method)
    ))


(defun draw-sound-waveform (sound editor view from to &optional (sound-id 0))
  (unless (nth sound-id (cache-display-list editor))
    (setf (cache-display-list editor)
          (if (multi-display-p editor)
              (mapcar #'(lambda (o) (get-cache-display-for-draw o nil)) (multi-obj-list editor))
            (list (get-cache-display-for-draw (object-value editor) nil)))))
  (let ((dur (get-obj-dur sound))
        (pict (nth sound-id (cache-display-list editor))))
    (when (and pict (not (equal :error pict)))
      (om-draw-picture pict
                       :w (w view) :h (h view)
                       :src-h (* (om-pict-height pict))
                       :src-x (* (om-pict-width pict) (/ from dur))
                       :src-w (* (om-pict-width pict) (/ (- to from) dur))))
    ))


(defmethod update-to-editor ((editor sound-editor) (from t))
  (unless (or (equal from editor)
              (and (multi-display-p editor)
                   (equal from (container-editor editor))))
    (setf (cache-display-list editor) nil))
  (call-next-method))


(defmethod editor-key-action ((editor sound-editor) key)
  (let* ((panel (active-panel editor))
         (stream (object-value editor)))
    (case key
      (#\l
       (when (selection editor)
         (let ((newlabel (om-get-user-string "enter a new label for selected markers"
                                             :initial-string (or (label (nth (car (selection editor)) (frames stream)))
                                                                 ""))))
           (when newlabel
             (loop for pos in (selection editor) do
                   (setf (label (nth pos (frames stream))) newlabel))
             (om-invalidate-view panel)
             (report-modifications editor)
             )
           )))
      (#\L
       (loop for pos in (selection editor) do
             (setf (label (nth pos (frames stream))) nil))
       (om-invalidate-view panel)
       (report-modifications editor))
      (:om-key-esc NIL) ;;; we don't want to reinit-x-ranges as in the next-method
      (otherwise (call-next-method)))
    ))
