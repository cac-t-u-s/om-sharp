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

(defmethod extra-window-title-info ((self sound))
  (if (file-pathname self)
      (namestring (file-pathname self))
    "temp buffer"))

(defmethod frame-display-modes-for-object ((self sound-editor) (object sound)) '(:lines))

(defmethod make-left-panel-for-object ((editor sound-editor) (object t) view)
  (declare (ignore object view))
  (om-make-view 'om-view :size (omp 28 nil)))

(defmethod make-editor-controls ((editor sound-editor))
  (let ((sound (object-value editor)))
    (om-make-layout
     'om-row-layout
     :align :center
     :subviews
     (list
      (om-make-di 'om-simple-text :text "Gain"
                  :size (omp 30 20)
                  :font (om-def-font :gui))
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

        (editor-draw-sound-waveform sound editor self (x1 self) (x2 self) (stream-id self))

      ;;; no sound
      (om-with-fg-color (om-def-color :light-gray)
        (om-with-font (om-make-font "Arial" (round (h self) 4) :style '(:bold))
                      (om-draw-string 10 (+ (round (h self) 2) (round (h self) 8)) "No sound loaded..")
                      )))

    ; will draw the markers
    (call-next-method)
    ))



;;;; EDITOR CACHE DISPLAY

(defstruct sound-display-cache (array) (resolutions))
(defstruct sound-display-cache-resolution (samples-per-pixel) (cache))


;;; box null = this is for the sound editor
(defmethod get-cache-display-for-draw ((self sound) (box null))
  (when (or (buffer self) (file-pathname self))
    (let* ((height 512)
           (window 8)
           (array (get-sample-array-from-sound self window)))
      (when array
        (let ((max-pict-res (min (array-dimension array 1) 16368))
              (n-samples (n-samples self)))
          (make-sound-display-cache
           :resolutions
           (reverse
            (append
             (list (make-sound-display-cache-resolution
                    :samples-per-pixel 1
                    :cache (get-buffer self))
                   (make-sound-display-cache-resolution
                    :samples-per-pixel window
                    :cache array))
             (loop for samples-per-pixel = (* window 4) then (* samples-per-pixel 4)
                   for new-array-size = (ceiling n-samples samples-per-pixel)
                   for new-array = (resample-sample-array array new-array-size)
                   while (>= new-array-size 1024)
                   collect
                   (make-sound-display-cache-resolution
                    :samples-per-pixel samples-per-pixel
                    :cache (if (< new-array-size max-pict-res)
                               (create-waveform-pict new-array height)
                             new-array))))))
          )))))


(defmethod find-best-cache-display ((self sound-display-cache) samples-per-pixel)
  (let ((rep
         (or (find samples-per-pixel (sound-display-cache-resolutions self)
                   :key #'sound-display-cache-resolution-samples-per-pixel
                   :test #'>=)
             (car (last (sound-display-cache-resolutions self))))))
    ; (print (list "search" samples-per-pixel (sound-display-cache-resolution-samples-per-pixel rep)))
    rep))

(defparameter *gen-cache-flag* nil)

(defun editor-draw-sound-waveform (sound editor view from to &optional (sound-id 0))

  (cond ((and (access-from-file sound) (not (probe-file (file-pathname sound))))
         (om-draw-string 10 16 (string+ "File not found: " (namestring (file-pathname sound)))
                         :color (om-def-color :red)))

        ((not (and (sample-rate sound) (n-samples sound) (n-channels sound)))
         (om-draw-string 10 16 "Error with loaded sound data."
                         :color (om-def-color :red)))

        (t
         (let ((editor-display-cache-ready-p (nth sound-id (cache-display-list editor))))
           ;; (re)create display cache if needed
           (unless (or editor-display-cache-ready-p *gen-cache-flag*)
             (setf *gen-cache-flag* t)
             (om-run-process "Generate waveform cache"
                             #'(lambda (ed)
                                 (setf (cache-display-list ed)
                                       (if (multi-display-p ed)
                                           (mapcar #'(lambda (o) (get-cache-display-for-draw o nil)) (multi-obj-list ed))
                                         (list (get-cache-display-for-draw (object-value ed) nil))))
                                 (setf *gen-cache-flag* nil)
                                 (editor-invalidate-views editor))
                             :args
                             (list editor)))

           (let* (;; Use the box display cache if the editor cache is not ready
                  (display-cache (or editor-display-cache-ready-p
                                     (ensure-cache-display-draw (object editor) (object-value editor))))
                  (sound-duration (get-obj-dur sound))
                  (draw-end-time (min sound-duration to)) ;; space after sound end
                  (draw-end-pix (x-to-pix view draw-end-time))
                  (duration-to-draw (- draw-end-time from))
                  (samples-per-pixel (ceiling (ms->samples duration-to-draw (sample-rate sound)) draw-end-pix))
                  ;; If using the editor display cache, find best resolution
                  (cache (if (sound-display-cache-p display-cache)
                             (find-best-cache-display display-cache samples-per-pixel)
                           display-cache)))
             (when cache
               (cond
                ;; Using the editor display cache at high-res (cache is an array or a buffer)
                ((and (sound-display-cache-resolution-p cache)
                      (or (arrayp (sound-display-cache-resolution-cache cache))
                          (om-sound-buffer-p (sound-display-cache-resolution-cache cache))))
                 (let* ((array (sound-display-cache-resolution-cache cache))
                        (res (sound-display-cache-resolution-samples-per-pixel cache))
                        (sr (sample-rate sound)))
                   (draw-waveform array
                                  (w view) (h view)
                                  (ms->samples from sr)
                                  (ms->samples to sr)
                                  res)))

                ;; Using the box display cache or the editor display cache at low-res (cache is a picture)
                (t
                 (let* ((pict (if (sound-display-cache-resolution-p cache)
                                  (sound-display-cache-resolution-cache cache)
                                cache))
                        (pict-factor (/ (om-pict-width pict) sound-duration)))
                   (when (and pict (not (equal :error pict)))
                     (let* ((n-channels (n-channels sound))
                            (channel-h (/ (h view) n-channels)))
                       (dotimes (c n-channels)
                         (let ((y (* channel-h (+ c .5))))
                           (om-draw-line 0 y (w view) y
                                         :color (get-pref-value :appearance :waveform-color)))))
                     (om-draw-picture pict
                                      :w (w view) :h (h view)
                                      :src-x (* pict-factor from)
                                      :src-w (* pict-factor (- to from))))
                   (unless editor-display-cache-ready-p
                     (om-draw-string 10 16 "Generating waveform display..." :color (om-def-color :orange)))
                   ))))
             )))))


(defmethod update-to-editor ((editor sound-editor) (from t))
  (unless (or (equal from editor)
              (and (multi-display-p editor)
                   (equal from (container-editor editor))))
    (setf (cache-display-list editor) nil))
  (update-window-name editor)
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
