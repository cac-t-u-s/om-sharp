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


(in-package :om)


;================================================
;=== CHANNEL CONTROLLER                     
;=== a single track controller 
;================================================
(defclass channel-controls () 
  ((midiport :initform nil :initarg :midiport :accessor midiport :type integer)
   (midichannel :initform 1 :initarg :midichannel :accessor midichannel :type integer)
   (program :initform 0 :initarg :program :accessor program :type integer)
   (pan-ctrl :initform 64 :initarg :pan-ctrl  :accessor pan-ctrl :type integer)
   (control1-num :initform 1 :initarg :control1-num :accessor control1-num :type integer)
   (control2-num :initform 2 :initarg :control2-num :accessor control2-num :type integer)
   (control1-val :initform 0 :initarg :control1-val :accessor control1-val :type integer)
   (control2-val :initform 0 :initarg :control2-val :accessor control2-val :type integer)
   (vol-ctrl :initform 100 :initarg :vol-ctrl :accessor vol-ctrl :type integer)
   (pitch-ctrl :initform 8192 :initarg :pitch-ctrl :accessor pitch-ctrl :type integer)))


(defclass* midi-mix-console (data-frame)
  ((midiport :initform nil :accessor midiport :type integer :documentation "output port number")
   (miditrack :initform 0 :accessor miditrack)
   (channels-ctrl :initform nil :accessor channels-ctrl)))


(defmethod additional-class-attributes ((self midi-mix-console)) '(midiport))

(defmethod initialize-instance :after ((self midi-mix-console) &rest l)
  
  (declare (ignore args))
  
  (setf (channels-ctrl self) 
        (loop for i from 1 to 16 collect 
              (make-instance 'channel-controls 
                             :midiport (midiport self)
                             :midichannel i)))
  )


;======================
; GET-MIDIEVENTS
;======================

(defmethod! get-midievents ((self midi-mix-console) &optional test)
  (let ((evt-list (loop for chan-ctrl in (channels-ctrl self) append 
                        (get-midievents chan-ctrl test))))
    
    (when (miditrack self)
      (setf evt-list
            (loop for tr in (list! (miditrack self)) append
                  (loop for evt in evt-list collect
                        (let ((new-ev (clone evt)))
                          (setf (ev-track new-ev) tr)
                          new-ev))))
      )
    ;;; in case the test had to do with track number... ?
    (get-midievents evt-list test)))


(defmethod! get-midievents ((self channel-controls) &optional test)
  (list
   (make-midievent
    :ev-date 0
    :ev-type :ProgChange
    :ev-chan (midichannel self)
    :ev-port (midiport self)
    :ev-values (list (program self)))
        
   (make-midievent
    :ev-date 0
    :ev-type :CtrlChange
    :ev-chan (midichannel self)
    :ev-port (midiport self)
    :ev-values (list 7 (vol-ctrl self)))
        
   (make-midievent
    :ev-date 0
    :ev-type :CtrlChange
    :ev-chan (midichannel self)
    :ev-port (midiport self)
    :ev-values (list 10 (pan-ctrl self)))
        
   (make-midievent
    :ev-date 0
    :ev-type :PitchBend
    :ev-chan (midichannel self)
    :ev-port (midiport self)
    :ev-values (val2lsbmsb (pitch-ctrl self)))
        
   (make-midievent
    :ev-date 0
    :ev-type :CtrlChange
    :ev-chan (midichannel self)
    :ev-port (midiport self)
    :ev-values (list (control1-num self) (control1-val self)))
        
   (make-midievent
    :ev-date 0
    :ev-type :CtrlChange
    :ev-chan (midichannel self)
    :ev-port (midiport self)
    :ev-values (list (control2-num self) (control2-val self)))
   ))



;=================================
; SENDING CONTROLLER MIDI 
;=================================

(defmethod channel-send-prog ((self channel-controls))
  (let ((event (om-midi::make-midi-evt 
                :type :ProgChange 
                :chan (midichannel self)
                :port (or (midiport self) (get-pref-value :midi :out-port))
                :fields (list (program self)))))
    
    (om-midi::midi-send-evt event)
    t))

(defmethod channel-send-vol ((self channel-controls))
  (let ((event (om-midi::make-midi-evt 
                :type :CtrlChange
                :chan (midichannel self) 
                :port (or (midiport self) (get-pref-value :midi :out-port))
                :fields (list 7  (vol-ctrl self)))))
    (om-midi::midi-send-evt event)
    t))

(defmethod channel-send-pan ((self channel-controls))
  (let ((event (om-midi::make-midi-evt 
                :type :CtrlChange
                :chan (midichannel self) 
                :port (or (midiport self) (get-pref-value :midi :out-port))
                :fields (list 10 (pan-ctrl self)))))
    (om-midi::midi-send-evt event)
    t))
    
(defmethod channel-send-ct1 ((self channel-controls))
  (let ((event (om-midi::make-midi-evt 
                :type :CtrlChange
                :chan (midichannel self) 
                :port (or (midiport self) (get-pref-value :midi :out-port))
                :fields (list (control1-num self) (control1-val self)))))
    (om-midi::midi-send-evt event)
    t))
    
(defmethod channel-send-ct2 ((self channel-controls))
  (let ((event (om-midi::make-midi-evt 
                :type :CtrlChange
                :chan (midichannel self) 
                :port (or (midiport self) (get-pref-value :midi :out-port))
                :fields (list (control2-num self) (control2-val self)))))
    (om-midi::midi-send-evt event)
    t))

(defmethod channel-send-pitch ((self channel-controls))
  (let ((event (om-midi::make-midi-evt 
                :type :PitchBend
                :chan (midichannel self) 
                :port (or (midiport self) (get-pref-value :midi :out-port))
                :fields (pitch-ctrl self))))
    (om-midi::midi-send-evt event)
    t))

(defmethod send-midi-settings ((self channel-controls))
  (channel-send-prog self) 
  (channel-send-vol self) 
  (channel-send-pan self) 
  (channel-send-ct1 self) 
  (channel-send-ct2 self) 
  (channel-send-pitch self))

(defmethod send-midi-settings ((self midi-mix-console))
  (loop for chan-ctrl in (channels-ctrl self) do
        (send-midi-settings chan-ctrl)))

;=================================
; PLAY
;=================================

;;; play in a DATA-STREAM
(defmethod get-frame-action ((self midi-mix-console))
  #'(lambda () 
      (loop for e in (get-midievents self) do
            (funcall (get-frame-action e)))
      ))

;;; PLAY BY ITSELF IN A MAQUETTE...
;;; Interval is the interval INSIDE THE OBJECT
(defmethod get-action-list-for-play ((self midi-mix-console) interval &optional parent)
  (when (in-interval 0 interval :exclude-high-bound t) 
    (list 
     (list 0
           #'(lambda (obj) (funcall (get-frame-action obj)))
           (list self))
     )))


;=================================
; EDITOR
;=================================

(defclass midi-mix-editor (omeditor) 
  ((channel-panels :accessor channel-panels :initform nil)))

(defmethod object-default-edition-params ((self midi-mix-console))
  '((:auto-send t)))
 
(defclass channel-panel (om-column-layout) 
  ((channel-controller :initarg :channel-controller :accessor channel-controller :initform nil)
   
   (programMenu :initform nil :accessor programMenu)
   (volumeText :initform nil :accessor volumeText)
   (volumpeSlider :initform nil :accessor volumeSlider)
   (pitchText :initform nil :accessor pitchText)
   (pitchSlider :initform nil :accessor pitchSlider)
   (panText :initform nil :accessor panText)
   (panSlider :initform nil :accessor panSlider)
   (ctrl1menu :initform nil :accessor ctrl1menu)
   (ctrl1Val :initform nil :accessor ctrl1Val)
   (ctrl1Slider :initform nil :accessor ctrl1Slider)
   (ctrl2menu :initform nil :accessor ctrl2menu)
   (ctrl2Val :initform nil :accessor ctrl2Val)
   (ctrl2Slider :initform nil :accessor ctrl2Slider)
   ))

(defmethod object-has-editor ((self midi-mix-console)) t)
(defmethod get-editor-class ((self midi-mix-console)) 'midi-mix-editor)
(defmethod editor-view-class ((self midi-mix-editor)) 'channel-panel)
(defmethod get-obj-to-play ((self midi-mix-editor)) (object-value self))



;==============================
; ACTIONS ON CHANNEL PANELS:
;==============================

(defun pan2str (panvalue)
  (let* ((value (- panvalue 64))
         (new-str (cond ((= value 0) (number-to-string value))
                        ((< value 0) (format nil "L~D" (- value)))
                        ((> value 0) (format nil "R~D" value)))))
    new-str))


(defmethod set-values-on-panel ((cc channel-controls) (panel channel-panel))
  
  (om-set-selected-item (programMenu panel) (number-to-name (program cc) *midi-gm-programs*))

  (om-set-slider-value (panSlider panel) (pan-ctrl cc))
  (om-set-dialog-item-text (panText panel) (string+ "Pan " (pan2str (pan-ctrl cc))))

  (om-set-slider-value (volumeSlider panel) (vol-ctrl cc))
  (om-set-dialog-item-text (volumeText panel) (format nil "Vol ~D" (vol-ctrl cc)))

  (om-set-slider-value (pitchSlider panel) (pitch-ctrl cc))
  (om-set-dialog-item-text (pitchText panel) (format nil "Pitch (mc) ~D" (pitchwheel-to-mc (pitch-ctrl cc))))
 
  (om-set-selected-item (ctrl1Menu panel) (number-to-name (control1-num cc) *midi-controllers*))

  (om-set-slider-value (ctrl1Slider panel) (control1-val cc))
  (om-set-dialog-item-text (ctrl1Val panel) (number-to-string (control1-val cc)))
  
  
  (om-set-selected-item (ctrl2Menu panel) (number-to-name (control2-num cc) *midi-controllers*))
  
  (om-set-slider-value (ctrl2Slider panel) (control2-val cc))
  (om-set-dialog-item-text (ctrl2Val panel) (number-to-string (control1-val cc)))

  )


;;; change program from menu
(defmethod change-channel-program ((self channel-panel) editor value)
  (let ((cc (channel-controller self)))    
    (setf (program cc) value)
    (report-modifications editor)
  
    (when (editor-get-edit-param editor :auto-send)
      (channel-send-prog cc))))


;;; change pan from slider
(defmethod change-channel-pan ((self channel-panel) editor value)
  (let ((cc (channel-controller self)))
    (unless (= value (pan-ctrl cc))
      (setf (pan-ctrl cc) value)
      (om-set-dialog-item-text (panText self) (string+ "Pan " (pan2str value)))
      (report-modifications editor))
    
    (when (editor-get-edit-param editor :auto-send)
      (channel-send-pan cc))))


(defmethod change-channel-vol ((self channel-panel) editor value)
 (let ((cc (channel-controller self)))
  (unless (= value (pan-ctrl cc))
    (setf (vol-ctrl cc) value)
    (om-set-dialog-item-text (VolumeText self) (format nil "Vol ~D" value))
    (report-modifications editor))
  
  (when (editor-get-edit-param editor :auto-send)
    (channel-send-vol cc))
  ))

  
(defmethod change-channel-pitchbend ((self channel-panel) editor value)
  (let ((cc (channel-controller self)))
    (unless (= value (pitch-ctrl cc))
      (setf (pitch-ctrl cc) value)
      (om-set-dialog-item-text (PitchText self) (format nil "Pitch (mc) ~D" (pitchwheel-to-mc value)))
      (report-modifications editor))
    
    (when (editor-get-edit-param editor :auto-send) 
      (channel-send-pitch cc))
    ))


;;; change ctrl1 from menu
(defmethod change-channel-ctrl1 ((self channel-panel) editor value)
  (let ((cc (channel-controller self)))    
    (setf (control1-num cc) value)
    (report-modifications editor)
  
    (when (editor-get-edit-param editor :auto-send)
      (channel-send-ct1 cc))))

;;; change ctrl2 from menu
(defmethod change-channel-ctrl2 ((self channel-panel) editor value)
  (let ((cc (channel-controller self)))    
    (setf (control2-num cc) value)
    (report-modifications editor)
  
    (when (editor-get-edit-param editor :auto-send)
      (channel-send-ct2 cc))))

;;; change ctrl1 val from slider
(defmethod change-channel-ctrl1-val ((self channel-panel) editor value)
  (let ((cc (channel-controller self)))
    (unless (= value (control1-val cc))
      (setf (control1-val cc) value)
      (om-set-dialog-item-text (Ctrl1Val self) (format nil "~D" value))
      (report-modifications editor))
    (when (editor-get-edit-param editor :auto-send) 
      (channel-send-ct1 cc))
    ))

;;; change ctrl2 val from slider
(defmethod change-channel-ctrl2-val ((self channel-panel) editor value)
  (let ((cc (channel-controller self)))
    (unless (= value (control2-val cc))
      (setf (control2-val cc) value)
      (om-set-dialog-item-text (Ctrl2Val self) (format nil "~D" value))
      (report-modifications editor))
    (when (editor-get-edit-param editor :auto-send) 
      (channel-send-ct2 cc))
    ))


;;; reset from button
;;; note: doesn't reset the choic of CTL1 et CTL2 controllers
;;; probably shouldn't reset GM program either... ?
(defmethod reset-all-values ((self channel-panel) editor)
  (change-channel-program self editor 0)
  (om-set-selected-item (programMenu self) (number-to-name 0 *midi-gm-programs*))
  (change-channel-pan self editor 64)
  (om-set-slider-value (panSlider self) 64)
  (change-channel-vol self editor 100)
  (om-set-slider-value (volumeSlider self) 100)
  (change-channel-pitchbend self editor 8192)
  (om-set-slider-value (pitchSlider self) 8192)
  (change-channel-ctrl1-val self editor 0)
  (om-set-slider-value (ctrl1Slider self) 0)
  (change-channel-ctrl2-val self editor 0)
  (om-set-slider-value (ctrl2Slider self) 0)
)



(defmethod make-channel-track-view ((self channel-controls) editor)
  
  (let ((font (om-def-font :font1))
        (w 80)
        (panel (om-make-layout 
                'channel-panel 
                :align :center
                :channel-controller self)))
    
    (om-add-subviews 
     panel 
     (om-make-di 'om-simple-text 
                 :size (omp 40 20) 
                 :font (om-def-font :font3b)
                 :text (format nil "Ch. ~D" (midichannel self))
                 )
     
     (setf (programMenu panel)
           (om-make-di 'om-popup-list 
                       :size (omp w 24)
                       :font font
                       :items (mapcar #'car *midi-gm-programs*)
                       :value (number-to-name (program self) *midi-gm-programs*)
                       :di-action #'(lambda (item)
                                      (change-channel-program panel editor (name-to-number (om-get-selected-item item) *midi-gm-programs*)))
                       ))
     
     (setf (pantext panel)
           (om-make-di 'om-simple-text 
                       :size (omp w 20) 
                       :font font
                       :text (string+ "Pan " (pan2str (pan-ctrl self)))
                       ))
     
     (setf (panSlider panel)
           (om-make-di 'om-slider  
                       :size (om-make-point w 24)
                       :di-action #'(lambda (item)
                                      (change-channel-pan panel editor (om-slider-value item)))
                       :increment 1
                       :range '(0 127)
                       :value (pan-ctrl self)
                       :direction :horizontal
                       :tick-side :none
                       ))

     :separator
     
     (setf (volumetext panel)
           (om-make-di 'om-simple-text 
                       :size (omp 40 20) 
                       :font font
                       :text (format nil "Vol ~D" (vol-ctrl self))
                       ))
     
     (setf (volumeSlider panel)
           (om-make-di 'om-slider  
                       :size (om-make-point 24 100)
                       :di-action #'(lambda (item)
                                      (change-channel-vol panel editor (om-slider-value item)))
                       :increment 1
                       :range '(0 127)
                       :value (vol-ctrl self)
                       :direction :vertical
                       :tick-side :none
                       ))

     :separator
     
     (setf (pitchtext panel)
           (om-make-di 'om-simple-text 
                       :size (omp w 20) 
                       :font font
                       :text (format nil "Pitch (mc) ~D" (pitchwheel-to-mc (pitch-ctrl self)))
                       ))
     
     (setf (pitchSlider panel)
           (om-make-di 'om-slider  
                       :size (om-make-point w 24)
                       :di-action #'(lambda (item)
                                      (change-channel-pitchbend panel editor (om-slider-value item)))
                       :increment 1
                       :range '(0 16383)
                       :value (pitch-ctrl self)
                       :direction :horizontal
                       :tick-side :none
                       ))
     
     :separator
     
     (om-make-di 'om-simple-text 
                 :size (omp w 20) 
                 :font (om-def-font :font1)
                 :text "Ctrl Changes"
                 )

     (setf (Ctrl1Menu panel)
           (om-make-di 'om-popup-list 
                       :size (omp w 24)
                       :font font
                       :items (mapcar #'car *midi-controllers*)
                       :value (number-to-name (control1-num self) *midi-controllers*)
                       :di-action #'(lambda (item)
                                      (change-channel-ctrl1 panel editor 
                                                            (name-to-number (om-get-selected-item item) *midi-controllers*)))
                       ))
               
     (setf (Ctrl1Slider panel)
           (om-make-di 'om-slider  
                       :size (om-make-point w 24)
                       :di-action #'(lambda (item)
                                      (change-channel-ctrl1-val panel editor (om-slider-value item)))
                       :increment 1
                       :range '(0 127)
                       :value (control1-val self)
                       :direction :horizontal
                       :tick-side :none
                       ))
     
     (setf (Ctrl1Val panel)
           (om-make-di 'om-simple-text 
                       :size (omp w 20) 
                       :font font
                       :text (format nil "~D" (control1-val self))
                       ))

     (setf (Ctrl2Menu panel)
           (om-make-di 'om-popup-list 
                       :size (omp w 24)
                       :font font
                       :items (mapcar #'car *midi-controllers*)
                       :value (number-to-name (control2-num self) *midi-controllers*)
                       :di-action #'(lambda (item)
                                      (change-channel-ctrl2 panel editor 
                                                            (name-to-number (om-get-selected-item item) *midi-controllers*)))
                       ))

          (setf (Ctrl2Slider panel)
           (om-make-di 'om-slider  
                       :size (om-make-point w 24)
                       :di-action #'(lambda (item)
                                      (change-channel-ctrl2-val panel editor (om-slider-value item)))
                       :increment 1
                       :range '(0 127)
                       :value (control2-val self)
                       :direction :horizontal
                       :tick-side :none
                       ))
     
          (setf (Ctrl2Val panel)
                (om-make-di 'om-simple-text 
                            :size (omp w 20) 
                            :font font
                            :text (format nil "~D" (control2-val self))
                            ))

          :separator

          (om-make-di 'om-button 
                      :text "Reset" 
                      :size (omp 80 24)
                      :di-action #'(lambda (item)
                                     (declare (ignore item))
                                     (reset-all-values panel editor))
                      )

     ;;; end add-subviews
     )
    
    panel))







(defmethod make-editor-window-contents ((editor midi-mix-editor))
  
  (let ((obj (object-value editor)))

    (setf (channel-panels editor)
          (loop for cc in (channels-ctrl obj)
                collect (make-channel-track-view cc editor)))
  
    (let ((port-box ;;; needs to be enabled/disabled by other items... 
                   (om-make-graphic-object 'numbox 
                                           :size (omp 40 20) 
                                           :position (omp 0 0) 
                                           :font (om-def-font :font1)
                                           :bg-color (om-def-color :white)
                                           :value (or (midiport obj)
                                                      (get-pref-value :midi :out-port))
                                           :enabled (midiport obj)
                                           :after-fun #'(lambda (item)
                                                         (setf (midiport obj) (value item))
                                                         (report-modifications editor))
                                           )))
      (om-make-layout 
       'om-column-layout 
       :subviews (list
                  (om-make-layout 
                   'om-row-layout 
                   :subviews (channel-panels editor))
              
                  :separator
              
                  (om-make-layout 
                   'om-row-layout 
                   :subviews (list 
                              (om-make-di 'om-check-box 
                                          :size (omp 100 20) 
                                          :font (om-def-font :font1)
                                          :text "Out MIDI port:"
                                          :checked-p (midiport obj)
                                          :di-action #'(lambda (item)
                                                         (if (om-checked-p item)
                                                             (progn 
                                                               (enable-numbox port-box t)
                                                               (setf (midiport obj) (get-pref-value :midi :out-port)))
                                                           (progn 
                                                             (set-value port-box (get-pref-value :midi :out-port))
                                                             (enable-numbox port-box nil)
                                                             (setf (midiport obj) nil)))
                                                         (report-modifications editor)))
                          
                              (om-make-view 'om-view 
                                            :size (omp 80 20) 
                                            :subviews (list port-box))
                              
                              nil

                              (om-make-di 'om-check-box 
                                          :size (omp 80 20) 
                                          :font (om-def-font :font1)
                                          :text "send on edit"
                                          :checked-p (editor-get-edit-param editor :auto-send)
                                          :di-action #'(lambda (item)
                                                         (editor-set-edit-param editor :auto-send
                                                                                (om-checked-p item)))
                                          )
                          
                              ))
                  ))
      )))
