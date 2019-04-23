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
; File author: J. Garcia
;============================================================================

(in-package :om)

(defclass osc-curvce-input-manager ()
  ((editor :accessor editor :initform editor :initarg :editor)
   (port :accessor port :initform 6666)
   (active-p :accessor active-p :initform nil)
   (process :accessor process :initform nil)
   (window :accessor window :initform nil)
   (cursor-position :accessor cursor-position :initform '(0 0 0))
   (cursor-status :accessor cursor-status :initform nil)
   (dist-threshold :accessor dist-threshold :initform 0.1)
   (last-osc-point :accessor last-osc-point :initform nil)
   (osc-first-time :accessor osc-first-time :initform nil))
  )

(defmethod osc-manager-restart ((self osc-curvce-input-manager))
  (when (active-p self)
    (osc-manager-stop-receive self)
    (osc-manager-start-receive self)))

(defmethod osc-manager-start-receive ((self osc-curvce-input-manager))
  (osc-manager-stop-receive self) ;; just in case
  (let ((port (port self)))
    (if (and port (numberp port))
        (progn 
          (print (format nil "3DC-OSC-RECEIVE START on port ~D" port))
          (setf (active-p self) t)
          (setf (process self)
                (om-start-udp-server port "localhost"
                               #'(lambda (msg) 
                                   (let* ((message (osc-decode msg)))
                                     (osc-manager-process-message self message)
                                     )
                                   nil)))
          (update-to-editor (editor self) self))
      (om-beep-msg (format nil "error - bad port number for OSC-RECEIVE: ~A" port))
      )))

(defmethod osc-manager-stop-receive ((self osc-curvce-input-manager))
  (when (process self)
    (om-stop-udp-server (process self))
    (om-print (format nil "RECEIVE STOP: ~A" (om-process-name (process self))) "UDP"))
    (setf (process self) nil)
    (setf (active-p self) nil)
    (update-to-editor (editor self) self))

(defmethod open-osc-manager ((self osc-curvce-input-manager))
  (if (and (window self) (om-window-open-p (window self)))
      (om-select-window (window self))
    (setf (window self)
          (om-make-window  
           'om-window  :title "OSC manager"
           :size (om-make-point 300 nil)
           :subviews 
           (list  
            (om-make-layout 
             'om-column-layout
             :subviews
             (list 
              (om-make-di 'om-simple-text :size (omp 400 80) 
                          :text (format nil "Send OSC Messages to edit 2D/3D objects:~%- /3dc/clear resets the current object.~%- /3dc/move x y z displays the current position.~%- /3dc/add x y z time appends a new point.")
                          )
              ;;; (if the distance with the previous one is lesser than the distance treshold)
              (om-make-layout 
               'om-row-layout
               :subviews
               (list 
                (om-make-di 'om-simple-text 
                            :text "OSC port:" 
                            :size (omp 150 20) 
                           ;:font (om-def-font :font1)
                            )
                (om-make-graphic-object 'numbox 
                                        :value (port self) :size (omp 40 18)
                                        :bg-color (om-def-color :white)
                                        ;:font (om-def-font :font1)
                                        :min-val 0 
                                        :after-fun #'(lambda (numbox) 
                                                       (setf (port self) (value numbox))
                                                       (osc-manager-restart self)))

                nil
                (om-make-di 'om-check-box 
                          :text "Start/Stop OSC" 
                           ;:font (om-def-font :font1)
                          :checked-p (active-p self)
                          :size (omp 150 20)
                          :di-action #'(lambda (item)
                                         (if (om-checked-p item)
                                             (osc-manager-start-receive self)
                                           (osc-manager-stop-receive self)
                                           )))
                ))
              
              (om-make-layout 
               'om-row-layout
               :subviews
               (list 
                (om-make-di 'om-simple-text :text "Distance treshold:" 
                            ;:font (om-def-font :font1)
                            :size (omp 150 30))
                (om-make-graphic-object 'numbox 
                                        :value (dist-threshold self) 
                                        :min-val 0.0 :size (omp 40 18)
                                        ;:font (om-def-font :font1)
                                        :bg-color (om-def-color :white)
                                        :after-fun #'(lambda (numbox) 
                                                       (setf (dist-threshold self) (value numbox)))))
               ))))
           ))))
   
  
(defmethod close-osc-manager ((self osc-curvce-input-manager)) 
  (osc-manager-stop-receive self)
  (when (window self) 
    (om-close-window (window self))
    (setf (window self) nil)))


(defvar *3dc_osc_state* nil)

(defmethod osc-manager-clear-callback ((editor t)) nil)
(defmethod osc-manager-add-callback ((editor t)) nil)
(defmethod osc-manager-move-callback ((editor t)) nil)

(defmethod osc-manager-process-message ((self osc-curvce-input-manager) message)
  (let ((address (car message))
        (content (cdr message)))
    (cond
     ((string-equal address "/3dc/clear")
      (setf (last-osc-point self) nil)
      (setf (osc-first-time self) nil)
      (osc-manager-clear-callback (editor selff)))
     ((string-equal address "/3dc/add") 
      (unless *3dc_osc_state* (setf *3dc_osc_state* t))
      (setf (cursor-status self) t)
      (setf (cursor-position self) content)
      (unless (osc-first-time self)
        (setf (osc-first-time self) (or (cadddr content) 0)))
      (let ((point (make-3dpoint :x (car content) :y (cadr content) :z (caddr content) :time (- (cadddr content) (osc-first-time self)))))
        (when (or (not (last-osc-point self))
                  (>= (om-points-distance (last-osc-point self) point) (dist-threshold self)))
          (setf (last-osc-point self) point)
          (osc-manager-add-callback (editor self))
          )))
     ((equal address "/3dc/move")
      (when *3dc_osc_state*
        (osc-manager-move-callback (editor self))
        (setf 3dc_osc_state nil))
      (setf (cursor-status self) nil)
      (setf (cursor-position self) content)
      ))
    ))
