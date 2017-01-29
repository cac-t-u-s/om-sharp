;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
;Authors: J. Bresson


(in-package :om)

;editors default parameters
(defparameter *OM-DEFAULT-ROOM-COLOR* (list 0.9 0.9 0.9 1.0))


;;; 3DC editor 

(defclass 3DC-editor (multi-display-editor-mixin OMEditor multi-view-editor play-editor-mixin)
  ((top-bpc-editor :accessor top-bpc-editor :initform nil)
   (front-bpc-editor :accessor front-bpc-editor :initform nil)
   (3Dp :accessor 3Dp :initform nil)
   (ctrlp :accessor ctrlp :initform nil)

   (color-style :accessor color-style :initform :single)
   
   (timeline-editor :accessor timeline-editor :initform nil)
   ;(time-x-editor :accessor time-x-editor :initform nil)
   ;(time-y-editor :accessor time-y-editor :initform nil)
   ;(time-z-editor :accessor time-z-editor :initform nil)
   ;room parameters
   (room-editor :accessor room-editor :initform nil)
   (room-size :accessor room-size :initform 2)
   

   ;interpolation parameters
   (interpolation-mode :accessor interpolation-mode :initform :none)
      ;OSC parameters
   (osc-editor :accessor osc-editor :initform nil)
   (osc-port :accessor osc-port :initform 6666)
   (osc-status :accessor osc-status :initform nil)
   (osc-process :accessor osc-process :initform nil)
   (cursor-position :accessor cursor-position :initform '(0 0 0))
   (cursor-status :accessor cursor-status :initform nil)
   (last-osc-point :accessor last-osc-point :initform nil)
   (osc-first-time :accessor osc-first-time :initform nil)
   ))


(defmethod object-default-edition-params ((self 3DC))
  (append (call-next-method)
          '((:line-width 1))))


(defmethod object-has-editor ((self 3dc)) t)
(defmethod get-editor-class ((self 3DC)) '3DC-editor)

(defmethod make-3D-background-element ((self background-element)) nil)


;===== MultiDisplay API

(defmethod enable-multi-display ((self 3dc-editor) obj-list) 
  (call-next-method)
  (enable-multi-display (top-bpc-editor self) obj-list)
  (enable-multi-display (front-bpc-editor self) obj-list)
  (set-3D-objects self))


(defmethod disable-multi-display ((self 3dc-editor))
  (call-next-method)
  (disable-multi-display (top-bpc-editor self))
  (disable-multi-display (front-bpc-editor self))
  (set-3D-objects self))

;=====

;;; from play-editor-mixin
(defmethod get-obj-to-play ((self 3dc-editor)) 
 (object-value self))

(defmethod play-editor-callback ((self 3dc-editor) time)
  (om-invalidate-view (3dp self))
  (om-invalidate-view (get-g-component (top-bpc-editor self) :main-panel))
  (om-invalidate-view (get-g-component (front-bpc-editor self) :main-panel))
  (call-next-method))

(defmethod cursor-panes ((self 3dc-editor))
  (when (timeline-editor self)
    (cursor-panes (timeline-editor self))))

(defmethod set-time-display ((self 3dc-editor) time)
  (set-time-display (timeline-editor self) time)
  (call-next-method))

;;;==========================
;;; SYSTEM CALLBACKS
;;;==========================

(defmethod make-editor-window-contents ((editor 3DC-editor))
  (let* ((object (object-value editor))
         (decimals (decimals object))
         (3dPanel (om-make-view '3dpanel
                                ;:title "3D view" 
                                :editor editor
                                :bg-color (om-def-color :dark-gray)
                                :g-objects (create-GL-objects editor)
                                ))
         (top-editor (make-instance 'bpc-editor :object (object editor)  :container-editor editor :decimals decimals
                                    :x-axis-key :x :y-axis-key :y 
                                    :make-point-function #'(lambda (x y) (make-3Dpoint :x x :y y))))
         (front-editor (make-instance 'bpc-editor :object (object editor) :container-editor editor :decimals decimals
                                      :x-axis-key :x :y-axis-key :z 
                                      :make-point-function #'(lambda (x y) (make-3Dpoint :x x :z y))))
         
         (top-panel (om-make-view 'BPC-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 100)
                                  :scale-fact (expt 10 decimals) :editor top-editor))
         (front-panel (om-make-view 'BPC-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 100)
                                    :scale-fact (expt 10 decimals) :editor front-editor))

         (top-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 60 16)))
         (front-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 60 16)))
         (timeline-editor (make-instance 'timeline-editor :object (object editor) :container-editor editor ))
         (timeline (om-make-layout 'om-row-layout))
         (top-area (om-make-layout 'om-row-layout ;:align :center
                                   :ratios '(0.8 0.5 0.05)
                                   :subviews (list
                                              nil
                                              (make-time-monitor editor :time (if (action object) 0 nil))
                                              (om-make-layout 
                                               'om-row-layout ;:size (omp 60 20)
                                               :subviews (list (make-play-button editor :enable t) ;(action object) 
                                                               (make-pause-button editor :enable t) 
                                                               (make-stop-button editor :enable t))))
                                   ))
         (bottom-area nil)   
         (top-layout 
          (om-make-layout 
           'om-grid-layout :dimensions '(2 3) :ratios '((0.01 0.99) (0.01 0.98 0.01))
           :subviews 
           (list nil 
                 (om-make-layout 
                  'om-row-layout 
                  :subviews (list (om-make-graphic-object 'om-item-text :text "Top view" :size (omp 60 18))
                                  top-mousepos-txt))
                 (setf (y-ruler top-panel) 
                       (om-make-view 'y-ruler-view :related-views (list top-panel) 
                                     :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
                 top-panel 
                 nil
                 (setf (x-ruler top-panel)
                       (om-make-view 'x-ruler-view :related-views (list top-panel) 
                                     :size (omp nil 30) :bg-color (om-def-color :white) :decimals decimals)))))
         (front-layout 
          (om-make-layout 
           'om-grid-layout :dimensions '(2 3)
           :ratios '((0.01 0.99) (0.01 0.98 0.01))
           :subviews 
           (list 
            nil 
            (om-make-layout 
             'om-row-layout 
             :subviews (list (om-make-graphic-object 'om-item-text :text "Front view" :size (omp 60 18))
                             front-mousepos-txt))
            (setf (y-ruler front-panel) 
                  (om-make-view 'y-ruler-view :related-views (list front-panel) 
                                :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
            front-panel nil
            (setf (x-ruler front-panel)
                  (om-make-view 'x-ruler-view :related-views (list front-panel) 
                                :size (omp nil 30) :bg-color (om-def-color :white) :decimals decimals)))))
        
         )

    ;bottom area definition
    (setf bottom-area (om-make-layout 
                       'om-row-layout :size (omp nil 40)
                       :ratios '(1 1 1 10)
                       :subviews
                       (list  ;;; GENERAL
                              (om-make-layout 
                               'om-column-layout
                               :subviews
                               (list 
                                (om-make-di 'om-simple-text :text "Options (general)" 
                                            :size (omp 200 22) 
                                            :font (om-def-font :font1b))
                                (om-make-layout 
                                 'om-row-layout
                                 :subviews
                                 (list 
                                  (om-make-di 'om-simple-text :text "draw mode:" 
                                              :size (omp 68 22) 
                                              :font (om-def-font :font1))
                                  (om-make-di 'om-popup-list :items '(:draw-all :points-only :lines-only) 
                                              :size (omp 80 24) :font (om-def-font :font1)
                                              :value (editor-get-edit-param editor :draw-style)
                                              :di-action #'(lambda (list) 
                                                             (editor-set-edit-param editor :draw-style (om-get-selected-item list))
                                                             ))))
                                (om-make-layout 
                                 'om-row-layout
                                 :subviews
                                 (list 
                                  (om-make-di 'om-simple-text :text "Line size:" 
                                              :size (omp 38 20) 
                                           :font (om-def-font :font1))
                                  (om-make-graphic-object 'numbox 
                                                          :value (editor-get-edit-param editor :line-width) 
                                                          :bg-color (om-def-color :white)
                                                      :border t
                                                       :size (om-make-point 40 24) 
                                                       :font (om-def-font :font1)
                                                       :min-val 1 :max-val 10
                                                       :after-fun #'(lambda (item)
                                                                      (editor-set-edit-param editor :line-width (value item))))
                                  ))

                                (om-make-di 'om-check-box :text "show timeline" :size (omp 100 24) :font (om-def-font :font1)
                                          :checked-p (editor-get-edit-param editor :show-timeline)
                                          :di-action #'(lambda (item) 
                                                         (editor-set-edit-param editor :show-timeline (om-checked-p item))
                                                         (clear-timeline timeline-editor)
                                                         (om-invalidate-view timeline)
                                                         (when (om-checked-p item) 
                                                           (make-timeline-view timeline-editor))
                                                         (om-update-layout (main-view editor))))
                                ))
                              (om-make-layout 
                               'om-column-layout
                               :subviews
                               (list 
                                (om-make-di 'om-simple-text :text "3D view" 
                                            :size (omp 100 22) 
                                            :font (om-def-font :font1b))
                                (om-make-layout 
                                 'om-row-layout
                                 :subviews
                                 (list (om-make-di 'om-check-box :text "Axes" :size (omp 46 24) :font (om-def-font :font1)
                                                   :checked-p (editor-get-edit-param editor :show-axes)
                                                   :di-action #'(lambda (item) 
                                                                  (editor-set-edit-param editor :show-axes (om-checked-p item))
                                                                  (update-3D-view editor)))
                                       (om-make-view '3D-axis-view :size (omp 30 24))
                                       )
                                 )
                                (om-make-di 'om-check-box :text "Room" :size (omp 46 24) :font (om-def-font :font1)
                                                   :checked-p (editor-get-edit-param editor :show-room)
                                                   :di-action #'(lambda (item) 
                                                                  (editor-set-edit-param editor :show-room (om-checked-p item))
                                                                  (update-3D-view editor)))))

                              (om-make-layout 
                               'om-column-layout
                               :subviews
                               (list 
                                (om-make-di 'om-simple-text :text "2D views" 
                                            :size (omp 100 22) 
                                            :font (om-def-font :font1b))
                                (om-make-di 'om-check-box :text "Indices" :size (omp 60 24) :font (om-def-font :font1)
                                            :checked-p (editor-get-edit-param editor :show-indices)
                                            :di-action #'(lambda (item) 
                                                           (editor-set-edit-param editor :show-indices (om-checked-p item))))))

                                 
                               
                               
                               ;(om-make-di 'om-simple-text :text "Offset:" 
                               ;            :size (omp 38 20) 
                               ;            :font (om-def-font :font1))
                               ;(om-make-graphic-object 'numbox 
                               ;                        :value (gesture-interval-time top-editor)
                               ;                        :bg-color (om-def-color :white)
                               ;                        :border t
                               ;                        :size (om-make-point 40 24) 
                               ;                        :font (om-def-font :font2)
                               ;                        :min-val 0
                               ;                        :after-fun #'(lambda (item)
                               ;                                       (setf (gesture-interval-time top-editor) (value item)
                               ;                                             (gesture-interval-time front-editor) (value item))))
                               ;(om-make-di 'om-button :text "Room..." :size (omp 70 24) :font (om-def-font :font1)
                               ;            :enabled (editor-get-edit-param editor :show-room)
                               ;            :di-action #'(lambda (item) 
                               ;                           (open-room-editor editor)))
                               ;(om-make-di 'om-button :text "Osc..." :size (omp 70 24) :font (om-def-font :font1)
                               ;            :di-action #'(lambda (item) 
                               ;                           (open-osc-editor editor)))
                              
                              nil
                              )))

    
    (set-g-component editor :main-panel 3dpanel)
    (set-g-component top-editor :main-panel top-panel)
    (set-g-component front-editor :main-panel front-panel)
    (set-g-component top-editor :mousepos-txt top-mousepos-txt)
    (set-g-component front-editor :mousepos-txt front-mousepos-txt)
    (setf (3Dp editor) 3dPanel
          (top-bpc-editor editor) top-editor
          (front-bpc-editor editor) front-editor)
    (update-3d-curve-vertices-colors editor)
    
     ;timeline
    (setf (timeline-editor editor) timeline-editor)
    (set-g-component timeline-editor :main-panel timeline)
    (when (editor-get-edit-param editor :show-timeline)
      (make-timeline-view timeline-editor))
      
    (om-make-layout 
     'om-row-layout
     :ratios '(9.9 0.1) 
     :subviews (list 
                (om-make-layout 
                 'om-column-layout 
                 :subviews (list 
                            top-area
                            (om-make-layout 
                             'om-row-layout 
                             :subviews (list 
                                        3dpanel 
                                        :divider  
                                        (om-make-layout 
                                         'om-column-layout 
                                         :subviews  (list
                                                     top-layout
                                                     :divider
                                                     front-layout)))
                             :ratios '(0.6 nil 0.4)
                             )
                            timeline
                            bottom-area) 
                 :delta 2
                 :ratios '(0.01 1 0.01 0.01))
                (call-next-method))) ;side panel 
    ))

(defmethod editor-close ((self 3dc-editor)) 
  (3dc-osc-stop-receive self)
  (setf (room-editor self) nil
        (osc-editor self) nil)
  (call-next-method))

(defmethod editor-invalidate-views ((self 3Dc-editor))
  (editor-invalidate-views (top-bpc-editor self))
  (editor-invalidate-views (front-bpc-editor self))
  (editor-invalidate-views (timeline-editor self))
  (update-editor-3d-object self)
  (update-3d-view self))

(defmethod editor-window-init-size ((self 3DC-editor)) (om-make-point 800 600))       

(defmethod init-window ((win OMEditorWindow) (editor 3DC-editor))
  (call-next-method)
  (reinit-ranges (top-bpc-editor editor))
  (reinit-ranges (front-bpc-editor editor))
  (update-editor-3d-object editor)
  (om-init-3D-view (3Dp editor)))

;when the object is modified
(defmethod update-to-editor ((self 3DC-editor) (from t))
  (setf (selection self) nil)
  (when (window self)
    (set-3d-objects self) 
    (time-sequence-update-internal-times (object-value self))
    (enable-play-controls self (action (object-value self)))
    (update-sub-editors self)
    ))

(defmethod update-to-editor ((self 3DC-editor) (from timeline-editor))
  (setf (selection self) (get-indices-from-points (object-value self) (selection from)))
  (when (window self)
    (update-editor-3d-object self)
    (time-sequence-update-internal-times (object-value self))
    (update-3d-view self)
    (update-sub-editors self)
    )
  (report-modifications self) 
  )

;; called from an internal editor
(defmethod update-to-editor ((self 3DC-editor) (from bpf-editor))
  ;(print (selection from))
  (setf (selection self) (selection from))
  ;(report-modifications self)
  (update-sub-editors self)
  (when (window self)
    (update-editor-3d-object self)
    (update-3d-view self)))



(defmethod format-3D-points  ((self 3DC))
  (mat-trans (list (x-points self) (y-points self) (z-points self) (times self))))

(defmethod create-GL-objects ((self 3DC-editor))
  (let ((obj-list (cons (object-value self) (remove (object-value self) (multi-obj-list self)))))
    (remove 
     nil
     (append 
      (mapcar #'(lambda (obj) 
                  (make-instance 
                   '3D-lines
                   :points (format-3d-points obj) :color (color obj) 
                   :draw-style :draw-all 
                   :line-width (editor-get-edit-param self :line-width)))
              obj-list)
      (mapcar 'make-3D-background-element 
             (editor-get-edit-param self :background))
      (when (editor-get-edit-param self :show-room) 
        (list (make-3D-background-element
               (make-instance 'project-room :size (room-size self)))))
      ))))

(defmethod set-3D-objects ((self 3dc-editor)) 
  (om-set-gl-objects (3Dp self) (create-GL-objects self))
  (update-3D-view self))


(defmethod update-editor-3d-object ((self 3dc-editor))
  (let ((3d-obj (car (om-get-gl-objects (3DP self))))
        (obj (object-value self)))
    (setf (selected-points 3d-obj) (selection self)
          (draw-style 3d-obj) (editor-get-edit-param self :draw-style))
    (om-set-3Dobj-points 3d-obj (format-3d-points obj))
    (update-3d-curve-vertices-colors self)))
    


(defmethod update-sub-editors ((self 3DC-editor))
  (when (window self)
    (setf (selection (top-bpc-editor self)) (selection self)
          (selection (front-bpc-editor self)) (selection self))
    (editor-invalidate-views (top-bpc-editor self))
    (editor-invalidate-views (front-bpc-editor self))
    (update-timeline-editor self)))

(defmethod update-3D-view ((self 3DC-editor))
  (gl-user::clear-gl-display-list (3Dp self))
  (om-invalidate-view (3Dp self)))


;;;==========================
;;; MENUS
;;;==========================

(defmethod om-menu-items ((self 3dc-editor))
  (remove nil
          (list 
           (main-app-menu-item)
           (om-make-menu "File" (default-file-menu-items self))
           (om-make-menu "Edit" (bpf-edit-menu-items self))
           (om-make-menu "Windows" (default-windows-menu-items self))
           (om-make-menu "Help" (default-help-menu-items self))
           )))

(defmethod reverse-points ((self 3dc-editor))
  (time-sequence-reverse (object-value self))
  (editor-invalidate-views self)
  (update-to-editor (timeline-editor self) self))


(defmethod select-all-command ((self 3dc-editor))
  #'(lambda () 
      (setf (selection self) (list T))
      (update-editor-3d-object self)
      (update-sub-editors self)
      (update-3d-view self)
      ))

(defmethod get-info-command ((self 3dc-editor)) 
  #'(lambda () 
      (show-inspector (object-value self) self)))

;;;==========================
;;; KEY LISTENER
;;;==========================


(defmethod editor-key-action ((editor 3DC-editor) key)
  (let* ((top-editor (top-bpc-editor editor))
         (top-panel (get-g-component top-editor :main-panel))
         (front-editor (front-bpc-editor editor))
         (front-panel (get-g-component front-editor :main-panel))
         (selected-editor (and (selected-view editor) (editor (selected-view editor))))
         (3dpanel (3dp editor)))
    (case key
      (#\- (zoom-rulers top-editor :dx -0.1 :dy -0.1)
           (zoom-rulers front-editor :dx -0.1 :dy -0.1)
           (zoom-view 3dpanel 0.8))
      (#\+ (zoom-rulers top-editor :dx 0.1 :dy 0.1)
           (zoom-rulers front-editor :dx 0.1 :dy 0.1)
           (zoom-view 3dpanel 1.2))
      (:om-key-delete 
       (delete-editor-selection editor)
       (report-modifications top-editor) ;; why ?
       (update-editor-3d-object editor)
       (update-sub-editors editor))
      (:om-key-esc 
       (set-selection editor nil)
       (call-next-method) ;;; will also reset the cursor interval
       (update-editor-3d-object editor)
       (update-sub-editors editor))
      ;;; we use the internal editors to make the moves because they have ruler hints
      (:om-key-left
       (move-editor-selection top-editor :dx (/ (- (get-units (x-ruler top-panel) (if (om-shift-key-p) 400 40))) (scale-fact top-panel)))
       (time-sequence-update-internal-times (object-value editor))
       (report-modifications top-editor))
      (:om-key-right
       (move-editor-selection top-editor :dx (/ (get-units (x-ruler top-panel) (if (om-shift-key-p) 400 40)) (scale-fact top-panel))) 
       (time-sequence-update-internal-times (object-value editor))
       (report-modifications top-editor))
      (:om-key-up
       (let* ((ed (or selected-editor top-editor))
              (panel (get-g-component top-editor :main-panel)))
         (move-editor-selection ed :dy (/ (get-units (y-ruler panel) (if (om-shift-key-p) 400 40)) (scale-fact panel)))
         (time-sequence-update-internal-times (object-value editor))
         (report-modifications ed)))
      (:om-key-down
       (let* ((ed (or selected-editor top-editor))
             (panel (get-g-component top-editor :main-panel)))
         (move-editor-selection ed :dy (/ (- (get-units (y-ruler panel) (if (om-shift-key-p) 400 40))) (scale-fact panel)))
         (time-sequence-update-internal-times (object-value editor))
         (report-modifications ed)))
      (:om-key-pageup
       (move-editor-selection front-editor :dy (/ (get-units (y-ruler front-panel) (if (om-shift-key-p) 400 40)) (scale-fact front-panel)))
       (time-sequence-update-internal-times (object-value editor))
       (report-modifications front-editor))
      (:om-key-pagedown
       (move-editor-selection front-editor :dy (/ (- (get-units (y-ruler front-panel) (if (om-shift-key-p) 400 40))) (scale-fact front-panel)))
       (time-sequence-update-internal-times (object-value editor))
       (report-modifications front-editor))
      (#\Space
       (editor-stop editor)
       (player-play-object *general-player* (get-obj-to-play editor) editor))
      (otherwise (editor-key-action (top-bpc-editor editor) key)
                 (editor-key-action (front-bpc-editor editor) key))
      )))


;;;==========================
;;; some functions from BPF/BPC-editor...
;;;==========================

(defmethod delete-editor-selection ((self 3dc-editor))
  (if (find T (selection self))
      (setf (point-list (object-value self)) nil)
    (mapcar 
     #'(lambda (i) (remove-nth-timed-point-from-time-sequence (object-value self) i))
     (sort (selection self) '>)
     ))
  (setf (selection self) nil)
  (update-sub-editors self)
  ;(update-to-editor (timeline-editor self) self)
  )


;;;==========================
;;; 3D PANEL
;;;==========================

;;;3D Panel
(defclass 3DPanel (OMEditorView om-opengl-view) ())

;;;(defmethod get-3DC-editor ((self 3DPanel))
;;;  (editor (om-view-window self)))

(defmethod update-bg-color ((self 3DPanel) color)
  (om-set-bg-color self color)
  (om-invalidate-view self))

(defmethod om-draw-contents ((self 3DPanel))
  (let ((editor (editor self)))
      
    (when (editor-get-edit-param editor :show-axes)
      (opengl:gl-push-matrix) 
      (draw-3D-axes editor)
      (opengl:gl-pop-matrix))
    
    (when (osc-process editor)
      (opengl:gl-push-matrix) 
      (draw-3D-cursor-position editor)
      (opengl:gl-pop-matrix))
    
    (when (equal (player-get-object-state (player editor) (object-value editor)) :play)
      (opengl:gl-push-matrix) 
      (draw-3D-player-cursor-position editor (player-get-object-time (player editor) (object-value editor)))
      (opengl:gl-pop-matrix))
    )
  )

(defmethod draw-3D-axes ((self 3DC-Editor))
  (let* ((l  (float (room-size self)))
         (arrow-size (/ l 20.0)))
    ;X axis
    (opengl:gl-color3-f 0.8 0.3 0.3)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f (- l) 0.0 0.0) 
    (opengl:gl-vertex3-f l 0.0 0.0)
    (opengl:gl-end)
    (draw-cone (list l 0.0 0.0) arrow-size 90.0 (list 0.0 1.0 0.0))

    ;Y axis
    (opengl:gl-color3-f 0.3 0.6 0.3)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f 0.0 (- l) 0.0) 
    (opengl:gl-vertex3-f 0.0 l 0.0) 
    (opengl:gl-end)
    (draw-cone (list 0.0 l 0.0) arrow-size -90.0 (list 1.0 0.0 0.0))

    ;Z axis
    (opengl:gl-color3-f 0.3 0.3 0.6)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f 0.0 0.0 (- l)) 
    (opengl:gl-vertex3-f 0.0 0.0 l)
    (opengl:gl-end)
    (draw-cone (list 0.0 0.0 l) arrow-size 0.0 (list 0.0 1.0 0.0))
    )
  (restore-om-gl-colors-and-attributes))

(defmethod  draw-3D-cursor-position ((self 3DC-Editor))
  "Draw the cursor" 
  (if (cursor-status self)
      (opengl:gl-color4-f 1.0 0.1 0.1 1.0)
    (opengl:gl-color4-f 0.1 1.0 0.1 1.0))
  (draw-sphere (cursor-position self) (/ (editor-get-edit-param self :line-width) 20))
  (restore-om-gl-colors-and-attributes))

(defmethod  draw-3D-player-cursor-position ((self 3DC-Editor) time)
  "Draw the current position point at given time" 
  (let ((point (time-sequence-get-active-timed-item-at (object-value self) time)))
    (when point
      (opengl:gl-color4-f 0.9 0.3 0.1 1.0)
      (draw-sphere (point-to-list point) (/ (editor-get-edit-param self :line-width) 20))
      ))    
  (restore-om-gl-colors-and-attributes))
 
(defmethod point-to-list ((point 3dpoint))
  (list (3dpoint-x point) (3dpoint-y point) (3dpoint-z point) (3dpoint-time point)))

(defmethod update-3d-curve-vertices-colors ((self 3dc-editor))
  (let ((g-objects (om-get-gl-objects (3Dp self)))
        (om-objects (cons (object-value self) (remove (object-value self) (multi-obj-list self)))))
    (loop for 3dobj in g-objects
          for 3dc in om-objects do 
          (setf (vertices-colors-interpol 3dobj) (not (eql (color-style self) :speed))
                (vertices-colors 3dobj) (get-vertices-colors self 3DC)))))

(defmethod get-vertices-colors ((self 3dc-editor) (obj 3dc))
  "create a vector of colors for a 3D-timed-curve depending on the mode selected"
  (let* ((size (max (length (point-list obj)) 1))
         (min_h 0.65)
         (max_h 0.0)
         (range_h (- max_h min_h))
         (h_step (/ (- max_h min_h) (if (= size 1) 1 (1- size)))))
    (if (not (point-list obj))
        (default-color-vertices self obj)
      (case (color-style self) 
        ((equal :indices)
         (om-display-curve-min-max self 0 (1- size))
         (loop for i from 0 to (1- size)
               collect
               (let ((h (+ min_h (* i h_step))))
                 (om-make-color-hsv h 0.8 0.8)))
         )
        ((equal :length)
         (let* ((profile (give-normalized-cumulative-length-profile obj)))
           (om-display-curve-min-max self 0 (give-length obj))
           (loop for val in profile
                 collect
                 (om-make-color-hsv (+ min_h (* val range_h)) 0.8 0.8)))
         )
        ((equal :speed)
         (let* ((speeds (give-speed-profile obj))
               (max_speed nil)
               (min_speed nil))
           (if (< (length speeds) 2)
               (progn
                 (setf max_speed (or (car speeds) 0))
                 (setf min_speed (or (car speeds) 0)))
             (progn
             ;removing -1 and replacing by max speed
               (setf max_speed (reduce 'max speeds))
               (setf speeds (loop for speed in speeds 
                                  collect (if (= speed -1) max_speed speed)))
               (setf min_speed (reduce 'min speeds))
               (setf speeds (normalize-speeds-list speeds min_speed max_speed))))
           (setf speeds (cons (car speeds) speeds))
           (om-display-curve-min-max self min_speed max_speed)
           (loop for speed in speeds
                 collect  (om-make-color-hsv (+ min_h (* range_h (or speed 0.5))) 0.8 0.8)
                 )))
        (otherwise 
         (om-display-curve-min-max self nil nil)
         (default-color-vertices self obj))))))


(defun normalize-speeds-list (speeds min_speed max_speed)
  (let ((range_speed (- max_speed min_speed)))
    (setf range_speed (- max_speed min_speed))
    (when (zerop range_speed)
      (setf range_speed 1))
    (setf speeds (om/ (om- speeds min_speed) range_speed))))

(defmethod default-color-vertices ((self 3dc-editor) (obj 3dc))
  (make-list (length (point-list obj)) :initial-element (or (color obj) (om-def-color :light-gray))))


;;;;;;;;;;;;;;;;;;;;
;; color-scale-view
;;;;;;;;;;;;;;;;;;;;

(defmethod om-display-curve-min-max ((self 3dc-editor) min max)
  (when (get-g-component self :curve-color-scale-min) 
    (om-set-dialog-item-text (get-g-component self :curve-color-scale-min) (if min (format nil "~,6f" min) "")))
  (when (get-g-component self :curve-color-scale-max)
    (om-set-dialog-item-text (get-g-component self :curve-color-scale-max) (if max (format nil "~,6f" max) "")))
  (when (room-editor self)
    (om-invalidate-view (room-editor self)))
  )

(defclass color-scale-view (om-view) ())

(defmethod om-draw-contents ((self color-scale-view))
  (let* ((size (om-view-size self))
        (width (om-point-x size))
        (steps 20)
        (w_step (round (/ width steps))) 
        (height (om-point-y size))
        (min_h 0.65)
        (max_h 0.0)
        (h_step (/ (- max_h min_h) steps)))
    (loop for i from 0 to steps
          do
          (let ((h (+ min_h (* i h_step))))
            (om-draw-rect (* i w_step)  0 (+ (* (+ 1 i) w_step) 1) height :color (om-make-color-hsv h 0.8 0.8) :fill t)))))

(defclass 3D-axis-view (om-view) ())

(defmethod om-draw-contents ((self 3d-axis-view))
  (call-next-method)
  (om-with-fg-color (om-make-color 0.8 0.3 0.3)
    (om-draw-line 10 5 40 5)
    (om-draw-string 1 7 "x"))
  (om-with-fg-color (om-make-color 0.3 0.6 0.3)
    (om-draw-line 10 12 40 12)
    (om-draw-string 1 14 "y"))
  (om-with-fg-color (om-make-color 0.3 0.3 0.6)
    (om-with-font (om-def-font :font1)
                  (om-draw-line 10 19 40 19)
                  (om-draw-string 1 21 "z"))))

;;;;;;;;;;;;;;;;;;
;;ROOM INSPECTOR...
;;;;;;;;;;;;;;;;;;

(defmethod open-room-editor ((editor 3dc-editor))
  (unless (room-editor editor)
    (let ((scale-min (om-make-di 'om-simple-text :size (omp 80 20)))
          (scale-view (om-make-view 'color-scale-view :size (om-make-point 120 30)))
          (scale-max (om-make-di 'om-simple-text :text "" :size (omp 80 20))))
      (setf (room-editor editor) 
            (om-make-window 
             'om-window 
             :title "room editor" 
             :subviews
             (list (om-make-layout 
                    'om-column-layout
                    :size (om-make-point 400 400) 
                    :subviews
                    (list 
                     (om-make-layout 
                      'om-row-layout
                      :subviews
                      (list
                       (om-make-di 'om-simple-text :text "Show Room:" 
                                   :size (omp 80 30))
                       (om-make-di 'om-check-box :size (omp 16 20) :font (om-def-font :font1)
                                   :checked-p (editor-get-edit-param editor :show-room)
                                   :di-action #'(lambda (item) 
                                                  (editor-set-edit-param editor :show-room (om-checked-p item))
                                                  (set-3D-objects editor)
                                                  (update-3D-view editor)))))
                     (om-make-layout 
                      'om-row-layout
                      :subviews
                      (list
                       (om-make-di 'om-simple-text :text "Room size:" 
                                   :size (omp 80 30))
                       (om-make-graphic-object 'numbox 
                                               :value (room-size editor) :min-val 0.01 :size (omp 40 18)
                                               :bg-color (om-def-color :white)
                                               :after-fun #'(lambda (numbox) 
                                                              (setf (room-size editor) (value numbox) )
                                                              (om-invalidate-view (3dp editor))))))
                     (om-make-layout 
                      'om-row-layout
                      :subviews
                      (list
                       (om-make-di 'om-simple-text :text "Color style:" :size (omp 80 30))
                       (om-make-di 'om-popup-list 
                                   :items '(:single :indices :length :speed) 
                                   :size (omp 80 22) 
                                   :value (color-style editor)
                                   :di-action #'(lambda (list) 
                                                  (setf (color-style editor) (om-get-selected-item list))
                                                  (update-3d-curve-vertices-colors editor)
                                                  (update-3D-view editor)))))

                     (om-make-layout 'om-row-layout
                                     :subviews
                                     (list scale-min scale-view scale-max))                 
                     (om-make-di 'om-check-box :text "Anaglyph" :size (omp 75 24) :font (om-def-font :font1)
                                 :checked-p gl-user::*om-3d-anaglyph*
                                 :di-action #'(lambda (item) 
                                                (gl-user::opengl-enable-or-disable-anaglyph (om-checked-p item))
                                                (update-3D-view editor))))))))
      (set-g-component editor :curve-color-scale scale-view)
      (set-g-component editor :curve-color-scale-min scale-min)
      (set-g-component editor :curve-color-scale-max scale-max)
      ))
  (if (om-window-open-p (room-editor editor))
      (om-select-window (room-editor editor))
    (om-open-window (room-editor editor)))
  (update-3d-curve-vertices-colors editor))

;;;;;;;;;;;;;;;;;;;;;;;
;OSC specific methods
;;;;;;;;;;;;;;;;;;;;;;;

(defmethod open-osc-editor ((editor 3dc-editor))
  (unless (osc-editor editor)
    (setf (osc-editor editor) 
          (om-make-window 
           'om-window :title "OSC editor" 
           :size (om-make-point 300 nil)
           :subviews 
           (list (om-make-layout 
                  'om-column-layout
                  :subviews
                  (list 
                   (om-make-di 'om-multi-text :size (omp 300 100) :text '("You can send OSC Messages to the 3DC object." "/3dc/clear resets the current 3dc." "/3dc/move x y z displays a green dot representing the current position." "/3dc/add x y z time appends the point to the curve (if the distance with the previous point is less than the distance treshold)."))
                   (om-make-layout 
                    'om-row-layout
                    :subviews
                    (list 
                     (om-make-di 'om-simple-text :text "OSC-Port:" 
                                 :size (omp 100 30))
                     (om-make-graphic-object 'numbox 
                                             :value (osc-port editor) :min-val 0 :size (omp 40 18)
                                             :bg-color (om-def-color :white)
                                             :after-fun #'(lambda (numbox) 
                                                            (setf (osc-port editor) (value numbox) )
                                                            (3DC-osc-refresh-port editor)))
                     (om-make-di 'om-check-box :text "Start/Stop OSC" 
                                 :checked-p (osc-status editor)
                                 :size (omp 100 40)
                                 :di-action #'(lambda (item)
                                                (3DC-osc-start-stop-receive editor)))))
                   (om-make-layout 
                    'om-row-layout
                    :subviews
                    (list 
                     (om-make-di 'om-simple-text :text "Distance Treshold:" 
                                 :size (omp 100 30))
                     (om-make-graphic-object 'numbox 
                                             :value *3DC_OSC_DIST_TRESHOLD* :min-val 0. :size (omp 40 18)
                                             :bg-color (om-def-color :white)
                                             :after-fun #'(lambda (numbox) 
                                                            (setf *3DC_OSC_DIST_TRESHOLD* (value numbox))))))))))))
  (if (om-window-open-p (osc-editor editor))
      (om-select-window (osc-editor editor))
    (om-open-window (osc-editor editor)))
  )

(defmethod 3DC-osc-refresh-port ((self 3dc-editor))
  (when (osc-status self)
    (3DC-osc-stop-receive self)
    (3DC-osc-start-receive self)))

(defmethod 3DC-osc-start-stop-receive ((self 3DC-editor))
  (print (osc-status self))
  (if (osc-status self)
      (3DC-osc-stop-receive self)
    (setf (osc-process self) (3DC-osc-start-receive self)))
  (update-3d-view self)
  )

(defmethod 3DC-osc-start-receive ((self 3DC-editor))
  (let ((port (osc-port self)))
    (if (and port (numberp port))
        (progn 
          (print (format nil "3DC-OSC-RECEIVE START on port ~D" port))
          (setf (osc-status self) t)
          (om-start-udp-server port "localhost"
                               #'(lambda (msg) 
                                   (let* ((message (osc-decode msg)))
                                     (3dc-osc-process-message self message)
                                     )
                                   nil)))
      (om-beep-msg (format nil "error - bad port number for OSC-RECEIVE: ~A" port)))))

(defmethod 3dc-osc-stop-receive ((self 3DC-editor))
  (when (osc-process self)
    (om-stop-udp-server (osc-process self))
    (om-print (format nil "RECEIVE STOP: ~A" (om-process-name (osc-process self))) "UDP"))
    (setf (osc-process self) nil)
    (setf (osc-status self) nil))

(defconstant 3dc_osc_add "/3dc/add")
(defconstant 3dc_osc_move "/3dc/move")
(defconstant 3dc_osc_clear "/3dc/clear")
(defvar 3dc_osc_state nil)

(defvar *3DC_OSC_DIST_TRESHOLD* 0.1)

(defmethod 3dc-osc-process-message ((self 3dc-editor) message)
  (let ((address (car message))
        (content (cdr message))
        (object-value self))
    (cond
     ((equal address 3dc_osc_clear)
      (setf (last-osc-point self) nil)
      (setf (osc-first-time self) nil)
      (setf (point-list obj) nil)
      (report-modifications self)
      (update-sub-editors self))
     ((equal address 3dc_osc_add) 
      (unless 3dc_osc_state
        (setf 3dc_osc_state t))
      (setf (cursor-status self) t)
      (setf (cursor-position self) content)
      (unless (osc-first-time self)
        (setf (osc-first-time self) (or (cadddr content) 0)))
      (let ((point (make-3dpoint :x (car content) :y (cadr content) :z (caddr content) :time (- (cadddr content) (osc-first-time self)))))
        (if (or (not (last-osc-point self))(>= (om-points-distance (last-osc-point self) point) *3DC_OSC_DIST_TRESHOLD*))
            (progn 
              (insert-timed-point-in-time-sequence obj point (length (point-list obj)))
              (setf (last-osc-point self) point)
              (report-modifications self)
              (if (= (mod (length (point-list obj)) 10) 9)
                  (editor-invalidate-views self))
              (om-invalidate-view (3Dp self))))
        ))
     ((equal address 3dc_osc_move)
      (when 3dc_osc_state
        (update-sub-editors self)
        (setf 3dc_osc_state nil))
      (setf (cursor-status self) nil)
      (setf (cursor-position self) content)
      (om-invalidate-view (3dp self)))
     )
    ))

;;; Timeline specific code not used anymore...
(defmethod get-timeline-foldable-views ((self 3dc-editor) &key obj time-ruler)
  (let* ((decimals (decimals obj))
         (x-label (om-make-graphic-object 'om-item-text :text " x/t" :size (omp 30 15) :font (om-def-font :font1)))
         (x-editor (make-instance 'bpf-editor :object (object self)  :container-editor self :decimals decimals
                                  :x-axis-key :time :y-axis-key :x))
         (x-panel (om-make-view 'bpf-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 80) ;:scale-fact (expt 10 decimals)
                                :editor x-editor))
         (x-y-ruler (om-make-view 'y-ruler-view :related-views (list x-panel) :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
         (x-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 120 15) :font (om-def-font :font1)))
         (y-label (om-make-graphic-object 'om-item-text :text " y/t" :size (omp 30 15) :font (om-def-font :font1)))
         (y-editor (make-instance 'bpf-editor :object (object self)  :container-editor self :decimals decimals
                                  :x-axis-key :time :y-axis-key :y))
         (y-panel (om-make-view 'bpf-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 80) ;:scale-fact (expt 10 decimals)
                                :editor y-editor))
         (y-y-ruler (om-make-view 'y-ruler-view :related-views (list y-panel) :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
         (y-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 120 15) :font (om-def-font :font1)))
         (z-label (om-make-graphic-object 'om-item-text :text " z/t" :size (omp 30 15) :font (om-def-font :font1)))
         (z-editor (make-instance 'bpf-editor :object (object self)  :container-editor self :decimals decimals
                                  :x-axis-key :time :y-axis-key :z))
         (z-panel (om-make-view 'bpf-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 80) ;:scale-fact (expt 10 decimals)
                                :editor z-editor))
         (z-y-ruler (om-make-view 'y-ruler-view :related-views (list z-panel) :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
         (z-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 120 15) :font (om-def-font :font1))))
    
    (set-g-component x-editor :main-panel x-panel)
    ;(setf (grid x-editor) t)
    (set-g-component x-editor :mousepos-txt x-mousepos-txt)
    (setf (y-ruler x-panel) x-y-ruler)
    (setf (related-views time-ruler) (append (list x-panel) (related-views time-ruler)))
    (reinit-ranges x-editor)

    (set-g-component y-editor :main-panel y-panel)
    ;(setf (grid y-editor) t)
    (set-g-component y-editor :mousepos-txt y-mousepos-txt)
    (setf (y-ruler y-panel) y-y-ruler)
    (setf (related-views time-ruler) (append (list y-panel) (related-views time-ruler)))
    (reinit-ranges y-editor)

    (set-g-component z-editor :main-panel z-panel)
    ;(setf (grid z-editor) t)
    (set-g-component z-editor :mousepos-txt z-mousepos-txt)
    (setf (y-ruler z-panel) z-y-ruler)
    (setf (related-views time-ruler) (append (list z-panel) (related-views time-ruler)))
    (reinit-ranges z-editor)

    (list
     (om-make-layout 'om-column-layout 
                     :ratios '(0.01 1)
                     :subviews
                     (list
                      (om-make-layout 'om-row-layout :subviews (list x-label x-mousepos-txt) :align :bottom)
                      (om-make-layout 'om-row-layout :subviews (list x-y-ruler x-panel) :ratios '(0.001 1))))
      (om-make-layout 'om-column-layout 
                     :ratios '(0.01 1)
                     :subviews
                     (list
                      (om-make-layout 'om-row-layout :subviews (list y-label y-mousepos-txt) :align :bottom)
                      (om-make-layout 'om-row-layout :subviews (list y-y-ruler y-panel) :ratios '(0.001 1))))
      (om-make-layout 'om-column-layout 
                     :ratios '(0.01 1)
                     :subviews
                     (list
                      (om-make-layout 'om-row-layout :subviews (list z-label z-mousepos-txt) :align :bottom)
                      (om-make-layout 'om-row-layout :subviews (list z-y-ruler z-panel) :ratios '(0.001 1)))))))



