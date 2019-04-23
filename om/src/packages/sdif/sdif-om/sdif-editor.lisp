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


(defclass sdiffile-editor (OMEditor) ())

(defmethod object-has-editor ((self SDIFFile)) t)
(defmethod get-editor-class ((self SDIFFile)) 'sdiffile-editor)

(defmethod window-title-for-object ((self SDIFFile)) 
  (string+ "SDIF File - " 
           (if (file-pathname self)
               (namestring (file-pathname self))
             "No file attached")))
  
(defmethod make-editor-window-contents ((editor sdiffile-editor))
  
  (set-g-component editor :filemap-layout (om-make-layout 'om-column-layout))
  (set-g-component editor :matrix-text (om-make-di 'om-simple-text :size (omp nil 22) :font (om-def-font :font1)))
  (set-g-component editor :field-plot (om-make-view 'field-plot-view :editor editor :bg-color (om-def-color :white)))

  (set-g-component editor :matrix-field-menu (om-make-di 
                                              'om-popup-list :size (omp nil 22) :font (om-def-font :font1)
                                              :di-action #'(lambda (item) 
                                                             (when (selection editor)
                                                               (update-plot-data (get-g-component editor :field-plot) 
                                                                                 (f-desc (selection editor)) 
                                                                                 (m-desc (selection editor)) 
                                                                                 (om-get-selected-item-index item))
                                                             ))))
  
  (om-make-layout 'om-row-layout :ratios '(1 nil 1) 
                  :subviews 
                  (list (get-g-component editor :filemap-layout)
                        :divider
                        (om-make-layout 'om-column-layout :ratios '(1 100)
                                        :subviews (list 
                                                   (om-make-layout 'om-row-layout 
                                                                   :subviews 
                                                                   (list 
                                                                    (get-g-component editor :matrix-text)
                                                                    (get-g-component editor :matrix-field-menu)))
                                                   (get-g-component editor :field-plot))
                                        ))
                  )
  )


(defmethod update-to-editor ((editor sdiffile-editor) (from t))
  (call-next-method)
  (init-editor-window editor)) 

;;;==========================================================
;;; DISPLAY / SELECT MATRIX STREAMS
;;;==========================================================

(defclass sdifmat-stream-view (OMEditorView) 
  ((f-desc :initform nil :initarg :f-desc :accessor f-desc)
   (m-desc :initform nil :initarg :m-desc :accessor m-desc)))


(defmethod om-draw-contents ((self sdifmat-stream-view))
  
  (let ((selected (equal (selection (editor self)) self)))
    
    (om-draw-rect 3 0 (- (w self) 6) (- (h self) 3) :fill t
                  :color (if selected 
                             (om-make-color .6 .64 .64) 
                           (om-def-color :light-gray)))
    
    (om-with-fg-color (if selected (om-def-color :white) (om-def-color :dark-gray))
      (om-with-font (om-def-font :font1)
                    (om-draw-string 6 12 (format nil "Matrix: ~A" (mstream-desc-msig (m-desc self))))
                    (om-draw-string 6 24 (format nil "  Matrix Fields: ~A" (mstream-desc-fields (m-desc self))))
                    (om-draw-string 6 36 (format nil "  Max. Elts.: ~D" (mstream-desc-rmax (m-desc self))))
                    (om-draw-string 6 48 (format nil "  Nb. Occurrences: ~D" (mstream-desc-nf (m-desc self)))))
      )))

(defmethod om-view-click-handler ((self sdifmat-stream-view) pos)
  (let ((ed (editor self)))
    (unless (equal (selection ed) self)
      (setf (selection ed) self)
      (update-selected-contents ed (f-desc self) (m-desc self))
      (om-invalidate-view (get-g-component ed :filemap-layout))
      )))

(defmethod update-selected-contents ((editor sdiffile-editor) f-desc m-desc)
  
  (om-set-dialog-item-text 
   (get-g-component editor :matrix-text)
   (if m-desc 
       (format nil "Matrix: ~A" (mstream-desc-msig m-desc))
     "Select a matrix stream on the left pane..."))
  
  (om-set-item-list 
   (get-g-component editor :matrix-field-menu)
   (if m-desc 
       (mstream-desc-fields m-desc)
     nil))

  (update-plot-data (get-g-component editor :field-plot) f-desc m-desc 0)
  
  )

(defmethod init-editor-window ((editor sdiffile-editor))
  
  (let ((sdiffile (object-value editor))
        (map-layout (get-g-component editor :filemap-layout)))
        
    (om-remove-all-subviews map-layout)
    
    (apply 'om-add-subviews 
           (cons map-layout
                 (cons (om-make-di 
                        'om-simple-text :size (omp nil 16)
                        :font (om-def-font :font1) :fg-color (om-def-color :dark-gray)
                        :text (format nil "File: ~A" (file-pathname sdiffile))) 
                       (loop for stream-desc in (file-map sdiffile) collect
                             (om-make-layout 
                              'om-simple-layout :size (omp nil nil) :bg-color (om-def-color :gray) :delta 10
                              :subviews (list 
                                         (om-make-layout 
                                          'om-column-layout :delta 0
                                          :subviews 
                                          (cons 
                                           (om-make-di 
                                            'om-simple-text :size (omp nil 16)
                                            :font (om-def-font :font1) :fg-color (om-def-color :white)
                                            :text (format nil "Stream ~D: ~A [~D frames from ~f to ~fs]" 
                                                          (fstream-desc-id stream-desc) (fstream-desc-fsig stream-desc)
                                                          (fstream-desc-nf stream-desc) (fstream-desc-tmin stream-desc) (fstream-desc-tmax stream-desc)))
                                           (if (fstream-desc-matrices stream-desc)
                                               (loop for mat-desc in (fstream-desc-matrices stream-desc) collect
                                                     (om-make-view 'sdifmat-stream-view :editor editor
                                                                   :m-desc mat-desc :f-desc stream-desc
                                                                   :size (omp nil nil) :bg-color (om-def-color :gray))
                                                     )
                                             (list 
                                              (om-make-di 
                                               'om-simple-text :size (omp nil nil)
                                               :font (om-def-font :font1) :fg-color (om-def-color :white)
                                               :text "[no matrices inside]"))
                                             )
                                           )))
                              )
                             ))))
    
    (update-selected-contents editor nil nil)
    
    ))


;;;==========================================================
;;; PLOTS NUMERIC DATA
;;;==========================================================

(defclass field-plot-view (OMEditorView) 
  ((data :initform nil :initarg :data :accessor data)
   (vmin :accessor vmin :initarg :vmin :initform nil)
   (vmax :accessor vmax :initarg :vmax :initform nil)
   (tmax :accessor tmax :initarg :tmax :initform 0))
  (:default-initargs :draw-with-buffer t))

(defmethod om-draw-contents ((self field-plot-view))
  (when (data self)
    (let* ((mi (vmin self))
           (ma (vmax self))
           (lx 10) (ly 10)
           (lh (- (h self) 20))
           (xfact (if (plusp (tmax self))
                      (/ (- (w self) 20) (tmax self))
                    1))
           (c-min .7)
           (c-fact (if (= 1 (length (data self))) c-min (/ c-min (length (data self)))))
           (delta 0))
      (when (= mi ma) (setf mi (- mi 10) ma (+ ma 10)))
      (loop for row in (reverse (data self))
            for i = 1 then (+ i 1) do
            (let ((c (- c-min (* i c-fact))))
              (om-with-fg-color (om-make-color c c c)
                (if (= 1 (length row)) 
                    (om-draw-string 20 (+ 20 (* i 20)) (format nil "~f" (cadr (car row))))
                  (loop for v on row
                        when (cdr v) do
                        (om-draw-line  (+ lx (* (first (car v)) xfact))
                                       (+ ly (om-scale (second (car v)) lh 0 mi ma))
                                       (+ lx (* (first (cadr v)) xfact))
                                       (+ ly (om-scale (second (cadr v)) lh 0 mi ma))
                                       )
                        )
                  )))
            ))))


(defmethod update-plot-data ((self field-plot-view) f-desc m-desc field-num)
  (let ((ed (editor self)))
    
    (if (and f-desc m-desc field-num)
        
        (multiple-value-bind (sdifdata sdiftimes) 
            (getsdifdata (object-value ed)
                         (fstream-desc-id f-desc)
                         (fstream-desc-fsig f-desc)
                         (mstream-desc-msig m-desc)
                         field-num nil nil nil nil)
          (setf (vmin self) (list-min (flat sdifdata)))
          (setf (vmax self) (list-max (flat sdifdata)))
          (setf (tmax self) (car (last sdiftimes)))
      
          (setf (data self) 
                ;;; TAKES ONLY the FIRST 100 ROWS
                (loop for r from 0 to (min 100 (1- (mstream-desc-rmax m-desc))) collect
                      (loop for timetag in sdiftimes
                            for data in sdifdata 
                            when (nth r data) collect 
                            (list timetag (nth r data)))
                      ))
          )
      (setf (data self) nil))
    (om-invalidate-view self)
    ))


















