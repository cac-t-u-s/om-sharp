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


(in-package :om)

(defclass CercleEditor (OMEditor) 
  ((front :accessor front :initform 0)
   (bg-mode :accessor bg-mode :initform :draw-all)))

(defmethod object-has-editor ((self n-cercle)) t)
(defmethod get-editor-class ((self n-cercle)) 'CercleEditor)

(defclass CercleView (omeditorview) ())


;;; UPDATE AFTER EVAL

(defmethod update-to-editor ((self cercleeditor) (from OMBox))
  (let ((obj (object-value self)))
    (when (>= (front self) (length (puntos obj)))
      (setf (front self) 0))
    (om-invalidate-view (main-view self))))

;;; BUILD

(defmethod make-editor-window-contents ((self CercleEditor))
  (let* ((view (om-make-view 'cercleview
                             :editor self
                             :size (omp 200 200)
                             :bg-color (om-def-color :white)))
         (controls 
          (om-make-layout 'om-row-layout
                          :size (omp nil 24)
                          :subviews (list 
                                     (om-make-di 'om-simple-text 
                                                 :text "N="
                                                 :font (om-def-font :font1)
                                                 :size (omp 20 24))
                                     (om-make-view 
                                      'om-view ;;; needed to get the scroll-feature of numbox to work
                                      :size (omp nil 24)
                                      :subviews 
                                      (list (om-make-graphic-object 
                                             'numbox 
                                             :value (n (object-value self))
                                             :bg-color (om-def-color :white)
                                             :position (omp 0 0)
                                             :db-click t
                                             :decimals 0
                                             :size (om-make-point 40 20) 
                                             :font (om-def-font :font2)
                                             :min-val 1 :max-val 120
                                             :after-fun #'(lambda (item)
                                                            (setf (n (object-value self)) (get-value item))
                                                            (report-modifications self)
                                                            (om-invalidate-view view)
                                                            ))
                                            ))
                                     nil 
                                     (om-make-di 'om-simple-text 
                                                 :text "background"
                                                 :font (om-def-font :font1)
                                                 :size (omp 70 24))
                                     (om-make-di 'om-popup-list 
                                                 :items (list :draw-all :light :hide)
                                                 :size (omp 80 24)
                                                 :font (om-def-font :font1)
                                                 :di-action #'(lambda (item)
                                                                (setf (bg-mode self) (om-get-selected-item item))
                                                                (om-invalidate-view view))
                                                 )))))
          
    (values (om-make-layout 'om-column-layout
                            :ratios '(100 1)
                            :subviews (list view controls))
            view)
    ))
    
        
;;; DRAW 

(defmethod om-draw-contents ((self CercleView))
  
  (let* ((ed (editor self))
         (obj (object-value ed))
         (step (/ (* pi 2) (n obj)))
         (cx (round (w self) 2))
         (cy (round (h self) 2))
         (r (round (min (w self) (h self)) 2.5)))
    
    (om-with-line-size 2
      (om-draw-circle cx cy r))
      
    (draw-point-list-in-circle 
     (loop for i from 0 to (- (n obj) 1) collect (* step i))
     cx cy r :thickness 2)
    
    (let ((colorlist (loop for i from 0 to (1- (length (puntos obj)))
                           collect (1+ (mod i 16)))))
      
      ;;; bacground puntos
      (unless (equal (bg-mode ed) :hide) 
        (loop for point-list in (append (subseq (puntos obj) 0 (front ed))
                                        (subseq (puntos obj) (1+ (front ed))))
              for c in (append (subseq colorlist 0 (front ed))
                               (subseq colorlist (1+ (front ed))))
              do 
                (draw-point-list-in-circle 
                 (om* point-list step)
                 cx cy r
                 :thickness (if (equal (bg-mode ed) :light) 1 1) 
                 :color (if (equal (bg-mode ed) :light)
                            (om-make-color-alpha (get-midi-channel-color c) .3)
                          (get-midi-channel-color c))
                 :lines t)
                ))
      
      ;;; front puntos
      (draw-point-list-in-circle 
       (om* (nth (front ed) (puntos obj)) step)
       cx cy r
       :thickness 2
       :color (get-midi-channel-color (nth (front ed) colorlist))
       :lines t)

      (om-draw-string 10 (- (h self) 20)
                      (if (> (length (puntos obj)) 1)
                          (format nil "~D/~D: ~A" (1+ (front ed)) (length (puntos obj)) (nth (front ed) (puntos obj)))
                        (format nil "~A" (nth (front ed) (puntos obj))))
                      :font (om-def-font :font2b)
                      :color (om-def-color :dark-gray))
      
      )))


;;; TRANSFORMATIONS

(defmethod rotate-front-list ((self cercleeditor) n)
  (let ((obj (object-value self)))
    (setf (nth (front self) (puntos obj))
          (sort (loop for p in (nth (front self) (puntos obj)) 
                      collect (mod (+ p n) (n obj)))
                '<))
    (report-modifications self)
    ))

(defmethod inverse-front-list ((self cercleeditor)) 
  (let ((obj (object-value self)))
    (setf (nth (front self) (puntos obj))
          (sort (x->dx (reverse (dx->x 0 (nth (front self) (puntos obj))))) '<))
    (report-modifications self)))


(defmethod complement-front-list ((self cercleeditor)) 
  (let ((obj (object-value self)))
    (setf (nth (front self) (puntos obj))
          (loop for item from 0 to (- (n obj) 1)
                when (not (find item (nth (front self) (puntos obj))
                                :test #'(lambda (a b) (= (mod a (n obj)) (mod b (n obj))))))
                collect item))
    (report-modifications self)))



(defmethod add-remove-point-at ((self cercleview) clic-pos)
  ;;; copied from the drawing method...
  (let* ((ed (editor self))
         (obj (object-value ed))
         (step (/ (* pi 2) (n obj)))
         (points-angles (loop for i from 0 to (- (n obj) 1) collect (* step i)))
         (cx (round (w self) 2))
         (cy (round (h self) 2))
         (r (round (min (w self) (h self)) 2.5))
         (points-xy (loop for theta in points-angles 
                          collect 
                          (multiple-value-bind (x y) 
                              (pol->car r (+ theta (/ pi -2)))
                            (list (+ cx x) (+ cy y)))))
         (delta 5)
         (position-in-list (position-if #'(lambda (p)
                                                 (and (<= (abs (- (om-point-x clic-pos) (car p))) delta)
                                                      (<= (abs (- (om-point-y clic-pos) (cadr p))) delta)))
                                        points-xy)))
    (when position-in-list
      (if (find position-in-list (nth (front ed) (puntos obj)))
          (setf (nth (front ed) (puntos obj))
                (remove position-in-list (nth (front ed) (puntos obj))))
        (setf (nth (front ed) (puntos obj))
              (sort (cons position-in-list (nth (front ed) (puntos obj))) '<)))
      (report-modifications ed)
      (om-invalidate-view self)
      )
    ))


;;; USER ACTION CALLBACKS

(defmethod editor-key-action ((self cercleeditor) key)
     
    (case key

      (:om-key-tab 
       (setf (front self) (mod (1+ (front self)) (length (puntos (object-value self)))))
       (om-invalidate-view (main-view self)))

      (#\r (rotate-front-list self 1)
           (om-invalidate-view (main-view self)))
      
      (#\i (inverse-front-list self)
           (om-invalidate-view (main-view self)))

      (#\c (complement-front-list self)
           (om-invalidate-view (main-view self)))

    (otherwise (om-beep))
    ))
    

(defmethod om-view-click-handler ((self cercleview) position)
  (when (om-add-key-down)
    (add-remove-point-at self position)))




