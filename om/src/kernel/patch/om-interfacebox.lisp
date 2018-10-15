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

;=======================================================================
; INTERFACE BOXES
;=======================================================================

(in-package :om)


(defclass OMInterfaceBox (OMBox) ())
(defmethod lock-state ((self OMInterfaceBox)) nil)

(defmethod box-symbol ((self OMInterfaceBox)) 'interface)


(defmethod get-properties-list ((self OMInterfaceBox))
  (hide-property (call-next-method) '(:text-font :align :group-id)))

(defmethod create-box-outputs ((self OMInterfaceBox))
  (list (make-instance 'box-output :box self :name "value")))

(defmethod maximum-size ((self OMInterfaceBox)) nil)
(defmethod minimum-size ((self OMInterfaceBox)) (omp 20 28))

;;; set the box attributes
(defmethod apply-one-box-attribute ((self OMInterfaceBox) attr val) 
  (setf (slot-value self (intern-om attr)) val))

(defmethod apply-one-box-attribute ((self OMInterfaceBox) (attr (eql :value)) val) 
  (set-value self (list val)))

(defmethod apply-box-attributes ((self OMInterfaceBox) attributes) 
  (loop for attr on attributes by 'cddr do
        (apply-one-box-attribute self (car attr) (cadr attr)))
  (update-inspector-for-object self)
  )

(defmethod omNG-box-value ((self OMInterfaceBox) &optional (numout 0)) 
  
  ;;; we move out of the eval process to do that! 
  (capi:apply-in-pane-process 
   (om-view-container (frame self)) 
   'apply-box-attributes self (eval-box-inputs self))
  
  (current-box-value self numout))

(defmethod gen-code ((self OMInterfaceBox) &optional (numout 0))
  (current-box-value self numout))

;(defmethod eval-box :before ((self OMInterfaceBox)) 
;  (apply-box-attributes self (eval-box-inputs self)))


(defmethod omng-save ((self OMInterfaceBox))  
  (append (call-next-method)
          (list (save-value self))))

;;; FRAME
(defclass InterfaceBoxFrame (OMBoxFrame) ())
(defmethod get-box-frame-class ((self OMInterfaceBox)) 'InterfaceBoxFrame)

(defmethod om-view-click-handler ((self InterfaceBoxFrame) position)
  ;;; this test avoids doing it when clicked on areas etc.
  (when (equal self (call-next-method))
    (interfacebox-action (object self) self position)
    ))

(defmethod interfacebox-action ((self OMInterfaceBox) frame pos) nil)

(defmethod draw-interface-component ((self OMInterfaceBox) x y w h) nil)

(defmethod boxframe-draw-contents ((self InterfaceBoxFrame) (box OMInterfaceBox))
  (let ((io-hspace 4))
    ;;; interior
    (when (box-draw-color box)
      (om-draw-rounded-rect 0 io-hspace (w self) (- (h self) (* 2 io-hspace)) 
                            :color (box-draw-color box)
                            :round (box-draw-roundness box)
                            :fill t))
    
    (when (selected box)
      (om-draw-rounded-rect 0 io-hspace (w self) (- (h self) (* 2 io-hspace)) 
                            :color (om-make-color-alpha (om-def-color :gray) 0.3)
                            :round (box-draw-roundness box)
                            :fill t))
      
    (draw-interface-component box 0 io-hspace (w self) (- (h self) (* 2 io-hspace)))
      
    ;;; border
    (when (and (box-draw-border box) (plusp (box-draw-border box))) 
      (draw-border box 0 io-hspace (w self) (- (h self) (* 2 io-hspace))))
       
    ;;; in/outs etc.
    (mapcar #'(lambda (a) (om-draw-area a)) (areas self))
    ))


;;;===============================================================
;;; SLIDER
;;;===============================================================

(defclass SliderBox (OMInterfaceBox)
  ((min-value :accessor min-value :initarg :min-value :initform 0)
   (max-value :accessor max-value :initarg :max-value :initform 100)
   (increment :accessor increment :initarg :increment :initform 1)
   (decimals :accessor decimals :initarg :decimals :initform 0)
   (orientation :accessor orientation :initarg :orientation :initform :vertical)
   (action :accessor action :initarg :action :initform nil)))

(defmethod special-item-reference-class ((item (eql 'slider))) 'SliderBox)


(defmethod special-box-p ((self (eql 'slider))) t)

(defmethod get-all-keywords ((self SliderBox))
  '((:min-value :max-value :increment :decimals :orientation :action :value)))

(defmethod default-value ((self SliderBox)) 50)

(defmethod set-value ((self SliderBox) val)
  (when (numberp (car val)) 
    (call-next-method)))

(defmethod get-properties-list ((self SliderBox))
  (add-properties (call-next-method)
                  "Slider" 
                  `((:min-value "Min value" :number min-value
                     (nil nil ,#'decimals))
                    (:max-value "Max value" :number max-value
                     (nil nil ,#'decimals))
                    (:increment "Increment" :number increment 
                     (,#'min-incr ,#'max-value ,#'decimals))
                    (:decimals "Decimals" :number slider-decimals (0 10 0))
                    (:orientation "Orientation" (:vertical :horizontal) orientation)
                    )))


(defmethod apply-one-box-attribute ((self SliderBox) attr val) 
  (if (member attr '(:min-value :max-value :increment))
   (when (numberp val) 
     (setf (slot-value self (intern-om attr)) val))
    (call-next-method)))


(defun min-incr (slider)
  (float (expt 10 (- (decimals slider)))))

(defun round-decimals (val dec)
  (if (zerop dec)
      (round val)
    (float (* (round (* val (expt 10 dec))) (expt 10 (- dec))))))

(defun slider-decimals (slider &optional (val nil val-supplied-p))
  (if val-supplied-p 
      (progn
        (setf (decimals slider) val
              (min-value slider) (round-decimals (min-value slider) val)
              (max-value slider) (round-decimals (max-value slider) val)
              (increment slider) (round-decimals (increment slider) val))
        (setf (car (value slider)) (round-decimals (car (value slider)) val))
        (update-inspector-for-object slider))
    (decimals slider)))
  
(defmethod default-size ((self SliderBox)) 
  (omp 28 100))

(defmethod omNG-make-special-box ((reference (eql 'slider)) pos &optional init-args)
  (let* ((box (make-instance 'SliderBox
                             :name "slider"
                             :reference 'slider)))
    (set-value box (list (default-value box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))


(defmethod draw-interface-component ((self SliderBox) x y w h) 
  (let* ((val (or (car (value self)) (default-value self)))
         (pos-ratio (/ (- val (min-value self)) (- (max-value self) (min-value self)))))
    (cond ((equal (orientation self) :vertical)
           (om-draw-line (+ x (* 2 (box-draw-border self))) (+ y (* (- 1 pos-ratio) h)) (- (+ x w) (* (box-draw-border self) 2)) (+ y (* (- 1 pos-ratio) h)) 
                         :color (om-def-color :dark-gray) :line 3))
          ((equal (orientation self) :horizontal)
           (om-draw-line (+ x (* pos-ratio w)) (+ y (box-draw-border self)) (+ x (* pos-ratio w)) (- (+ y h) (box-draw-border self))
                         :color (om-def-color :dark-gray) :line 3))
          )))


(defun sliderbox-pos-value (sbox frame pos)
  (let ((ratio (min 1 
                    (max 0 
                         (if (equal (orientation sbox) :vertical)
                             (- 1 (/ (om-point-y pos) (h frame)))
                           (/ (om-point-x pos) (w frame)))))))
    (round-decimals 
     (+ (min-value sbox) 
        (* (increment sbox)
           (round (* ratio (- (max-value sbox) (min-value sbox)))
                  (increment sbox))))
     (decimals sbox))))
  
    
(defmethod interfacebox-action ((self SliderBox) frame pos)

  (when (or (om-command-key-p)
            (container-frames-locked (om-view-container frame)))

    (flet ((slider-action (view pos)
             (let ((val (sliderbox-pos-value self view pos)))
                   (unless (equal val (get-box-value self))
                     (set-value self (list val))
                     (when (reactive (car (outputs self))) (self-notify self))
                     (om-invalidate-view view)
                     ))))
      
      ;;; action for the click
      (slider-action frame pos)
      ;;; more if drag
      (om-init-temp-graphics-motion frame pos nil :motion #'slider-action)
      )))


;;;===============================================================
;;; BUTTON
;;;===============================================================

(defclass ButtonBox (OMInterfaceBox)
  ((send-value :accessor send-value :initarg :send-value :initform t)
   (text :accessor text :initarg :text :initform "")
   (action :accessor action :initarg :action :initform nil)))

(defmethod special-box-p ((self (eql 'button))) t)
(defmethod special-item-reference-class ((item (eql 'button))) 'ButtonBox)

(defmethod get-all-keywords ((self ButtonBox))
  '((:send-value :text :action)))

(defmethod get-properties-list ((self ButtonBox))
  (add-properties (call-next-method)
                  "Button" 
                  `((:send-value "Value sent" NIL send-value)
                    (:text "Text" :string text)
                    )))

(defmethod default-size ((self ButtonBox)) (omp 36 36))

(defmethod omNG-make-special-box ((reference (eql 'button)) pos &optional init-args)
  (let* ((box (make-instance 'ButtonBox
                             :name "button"
                             :reference 'button)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

(defmethod draw-interface-component ((self ButtonBox) x y w h) 
  (let ((textcolor (if (car (value self)) 
                       (om-def-color :light-gray) 
                     (om-def-color :dark-gray))))
    (when (car (value self))
      (om-draw-rounded-rect x y w h :fill t :round (box-draw-roundness self) :color (om-def-color :gray)))
    (when (text self)
      (let ((font (om-def-font :font1b)))
        (multiple-value-bind (sw sh) (om-string-size (text self) font)
          (om-with-fg-color textcolor
            (om-with-font 
             font
             (om-draw-string (+ x (/ w 2) (- (/ sw 2)))
                             (+ y (/ h 2) (- (/ sh 2)) 10)
                             (text self)))))))
    ))
 

(defmethod interfacebox-action ((self ButtonBox) frame pos)
  (when  (or (om-command-key-p)
             (and (om-view-container frame) 
                  ;;; for some reason sometimes (e.g. while opening the inspector) the container becomes temporarily nil..
                  (container-frames-locked (om-view-container frame))))
    (set-value self (list (send-value self)))
    (om-invalidate-view frame)
    (when (reactive (car (outputs self))) (self-notify self))
    (om-init-temp-graphics-motion 
     frame pos nil :min-move nil
     :release #'(lambda (view pos)
                  (set-value self nil)
                  (om-invalidate-view frame)
                  ))))


(defmethod OMR-Notify ((self ButtonBox) &optional input-name)
  (cond 
   ((and input-name (string-equal input-name "send-value"))
    (unless (push-tag self)
      (setf (push-tag self) t)
      (let ((listeners (get-listeners self)))
        (if listeners
            (progn 
              (set-value self (list (send-value self)))
              (om-invalidate-view (frame self))
              (loop for listener in listeners do (omr-notify (car listener) (cadr listener)))
              (set-value self nil)
            )
          (omng-box-value self)))))
   (t (call-next-method))
   ))


(defmethod gen-code ((self ButtonBox) &optional (numout 0))
  (let ((val-input (find "send-value" (inputs self) :key 'name :test 'string-equal)))
    (if val-input
        (gen-code val-input)
      (current-box-value self numout))))



;;;===============================================================
;;; LIST
;;;===============================================================

(defclass ListSelectionBox (OMInterfaceBox)
  ((items :accessor items :initarg :items :initform nil)
   (selection :accessor selection :initform nil)
   (multiple-selection :accessor multiple-selection :initform nil)
   (cell-height :accessor cell-height :initform 12)
   (cell-font :accessor cell-font :initform (om-def-font :font1))))
 

(defmethod special-box-p ((self (eql 'list-selection))) t)
(defmethod special-item-reference-class ((item (eql 'list-selection))) 'ListSelectionBox)

(defmethod default-size ((self ListSelectionBox)) (omp 60 60))

(defmethod get-all-keywords ((self ListSelectionBox))
  '((:items)))

(defmethod get-properties-list ((self ListSelectionBox))
  (add-properties (call-next-method)
                  "List selection display" 
                  `((:multiple-selection "Multiple selection" :bool multiple-selection)
                    (:cell-height "Cell size (px)" :number cell-height)
                    (:cell-font "Cell font" :font cell-font)
                    )))

(defmethod apply-box-attributes ((self ListSelectionBox) attributes) 
  (when attributes 
    (let ((newlist (getf attributes :items)))
      (unless (equal newlist (items self))
        (setf (selection self) nil)
        (set-value self nil))))
  (call-next-method))


(defmethod omng-save ((self ListSelectionBox))  
  (append (call-next-method)
          `((:items ,(omng-save (items self)))
            (:selection ,(omng-save (selection self))))))

(defmethod load-box-attributes ((box ListSelectionBox) data)
  (setf (items box) (omng-load (find-value-in-kv-list data :items)))
  (setf (selection box) (omng-load (find-value-in-kv-list data :selection)))
  box)


(defmethod omNG-make-special-box ((reference (eql 'list-selection)) pos &optional init-args)
  (let* ((box (make-instance 'ListSelectionBox
                             :name "list-selection"
                             :reference 'list-selection)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

(defmethod draw-interface-component ((self ListSelectionBox) x y w h) 
  (om-with-font (cell-font self)
     (loop for i = 0 then (+ i 1) 
           for yy = y then (+ yy (cell-height self))
           while (< (+ yy (cell-height self)) h)
           while (< i (length (items self)))
           do
           (when (member i (selection self))
             (om-draw-rect 10 (+ yy 2) (- w 20) (cell-height self) :fill t :color (om-def-color :dark-gray)))
           (om-draw-string 10 (+ yy (cell-height self)) (format nil "~A" (nth i (items self)))
                           :color (if (member i (selection self)) (om-def-color :white) (om-def-color :black)))
           )))
        

(defmethod interfacebox-action ((self ListSelectionBox) frame pos)
  (let* ((y (- (om-point-y pos) 4))
         (n (floor y (cell-height self))))
    (when (and (> (om-point-x pos) 10)
               (< (om-point-x pos) (- (w frame) 20))
               (< n (length (items self))))

    (if (member n (selection self))

        (setf (selection self) (remove n (selection self)))
      
      (setf (selection self) 
            (if (multiple-selection self)
                (sort (cons n (selection self)) '<)
              (list n))))
    
    (set-value self 
               (if (multiple-selection self)
                   (list (posn-match (items self) (selection self)))
                 (and (selection self)
                      (list (nth (car (selection self)) (items self)))))
               )

    (when (reactive (car (outputs self))) (self-notify self))
    (om-invalidate-view frame)
    )))

