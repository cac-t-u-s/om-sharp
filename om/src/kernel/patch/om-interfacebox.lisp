;============================================================================
; o7: visual programming language for computer-aided music composition
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
; INTERFACE BOXES
;=========================================================================

(in-package :om)

(defvar *interfaceboxes* 
  (omNG-make-package "Interface Boxes"
                     :container-pack *om-package-tree*
                     :doc "This package contains special interface boxes and widgets to use in OM patches (sliders, buttons, etc.)"))


(defclass OMInterfaceBox (OMBox) ())
(defmethod lock-state ((self OMInterfaceBox)) nil)

(defmethod get-properties-list ((self OMInterfaceBox))
  (hide-property (call-next-method) '(:text-font :align :group-id)))

(defmethod create-box-outputs ((self OMInterfaceBox))
  (list (make-instance 'box-output :box self :name "value")))

(defmethod omNG-box-value ((self OMInterfaceBox) &optional (numout 0)) 
  (apply-box-attributes self (eval-box-inputs self))
  (current-box-value self numout))

;;; set the box attributes
(defmethod apply-box-attributes ((self OMInterfaceBox) attributes) 
  (loop for attr on attributes by 'cddr do
        (cond ((equal (car attr) :value) 
               (set-value self (list (cadr attr))))
              ((member (car attr) '(:min-value :max-value :increment))
               (when (numberp (cadr attr)) 
                 (setf (slot-value self (intern-om (car attr))) 
                       (cadr attr))))
              (t
               (setf (slot-value self (intern-om (car attr))) 
                     (cadr attr)))))
  (update-inspector-for-object self))

;(defmethod eval-box :before ((self OMInterfaceBox)) 
;  (apply-box-attributes self (eval-box-inputs self)))

(defmethod maximum-size ((self OMInterfaceBox)) nil)
(defmethod minimum-size ((self OMInterfaceBox)) (omp 20 28))

;;; FRAME
(defclass InterfaceBoxFrame (OMBoxFrame) ())
(defmethod get-box-frame-class ((self OMInterfaceBox)) 'InterfaceBoxFrame)

(defmethod om-view-click-handler ((self InterfaceBoxFrame) position)
  (call-next-method)
  (interfacebox-action (object self) self position))

(defmethod interfacebox-action ((self OMInterfaceBox) frame pos) nil)

(defmethod draw-interface-component ((self OMInterfaceBox) x y w h) nil)

(defmethod boxframe-draw-contents ((self InterfaceBoxFrame) (box OMInterfaceBox))
  (let ((io-hspace 4))
    (om-with-fg-color (om-def-color :dark-gray)
      ;;; interior
      (when (box-draw-color box)
        (om-draw-rect 0 io-hspace (w self) (- (h self) (* 2 io-hspace)) 
                      :color (box-draw-color box)
                      :angles :round
                      :fill t))
    
      (when (selected box)
        (om-draw-rect 0  io-hspace (w self) (- (h self) (* 2 io-hspace)) 
                      :color (om-make-color-alpha (om-def-color :gray) 0.3)
                      :angles :round
                      :fill t))
      
      ;;; border
      (when (border box) 
        (draw-border box 0 io-hspace (w self) (- (h self) (* 2 io-hspace))))
      )
  
    (draw-interface-component box 0 io-hspace (w self) (- (h self) (* 2 io-hspace)))
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

(AddSpecialItem2Pack 'slider *interfaceboxes*)
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
    ;(print init-args)
    (set-value box (list (default-value box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))


(defmethod draw-interface-component ((self SliderBox) x y w h) 
  (let* ((val (or (car (value self)) (default-value self)))
         (pos-ratio (/ (- val (min-value self)) (- (max-value self) (min-value self)))))
    (cond ((equal (orientation self) :vertical)
           (om-draw-line x (+ y (* (- 1 pos-ratio) h)) (+ x w) (+ y (* (- 1 pos-ratio) h)) 
                         :color (om-def-color :dark-gray) :line 3))
          ((equal (orientation self) :horizontal)
           (om-draw-line (+ x (* pos-ratio w)) y (+ x (* pos-ratio w)) (+ y h) 
                         :color (om-def-color :dark-gray) :line 3))
          )))


(defmethod interfacebox-action ((self SliderBox) frame pos)
  (when (or (om-command-key-p)
            (container-frames-locked (om-view-container frame)))
    (om-init-temp-graphics-motion 
     frame pos nil
     :motion #'(lambda (view pos)
                 (let* ((ratio (min 1 
                                    (max 0 
                                         (if (equal (orientation self) :vertical)
                                             (- 1 (/ (om-point-y pos) (h frame)))
                                           (/ (om-point-x pos) (w frame))))))
                        (val (+ (min-value self) 
                                (* (increment self)
                                   (round (* ratio (- (max-value self) (min-value self)))
                                          (increment self))))))
                   (set-value self (list (round-decimals val (decimals self))))
                   (when (reactive (car (outputs self))) (self-notify self))
                   (om-invalidate-view frame)
                   )))
    ))


;;;===============================================================
;;; BUTTON
;;;===============================================================

(defclass ButtonBox (OMInterfaceBox)
  ((send-value :accessor send-value :initarg :send-value :initform t)
   (text :accessor text :initarg :text :initform "")
   (action :accessor action :initarg :action :initform nil)))

(AddSpecialItem2Pack 'button *interfaceboxes*)
(defmethod special-box-p ((self (eql 'button))) t)
(defmethod special-item-reference-class ((item (eql 'button))) 'ButtonBox)

(defmethod get-all-keywords ((self ButtonBox))
  '((:send-value :text :action)))

(defmethod get-properties-list ((self ButtonBox))
  (add-properties (call-next-method)
                  "Button" 
                  `((:send-value "Value sent at pushing" t send-value)
                    (:text "Text" :string text)
                    )))

(defmethod default-size ((self ButtonBox)) 
  (omp 28 28))

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
    (if (car (value self))
      (om-draw-rect x y w h :fill t :color (om-def-color :gray))
    (om-draw-rect x y w h :fill nil :line 3 :color (om-def-color :gray)))
    (when (text self)
      (let ((font (om-def-font :font1b)))
        (multiple-value-bind (sw sh) (om-string-size (text self) font)
          (om-with-fg-color textcolor
            (om-with-font 
             font
             (om-draw-string (+ x (/ w 2) (- (/ sw 2)))
                             (+ y (/ h 2) (- (/ sh 2)) 8)
                             (text self)))))))
    ))
 


(defmethod interfacebox-action ((self ButtonBox) frame pos)
  (when (or (om-command-key-p)
            (container-frames-locked (om-view-container frame)))
    (set-value self (list (send-value self)))
    (om-invalidate-view frame)
    (when (reactive (car (outputs self))) (self-notify self))
    (om-init-temp-graphics-motion 
     frame pos nil :min-move nil
     :release #'(lambda (view pos)
                  (set-value self nil)
                  (om-invalidate-view frame)
                  ))))



