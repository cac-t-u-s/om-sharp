;;;===============================================================
;;; INTERFACE BOXES
;;;===============================================================

(in-package :om)

(defvar *interfaceboxes* 
  (omNG-make-package "Interface Boxes"
                     :container-pack *om-package-tree*
                     :doc "This package contains special interface boxes and widgets to use in OM patches (sliders, buttons, etc.)"))


(defclass OMInterfaceBox (OMBox) ())

(defmethod get-properties-list ((self OMInterfaceBox))
  (hide-property (call-next-method) '(:text-font :align :group-id)))

(defmethod create-box-outputs ((self OMInterfaceBox))
  (list (make-instance 'box-output :box self :name "value")))

(defmethod omNG-box-value ((self OMInterfaceBox) &optional (numout 0)) 
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
  (update-inspector-for-box self))

(defmethod eval-box :before ((self OMInterfaceBox))
  (apply-box-attributes self (eval-box-inputs self)))

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
        (draw-border box 0 io-hspace (w self) (- (h self) (* 2 io-hspace)) (border box)))
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
   (orientation :accessor orientation :initarg :orientation :initform :vertical)
   (action :accessor action :initarg :action :initform nil)))

(AddSpecialItem2Pack 'slider *interfaceboxes*)
(defmethod special-box-p ((self (eql 'slider))) t)

(defmethod get-all-keywords ((self SliderBox))
  '((:min-value :max-value :increment :orientation :action :value)))

(defmethod default-value ((self SliderBox)) 50)

(defmethod set-value ((self SliderBox) val)
  (when (numberp (car val)) 
    (call-next-method)))

(defmethod get-properties-list ((self SliderBox))
  (add-properties (call-next-method)
                  "Slider" 
                  `((:min-value "Min value" :number min-value)
                    (:max-value "Max value" :number max-value)
                    (:increment "Increment" :number increment)
                    (:orientation "Orientation" (:vertical :horizontal) orientation)
                    )))

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
                                (round (* ratio (- (max-value self) (min-value self))) 
                                       (increment self)))))
                   (set-value self (list val))
                   (when (reactive (car (outputs self))) 
                     (self-notify self))
                   ;(print val)
                   (om-invalidate-view frame)
                   )))
    ))

      

