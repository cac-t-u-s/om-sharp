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

;=======================================================================
; INTERFACE BOXES
;=======================================================================

(in-package :om)


(defclass OMInterfaceBox (OMBox) ())
(defmethod lock-state ((self OMInterfaceBox)) nil)

(defmethod box-symbol ((self OMInterfaceBox)) 'interface)


(defmethod get-properties-list ((self OMInterfaceBox))
  (hide-property (call-next-method) '(:text-font :align :group-id)))

(defmethod get-documentation ((self OMInterfaceBox)) (class-documentation (class-of self)))

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
  (when (selected self) 
    (update-inspector-for-object self))
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

(defmethod get-state ((self OMInterfaceBox)) nil)
(defmethod restore-state ((self OMInterfaceBox) state) nil)

(defmethod omng-save ((self OMInterfaceBox))  
  (append (call-next-method)
          (list (save-value self))
          (when (get-state self) 
            `((:state ,(omng-save (get-state self)))))))

(defmethod additional-slots-to-copy ((from OMInterfaceBox)) '(value))


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
;;; CHECK-BOX
;;;===============================================================
(defclass CheckBoxBox (OMInterfaceBox) ())

(defmethod special-item-reference-class ((item (eql 'check-box))) 'CheckBoxBox)
(defmethod special-box-p ((self (eql 'check-box))) t)

(defmethod omNG-make-special-box ((reference (eql 'check-box)) pos &optional init-args)
  (let* ((box (make-instance 'CheckBoxBox
                             :name "check-box"
                             :reference 'check-box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

(defmethod maximum-size ((self CheckBoxBox)) (om-make-point 40 40))

(defmethod draw-interface-component ((self CheckBoxBox) x y w h) 
  (let* ((line-w 2)
         (border 4)
         (size (- (min w h) 10))
         (x1 (- (/ w 2) (/ size 2)))
         (y1 (+ border (- (/ h 2) (/ size 2))))
         (x2 (+ (/ w 2) (/ size 2)))
         (y2 (+ border (/ h 2) (/ size 2))))
    
    (when (car (value self))
      (om-draw-line x1 y1
                    x2 y2
                    :line line-w
                    :color (om-def-color :dark-gray))
      (om-draw-line x1 y2 
                    x2 y1
                    :line line-w
                    :color (om-def-color :dark-gray))
      )))
   
(defmethod interfacebox-action ((self CheckBoxBox) frame pos)
   (when (or (om-action-key-down)
             (container-frames-locked (om-view-container frame)))
     
     (store-current-state-for-undo (editor (container self)))

     (set-value self (list (not (get-box-value self))))
     (when (reactive (car (outputs self))) (self-notify self))
     (om-invalidate-view frame)))

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
        (when (car (value slider))
          (setf (car (value slider)) (round-decimals (car (value slider)) val)))
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

  (when (or (om-action-key-down)
            (container-frames-locked (om-view-container frame)))

    (flet ((slider-action (view pos)
             (let ((val (sliderbox-pos-value self view pos)))
                   (unless (equal val (get-box-value self))
                     (set-value self (list val))
                     (when (reactive (car (outputs self))) (self-notify self))
                     (om-invalidate-view view)
                     ))))
      
      (store-current-state-for-undo (editor (container self)))

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
   (text :accessor text :initarg :text :initform "")))

(defmethod special-box-p ((self (eql 'button))) t)
(defmethod special-item-reference-class ((item (eql 'button))) 'ButtonBox)

(defmethod get-all-keywords ((self ButtonBox))
  '((:send-value :text)))

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
  (when  (or (om-action-key-down)
             (and (om-view-container frame) 
                  ;;; for some reason sometimes (e.g. while opening the inspector) the container becomes temporarily nil..
                  (container-frames-locked (om-view-container frame))))

     (let ((val-input (find "send-value" (inputs self) :key 'name :test 'string-equal)))
       (when val-input
         (setf (send-value self) (omng-box-value val-input))))
      
    (set-value self (list (send-value self)))
    (om-invalidate-view frame)
    (when (reactive (car (outputs self))) (self-notify self))
    (om-init-temp-graphics-motion 
     frame pos nil :min-move nil
     :release #'(lambda (view pos)
                  (declare (ignore view pos))
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
   (selection :accessor selection :initarg :selection :initform nil)
   (multiple-selection :accessor multiple-selection :initarg :multiple-selection :initform nil)
   (cell-height :accessor cell-height :initarg :cell-height :initform 12)
   (cell-font :accessor cell-font :initarg :cell-height :initform (om-def-font :font1))
   (output-mode :accessor output-mode :initarg :output-mode :initform :value)))
 

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
                    (:output-mode "Output mode" (:value :index) output-mode-accessor)
                    )))



(defmethod update-value-from-selection ((self ListSelectionBox))
  (set-value self 
             (if (multiple-selection self)
                 
                 (if (equal (output-mode self) :value)
                     (list (posn-match (items self) (selection self)))
                   (list (selection self)))
               
               (if (equal (output-mode self) :value)
                     (and (selection self)
                          (list (nth (car (selection self)) (items self))))
                 (list (car (selection self)))
                 )
               )))

(defmethod output-mode-accessor ((self ListSelectionBox) &optional value)
  (when value 
    (setf (output-mode self) value)
    (update-value-from-selection self))
  (output-mode self))

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
             (om-draw-rect 5 (+ yy 2) (- w 10) (cell-height self) :fill t :color (om-def-color :dark-gray)))
           (om-draw-string 5 (+ yy (cell-height self)) (format nil "~A" (nth i (items self)))
                           :color (if (member i (selection self)) (om-def-color :white) (om-def-color :black)))
           )))
   

(defmethod interfacebox-action ((self ListSelectionBox) frame pos)
  (let* ((y (- (om-point-y pos) 4))
         (n (floor y (cell-height self))))
    (when (and (> (om-point-x pos) 5)
               (< (om-point-x pos) (- (w frame) 10))
               (< n (length (items self))))

      (store-current-state-for-undo (editor (container self)))
      
      (if (member n (selection self))
          
          (setf (selection self) (remove n (selection self)))
        
        (setf (selection self) 
              (if (multiple-selection self)
                  (sort (cons n (selection self)) '<)
                (list n))))
      
      (update-value-from-selection self)
      
      (when (reactive (car (outputs self))) (self-notify self))
      (om-invalidate-view frame)
      )))



;;;===============================================================
;;; MENU
;;;===============================================================

(defclass ListMenuBox (OMInterfaceBox)
  ((items :accessor items :initarg :items :initform nil)
   (selection :accessor selection :initarg :selection :initform 0)))
 

(defmethod special-box-p ((self (eql 'list-menu))) t)
(defmethod special-item-reference-class ((item (eql 'list-menu))) 'ListMenuBox)

(defmethod default-size ((self ListMenuBox)) (omp 100 30))
(defmethod maximum-size ((self ListMenuBox)) (omp nil 30))
(defmethod minimum-size ((self ListMenuBox)) (omp 100 30))

(defmethod get-all-keywords ((self ListMenuBox))
  '((:items)))


(defmethod apply-box-attributes ((self ListMenuBox) attributes) 
  (when attributes 
    (let ((newlist (getf attributes :items)))
      (unless (equal newlist (items self))
        (setf (selection self) 0)
        (set-value self nil))))
  (call-next-method))

(defmethod omng-save ((self ListMenuBox))  
  (append (call-next-method)
          `((:items ,(omng-save (items self)))
            (:selection ,(omng-save (selection self))))))

(defmethod load-box-attributes ((box ListMenuBox) data)
  (setf (items box) (omng-load (find-value-in-kv-list data :items)))
  (setf (selection box) (omng-load (find-value-in-kv-list data :selection)))
  box)


(defmethod omNG-make-special-box ((reference (eql 'list-menu)) pos &optional init-args)
  (let* ((box (make-instance 'ListMenuBox
                             :name "list-menu"
                             :reference 'list-menu)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

(defmethod draw-interface-component ((self ListMenuBox) x y w h) 
  (om-draw-rect x y 24 h :color (om-def-color :gray) :fill t)
  (om-with-font 
   (om-def-font :font1b)
   (om-draw-string (+ x 30) (+ y 14) 
                   (format nil "~A"
                           (nth (selection self) (items self))))))

 
(defmethod interfacebox-action ((self ListMenuBox) frame pos)
 
  (when (< (om-point-x pos) 30)
    
    (let ((menu (om-make-menu "list items"                            
               (loop for item in (items self)
                     for i from 0
                     collect (let ((sel i)
                                   (val item))
                               (om-make-menu-item 
                                (format nil "~A" item)
                                #'(lambda () 
                                    (store-current-state-for-undo (editor (container self)))
                                    (setf (selection self) sel)
                                    (set-value self (list val))
                                    (when (reactive (car (outputs self))) (self-notify self))
                                    (om-invalidate-view frame))
                                :selected (= i (selection self))
                                ))))))
    
    (om-open-pop-up-menu menu (om-view-container frame))
    
    )))




;;;===============================================================
;;; SWITCH
;;;===============================================================
(defclass SwitchBox (OMInterfaceBox) 
   ((selection :accessor selection :initarg :selection :initform nil)
    (multiple-selection :accessor multiple-selection :initarg :multiple-selection :initform nil))
   (:documentation "An interface utility to graphically select among different options to pass through.

Click with CMD or when the patch is locked to change the selected input." 
    ))


(defmethod special-item-reference-class ((item (eql 'switch))) 'SwitchBox)
(defmethod special-box-p ((self (eql 'switch))) t)


(defmethod get-state ((self SwitchBox)) (selection self))
(defmethod restore-state ((self SwitchBox) state) (setf (selection self) state))



(defmethod omNG-make-special-box ((reference (eql 'switch)) pos &optional init-args)
  (let* ((box (make-instance 'SwitchBox
                             :name "switch"
                             :reference 'switch)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

(defmethod create-box-inputs ((self SwitchBox)) 
  (list 
   (make-instance 'box-input 
                  :name "opt1"
                  :box self
                  :value NIL
                  :doc-string "option 1")))


(defmethod next-optional-input ((self SwitchBox)) t)

(defmethod more-optional-input ((self SwitchBox) &key name (value nil val-supplied-p) doc reactive)
  (declare (ignore name doc))
  (let ((n (1+ (length (inputs self)))))
    (add-optional-input self :name (format nil "opt~D" n) :value (if val-supplied-p value nil) 
                        :doc (format nil "option ~D" n) 
                        :reactive reactive)
    t))


(defmethod get-properties-list ((self SwitchBox))
  (add-properties (call-next-method)
                  "Switch options" 
                  `((:multiple-selection "Multiple selection" :bool multiple-selection)
                    )))


(defmethod set-property ((self SwitchBox) (attr (eql :multiple-selection)) val) 
  (call-next-method)
  (when (null val)
    (setf (selection self) (list (car (selection self))))))


(defmethod maximum-size ((self SwitchBox)) (om-make-point nil 40))

(defmethod draw-interface-component ((self SwitchBox) x y w h) 
  (let* ((border 4)
         (n (length (inputs self)))
         (cell-w (/ (- w (* 2 border)) n))
         (cell-h (- h (* 2 border))))
    
    (dotimes (i n)
      (om-draw-rect (+ border (* i cell-w)) (+ 4 border) cell-w cell-h
                    :color (om-def-color :dark-gray))
      (when (find i (selection self))
        (om-draw-rect (+ border (* i cell-w)) (+ 4 border) cell-w cell-h 
                      :color (om-def-color :dark-gray) :fill t))
      )
    ))
     
(defmethod interfacebox-action ((self SwitchBox) frame pos)
  
  (when (or (om-action-key-down)
            (container-frames-locked (om-view-container frame)))
    
    (let* ((border 4)
           (n (length (inputs self)))
           (cell-w (/ (- (w frame) (* 2 border)) n))
           (cell-h (- (h frame) (* 2 border)))
           (sel (floor (- (om-point-x pos) border) cell-w)))
     
      (when (and (> (om-point-y pos) (+ 4 border))
                 (< (om-point-y pos) cell-h)
                 (< sel n))
        
        (store-current-state-for-undo (editor (container self)))

        (if (member sel (selection self))
          
            (setf (selection self) (remove sel (selection self)))
      
          (setf (selection self) 
                (if (multiple-selection self)
                    (sort (cons sel (selection self)) '<)
                  (list sel))))
    
        (when (reactive (car (outputs self))) 
          (self-notify self t t)) ;;; evaluate

        (om-invalidate-view frame)
        (when (container self)
          (report-modifications (editor (container self))))
        ))))


(defmethod omNG-box-value ((self SwitchBox) &optional (numout 0)) 
  (let ((vals (loop for sel in (list! (selection self)) collect
                    (when (< sel (length (inputs self)))
                      (omNG-box-value (nth sel (inputs self)))))))
    (set-value self 
               (list 
                (if (multiple-selection self)
                    vals 
                  (car vals))))
    ;;; numout always 0...
    (nth numout (value self))))


(defmethod gen-code ((self SwitchBox) &optional (numout 0))
  (let ((vals (loop for sel in (list! (selection self)) collect
                    (when (< sel (length (inputs self)))
                      (gen-code (nth sel (inputs self)))))))
    (if (multiple-selection self)
        `(list .,vals) 
      (car vals))))


    

