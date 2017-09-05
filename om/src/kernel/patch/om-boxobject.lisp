;;;====================================================
;;; OBJECT/DATA BOXES:  FACTORY, SLOTS, INSTANCE
;;;====================================================

(in-package :om)

;;===========================================
;  ABSTRACT SUPERCLASS
;;===========================================
;;; for boxeditcall ans slotsbox 
(defclass OMBoxRelatedWClass (OMBoxCall) ()
  (:documentation "Boxes with a class as reference")
  (:metaclass omstandardclass))

(defmethod get-icon-id-from-reference ((self OMBoxRelatedWClass)) 'icon-class)


;;; for the moment we do not allow object boxes in lambda mode
(defmethod valid-property-p ((self OMBoxRelatedWClass) (prop-id (eql :lambda))) nil)
(defmethod eval-modes-for-box ((self OMBoxRelatedWClass)) '(nil :reference :box))

(defmethod box-def-self-in ((self t)) NIL)

(defmethod create-box-inputs ((self OMBoxRelatedWClass)) 
  (let ((class (find-class (reference self) nil)))
    (when class 
      (cons 
       (make-instance 'box-input 
                                 :name "SELF" 
                                 :box self :reference :self
                                 :value (box-def-self-in (reference self))
                                 :doc-string "Connect here to create a new instance by copy")
       (mapcar #'(lambda (slot) 
                  (make-instance 'box-input 
                                 :name (string (slot-name slot)) :reference (slot-name slot) 
                                 :box self
                                 :value (valued-val (slot-initform slot))
                                 :doc-string (format nil "[~A] ~S" 
                                                     (slot-type slot) 
                                                     ;;; (class-slot-input-doc class slot)
                                                     (or (slot-doc slot) "")
                                                     )))
              (remove-if-not 'slot-initargs (class-direct-instance-slots class)))   ;;; direct ?
      )
      )))


(defmethod create-box-outputs ((self OMBoxRelatedWClass))
  (let ((class (find-class (reference self) nil)))
    (when class 
      (cons 
       (make-instance 'box-output 
                                 :name "SELF" :reference :self
                                 :box self
                                 :doc-string (format nil "Instance of class ~A" (class-name class)))
       (mapcar #'(lambda (slot) 
                   (make-instance 'box-output 
                                  :name (string (slot-name slot)) 
                                  :reference (slot-name slot)
                                  :box self
                                 :doc-string  (or (slot-doc slot) "")))
               (remove-if-not 'slot-initargs (class-direct-instance-slots class)))    ;;; direct ?
      ))))


;;; ADDITIONAL CLASS AND BOX/EDITOR ATTRIBUTES CAN BE ADDED AS OPTIONAL/KEYWORDS
(defmethod additional-class-attributes ((self t)) nil)

(defmethod allow-more-optionals ((self OMBoxRelatedWClass)) t)

;;; box attributes can be just a name, or 
;;; (name doc menu) 
(defmethod additional-box-attributes ((self t)) nil)
(defmethod box-attributes-names ((attributes list))
  (mapcar #'(lambda (attr) (if (listp attr) (car attr) attr)) attributes))

(defmethod additional-box-attributes-names ((self OMBoxRelatedWClass))
  (box-attributes-names (additional-box-attributes (get-box-value self))))

(defmethod get-all-keywords ((self OMBoxRelatedWClass))
  ;; this function can be called when the value is not yet initialised in the box
  (let ((val (or (and (null (lambda-state self)) (get-box-value self))
                 (make-instance (reference self)))))
    (list (additional-class-attributes val)
          (box-attributes-names (additional-box-attributes val)))
    ))

(defmethod next-keyword-input ((self OMBoxRelatedWClass))
  (let ((keywordlist (apply 'append (get-all-keywords self)))
        (usedkeywords (mapcar #'(lambda (in) (name in)) (get-keyword-inputs self))))
    (if keywordlist 
        (or (find-if-not #'(lambda (elt) (member elt usedkeywords :test 'string-equal)) keywordlist :key 'string)
            (values nil "All keywords are already used.."))
      (values nil (string+ "No keyword for box '" (name self) "'.")))))


(defmethod add-keyword-input ((self OMBoxRelatedWClass) &key key (value nil val-supplied-p) doc reactive)
  (call-next-method)
  (let ((name (string-downcase key)))
    (set-box-outputs self (append (outputs self)
                                 (list (make-instance 'box-keyword-output 
                                                      :name name
                                                      :box self
                                                      :doc-string (get-input-doc self name)))))
    ))


(defmethod update-output-from-new-in ((box OMBoxRelatedWClass) name in) 
  (let ((out (find name (outputs box) :key 'name :test 'string-equal)))
    (when out
      (setf (name out) (name in)
            (doc-string out) (get-input-doc box (name in)))
      )))


(defmethod remove-one-keyword-input ((self OMBoxRelatedWClass))
  (when (call-next-method)
    (set-box-outputs self (butlast (outputs self)))
    ))


;;;-------------------------------------------------------
;;; RELATION TO REFERENCE CLASS (WHEN THE CLASS IS AN OMCLASS)
(defmethod om-copy ((self OMBoxRelatedWClass)) 
  (let* ((newbox (call-next-method))
         (class (find-class (reference newbox) nil)))
    (when (omclass-p class)
      (push newbox (references-to class)))
    newbox))

(defmethod omng-delete ((box OMBoxRelatedWClass))
  (call-next-method)
  (let ((class (find-class (reference box) nil)))
    (when (omclass-p class) 
      (release-reference class box))))


;;;===========================
;;; EVALUATION
;;;===========================

(defmethod objFromObjs ((model t) (target t))
  (clone-object model target))

(defun set-slot-val (obj slot-name value)
  (eval `(setf (,(intern-pack slot-name (symbol-package (type-of obj))) ,obj) ',value)))

(defmethod get-slot-val (obj slot-name)
  (eval `(,(intern-pack slot-name (symbol-package (type-of obj))) ,obj)))

(defun set-value-slots (value args) 
  (mapcar #'(lambda (item) (set-slot-val value (car item) (cadr item))) args))

;;; This is redefined by the graphical initialization
;;; method of visual OM classes
(defmethod v-oop-init ((self t) &rest args) args)

;;; called when all slots are set (including in OM)
;;; or when a new slot is set (e.g. slot box, property, etc.) 
;;; ARGS is NIL for save/load
;;; ARGS = all args in MAKE-VALUE/MAKE-VALUE-FROM-MODEL
;;; ARGS = only the specific arg e.g. in set-property
(defmethod om-init-instance ((self t) &optional args) self)


(defmethod update-after-eval ((self OMBoxRelatedWClass)) nil)
  
(defmethod get-connected-args ((self OMBoxRelatedWClass) &optional (accessor #'omng-box-value))
  (remove nil 
          (mapcar #'(lambda (input) 
                      (when (connections input) 
                        (list (intern-k (name input)) 
                              (funcall accessor input))))
                  (cdr (inputs self)))))

(defmethod get-all-args ((self OMBoxRelatedWClass) &optional (accessor #'omng-box-value))
  (mapcar #'(lambda (input) 
              (list (intern-k (name input)) 
                    (funcall accessor input)))
          (cdr (inputs self))))


(defun make-value (classname args)
  (let* ((class-slots (class-instance-slots (find-class classname)))
         (class-initargs (remove nil (mapcar 'slot-initargs class-slots)))
         (class-slots-names (mapcar 'slot-name class-slots))
         ;;; the regular class initargs
         (supplied-initargs (remove-if #'(lambda (item) (not (find item class-initargs :test 'find))) args :key 'car))
         ;;; not initargs but valid slots
         (supplied-other-args (remove nil (loop for arg in args 
                                                when (not (member (car arg) supplied-initargs :key 'car))
                                                collect (list (find (car arg) class-slots-names :key 'intern-k)
                                                              (cadr arg)))
                                      :key 'car)))
    ;(print args)
    ;(print supplied-initargs)
    ;(print supplied-other-args)    
    (om-init-instance 
     (let ((obj (apply 'make-instance (cons classname (reduce 'append supplied-initargs)))))
       (set-value-slots obj supplied-other-args)
       obj)
     args)))

;;; SPECIAL BOXEDITCALL IF THE FIRST INPUT IS CONNECTED
(defun make-value-from-model (type model args)
  (let* ((target (make-instance type))
         (rep (objFromObjs model target))
         (class-slots-names (mapcar 'slot-name (class-instance-slots (find-class type))))
         (slot-args (remove-if-not #'(lambda (arg) (find (car arg) class-slots-names :key 'intern-k)) args)))
    (if rep
        (progn 
          (set-value-slots rep slot-args)
          (om-init-instance rep args)  ;; ok ?
          )
      (progn (om-beep-msg "Can not create a ~A from ~A" type model)
        (om-abort)))
    ))


  
(defmethod prepare-obj-for-request ((object t) (box OMBoxRelatedWClass)) object)

;;; A REVOIR
(defmethod rep-editor ((box t) num)
  (if (= num 0) (car (value box))
    (cond 
     ;;; GENERAL CASE
     ((null (lambda-state box))
      (let* ((obj (prepare-obj-for-request (get-box-value box) box))
             (slot (intern-pack (name (nth num (outputs box))) (symbol-package (type-of obj)))))
        (cond ((find slot (class-instance-slots (find-class (type-of obj))) :key 'slot-name)
               (get-slot-val (car (value box)) slot))
              ((find (intern-k slot) (additional-box-attributes-names box))
               (get-edit-param box (intern-k slot)))
              (t nil))))
     ;;; LAMBDA
     ((equal (lambda-state box) :lambda)
      (let ((new-arg-list (function-lambda-list (car (value box)))))  
        ; (loop for n from 1 to (length (function-lambda-list (car (value box)))) collect (gensym))))
        (eval `#'(lambda ,new-arg-list 
                   (let ((value (funcall ,(car (value box)) ,.new-arg-list)))
                     (get-slot-val value ,(name (nth num (outputs box)))))))))
     ;;; OTHER CASES
     (t (car (value box))))
    ))


;;===========================================
;; THE MAIN OBJECT BOX (FACTORY)
;;===========================================

(defclass OMBoxEditCall (OMBoxRelatedWClass ObjectWithEditor object-with-edit-params) 
  ((play-state :initform nil :accessor play-state))
  (:metaclass omstandardclass))

(defmethod special-box-type ((self t)) nil)

(defmethod maximum-size ((self OMBoxEditCall)) nil)

(defmethod get-box-class ((self standard-class)) 
  (or (special-box-type (class-name self))
      'OMBoxEditCall))

(defmethod window-title-for-object ((self t)) 
  (get-object-type-name self))

(defmethod get-window-title ((self OMBoxEditCall)) 
  (window-title-for-object (car (value self))))

(defmethod def-reactive ((self OMBoxEditCall) key) 
  (find key (additional-box-attributes-names self)))

;;; called when properties are changed in the inspector
(defmethod om-init-instance ((self omboxeditcall) &optional args)
  (let ((name (find-value-in-kv-list args :name)))
    (when (and name (editor self))
      (report-modifications (editor self)))
    self))

(defmethod omng-delete ((box OMBoxEditCall))
  (call-next-method)
  (when (editor box) 
    (om-close-window (editor-window box))
  (when (editor box) 
    ;;; in principle the window-close callback will have closed the editor and set it to NIL
    ;;; but for instance not if the window is not an OM window (e.g. external app or library...)
    (editor-close (editor box)))
  ))

(defmethod get-box-value ((self OMBoxEditCall)) 
  (car (value self)))

(defmethod get-input-def-value ((self OMBoxEditCall) name)
  (let ((slot (find name (class-instance-slots (find-class (reference self) nil)) :key 'slot-name)))
    (if slot 
        (eval (slot-initform slot))
      ;;; maybe it's the edit-params.. ? 
      (get-default-edit-param self name))))

(defmethod get-input-doc ((self OMBoxEditCall) name)
  (let ((slot (find (intern name (symbol-package (reference self)))
                    (class-instance-slots (find-class (reference self) nil)) :key 'slot-name)))
    (if slot (slot-doc slot)
      ;;; maybe it's the edit-params.. ? 
      (let* ((val (or (and (null (lambda-state self)) (car (value self)))
                      (make-instance (reference self))))
             (pos (position (intern-k name) (additional-box-attributes val) :key 'car)))
        (when pos (nth 1 (nth pos (additional-box-attributes val))))))))


(defmethod class-attributes-menus ((self t)) nil)

(defmethod get-input-menu ((self OMBoxEditCall) name)
  (let* ((val (or (and (null (lambda-state self)) (car (value self)))
                  (make-instance (reference self))))
         (found-in-class-attr (find name (class-attributes-menus val) :key #'(lambda (entry) (string (car entry))) :test 'string-equal))
         (found-in-box-attr (find (intern-k name) (additional-box-attributes val) :key 'car)))
    (or (nth 1 found-in-class-attr)
        (nth 2 found-in-box-attr))))


(defmethod object-has-editor ((self t)) nil)
(defmethod object-has-editor ((self omobject)) t)

(defmethod get-value-for-editor ((self OMBoxEditCall)) (get-box-value self))

(defmethod open-editor ((self OMBoxEditCall))
  (when (object-has-editor (car (value self)))
    (unless (editor self)
      (setf (editor self) (make-instance (get-editor-class (car (value self)))
                                         :object self))
      (init-editor (editor self)))
    (open-editor-window (editor self))))

;;;=============================
;;; EDIT-PARAMS
;;;=============================

(defmethod object-default-edition-params ((self t)) nil)

(defmethod get-default-edit-param ((self OMBoxEditCall) param)
  (let ((val (or (car (value self)) (make-instance (reference self)))))
    (find-value-in-kv-list (object-default-edition-params val) param)))

;;;=============================
;;; FRAME
;;;=============================
(defclass OMObjectBoxFrame (OMBoxFrame) 
  ((box-play-time :initform nil :accessor box-play-time)))

(defmethod get-box-frame-class ((self OMBoxEditCall)) 'OMObjectBoxFrame)
(defmethod default-size ((self OMBoxEditCall)) (om-make-point 80 36))
(defmethod get-box-help ((self OMBoxEditCall)) (format nil "Object of type ~A" (string-upcase (reference self))))

(defmethod box-ed-params-properties ((self t)) nil)
(defmethod editor-ed-params-properties ((self t)) nil)

(defmethod get-properties-list ((self OMBoxEditCall))
  (let ((properties 
         (append 
          (hide-property (call-next-method) '(:icon :align))
          ;'(("Editor/Display" 
          ;  (:lock "Lock state (b/1)" (nil :locked :eval-once) lock-state) ;;; id text type slot
          ;  (:lambda "Eval mode" (nil :lambda :reference :box) lambda-state) ;;; ex. :bool 
          ; (:reactive "Reactive (r)" :bool reactive)))
           )))
    (add-properties properties "Appearance" 
                    (append 
                     `((:name "Name" :text name)
                       (:display "View (m)" ,(display-modes-for-object (car (value self))) display)
                       (:showname "Show name (n)" :bool show-name))
                     (when (play-obj? (car (value self)))
                       '((:show-markers "Show markers" :bool show-markers)))))))


(defmethod display-modes-for-object ((self t)) '(:hidden :text))


(defmethod set-display ((self OMBox) val)
  (setf (display self) val)
  (update-inspector-for-box self)
  (when (frame self) (om-invalidate-view (frame self))))

(defmethod change-display ((self OMBox)) 
  (when (visible-property (get-properties-list self) :display)
    (let ((next-mode (next-in-list (display-modes-for-object (car (value self)))
                                   (display self))))
      (set-display self next-mode))))


;;; redefine when default init value is different
(defmethod initialize-box-initval ((self t)) 
  (set-name self (string-upcase (type-of self)))
  self)

(defmethod omNG-make-new-boxcall ((reference standard-class) pos &optional init-args)
  (let* ((box (make-instance (get-box-class reference)
                             :name (if (stringp init-args) ;;; when typed from the pach editor 
                                       (format nil "~A" init-args)
                                     nil)  ;; (string-upcase (class-name reference))
                             :reference (class-name reference)
                            :icon-pos :noicon :show-name nil
                            :text-font (om-def-font :font1 :style '(:italic))
                            :text-align :right))
         (size (default-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size)        
          (value box) (list (or 
                             (if (not (stringp init-args)) init-args) 
                             ;;; should actually test that this is an instance of reference
                             (initialize-box-initval (make-instance (class-name reference))))))
    box))


(defmethod omNG-make-new-boxcall ((reference OMClass) pos &optional init-args)
  (let ((box (call-next-method)))
    (push box (references-to reference))
    box))

(defmethod om-copy ((self OMBoxEditCall)) 
  (let ((newbox (call-next-method)))
    (setf (value newbox) (om-copy (value self)))
    newbox))

(defmethod om-view-doubleclick-handler ((self OMObjectBoxFrame) pos) 
  (or (apply-in-area self 'click-in-area pos)
      (open-editor (object self))))
    
(defmethod update-view ((self OMBoxFrame) (object OMBoxEditCall))
  (call-next-method)
  (when (editor object) (update-to-editor (editor object) object)))


;;;=======================
;;; DRAW
;;;=======================

(defmethod box-draw ((self OMBoxEditCall) (frame OMBoxFrame))
  (draw-value-in-frame (get-box-value self) frame) 
  t)

;;; to be redefined by objects if they have a specific miniview
(defmethod draw-mini-view ((object t) (box OMBox) x y w h &optional time) nil)

;; the bold text that is written on the object box
(defmethod object-box-label ((object t)) (string-upcase (type-of object)))
(defmethod object-box-label ((object null)) "NIL")

(defmethod draw-label ((box OMBox) object &key color)
  (let ((frame (frame box))
        (str (if (eval-flag box) 
                 ".oO__.." 
               (object-box-label object))))
    (om-with-font 
     (om-def-font :font1 :face "arial" :size 18 :style '(:bold))
     (om-with-fg-color (or color (om-make-color 0.6 0.6 0.6 0.5))
       (om-draw-string 10 (max 22 (+ 6 (/ (h frame) 2))) 
                       str))
     )))


(defmethod draw-mini-text ((object t) (box OMBox) x y w h &optional time)
  ;(om-with-font 
  ; (om-def-font :font1b :size 10)
  ; (om-draw-string (+ x 14) (+ y 14) (draw-type-of-object object)))
  (om-with-font 
   (om-def-font :font1 :size 8)
   (loop for i = (+ y 20) then (+ i 10) 
         for sl in (ensure-cache-display-text box object)
         while (< i (- h 6)) do
         (let ((str (format nil "~A: ~A" (car sl) (cadr sl))))
           (if (> (om-string-size str (om-def-font :font1 :size 8)) (- w 10))
               (om-draw-string (+ x 4) i (concatenate 'string (subseq str 0 (min (length str) (1- (round w 5)))) "..."))
             (om-draw-string (+ x 4) i str))
         ))))

(defmethod draw-type-of-object ((object t))
  (string-upcase (type-of object)))

(defmethod draw-value-in-frame ((object t) (frame OMObjectBoxFrame)) 
  (let ((box (object frame)))
    (case (display box)
      (:text 
       (draw-label box object :color (om-make-color 0.6 0.6 0.6 0.2))
       (draw-mini-text object box 0 0 (w frame) (h frame) (box-play-time frame)))
      (:mini-view 
       (draw-label box object) ; :color (om-make-color 0.6 0.6 0.6 0.2)
       (ensure-cache-display-draw box object)
       (om-with-clip-rect frame  0 4 (w frame) (- (h frame) 8)
         (draw-mini-view object box 0 4 (w frame) (- (h frame) 8) (box-play-time frame))))
      (:hidden 
       (draw-label box object))
      (otherwise nil) 
      )

    (when (play-state box)
      (or (draw-cursor-on-box (get-obj-to-play box) frame (box-play-time frame))
          (om-with-fg-color (om-make-color 0.37 0.73 0.62)
            (om-draw-polygon (mapcan #'(lambda (x y) 
                                         (list (+ x (/ (w frame) 2))
                                               (+ y (/ (h frame) 2))))
                                     '(-5 5 -5)
                                     '(-5 0 5))
                             :fill t))
          )
      )
    ))


(defmethod draw-cursor-on-box (object frame pos)
  (when pos
    (om-with-fg-color (om-make-color 0.73 0.37 0.42)
      (let ((x (* (w frame) 
                  (/ pos (if (plusp (get-obj-dur object)) (get-obj-dur object) 1000)))))
        (om-draw-polygon (list (- x 5) 4 x 9 (+ x 5) 4) :fill t)
        (om-with-line '(2 2)
        (om-draw-line x 4 x (- (h frame) 6))
        )))
    t))

;;; specific update depending on context
(defmethod contextual-update ((self OMBox) (container t)) nil)


(defmethod update-after-eval ((self OMBoxEditCall))
  (when (frame self) 
    (reset-cache-display self)
    (let ((val (car (value self))))
     ;;; if the object has a name, the box takes the object name
     ;;; if not, the object takes the box name...
     (if (get-name val)
         (setf (name self) (get-name val))
       (set-name val (name self))))
    (om-invalidate-view (frame self)))
  (contextual-update self (container self))
  (when (editor self) 
    (update-to-editor (editor self) self)))


(defmethod update-from-editor ((self OMBoxEditCall))
  (setf (lock-state self) :locked)
  (report-modifications (editor (container self)))
  (contextual-update self (container self))
  (when (frame self)
    (update-inspector-for-box (frame self))
    (reset-cache-display self)
    (om-invalidate-view (frame self))))

(defmethod soft-update-from-editor ((self OMBoxEditCall))
  (setf (lock-state self) :locked)
  (report-modifications (editor (container self)))
  (contextual-update self (container self))
  (when (frame self)
    (update-inspector-for-box (frame self))
    (reset-cache-display self)
    (om-invalidate-view (frame self))))

;; e.g. OMABstractContainer
(defmethod soft-update-from-editor ((self t)) nil)

;;===========================================
;; THE SLOTS BOX
;;===========================================
(defclass OMSlotsBox (OMBoxRelatedWClass) ()
  (:metaclass omstandardclass)
  (:default-initargs :border nil))

(defmethod default-size ((self OMSlotsBox))
  (om-make-point 80 28))

(defmethod get-box-help ((self OMSlotsBox)) (format nil "Get/set values for the slots of a ~A" (string-upcase (reference self))))

;;; init-args MUST be a class
(defmethod omNG-make-new-boxcall ((reference (eql 'slots)) pos &optional init-args)
  (make-slots-boxcall init-args pos))

(defmethod make-slots-boxcall ((reference standard-class) pos)
  (let* ((box (make-instance 'OMSlotsBox
                            :name (string+ (string-downcase (class-name reference)) " slots")
                            :reference (class-name reference)
                            :icon-pos :left
                            :text-font (om-def-font :font1 :style '(:italic))
                            :text-align :right))
         (size (default-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))
    box))

(defmethod make-slots-boxcall ((reference OMClass) pos)
  (let ((box (call-next-method)))
    (push box (references-to reference))
    box))

(defmethod make-slots-boxcall ((reference t) pos)
  (om-beep-msg "Error: can not create a SLOTS box from ~A" reference))



;;===========================================
;; THE 'INSTANCE' BOX
;;===========================================

;;; not necessary ?
;;; instances can be contained in existing value/factory box
;;; maybe add an attribuite just to remove the inputs ?
;;; (defclass OMInstanceBox (OMBoxRelatedWClass) ())


(defmethod make-new-box-with-instance ((instance standard-object) pos)
  (let ((box (omng-make-new-boxcall (class-of instance) pos instance)))
    (when box
      (display box) :mini-view)
    box))

(defmethod make-new-box-with-instance ((instance t) pos)
  (omng-make-new-boxcall 'value pos instance))


