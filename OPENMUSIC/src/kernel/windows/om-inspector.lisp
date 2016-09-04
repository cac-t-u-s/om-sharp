(in-package :om)


;;; called after a property is changed
(defmethod update-view ((self t) (object t)) nil)


;;;===========================================
;;; SPECIAL FOR EDITOR: MUST ACCESS THE EDITOR PROPS + THE VALUE PROPS
;;;===========================================
#|
(defmethod editor-properties-list ((self OMEditor)) nil)
(defmethod object-properties-list ((self t)) nil)

(defun convert-accessors (editor prop-list)
  (loop for prop in prop-list 
        collect 
        (let ((access-name (intern (string+ "ACCESS-EDITOR-VALUE-" (string (nth 3 prop))))))
          (unless nil ;(fboundp access-name)
            (eval 
             `(defmethod ,access-name ((ed ,(type-of editor)) &optional (val nil val-supplied-p)) 
                (if val-supplied-p
                    (access-value (object-value ed) ',(nth 3 prop) val)
                  (access-value (object-value ed) ',(nth 3 prop))))))
          (list (nth 0 prop) (nth 1 prop) (nth 2 prop) access-name))))
              
(defmethod get-properties-list ((self omeditor))
  (remove nil 
          (list (and (editor-properties-list self) (cons "Editor Parameters" (editor-properties-list self)))
                ;(and (object-properties-list (object-value self)) 
                ;     (cons (string+ "Value (" (string (type-of (object-value self))) ")")
                ;           (convert-accessors self (object-properties-list (object-value self)))))
                )))
|#



;;;===========================================
;;; INSPECTOR WINDOW
;;;===========================================

(defparameter *inspector-window* nil)

(defclass inspector-window (oa::om-windoid) 
  ((object :initarg :object :initform nil :accessor object)))

(defmethod om-window-close-event ((self inspector-window))
  (setf *inspector-window* nil))

(defun hide-inspector ()
  (when *inspector-window*
    (om-close-window *inspector-window*)))

(defun release-inspector (object)
  (when (and *inspector-window* 
             (equal object (object *inspector-window*)))
    (setf (object *inspector-window*) nil)
    (hide-inspector)))

(defun update-inspector (object view)
  (when (and *inspector-window* (om-window-open-p *inspector-window*)
                    (not (equal object (object *inspector-window*))))
    (set-inspector-contents *inspector-window* object view)))

(defmethod object-name-in-inspector ((self OMObject)) (name self))
(defmethod object-name-in-inspector ((self t)) nil)

(defmethod set-inspector-contents (win object view)
  (om-remove-all-subviews win)
  (setf (object win) object)
  (om-set-window-title 
   win 
   (or (and (null object) "-")
       (and (consp object) "[MULTIPLE SELECTION]")
       (object-name-in-inspector object) 
       (string (type-of object)))
   )
  (when (get-properties-list object)
    (let ((inspector-layout (om-make-layout 
                             'om-grid-layout
                             :delta '(10 0)
                             :subviews (loop for category in (get-properties-list object)
                                             append (append 
                                                     (list  ;     (car category)  ; (list (car category) (om-def-font :font1b)) 
                                                      (om-make-di 'om-simple-text :size (om-make-point 20 20) :text "" :focus t)
                                                      (om-make-di  'om-simple-text :text (car category) :font (om-def-font :font2b)
                                                                   :size (om-make-point 200 20)  ;:position (om-make-point 0 0)
                                                                   )
                                                      ;; prevents focus on other items :)  :right-extend
                                                      )
                                                     (loop for prop in (cdr category) append
                                                           (list (om-make-di 'om-simple-text :text (nth 1 prop) :font (om-def-font :font1)
                                                                             :size (om-make-point 110 20) :position (om-make-point 10 16))
                                                                                       ; (nth 1 prop) ; (list (nth 1 prop) (om-def-font :font1))  
                                                                 (make-prop-item (nth 2 prop) (nth 0 prop) object :default (nth 4 prop) :update view)))
                                                     (list (om-make-di 'om-simple-text :size (om-make-point 20 6) :text "" :focus t) 
                                                           (om-make-di 'om-simple-text :size (om-make-point 20 6) :text "" :focus t))
                                                     )
                                             )
                             )))
      
      (om-add-subviews win inspector-layout)
      (om-set-view-size win (om-make-point 300 (om-height inspector-layout)))
      ))
  )

(defun show-inspector (object &optional view)
  (let ((pos (and *inspector-window* (om-view-position *inspector-window*))))
    (unless *inspector-window*
      (setf *inspector-window* (om-make-window 'inspector-window :title "inspector"
                                               :size (om-make-point 300 nil)
                                               ;:resizable :h
                                               :position pos))
      (set-inspector-contents *inspector-window* object view)
      ;;; sets reizable NIL
      ;(om-set-view-size *inspector-window* (om-view-size *inspector-window*))
      (if (om-window-open-p *inspector-window*)    
          (om-show-window *inspector-window*)
        (om-open-window *inspector-window*))
      )))

 

    
(defun close-inspector-for-box (box)
  (when (and *inspector-window* (equal (object *inspector-window*) box))
    (om-close-window *inspector-window*)))


(defmethod get-update-frame ((self t)) nil)

(defun update-inspector-for-box (box)
  (when (and *inspector-window* (equal (object *inspector-window*) box))
    (set-inspector-contents *inspector-window* box (get-update-frame box))))








