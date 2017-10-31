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
(in-package :om)


;;; called after a property is changed
(defmethod update-after-prop-edit ((self t) (object t)) nil)


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

(defun update-inspector (object view &optional force)
  (when (and *inspector-window* (om-window-open-p *inspector-window*)
                    (or force 
                        (not (equal object (object *inspector-window*)))))
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
    (let ((inspector-layout 
           (om-make-layout 
            'om-grid-layout
            :delta '(10 0) :align nil
            :subviews (loop for category in (get-properties-list object)
                            when (cdr category)
                            append 
                            (append 
                             (list  ;     (car category)  ; (list (car category) (om-def-font :font1b)) 
                              (om-make-di 'om-simple-text :size (om-make-point 20 20) :text "" :focus t)
                              (om-make-di 'om-simple-text :text (car category) :font (om-def-font :font2b)
                                          :size (om-make-point (+ 10 (om-string-size (car category) (om-def-font :font2b))) 20)
                                          )
                              ;; prevents focus on other items :)  :right-extend
                              )
                             (loop for prop in (cdr category) append
                                   (list (om-make-di 'om-simple-text :text (string (nth 1 prop)) :font (om-def-font :font1)
                                                     :size (om-make-point 110 20) :position (om-make-point 10 16))
                                                                                       ; (nth 1 prop) ; (list (nth 1 prop) (om-def-font :font1))  
                                         (make-prop-item (nth 2 prop) (nth 0 prop) object :default (nth 4 prop) 
                                                         ; :update view
                                                         :update (get-update-frame object)
                                                         )))
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



;;;============================================================================
;;; A VIRTUAL OBJECT TO HANDLE MULTIPLE-SELECTION IN INSPECTOR...
;;;============================================================================

(defclass virtual-object-selection () 
  ((objects :initarg :objects :initform nil :accessor objects)))
   
(defmethod object-name-in-inspector ((self virtual-object-selection)) "[MULTIPLE SELECTION]")

(defmethod get-update-frame ((self virtual-object-selection)) self)

;;; dummy signature
(defmethod update-after-prop-edit ((view virtual-object-selection) (object virtual-object-selection))
  (loop for obj in (objects object) do
        (update-after-prop-edit (get-update-frame obj) obj)))

;;; !! object and view can be lists !!
(defmethod set-inspector-contents (win (object cons) (view list))
  (let ((virtual-obj (make-instance 'virtual-object-selection :objects object)))
    (set-inspector-contents win virtual-obj view)
    ))

;;; returns only the properties shared between all the objects
(defmethod get-properties-list ((self virtual-object-selection))
  (let ((one-list (get-properties-list (car (objects self))))
        (invalid-properties nil))
    (loop for category in one-list do
          (loop for prop in (cdr category) do
                (let ((valid t))
                  (loop for other-object in (cdr (objects self)) 
                        while valid do
                        (unless (valid-property-p other-object (car prop))
                          (push (car prop) invalid-properties)
                          (setf valid nil))
                      ))
                ))
    (hide-properties one-list invalid-properties)))


;;; will return a value only if all the inspected objects have the same
;;; otherwise, will trust the default spec of the property 
(defmethod get-property ((self virtual-object-selection) prop-id &key (warn t)) 
  (let ((val (get-property (car (objects self)) prop-id)))
    (loop for o in (cdr (objects self))
          while val do
          (unless (equal val (get-property o prop-id))
            (setf val nil)))
    val))

;;; set the same value to all objects
(defmethod set-property ((self virtual-object-selection) prop-id val)
  (loop for o in (objects self) do
        (set-property o prop-id val)))






