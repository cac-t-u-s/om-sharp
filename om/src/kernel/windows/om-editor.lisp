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

;;;====================
;;; EDITOR GENERAL CLASSES
;;;====================

(defclass OMEditor ()
  ((object :initarg :object :initform nil :accessor object)
   (container-editor :initarg :container-editor :initform nil :accessor container-editor)
   (related-editors :initarg :related-editors :initform nil :accessor related-editors)
   (window :initarg :window :initform nil :accessor window)
   (main-view :initarg :main-view :initform nil :accessor main-view)
   (g-components :initarg :g-components :initform nil :accessor g-components)
   (selection :accessor selection :initform nil)))


;;;=============================
;;; EDIT-PARAMS
;;;=============================

(defclass object-with-edit-params () 
  ((edition-params :initform nil :accessor edition-params :initarg :edition-params)))

(defmethod get-edit-param ((self object-with-edit-params) param)
  (if (find param (edition-params self) :key 'car)
      (find-value-in-kv-list (edition-params self) param)
    (get-default-edit-param self param)))

(defmethod get-default-edit-param ((self object-with-edit-params) param) nil)

(defmethod set-edit-param ((self object-with-edit-params) param value)
  (setf (edition-params self)
        (set-value-in-kv-list (edition-params self) param value)))

;;; this is useful to open the editor of something that is not necessarily in a box 
;;; serves as the 'object' of the editor
(defclass OMAbstractContainer (ObjectWithEditor object-with-edit-params)
  ((contents :initarg :contents :initform nil :accessor contents)))

(defmethod get-default-edit-param ((self OMAbstractContainer) param)
  (find-value-in-kv-list (object-default-edition-params (contents self)) param))


(defmethod window ((self OMEditor))
  (or (slot-value self 'window)
      (and (container-editor self) 
           (not (equal self (container-editor self))) 
           ;;; in teh maquette editor, the editor is its own container..
           (window (container-editor self)))))

(defmethod get-value-for-editor ((self t)) self)
(defmethod get-value-for-editor ((self OMAbstractContainer)) (contents self))

(defmethod object-value ((self OMEditor))
  (cond ((object self) (get-value-for-editor (object self)))
        ((container-editor self) (object-value (container-editor self)))
        (t nil)))

;;;=============================
;;; Superclass for OM root editors (patch, maquette, Lispfile, etc.)
(defclass OMDocumentEditor (OMEditor) ())

(defmethod save-command ((self OMDocumentEditor))
  #'(lambda () 
      (let ((doc-to-save (if (is-persistant (object self)) 
                               (object self)
                             (find-persistant-container (object self)))))
        (if doc-to-save
            (progn 
                (save-document doc-to-save)
                (update-window-name (editor doc-to-save)))
            (om-beep-msg "No container document to save !!!"))
          )))

(defmethod save-as-menu-name ((self OMDocumentEditor)) 
  (if (is-persistant (object self)) "Save as..." "Externalize..."))

(defmethod save-as-command ((self OMDocumentEditor))
 (let ((doc (object self)))
   (if (is-persistant doc)
      ;;; rename/resave the doc
      #'(lambda ()
          (let ((sg-pathname (mypathname doc)))
            (setf (mypathname doc) nil)
            (if (save-document doc)   ;; set name is done here in save-document
                (update-window-name self)
              (setf (mypathname doc) sg-pathname)))
          )
      
      ;;; create a persistant patch
      #'(lambda ()
          (change-class doc (externalized-type doc) 
                        :icon (externalized-icon doc))
          (setf (create-info doc) (list (om-get-date) (om-get-date)))
          (register-document doc)
          (save-document doc)   ;; set name is done here in save-document
          (funcall (revert-command self)) ;; to update menus etc.
        )
    )))

(defmethod revert-command ((self OMDocumentEditor))
  (and (is-persistant (object self)) (mypathname (object self))
       #'(lambda () 
           (with-no-check (om-close-window (window self)))
           (open-doc-from-file (object-doctype (object self)) (mypathname (object self)))
           )))

;;; Externalize an internal abstraction

;;;=============================

(defclass OMEditorWindow (om-window) 
  ((editor :initarg :editor :initform nil :accessor editor)
   (side-panel :initarg :side-panel :initform nil :accessor side-panel)))


;;; CLASS LINKS
(defmethod editor-window-class ((self OMEditor)) 'OMEditorWindow)

;;; GET DEFAULT EDITOR VIEW USING THESE METHODS
(defmethod editor-view-class ((window OMEditor)) 'OMDefaultEditorView)
(defmethod editor-view-bg-color ((self OMEditor)) nil)
(defmethod editor-view-scroll-params ((self OMEditor)) nil)
(defmethod editor-view-drawable ((self OMEditor)) nil)

;;; ACCESSORS
(defmethod editor-window ((self ObjectWithEditor)) (and (editor self) (window (editor self))))
(defmethod editor-view ((self ObjectWithEditor)) (main-view (editor self)))
(defmethod editor ((self om-graphic-object)) (editor (om-view-window self)))
(defmethod editor ((self t)) nil)

;;;====================
;;; OBJECT WITH EDITOR
;;;====================

;;; Create and open the editor for an object
(defmethod open-editor ((self t)) nil)
(defmethod open-editor ((self ObjectWithEditor))
  (unless (editor self)
    (setf (editor self) (make-instance (get-editor-class self) :object self))
    (init-editor (editor self)))
  (open-editor-window (editor self)))

;;; Close the editor for the object
(defmethod close-editor ((self ObjectWithEditor))
  (when (editor-window self) (om-close-window (editor-window self)))
  t)

(defmethod close-editor ((self t)) t)


;;; called by the editor to notify a change
;;; to be redefined by the ObjectWithEditor subclasses
;;; <value-changed> specifies if the object value has changed, typically to decide wether or not to invalidate display, lock the box, etc.
;;; you don't wan to set it true, for instance, if this is just a graphical update (e.g. window size)
;;; <reactive> specifies if this change should be a candidate for triggering reactive updates
;;; you don't want to set it true, e.g. with changes that happen too frequently (e.g. results of move-drag actions).  
(defmethod update-from-editor ((self ObjectWithEditor) &key (value-changed t) (reactive t)) nil)
(defmethod update-from-editor ((self t) &key (value-changed t) (reactive t)) nil)


(defmethod window-name-from-object ((self ObjectWithEditor)) (name self))

(defmethod editor-set-edit-param ((self OMEditor) param value)
  (set-edit-param (object self) param value)
  (editor-invalidate-views self))

(defmethod editor-get-edit-param ((self OMEditor) param)
  (get-edit-param (object self) param))

;;;====================
;;; EDITOR
;;;====================

(defmethod get-editor-class ((self t)) 'OMEditor)
(defmethod init-editor ((ed OMEditor)) nil)


(defmethod editor-window-init-size ((self OMEditor)) (om-make-point 500 500))
(defmethod editor-window-init-pos ((self OMEditor)) (om-make-point 500 500))
(defmethod get-object-type-name ((object t)) (string-upcase (type-of object)))
(defmethod get-window-title ((object t)) (get-object-type-name object))

(defmethod update-window-name ((self OMEditor))
  (when (window self)
    ;;; the main window editor is not necessary the one calling
    (om-set-window-title (window self) (editor-window-title (editor (window self))))))

(defmethod editor-window-title ((editor OMEditor))
  (if (container-editor editor)
      (editor-window-title (container-editor editor))
    (string+ (get-name (object editor))
             (if (get-window-title (object editor))
                 (string+ " [" (get-window-title (object editor)) "]")
               "")
             )))

(defmethod editor-window-title ((editor OMDocumentEditor))
  (window-name-from-object (object editor)))

;;; Opens the window for an editor
(defmethod open-editor-window ((self OMEditor))
  (if (and (window self) (om-window-open-p (window self)))
      (om-select-window (window self))
    (let* ((size (or (window-size (object self))
                     (editor-window-init-size self)))
           (win (om-make-window (editor-window-class self)
                                :editor self 
                                :size size
                                :position (window-pos (object self))
                                :title (editor-window-title self)
                                :win-layout 'om-simple-layout 
                                :border 0
                                :menu-items (om-menu-items self))))
      ;;; something strange going on during window creation, resize happening
      ;;; so we need to reset window-size here
      (setf (window-size (object self)) (om-view-size win))
      (setf (window self) win)
      (build-editor-window self)
      (init-editor-window self)
      (om-show-window win)
      (when (get-g-component self :main-panel)
        (om-set-focus (get-g-component self :main-panel)))
      )))

;;; the g-component p-list allows to store and access 
;;; views and dialog items related to the editor
(defmethod set-g-component ((self OMEditor) name comp)
  (setf (getf (g-components self) name) comp))
(defmethod get-g-component ((self OMEditor) name)
  (getf (g-components self) name))


;;; callback called when anything closes the window
;;; called by the close-window event
(defmethod editor-close ((self t)) nil) 

(defmethod editor-close ((self OMEditor)) 
  (call-next-method)
  (setf (editor (object self)) nil))

;;; callback called when the window is brought to front or back
(defmethod editor-activate ((self OMEditor) t-or-nil) 
  (if t-or-nil (update-inspector-for-editor self)
    ;;; (release-inspector self) ;; can remove this
    ))

(defmethod update-inspector-for-editor ((self OMEditor) &optional obj)
  (update-inspector-for-object (or obj (object self))))

;;; called by the window/view to notify a change in the model
(defmethod report-modifications ((self null)) t)
(defmethod report-modifications ((self OMEditor))
  ;;; update the object
  (update-from-editor (object self))
  ;;; update the context (in case of embedded editors) 
  (when (container-editor self)
    (update-to-editor (container-editor self) self))
  (when (related-editors self) 
    (loop for ed in (related-editors self) do
          (update-to-editor ed self)))
  ;;; window title
  (update-window-name self))

;;; called by the object to notify a change to the editor
(defmethod update-to-editor ((self OMEditor) (from t)) 
  ;(print (list "update" self "from" from))
  (update-window-name self))

(defmethod update-default-view ((self OMEditor)) 
  (when (get-g-component self :default-view)
    (update-view-contents (get-g-component self :default-view))))

;;; called by a sub-editor when modified
;(defmethod update-container ((self OMEditor)) nil)

(defmethod editor-invalidate-views ((self OMEditor)) nil)

;;;===============================
;;; EDITORS HANDLE USER ACTIONS

(defmethod allowed-element ((self OMObject) elem) nil)
(defmethod allow-remove ((self OMObject) elem) t)

(defmethod omNG-add-element ((self OMEditor) elem)
  (if (allowed-element (object self) elem)
      (progn
        (omNG-add-element (object self) elem)
        (report-modifications self)
        t)
    (om-beep-msg (format nil "Elements of type ~A not allowed in ~A" (type-of elem) (type-of (object self))))
    ))
      
(defmethod omNG-remove-element ((self OMEditor) elem)
  (if (allow-remove (object self) elem)
      (progn
        (omNG-remove-element (object self) elem)
        (report-modifications self)
        t)
    (om-beep-msg (format nil "Elements of type ~A can not be removed from ~A" (type-of elem) (type-of (object self))))
    ))

;;; GENERAL COMMANDS FOR ALL OM WINDOWS
(defmethod close-command ((self OMEditor))
  #'(lambda () (om-close-window (window self))))


(defmethod editor-key-action ((self OMEditor) key) nil)

(defmethod dispatch-key-action ((self OMEditor) key)
  (editor-key-action self key))



(defun om-add-key-down ()
  #+macosx(om-command-key-p)
  #-macosx(om-control-key-p))

;;;====================
;;; EDITOR VIEW 
;;; the main-view of the editor should be an editor view (?)
;;;====================

(defclass OMEditorView (om-view)
  ((editor :accessor editor :initform nil :initarg :editor)))

;;;====================
;;; EDITOR WINDOW 
;;;====================

(defmethod build-editor-window ((editor OMEditor))
  (let ((win (window editor)))
    (when win 
      (om-remove-all-subviews win)
      (multiple-value-bind (contents main)
          (make-editor-window-contents editor)
        (setf (main-view editor) (or main contents))
        (om-add-subviews win contents)
        ))))

(defmethod init-editor-window ((ed OMEditor)) nil)

;;; called by build-editor-window
;;; this function is given the opportunity to provide what's the main-view of the editor thanks as a second value
(defmethod make-editor-window-contents ((editor OMEditor))
  (make-default-editor-view editor))

;;; can be called in redefinitions of make-editor-window-contents
(defmethod make-default-editor-view ((editor OMEditor))
  (apply 'om-make-view 
         (append (list 
                  (editor-view-class editor)
                  :direct-draw (editor-view-drawable editor)
                  :editor editor
                  :scrollbars (editor-view-scroll-params editor))
                 (and (editor-view-bg-color editor) (list :bg-color (editor-view-bg-color editor))))
         ))

;;; not very clean...
;(defmethod build-editor-window :after (editor) 
;  (when (main-view editor)
;    (om-view-resized (main-view editor) (om-view-size (main-view editor)))))

(defmethod om-window-activate ((self OMEditorWindow) &optional (activatep t))
  (editor-activate (editor self) activatep))

(defmethod om-window-close-event ((self OMEditorWindow))
  (editor-close (editor self))
  (setf (window-size (object (editor self))) (om-view-size self))
  (setf (window (editor self)) nil)
  (setf (g-components (editor self)) nil))

(defmethod om-window-check-before-close ((self OMEditorWindow)) 
  ;(print (list "close" self)) 
  (and (ask-save-before-close (object (editor self)))
       (call-next-method)))

(defmethod om-view-key-handler ((self OMEditorWindow) key)
  ;(print (list "omeditorwindow" self key))
  (dispatch-key-action (editor self) key))

(defmethod om-window-resized ((self OMEditorWindow) size)
  (let ((ed (editor self)))
    (when ed ;;; sometimes the editor is not yet set (e.g. textbuffer editor)
      (let ((obj (object ed)))
        (setf (window-size obj) size)
        (when (om-initialized-p self) ;;; avoids to do this during init calls
          (update-from-editor obj :value-changed nil :reactive nil))
        ))))
  
(defmethod om-window-moved ((self OMEditorWindow) pos)
  (let ((ed (editor self)))
    (when ed ;;; sometimes the editor is not yet set (e.g. textbuffer editor)
      (let ((obj (object ed)))
        (setf (window-pos obj) pos)
        (when (om-initialized-p self) ;;; avoids to do this during init calls
          (update-from-editor obj :value-changed nil :reactive nil))
        ))))


;;;====================
;;; SELECTION TOOLS
;;;====================

(defmethod set-selection ((editor OMEditor) new-selection)
  (cond ((and (om-shift-key-p) new-selection)
         (if (find new-selection (selection editor))
             (setf (selection editor) (remove new-selection (selection editor)))
           (progn 
             (setf (selection editor) (cons new-selection (selection editor)))
             )))
        (t 
         (unless (find new-selection (selection editor)) 
           (setf (selection editor) (and new-selection (list new-selection))))))
  (when (container-editor editor) 
    (update-to-editor (container-editor editor) editor)
    ))

(defmethod set-selection ((editor OMEditor) (new-selection list))
  (cond ((and (om-shift-key-p) new-selection)
         (setf (selection editor) (append new-selection (selection editor))))
        (t (setf (selection editor) new-selection)))
  ;(print (list editor (selection editor) "=>" (container-editor editor)))
  (when (container-editor editor)
    (update-to-editor (container-editor editor) editor)
    ))


;;;====================
;;; DEFAULT INSPECTOR/EDITOR
;;;====================

(defclass OMDefaultEditorView (OMEditorView) ())

(defmethod set-contents ((self OMDefaultEditorView))
  (let* ((ed (editor self))
         (object (object-value ed)))
    (om-add-subviews 
     self 
     (om-make-layout 
      'om-column-layout
      :delta 0
      :subviews (loop for category in (get-properties-list object)
                      append (append 
                              (list  ;     (car category)  ; (list (car category) (om-def-font :font1b)) 
                               ;(om-make-di 'om-simple-text :size (om-make-point 20 26) :text "" :focus t)
                               (om-make-di  'om-simple-text :text (car category) :font (om-def-font :font2b)
                                              :size (om-make-point (list :string (car category)) 20) 
                                              :position (om-make-point 0 6))
                               )
                              (loop for prop in (cdr category) append
                                    (list (om-make-di 'om-simple-text :text (nth 1 prop) :font (om-def-font :font1b)
                                                      :size (om-make-point 120 18)) ; :position (om-make-point 10 16))
                                                                                       ; (nth 1 prop) ; (list (nth 1 prop) (om-def-font :font1))  
                                          (make-prop-item (nth 2 prop) (nth 0 prop) object :default (nth 4 prop) :update self)
                                          (om-make-di 'om-simple-text :size (om-make-point 20 4) :text "" :focus t)))))))))

(defmethod update-view-contents ((self OMDefaultEditorView))
  (apply 'om-remove-subviews (cons self (om-subviews self)))
  (set-contents self))

(defmethod initialize-instance :after ((self OMDefaultEditorView) &rest args)
  (set-g-component (editor self) :default-view self)
  (set-contents self))

;;; called from the property-item
(defmethod update-after-prop-edit ((view OMDefaultEditorView) object)
  (let ((editor (editor view)))
    (update-to-editor editor editor)
    (editor-invalidate-views editor)
    (report-modifications editor)))



#|
(defclass test-class () 
  ((a :initform nil :initarg :a :accessor a)
   (b :initform "qdfqsdf" :initarg :b :accessor b)
   (c :initform nil :initarg :c :accessor c)))
   
(defmethod get-properties-list ((self test-class))
  '(("class attibutes"
     (:a "Color" :color a)
     (:b "Name" :text b)
     (:c "Action" :action c))))
|#






