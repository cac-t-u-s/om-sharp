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

;;;=============================
;;; EDITOR
;;;=============================

(defclass patch-editor (OMDocumentEditor undoable-editor-mixin) 
  ((editor-window-config :accessor editor-window-config :initarg :editor-window-config :initform nil)))

(defmethod object-has-editor ((self OMPatch)) t)
(defmethod get-editor-class ((self OMPatch)) 'patch-editor)

(defmethod edit-lock ((self patch-editor))
  (let ((ed (or (container-editor self) self))) ;; in case its an internal patch (e.g. in the maquette)
    (lock (object ed))))



(defclass patch-editor-window (OMEditorWindow) ())
(defclass patch-editor-view (OMEditorView om-drop-view om-tt-view multi-view-editor-view) 
  ((dragged-views :initform nil :accessor dragged-views)))

(defmethod editor-window-class ((self patch-editor)) 'patch-editor-window)
(defmethod editor-view-class ((self patch-editor)) 'patch-editor-view)
(defmethod editor-view-bg-color ((self patch-editor)) (om-def-color :white))
(defmethod editor-view-scroll-params ((self patch-editor)) t)
(defmethod editor-view-drawable ((self patch-editor)) t)
(defmethod editor-window-init-size ((self patch-editor)) (omp 500 500))


(defmethod init-editor ((ed patch-editor))
  (unless (loaded? (object ed))
    (load-contents (object ed)))
  (call-next-method))
 
;;; the default location of the editor view
;;; can change, e.g. for a maquette-editor
(defmethod get-editor-view-for-action ((self patch-editor)) (main-view self))

;; will fall here for a maquette editor (main-view is not a patch-editor-view)
(defmethod put-patch-boxes-in-editor-view ((self OMPatch) (view t)) nil)
 

(defmethod put-patch-boxes-in-editor-view ((self OMPatch) (view patch-editor-view)) 
  (mapc 
   #'(lambda (box) 
       (omg-add-element view (make-frame-from-callobj box)))
   (boxes self))
  (mapc
   #'(lambda (c) (add-connection-in-view view c)) 
   (connections self))
  )


  
(defmethod init-editor-window ((editor patch-editor))
  (call-next-method)
  (put-patch-boxes-in-editor-view (object editor) (main-view editor)) 
  (update-inspector-for-editor editor)
  (add-lock-item editor (main-view editor))
  (update-window-name editor))


(defun draw-h-grid-line (view y) 
  (om-draw-line 0 y (w view) y))

(defun draw-v-grid-line (view x) 
  (om-draw-line x 0 x (h view)))

(defmethod draw-patch-grid ((self patch-editor-view) &optional (d 50))
  (om-with-fg-color (om-make-color .95 .95 .95)
    ;(om-with-line '(2 2) ;;; dash-lines are VERY unefficient
      (loop for i from d to (w self) by d do
            (draw-v-grid-line self i))
      (loop for i from d to (h self) by d do
            (draw-h-grid-line self i))
    ;  )
    ))


(defmethod om-draw-contents ((self patch-editor-view))
  (let ((editor (editor (om-view-window self))))
    (when (grid (object editor)) (draw-patch-grid self (grid (object editor))))
    (mapcar 'om-draw-contents (get-grap-connections self))))

(defmethod window-name-from-object ((self OMPatchInternal))
  (format nil "~A  [~A]" (name self) "internal patch"))


;(defmethod init-editor ((self patch-editor))
;  (setf (saved? (object self)) t))

;;;==========================
;;; LOCK
;;;==========================

(defclass lock-view-area (om-item-view) 
  ((editor :accessor editor :initarg :editor :initform nil)))

(defmethod om-draw-contents ((self lock-view-area))
  (om-draw-picture (if (lock (object (editor self))) :lock :unlock) 
                   :x 0 :y 0 :w (om-width self) :h (om-height self)))

(defmethod om-view-click-handler ((self lock-view-area) position)
  (declare (ignore position))
  (setf (lock (object (editor self))) (not (lock (object (editor self)))))
  (om-invalidate-view self)
  (om-invalidate-view (main-view (editor self))))

(defmethod add-lock-item ((editor patch-editor) view)
  (om-add-subviews 
   view 
   (om-make-graphic-object 'lock-view-area 
                           :position (omp 0 2)
                           :size (omp 20 20)
                           :editor editor))
  )

;;;==========================
;;; MENU
;;;==========================



(defmethod om-menu-items ((self patch-editor))
  (remove nil
            (list 
             (main-app-menu-item)
             (om-make-menu "File" (append 
                                   (default-file-menu-items self)
                                   (list (om-make-menu-item "Open as text"
                                                            #'(lambda () 
                                                                (om-lisp::om-open-text-editor :contents (pathname (mypathname (object self))) :lisp t))
                                                            :enabled (and (is-persistant (object self))
                                                                          (mypathname (object self)))))))
             (om-make-menu "Edit" 
                           (append 
                            (default-edit-menu-items self)
                            (list 
                             ;(om-make-menu-comp 
                             ;      (list (om-make-menu-item  
                             ;             "Auto align boxes..."
                             ;             #'(lambda () 
                             ;                 (store-current-state-for-undo self)
                             ;                 (align-selected-boxes self))
                             ;             :key "a" :key-mod nil 
                             ;             :enabled #'(lambda () (not (edit-lock self)))
                             ;             )))
                             (om-make-menu-comp 
                                   (list (om-make-menu-item  
                                          "Show Lisp code"
                                          #'(lambda () (patch-editor-set-window-config 
                                                        self 
                                                        (if (equal (editor-window-config self) :lisp-code) nil :lisp-code)))
                                          :key "l" :selected #'(lambda () (equal (editor-window-config self) :lisp-code))
                                          )
                                         
                                         (om-make-menu-item 
                                          "Show Inspector" 
                                          #'(lambda () (patch-editor-set-window-config 
                                                        self 
                                                        (if (equal (editor-window-config self) :inspector) nil :inspector))) 
                                          :key "i" :selected #'(lambda () (equal (editor-window-config self) :inspector))
                                          )

                                         (om-make-menu-item  
                                          "Show Listener Output"
                                          #'(lambda () (patch-editor-set-window-config 
                                                        self 
                                                        (if (equal (editor-window-config self) :listener) nil :listener)))
                                          :key "m" :selected #'(lambda () (equal (editor-window-config self) :listener))
                                          )

                                         (om-make-menu-item 
                                          "Show Grid" 
                                          #'(lambda () 
                                              (setf (grid (object self)) (if (grid (object self)) nil 50))
                                              (om-invalidate-view (main-view self)))
                                          :key "g" :selected #'(lambda () (grid (object self)))
                                          )
                                         
                                         (om-make-menu-item  
                                          "Edit lock" 
                                          #'(lambda () (setf (lock (object self)) (not (lock (object self))))
                                              (om-invalidate-view (main-view self)))
                                          :key "e" :selected #'(lambda () (lock (object self)))
                                          ))
                                   :selection t)

                             (om-make-menu-comp 
                              (list (om-make-menu-item  
                                     "Abort Evaluation"
                                         #'(lambda () (om-abort))
                                         :key "A"
                                         :enabled #'(lambda () (not (edit-lock self)))
                                          )))
                            
                                  ))
                           )
             (om-make-menu "Windows" (default-windows-menu-items self))
             (om-make-menu "Help" (default-help-menu-items self))
             )))

;;; will be passed to the Help menus
(defmethod get-selection-for-menu ((self patch-editor))
  (remove-duplicates 
   (mapcar 
    #'(lambda (b) (box-symbol (reference b)))
    (get-selected-boxes self))
   ))

(defmethod box-symbol ((self t)) self)

(defmethod get-boxframes ((self patch-editor-view) &key only-selected)
  (let ((frames (remove-if-not #'(lambda (item) (subtypep (type-of item) 'omframe)) (om-subviews self))))
    (if only-selected 
        (remove-if-not 'selected frames :key 'object)
      frames)))

(defmethod get-grap-connections ((self patch-editor-view) &key only-selected)
  (if only-selected
      (remove nil (mapcar 'graphic-connection (remove-if-not 'selected (connections (object (editor self))))))
    (remove nil (mapcar 'graphic-connection (connections (object (editor self)))))))

;;;=========================
;;; EVENT HANDLERS
;;;=========================

(defmethod get-internal-elements ((self OMPatch))
  (append (boxes self) (connections self)))

(defmethod close-internal-elements ((self OMPatch))
  (let ((not-closed-elements 
         (loop for element in (get-internal-elements self)
               when (null (close-internal-element element))
               collect element)))
    (when not-closed-elements
      (om-print (format nil "~%The following elements were not closed:~{~%~A~}~%------" not-closed-elements) "ERROR"))
    t))

(defmethod get-selected-boxes ((self patch-editor))
  (let ((boxes (boxes (object self))))
    (remove-if-not 'selected boxes)))

(defmethod get-selected-connections ((self patch-editor))
  (let ((connections (connections (object self))))
    (remove-if-not 'selected connections)))

(defmethod editor-close ((self patch-editor))
  (call-next-method)
  (close-internal-elements (object self))
  (unless (references-to (object self))
    (unregister-document (object self)))
  )

(defmethod close-internal-element ((self t)) t)

(defmethod close-internal-element ((self ObjectWithEditor)) 
  (close-editor self)
  (call-next-method)
  t)

(defmethod close-internal-element ((self OMBox)) 
  (call-next-method)
  (omng-delete self)
  (setf (frame self) nil)
  t)

(defmethod om-view-doubleclick-handler ((self patch-editor-view) pos) 
  (unless (or (om-point-in-rect-p pos 0 0 20 20)
              (edit-lock (editor self)))
    (enter-new-box self (om-add-points pos (om-make-point -8 -14)))))


(defmethod om-view-click-handler ((self patch-editor-view) position)
  ;;; special : click on the lock-button
  (unless (om-shift-key-p)
    (select-unselect-all (editor self) nil))
  (or (click-connection-handle self position)
      (progn
        (when (om-get-clipboard) (set-paste-position position self))
        (mouse-selection self position))))

;;;handles the selction/drag, etc. of connections
(defmethod click-connection-handle ((self patch-editor-view) pos)
  
  (let ((selected-connection (find-if #'(lambda (c) 
                                          (point-in-connection pos c))
                                      (get-grap-connections self)))
        (p0 pos))
    (when selected-connection
      (setf (view selected-connection) self)
      (select-box (object selected-connection) 
                  (if (om-shift-key-p) (not (selected (object selected-connection))) t))

      (unless (edit-lock (editor self))  
        (store-current-state-for-undo (editor self))
        (om-init-temp-graphics-motion  
         self pos nil
         :motion #'(lambda (view p)
                           
                     (drag-connection (object selected-connection) 
                                      (- (om-point-x p) (om-point-x p0))
                                      (- (om-point-y p) (om-point-y p0)))
                     (om-invalidate-view self)
                     (setf p0 p)
                     )
         :min-move 4)
        )
      t)
    ))
  
(defmethod editor-key-action ((editor patch-editor) key)

  (let* ((panel (get-editor-view-for-action editor))
         (patch (object editor))
         (selected-boxes (get-selected-boxes editor))
         (selected-connections (get-selected-connections editor)))
    
    (when panel

      (case key

        (:om-key-delete (unless (edit-lock editor) 
                          (store-current-state-for-undo editor)
                          (remove-selection editor)))

        (#\n (if selected-boxes
                 (mapc 'set-show-name selected-boxes)
               (unless (edit-lock editor)
                 (make-new-box panel))))
        
        (#\p (unless (edit-lock editor)
               (make-new-abstraction-box panel)))

        (#\i (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'initialize-size (or selected-boxes selected-connections))))
        
        (#\I (mapc 'initialize-box-value selected-boxes))
        
        (:om-key-left (unless (edit-lock editor)
                        (if (om-option-key-p) 
                            (when selected-boxes
                              (store-current-state-for-undo editor)
                              (mapc 'optional-input-- selected-boxes))
                          (let ((selection (or selected-boxes selected-connections)))
                            (store-current-state-for-undo editor :action :move :item selection)
                            (mapc 
                             #'(lambda (f) (move-box f (if (om-shift-key-p) -10 -1) 0)) 
                             selection))
                          )))
        (:om-key-right (unless (edit-lock editor)
                         (if (om-option-key-p) 
                             (when selected-boxes
                               (store-current-state-for-undo editor)
                               (mapc 'optional-input++ selected-boxes))
                            (let ((selection (or selected-boxes selected-connections)))
                              (store-current-state-for-undo editor :action :move :item selection)
                              (mapc #'(lambda (f) (move-box f (if (om-shift-key-p) 10 1) 0)) 
                                    selection))
                         )))
        (:om-key-up (unless (edit-lock editor)
                      (store-current-state-for-undo editor :action :move :item (or selected-boxes selected-connections))
                      (mapc #'(lambda (f) (move-box f 0 (if (om-shift-key-p) -10 -1))) 
                            (or selected-boxes selected-connections))
                      ))
        (:om-key-down (unless (edit-lock editor)
                        (store-current-state-for-undo editor :action :move :item (or selected-boxes selected-connections))
                        (mapc #'(lambda (f) (move-box f 0 (if (om-shift-key-p) 10 1))) 
                              (or selected-boxes selected-connections))
                        ))
      
        (#\k (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'keyword-input++ selected-boxes))))
        (#\+ (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'keyword-input++ selected-boxes))))
        (#\K (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'keyword-input-- selected-boxes))))
        (#\- (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'keyword-input-- selected-boxes))))
        (#\> (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'optional-input++ selected-boxes))))
        (#\< (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'optional-input-- selected-boxes))))
    
        (#\b (when selected-boxes
               (store-current-state-for-undo editor) 
               (mapc 'switch-lock-mode selected-boxes)))

        (#\1 (unless (or (edit-lock editor) (get-pref-value :general :auto-ev-once-mode))  
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'switch-evonce-mode selected-boxes))))

        (#\l (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'switch-lambda-mode selected-boxes))))
             
        (#\m (mapc 'change-display selected-boxes))

        (#\c (unless (edit-lock editor)
               (store-current-state-for-undo editor)
               (if selected-boxes
                   (auto-connect-box selected-boxes editor panel)
                 (make-new-comment panel))))
        (#\C (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (auto-connect-seq selected-boxes editor panel)))
        
        ;;; => Edit menu command ?
        (#\A (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (align-selected-boxes editor)))

        ;;; make a menu command ?
        (#\a (unless (edit-lock editor) 
               (when selected-boxes
                 (store-current-state-for-undo editor)
                 (mapc 'internalize-abstraction selected-boxes))))
        
        ;;; make a menu command ?
        (#\E (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (encapsulate-patchboxes editor panel selected-boxes)))

        ;;; make a menu command ?
        (#\U (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (unencapsulate-patchboxes editor panel selected-boxes)))

        (#\v (eval-command panel selected-boxes))
    
        (#\r (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'set-reactive-mode (or selected-boxes selected-connections))))
      
        ;;; play/stop commands
        ;(#\p (play-boxes selected-boxes))
        (#\s (stop-boxes selected-boxes))   
        (#\Space (play/stop-boxes selected-boxes))
      
        (#\w (om-debug))

        (otherwise nil))
      )))



;; redefine to do something :)
(defun om-debug () (om-beep))
;(defun om-debug () (format *standard-output* "~%===================================") (pprint *open-documents*))

;;;=============================
;;; BASIC ACTIONS
;;;=============================

(defmethod select-unselect-all ((self patch-editor) val)
  (mapcar #'(lambda (x) (select-box x val))
          (append (boxes (object self))
                  (connections (object self))))
  (om-invalidate-view (main-view self)))


(defmethod remove-boxes ((self patch-editor) boxes)
  (mapc #'(lambda (box) 
            (mapcar #'(lambda (c) (omng-remove-element self c)) (get-box-connections box))
            (omng-remove-element self box)
            (omng-delete box) ;;; will om-remove-subviews
            )
        boxes))

(defmethod remove-selection ((self patch-editor))
  (let ((selectedboxes (get-selected-boxes self))
        (selectedconnections (get-selected-connections self))
        (view (main-view self)))
    (when (or selectedboxes selectedconnections)
      
      (remove-boxes self selectedboxes)
      
      (mapc 
       #'(lambda (c) (omng-remove-element self c)) 
       selectedconnections)
      
      (report-modifications self)
      (when view (om-invalidate-view view)))
    ))

(defmethod align-selected-boxes ((editor patch-editor))
  (let ((selected-boxes (get-selected-boxes editor)))
    (if selected-boxes
        (progn 
          (store-current-state-for-undo editor)
          (mapc 'align-box selected-boxes))
      (om-beep))
    ))
    
;;;=============================
;;; MAKE BOXES
;;;=============================

(defmethod make-new-box ((self patch-editor-view))
  (let ((mp (om-mouse-position self)))
    (enter-new-box self (if (om-point-in-rect-p mp 0 0 (w self) (h self))
                            mp (om-make-point (round (w self) 2) (round (h self) 2))))
    ))

(defmethod make-new-abstraction-box ((self patch-editor-view))
  (let ((mp (om-mouse-position self)))
    (enter-new-box self (if (om-point-in-rect-p mp 0 0 (w self) (h self))
                            mp (om-make-point (round (w self) 2) (round (h self) 2)))
                   :patch)
    ))

(defmethod make-new-comment ((self patch-editor-view))
  (let ((mp (om-mouse-position self)))
    (enter-new-comment self (if (om-point-in-rect-p mp 0 0 (w self) (h self))
                            mp (om-make-point (round (w self) 2) (round (h self) 2))))
    ))


;;;==========================
;;; SELECTION
;;;==========================

(defmethod mouse-selection ((self patch-editor-view) pos)
  (let ((p0 pos))
    (om-init-temp-graphics-motion 
     self pos 
     (om-make-graphic-object 'selection-rectangle :position pos :size (om-make-point 4 4))
     :min-move 4
     :release #'(lambda (view position)
                  (select-items-in-rect view 
                                        (om-point-x p0) (om-point-y p0)
                                        (om-point-x position) (om-point-y position)))
     )))


(defmethod select-items-in-rect ((self patch-editor-view) x1 y1 x2 y2) 
  (dolist (item (get-boxframes self))
    (when (rect-intersection x1 y1 x2 y2 
                             (x item) (y item) (+ (x item) (w item)) (+ (y item) (h item)))
      (select-box (object item) t)
      )))


;;;=============================
;;; DEF MENU COMMANDS
;;;=============================

;;; called from menu
(defmethod clear-command ((self patch-editor))
  #'(lambda () (remove-selection self)))

(defmethod select-all-command ((self patch-editor))
  #'(lambda () 
      (let ((focus (or (om-get-subview-with-focus (window self))
                       (main-view self))))
        (when focus ;;; can be the main view or a text-input field for instance
          (select-all-command-for-view self focus)
          ))))

(defmethod select-all-command-for-view ((self patch-editor) (view t))
  (select-unselect-all self t))


;;;===========================================
;;; COPY / CUT / PASTE
;;;===========================================

(defmethod copy-command-for-view ((editor patch-editor) (view t))
  (let* ((boxes (get-selected-boxes editor))
         (connections (save-connections-from-boxes boxes)))
    (set-paste-position nil)
    (set-om-clipboard (list (mapcar 'om-copy boxes) connections))))

(defmethod cut-command-for-view ((editor patch-editor) (view t))
  (copy-command-for-view editor view)
  (store-current-state-for-undo editor) 
  (remove-selection editor))

(defmethod paste-command-for-view ((editor patch-editor) (view t)) nil)

(defmethod paste-command-for-view ((editor patch-editor) (view patch-editor-view))
  (unless (edit-lock editor)
    (let* ((boxes (car (get-om-clipboard)))
           (connections (cadr (get-om-clipboard)))
           (paste-pos (get-paste-position view))
           (ref-pos))
      (select-unselect-all editor nil)
      (when paste-pos
        (setq ref-pos (loop for bb in boxes 
                            minimize (omg-x view (box-x bb)) into xmin
                            minimize (omg-y view (box-y bb)) into ymin
                            finally (return (om-make-point xmin ymin))))
        (set-paste-position nil))

      (store-current-state-for-undo editor)
    
      (loop for b in boxes do
            (let ((graphic-pos (if ref-pos 
                                   (om-add-points paste-pos 
                                                  (om-subtract-points (omg-position view (omp (box-x b) (box-y b))) 
                                                                      ref-pos))
                                 (om-add-points (omg-position view (omp (box-x b) (box-y b))) 
                                                (om-make-point 40 10)))))
              (omng-move b (omng-position view graphic-pos))
              (when (omNG-add-element editor b)
                (let ((frame (make-frame-from-callobj b)))
                  (om-set-view-position frame graphic-pos)
                  (om-add-subviews view frame)
                  (select-box b t)
                  ))))
    
      ;;; connections
      (loop for c in (restore-connections-to-boxes connections boxes) do
            (omng-add-element editor c)
            (add-connection-in-view view c))
    
      (mapc 'after-copy-action boxes) 
      (om-invalidate-view view)
      (set-om-clipboard (list (mapcar 'om-copy boxes) connections))
      )))

(defmethod after-copy-action ((self t)) nil)

(defmethod copy-command-for-view ((editor patch-editor) (view om-editable-text))
  (om-copy-command view))

(defmethod cut-command-for-view ((editor patch-editor) (view om-editable-text))
  (if (edit-lock editor) (om-beep) (om-cut-command view)))

(defmethod paste-command-for-view ((editor patch-editor) (view om-editable-text))
  (if (edit-lock editor) (om-beep) (om-paste-command view)))

(defmethod select-all-command-for-view ((editor patch-editor) (view om-editable-text))
  (om-select-all-command view))



;;; called from menu
(defmethod copy-command ((self patch-editor))
  #'(lambda () 
      (let ((focus (or (om-get-subview-with-focus (window self))
                       (main-view self))))
        (when focus ;;; can be the main view or a text-input field for instance
          (copy-command-for-view self focus)
          ))))

;;; called from menu
(defmethod cut-command ((self patch-editor))
  #'(lambda () 
      (let ((focus (or (om-get-subview-with-focus (window self))
                       (main-view self))))
        (when focus ;;; can be the main view or a text-input field for instance
            (cut-command-for-view self focus)
          ))))

(defmethod paste-command ((self patch-editor))
  #'(lambda () 
      (let ((focus (or (om-get-subview-with-focus (window self))
                       (main-view self))))
        (when focus ;;; can be the main view or a text-input field for instance
            (paste-command-for-view self focus)))))


(defmethod report-modifications ((self patch-editor))
  (call-next-method)
  (patch-editor-set-lisp-code self))

;;;=============================
;;; DRAG BOXES
;;;=============================

(defmethod container-frames-locked ((self t)) t)
(defmethod container-frames-locked ((self patch-editor-view)) 
  (edit-lock (editor self)))

(defmethod om-drag-start ((self OMBoxFrame) pos)
 
  (unless (or *resize-handler* *connection-handler* 
              (active-area-at-pos self pos)
              (om-command-key-p)
              (container-frames-locked (om-view-container self)))
    (let ((pv (om-view-container self)))
      (om-set-focus pv) ;; will close the temporary text-edit field if any
      (setf (dragged-views pv)
            (get-boxframes pv :only-selected t))
      t)))

(defun get-frames-rect (frames-list)
  (when frames-list
  (list (reduce 'min (mapcar 'x frames-list)) 
        (reduce 'min (mapcar 'y frames-list))
        (reduce 'max (mapcar 'x+w frames-list)) 
        (reduce 'max (mapcar 'y+h frames-list)) 
        )))
  
(defmethod om-drag-area ((self OMBoxFrame))
  (let ((pv (om-view-container self)))
    (if (dragged-views pv)
        (values-list (get-frames-rect (dragged-views pv)))
      (call-next-method))))
    
(defmethod om-draw-contents-for-drag ((self OMBoxFrame))
  (let* ((pv (om-view-container self))
         (patch (object (editor pv)))
         (boxframes (dragged-views pv))
         (rect (get-frames-rect boxframes)))
    (om-with-alpha 0.6 
    (loop for v in boxframes do
          (om-with-translation (- (x v) (car rect)) (- (y v) (cadr rect))
            (boxframe-draw-contents v (object v))))
    (loop for c in (connections patch) 
          when (and (find (box (from c)) boxframes :key 'object)
                    (find (box (to c)) boxframes :key 'object))
          do (let ((gc (graphic-connection c)))
               (om-with-translation (- (x gc) (car rect)) (- (y gc) (cadr rect))
                 (om-draw-contents gc)))
          )
    )))

(defmethod om-drag-receive ((self patch-editor-view) (dragged-view OMBoxFrame) position &optional (effect nil))
  (unless (om-points-equal-p (om-view-position dragged-view) position)
    (let* ((init-patch (container (object dragged-view)))
           (patchview (om-view-container dragged-view))
           (editor (editor self))
           (target-patch (object editor))
           (initpos (om-view-position dragged-view))
           (newpositions (mapcar 
                          #'(lambda (view) (om-add-points position (om-subtract-points (om-view-position view) initpos))) 
                          (dragged-views patchview))))
      (unless (or (edit-lock editor)
                  (find-if #'(lambda (p) (or (< (om-point-x p) 0) (< (om-point-y p) 0))) newpositions))
        (let ((connections (save-connections-from-boxes (mapcar 'object (dragged-views patchview))))
              (newboxes nil))
          
          (store-current-state-for-undo editor)
              
          (loop for dview in (dragged-views patchview) 
                for pos in newpositions do
                (let* ((box (object dview))
                       ;;; in case of maquette container
                       (box-beg-position (omng-position self pos)))
                  (if (equal effect :copy)
                      ;;; COPY
                      (let ((newbox (om-copy box)))
                        (pushr newbox newboxes)     
                        (when (and newbox (omNG-add-element (editor self) newbox))
                          (omng-move newbox box-beg-position)
                          (let ((frame (make-frame-from-callobj newbox)))
                            ;;; set the frame at the graphic pos
                            (om-set-view-position frame pos)
                            ;;; set the frame at the graphic size
                            (om-set-view-size frame (om-view-size dview)) 
                            (om-add-subviews self frame)
                            ;;; update connections
                            (update-connections newbox)
                            (select-box newbox t)
                            (select-box box nil)
                            (om-invalidate-view frame)
                            (om-invalidate-view dview)
                            t
                            )))
                    ;;; NOT COPY = MOVE
                    (cond ((equal init-patch target-patch) ;;; IN THE SAME PATCH
                           (omng-move box box-beg-position)
                           (om-set-view-position dview pos)
                           (update-connections box)
                           (redraw-connections dview)
                           (om-invalidate-view self)
                           t)

                          (t ;;; IN ANOTHER PATCH
                             (omng-remove-element init-patch box)
                             (report-modifications (editor init-patch))
                             (mapcar #'(lambda (c) (omng-remove-element init-patch c)) (get-box-connections box))
                             (om-remove-subviews patchview dview)
                     
                             (pushr box newboxes)
                             (when (omNG-add-element (editor self) box)
                               (omng-move box box-beg-position)
                               ;;; set the frame at the graphic pos & size
                               (om-set-view-position dview pos)
                               (om-set-view-size dview (om-view-size dview))
                               (om-add-subviews self dview)
                               (update-connections box)
                               (om-invalidate-view dview)
                               )
                             t)
                          ))))
          (setf (dragged-views self) nil)
          (when newboxes ;;; the boxes have been copied: restore the connections !
            (mapcar #'(lambda (c)
                        (omng-add-element target-patch c)
                        (add-connection-in-view self c)
                        (update-points c))
                    (restore-connections-to-boxes connections newboxes))
            (mapcar 'redraw-connections (get-boxframes self :only-selected t))
            (mapc 'after-copy-action newboxes)
            (om-invalidate-view (editor-view init-patch))
            )
          t))
      )))

;;; drag&drop on oneself = slight move
(defmethod om-drag-receive ((self OMBoxFrame) (dragged-view OMBoxFrame) position &optional (effect nil))
  (let ((patchview (om-view-container self)))
    (when (find self (dragged-views (om-view-container dragged-view)))
      (om-drag-receive patchview dragged-view position effect))))


;;; drag&drop from the computer...
(defmethod om-import-files-in-app ((self patch-editor-view) file-list position) 
  (let* ((file (pathname (car file-list)))
         (objtype (extension-to-doctype (pathname-type file)))
         (newbox 
          (cond ((member objtype '(:patch :maquette :textfun))
                 ;;; TRY TO LOAD THE PATCH
                 (let ((obj (load-doc-from-file file objtype)))
                   (when obj (omNG-make-new-boxcall obj position))))
                (t (om-beep)))))
    (when newbox
      (add-box-in-patch-editor newbox self)
      )))

;;;=============================
;;; TEXT COMPLETION
;;;=============================

(defparameter *om-box-name-completion* t)

(defparameter *all-om-pack-symbols* nil)

(defun set-om-pack-symbols ()
  (setf *all-om-pack-symbols*
        (sort (mapcar 'string-downcase 
                      (append (get-all-symbol-names *om-package-tree*)
                              (get-all-symbol-names *om-libs-root-package*)))
              'string<)))

(defun box-name-completion (string)
  (if (and *om-box-name-completion* (>= (length string) 1))
      (let ((all-str (or *all-om-pack-symbols* (set-om-pack-symbols))))
        (remove-if #'(lambda (str) (not (equal 0 (search string str :test 'string-equal)))) all-str))
    ;:destroy
    ))

(defun patch-name-completion (patch string)
  (if (and *om-box-name-completion* (>= (length string) 1))
      (let* ((searchpath-strings (mapcar 'pathname-name 
                                (and (get-pref-value :files :search-path)
                                     (om-directory (get-pref-value :files :search-path)
                                                   :files t :directories nil
                                                   :type '("opat" "omaq" "olsp")
                                                   :recursive (get-pref-value :files :search-path-rec)))))
             
             (localpath-strings 
              (when (mypathname patch)
                (let* ((currentpathbase (namestring (pathname-dir string)))
                       (localfolder (pathname-dir (merge-pathnames string (mypathname patch))))
                       (filecandidates
                        (mapcar 'pathname-name 
                                (om-directory localfolder
                                              :files t :directories nil
                                              :type '("opat" "omaq" "olsp")
                                              :recursive nil))))
                  (loop for file in filecandidates collect
                        (string+ currentpathbase file))))))
        
        (remove-if 
         #'(lambda (str) (not (equal 0 (search string str :test 'string-equal))))
         (append searchpath-strings localpath-strings))
        
        )
    ))
        
    

;(search "om-" "OM-SCALE" :test 'string-equal)
;(defmethod activate-completion ((self text-input-item))
  ;;; mode 1
  ;(when *om-box-name-completion* (om-set-text-completion self 'box-name-completion))
  ;;; mode 2
;  (setf *om-box-name-completion* t)
;  (om-complete-text self))
       

;;;=============================
;;; NEW BOX
;;;=============================


(defclass text-input-item (om-editable-text) ())
;(defclass text-input-item (om-custom-edit-text) ())

(defmethod get-new-box-from-type ((type t) position container) nil)

;(defmethod om-view-key-handler ((self text-input-item) key)
  ;(if (capi:text-input-pane-text self) (capi:text-input-pane-complete-text self) nil)
  ;)

(defmethod enter-new-box ((self patch-editor-view) position &optional type)
  ;;; mode 2
  ;(setf *om-box-name-completion* nil)
  (let* ((patch (object (editor self)))
         (prompt (if (equal type :patch) "enter patch name (or pathname)" "enter box name"))
         (completion-fun (if (equal type :patch) 
                             #'(lambda (string) (unless (string-equal string prompt)
                                                  (patch-name-completion patch string)))
                           'box-name-completion))
         (textinput 
          (om-make-di 'text-input-item
                      :text prompt
                     ;:focus t
                      :fg-color (om-def-color :gray)
                      :di-action #'(lambda (item) 
                                     (let ((text (om-dialog-item-text item)))
                                       (om-end-text-edit item)
                                       (om-remove-subviews self item)
                                       (unless (string-equal text prompt)
                                         (if (equal type :patch)
                                             (new-abstraction-box-in-patch-editor self text position)
                                           (new-box-in-patch-editor self text position)
                                           ))
                                       (om-set-focus self)))
                      :begin-edit-action #'(lambda (item)
                                             (om-set-fg-color item (om-def-color :dark-gray))
                                             )
                      :edit-action #'(lambda (item)
                                       (let ((textsize (length (om-dialog-item-text item))))
                                         (om-set-fg-color item (om-def-color :dark-gray))
                                         (om-set-view-size item (om-make-point (list :character (+ 2 textsize)) 20))
                                         ))
                      :completion completion-fun
                      :font (om-def-font :font1)
                      :size (om-make-point 100 30)
                      :position position
                     ;ouvre une nouvelle vue de clompletion mais ne la ferme pas...
                     ;:gesture-callbacks (list 
                     ;                    (cons
                     ;                     #\tab
                     ;                     #'(lambda (tip)
                     ;                         (if (capi:text-input-pane-complete-text tip) t :destroy)
                     ;                         ))
                     ;                    (cons
                     ;                     #\space 
                     ;                     #'(lambda (tip)
                     ;                         (if (capi:text-input-pane-in-place-complete tip) nil :destroy)
                     ;                        
                     ;                         )
                     ;                     )
                     ;                    )
                      )))
    (om-add-subviews self textinput)
    (om-set-text-focus textinput t)
    t))


(defmethod special-box-p (name) nil)



(defvar *known-packages* nil)

(defun declare-known-package (p)
  (pushnew p *known-packages*))

(defun search-known-symbol (str)
  (let* ((strUC (string-upcase str))
         (package (when (find #\: strUC) (string-until-char strUC ":"))))
    
    (if package 
        
        (let ((sym-name (subseq strUC (1+ (position #\: strUC :from-end t)))))
          (find-symbol sym-name (intern-k package)))
    
      (let ((sym nil))
        (loop for p in (reverse *known-packages*) 
              while (not sym) do
              (let ((s (find-symbol strUC p)))
                (when (or (fboundp s) (find-class s nil) (special-box-p s))
                  (setf sym s))))
        sym)
      )))

;(string-until-char "CR::CS-EVT" ":")
;(position #\: "CR:CS-EVT" :from-end t)
; (search-known-symbol "iae")
; (find-symbol "CR::CS-EVT" :cr)

(defun export-symbol-from-om (symb)
  (let ((p (symbol-package symb)))
    (export symb p)
    (declare-known-package p)))

(defun decode-input-arguments (text)
  (let ((args nil)
        (texte text)
        (pos 0))
    (loop while (> (length texte) 0) do
          (setf temp (multiple-value-list (read-from-string texte)))
          (setf pos (cadr temp))
          (push (car temp) args)
          (setf texte (subseq texte pos)))
    (reverse args)))


(defmethod new-box-in-patch-editor ((self patch-editor-view) str position)     
  
  (om-with-error-handle 
  
    (when (and (stringp str) (> (length str) 0))
      (multiple-value-bind (first-item other-items) 
          (string-until-char str " ")
    
        (let* ((*package* (find-package :om))
               (read-sym (and (not (equal #\( (elt first-item 0)))
                              (search-known-symbol first-item)))
               (pos (omng-position self position))
               (newbox nil))
          
          ;(print (list str read-sym))
          
          (if read-sym ;;; the symbol is known
              
              (let ((args (decode-input-arguments (delete-spaces other-items))))
                
                (setf newbox
                  
                  (cond       
                   ((special-box-p read-sym)
                    (omNG-make-special-box read-sym pos args))
                   
                   ((om-special-lisp-form-p read-sym)
                    (om-beep-msg  (string+ "Special Lisp form '" str "' can not be created as an OM box!")))
           
                   ((macro-function read-sym)
                    (om-beep-msg  (string+ "OM does not accept macro functions: " str "")))
               
                   ((equal read-sym t)
                    (omNG-make-new-boxcall 'value pos T))
                   
                   ((fboundp read-sym) ;;; FUN BOX (Lisp or OM)
                    (let ((box (omNG-make-new-boxcall (fdefinition read-sym) pos args)))
                      (setf (name box) (string-downcase read-sym)) ;;; sometimes the "real name" is not the same.. (e.g. "list")
                      box))
                   
                   ((and (find-class read-sym nil)  ;;; CLASS BOX
                          ;(subtypep (class-of (find-class name-sym nil)) 'OMClass)
                          ; why not standard-classes... ?
                         )
                    (if (or (om-shift-key-p) (string-equal "slots" (format nil "~A" (car args))))
                        (omNG-make-new-boxcall 'slots pos (find-class read-sym))
                      (let ((box (omNG-make-new-boxcall (find-class read-sym) pos other-items)))
                        (if (and box other-items) (setf (show-name box) t))
                        box)))
                   
                   (t nil)))
                )
            
            ;;; this is not a 'known' symbol.. (but there are more options..)
            (let ((read-2 (read-from-string str nil)))
              
              (if (or (listp read-2) (numberp read-2) (stringp read-2) (quoted-form-p read-2)
                      (characterp read-2)
                      (and (symbolp read-2) (string-equal (package-name (symbol-package read-2)) "KEYWORD")))
                  
                  ;;; => make a value box
                  (progn 
                    (when (quoted-form-p read-2) (setf read-2 (eval read-2)))
                    (setf newbox (omNG-make-new-boxcall 'value pos read-2)))
                
                ;;; unintern the symbol
                (when (symbolp read-2) 
                  (unintern read-2)
                  ))
              )
            )
       
          (if newbox
              (progn 
            ;(when (and (allow-rename newbox) (car args)) ;; not sure this is needed anymore..
            ;  (set-name newbox text))
                (add-box-in-patch-editor newbox self))
            (om-print (format nil "Could not create a box from '~A'" first-item) "PATCH")
            )
          )
        ))))


;;; handle/warn on possible duplicates
(defmethod new-abstraction-box-in-patch-editor ((self patch-editor-view) str position)     
  (let ((patch (object (editor self)))
        (abs-types '("opat" "omaq" "olsp"))
        (doc-path nil))
    
    (when (mypathname patch)
      (let* ((local-restored-path (merge-pathnames str (pathname-dir (mypathname patch))))
             (local-matches (remove nil 
                                    (loop for type in abs-types collect
                                          (probe-file 
                                           (merge-pathnames local-restored-path
                                                            (make-pathname :type type)))))))
        (setq doc-path (car local-matches))
      
        (when (> (length local-matches) 1)
          (om-beep-msg "Warning: there's more than 1 document named ~s in ~s" 
                       (pathname-name local-restored-path)
                       (pathname-dir local-restored-path)))
        ))
    
    (unless doc-path
      ;;; try with the search folder
      (let ((search-matches (remove nil 
                                    (loop for type in abs-types collect
                                          (check-path-using-search-path 
                                           (merge-pathnames str
                                                            (make-pathname :type type)))))))
            (when (> (length search-matches) 1)
              (om-beep-msg "Warning: there's more than 1 document named ~s in your search-path folder" 
                           str))
        ;(print search-matches)
            (setf doc-path (car search-matches))))
        
    (when doc-path
      (let ((obj (load-doc-from-file doc-path (extension-to-doctype (pathname-type doc-path)))))
        (when obj
          (add-box-in-patch-editor 
           (omNG-make-new-boxcall obj position) 
           self))))
    ))
                
(defmethod get-default-size-in-editor ((self OMBox) (editor patch-editor))
  (default-size self))

(defmethod add-box-in-patch-editor ((box OMBox) (view patch-editor-view))
  (store-current-state-for-undo (editor view))
  (when (omNG-add-element (editor view) box)
    (let ((def-size (get-default-size-in-editor box (editor view))))
      (unless (box-w box) 
        (setf (box-w box) (if (scale-in-x-? box) (omng-w view (om-point-x def-size)) (om-point-x def-size))))
      (unless (box-h box) 
        (setf (box-h box) (if (scale-in-y-? box) (omng-h view (om-point-y def-size)) (om-point-y def-size))))
      (let ((frame (make-frame-from-callobj box)))
        (omg-add-element view frame)
        (select-box box t)
        frame))))



;;;======================================;;;======================================
;;; SIDE PANEL
;;;======================================;;;======================================

;;;======================================
;;; LISP CODE
;;;======================================

(defun format-lisp-code-string (code margin) 
  (concatenate 'string (string #\Newline) 
               (write-to-string code :escape t :pretty t :right-margin margin :miser-width margin)))

(defun make-lisp-code-pane (editor)
  (let ((lisp-pane (om-make-di 'om-multi-text 
                               :text (format-lisp-code-string (get-patch-lambda-expression (object editor)) 60)
                               :font (om-make-font "Courier New" 12)
                               :size (omp nil nil)
                               )))
                        
    (set-g-component editor :lisp-code lisp-pane)
                        
    (om-make-layout 
     'om-column-layout :ratios '(1 nil) :delta 10
     :subviews (list 
                
                ;; main pane
                ;; om-simple-layout allows to st a background color
                (om-make-layout 'om-simple-layout :bg-color (om-def-color :white)
                                :subviews (list lisp-pane))
                                    
                (om-make-di 'om-button :text "Copy Lisp code" 
                            :size (omp nil 32) :font (om-def-font :font1)
                            :di-action #'(lambda (b) (om-copy-command lisp-pane)
                                           (om-print "Lisp code copied to clipboard")))
                ))
    ))


(defmethod patch-editor-set-lisp-code ((self patch-editor))
  (when (and (equal (editor-window-config self) :lisp-code)
             (get-g-component self :lisp-code)) ;; just in case..
    (let* ((textpane (get-g-component self :lisp-code))
           (w (om-width textpane))
           (wem (om-string-size "m" (om-get-font textpane))))
    (om-set-dialog-item-text 
     (get-g-component self :lisp-code)
     (format-lisp-code-string (get-patch-lambda-expression (object self)) (round w wem)))
    )))

;;;======================================
;;; LISTENER
;;;======================================

(defun make-listener-pane (editor)
  (let ((listener-pane (om-lisp::om-make-listener-output-pane)))
                        
    (set-g-component editor :listener listener-pane)
                        
    (om-make-layout 
     'om-column-layout :ratios '(1 nil) :delta 0
     :subviews (list 
                
                ;; main pane
                listener-pane
                
                (om-make-layout 'om-row-layout :subviews
                                (list
                                 nil
                                 (om-make-di 'om-button :text "x" 
                                             :size (omp 40 32) :font (om-def-font :font1)
                                             :di-action #'(lambda (b) 
                                                            (om-lisp::om-clear-listener-output-pane listener-pane)
                                                            ))))
                ))
    ))

(defun prompt-on-patch-listener (editor message)
  (when (get-g-component editor :listener)
    (om-lisp::om-prompt-on-echo-area 
     (get-g-component editor :listener)
     message)))

(defun prompt-on-all-patch-listeners (message)
  (loop for win in (om-get-all-windows 'patch-editor-window)
        do (prompt-on-patch-listener (editor win) message)))

(defmethod copy-command-for-view ((editor patch-editor) (view om-lisp::om-listener-pane))
  (om-lisp::om-copy-command view))

(defmethod cut-command-for-view ((editor patch-editor) (view om-lisp::om-listener-pane))
  (om-lisp::om-cut-command view))

(defmethod paste-command-for-view ((editor patch-editor) (view om-lisp::om-listener-pane))
  (om-lisp::om-paste-command view))

(defmethod select-all-command-for-view ((self patch-editor) (view om-lisp::om-listener-pane))
  (om-lisp::om-select-all-command view))

;;;======================================
;;; INSPECTOR
;;;======================================

(defclass inspector-view (om-view) 
  ((object :initarg :object :initform nil :accessor object)))


(defmethod make-inspector-pane ((editor patch-editor))

  (let ((inspector-pane (om-make-view 'inspector-view :size (omp nil nil) :direct-draw nil)))
    
    (set-g-component editor :inspector inspector-pane)
    
    ;;; initialize with current selection
    (update-inspector-for-editor editor)
    
    inspector-pane))


(defmethod object-name-in-inspector ((self OMObject)) (name self))
(defmethod object-name-in-inspector ((self t)) "-")

(defmethod get-documentation ((self t)) "...")
(defmethod get-documentation ((self OMFunBoxCall)) (function-documentation (reference self)))
(defmethod get-documentation ((self OMBoxEditCall)) (class-documentation (reference self)))
(defmethod get-documentation ((self OMPatchComponentBox)) (class-documentation (class-of (reference self))))

;;; redefined for connections
(defmethod get-update-frame ((self t)) nil)

(defmethod set-inspector-contents ((self inspector-view) object)
  
  (setf (object self) object)
  (om-remove-all-subviews self)        

  (let ((inspector-layout
         
         (if object
             
             (om-make-layout
              'om-column-layout
              :subviews 
              (append 
               (cons 
                (om-make-di 'om-simple-text :size (om-make-point nil 20) 
                            :fg-color (om-def-color :dark-gray)
                            :text (object-name-in-inspector object)
                            :focus t  ;; prevents focus on other items :)
                            :font (om-def-font :font3b))
                  
                (when t ;object 
                  (list 
                   (om-make-layout
                    'om-grid-layout
                    :delta '(10 0) :align nil
                    :subviews 
                  
                    (if (get-properties-list object)
                   
                        ;;; ok
                        (loop for category in (get-properties-list object)
                              when (cdr category)
                              append 
                              (append 
                               (list  ;     (car category)  ; (list (car category) (om-def-font :font1b))  ; :right-extend          
                                (om-make-di 'om-simple-text :size (om-make-point 20 20) :text "" :focus t)
                                (om-make-di 'om-simple-text :text (car category) :font (om-def-font :font2b)
                                            :size (om-make-point (+ 10 (om-string-size (car category) (om-def-font :font2b))) 20)
                                            )
                                )
                               (loop for prop in (cdr category) append
                                     (list (om-make-di 'om-simple-text :text (string (nth 1 prop)) :font (om-def-font :font1)
                                                       :size (om-make-point 90 20) :position (om-make-point 10 16))
                                           (make-prop-item (nth 2 prop) (nth 0 prop) object :default (nth 4 prop) 
                                                           :update (get-update-frame object)
                                                           )
                                           ))
                          
                               (list (om-make-di 'om-simple-text :size (om-make-point 20 6) :text "" :focus t) 
                                     (om-make-di 'om-simple-text :size (om-make-point 20 6) :text "" :focus t))
                               )
                              )
                 
                      ;;; object has no properties (unlikely)
                      (list 
                       (om-make-di 'om-simple-text :size (om-make-point 100 20) 
                                   :text "[no properties]"
                                   :font (om-def-font :font1)) 
                       nil))
           
             
                    ))))
             
               (when (get-documentation object)
                 (list
                
                ;(om-make-di 'om-simple-text :size (om-make-point nil 18) 
                ;            :text "----------documentation"
                ;            :font (om-def-font :font2)
                ;            :fg-color (om-def-color :dark-gray))
                  :separator 
                
                  (let ((doc (get-documentation object)))
                    (om-make-di 'om-multi-text :size (om-make-point nil nil) ; (* 40 (length (string-lines-to-list doc))) 
                                :text doc
                                :font (om-def-font :font1)))
                  
                  ))
               )
              )

           ;;; else: no object
           (om-make-layout
            'om-simple-layout
            :subviews 
            (list 
             (om-make-di 'om-simple-text :size (om-make-point 225 20) 
                         :text "[no selection]"
                         :fg-color (om-def-color :dark-gray)
                         :focus t  ;; prevents focus on other items :)
                         :font (om-def-font :font3b))
             
             )))
         ))
    
    
      (om-add-subviews self inspector-layout))
  
  (when (editor self)
    (om-update-layout (window (editor self))))
  
  )


(defmethod update-inspector-for-editor ((self patch-editor) &optional obj)

  (let ((obj-to-inspect 
         (or obj 
             (let ((selection (append (get-selected-boxes self)
                                      (get-selected-connections self))))
               (if (= 1 (length selection)) 
                   (car selection) 
                 selection)))))

    (when (get-g-component self :inspector)
      (set-inspector-contents (get-g-component self :inspector) obj-to-inspect))
      
    ))

(defun update-inspector-for-object (obj)
  (let ((ed (editor (get-update-frame obj))))
    (when ed
      (update-inspector-for-editor ed obj))))


;;; when the object is deleted
(defun close-inspector-for-box (box)
  (let ((ed (editor (get-update-frame box))))
    (when ed
      (update-inspector-for-editor ed nil))))

(defmethod remove-selection :after ((self patch-editor))
  (let ((inspector (get-g-component self :inspector)))
    (when (and inspector
               (not (find (object inspector) 
                          (append (boxes (object self))
                                (connections (object self)))
                          :test 'equal)))
      
    (update-inspector-for-editor self nil))))



;;; !! object and view can be lists !!
(defmethod set-inspector-contents ((self inspector-view) (object cons))
  (let ((virtual-obj (make-instance 'virtual-object-selection :objects object)))
    (set-inspector-contents self virtual-obj)
    ))


;;;======================================
;;; OPTIONAL SIDE PANEL (GENERAL)
;;;======================================

(defmethod patch-editor-set-window-config ((self patch-editor) mode)
  (unless (equal (editor-window-config self) mode)
    (setf (editor-window-config self) mode)
    (build-editor-window self)
    (init-editor-window self)))

(defmethod make-layout-items ((editor patch-editor))
    
  (om-make-layout 
   'om-column-layout 
   :subviews (append
               
              (when (editor-window-config editor)
                 (list 
                  (om-make-graphic-object 
                   'om-icon-button :icon :xx :icon-pushed :xx-pushed
                   :size (omp 12 12)
                   :action #'(lambda (b) (patch-editor-set-window-config editor nil))
                   )
                  nil))
                                
               (list 
                (om-make-graphic-object 
                 'om-icon-button :size (omp 16 16) 
                 :icon :info-gray :icon-disabled :info
                 :lock-push nil :enabled (not (equal (editor-window-config editor) :inspector))
                 :action #'(lambda (b) 
                             (patch-editor-set-window-config 
                              editor 
                              (if (equal (editor-window-config editor) :inspector) nil :inspector)))
                 )
    
                (om-make-graphic-object 
                 'om-icon-button :size (omp 16 16) 
                 :icon :lisp-gray :icon-disabled :lisp
                 :lock-push nil :enabled (not (equal (editor-window-config editor) :lisp-code))
                 :action #'(lambda (b) 
                             (patch-editor-set-window-config 
                              editor 
                              (if (equal (editor-window-config editor) :lisp-code) nil :lisp-code)))
                 )
                    
                (om-make-graphic-object 
                 'om-icon-button :size (omp 16 16) 
                 :icon :listen-gray :icon-disabled :listen
                 :lock-push nil :enabled (not (equal (editor-window-config editor) :listener))
                 :action #'(lambda (b) 
                             (patch-editor-set-window-config 
                              editor 
                              (if (equal (editor-window-config editor) :listener) nil :listener)))
                 )
                               
                nil ;;; space at the bottom
                ))
   ))


(defmethod make-editor-window-contents ((editor patch-editor))

  (let ((patch-view (call-next-method))
        (layout-items (make-layout-items editor)))
    
     (set-g-component editor :lisp-code nil)
     (set-g-component editor :listener nil)
     (set-g-component editor :inspector nil)
       
     (if (editor-window-config editor) ;; non-NIL

        ;;; patch editor with side-panel
        (let ((side-pane 
               
               (om-make-layout 
                'om-column-layout 
                :ratios '(nil nil 1) :delta 10
                :subviews (list 
                           ;; top of the pane
                           (om-make-di 'om-simple-text :size (omp 230 18)
                                                   :font (om-def-font :font2b) :text 
                                                   (case (editor-window-config editor)
                                                     (:lisp-code "Lisp code")
                                                     (:listener "listener / system out")
                                                     (:inspector "inspector"))
                                                   :fg-color (om-def-color :dark-gray))
                           :separator
                           
                           ;; main pane
                           (cond ((equal (editor-window-config editor) :lisp-code)
                                  (make-lisp-code-pane editor))
                                 
                                 ((equal (editor-window-config editor) :listener)
                                  (make-listener-pane editor))

                                 ((equal (editor-window-config editor) :inspector)
                                  (make-inspector-pane editor))
                                 
                                 (t nil))
                           ))

               
               
               ))
           
          (values 
           (case (editor-window-config editor) 
             
             (:inspector (om-make-layout 'om-row-layout 
                                         :delta 2 :ratios '(1 nil nil)
                                         :subviews (list patch-view side-pane layout-items)))
             
             (otherwise (om-make-layout 'om-row-layout 
                                         :delta 2 :ratios '(10 nil 1 nil)
                                         :subviews (list patch-view :divider side-pane layout-items)))
             )
           
           patch-view))
      
      ;; normal patch editor 
      (values (om-make-layout 'om-row-layout 
                              :ratios '(1 nil)
                              :subviews (list patch-view layout-items))
              patch-view)
      )
    ))





    
