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

;;;=============================
;;; EDITOR
;;;=============================

(defclass patch-editor (OMDocumentEditor undoable-editor-mixin) 
  ((show-lisp-code :accessor show-lisp-code :initarg :show-lisp-code :initform nil)
   (show-documentation :accessor show-documentation :initarg :show-documentation :initform nil)))

(defmethod object-has-editor ((self OMPatch)) t)
(defmethod get-editor-class ((self OMPatch)) 'patch-editor)

(defmethod edit-lock ((self patch-editor)) (lock (object self)))

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

(defmethod put-patch-boxes-in-editor-view ((self OMPatch) (view t)) nil)
 
(defmethod put-patch-boxes-in-editor-view ((self OMPatch) (view patch-editor-view)) 
  (mapc 
   #'(lambda (box) 
       (omg-add-element view (make-frame-from-callobj box)))
   (boxes self))
  (mapc
   #'(lambda (c) (add-connection-in-view view c)) 
   (connections self)))

;;; redefined in maquette-editor
(defmethod init-editor-window ((editor patch-editor))
  (call-next-method)
  (put-patch-boxes-in-editor-view (object editor) (main-view editor)) 
  (update-window-name editor))


(defun draw-h-grid-line (view y) 
  (om-draw-line 0 y (w view) y))

(defun draw-v-grid-line (view x) 
  (om-draw-line x 0 x (h view)))

(defmethod draw-patch-grid ((self patch-editor-view) &optional (d 50))
  (om-with-fg-color (om-def-color :light-gray)
    (om-with-line '(2 2)
      (loop for i from d to (w self) by d do
            (draw-v-grid-line self i))
      (loop for i from d to (h self) by d do
            (draw-h-grid-line self i)))))


(defmethod draw-lock-buttons ((self patch-editor))
  (om-draw-picture (if (lock (object self)) 'lock 'unlock) :x 0 :y 2 :w 20 :h 20))

(defmethod om-draw-contents ((self patch-editor-view))
  (let ((editor (editor (om-view-window self))))
    (when (grid (object editor)) (draw-patch-grid self (grid (object editor))))
    (draw-lock-buttons editor) 
    (mapcar 'om-draw-contents (get-grap-connections self))))

(defmethod window-name-from-object ((self OMPatchInternal))
  (format nil "~A  [~A]" (name self) "internal patch"))

(defmethod update-inspector-for-editor ((self patch-editor))
  (when (window self) ;; window is open and initialized
    (let ((selection (append (get-selected-boxes self) 
                             (get-selected-connections self))))
 (if (= 1 (length selection)) 
     (update-inspector (car selection) (get-update-frame (car selection)))
   (update-inspector selection (mapcar 'get-update-frame selection)))
 
 )))

;(defmethod init-editor ((self patch-editor))
;  (setf (saved? (object self)) t))


(defmethod om-menu-items ((self patch-editor))
  (remove nil
            (list 
             (main-app-menu-item)
             (om-make-menu "File" (default-file-menu-items self))
             (om-make-menu "Edit" 
                           (append 
                            (default-edit-menu-items self)
                            (list (om-make-menu-comp 
                                   (list (om-make-menu-item  
                                          "Show Lisp code" ;(if (show-lisp-code self) "Hide Lisp code" "Show Lisp code")
                                          #'(lambda () (patch-editor-show-lisp-code self (not (show-lisp-code self))))
                                          :key "l" :selected #'(lambda () (show-lisp-code self))
                                          )
                                         (om-make-menu-item  
                                          "Edit lock" 
                                          #'(lambda () (setf (lock (object self)) (not (lock (object self))))
                                              (om-invalidate-view (main-view self)))
                                          :key "e" :selected #'(lambda () (lock (object self)))
                                          ))
                                   :selection t
                                   )
                                  )))
             (om-make-menu "Windows" (default-windows-menu-items self))
             (om-make-menu "Help" (cons 
                                   (om-make-menu-item  
                                          "See Function/Class reference page..." 
                                          #'(lambda () 
                                              (loop for ref in (remove-duplicates (mapcar 'reference (get-selected-boxes self)))
                                                    do (show-reference-page ref)))
                                          :key "d")
                                   (default-help-menu-items self)))
             )))

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
    (unregister-document (object self))))

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
  (if (om-point-in-rect-p position 0 0 20 20)
      (progn
        (setf (lock (object (editor self))) (not (lock (object (editor self)))))
        (om-invalidate-area self 0 0 20 20))
    (progn
      (unless (om-shift-key-p)
        (select-unselect-all (editor self) nil))
      (or (click-connection-handle self position)
          (progn
            (when (om-get-clipboard) (set-paste-position position self))
            (mouse-selection self position))))
    ))

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

        (#\g (setf (grid patch) (if (grid patch) nil 50))
             (om-invalidate-view panel))

        (#\n (if selected-boxes
                 (mapc 'set-show-name selected-boxes)
               (unless (edit-lock editor)
                 (make-new-box panel))))

        (#\i (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'initialize-size (or selected-boxes selected-connections))))
        
        (#\I (mapc 'initialize-box-value selected-boxes))
        
        (:om-key-left (unless (edit-lock editor)
                        (if (om-option-key-p) 
                            (mapc 'optional-input-- selected-boxes)
                          (let ((selection (or selected-boxes selected-connections)))
                            (store-current-state-for-undo editor :action :move :item selection)
                            (mapc 
                             #'(lambda (f) (move-box f (if (om-shift-key-p) -10 -1) 0)) 
                             selection))
                          )))
        (:om-key-right (unless (edit-lock editor)
                         (if (om-option-key-p) 
                             (mapc 'optional-input++ selected-boxes)
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
               (store-current-state-for-undo editor)
               (mapc 'keyword-input++ selected-boxes)))
        (#\+ (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'keyword-input++ selected-boxes)))
        (#\K (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'keyword-input-- selected-boxes)))
        (#\- (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'keyword-input-- selected-boxes)))
        (#\> (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'optional-input++ selected-boxes)))
        (#\< (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'optional-input-- selected-boxes)))
    
        (#\b (store-current-state-for-undo editor) 
             (mapc 'switch-lock-mode selected-boxes))
        (#\1 (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'switch-evonce-mode selected-boxes)))
        (#\l (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'switch-lambda-mode selected-boxes)))

        (#\m (mapc 'change-display selected-boxes))

        (#\a (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'internalize-abstraction selected-boxes)))

        (#\c (unless (edit-lock editor)
               (store-current-state-for-undo editor)
               (if selected-boxes
                   (auto-connect-box selected-boxes editor panel)
                 (make-new-comment panel))))
        (#\C (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (auto-connect-seq selected-boxes editor panel)))
        
        (#\E (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (encapsulate-patchboxes editor panel selected-boxes)))
        (#\U (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (unencapsulate-patchboxes editor panel selected-boxes)))

        (#\v (eval-command panel selected-boxes))
    
        (#\r (unless (edit-lock editor) 
               (store-current-state-for-undo editor)
               (mapc 'set-reactive-mode (or selected-boxes selected-connections))))
      
        ;;; play/stop commands
        (#\p (play-boxes selected-boxes))
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

;;; CLOSE INSPECTOR IF ITS OBJECT HAS BEEN DESTROYED (?)
(defmethod remove-selection :after ((self patch-editor))
  (when (and *inspector-window*
             (not (find (object *inspector-window*) (append (boxes (object self))
                                                            (connections (object self)))
                        :test 'equal)))
    (om-close-window *inspector-window*)))

;;; called from menu
(defmethod clear-command ((self patch-editor))
  #'(lambda () (remove-selection self)))

(defmethod make-new-box ((self patch-editor-view))
  (let ((mp (om-mouse-position self)))
    (enter-new-box self (if (om-point-in-rect-p mp 0 0 (w self) (h self))
                            mp (om-make-point (round (w self) 2) (round (h self) 2))))
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
;;; MENU COMMANDS
;;;=============================

;Redefine for maquette etc ?
(defmethod get-info-command ((self patch-editor)) 
  #'(lambda () (show-inspector-window self)))

(defmethod show-inspector-window ((self patch-editor))
  (let ((selection (append (get-selected-boxes self)
                           (get-selected-connections self))))
    (if (= 1 (length selection))
        (show-inspector (car selection) (get-my-view-for-update (get-update-frame (car selection)))))
    (show-inspector selection (loop for obj in selection collect (get-my-view-for-update (get-update-frame obj))))
    ))
      
(defmethod get-my-view-for-update ((self t)) self)

(defmethod select-all-command ((self patch-editor))
  #'(lambda () (select-unselect-all self t)))


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
    ))

(defmethod after-copy-action ((self t)) nil)

(defmethod copy-command-for-view ((editor patch-editor) (view om-editable-text))
  (om-copy-command view))

(defmethod cut-command-for-view ((editor patch-editor) (view om-editable-text))
  (if (edit-lock editor) (om-beep) (om-cut-command view)))

(defmethod paste-command-for-view ((editor patch-editor) (view om-editable-text))
  (if (edit-lock editor) (om-beep) (om-paste-command view)))


;;; called from menu
(defmethod copy-command ((self patch-editor))
  #'(lambda () 
      (let ((focus (or (om-get-subview-with-focus (main-view self))
                       (main-view self))))
        (when focus ;;; can be the main view or a text-input field for instance
          (copy-command-for-view self focus)
          ))))

;;; called from menu
(defmethod cut-command ((self patch-editor))
  #'(lambda () 
      (let ((focus (or (om-get-subview-with-focus (main-view self))
                       (main-view self))))
        (when focus ;;; can be the main view or a text-input field for instance
            (cut-command-for-view self focus)
          ))))

(defmethod paste-command ((self patch-editor))
  #'(lambda () 
      (let ((focus (or (om-get-subview-with-focus (main-view self))
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

(defmethod enter-new-box ((self patch-editor-view) position)
  ;;; mode 2
  ;(setf *om-box-name-completion* nil)
  (let ((textinput 
         (om-make-di 'text-input-item
                     :text "enter box name"
                     ;:focus t
                     :fg-color (om-def-color :gray)
                     :di-action #'(lambda (item) 
                                    (let ((text (om-dialog-item-text item)))
                                      (om-end-text-edit item)
                                      (om-remove-subviews self item)
                                      (unless (string-equal text "enter box name")
                                        (new-box-in-patch-editor self text position))
                                      (om-set-focus self)))
                     :begin-edit-action #'(lambda (item)
                                            (om-set-fg-color item (om-def-color :dark-gray))
                                            )
                     :edit-action #'(lambda (item)
                                      (let ((textsize (length (om-dialog-item-text item))))
                                        (om-set-fg-color item (om-def-color :dark-gray))
                                        (om-set-view-size item (om-make-point (list :character (+ 2 textsize)) 20))
                                        ))
                     :completion 'box-name-completion
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
         (package (when (find #\: strUC) (string-until-char strUC ":")))
         (sym nil))
    (when package 
      (setf strUC (subseq strUC (1+ (position #\: strUC :from-end t)))))
    (unless sym
      (if package
          (setf sym (find-symbol strUC (intern-k package)))
        (loop for p in (reverse *known-packages*) 
              while (not sym) do
              (setf sym (find-symbol strUC p))))
      sym)))

;(string-until-char "CR::CS-EVT" ":")
;(position #\: "CR:CS-EVT" :from-end t)
; (search-known-symbol "cr::cs-evt")
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

;;;======================================
;;; LISP CODE
;;;======================================

(defun format-lisp-code-string (code margin) 
  (write-to-string code :escape t :pretty t :right-margin margin :miser-width margin))

(defmethod make-editor-window-contents ((editor patch-editor))
  (let ((patch-view (call-next-method)))
    (if (show-lisp-code editor)
        (let ((text-pane (om-make-di 'om-multi-text 
                                     :text (format-lisp-code-string (get-patch-lisp-code (object editor)) 60)
                                     :font (om-make-font "Courier New" 12)
                                     ;:bg-color (om-def-color :white)
                                     :size (omp nil nil)
                                     )))
          (set-g-component editor :lisp-code text-pane)
          (values 
           (om-make-layout 
            'om-row-layout :delta 2 :ratios '(60 nil 40)
            :subviews (list 
                       patch-view 
                       :divider
                       (om-make-layout 
                        'om-column-layout :ratios '(1 nil)
                        :subviews (list text-pane
                                        (om-make-layout 
                                         'om-row-layout
                                         :subviews (list
                                                    (om-make-di 'om-button :text "Copy Lisp code" 
                                                                :size (omp 140 32) :font (om-def-font :font1)
                                                                :di-action #'(lambda (b) (om-copy-command text-pane)
                                                                               (om-print "Lisp code copied to clipboard")))
                                                    nil
                                                    (om-make-di 'om-button :text "close" 
                                                                :size (omp 60 32) :font (om-def-font :font1)
                                                                :di-action #'(lambda (b) (patch-editor-show-lisp-code editor nil)))
                                                    ))))))
           patch-view))
      (progn 
        (set-g-component editor :lisp-code nil)
        patch-view))))

(defmethod patch-editor-show-lisp-code ((self patch-editor) t-or-nil)
  (setf (show-lisp-code self) t-or-nil)
  (build-editor-window self)
  (init-editor-window self))


(defmethod patch-editor-set-lisp-code ((self patch-editor))
  (when (and (show-lisp-code self)
             (get-g-component self :lisp-code)) ;; just in case..
    (let* ((textpane (get-g-component self :lisp-code))
           (w (om-width textpane))
           (wem (om-string-size "m" (om-get-font textpane))))
    (om-set-dialog-item-text 
     (get-g-component self :lisp-code)
     (format-lisp-code-string (get-patch-lisp-code (object self)) (round w wem)))
    )))
    
