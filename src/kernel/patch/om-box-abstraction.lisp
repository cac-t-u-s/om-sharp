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

(in-package :om)

(defclass OMBoxAbstraction (OMBoxCall) 
  ((pre-delay :initform 0 :accessor pre-delay)
   (ready :initform t :accessor ready))
  (:default-initargs :text-align :center :icon-pos :left)
  (:metaclass omstandardclass))


;;; a tricky accessor for patch names :)
(defmethod box-patch-name-access ((box OMBoxAbstraction) &optional (name nil name-provided-p))
  (if name-provided-p
      ;;; SET
      (if (is-persistant (reference box))
          
          ;;; Global abstraction:
          ;;; don't do it if it doesn't change!
          ;;; this can break the recursive-patch loading system
          ;;; by updating the patch reference
          (when (not (pathname-match-p (mypathname (reference box))
                                       (pathname name))) 
            
            (print (mypathname (reference box)))
            (print (pathname name))
            
            (let ((patch (container box)))
                
              (when (editor patch) (store-current-state-for-undo (editor patch)))
              
              (let ((newpatch (and (probe-file name)
                                    (load-doc-from-file name :patch))))
                (if newpatch
                     (let ((oldpatch (reference box)))
                       (release-reference oldpatch box)
                       (setf (reference box) newpatch)
                       (retain-reference newpatch box)
                       (update-from-reference box)
                       )
                  (progn 
                    (om-message-dialog "this file is not a valid patch !")
                    (update-inspector-for-object box))))))
        
        ;;; Internal abstraction
        (let ((patch (container box)))
          (when (editor patch) (store-current-state-for-undo (editor patch)))
          (set-name (reference box) name))
        )
  
    ;;; GET
    (if (is-persistant (reference box))
        (mypathname (reference box))
      (name (reference box)))
    ))


;;; Note :file-name will be stored and saved as a property of teh file. 
;;; This is a redundant information with the box reference's "mypathname"
;;; (but also an absoute path, while the reference will be saved relative)
(defmethod get-properties-list ((self OMBoxAbstraction))
  
  (add-properties-list 
   
   (call-next-method) 
   
   `(("Appearance"
        ((:display "View (m)" ,(display-modes-for-object (reference self)) display)))
       ("Abstraction"
        (
         ,(if (is-persistant (reference self))
              '(:filename "File name" :path box-patch-name-access)   
            '(:name "Name" :string box-patch-name-access))
         ))
       ("Scheduling"
        ((:pre-delay "Pre-delay (ms)" :number pre-delay))))
   ))

(defmethod object-name-in-inspector ((self OMBoxAbstraction)) 
  (format nil "~A box" (get-object-type-name (reference self))))

(defmethod set-reactive ((self OMBoxAbstraction) val)
  (setf (ready self) nil)
  (call-next-method))

(defmethod initialize-box-value ((self OMBoxAbstraction) &optional value)
  (setf (ready self) nil)
  (call-next-method))


(defmethod omNG-make-new-boxcall ((reference OMProgrammingObject) pos &optional init-args)
  (let* ((box (make-instance (get-box-class reference)
                            :name (if init-args (format nil "~A" (car init-args)) (name reference))
                            :reference reference)))

    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-h box) 60)

    (retain-reference reference box)
    box))


(defmethod get-icon-id ((self OMBoxAbstraction)) 
  (icon (reference self)))

 
(defmethod create-box-inputs ((self OMBoxAbstraction)) 
 
  (when (reference self)

    ;;; temp inputs might have been created for non-finished abstraction boxes (e.g. loading recursive patches)
    (let* ((temp-ins (remove-if-not #'numberp (inputs self) :key #'reference))
          
           (new-inputs 
            (mapcar #'(lambda (in) 
                        (make-instance 'box-input :reference in 
                                       :name (name in)
                                       :box self
                                       :value (eval (defval in))
                                       :doc-string (doc in)))
                    (sort (get-inputs (reference self)) '< :key 'index)
                    )))
      
      (when (> (length temp-ins) (length new-inputs))
          (setf new-inputs (append new-inputs (nthcdr (length new-inputs) temp-ins))))
      
      new-inputs)))
      

(defmethod create-box-outputs ((self OMBoxAbstraction)) 
 
  (when (reference self)

    ;;; when temp inputs have been created for non-finished abstraction boxes (e.g. loading recursive patches)
    (let ((temp-outs (remove-if-not #'numberp (outputs self) :key #'reference))
          
          (new-outputs 
           (mapcar #'(lambda (out) 
                       (make-instance 'box-output :reference out 
                                      :name (name out)
                                      :box self
                                      :doc-string (doc out)))
                   (sort (get-outputs (reference self)) '< :key 'index)
                   )))
      
      (when (> (length temp-outs) (length new-outputs))
          (setf new-outputs (append new-outputs (nthcdr (length new-outputs) temp-outs))))
      
      new-outputs)))
      
      

;;; artificially create temporary inputs for non-finished abstraction boxes (e.g. loading recursive patches)
(defmethod gen-temp-nth-input ((self OMBoxAbstraction) n)
  (setf (inputs self) 
        (append (inputs self)
                (loop for i from (length (inputs self)) to n
                      collect (make-instance 'box-input :box self :reference i))))
  (nth n (inputs self)))

;;; artificially create temporary outputs for non-finished abstraction boxes (e.g. loading recursive patches)
(defmethod gen-temp-nth-output ((self OMBoxAbstraction) n)  
  (setf (outputs self) 
        (append (outputs self)
                (loop for i from (length (outputs self)) to n
                      collect (make-instance 'box-output :box self :reference i))))
  (nth n (outputs self)))


(defmethod update-from-reference ((self OMBoxAbstraction))
  (setf (name self) (name (reference self)))
  (call-next-method))


(defmethod om-copy ((self OMBoxAbstraction)) 
  (let ((newbox (call-next-method)))
    (setf (box-w newbox) (box-w self) (box-h newbox) (box-h self))
    (retain-reference (reference newbox) newbox)
    newbox))


(defmethod collect-all-containers ((patch OMPatch))
  (let ((containers nil))
    (loop for ref in (references-to patch)
          when (and (subtypep (type-of ref) 'OMBox) 
                    (not (find (container ref) containers)))
          do (setf containers (append containers (cons (container ref) 
                                                       (collect-all-containers (container ref))))))
    containers))


;;;=======================
;;; THE FOLLOWING BEHAVIOURS DIFFER BETWEEN INTERNAL- AND FILE-ABSTRACTION
;;;=======================
  
;;; closes editor / removes view (may still exist in the undo stack)
(defmethod omng-delete ((box OMBoxAbstraction))
  
  (let ((patch (reference box)))
    
    (when (find box (references-to patch))  
      
      (release-reference patch box)
       
      ;;; close/delete recursive only if this is an internal patch
      ;;; OR a persistant patch that has no outside reference and no open editor
      (unless (and (is-persistant patch)
                   (or (get-outside-references patch)
                       (editor patch)))
        
        ;;; same as closing the editor 
        ;(loop for refb in (box-references-to patch)
        ;    do (release-reference patch refb))
    
        (delete-internal-elements patch)
        (unless (is-persistant patch) (close-editor patch)) ;; persistent patches may remain open
        )
      )
    t
    ))


(defmethod allow-rename ((self OMBoxAbstraction)) 
  (not (is-persistant (reference self))))


;;; only internal does report to the container's editor
(defmethod update-from-editor ((self OMBoxAbstraction) &key (value-changed t) (reactive t))

  (declare (ignore value-changed reactive)) ;;; reactive is handled in a :around method
  
  (when (frame self)
    (reset-cache-display self)
    (om-invalidate-view (frame self)))
  
  (unless (is-persistant (reference self))
    (report-modifications (editor (container self))))
  
  (call-next-method))


(defmethod allow-text-input ((self OMBoxAbstraction)) 
  (unless (is-persistant (reference self))
    (values (name self)
            #'(lambda (box text)
                (declare (ignore box))
                ;;; the box name shall be updated as well
                (set-name (reference self) text)
                (update-inspector-for-object self)
                ))))


(defmethod internalize-abstraction ((self OMBox)) nil)
(defmethod internalized-type ((self t)) (type-of self))
(defmethod copy-contents ((from t) (to t)) nil)

(defmethod internalize-abstraction ((self OMBoxAbstraction))
  (if (is-persistant (reference self))
      (let* ((old-ref (reference self))
             (new-ref (make-instance (internalized-type old-ref) :name (name old-ref))))
        (close-editor old-ref)
        (copy-contents old-ref new-ref)
        (release-reference old-ref self)
        (setf (reference self) new-ref)
        (retain-reference new-ref self)
        (redraw-frame self))
    (om-beep-msg "The ~A '~A' is already internal." (type-of (reference self)) (name (reference self)))
    ))
      

(defmethod allowed-move ((box OMBoxAbstraction) (destination patch-editor)) 
  (or (is-persistant (reference box)) ;; recursion
      (and 
       (not (equal (reference box) (object destination))) ;; patch in itself
       (not (find (reference box) (collect-all-containers (object destination)))) ;;; patch in one of its own childs
       )))


;;;===================
;;; DISPLAY
;;;===================

(defmethod box-draw ((self OMBoxAbstraction) (frame OMBoxFrame))
  
  (when (> (h frame) 36)
    (case (display self)
      
      (:mini-view 
       (draw-mini-view (reference self) self 10 0 (- (w frame) 20) (h frame) nil))
      
      (:value 
       ;(draw-mini-view (get-box-value self) self 4 4 (- (w frame) 8) (- (h frame) 12) nil)
       ;(draw-mini-arrow 24 9 3 10 7 1)
       (draw-values-as-text self 0 0)
       )
      
      (otherwise 
       (let ((label (string-upcase (get-object-type-name (reference self))))
             (font (om-def-font :font1 :face "arial" :size 18 :style '(:bold))))
       (om-with-font font
                     (om-with-fg-color (om-make-color 0.6 0.6 0.6 0.5)
                       (om-draw-string (- (/ (w frame) 2) (/ (om-string-size label font) 2))
                                       (max 22 (+ 6 (/ (h frame) 2)))
                                       label)))


                     ))
      ))
  (draw-patch-icon self)
  t)


(defun draw-mini-arrow (ax ay b w h i) 
  (om-with-fg-color (om-make-color 1 1 1)
    (om-draw-polygon (list 
                      (+ ax b) ay 
                      (+ ax b w) ay 
                      (+ ax b w) (+ ay h)
                      (+ ax b w b) (+ ay h) 
                      (+ ax b (/ w 2)) (+ ay h 5) 
                      ax (+ ay h)
                      (+ ax b) (+ ay h))
                     :fill t))
  (om-with-fg-color (om-make-color .5 .5 .5)
    (om-draw-polygon (list 
                      (+ ax b) ay 
                      (+ ax b w) ay 
                      (+ ax b w) (+ ay h)
                      (+ ax b w b) (+ ay h) 
                      (+ ax b (/ w 2)) (+ ay h 5) 
                      ax (+ ay h)
                      (+ ax b) (+ ay h))
                     :fill nil))
   
   (om-draw-string  (+ ax 5) (+ ay 9) (format nil "~D" i) :font (om-def-font :font1b) :color (om-make-color .5 .5 .5))
   
   )


;;; display reference instead of value
(defmethod change-display ((self OMBoxAbstraction)) 
  (when (visible-property (get-properties-list self) :display)
    (let ((next-mode (next-in-list (display-modes-for-object (reference self))
                                   (display self))))
      (set-display self next-mode))))


(defmethod draw-values-as-text ((self OMBox) &optional (offset-x 0) (offset-y 0))
  (om-with-fg-color (om-def-color :gray)
    (om-with-font (om-def-font :font1b)
                         ;(om-draw-string 40 18 "values:")
                  (loop for v in (or (value self) (make-list (length (outputs self)))) 
                        for y = (+ offset-y 18) then (+ y 16) 
                        for i = 1 then (+ 1 i) do 
                        (draw-mini-arrow (+ offset-x 24) (- y 9) 3 10 7 i)
                        (om-draw-string (+ offset-x 45) y (format nil "~A" v)))
                  ))
  )

(defmethod draw-patch-icon ((self OMBoxAbstraction) &optional (offset-x 0) (offset-y 0))
  (let* ((abs (reference self))
         (iconsize (if (is-persistant abs) 22 16)))
    (om-draw-picture (icon abs) :x (+ 4 offset-x) :y (+ 6 offset-y) :w iconsize :h iconsize)
    ))

(defmethod minimum-size ((self OMBoxAbstraction))
  (multiple-value-bind (tw th) 
      (om-string-size (name self) (box-draw-font self))
    (om-make-point (round 
                    (+ 10 
                       (max (+ 28 tw)
                            (* (length (inputs self)) 10)
                            (* (box-n-outs self) 10))))
                   (round (max th (if (is-persistant (reference self)) 36 28))))))

(defmethod maximum-size ((self OMBoxAbstraction)) (omp 200 200))

