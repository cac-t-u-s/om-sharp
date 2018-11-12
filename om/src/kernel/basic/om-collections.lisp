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

;;;===========================
;;; COLLECTION = A SET OF OBJECTS OF A SAME TYPE
;;;===========================

(defclass* collection (named-object)
  ((obj-type :accessor obj-type :initform nil)
   (obj-list :initarg :obj-list :accessor obj-list :initform nil)))

(defmethod get-properties-list ((self collection))
  '(("COLLECTION attibutes"
     (:name "Name" :text name))))

;;; the collection box has same additional attributes as the object it contains (if any)
(defmethod additional-box-attributes ((self collection))
  (additional-box-attributes (car (obj-list self))))

(defmethod homogenize-collection (model list) nil)
                                        
(defmethod om-init-instance ((self collection) &optional initargs)
  (setf (obj-list self)
        (if (listp (obj-list self)) 
            (om-copy (obj-list self))
          (list (om-copy (obj-list self)))))
  (when (obj-list self)
     ;;; check if all items are of the same type
     (if (list-typep (obj-list self) (type-of (car (obj-list self))))
        (progn 
          (setf (obj-type self) (type-of (car (obj-list self))))
          (homogenize-collection (car (obj-list self)) (obj-list self)))
      (progn
        (om-beep-msg "WARNING: ELEMENTS IN COLLECTION ARE NOT OF THE SAME TYPE!!")
        (setf (obj-type self) nil))))
  self)

;;;===========================
;;; BOX
;;;===========================

(defmethod special-box-p ((name (eql 'collection))) t)

(defmethod omNG-make-special-box ((reference (eql 'collection)) pos &optional init-args)
  (let ((type (and init-args (find-class (car init-args) nil)))
        (val (make-instance 'collection)))
    (when type (setf (obj-type val) (class-name type)))
    (let ((box (omNG-make-new-boxcall (find-class 'collection) pos val)))
      (setf (name box) "...")
      (setf (value (car (inputs box))) (obj-type val))
      box)))

(defmethod objfromobjs ((model symbol) (target collection))
  (if (find-class model nil)
      (setf (obj-type target) model)
    (om-beep-msg "WARNING: class not found for collection: ~S" model))
  target)

(defmethod draw-type-of-object ((object collection))
  (string+ (string-upcase (type-of object)) " OF " (string-upcase (obj-type object))))

(defmethod get-object-type-name ((object collection)) 
  (and (obj-type object)
       (string+ "COLLECTION OF " (string-upcase (obj-type object)) 
                "s (" (number-to-string (length (obj-list object))) ")")))
  
(defmethod object-box-label ((object collection))
  (string+ (string-upcase (type-of object)) " of "
           (number-to-string (length (obj-list object))) " " 
           (string-upcase (obj-type object)) 
           (if (> (length (obj-list object)) 1) "s" "")
           ))


(defmethod display-modes-for-object ((self collection)) '(:hidden :text :mini-view))

(defmethod get-cache-display-for-draw ((object collection)) 
  (when (subtypep (obj-type object) 'BPF)
    (list (nice-bpf-range (obj-list object)))))

(defmethod draw-mini-view ((self collection) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (if (subtypep (obj-type self) 'BPF)
        (let ((ranges (car display-cache)))
          (loop for o in (obj-list self) do 
                (draw-bpf-points-in-rect (point-pairs o)
                                         (color o) 
                                         ranges
                                         x (+ y 10) w (- h 20)
                                         :lines))
          (om-with-font  (om-def-font :font1 :size 8)
            (om-draw-string (+ x 10) (+ y (- h 4)) (number-to-string (nth 0 ranges)))
            (om-draw-string (+ x (- w (om-string-size (number-to-string (nth 1 ranges)) (om-def-font :font1 :size 8)) 4))
                            (+ y (- h 4)) 
                            (number-to-string (nth 1 ranges)))
            (om-draw-string x (+ y (- h 14)) (number-to-string (nth 2 ranges)))
            (om-draw-string x (+ y 10) (number-to-string (nth 3 ranges)))
            ))
      (loop for o in (obj-list self) do 
              (set-cache-display box o)
              (draw-mini-view o box x y w h time)))))



;;;===========================
;;; EDITOR
;;;===========================

(defclass collection-editor (OMEditor)
  ((internal-editor :accessor internal-editor :initform nil)
   (current :accessor current :initform 0)))
 
(defmethod object-default-edition-params ((self collection))
  '((:show-all t)))

(defmethod object-has-editor ((self collection)) t)
(defmethod get-editor-class ((self collection)) 'collection-editor)

(defmethod object-value ((self collection-editor))
  (nth (current self) (obj-list (get-value-for-editor (object self)))))

(defmethod get-obj-to-play ((self collection-editor)) (object-value self))

(defmethod editor-play-state ((self collection-editor))
  (editor-play-state (internal-editor self)))

(defmethod editor-close ((self collection-editor)) 
  (editor-close (internal-editor self))
  (call-next-method))


;;;==================================
;multidisplay API
(defclass multi-display-editor-mixin ()
  ((multi-display-p :accessor multi-display-p :initarg :multi-display-p :initform nil)
   (multi-obj-list :accessor multi-obj-list :initform nil)))

(defmethod handle-multi-display ((self t)) nil)
(defmethod handle-multi-display ((self multi-display-editor-mixin)) t)

(defmethod enable-multi-display ((self t) obj-list) nil)
(defmethod enable-multi-display ((self multi-display-editor-mixin) obj-list) 
  (setf (multi-display-p self) t (multi-obj-list self) obj-list))

(defmethod disable-multi-display ((self t)) nil)
(defmethod disable-multi-display ((self multi-display-editor-mixin))
  (setf (multi-display-p self) nil)
  (setf (multi-obj-list self)  nil))

(defmethod update-multi-display ((editor collection-editor) t-or-nil)
    
  (if t-or-nil
      (enable-multi-display (internal-editor editor) (obj-list (get-value-for-editor (object editor))))
    (disable-multi-display (internal-editor editor)))
  
  (update-to-editor (internal-editor editor) editor)
  (editor-invalidate-views (internal-editor editor))
  )

;;;==================================

(defmethod init-editor ((editor collection-editor)) 
  (let* ((collection (get-value-for-editor (object editor)))
         (current-object (and (obj-type collection) (nth (current editor) (obj-list collection))))
         (abs-container (make-instance 'OMAbstractContainer :contents current-object)))
    (setf (internal-editor editor) 
          (make-instance (get-editor-class current-object)
                         :container-editor editor 
                         :object abs-container)
          )
    (setf (edition-params abs-container) (edition-params (object editor))) ;;; will share the same list in principle
    (init-editor (internal-editor editor))
    ))

(defmethod init-editor-window ((editor collection-editor))
  (call-next-method)
  (init-editor-window (internal-editor editor))
  (when (editor-get-edit-param editor :show-all)
    (update-multi-display editor t)))


(defmethod make-editor-window-contents ((editor collection-editor))
  (let* ((collection (get-value-for-editor (object editor)))
         (text (format-current-text editor))
         (current-text (om-make-graphic-object 'om-item-text :size (omp (om-string-size text (om-def-font :font3b)) 16) 
                                               :text text :font (om-def-font :font2b)))
         (prev-button (om-make-graphic-object 'om-icon-button 
                                              :size (omp 16 16)
                                              :icon :l-arrow :icon-pushed :l-arrow-pushed :icon-disabled :l-arrow-disabled
                                              :lock-push nil :enabled (> (length (obj-list collection)) 1)
                                              :action #'(lambda (b)
                                                          (declare (ignore b))
                                                          (set-current-previous editor)
                                                          )))
         (next-button (om-make-graphic-object 'om-icon-button 
                                              :size (omp 16 16)
                                              :icon :r-arrow :icon-pushed :r-arrow-pushed :icon-disabled :r-arrow-disabled
                                              :lock-push nil :enabled (> (length (obj-list collection)) 1)
                                              :action #'(lambda (b)
                                                          (declare (ignore b))
                                                          (set-current-next editor)
                                                          )))
         (-button (om-make-graphic-object 'om-icon-button 
                                          :size (omp 16 16)
                                          :icon :- :icon-pushed :--pushed :icon-disabled :--disabled
                                          :lock-push nil :enabled (obj-list collection)
                                          :action #'(lambda (b)
                                                      (remove-current-object editor)
                                                      (let ((coll (get-value-for-editor (object editor))))
                                                        (when (null (obj-list coll))
                                                          (disable b))
                                                        (when (<= (length (obj-list coll)) 1)
                                                          (disable prev-button) (disable next-button))
                                                        (update-multi-display editor (editor-get-edit-param editor :show-all)) 
                                                        ))
                                          ))
         (+button (om-make-graphic-object 'om-icon-button 
                                          :size (omp 16 16)
                                          :icon :+ :icon-pushed :+-pushed :icon-disabled :+-disabled
                                          :lock-push nil :enabled (obj-type (get-value-for-editor (object editor)))
                                          :action #'(lambda (b)
                                                      (declare (ignore b))
                                                      (add-new-object editor)
                                                      (let ((coll (get-value-for-editor (object editor))))
                                                        (enable -button)  ;; in case it was disabled..
                                                        (when (> (length (obj-list coll)) 1)
                                                          (enable prev-button) (enable next-button)))
                                                      (update-multi-display editor (editor-get-edit-param editor :show-all))
                                                      )))
         )
    (set-g-component editor :current-text current-text)
    (om-make-layout 
     'om-column-layout 
     :ratios '(1 99)
     :subviews 
     (list 
      (om-make-layout 
       'om-row-layout 
       :subviews 
       (list (om-make-layout 
              'om-row-layout :delta 0 :align :bottom
              :subviews (list prev-button next-button 
                              (om-make-graphic-object 'om-item-view :size (omp 20 20))
                              (when (handle-multi-display (internal-editor editor))
                                (om-make-di 'om-check-box :text " Show All" :size (omp 80 16) :font (om-def-font :font2)
                                            :checked-p (editor-get-edit-param editor :show-all) :focus nil :default nil
                                            :di-action #'(lambda (item) 
                                                           (editor-set-edit-param editor :show-all (om-checked-p item))
                                                           (update-multi-display editor (om-checked-p item)))
                                            ))
                              nil
                              current-text))
             nil
             (om-make-layout 'om-row-layout :delta 0 
                             :subviews 
                             (list +button -button))
             ))
      (if (object-value (internal-editor editor))
          (setf (main-view (internal-editor editor)) 
                (make-editor-window-contents (internal-editor editor))))
      ))
    ))


(defmethod set-window-contents ((editor collection-editor))
  (when (window editor) 
    (om-remove-subviews (window editor) (main-view editor))
    (om-add-subviews (window editor) 
                     (setf (main-view editor)
                           (make-editor-window-contents editor)))
    ))

(defmethod update-to-editor ((editor collection-editor) (from t))
  (let ((collection (get-value-for-editor (object editor))))
    (unless (or (null (obj-type collection))
                (equal (type-of (internal-editor editor))
                       (get-editor-class (nth (current editor) (obj-list collection)))))
      (editor-close (internal-editor editor))
      (init-editor editor)
      (setf (current editor) 0)
      (set-window-contents editor))
    (set-current-text editor)
    (update-to-editor (internal-editor editor) from)
    )
  (call-next-method))

(defmethod format-current-text ((editor collection-editor))
  (let ((collection (get-value-for-editor (object editor))))
    (if (obj-list collection)
        (format nil "Current ~A: ~D/~D" ;; [~A] 
                (string-upcase (obj-type collection))
                (1+ (current editor)) (length (obj-list collection))
                ;(name (nth (current editor) (obj-list collection)))
                )
      "[empty collection]")))

(defmethod editor-invalidate-views ((editor collection-editor))
  (when (internal-editor editor)
    (om-invalidate-view (internal-editor editor))))

(defmethod set-current-text ((editor collection-editor))
  (let ((text-component (get-g-component editor :current-text)))
    (when text-component
      (let ((text (format-current-text editor)))
        (om-set-view-size text-component 
                          (omp (+ 20 (om-string-size text (om-get-font text-component))) 16))
        (om-set-text text-component text)))))
      
(defmethod update-collection-editor ((editor collection-editor))
  (set-current-text editor)
  (let ((internal-editor (internal-editor editor)))
    (editor-stop internal-editor)
    (setf (selection internal-editor) nil)
    (let ((abs-container (object internal-editor))) ;; in principle this is an OMAbstractContainer
      (setf (contents abs-container) 
            (nth (current editor) (obj-list (get-value-for-editor (object editor))))))
    (update-default-view internal-editor)
    (update-to-editor internal-editor editor)
    (editor-invalidate-views internal-editor)))
   

(defmethod set-current-nth ((editor collection-editor) n)
  (let ((collection (get-value-for-editor (object editor))))
    (when (obj-list collection)
      (setf (current editor) n))
    (update-collection-editor editor)))

(defmethod set-current-next ((editor collection-editor))
  (let ((collection (get-value-for-editor (object editor))))
    (when (obj-list collection)
      (set-current-nth editor (mod (1+ (current editor)) (length (obj-list collection))))
      )))

(defmethod set-current-previous ((editor collection-editor))
  (let ((collection (get-value-for-editor (object editor))))
    (when (obj-list collection)
      (set-current-nth editor (mod (1- (current editor)) (length (obj-list collection))))
      )))

(defmethod remove-current-object ((editor collection-editor))
  (let ((collection (get-value-for-editor (object editor))))
    ;;; update the obj-list
    (when (obj-list collection)
      (setf (obj-list collection) (remove (nth (current editor) (obj-list collection)) (obj-list collection)))
      (setf (current editor) (max 0 (min (current editor) (1- (length (obj-list collection)))))))  
    ;;; if no more objects...
    (if (null (obj-list collection))
      (progn 
        ;;; close the internal editor
        (editor-close (internal-editor editor))
        (setf (contents (object (internal-editor editor))) nil)
        ;;; rest the (empty) window
        (set-window-contents editor))
      ;;; othewise just update the editor
      (update-collection-editor editor))
    (report-modifications editor)
    ))

(defmethod add-new-object ((editor collection-editor))
  (let* ((collection (get-value-for-editor (object editor)))
         (new? (null (obj-list collection))))
    (setf (obj-list collection)
          (append (obj-list collection)
                  (list (om-init-instance (make-instance (obj-type collection))))))
    (setf (current editor) (1- (length (obj-list collection))))
    (when new? ;;; need to (re)initialize an editor
      (init-editor editor)
      (set-window-contents editor)
      (init-editor-window (internal-editor editor)))
    (update-collection-editor editor)
    (update-multi-display editor (editor-get-edit-param editor :show-all)) ;; will add the new object to te multi-display list
    (report-modifications editor)
    ))


;;;=========================
;;; DISPATCH ACTIONS...
;;;=========================
(defmethod editor-key-action ((editor collection-editor) key)
  ;(print key)
  (cond ((and (om-command-key-p) (equal key :om-key-left))
         (set-current-previous editor)
         (update-collection-editor editor))
        ((and (om-command-key-p) (equal key :om-key-right))
         (set-current-next editor)
         (update-collection-editor editor))
        (t (editor-key-action (internal-editor editor) key))
        ))

(defmethod select-all-command ((self collection-editor))
  #'(lambda () 
      (when (and (internal-editor self) (select-all-command (internal-editor self)))
        (funcall (select-all-command (internal-editor self))))))

