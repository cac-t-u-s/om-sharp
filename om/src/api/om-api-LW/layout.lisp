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


(export '(
          om-abstract-layout
          om-simple-layout
          om-column-layout
          om-row-layout 
          om-grid-layout 
          om-draw-layout
          om-make-layout 
          om-get-current-view
          om-set-current-view
          om-tab-layout
          om-set-layout-ratios
          om-update-layout
          ) :oa)


(in-package :oa)

(defclass om-abstract-layout (om-graphic-object) ())
 
; 
(defclass om-simple-layout (om-abstract-layout om-interactive-object capi::simple-pinboard-layout) ()
  (:default-initargs :background :transparent))
(defclass om-tab-layout (om-abstract-layout capi::tab-layout) ())
(defclass om-column-layout (om-abstract-layout capi::column-layout) ())
(defclass om-row-layout (om-abstract-layout capi::row-layout) ())
(defclass om-grid-layout (om-abstract-layout capi::grid-layout) ())


;;; a layout subview can be a string, a list (string font), or another item
;;; the only case to handle with LW is the translation of (string font)
(defun layout-view-process (item)
  (if (and (listp item) (stringp (car item)))
      (list :title (car item) :title-font (cadr item) 
            :title-args '(:visible-min-height 20))
    item))
          
(defun make-xy-args (class arg val)
  (if (subtypep class 'om-grid-layout)
      (let ((x-arg (intern (concatenate 'string "X-" (string-upcase arg)) :keyword))
            (y-arg (intern (concatenate 'string "Y-" (string-upcase arg)) :keyword))
            (valval (if (listp val) val (list val val))))
        (list x-arg (car valval) y-arg (cadr valval)))
    (list arg val)))


(defun om-make-layout (class &rest other-args
                             &key
                             subviews ratios delta dimensions align name position size bg-color scrollbars
                             (selection 0)
                             &allow-other-keys
                             )
  (let ((ll
         (cond ((subtypep class 'om-tab-layout)
                (make-instance class
                               :accepts-focus-p t
                               :items subviews :name name
                               :print-function #'(lambda (pane) (or (capi::capi-object-name pane) "Untitled Tab"))
                               :visible-child-function 'identity
                               :selected-item (nth selection subviews)
                               ))
               (t (apply 'make-instance 
                         (append (list class :name name
                                       ;;:automatic-resize '(:width-ratio 1.0 :height-ratio 1.0)
                                       :description (mapcar 'layout-view-process subviews)
                                       :has-title-column-p nil
                                       :allow-other-keys t
                                       :horizontal-scroll (or (equal scrollbars t) (equal scrollbars :h)) ;;; will work only if inside a simple-layout
                                       :vertical-scroll (or (equal scrollbars t) (equal scrollbars :v))
                                       )
                                 (make-xy-args class :uniform-size-p nil)
                                 (when ratios (make-xy-args class :ratios ratios))
                                 (when delta (make-xy-args class :gap delta))
                                 (when align (make-xy-args class :adjust align))  ;;; APPARENTLY ADJUST DOES NOT WORK.... I think it should not be x and y specific (cf example en bas)
                                 (when dimensions 
                                   (list :columns (if (listp dimensions) (car dimensions) dimensions)
                                         :rows (if (listp dimensions) (cadr dimensions) dimensions)))
                                 (when position (list :default-x (om-point-x position) :default-y (om-point-y position)))
                                 (when size (list :default-width (om-point-x size) :default-height (om-point-y size)))
                                 other-args
                                 )))
               )
         ))
    (when bg-color (om-set-bg-color ll bg-color))
    ll))

(defmethod om-set-layout-ratios ((self om-abstract-layout) ratio-list) nil)
(defmethod om-set-layout-ratios ((self om-column-layout) ratio-list)
  (setf (capi::layout-ratios self) ratio-list))
(defmethod om-set-layout-ratios ((self om-row-layout) ratio-list)
  (setf (capi::layout-ratios self) ratio-list))
(defmethod om-set-layout-ratios ((self om-grid-layout) ratio-list)
  (if (and (listp ratio-list) (listp (car ratio-list)))
      (setf (capi::layout-x-ratios self) (car ratio-list)
            (capi::layout-y-ratios self) (cadr ratio-list))
    (setf (capi::layout-x-ratios self) ratio-list
          (capi::layout-y-ratios self) ratio-list)))


(defmethod om-update-layout ((self om-column-layout))
  (setf (capi::layout-ratios self) (capi::layout-ratios self)))
(defmethod om-update-layout ((self om-row-layout))
  (setf (capi::layout-ratios self) (capi::layout-ratios self)))

(defmethod om-update-layout ((self om-grid-layout))
  (setf (capi::layout-x-ratios self) (capi::layout-y-ratios self)
        (capi::layout-y-ratios self) (capi::layout-y-ratios self)))

(defmethod om-update-layout ((self om-abstract-window))
  (mapc 'om-update-layout (om-subviews self)))

(defmethod om-subviews ((self om-abstract-layout)) (capi:layout-description self))

(defmethod om-view-parent ((self om-abstract-layout)) (element-parent self))

(defmethod om-add-subviews ((self om-abstract-layout) &rest subviews)
  (capi::apply-in-pane-process 
   self
   #'(lambda (layout views)
       (setf (capi::layout-description layout)
             (append (capi::layout-description layout) views))
       (when (car (last subviews))
         (capi::set-pane-focus (car (last subviews))))
       )
   self subviews
   ))

(defmethod om-remove-subviews ((self om-abstract-layout) &rest subviews)
  (capi::apply-in-pane-process 
   self #'(lambda ()
            (let ((layout-desc (capi::layout-description self)))
              (loop for item in subviews do (setf layout-desc (remove item layout-desc :test 'equal)))
              (setf (capi::layout-description self) layout-desc)))
     ))

;need to remove the subviews from contained layout also.
(defmethod om-remove-all-subviews ((self t)) nil)

(defmethod om-remove-all-subviews ((self om-abstract-layout))
  (capi::apply-in-pane-process 
   self #'(lambda ()
            (mapcar 'om-remove-all-subviews (capi::layout-description self))
            (om-set-layout-ratios self nil)
            (setf (capi::layout-description self) nil))))

(defmethod om-substitute-subviews ((self om-abstract-layout) old new)
  (capi::apply-in-pane-process 
   self #'(lambda ()
            (om-remove-all-subviews old)
            (setf (capi::layout-description self)
                  (substitute new old (capi::layout-description self) :test 'equal)))
   ))

(defmethod om-invalidate-view ((self om-abstract-layout))
  (mapc 'om-invalidate-view (capi::layout-description self)))

;;;================================
;;; SPECIAL FOR TAB-LAYOUT
;;;================================

(defmethod om-get-current-view ((self om-tab-layout)) 
  (capi::tab-layout-visible-child self))

(defmethod om-set-current-view ((self om-tab-layout) view) 
  (let ((num (capi::search-for-item self view)))
    (when num
      (setf (capi::choice-selection self) num))))

(defmethod om-subviews ((self om-tab-layout)) 
  (loop for i from 0 to (1- (length (capi::collection-items self))) collect
        (capi::get-collection-item self i)))

(defmethod om-remove-subviews ((self om-tab-layout) &rest subviews)
  (capi::apply-in-pane-process 
   self #'(lambda ()
            (setf (capi::collection-items self)
                  (loop for i from 0 to (1- (length (capi::collection-items self))) 
                        unless (find (capi::get-collection-item self i) subviews)
                        collect (capi::get-collection-item self i)))
            )))

(defmethod om-remove-all-subviews ((self om-tab-layout))
  (capi::apply-in-pane-process 
   self #'(lambda ()
            (mapcar 'om-remove-all-subviews (capi::layout-description self))
            (dotimes (i (length (capi::collection-items self))) 
              (om-remove-all-subviews (capi::get-collection-item self i)))
            (setf (capi::layout-description self) nil)
            (setf (capi::collection-items self) nil)
            )))

(defmethod om-substitute-subviews ((self om-tab-layout) old new)
  (capi::apply-in-pane-process 
   self #'(lambda ()
            (om-remove-all-subviews old)
            (setf (capi::collection-items self)
                  (loop for i from 0 to (1- (length (capi::collection-items self))) 
                        collect (if (equal (capi::get-collection-item self i) old) new
                                  (capi::get-collection-item self i)))
                  )
            )))

(defmethod om-add-subviews ((self om-tab-layout) &rest subviews)
  (capi::apply-in-pane-process 
   self
   #'(lambda () (capi::append-items self subviews))))

;;;================================
;;; LAYOUT HANDLES SUBVIEWS
;;;================================

;(defmethod interface-display :after ((self om-abstract-window)) 
;  (update-for-subviews-changes self t))

;(defmethod update-for-subviews-changes ((self om-abstract-window) &optional (recursive nil))
;  (capi::execute-with-interface self (lambda () (set-layout (pane-layout self))))
;  (when recursive (mapc #'(lambda (view) (if (om-view-p view) (update-for-subviews-changes view t))) (vsubviews self)))
;  (when (pane-layout self) (mapc 'update-po-position (item-subviews (pane-layout self))))
;  )

; temp 
;(defmethod item-subviews ((self t)) nil)

;(defmethod internal-add-subview ((self om-abstract-window) (subview om-graphic-object))
;  (setf (vcontainer subview) (pane-layout self))
;  (setf (vsubviews (pane-layout self)) (append (vsubviews (pane-layout self)) (list subview))))


;(defmethod internal-remove-subview ((self om-abstract-window) (subview om-graphic-object))
;  (setf (vcontainer subview) nil)
;  (setf (vsubviews (pane-layout self)) (remove subview (vsubviews (pane-layout self))))
  ; (setf (element-parent subview) nil)
;  )

(defmethod om-subviews ((self om-abstract-window)) 
    (om-subviews (capi::pane-layout self)))

(defmethod om-add-subviews ((self om-abstract-window) &rest subviews)
  (apply 'om-add-subviews (cons (capi::pane-layout self) subviews))
  ;(setf (capi::layout-description (capi::pane-layout self)) subviews)
  )

(defmethod om-remove-subviews ((self om-abstract-window) &rest subviews)
  (apply 'om-remove-subviews (cons (capi::pane-layout self) subviews)))

(defmethod om-remove-all-subviews ((self om-abstract-window))
  (apply 'om-remove-all-subviews (list (capi::pane-layout self))))

(defmethod om-substitute-subviews ((self om-abstract-window) old new)
  (om-substitute-subviews (capi::pane-layout self) old new))