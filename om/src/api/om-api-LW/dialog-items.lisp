;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;=========================================================================
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed; in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Authors: J. Bresson, C. Agon
;=========================================================================

;=========================================================================
; DIALOG ITEMS CLASSES AND FUNCTIONS
;=========================================================================

(in-package :om-api)
;;;========
;;; export :
;;;========
(export '(
          om-make-di
          om-dialog-item-act
          om-dialog-item-action
          om-dialog-item-action-function
          om-set-dialog-item-action-function
                
          om-dialog-item-text
          om-set-dialog-item-text
          om-enable-dialog-item
          om-dialog-item-enabled
                
          om-simple-text
          om-multi-text
          om-custom-edit-text
          om-editable-text
          om-set-text-focus
          om-set-text-completion
          om-complete-text
          om-copy-command
          om-cut-command
          om-paste-command
          om-select-all-command

          om-end-text-edit
          om-text-edit-view
          om-scroll-text-edit-view
          om-make-edit-view
                
          om-button
          om-check-box
          om-radio-button
                
          om-checked-p
          om-set-check-box
                
          om-single-item-list
          om-multi-item-list
          om-popup-list
          om-set-item-list
          om-get-item-list
          om-get-selected-item-index
          om-set-selected-item-index
          om-select-item-index
          om-unselect-item-index
          om-get-selected-item
          om-set-selected-item
                
          om-slider
          om-slider-value
          om-slider-increment
          om-set-slider-value
          om-get-slider-range
          om-get-slider-orientation

          om-simple-text
          om-list-item
          om-sort-list-by
          om-multicol-item-list
          ) :om-api)

;;;=====================
;;;ABSTRACT
;;;=====================



(defclass om-standard-dialog-item (om-graphic-object) 
  ((di-action :accessor di-action :initform nil :initarg :di-action :documentation "the main dialog-item action"))
  (:default-initargs 
   :visible-border :default
   :callback-type :item 
   :scroll-if-not-visible-p nil
   ))

;;;==========
;;; GENERAL API CALLS
;;;==========

(defmethod om-subviews ((self om-standard-dialog-item)) nil)

(defmethod om-get-view ((self om-standard-dialog-item)) self)

(defmethod om-invalidate-view ((self om-standard-dialog-item))
  (capi::redisplay self))

;;;==========
;;; ACTION
;;;==========

(defmacro om-dialog-item-act (var &body body)
  `#'(lambda (,var) 
       (handler-bind 
           ((error #'(lambda (err)
                       (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                       (abort err))))
         ,@body)))

(defmethod om-dialog-item-action ((self om-standard-dialog-item))  
  (when (di-action self)
    (funcall (di-action self) self)))

(defmethod om-set-dialog-item-action-function ((self om-standard-dialog-item) action)
   (setf (di-action self) action))

(defmethod om-dialog-item-action-function ((self om-standard-dialog-item))
   (di-action self))

(defmethod om-enable-dialog-item ((self t) t-or-nil) nil)
(defmethod om-dialog-item-enabled ((self t)) t)

;;;=====================================
;;; SPATIAL ATTRIBUTES (SIZE, POSITION)
;;;=====================================

(defmethod om-dialog-item-text ((self om-standard-dialog-item))
  (capi::item-text self))

(defmethod om-set-dialog-item-text ((self om-standard-dialog-item) text)
  (setf (capi::item-text self) text))


(defmethod om-create-callback ((self om-standard-dialog-item))
  (set-hint-table self (list :default-x (vx self) :default-y (vy self) 
                             :default-width (vw self) :defalut-height (vh self))))

(defmethod om-view-position ((self om-standard-dialog-item)) 
   (if  (capi::interface-visible-p self) 
       (let ((point (multiple-value-list (capi::pinboard-pane-position self))))
         (om-make-point (first point) (second point)))
     (om-make-point (vx self) (vy self))))

(defmethod om-set-view-position ((self om-standard-dialog-item) pos-point) 
  (capi::apply-in-pane-process self 
                         (lambda () 
                           (setf (capi::pinboard-pane-position self) 
                                 (values (om-point-x pos-point) (om-point-y pos-point)))
                           ))
  (setf (vx self) (om-point-x pos-point) 
        (vy self) (om-point-y pos-point)))

(defmethod om-view-size ((self om-standard-dialog-item))
  (if  (capi::interface-visible-p self) 
      (let ((point (multiple-value-list (capi::pinboard-pane-size self))))
        (om-make-point (first point) (second point)))
    (om-make-point (vw self) (vh self))))

(defmethod om-set-view-size ((self om-standard-dialog-item) size-point)
  (setf (vw self) (om-point-x size-point) 
        (vh self) (om-point-y size-point))
  (when (capi::interface-visible-p self)
    (capi::apply-in-pane-process self 
                           (lambda () 
                             (capi::set-hint-table self (list :default-width (om-point-x size-point) :defalut-height (om-point-y size-point)
                                                        :visible-min-width (om-point-x size-point) :visible-min-height (om-point-y size-point)
                                                        ;:internal-min-width (om-point-x size-point) :internal-min-height (om-point-y size-point)
                                                        ;:internal-max-width (om-point-x size-point) :internal-max-height (om-point-y size-point)
                                                        :visible-max-width (om-point-x size-point) :visible-max-height (om-point-y size-point)
                                                        ))
                             (when (and (numberp (om-point-x size-point))
                                        (numberp (om-point-y size-point)))
                               (setf (capi::pinboard-pane-size self) (values (om-point-x size-point) (om-point-y size-point))))
                             (setf (capi::pinboard-pane-position self) (values (vx self) (vy self)))
                             ))
    ;(om-set-view-position self (om-make-point (vx self) (vy self)))
    )
  (di-after-settings self))


;;;=====================================
; CONSTRUCTOR
;;;=====================================

(defun dialog-item-scrollbar-h (scroll)
  (or (equal scroll :h) (equal scroll t)))
 
(defun dialog-item-scrollbar-v (scroll)
  (or (equal scroll :v) (equal scroll t)))

(defmethod special-bg ((self t)) nil)

(defmethod di-after-settings ((self t)) nil)

(defun om-make-di (class  &rest attributes &key
                          position size (text "")
                          container font bg-color fg-color (enable t) print
                          (checked-p nil) cancel default focus range items
                          sort-styles
                          (scrollbars nil) selection
                          (direction :horizontal) (tick-side :default) (value 0) (radio-button-cluster nil)
                          di-action edit-action begin-edit-action completion resizable
                          image
                          &allow-other-keys)
  (let* (;(wi (or (and size (om-point-x size)) 20)) 
         (wi (if size (om-point-x size) 20)) 
         (hi (if size (om-point-y size) 16))
         (shift #+macosx -5 #-macosx 0)
         (x (and position (+ (om-point-x position) shift)))
         (y (and position (+ (om-point-y position) shift))))
    (multiple-value-bind (w h) (adjust-size class wi hi) 
    (let ((di (apply 'make-instance 
                   (append (list class
                           ;:x x :y y
                           :default-x x :default-y y
                           ;:external-min-width (max w 30)
                           ;:external-max-width w
                           :initial-constraints (list :visible-min-width w :visible-min-height h)
                           :visible-min-width w
                           :visible-max-width (if (or (equal resizable t) (equal resizable :w)) nil w)
                           ;:internal-min-width w
                           ;:internal-max-width w
                           
                           :external-min-height h
                           :visible-min-height h                       
                           :visible-max-height (if (or (equal resizable t) (equal resizable :h)) nil h)
                           ;:internal-min-height h
                           ;:internal-max-height (print h)
                                                  
                           :text text
                           :font font
                           :enabled enable
                           :background (or (and (om-color-p bg-color) (omcolor-c bg-color)) :transparent)
                           :foreground (and (om-color-p fg-color) (omcolor-c fg-color))
                           :color-function (when (or (functionp fg-color)
                                                    (and (symbolp fg-color) (fboundp fg-color)))
                                             #'(lambda (list-panel item state)
                                                 (let ((col (funcall fg-color item)))
                                                   (and col (omcolor-c col)))))
                           :sort-descriptions (convert-sort-styles sort-styles)
                           :selected checked-p
                           :default-p default
                           :cancel-p cancel
                           :items items
                           :start (or (first range) 0) :end (or (second range) 100)
                           :orientation direction
                           :start-point :default
                           :slug-start value
                           :internal-border 0
                           :title-adjust t
                           :accepts-focus-p enable
                           :horizontal-scroll (dialog-item-scrollbar-h scrollbars)
                           :vertical-scroll (dialog-item-scrollbar-v scrollbars)
                           
                           :visible-border t

                           :di-action di-action 
                           
                           ;;; only for text-edit
                           :allows-newline-p nil
                           :edit-action edit-action :begin-edit-action begin-edit-action
                           :in-place-completion-function 
                           (when completion 
                             #'(lambda (item str) 
                                 (or 
                                  (funcall completion str)
                                  (progn (capi::beep-pane) :destroy))))
                           :allow-other-keys t)
                   attributes))))
    
    (when print 
      (setf (capi::collection-print-function di) print))
    
    (when selection
      (om-set-selected-item di selection))
    
    (when (or bg-color (special-bg di))
      (om-set-bg-color di (or bg-color (special-bg di))))
    
    (when position 
      (setf (vx di) x ; (om-point-x position)
            (vy di) y ; (om-point-y position)
            ))
    (when size 
      (setf (vw di) w ; (om-point-x size) 
            (vh di) h ;(om-point-y size)
            ))
    
    (when container (om-add-subviews container di))
    
    (when font (om-set-font di font))
   
    (di-after-settings di)
    
    (if focus (capi::set-pane-focus di))

    (when completion (om-complete-text di))
    
    di))))

(defun adjust-size (class w h)
  #+linux (values (or (and (numberp w) (round w 3/4)) w)
		  (or (and (numberp h) (round h 3/4)) h))
  #-linux (if (subtypep class 'om-editable-text)
	      #+windows (values (- w 10) (- h 10))
	      #-windows (values w h)
	      (values w h))
  )




    

;=============================
; SIMPLE TEXT
;=============================

; (capi::contain (make-instance 'capi::title-pane :text "hello" :visible-border :outline))

(defclass om-simple-text (om-standard-dialog-item capi::title-pane) ())

(defmethod om-set-dialog-item-text ((self om-simple-text) text)
  (setf (capi::title-pane-text self) text))

(defmethod om-dialog-item-text ((self om-simple-text))
 (capi::title-pane-text self))

;=================
; TEXT MULTI-LINES
;=================

(defclass om-multi-text (om-standard-dialog-item capi::display-pane) ()
  (:default-initargs :visible-border nil
   :accepts-focus-p nil
   :callback 'om-dialog-item-action))

(defmethod om-dialog-item-text ((self om-multi-text))
 (capi::display-pane-text self))

(defmethod om-set-dialog-item-text ((self om-multi-text) text)
  (capi:apply-in-pane-process self #'(setf capi:display-pane-text) text self))

(defmethod om-enable-dialog-item ((self om-multi-text) t-or-nil)
  (setf (capi::simple-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-multi-text)) 
  (capi::simple-pane-enabled self))

(defmethod om-copy-command ((self om-multi-text))
  (capi::set-clipboard self (om-dialog-item-text self)))


;=============================
; TEXT EDIT CUSTOM
;=============================

;; cursor = nil (no edit), int (pos in text) or list (b e) for selection.
(defclass om-custom-edit-text (om-standard-dialog-item capi::title-pane) 
  ((cursor-pos :accessor cursor-pos :initform nil)))

;;; todo
;;; problem: completion

;=============================
; TEXT EDIT SYSTEM
;=============================

(defclass om-editable-text (om-standard-dialog-item capi::text-input-pane) 
  ((begin-edit-action :accessor begin-edit-action :initarg :begin-edit-action :initform nil :documentation "called when the text edit starts")
   (edit-action :accessor edit-action :initarg :edit-action :initform nil :documentation "called when the text is edited"))
  (:default-initargs
   :visible-border #+windows t #-windows nil
   :navigation-callback 'text-edit-special-action 
   :callback 'text-edit-validate-action 
   :change-callback 'text-edit-changed-action ; :change-callback-type '(:element :data)
   :editing-callback 'text-edited-action   ;;; EDITING START-STOP
   ))


(defmethod om-text-edit-begin-action ((self om-editable-text)) 
  (when (begin-edit-action self)
    (funcall (begin-edit-action self) self)))

(defmethod om-text-edit-action ((self om-editable-text)) 
  (if (edit-action self)
    (funcall (edit-action self) self)))

;;; prevents further action calls 
(defmethod om-end-text-edit ((self om-editable-text)) 
  (setf (edit-action self) nil
        (di-action self) nil))


(defmethod om-view-click-handler ((self om-editable-text) position) nil)

(defmethod special-bg ((self om-editable-text)) (om-def-color :light-gray))


(defun text-edit-validate-action (self)
  (om-dialog-item-action self))

;;; :tab-forward :tab-backward :return 
;;; :shift-return :enter :shift-enter
(defun text-edit-special-action (self action)
  (cond ((equal action :enter) 
         (om-dialog-item-action self))
        ((equal action :return) (if (capi::text-input-allows-newline-p self)
                                    (let ((rec (capi::clipboard self)))
                                      (capi::set-clipboard self (string #\Newline))
                                      (capi::text-input-pane-paste self)
                                      (capi::set-clipboard self rec))
                                  (om-dialog-item-action self)
                                  ))
        ((equal action :tab-forward) 
         (om-view-key-handler self :om-key-tab))
        ))

(defun text-edited-action (self action)
  (case action 
    (:start (om-text-edit-begin-action self))
    (:end (om-dialog-item-action self))
    ))
 
(defun text-edit-changed-action (text self win position)
  (om-text-edit-action self)
  ;(unless (or (string-equal text "") (> position (length text)))
  ;  (om-view-key-handler self (elt text (max 0 (- position 1)))))
  )


(defmethod om-dialog-item-text ((self om-editable-text))
 (capi::text-input-pane-text self))

(defmethod om-set-dialog-item-text ((self om-editable-text) text)
  (capi::apply-in-pane-process 
   self
   #'(lambda () 
       (setf (capi::text-input-pane-text self) text)
       ;(setf (capi:text-input-pane-caret-position self)
       ;      (length (capi:text-input-pane-text self)))
   )))

(defmethod om-enable-dialog-item ((self om-editable-text) t-or-nil)
  (setf (capi::text-input-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-editable-text)) 
  (capi::text-input-pane-enabled self))

(defmethod om-copy-command ((self om-editable-text))
  ;(capi::set-clipboard self (om-dialog-item-text self))
  (capi::text-input-pane-copy self))

;(defmethod om-paste-command ((self om-editable-text))
;  (let ((pos (capi::text-input-pane-selection self))
;        (pasted (length (capi::clipboard self))))
;    (if (capi::text-input-allows-newline-p self)
;        (capi::text-input-pane-paste self)      
;     (om-set-dialog-item-text self (remove #\Newline (capi::clipboard self))))
;    (capi::set-text-input-pane-selection self (+ pos pasted) (+ pos pasted))))

(defmethod om-paste-command ((self om-editable-text))
  (let ((pos (capi::text-input-pane-selection self))
        (txt (capi::clipboard self)))
    (unless (capi::text-input-allows-newline-p self)   
      (capi::set-clipboard self (substitute #\Space #\Newline txt)))
    (capi::text-input-pane-paste self)
    (capi::set-clipboard self txt)
    (capi::set-text-input-pane-selection self (+ pos (length txt)) (+ pos (length txt)))
    ))

(defmethod om-cut-command ((self om-editable-text))
  (capi::text-input-pane-cut self))

(defmethod om-select-all-command ((self om-editable-text))
  (capi::set-text-input-pane-selection self 0 (length (capi::text-input-pane-text self))))

(defmethod om-set-text-focus ((self om-editable-text) &optional select-contents)
  (om-set-focus self)
  (if select-contents
      (capi::set-text-input-pane-selection self 0 (length (capi::text-input-pane-text self)))
    (capi::set-text-input-pane-selection self 0 0)))

(defmethod om-set-text-completion ((self om-editable-text) completion-fun)
  (capi::apply-in-pane-process 
   self
   #'(lambda () (setf (capi::text-input-pane-completion-function self) completion-fun)
       (capi::text-input-pane-complete-text self)
       )))

(defmethod om-complete-text ((self om-editable-text))
  (when (capi::text-input-pane-completion-function self)
    (capi::apply-in-pane-process 
     self
     #'capi::text-input-pane-in-place-complete self)))


;=================
; EDIT MULTI-LINES

(defclass om-text-edit-view (om-editable-text capi::multi-line-text-input-pane) ()
  (:default-initargs 
   :external-min-height nil
   :external-max-height nil))

;(defmethod di-set-focus ((self om-text-edit-view)) 
;  (capi::set-pane-focus self)
;  (capi::set-text-input-pane-selection self 0 (length (capi::text-input-pane-text self))))

(defmethod om-paste-command ((self om-text-edit-view))
  (capi::text-input-pane-paste self))


;===============
; BUTTON
;===============

;;; these will apply to all the following subclasses of capi::button
(defmethod om-enable-dialog-item ((self capi::button) t-or-nil)
  (setf (capi::button-enabled self) t-or-nil))
(defmethod om-dialog-item-enabled ((self capi::button)) 
  (capi::button-enabled self))


(defclass om-button (om-standard-dialog-item capi::push-button) ()
  (:default-initargs :callback 'om-dialog-item-action))

;===============
; CHECK-BOX
;===============

(defclass om-check-box (om-standard-dialog-item capi::check-button) ()
  (:default-initargs 
   #+win32 :accepts-focus-p #+win32 nil
   :selection-callback 'om-dialog-item-action
   :retract-callback  'om-dialog-item-action))

(defmethod om-checked-p ((self om-check-box)) (capi::button-selected self))

(defmethod om-set-check-box  ((self om-check-box) check?)
  (capi:apply-in-pane-process 
   self 
   #'(lambda () (setf (capi::button-selected self) check?))))


;--------om-radio-button

(defclass om-radio-button (om-standard-dialog-item capi::radio-button) 
  ((radio-button-cluster :initarg :radio-button-cluster :initform nil :accessor radio-button-cluster))
  (:default-initargs :callback 'om-dialog-item-action))

(defmethod om-radio-button-p ((self om-radio-button))  t)
(defmethod om-radio-button-p ((self t))  nil)

(defmethod om-dialog-item-action ((self om-radio-button))  
  (let ((container (capi::element-parent self)))
    (when container
      (let ((elems (capi::pane-children container)))
        (loop for item in elems do
              (when (and (om-radio-button-p item) (not (equal item self))
                         (equal (radio-button-cluster item) (capi::radio-button-cluster self)))
                (om-set-check-box item nil)))))
  (call-next-method)))

(defmethod om-checked-p ((self om-radio-button)) (button-selected self))

(defmethod om-set-check-box  ((self om-radio-button) check?)
  (setf (capi::button-selected self) check?))



;--------om-item-list abstract
(defclass om-item-list (om-standard-dialog-item capi::list-panel) ()
  (:default-initargs 
   :callback-type '(:collection)
   :selection-callback 'om-dialog-item-action
   :action-callback 'double-click-on-list
   ;:vertical-scroll t
   :test-function 'string-equal))



(defclass om-list-item (capi::item) ())

(defmethod di-after-settings ((self om-item-list)) 
  (if (remove nil (map 'list 'stringp (capi::collection-items self)))
      (setf (capi::collection-test-function self) 'string-equal)
    (setf (capi::collection-test-function self) 'equal)
    ))

(defmethod special-bg ((self om-item-list)) (om-def-color :white))

(defun vector-col-to-list (v)
  (loop for i from 0 to (- (length v) 1) collect (elt v i)))

(defmethod om-set-item-list ((self om-item-list) names)
  (setf (capi::collection-items self) names))

(defmethod om-get-item-list ((self om-item-list))
  (vector-col-to-list (capi::collection-items self )))

(defmethod om-get-selected-item-index ((self om-item-list))
  (capi::choice-selection self))

(defmethod om-set-selected-item-index ((self om-item-list) ind)
  (setf (capi::choice-selection self) ind))

(defmethod double-click-on-list ((self om-item-list)) nil)

(defun convert-sort-styles (styles)
  (loop for style in styles collect 
        (capi:make-sorting-description 
         :type (car style)
         :sort (nth (1+ (position :sort (cadr style))) (cadr style))
         :reverse-sort (nth (1+ (position :reverse (cadr style))) (cadr style)))))

(defmethod om-sort-list-by ((self om-item-list) style)
  (capi:sorted-object-sort-by self style)
  (setf (capi::collection-items self)
        (capi::sort-object-items-by self (capi::collection-items self))))

(defmethod om-invalidate-view ((self om-item-list))
  ;(capi:map-collection-items self 
  (map nil #'(lambda (item)
               (capi::redisplay-collection-item self item))
       (capi::collection-items self)))
  
;--------om-single-item-list  

(defclass om-single-item-list (om-item-list) ()
  (:default-initargs :interaction :single-selection
   :retract-callback 'item-list-unselect))

(defmethod item-list-unselect ((self om-single-item-list))
  (unless (capi::choice-selection self)
    (setf (capi::choice-selection self) 0)
    ))

(defmethod om-select-item-index ((self om-single-item-list) i)
 (setf (capi::choice-selection self) i))

(defmethod om-unselect-item-index ((self om-single-item-list) i)
  (when (and (capi::choice-selection self) (= (capi::choice-selection self) i))
    (setf (capi::choice-selection self) nil)))

(defmethod om-get-selected-item ((self om-single-item-list)) 
  (capi::choice-selected-item self))

(defmethod om-set-selected-item ((self om-single-item-list) item) 
  (setf (capi::choice-selected-item self) item))

(defmethod om-dialog-item-action ((self om-single-item-list)) 
  (call-next-method))


;--------om-multi-item-list
 
(defclass om-multi-item-list (om-item-list) ()
 (:default-initargs 
   :interaction  #+win32 :multiple-selection #-win32 :extended-selection
   :right-click-selection-behavior :clicked/restore/restore
   :extend-callback 'om-dialog-item-action
   :retract-callback 'om-dialog-item-action))

;;; :multiple-selection
(defmethod om-select-item-index ((self om-multi-item-list) i)
  (setf (capi::choice-selection self) (union (capi::choice-selection self) (if (listp i) i (list i)))))

(defmethod om-unselect-item-index ((self om-multi-item-list) i)
  (setf (capi::choice-selection self) (remove i (capi::choice-selection self))))

(defmethod om-get-selected-item ((self om-multi-item-list)) 
  (capi::choice-selected-items self))

(defmethod om-set-selected-item ((self om-multi-item-list) items) 
  (setf (capi::choice-selected-items self) (if (listp items) items (list items))))



;--------multi-column-list

(defclass om-multicol-item-list (om-multi-item-list capi::multi-column-list-panel) ()
  (:default-initargs 
   :callback-type '(:collection)
   :selection-callback 'om-dialog-item-action
   :action-callback 'double-click-on-list
   :interaction  #+win32 :multiple-selection #-win32 :extended-selection
   :right-click-selection-behavior :clicked/restore/restore
   :extend-callback 'om-dialog-item-action
   :retract-callback 'om-dialog-item-action
   :test-function 'string-equal))


;--------om-slider 

(defclass om-slider (om-standard-dialog-item capi::slider) 
  ((increment :initarg :increment :initform 1 :accessor increment))
  (:default-initargs 
   :callback 'om-slider-item-action
   :show-value-p t))
 
(defmethod om-slider-value ((self om-slider))
  (* (round (capi::range-slug-start self) (increment self)) (increment self)))

(defmethod om-slider-increment ((self om-slider))
  (increment self))

(defmethod om-set-slider-value ((self om-slider) value)
   (setf (capi::range-slug-start self) value))

(defmethod om-slider-item-action ((self om-standard-dialog-item) value type)
  (when (di-action self)
    (funcall (di-action self) self)))

(defmethod om-get-slider-range ((self om-slider))
  (list (capi::range-start self) (capi::range-end self)))

(defmethod om-get-slider-orientation ((self om-slider))
  (capi::range-orientation self))

;--------om-popup-list

(defclass om-popup-list (om-standard-dialog-item capi::option-pane) 
  ((value :initform nil :initarg :value :accessor value))
  (:default-initargs 
   :callback-type '(:collection)
   :selection-callback 'om-dialog-item-action
   :test-function 'equal
   :separator-item "-"))   

(defmethod initialize-instance :after ((self om-popup-list) &rest l)
  (when (value self)
    (let ((pos (position (value self) (vector-col-to-list (capi::collection-items self )) 
                         :test (if (stringp (value self)) 'string-equal 'equal))))
      (when pos
        (setf (capi::choice-selection self) pos)))))

(defmethod om-dialog-item-action ((self om-popup-list))  
  (when (di-action self)
    (funcall (di-action self) self)))

;(defmethod set-dialog-item-action-function ((self om-popup-list) f)
;  (when f
;    (loop for item in (menu-items self) do
;          (set-menu-item-action-function item #'(lambda () (funcall f self))))))

(defmethod om-get-item-list ((self om-popup-list)) 
  (vector-col-to-list (capi::collection-items self)))

(defmethod om-set-item-list ((self om-popup-list) names)
  (setf (capi::collection-items self) names))

(defmethod om-enable-dialog-item ((self om-popup-list) t-or-nil)
  (setf (capi::option-pane-enabled self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-popup-list))
  (capi::option-pane-enabled self))

;;; !!!!
(defmethod om-get-selected-item ((self om-popup-list))
  (nth (capi::choice-selection self) (om-get-item-list self)))

(defmethod om-get-selected-item-index ((self om-popup-list))
  (capi::choice-selection self))

(defmethod om-set-selected-item-index ((self om-popup-list) pos)
  (setf (capi::choice-selection self) pos))

(defmethod om-set-selected-item ((self om-popup-list) str)
  (let ((pos (position str (om-get-item-list self) :test 'equal)))
    (when pos (setf (capi::choice-selection self) pos))))

(defmethod om-select-item-index ((self om-popup-list) index)
 (setf (capi::choice-selection self) index))
 


