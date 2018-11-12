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

;;;===========================================
;;; OBJECT PROPERTIES
;;;===========================================
;;; PROPERTY LIST FORMAT
;;;  '(("CATEGORY1" 
;;;               (:prop-id "name" type 'slot-name-or-acessor default)
;;;               ...)
;;;    ("CATEGORY2" ...))

(defmethod get-properties-list ((self t)) nil)

(defmethod get-flat-properties-list ((self t)) 
  (loop for cat in (get-properties-list self) append 
        (remove-if-not #'(lambda (prop) (valid-property-p self prop)) (cdr cat) :key 'car)))

(defun get-property-spec (object prop-id)
  (find prop-id (apply 'append (mapcar 'cdr (get-properties-list object)))
        :key 'car :test 'equal))

(defun visible-property (plist prop-id)
  (find prop-id plist
        :key 'cdr
        :test #'(lambda (prop cat)
                  (find prop cat :key 'car :test 'equal))))

(defun hide-property (plist prop-id)
  (loop for cat in plist collect
        (cons (car cat) (remove prop-id (cdr cat) :key 'car 
                                :test #'(lambda (prop elt) (find elt (list! prop) :test 'equal))
                                ))))

(defun hide-properties (plist prop-id-list)
  (if prop-id-list
      (hide-properties 
       (hide-property plist (car prop-id-list))
       (cdr prop-id-list))
    plist))


(defun add-properties (plist category props)
  (if (find category plist :test 'string-equal :key 'car)
      (loop for cat in plist collect
            (if (string-equal (car cat) category)
                (cons (car cat) (append (cdr cat) props))
              cat))
    (append plist
            (list (cons category props)))))

(defun add-properties-list (plist cat-props)
  (loop for elt in cat-props do
        (setf plist (add-properties plist (car elt) (cadr elt))))
  plist)

;(get-property-spec (make-instance 'ombox) :icon)

(defmethod valid-property-p (object prop-id)
  (not (equal (get-property object prop-id :warn nil) :invalid)))

(defun access-value (object accessor &optional (val nil val-supplied-p))
  (if val-supplied-p
      (cond ((and accessor (slot-exists-p object accessor))
             (setf (slot-value object accessor) val))
            ((fboundp accessor)
             (funcall accessor object val))
            (t :invalid))
    (cond 
     ((and accessor (slot-exists-p object accessor))
      (slot-value object accessor))
     ((fboundp accessor) ;; not sure if it applies to the right object !
      (funcall accessor object))
     (t :invalid))))

(defmethod get-property (object prop-id &key (warn t)) 
  (let* ((prop (get-property-spec object prop-id))
         (val (access-value object (nth 3 prop))))
    (if (and (equal val :invalid) warn)
        (om-print (format nil "No accessor for property ~A in object ~A" prop-id object) "OM PROPERTIES"))
    val))

(defmethod set-property (object prop-id val)
  (let* ((prop (get-property-spec object prop-id))
         (slot-name (nth 3 prop))
         (rep (access-value object slot-name val)))
    (when (equal rep :invalid) 
      (format nil "No method to set property ~A in object ~A" prop-id object))
    ;(when (slot-exists-p object slot-name)
    ;  (om-init-instance object (list (list (intern-k slot-name) val))))
    ))


;;; default-value can be given as a list (:pref-module :pref-key)
(defun get-default-value (def)
  (if (consp def)
      (get-pref-value (car def) (cadr def))
    def))

;;;===========================================
;;; DIFFERENT KINDS OF ITEM IN THE INSPECTOR FOR THE PROPERITIES
;;; USED IN EDITORS OR INSPECTOR WINDOWS
;;;===========================================

;;; called after a property is changed
(defmethod update-after-prop-edit ((self t) (object t)) nil)

;;;====================================
;;; DEFAULT (UNSPECIFIED)
;;;====================================

;;; general case = text box (value is READ by Lisp)
(defmethod make-prop-item (type prop-id object &key default update)
  (om-make-view 'click-and-edit-text 
                ;:enabled (valid-property-p object prop-id)
                :text (format nil "~A" (get-property object prop-id))
                :resizable :w
                :bg-color (om-def-color :window)
                :border nil ;(om-def-color :gray)
                :size (om-make-point (list :string (format nil "~A" (get-property object prop-id))) 20)
                :font (om-def-font :font1)
                :after-fun #'(lambda (item)
                               (set-property object prop-id 
                                             (if (string-equal (text item) "") nil
                                               (read-from-string (text item))))
                               (when update (update-after-prop-edit update object))
                               )))

;;;====================================
;;; STRINGS
;;;====================================

(defmethod make-prop-item ((type (eql :string)) prop-id object &key default update)
  (om-make-view 'click-and-edit-text 
                ;:enabled (valid-property-p object prop-id)
                :text (if (get-property object prop-id) (format nil "~A" (get-property object prop-id)) "")
                :resizable :w
                :bg-color (om-def-color :window)
                :border nil ;(om-def-color :gray)
                :size (om-make-point (list :string (format nil "~A" (get-property object prop-id))) 20)
                :font (om-def-font :font1)
                :after-fun #'(lambda (item)
                               (set-property object prop-id 
                                             (if (string-equal (text item) "") nil
                                               (text item)))
                               (when update (update-after-prop-edit update object))
                               )))

;;;====================================
;;; NUMBERS
;;;====================================

(defmethod prop-item-call-function-to-object ((object t) function)
  (funcall function object))

;;; for number default is a list (min-val max-val decimals)
(defmethod make-prop-item ((type (eql :number)) prop-id object &key default update)
  (let ((def (loop for element in (list! default) collect
                   (if (functionp element) (prop-item-call-function-to-object object element)
                     element))))
  (om-make-graphic-object 'numbox 
                          :value (get-property object prop-id)
                          :bg-color (om-def-color :white)
                          :border t
                          :db-click t
                          :decimals (or (caddr def) 0)
                          :size (om-make-point 60 18) 
                          :font (om-def-font :font2)
                          :min-val (or (car def) 0) :max-val (or (cadr def) 10000)
                          :after-fun #'(lambda (item)
                             (set-property object prop-id (get-value item))
                             (when update (update-after-prop-edit update object))
                             ))))


;;; number-or-nil work slightly differently:
;;; the type needs to be intanciated in order to embed a min and max value
;;; t-or-nil allow to specify an unspecified number, typically to get it from the preferences
(defstruct number-or-nil 
  (t-or-nil) (number) 
  (min) (max) (decimals)  ;;; 'meta-attributes' used to instanciate the type
  )

;;; for compatibility with simple numbers...
(defmethod number-? ((self number-or-nil)) (number-or-nil-t-or-nil self))
(defmethod number-number ((self number-or-nil)) (number-or-nil-number self))
(defmethod number-? ((self number)) t)
(defmethod number-number ((self number)) self)
(defmethod number-? ((self t)) nil)
(defmethod number-number ((self t)) nil)

(defmethod make-prop-item ((type number-or-nil) prop-id object &key default update)

  (let* ((def (or default 0))
         (current-value (get-property object prop-id))
         numbox checkbox)

    (setf numbox 
          (om-make-graphic-object 'numbox 
                                  :value (or (number-number current-value)
                                             (get-default-value def))
                                  ;(and (valid-property-p object prop-id)
                                  ;  (if (number-? (get-property object prop-id)) 
                                  ;      (number-number (get-property object prop-id))
                                  ;    (get-default-value def)))
                                  :enabled (number-? current-value)
                                  :bg-color (om-def-color :white)
                                  :border t
                                  :db-click t
                                  :decimals (or (number-or-nil-decimals type) 0)
                                  :size (om-make-point 60 18) 
                                  :resizable nil
                                  :font (om-def-font :font2)
                                  :min-val (or (number-or-nil-min type) 0) 
                                  :max-val (or (number-or-nil-max type) 10000)
                                  :after-fun #'(lambda (item)
                                                 (set-property 
                                                  object prop-id 
                                                  (make-number-or-nil :number (get-value item)
                                                                      :t-or-nil t))
                                                 ;(unless (om-checked-p checkbox)
                                                 ;  (om-set-check-box checkbox t))
                                                 (when update (update-after-prop-edit update object))
                                                 )))

    (setf checkbox 
          (om-make-di 'om-check-box 
                      :checked-p (and (valid-property-p object prop-id)
                                      (number-? current-value))
                      :text ""
                      :resizable nil
                      :size (om-make-point 20 14)
                      :font (om-def-font :font1)
                      :di-action #'(lambda (item)
                                     (enable-numbox numbox (om-checked-p item))
                                     ;;; when there is a default, it gets set when unckecked
                                     (when (and default (null (om-checked-p item)))
                                       (set-value numbox (get-default-value default)))
                                     (set-property  
                                      object prop-id 
                                      (make-number-or-nil 
                                       ; :number (if (om-checked-p item) (get-default-value default) nil)
                                       :number (get-value numbox)
                                       :t-or-nil (om-checked-p item)))
                                     (when update (update-after-prop-edit update object))
                                     )))
    
    (om-make-layout 'om-row-layout
                    :subviews (list checkbox numbox)
                    :delta nil :align :center)
    ))

;;;====================================
;;; BOOL
;;;====================================

(defmethod make-prop-item ((type (eql :bool)) prop-id object &key default update)
  (om-make-di 'om-check-box 
              ;:enable (valid-property-p object prop-id)
              :checked-p (get-property object prop-id) ; (and (valid-property-p object prop-id) (get-property object prop-id))
              :text ""
              :resizable nil
              :size (om-make-point nil 14)
              :font (om-def-font :font1)
              :di-action #'(lambda (item)
                             (set-property object prop-id (om-checked-p item))
                             (when update (update-after-prop-edit update object))
                             )))

;;;====================================
;;; LIST OF CHOICES
;;;====================================

(defmethod make-prop-item ((type cons) prop-id object &key default update)
  (let ((popup (om-make-di 'om-popup-list 
                           :items (remove :default type) 
                           :resizable nil
                           :enable (and (valid-property-p object prop-id) 
                                        (if default (get-property object prop-id) t))
                           :value (or (get-property object prop-id)
                                      (get-default-value default))
                           :size (om-make-point ;(list :string (format nil "~A" (get-property object prop-id))) 
                                  (+ 40 (list-max (mapcar #'(lambda (x) (om-string-size (format nil "~A" x) (om-def-font :font1))) type)))
                                  22)
                           :font (om-def-font :font1)
                           :di-action #'(lambda (item)
                                          (set-property object prop-id (om-get-selected-item item))
                                          (when update (update-after-prop-edit update object))
                                          ))))
    (if (find :default type)
        
        ;;; list-or-nil
        (let ((checkbox (om-make-di 
                        'om-check-box 
                        :checked-p (and (valid-property-p object prop-id)
                                        (get-property object prop-id))
                        :text ""
                        :resizable nil
                        :size (om-make-point 20 14)
                        :font (om-def-font :font1)
                        :di-action #'(lambda (item)
                                       (om-enable-dialog-item popup (om-checked-p item))
                                       (when (null (om-checked-p item))
                                         (om-set-selected-item popup (get-default-value default)))
                                       (set-property object prop-id 
                                                     (if (om-checked-p item) 
                                                         (om-get-selected-item popup)
                                                       nil))
                                       (when update (update-after-prop-edit update object))
                                       ))))
          (om-make-layout 'om-row-layout
                    :subviews (list checkbox popup)
                    :delta nil :align :center)
          )
        
        ;;; simple list
        popup)
    ))
    

;;;====================================
;;; COLORS
;;;====================================

(defstruct color-or-nil (t-or-nil) (color))
;;; for compatibility with simple colors...
(defmethod color-? ((self color-or-nil)) (color-or-nil-t-or-nil self))
(defmethod color-color ((self color-or-nil)) (color-or-nil-color self))
(defmethod color-? ((self oa::omcolor)) t)
(defmethod color-color ((self oa::omcolor)) self)
(defmethod color-? ((self t)) nil)


(defmethod object-accept-transparency ((self t)) t)

(defmethod make-prop-item ((type (eql :color)) prop-id object &key default update)
  (om-make-view 'color-view 
                :size (om-make-point 60 16)
                :resizable nil
                :with-alpha (object-accept-transparency object)
                :enabled t ; (get-property object prop-id)
                :color (or (get-property object prop-id)
                           default
                           (om-def-color :gray))
                :after-fun #'(lambda (item)
                               (set-property object prop-id (color item))
                               (when update (update-after-prop-edit update object))
                               )))

(defmethod make-prop-item ((type (eql :color-or-nil)) prop-id object &key default update)

  (let* ((def (or default (om-def-color :gray)))
         colorview colorbox)

    (setf colorview 
          (om-make-view 'color-view 
                        :size (om-make-point 60 16)
                        :resizable nil
                        :with-alpha (object-accept-transparency object)
                        :enabled (and (valid-property-p object prop-id)
                                      (get-property object prop-id)
                                      (color-? (get-property object prop-id)))
                        :color (and (valid-property-p object prop-id)
                                    (if (color-? (get-property object prop-id)) 
                                        (color-color (get-property object prop-id))
                                      (get-default-value def)))
                        :after-fun #'(lambda (item)
                                       (set-property 
                                        object prop-id 
                                        (make-color-or-nil :color (color item)
                                                           :t-or-nil t))
                                       (om-set-check-box colorbox t)
                                       (when update (update-after-prop-edit update object))
                                       )))
    (setf colorbox 
          (om-make-di 'om-check-box 
                      :checked-p (and (valid-property-p object prop-id)
                                      (get-property object prop-id)
                                      (color-? (get-property object prop-id)))
                      :text ""
                      :resizable nil
                      :size (om-make-point 20 14)
                      :font (om-def-font :font1)
                      :di-action #'(lambda (item)
                                     (setf (enabled colorview) (om-checked-p item))
                                     (unless (om-checked-p item)
                                       (setf (color colorview) (get-default-value default)))
                                     (om-invalidate-view colorview)
                                     (set-property  
                                      object prop-id 
                                      (make-color-or-nil :color (if (om-checked-p item) 
                                                                    (get-default-value default)
                                                                  nil) ;(get-default-value def)
                                                         :t-or-nil (om-checked-p item)))
                                     (when update (update-after-prop-edit update object))
                                     )))
    (om-make-layout 'om-row-layout
                    :subviews (list colorbox colorview)
                    :delta nil :align :center)
    ))


;;;====================================
;;; FONTS
;;;====================================

(defstruct font-or-nil (t-or-nil) (font))
;;; for compatibility with simple fonts...
(defmethod font-? ((self font-or-nil)) (font-or-nil-t-or-nil self))
(defmethod font-font ((self font-or-nil)) (font-or-nil-font self))
(defmethod font-? ((self t)) (gp::font-description-p self))
(defmethod font-font ((self t)) (and (gp::font-description-p self) self))


(defmethod make-prop-item ((type (eql :font)) prop-id object &key default update)
  (flet ((font-to-str (font) 
           (if (om-font-p font)
               (format nil " ~A ~Dpt ~A" (om-font-face font) (round (om-font-size font)) 
                       (if nil ;(om-font-style font) 
                           (format nil "[~{~S~^ ~}]" (om-font-style font)) ""))
             "-")))
    (om-make-di 'om-button 
                :resizable nil
                ;:enable (valid-property-p object prop-id)
                :focus nil :default nil
                :text (font-to-str (get-property object prop-id))
                :size (om-make-point (list :string (font-to-str (get-property object prop-id))) 26)
                :font (om-def-font :font1 :style (om-font-style (get-property object prop-id)))
                :di-action #'(lambda (item)
                               (let ((choice (om-choose-font-dialog :font (or (get-property object prop-id)
                                                                              (and update (om-get-font update))))))
                                 (om-set-dialog-item-text item (font-to-str choice))
                                 (om-set-font item (om-def-font :font1 :style (om-font-style choice)))
                                 (set-property object prop-id choice)
                                 (when update (update-after-prop-edit update object))
                                 )))))


(defmethod make-prop-item ((type (eql :font-or-nil)) prop-id object &key default update)
  
  (flet (
         (font-to-str (font) 
           (if (om-font-p font)
               (format nil " ~A ~Dpt ~A" (om-font-face font) (round (om-font-size font)) 
                       (if nil ;(om-font-style font) 
                           (format nil "[~{~S~^ ~}]" (om-font-style font)) ""))
             "-"))
         )
         
    (let* ((def (or default (om-def-font :font1)))
           (current (and (valid-property-p object prop-id)
                         (if (font-? (get-property object prop-id)) 
                             (font-font (get-property object prop-id))
                           (get-default-value def))))
           fontbutton checkbox)

      (setf fontbutton

            (om-make-di 'om-button 
                        :resizable nil
                        :enable (and (valid-property-p object prop-id)
                                     (get-property object prop-id)
                                     (font-? (get-property object prop-id)))
                        :focus nil :default nil
                        :text (font-to-str (font-font current))
                        :size (om-make-point (list :string (font-to-str (font-font current))) 26)
                        :font (om-def-font :font1 :style (om-font-style (font-font current)))
                        :di-action #'(lambda (item)
                                       (let ((choice (om-choose-font-dialog 
                                                      :font (or (font-font (get-property object prop-id))
                                                                (and update (om-get-font update))))))
                                         (om-set-dialog-item-text item (font-to-str choice))
                                         (om-set-font item (om-def-font :font1 :style (om-font-style choice)))
                                         (set-property object prop-id 
                                                       (make-font-or-nil :font choice
                                                                         :t-or-nil t))
                                         (when update (update-after-prop-edit update object))
                                         )))

            )
      (setf checkbox 
            (om-make-di 'om-check-box 
                        :checked-p (and (valid-property-p object prop-id)
                                        (get-property object prop-id)
                                        (font-? (get-property object prop-id)))
                        :text ""
                        :resizable nil
                        :size (om-make-point 20 14)
                        :font (om-def-font :font1)
                        :di-action #'(lambda (item)
                                       (om-enable-dialog-item fontbutton (om-checked-p item))
                                       (unless (om-checked-p item)
                                         (om-set-dialog-item-text fontbutton (font-to-str (get-default-value def))))
                                       (set-property  
                                        object prop-id 
                                        (make-font-or-nil :font (if (om-checked-p item) 
                                                                    (get-default-value default)
                                                                  nil)
                                                          :t-or-nil (om-checked-p item)))
                                       (when update (update-after-prop-edit update object))
                                       )))
      (om-make-layout 'om-row-layout
                      :subviews (list checkbox fontbutton)
                      :align :center
                      :delta nil)
      )))


;;;====================================
;;; PATHNAME
;;;====================================

(defmethod make-prop-item ((type (eql :path)) prop-id object &key default update)
  (let ((textview (om-make-view 'click-and-edit-text 
                                :enabled (get-property object prop-id) ;; it can happen that the value is NIL, e.g. in multiple-selection
                                :text (if (get-property object prop-id)
                                          (format nil "~A" (get-property object prop-id))
                                        "   ...   ")
                                :resizable nil
                                :bg-color (om-def-color :window)
                                :fg-color (if (get-property object prop-id) 
                                              (if (probe-file (get-property object prop-id))
                                                  (om-def-color :black) 
                                                (om-def-color :red))
                                            (om-def-color :gray))
                                :border nil 
                                :size (om-make-point (if (get-property object prop-id)
                                                         (list :string (format nil "~A" (get-property object prop-id)))
                                                       100)
                                                     20)
                                :font (om-def-font :font1)
                                :after-fun #'(lambda (item)
                                               (set-property object prop-id (text item))
                                               (when update (update-after-prop-edit update object))
                                               (om-set-fg-color 
                                                item 
                                                (if (probe-file (get-property object prop-id)) 
                                                    (om-def-color :black) (om-def-color :red))))
                                )))
    (om-make-layout 'om-row-layout :subviews 
                    (list 
                     textview
                     (om-make-graphic-object 'om-icon-button :size (omp 20 18) 
                                             :icon :folder :icon-pushed :folder-pushed
                                             :action #'(lambda (button) (declare (ignore button))
                                                         (let ((file (om-choose-file-dialog :prompt "Select a new reference file"
                                                                                            :types (doctype-info :om)
                                                                                            :directory *last-open-dir*)))
                                                           (when file
                                                             (set-property object prop-id (namestring file))
                                                             (when update (update-after-prop-edit update object))
                                                             (setf (text textview) (get-property object prop-id))
                                                             (om-set-fg-color 
                                                              textview 
                                                              (if (probe-file (get-property object prop-id)) 
                                                                  (om-def-color :black) (om-def-color :red)))
                                                             (om-invalidate-view textview)
                                                             )
                                                           )))
                     ))
    ))



;;;====================================
;;; ACTION / BUTTON
;;;====================================

(defmethod get-def-action-list ((object t)) nil)
(defmethod arguments-for-action ((fun t)) nil)

(defun get-arguments-dialog (arglist &optional (vals nil vals-supplied-p))
  (let* ((win nil) (fields nil)
         (font (om-def-font :font1))
         (cb (om-make-di 'om-button :text "Cancel" :size (omp 80 25) :font font
                         :di-action #'(lambda (b) (om-return-from-modal-dialog win nil))))
         (ob (om-make-di 'om-button :text "OK" :size (omp 80 25) :default t :focus t :font font
                         :di-action #'(lambda (b) (om-return-from-modal-dialog 
                                                   win 
                                                   (loop for edt in (reverse fields) collect 
                                                         (read-from-string (om-dialog-item-text edt))))))))
    
    (setf win (om-make-window  
               'om-dialog ;:resizable :h 
               :size (omp nil nil)
               :position (om-mouse-position nil)
               :win-layout (om-make-layout 
                            'om-column-layout;  :ratios '(1 1 2) 
                            :align :left
                            :subviews (append 
                                       (loop for arg in arglist 
                                             for name = (nth 1 arg)
                                             for defval = (nth 2 arg)
                                             for n = 0 then (1+ n) collect
                                             (om-make-layout 
                                              'om-row-layout :align :center
                                              :subviews (let ((val (if vals-supplied-p (nth n vals) defval)))
                                                          (list (om-make-di 'om-simple-text :font font :text (string name)
                                                                            :size (omp (+ 20 (om-string-size (string name) font)) 18))
                                                                (let ((edt (om-make-di 'om-editable-text :bg-color (om-def-color :white)
                                                                                       :text (if (stringp val) (format nil "~s" val) (format nil "~A" val))
                                                                                       :size (omp 120 32) :font (om-def-font :font1))))
                                                                (push edt fields)
                                                                edt)))))
                                       (list (om-make-view 'om-view :size (omp nil 20))          
                                             (om-make-layout 'om-row-layout :subviews (list cb ob)))
                                       )
                            )))
                                                         
                                         
    (om-modal-dialog win)))



(defmethod make-prop-item ((type (eql :action)) prop-id object &key default update)
  (labels ((object-action-other-name (ob)
             (let* ((curr-fun (get-property ob prop-id))
                    (curr-fun-name (if (consp curr-fun) (car curr-fun) curr-fun))
                    (curr-fun-args (if (consp curr-fun) (cdr curr-fun) nil)))
               (if (and curr-fun (not (find curr-fun-name (get-def-action-list ob)))) curr-fun-name :?)))
           (action-set-params (ob)
             (let* ((curr-fun (get-property ob prop-id))
                    (curr-fun-name (if (consp curr-fun) (car curr-fun) curr-fun))
                    (curr-fun-args (if (consp curr-fun) (cdr curr-fun) nil))
                    (args (if curr-fun-args (get-arguments-dialog (arguments-for-action curr-fun-name) curr-fun-args)
                            (get-arguments-dialog (arguments-for-action curr-fun-name)))))
               (when args (set-property object prop-id (cons curr-fun-name args))))))
    (let* ((curr-fun (get-property object prop-id))
           (curr-fun-name (if (consp curr-fun) (car curr-fun) curr-fun))
           (curr-fun-args (if (consp curr-fun) (cdr curr-fun) nil))
           (other-name (object-action-other-name object))
           (def-action-list (get-def-action-list object))
           (print-action-list (append '(nil) def-action-list (list (format nil "other: ~A" other-name))))
           (layout (om-make-layout 'om-row-layout :delta nil))
           (b (om-make-di 'om-button 
                          :resizable nil :focus nil :default nil
                          :text "..." :size (om-make-point 40 24) :font (om-def-font :font1)
                          :di-action #'(lambda (b) (declare (ignore b)) (action-set-params object))))
           (poplist (om-make-di 'om-popup-list 
                                :items print-action-list 
                                :resizable nil
                                :value (if (equal other-name :?) curr-fun-name (format nil "other: ~A" other-name))
                                :size (om-make-point (list :string (format nil "~A   " curr-fun-name)) 22)
                                :font (om-def-font :font1)
                                :di-action #'(lambda (list)
                                               (let* ((fun-i (om-get-selected-item-index list))
                                                      (fun 
                                                       (if (= fun-i (1+ (length def-action-list)))
                                                           (let ((user-fun (om-get-user-string "Enter a function name or a list (function arguments)" 
                                                                                               :initial-string (get-property object prop-id))))
                                                             (when user-fun 
                                                               (om-set-item-list list (append '(nil) def-action-list (list (format nil "other: ~A" user-fun))))
                                                               (om-set-selected-item-index list (1+ (length def-action-list)))
                                                               (read-from-string user-fun)))
                                                         (nth fun-i print-action-list))))
                                                 (om-remove-subviews layout b)
                                                 (when (arguments-for-action fun)
                                             ;(om-set-dialog-item-action-function 
                                             ; b #'(lambda (b) (declare (ignore b))
                                             ;       (let ((args (get-arguments-dialog (arguments-for-action fun))))
                                             ;         (when args (setf fun (cons fun args))))))
                                                   (om-add-subviews layout b))
                                           
                                                 (set-property object prop-id fun))
                                           
                                               (when update (update-after-prop-edit update object))
                                               ))))
      (om-add-subviews layout poplist)
      (when (arguments-for-action curr-fun-name)
        (om-add-subviews layout b))
      layout)))

           

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


(defmethod prop-item-call-function-to-object ((object virtual-object-selection) function)
  (funcall function (car (objects object))))


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



              