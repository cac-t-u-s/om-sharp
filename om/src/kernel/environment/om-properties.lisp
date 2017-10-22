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


;;;===========================================
;;; DIFFERENT KINDS OF ITEM IN THE INSPECTOR FOR THE PROPERITIES
;;; USED IN EDITORS OR INSPECTOR WINDOWS
;;;===========================================

;;; special types
(defstruct number-in-range (min) (max))
(defstruct number-or-nil (t-or-nil) (number) (min) (max))

(defstruct color-or-nil (t-or-nil) (color))
;;; for compatibility with simple colors...
(defmethod color-? ((self color-or-nil)) (color-or-nil-t-or-nil self))
(defmethod color-color ((self color-or-nil)) (color-or-nil-color self))
(defmethod color-? ((self oa::omcolor)) t)
(defmethod color-color ((self oa::omcolor)) self)

;;; general case = text box
(defmethod make-prop-item (type prop-id object &key default update)
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
                               (when update (update-view update object))
                               )))

(defmethod make-prop-item ((type (eql :path)) prop-id object &key default update)
  (let ((textview (om-make-view 'click-and-edit-text 
                ;:enabled (valid-property-p object prop-id)
                                :text (format nil "~A" (get-property object prop-id))
                                :resizable :w
                                :bg-color (om-def-color :window)
                                :fg-color (if (probe-file (get-property object prop-id)) (om-def-color :black) (om-def-color :red))
                                :border nil ;(om-def-color :gray)
                                :size (om-make-point (list :string (format nil "~A" (get-property object prop-id))) 20)
                                :font (om-def-font :font1)
                                :after-fun #'(lambda (item)
                                                (set-property object prop-id (text item))
                                                (when update (update-view update object))
                                                (om-set-fg-color 
                                                 item 
                                                 (if (probe-file (get-property object prop-id)) 
                                                     (om-def-color :black) (om-def-color :red))))
                                )))
    (om-make-layout 'om-row-layout :subviews 
                    (list 
                     textview
                     (om-make-graphic-object 'om-icon-button :size (omp 20 18) 
                                             :icon 'folder :icon-pushed 'folder-pushed
                                             :action #'(lambda (button) (declare (ignore button))
                                                         (let ((file (om-choose-file-dialog :prompt "Select a new reference file"
                                                                                            :types (doctype-info :om)
                                                                                            :directory *last-open-dir*)))
                                                           (when file
                                                             (set-property object prop-id (namestring file))
                                                             (when update (update-view update object))
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

;;; for number default is a list (min-val max-val decimals)
(defmethod make-prop-item ((type (eql :number)) prop-id object &key default update)
  (let ((def (loop for element in default collect
                   (if (functionp element) (funcall element object)
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
                             (when update (update-view update object))
                             ))))

(defmethod make-prop-item ((type (eql :bool)) prop-id object &key default update)
  (om-make-di 'om-check-box 
              ;:enable (valid-property-p object prop-id)
              :checked-p (get-property object prop-id) ; (and (valid-property-p object prop-id) (get-property object prop-id))
              :text ""
              :resizable :w
              :size (om-make-point nil 14)
              :font (om-def-font :font1)
              :di-action #'(lambda (item)
                             (set-property object prop-id (om-checked-p item))
                             (when update (update-view update object))
                             )))

(defmethod make-prop-item ((type list) prop-id object &key default update)
  (om-make-di 'om-popup-list 
              ;:enable (valid-property-p object prop-id)
              :items type 
              :resizable :w
              :value (get-property object prop-id)
              :size (om-make-point (list :string (format nil "~A" (get-property object prop-id))) 22)
              :font (om-def-font :font1)
              :di-action #'(lambda (item)
                             (set-property object prop-id (om-get-selected-item item))
                             (when update (update-view update object))
                             )))

(defmethod make-prop-item ((type (eql :font)) prop-id object &key default update)
  (flet ((font-to-str (font) 
           (if (om-font-p font)
               (format nil " ~A ~Dpt ~A" (om-font-face font) (round (om-font-size font)) 
                       (if (om-font-style font) (format nil "[~{~S~^ ~}]" (om-font-style font)) ""))
             "-")))
    (om-make-di 'om-button 
                :resizable :w
                ;:enable (valid-property-p object prop-id)
                :focus nil :default nil
                :text (font-to-str (get-property object prop-id))
                :size (om-make-point (list :string (font-to-str (get-property object prop-id))) 26)
                :font (om-def-font :font1)
                :di-action #'(lambda (item)
                               (let ((choice (om-choose-font-dialog :font (or (get-property object prop-id)
                                                                              (and update (om-get-font update))))))
                                 (om-set-dialog-item-text item (font-to-str choice))
                                 (set-property object prop-id choice)
                                 (when update (update-view update object))
                                 )))))


(defmethod object-accept-transparency ((self t)) t)

(defmethod make-prop-item ((type (eql :color)) prop-id object &key default update)
  (om-make-view 'color-view 
                :size (om-make-point 50 16)
                :resizable :w
                :with-alpha (object-accept-transparency object)
                :enabled (and (valid-property-p object prop-id) (get-property object prop-id))
                :color (and (valid-property-p object prop-id)
                            (or (get-property object prop-id)
                                (om-def-color :light-gray)))
                :after-fun #'(lambda (item)
                               (set-property object prop-id (color item))
                               (when update (update-view update object))
                               )))

(defmethod make-prop-item ((type (eql 'color-or-nil)) prop-id object &key default update)
  (let* (colorview colorbox)
    (setf colorview 
          (om-make-view 'color-view 
                        :size (om-make-point 50 16)
                        :resizable :w
                        :with-alpha (object-accept-transparency object)
                        :enabled (and (valid-property-p object prop-id)
                                      (get-property object prop-id)
                                      (color-? (get-property object prop-id)))
                        :color (and (valid-property-p object prop-id)
                                    (if (get-property object prop-id) 
                                        (color-color (get-property object prop-id))
                                        (om-def-color :light-gray)))
                        :after-fun #'(lambda (item)
                                       (set-property 
                                        object prop-id 
                                        (make-color-or-nil :color (color item)
                                                           :t-or-nil t))
                                       (om-set-check-box colorbox t)
                                       (when update (update-view update object))
                                       )))
    (setf colorbox 
          (om-make-di 'om-check-box 
                      :checked-p (and (valid-property-p object prop-id)
                                      (get-property object prop-id)
                                      (color-? (get-property object prop-id)))
                      :text ""
                      :resizable :w
                      :size (om-make-point 20 14)
                      :font (om-def-font :font1)
                      :di-action #'(lambda (item)
                                     (setf (enabled colorview) (om-checked-p item))
                                     (om-invalidate-view colorview)
                                     (set-property  
                                      object prop-id 
                                      (make-color-or-nil :color (color colorview)
                                                         :t-or-nil (om-checked-p item)))
                                     (when update (update-view update object))
                                     )))
    (om-make-layout 'om-row-layout
                    :subviews (list colorbox colorview nil)
                    :delta nil)
    ))


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
                                :resizable :w
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
                                           
                                               (when update (update-view update object))
                                               ))))
      (om-add-subviews layout poplist)
      (when (arguments-for-action curr-fun-name)
        (om-add-subviews layout b))
      layout)))

           
              