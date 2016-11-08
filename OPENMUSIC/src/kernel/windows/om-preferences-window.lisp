;=========================================================================
; OpenMusic: Visual Programming Language for Music Composition
; PREFERENCES WINDOW
;=========================================================================


(in-package :om)

;;;===========================================================================
;;; DIFFERENT TYPEs OF PReFERENCE ITEMS
;;; TODO : try to use the same code here and in the inspector window

;;; general case = text box
(defmethod make-preference-item (type pref-item)
  (om-make-view 'click-and-edit-text 
                :text (format nil "~A" (pref-item-value pref-item))
                :resizable :w
                :bg-color (om-def-color :window)
                :border nil ;(om-def-color :gray)
                :size (om-make-point (list :string (format nil "  ~A  " (pref-item-value pref-item))) 20)
                :font (om-def-font :font2)
                :after-fun #'(lambda (item)
                               (setf (pref-item-value pref-item) (text item))
                               )))


(defmethod make-preference-item ((type (eql :bool)) pref-item)
  (om-make-di 'om-check-box 
              :checked-p (pref-item-value pref-item) 
              :text ""
              :resizable :w
              :size (om-make-point 20 14)
              :font (om-def-font :font1)
              :di-action #'(lambda (item)
                              (setf (pref-item-value pref-item) (om-checked-p item))
                              )))


#|
(defmethod make-preference-item ((type (eql :path)) pref-item)
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

(defmethod make-preference-item ((type (eql :number)) pref-item)
  (om-make-graphic-object 'numbox 
                          :value (get-property object prop-id)
                          :bg-color (om-def-color :white)
                          :border t
                          :size (om-make-point 40 18) 
                          :font (om-def-font :font2)
                          :min-val (if default (car default) 0) :max-val (if default (cadr default) 10000)
                          :after-fun #'(lambda (item)
                             (set-property object prop-id (value item))
                             (when update (update-view update object))
                             )))



(defmethod make-preference-item ((type list) pref-item)
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

(defmethod make-preference-item ((type (eql :font)) pref-item)
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


(defmethod make-preference-item ((type (eql :color)) pref-item)
  (let* (colorview colorbox)
    (setf colorview 
          (om-make-view 'color-view 
                        :size (om-make-point 50 16)
                        :resizable :w
                        :enabled (and (valid-property-p object prop-id) (get-property object prop-id))
                        :color (and (valid-property-p object prop-id)
                                    (or (get-property object prop-id)
                                        (om-def-color :light-gray)))
                        :after-fun #'(lambda (item)
                                       (set-property object prop-id (color item))
                                       (om-set-check-box colorbox t)
                                       (when update (update-view update object))
                                       )))
    (setf colorbox 
          (om-make-di 'om-check-box 
                      :checked-p (and (valid-property-p object prop-id) (get-property object prop-id))
                      :text ""
                      :resizable :w
                      :size (om-make-point 20 14)
                      :font (om-def-font :font1)
                      :di-action #'(lambda (item)
                                     (setf (enabled colorview) (om-checked-p item))
                                     (om-invalidate-view colorview)
                                     (set-property  
                                      object prop-id 
                                      (if (om-checked-p item) (color colorview) nil))
                                     (when update (update-view update object))
                                     )))
    (om-make-layout 'om-row-layout
                    :subviews (list colorbox colorview nil)
                    :delta nil)
    ))
|#

;;;===========================================================================
;;; THE VIEW OF ONE PREFERENCE

(defmethod make-preference-item ((type (eql :title)) pref-item) 
  (om-make-di 'om-simple-text :size (om-make-point 20 20) :text "" :focus t))

(defun make-preference-view (pref-module pref-item)
  (let ((main-row 
         (om-make-layout 'om-row-layout
                         :subviews (list
                                    (om-make-di 'om-simple-text :text (pref-item-name pref-item) 
                                                :font (if (equal (pref-item-type pref-item) :title) (om-def-font :font2b) (om-def-font :font2))
                                                :size (om-make-point 110 20)) ; :position (om-make-point 10 16))
                                    (make-preference-item (pref-item-type pref-item) pref-item)))))                                                         
    (if (pref-item-doc pref-item)
        (om-make-layout 'om-column-layout
                        :subviews (list main-row 
                                        (om-make-di 'om-simple-text :text (pref-item-doc pref-item) :font (om-def-font :font1)
                                                    :size (om-make-point 110 20))))
      main-row)))


;;;===========================================================================
;;; THE TAB VIEW OF ONE MODULE
(defclass preference-pane (om-grid-layout)
  ((module-id :accessor module-id :initarg :module-id :initform nil)))

(defun make-preference-panel (pref-module)
  (om-make-layout 'preference-pane 
                  :name (pref-module-name pref-module)
                  :module-id (pref-module-id pref-module)
                  :ratios '((1)(1))
                  :subviews (loop for pref in (pref-module-items pref-module)
                                  collect (make-preference-view pref-module pref))))


;;;===========================================================================
;;; PREFERENCES WINDOW
; (om-select-window (make-preferences-window))

(defun make-preferences-window ()
  (let ((win (om-make-window 
              'om-window :title "OpenMusic Preferences" 
              ;:size (om-make-point 800 nil) ;:resizable nil
              ))
        (preference-tabs (om-make-layout 
                          'om-tab-layout
                          :subviews (mapcar #'make-preference-panel (sort-pref-items *user-preferences*)))))
    (om-add-subviews
     win 
     (om-make-layout 
      'om-column-layout :ratios '(100 1)
      :subviews (list
                 preference-tabs
                 (om-make-layout 'om-row-layout
                                 :subviews (list 
                                            nil
                                            (om-make-di 
                                             'om-button 
                                             :text "Restore defaults" 
                                             :size (om-make-point 120 24)
                                             :di-action #'(lambda (item)
                                                            (let* ((current-panel (om-get-current-view preference-tabs))
                                                                   (module-id (module-id current-panel)))
                                                              (restore-default-preferences module-id)
                                                              (let ((new-panel (make-preference-panel (find-pref-module module-id)))) 
                                                                (om-substitute-subviews preference-tabs current-panel new-panel)
                                                                (om-set-current-view preference-tabs new-panel)
                                                                )))
                                             ))))
                ))
    win))
