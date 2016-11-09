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
  (let* ((curr-value (pref-item-value pref-item)))
    (om-make-view 'click-and-edit-text 
                  :text (format nil "~A" curr-value)
                  :resizable :w
                  :bg-color (om-def-color :white) ; (om-def-color :window)
                  :border nil
                  :size (om-make-point (list :string (format nil "  ~A  " curr-value)) 20)
                  :font (om-def-font :font2)
                  :after-fun #'(lambda (item)
                                 (setf (pref-item-value pref-item) (text item))
                                 ))))


(defmethod make-preference-item ((type (eql :bool)) pref-item)
  (om-make-di 'om-check-box 
              :checked-p (pref-item-value pref-item) 
              :text ""
              :resizable :w
              :size (om-make-point 20 20)
              :font (om-def-font :font1)
              :di-action #'(lambda (item)
                              (setf (pref-item-value pref-item) (om-checked-p item))
                              )))

(defmethod make-preference-item ((type (eql :folder)) pref-item) 
  (let* ((curr-value (maybe-eval-pref-item-value pref-item))
         (textview (om-make-view 'click-and-edit-text 
                                :text (format nil "~A" curr-value)
                                :resizable :w
                                :bg-color (om-def-color :window)
                                ;:fg-color (if (probe-file curr-value) (om-def-color :black) (om-def-color :red))
                                :border nil
                                :size (omp 200 ;(list :string (format nil " ~A " curr-value)) 
                                           20)
                                :font (om-def-font :font1)
                                :after-fun #'(lambda (item)
                                                (setf (pref-item-value pref-item) (text item)))
                                )))
    (om-make-layout 'om-row-layout
                  :resizable :w
                  :subviews (list 
                             textview
                             (om-make-view 'om-view 
                                           :size (omp 20 18) :resizable nil
                                           :subviews (list 
                                                      (om-make-graphic-object 'om-icon-button :size (omp 20 18) 
                                                                              :position (omp 0 0)
                                                                              :icon 'folder :icon-pushed 'folder-pushed
                                                                              :action #'(lambda (button) (declare (ignore button))
                                                                   (let ((dir (om-choose-directory-dialog :directory *last-open-dir*)))
                                                                     (when dir
                                                                       (setf *last-open-dir* dir)
                                                                       (setf (pref-item-value pref-item) (namestring dir))
                                                                       (setf (text textview) (pref-item-value pref-item))
                                                                       (om-invalidate-view textview)
                                                                       ))))
                                                      ))))
    ))
     
(defmethod make-preference-item ((type list) pref-item)
  (om-make-di 'om-popup-list 
              ;:enable (valid-property-p object prop-id)
              :items type 
              :resizable :w
              :value (pref-item-value pref-item)
              :size (om-make-point (list :string (format nil "~A" (pref-item-value pref-item))) 22)
              :font (om-def-font :font1)
              :di-action #'(lambda (item)
                             (setf (pref-item-value pref-item) (om-get-selected-item item))
                             ))) 

(defstruct number-in-range (min) (max))

(defmethod make-preference-item ((type number-in-range) pref-item)
  (om-make-graphic-object 'numbox 
                          :value (pref-item-value pref-item)
                          :bg-color (om-def-color :white)
                          :border t
                          :size (om-make-point 40 18) 
                          :font (om-def-font :font2)
                          :min-val (or (number-in-range-min type) 0) :max-val (or (number-in-range-max type) 10000)
                          :after-fun #'(lambda (item)
                             (setf (pref-item-value pref-item) (value item))
                             )))

(defmethod make-preference-item ((type (eql :number)) pref-item)
  (make-preference-item (make-number-in-range :min -1000 :max 1000) pref-item))

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
                :text (font-to-str (pref-item-value pref-item))
                :size (om-make-point (list :string (font-to-str (pref-item-value pref-item))) 26)
                :font (om-def-font :font1)
                :di-action #'(lambda (item)
                               (let ((choice (om-choose-font-dialog :font (pref-item-value pref-item))))
                                 (om-set-dialog-item-text item (font-to-str choice))
                                 (setf (pref-item-value pref-item) choice)
                                 )))))

(defmethod make-preference-item ((type (eql :color)) pref-item)
  (om-make-view 'color-view 
                :size (om-make-point 50 16)
                :resizable :w
                :color (pref-item-value pref-item)
                :after-fun #'(lambda (item)
                               (setf (pref-item-value pref-item) (color item)))))



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
                                                :size (om-make-point 160 ;(list :string (format nil "  ~A  " (pref-item-name pref-item))) 
                                                                     20))
                                    (make-preference-item (pref-item-type pref-item) pref-item)))))                                                         
    (if nil ;(pref-item-doc pref-item)
        (om-make-layout 'om-column-layout
                        :subviews (list main-row 
                                        (om-make-di 'om-simple-text :text (pref-item-doc pref-item) :font (om-def-font :font1)
                                                    :size (om-make-point (list :string (format nil "  ~A  " (pref-item-doc pref-item))) 20))))
      main-row)))


;;;===========================================================================
;;; THE TAB VIEW OF ONE MODULE
(defclass preference-pane (om-column-layout)
  ((module-id :accessor module-id :initarg :module-id :initform nil)))

(defun make-preference-panel (pref-module)
  (om-make-layout 'preference-pane 
                  :name (pref-module-name pref-module)
                  :module-id (pref-module-id pref-module)
                  ;:ratios '((1)(1))
                  :subviews (loop for pref in (pref-module-items pref-module)
                                  collect (make-preference-view pref-module pref))))


;;;===========================================================================
;;; PREFERENCES WINDOW
; (om-select-window (make-preferences-window))

(defun make-preferences-window ()
  (let ((win (om-make-window 
              'om-window :title "OpenMusic Preferences" 
              ;:size (om-make-point 800 400) 
              ;:resizable :w
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
                                                                   (module-id (module-id current-panel))
                                                                   (pref-module (find-pref-module module-id)))
                                                              (restore-default-preferences module-id)
                                                              (om-remove-all-subviews current-panel)
                                                              (apply 'om-add-subviews 
                                                                     (cons current-panel 
                                                                           (loop for pref in (pref-module-items pref-module)
                                                                                 collect (make-preference-view pref-module pref))))
                                                              ))
                                             ))))
                ))
    win))
