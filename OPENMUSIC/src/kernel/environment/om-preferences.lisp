;=========================================================================
; OpenMusic: Visual Programming Language for Music Composition
; PREFERENCES
;=========================================================================

(in-package :om)

(defvar *user-preferences* nil)

;;; PREFERENCES = LIST
;;; (:preferences
;;;   (:MODULE-NAME1 (:PREF1 val1) (:PREF-2 VAL2) ...))
;;;   (:MODULE-NAME2 (:PREF1 val1) (:PREF-2 VAL2) ...))
;;;   ... )

(defun find-pref-module (name &optional preferences-list)
   (find name (or preferences-list *user-preferences*) :key 'car))

(defun get-pref-in-module (module key)
  (when module
    (find-value-in-kv-list (copy-list (cdr module)) key)))

(defun get-pref-def-value (module-name pref-key)
  (let ((defvals (default-prefs-for-module module-name)))
    (find-value-in-kv-list defvals pref-key)))

(defun get-pref (module-name pref-key &optional preferences-list)
  (get-pref-in-module (find-pref-module module-name preferences-list) pref-key))

(defun get-pref-value (module-name pref-key &optional preferences-list)
  (or (get-pref module-name pref-key preferences-list)
      (get-pref-def-value module-name pref-key)))

(defun set-pref-in-module (module key val)
  ;(print module)
  (let ((pos (position key (cdr module) :test 'equal :key 'car)))
    (if pos 
        (setf (cadr (nth pos (cdr module))) val)
      (setf module (append module (list (list key val)))))))

(defun set-pref (module-name key val &optional preferences-list)
  (set-pref-in-module (find-pref-module module-name preferences-list) key val))


;;; ADD A NEW PREF MODULE
(defun push-pref-module (module)
  (let* ((pos (position module *user-preferences* :test 'equal :key 'car)))
    (unless pos
      (setf *user-preferences* (append *user-preferences* (list (list module (default-prefs-for-module module))))))
    (setf *user-preferences* (sort-pref-items *user-preferences*))))

(defparameter *pref-order* '(:general :appearance :userlibs :score :conversion :midi :audio :externals))

(defun sort-pref-items (list)
  (sort list #'(lambda (elt1 elt2) 
                 (let ((p1 (position (car elt1) *pref-order*))
                       (p2 (position (car elt2) *pref-order*)))
                   (or (null p2) (and p1 p2 (<= p1 p2)))))))


(defmethod default-prefs-for-module (module) nil)

(defmethod corrige-preference-list (modulename list &optional (version 0))
  (let ((def (default-prefs-for-module modulename)))
    (loop for item in def collect
          (let ((userval (cadr (find (car item) list :key 'car))))
            (if userval
                (list (car item) userval)
              (list (car item) (cadr item)))))
            ))


(defmethod load-saved-prefs (saved-prefs)
  ;;; applies the pref list
  (loop for saved-module in saved-prefs do
        ;;; find  the corresponding pref module in OM prefs
        (let* ((module-name (car saved-module))
               (real-module (find-pref-module module-name)))
          (when real-module
            (setf (cdr real-module) (corrige-preference-list module-name (load-preferences (cdr saved-module))))
            (apply-preferences module-name))))
  )


(defun load-preferences (pref-saved-values)
  (loop for item in pref-saved-values collect
        (list (car item) (omng-load (cadr item)))))


(defmethod apply-all-preferences (&optional preferences-list)
  (let ((prefs (or preferences-list *user-preferences*)))
    (loop for item in prefs do
          (apply-preferences (car item)))))

(defmethod restore-default-preferences ()
  (loop for item in *user-preferences* do
        (setf (cdr item) (default-prefs-for-module (car item)))
        (apply-preferences (car item))))

(defmethod save-pref-module (modulename)
   (list modulename))


(defmethod save-preferences ()
  (if *current-workSpace*
      (save-workspace-file *current-workSpace*)
    (save-om-preferences)))

; (save-preferences)

(defun display-om-preferences (&optional module-name)
  (let ((prefs-to-display (if module-name (list (find-pref-module module-name)) *user-preferences*)))
    (om-print "============================")
    (om-print "CURRENT OM PREFERENCES:")
    (om-print "============================")
    (loop for item in prefs-to-display do
          (om-print (format nil "MODULE: ~A" (car item)))
          (loop for prefitem in (cdr item) do 
                (om-print (format nil "    ~A = ~A" (car prefitem) (cadr prefitem))))
          (om-print "============================"))
    ))

; (display-om-preferences)



;;;====================================
;;; PREFERENCES WINDOW
;;;====================================

(defclass preferences-window (om-dialog)
  ((tabs :accessor tabs :initarg :tabs :initform nil)
   (local-prefs :accessor local-prefs :initarg :local-prefs :initform nil)))

(defclass preference-pane (om-view)
  ((pref-id :accessor pref-id :initarg :pref-id :initform nil)))

;; rebuilds the window and resets the previous selected tab
(defmethod update-pref-window ((self preferences-window) &optional selection)
  (let* ((selec 0)
         (panelist (loop for item in (local-prefs self) 
                         for i = 0 then (+ i 1) 
                         do
                         (when (and selection (equal selection (car item))) (setf selec i))
                         collect 
                         (make-pref-panel (car item) item)))
         (newtl (om-make-layout 'om-tab-layout :subviews panelist
                                :selection selec)))
    (om-substitute-subviews self (tabs self) newtl)
    (setf (tabs self) newtl)))
  
(defmethod make-pref-panel ((num t) modulepref) nil)

; (capi::display (make-preferences-window))

(defun make-preferences-window ()
   (let* ((prefs (sort-pref-items (copy-tree *user-preferences*)))
          (panelist (remove nil (mapcar #'(lambda (item)
                                (make-pref-panel (car item) item))
                            prefs)))
          (tl (om-make-layout 'om-tab-layout :subviews panelist))
          (win (om-make-window 'preferences-window :title "OpenMusic Preferences" 
                               :close t 
                               :size (om-make-point 800 450)
                               :win-layout (om-make-layout 'om-column-layout :ratios '(1 nil))
                               :resizable nil
                               :local-prefs prefs)))
     (om-add-subviews win 
                      (setf (tabs win) tl))
     (om-add-subviews win 
                      (om-make-layout 'om-row-layout
                                      :subviews (list 
                                                 (om-make-di 'om-button :size (om-make-point 100 24) :text "Restore..." 
                                                             :di-action #'(lambda (item)
                                                                            (let* ((win (om-view-window item))
                                                                                   (module (find-pref-module (pref-id (om-current-view (tabs win)))
                                                                                                             (local-prefs win))))
                                                                            (when module
                                                                                (setf (cadr module) (default-prefs-for-module (pref-id (om-current-view (tabs win)))))
                                                                                (update-pref-window win (pref-id (om-current-view (tabs win)))))
                                                                              )))
                                                 
                                                 (om-make-di 'om-button :size (om-make-point 80 24) :text "Apply" 
                                                             :di-action #'(lambda (item)
                                                                            (let ((win (om-view-window item)))
                                                                              (setf *user-preferences* (local-prefs win))
                                                                              (apply-all-preferences)
                                                                              (update-pref-window win (pref-id (om-current-view (tabs win))))
                                                                              )))
                                                 nil
                                                 (om-make-di 'om-button :size (om-make-point 80 24) :text "Cancel" 
                                                             :cancel t
                                                             :di-action #'(lambda (item)
                                                                            (om-close-window (om-view-window item))
                                                                            ))
                    
                                                 (om-make-di 'om-button :size (om-make-point 80 24) :text "OK" 
                                                             :default t
                                                             :di-action #'(lambda (item)
                                                                            (setf *user-preferences* (local-prefs win))
                                                                            (apply-all-preferences)
                                                                            (save-preferences)
                                                                            (om-close-window (om-view-window item))
                                                                            )
                                                             ))
                                      ))
     win))
















