;=========================================================================
; OpenMusic: Visual Programming Language for Music Composition
; PREFERENCES
;=========================================================================

(in-package :om)

(defstruct pref-module (id) (name) (items))
(defstruct pref-item (id) (name) (defval) (doc) (value))

;;; a list of pref-module
(defvar *user-preferences* nil)

;;;======================================================
;;; MODULES ARE USED TO SORT PREFERENCE ITEMS
(defparameter *pref-order* '(:general :appearance :userlibs :score :conversion :midi :audio :externals))

(defun sort-pref-items (list)
  (sort list #'(lambda (elt1 elt2) 
                 (let ((p1 (position (pref-module-id elt1) *pref-order*))
                       (p2 (position (pref-module-id elt2) *pref-order*)))
                   (or (null p2) (and p1 p2 (<= p1 p2)))))))

(defun find-pref-module (id &optional preferences-list)
   (find id (or preferences-list *user-preferences*) :key 'pref-module-id))

;;; ADD A NEW MODULE WITH NAME
(defun add-preference-module (id name)
  (let ((module (find-pref-module id)))
    (cond (module 
           (om-beep-msg "Warning: Preference module ~A already exists!" id)
           (setf (pref-module-name module) name))
          ((find name *user-preferences* :key 'pref-module-id :test 'string-equal)
           (om-beep-msg "Warning: A preference module with the name '~A' already exists!" name)
           (pushr (make-pref-module :id id :name name) *user-preferences*))
          (t (pushr (make-pref-module :id id :name name) *user-preferences*)))
    (setf *user-preferences* (sort-pref-items *user-preferences*))
    ))

(defun display-om-preferences (&optional module-name)
  (let ((prefs-to-display (if module-name (list (find-pref-module module-name)) *user-preferences*)))
    (om-print "============================")
    (om-print "CURRENT OM PREFERENCES:")
    (om-print "============================")
    (loop for module in prefs-to-display do
          (om-print (format nil "MODULE: ~A" (pref-module-name module)))
          (loop for prefitem in (pref-module-items module) do 
                (om-print (format nil "    ~A = ~A" (pref-item-name prefitem) (pref-item-value prefitem))))
          (om-print "============================"))
    ))
; (display-om-preferences)

;;;======================================================
;;; ADD A NEW PREF IN MODULE
(defun add-preference (module-id pref-id name defval &optional doc)
  (unless (find-pref-module module-id)
    (add-preference-module module-id (string module-id)))
  (let* ((module (find-pref-module module-id))
         (pref-item (find pref-id (pref-module-items module) :key 'pref-item-id)))
    (when pref-item
      (om-beep-msg "Warning: A preference with the same ID '~A' already exists!" pref-id))
    (setf (pref-module-items module)
          (append (remove pref-item (pref-module-items module))
                  (list (make-pref-item :id pref-id :name name :defval defval :doc doc :value defval))))))
    
;;;======================================================
;;; GET THE PREFERENCE VALUES
(defun get-pref-in-module (module pref-id)
  (find pref-id (pref-module-items module) :key 'pref-item-id))

(defun get-pref (module-name pref-key &optional preferences-list)
  (let ((module (find-pref-module module-name preferences-list)))
    (when module 
      (get-pref-in-module module pref-key))))

(defun get-pref-value (module-name pref-key &optional preferences-list)
  (let ((pref-item (get-pref module-name pref-key preferences-list)))
    (if pref-item
        (pref-item-value pref-item)
      (om-beep-msg "Preference '~A' not found in module '~A'" pref-key module-name))))

(defmethod restore-default-preferences ()
  (loop for module in *user-preferences* do
        (loop for pref in (pref-module-items module) do
              (setf (pref-item-value pref) (pref-item-defval pref)))))

;;;======================================================  
;;; SET THE PREFERENCE VALUES
(defun set-pref-in-module (module key val)
  (let ((pref-item (get-pref-in-module module key)))
    (if pref-item 
        (setf (pref-item-value pref-item) val)
      (add-preference (pref-module-id module) key (string key) val))))

(defun set-pref (module-name key val &optional preferences-list)
  (set-pref-in-module (find-pref-module module-name preferences-list) key val))

;;;======================================================
;;; PREFERENCES ARE SAVED AS
;;; (:preferences
;;;   (:MODULE-NAME1 (:PREF1 val1) (:PREF-2 VAL2) ...))
;;;   (:MODULE-NAME2 (:PREF1 val1) (:PREF-2 VAL2) ...))
;;;   ... )
;;;======================================================
; (save-preferences)
(defmethod save-pref-module (module)
  (cons (pref-module-id module)
        (loop for pref in (pref-module-items module) collect
              (list (pref-item-id pref) (omng-save (pref-item-value pref))))))

(defmethod save-preferences ()
  (if *current-workSpace*
      (save-workspace-file *current-workSpace*)
    (save-om-preferences)))

(defmethod load-saved-prefs (saved-prefs)
  (loop for saved-module in saved-prefs do
        ;;; find  the corresponding pref module in OM prefs
        (let* ((module-id (car saved-module))
               (real-module (find-pref-module module-id)))
          (when real-module
            (loop for pref in (cdr saved-module)
                  do (set-pref module-id (car pref) (omng-load (cadr pref)))))
          )))

;(defmethod apply-all-preferences (&optional preferences-list)
;  (let ((prefs (or preferences-list *user-preferences*)))
;    (loop for item in prefs do
;          (apply-preferences (car item)))))

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
   (let* ((prefs (sort-pref-items *user-preferences*))
          (panelist (remove nil (mapcar #'(lambda (item)
                                            (make-pref-panel (pref-module-id item) item))
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
                                                                                   (module (find-pref-module (pref-id (om-get-current-view (tabs win)))
                                                                                                             (local-prefs win))))
                                                                            (when module
                                                                                (setf (cadr module) (default-prefs-for-module (pref-id (om-get-current-view (tabs win)))))
                                                                                (update-pref-window win (pref-id (om-get-current-view (tabs win)))))
                                                                              )))
                                                 
                                                 (om-make-di 'om-button :size (om-make-point 80 24) :text "Apply" 
                                                             :di-action #'(lambda (item)
                                                                            (let ((win (om-view-window item)))
                                                                              (setf *user-preferences* (local-prefs win))
                                                                              (apply-all-preferences)
                                                                              (update-pref-window win (pref-id (om-get-current-view (tabs win))))
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



;;;====================================
;;; DEFAULT MODULE
;;;====================================

(add-preference-module :general "General")
(add-preference :general :user-name "User name" "me")










