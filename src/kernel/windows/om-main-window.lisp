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

;;;=================
;;; MAIN WINDOW
;;;=================

(defparameter *om-main-window* nil)


(defclass om-main-window (om-window)  
  ((elements-view :accessor elements-view :initform nil)
   (package-view :accessor package-view :initform nil)
   (libs-view :accessor libs-view :initform nil)
   (listener-view :accessor listener-view :initform nil)
   (main-layout :accessor main-layout :initform nil)))

; (show-main-om-window)

(defun show-main-om-window (&key front-tab)
  (if *om-main-window*
      (om-select-window *om-main-window*)
    (let ((win (om-make-window 'om-main-window
                               :title (format nil "~A Window~A"
                                              *app-name*
                                              (if *current-workspace* (list " [Workspace: " (name *current-workspace*) "]")
                                                ""))
                               :size (om-make-point 800 300)
                               :menu-items (om-menu-items nil) ;;; will be updated right after...
                               )))
      (setf (elements-view win) (make-ws-elements-tab)
            (package-view win) (make-om-package-tab)
            (libs-view win) (make-libs-tab)
            (listener-view win) (make-listener-tab))
      (om-set-menu-bar win (om-menu-items win))
      (om-add-subviews win (setf (main-layout win) 
                                 (om-make-layout 'om-tab-layout
                                                 :subviews (list (elements-view win) 
                                                                 (package-view win) 
                                                                 (libs-view win)
                                                                 (listener-view win)))))
      
      (case front-tab
        (:listener (om-set-current-view (main-layout win) (listener-view win)))
        (:documents (om-set-current-view (main-layout win) (elements-view win))))
      
      (setf *om-main-window* win)
      (om-show-window win))))

#+windows
(defmethod om-window-check-before-close ((self om-main-window)) 
  (om-y-or-n-dialog "Quit OM#?"))

#+windows
(defmethod om-window-close-event :after ((self om-main-window)) 
  (om-quit))


(defmethod om-window-close-event ((self om-main-window))
  (setf *om-main-window* nil))

;;; select-all works only in the documents view
(defmethod select-all-command ((self om-main-window))
  (cond ((equal (om-get-current-view (main-layout self))
                (elements-view self))
         #'(lambda () 
             (select-all-documents self)))
        ((equal (om-get-current-view (main-layout self))
                (listener-view self))
         #'(lambda () 
             (listener-view-select-all self)))
        (t nil)))
  
;;; copy-paste works only in the Listener view
(defmethod copy-command ((self om-main-window))
  (when (equal (om-get-current-view (main-layout self))
               (listener-view self))
    #'(lambda () 
        (listener-view-copy self))))

(defmethod cut-command ((self om-main-window))
  (when (equal (om-get-current-view (main-layout self))
               (listener-view self))
    #'(lambda () 
        (listener-view-cut self))))

(defmethod paste-command ((self om-main-window))
  (when (equal (om-get-current-view (main-layout self))
               (listener-view self))
    #'(lambda () 
        (listener-view-paste self))))

;;;===========================================
;;; WS TAB
;;;===========================================

(defun filter-ws-elements (elements filter)
  (cond ((equal filter :all) elements)
        ((equal filter :patches) (loop for elt in elements 
                                       when (and (mypathname elt)
                                                 (string-equal (pathname-type (mypathname elt)) "opat"))
                                       collect elt))
        ((equal filter :maquettes) (loop for elt in elements 
                                       when (and (mypathname elt)
                                                 (string-equal (pathname-type (mypathname elt)) "omaq"))
                                       collect elt))))


(defun gen-columns-list (names)
  (mapcar 
   #'(lambda (name)
       (case name
         (:name '(:title "filename" :adjust :left :visible-min-width 200))
         (:date '(:title "modified" :adjust :left :visible-min-width 150))
         (otherwise `(:title ,(string-downcase name) :adjust :left :visible-min-width 50))
         ))
   names))

(defun gen-column-elements (element display-params &optional editor)  
  (mapcar #'(lambda (item) 
              (cond ((equal item :name) (print-element-in-list element editor))
                    ((equal item :type) (string-downcase (get-object-type-name element)))
                    ((equal item :date) (cadr (create-info element)))))
          display-params))


(defun print-element-in-list (element editor)
  (if editor 
      (cond ((equal (elements-view-mode editor) :abs-path)
             (if (mypathname element) 
                 (namestring (mypathname element))
           (string+ (name element) " (no attached file)")))
            ((equal (elements-view-mode editor) :rel-path)
             (if (mypathname element) 
                 (namestring (relative-pathname (mypathname element) (mypathname (object editor))))
               (string+ (name element) " (no attached file)")))
            (t (name element))
            )
    (if (mypathname element) 
        (namestring (mypathname element))
      (string+ (name element) " (no attached file)"))
    ))


(defun make-ws-elements-tab ()
  (if *current-workspace*
      ;;; WORKSPACE VIEW
      (let* ((ed (editor *current-workspace*))
             (ws *current-workspace*)
             (display (list :name 
                            (and (get-pref-value :workspace :show-types) :type)
                            (and (get-pref-value :workspace :show-dates) :date)))
             (font (om-def-font :font1)))
        (let ((elements-list (om-make-di 'om-multicol-item-list
                                         :columns (gen-columns-list display)
                                         :items (filter-ws-elements (elements ws) (elements-view-filter ed))
                                         :column-function #'(lambda (item) (gen-column-elements item display))
                                         :fg-color #'(lambda (item) 
                                                       (if (and (mypathname item) (probe-file (mypathname item)))
                                                           (om-def-color :black) (om-make-color 0.8 0.2 0.2)))
                                         :font (om-def-font :mono)
                                         :alternating-background t
                                         :sort-styles 
                                         (list (list :name
                                                     (list :sort #'(lambda (x y) (string-greaterp (name x) (name y)))
                                                           :reverse  #'(lambda (x y)  (string-lessp (name x) (name y)))))
                                               (list :path
                                                     (list :sort #'(lambda (x y) (string-greaterp (namestring (mypathname x)) (namestring (mypathname y))))
                                                           :reverse  #'(lambda (x y) (string-lessp (namestring (mypathname x)) (namestring (mypathname y))))))
                                               (list :type
                                                     (list :sort #'(lambda (x y) (string-greaterp (pathname-type (mypathname x)) (pathname-type (mypathname y))))
                                                           :reverse  #'(lambda (x y) (string-lessp (pathname-type (mypathname x)) (pathname-type (mypathname y))))))
                                               (list :modif
                                                     (list :sort #'(lambda (x y) (string-greaterp (cadr (create-info x)) (cadr (create-info y))))
                                                           :reverse  #'(lambda (x y) (string-lessp (cadr (create-info x)) (cadr (create-info y))))))
                                               )
                                         )))

          
        (om-make-layout 
         'om-column-layout :name "Documents"
         :subviews (list 
                    elements-list
                    (om-make-layout 
                     'om-row-layout
                     :subviews (list (om-make-di 'om-simple-text :text "view"
                                                 :font font
                                                 :size (om-make-point 30 20))
                                     (om-make-di 'om-popup-list
                                                 :items '(:all :patches :maquettes)
                                                 :font font
                                                 :value (elements-view-filter ed)
                                                 :size (om-make-point 100 24)
                                                 :di-action #'(lambda (item) 
                                                                (setf (elements-view-filter ed) 
                                                                      (om-get-selected-item item))
                                                                (om-set-item-list 
                                                                 elements-list
                                                                 (filter-ws-elements (elements ws) (elements-view-filter ed)))
                                                                ))
                                     nil
                                     (om-make-di 'om-simple-text :text "sort"
                                                 :font font
                                                 :size (om-make-point 30 20))
                                     (om-make-di 'om-popup-list
                                                 :items '(:name :path :type :date-modified)
                                                 :value (elements-view-sort ed)
                                                 :font font
                                                 :size (om-make-point 100 24)
                                                 :di-action #'(lambda (item) 
                                                                (setf (elements-view-sort ed) 
                                                                      (om-get-selected-item item))
                                                                (om-sort-list-by elements-list (om-get-selected-item item))
                                                                ))
                                     nil
                                     (om-make-di 'om-simple-text :text "show"
                                                 :font font
                                                 :size (om-make-point 40 20)
                                                 )
                                     (om-make-di 'om-popup-list 
                                                 :size (om-make-point 100 24)
                                                 :font font
                                                 :value (elements-view-mode ed)
                                                 :items '(:name :abs-path :rel-path)
                                                 :di-action #'(lambda (item) 
                                                                (setf (elements-view-mode ed) 
                                                                      (om-get-selected-item item))
                                                                (om-invalidate-view elements-list)
                                                                )
                                                 )
                                     (om-make-di 
                                      'om-button :text ".." :font font
                                      :size (om-make-point 40 24) 
                                      :di-action #'(lambda (button) (declare (ignore button)) nil)
                                      ))
                     :ratios '(nil nil 2 1 1 1 1 1)
                     )
                                    
                    )
         ;:ratios '(nil nil)
         )))
    
    ;;; FILE VIEW
    (let ((doc-list 
           (om-make-di 
            'om-multicol-item-list
            :columns (gen-columns-list '(:name :type :date))
            :items (mapcar 'doc-entry-doc *open-documents*)
            :column-function #'(lambda (item) (gen-column-elements item '(:name :type :date)))
            :fg-color #'(lambda (item) 
                          (if (and (mypathname item) (probe-file (mypathname item)))
                              (om-def-color :black) (om-make-color 0.8 0.2 0.2)))
            :font (om-def-font :mono)
            :scrollbars t
            :alternating-background t
            :auto-reset-column-widths t
            :action-callback #'dbclicked-item-in-list
            :size (omp nil nil)
                     ;:sort-styles 
                     ;(list (list :name
                     ;            (list :sort #'(lambda (x y) (string-greaterp (name x) (name y)))
                     ;                  :reverse  #'(lambda (x y)  (string-lessp (name x) (name y)))))
                     ;      (list :path
                     ;            (list :sort #'(lambda (x y) (string-greaterp (namestring (mypathname x)) (namestring (mypathname y))))
                     ;                  :reverse  #'(lambda (x y) (string-lessp (namestring (mypathname x)) (namestring (mypathname y))))))
                     ;      (list :type
                     ;            (list :sort #'(lambda (x y) (string-greaterp (pathname-type (mypathname x)) (pathname-type (mypathname y))))
                     ;                  :reverse  #'(lambda (x y) (string-lessp (pathname-type (mypathname x)) (pathname-type (mypathname y))))))
                     ;      (list :modif
                     ;            (list :sort #'(lambda (x y) (string-greaterp (cadr (create-info x)) (cadr (create-info y))))
                     ;                  :reverse  #'(lambda (x y) (string-lessp (cadr (create-info x)) (cadr (create-info y))))))
                     ;      )
            )))
      
      (om-make-layout 
           'om-column-layout :name "Documents" :align :center ; :ratios '(1 nil nil)
           :subviews (list 
                      doc-list
                     
                    ;(om-make-di 'om-multi-text :enabled nil :size (om-make-point 300 20) 
                    ;            :font (om-def-font :font2)
                    ;            :fg-color (om-def-color :gray)
                    ;            :text "No Workspace has been created for this session")
                    ;(om-make-di 'om-button :enabled nil :size (om-make-point nil 24)
                    ;            :font (om-def-font :font2)
                    ;            :text "Create One?")

                      (om-make-layout 'om-row-layout :subviews
                                      (list
                                       nil
                                       (om-make-di 'om-button :text "Save selection" 
                                                   :size (omp 125 32) :font (om-def-font :font1)
                                                   :di-action #'(lambda (b) 
                                                                  (declare (ignore b))
                                                                  (save-documents doc-list)
                                                                  ))
                                       (om-make-di 'om-button :text "Close selection" 
                                                   :size (omp 125 32) :font (om-def-font :font1)
                                                   :di-action #'(lambda (b) 
                                                                  (declare (ignore b))
                                                                  (close-documents doc-list)
                                                                  ))))
                    
                      ))
      )
    ))


(defmethod select-all-documents ((window om-main-window))
  (let* ((view (elements-view window))
         (list (car (om-subviews view))))
    (dotimes (i (length *open-documents*))
      (om-select-item-index list i))))

(defmethod update-elements-tab ((window om-main-window))
  (let ((current (om-get-current-view (main-layout window))))
    (om-substitute-subviews (main-layout window) (elements-view window) (setf (elements-view window) (make-ws-elements-tab)))
    (om-set-current-view (main-layout window) current))
  )

(defun dbclicked-item-in-list (list)
  (mapc 'open-editor (om-get-selected-item list)))

(defun close-documents (list)
  (setf *save-apply-all* nil)
  (loop for doc in (om-get-selected-item list)
        do (if (editor-window doc)
               (close-editor doc) ;;; will close-document as well
             (close-document doc t)))
  (setf *save-apply-all* nil))

(defmethod close-document ((doc t) &optional force) nil)

(defun save-documents (list)
  (let ((selected-docs (om-get-selected-item list))
        (abort nil))
    (when (and (> (length selected-docs) 1)
               (find-if #'(lambda (doc) (null (mypathname doc))) selected-docs))
      (let ((action (om-y-or-n-dialog (format nil "Some documents in the list have no attached file yet ! ~%~% Select a common destination folder (Yes) or cancel (No)."))))
        (if action 
            (let ((folder (om-choose-directory-dialog)))
              (if folder
                (loop for doc in selected-docs 
                      when (null (mypathname doc))
                      do (setf (mypathname doc) (om-make-pathname :directory folder
                                                                  :name (name doc)
                                                                  :type (doctype-to-extension (object-doctype doc)))))
                (setf abort t)))
          (setf abort t))))
    (unless abort
      (setf *save-apply-all* :yes)
      (loop for doc in (om-get-selected-item list)
            do (save-document doc)))
    ))

(defmethod register-document :after ((self OMPersistantObject) &optional path)
  (when *om-main-window* (update-elements-tab *om-main-window*)))

(defmethod unregister-document :after ((self OMPersistantObject))
  (when *om-main-window* (update-elements-tab *om-main-window*)))

(defmethod update-document-path :after ((self OMPersistantObject))
  (when *om-main-window* (update-elements-tab *om-main-window*)))

(defmethod update-create-info :after ((self OMPersistantObject))
  (when *om-main-window* (update-elements-tab *om-main-window*)))



;;;; A FAIRE : drag file from/to finder
;(defun import-dragged-file (pane filename pos)
;  (let ((dirname (make-pathname :directory (append (pathname-directory filename) (list (pathname-name filename))))))
;    (cond ((or (string-equal "omp" (pathname-type filename)) 
;               (string-equal "omm" (pathname-type filename))
;               (string-equal "she" (pathname-type filename)))
;           (import-file-to-ws pane filename pos))
;          ((or 
;            (directoryp filename)
;            (and (directoryp dirname) (probe-file dirname)))
;          (make-new-folder pane dirname pos))
;        (t nil))
;    ))

;(defmethod om-import-files-in-app ((self workspacepanel) files)
;  (when (= 1 (length files))
;    (import-dragged-file self (pathname (car files)) (om-mouse-position self))))


;;;===========================================
;;; PACKAGES
;;;===========================================

(defmethod get-sub-items ((self OMAbstractPackage))
  (append (subpackages self) (classes self) (functions self) (special-items self)))

(defmethod get-sub-items ((self t)) nil)

(defmethod get-icon ((self OMAbstractPackage)) :icon-pack)
(defmethod get-icon ((self Function)) :icon-fun)
(defmethod get-icon ((self OMGenericFunction)) :icon-genfun)
(defmethod get-icon ((self OMClass)) :icon-class)
(defmethod get-icon ((self OMLib)) (if (loaded? self) :icon-lib-loaded :icon-lib))
(defmethod get-icon ((self symbol)) :icon-special)

(defparameter *packages-tab-text* "
The list on the left show the packages inbuilt in the environment.

Each packages contain classes (object constructors), functions, and/or special boxes.

Double click on one of these items and add it by just clicking in an patch window.
")

(defun make-om-package-tab ()
  (let ((pack-tree-view (om-make-tree-view (subpackages *om-package-tree*) 
                                           :size (omp 160 20)
                                           :expand-item 'get-sub-items
                                           :print-item 'get-name
                                           :font (om-def-font :font1)
                                           :bg-color (om-def-color :light-gray)
                                           :item-icon #'(lambda (item) (get-icon item))
                                           :icons (list :icon-pack :icon-fun :icon-genfun :icon-class :icon-special)
                                           ))
        (side-panel (om-make-di 'om-multi-text 
                                :size (om-make-point nil nil)
                                :font (om-def-font :font1)
                                :fg-color (om-def-color :dark-gray)
                                :text *packages-tab-text*)))
    (om-make-layout 
     'om-row-layout :name "Packages Library"
     :subviews (list pack-tree-view 
                     :divider 
                     side-panel)
     )))

(defmethod update-packages-tab ((window om-main-window))
  (om-substitute-subviews (main-layout window) (package-view window) (setf (package-view window) (make-om-package-tab)))
  (om-set-current-view (main-layout window) (package-view window)))



;;; !!! This should also work on the libraries tab below

(defmethod om-selected-item-from-tree-view ((self function) (window om-main-window)) 
  (show-doc-on-main-window (function-name self) window))

(defmethod om-selected-item-from-tree-view ((self standard-class) (window om-main-window)) 
  (show-doc-on-main-window (class-name self) window))

(defmethod om-selected-item-from-tree-view ((self symbol) (window om-main-window)) 
  (show-doc-on-main-window self window))
 

(defmethod show-doc-on-main-window ((self symbol) (window om-main-window))
  (let ((doc-info (get-documentation-info self))
        (view (om-get-current-view (main-layout window))))
    (when (or (equal view (libs-view window))
              (equal view (package-view window))))
      (let ((info-text (nth 2 (om-subviews view))))
        (om-set-fg-color info-text (om-def-color :dark-gray))
        (om-set-dialog-item-text 
         info-text 
         (if doc-info 
             (format nil "~%~A (~A)~%~%~A" 
                     (string-upcase (nth 0 doc-info))
                     (string-downcase (nth 1 doc-info))
                     (or (nth 3 doc-info) "-"))
           (format nil "~%~A (no documentation)" (string-upcase self)))
         )
        )))


(defmethod om-selected-item-from-tree-view ((self OMPackage) (window om-main-window)) 
  (let ((view (om-get-current-view (main-layout window))))
    (when (or (equal view (libs-view window))
              (equal view (package-view window))))
      (let ((info-text (nth 2 (om-subviews view))))
        (om-set-fg-color info-text (om-def-color :black))
        (om-set-dialog-item-text 
         info-text 
         (format nil "~%Package: ~A~%~%~A" 
                 (string-upcase (name self)) (or (doc self) "-"))
         )
        )))


;;;===========================================
;;; LIBRARIES
;;;===========================================


(defparameter *libs-tab-text* "
The list on the left show all libraries found in the libraries search paths.

- Double click on a library icon to load it.
- Double click on an internal item's icon (of a loaded library) and add it by just clicking in an patch window.
")


(defun make-libs-tab ()
  (let ((libs-tree-view (om-make-tree-view (subpackages *om-libs-root-package*) 
                                             :size (omp 120 20)
                                             :expand-item 'get-sub-items
                                             :print-item 'get-name
                                             :font (om-def-font :font1)
                                             :bg-color (om-def-color :light-gray)
                                             :item-icon #'(lambda (item) (get-icon item))
                                             :icons (list :icon-pack :icon-fun :icon-genfun :icon-class :icon-lib-loaded :icon-lib)
                                             ))
          (side-panel 
           (om-make-di 
            'om-multi-text 
            :size (om-make-point nil nil)
            :font (om-def-font :font1) 
            :fg-color (om-def-color :dark-gray)
            :text *libs-tab-text*)))
          
      (om-make-layout 
       'om-row-layout :name "External Libraries"
       :subviews (list 
                  (om-make-layout 
                   'om-column-layout  :align :right
                   :subviews (list libs-tree-view
                                   (om-make-di 'om-button :size (om-make-point nil 24)
                                               :font (om-def-font :font2) 
                                               :text "Refresh list"
                                               :di-action #'(lambda (b) 
                                                              (declare (ignore b))
                                                              (update-registered-libraries)
                                                              (update-libraries-tab *om-main-window*)))))
                  :divider 
                  side-panel))
      ))


(defmethod update-libraries-tab ((window om-main-window))
  (om-substitute-subviews (main-layout window) (libs-view window) (setf (libs-view window) (make-libs-tab)))
  (om-set-current-view (main-layout window) (libs-view window)))

(defmethod om-double-clicked-item-from-tree-view ((self OMLib) (window om-main-window)) 
  (when (or (not (loaded? self))
            (om-y-or-n-dialog (format nil "The library '~A' is already loaded. Reload it ?" (name self))))
    (load-om-library self)
    (update-libraries-tab window)

    ))

(defmethod om-selected-item-from-tree-view ((self OMLib) (window om-main-window)) 
  (let* ((view (libs-view window))
         (info-text (nth 2 (om-subviews view))))
    (om-set-fg-color info-text (om-def-color :black))
    (om-set-dialog-item-text 
     info-text 
     (format nil 
             "~%~A~%Version: ~A~%~%Location: ~A~%~%Author(s):~%~A~%~%Description:~%~A" 
             (string-upcase (name self))
             (or (version self) "-")
             (mypathname self) 
             (or (author self) "Unknown")
             (or (doc self) "-"))
     )
    ))
    

(defvar *add-item-on-patch* nil)

(defun set-add-item-on-patch (item)
  (setf *add-item-on-patch* item)
  (om-reset-mouse-motion)
  (om-set-view-cursor *om-main-window* (om-get-cursor :add)))

(defun cancel-add-item-on-patch ()
  (when *add-item-on-patch*
    (setf *add-item-on-patch* nil)
    (om-reset-mouse-motion)
    (om-set-view-cursor *om-main-window* nil)))

(defmethod om-view-cursor :around ((self t))
  (if *add-item-on-patch*
      (om-get-cursor :add) 
    (call-next-method)))

(defmethod om-view-key-handler :around ((self om-graphic-object) key)
  (declare (ignore key))
  (cancel-add-item-on-patch)
  (call-next-method))

(defmethod om-view-click-handler :around ((self om-graphic-object) pos)
  (declare (ignore pos))
  (call-next-method)
  (cancel-add-item-on-patch))




(defmethod om-double-clicked-item-from-tree-view ((self function) (window om-main-window)) 
  (set-add-item-on-patch (function-name self)))
(defmethod om-double-clicked-item-from-tree-view ((self standard-class) (window om-main-window)) 
  (set-add-item-on-patch (class-name self)))
(defmethod om-double-clicked-item-from-tree-view ((self symbol) (window om-main-window)) 
  (set-add-item-on-patch self))



;;; UNSELECT
(defmethod om-selected-item-from-tree-view ((self null) (window om-main-window)) 
  (let ((view (om-get-current-view (main-layout window))))
    (cond 
     ((equal view (libs-view window))
      (let ((info-text (nth 2 (om-subviews view))))
        (om-set-fg-color info-text (om-def-color :dark-gray))
        (om-set-dialog-item-text info-text *libs-tab-text*))) 

     ((equal view (package-view window))
      (let ((info-text (nth 2 (om-subviews view))))
        (om-set-fg-color info-text (om-def-color :dark-gray))
        (om-set-dialog-item-text info-text *packages-tab-text*))) 
     (t nil))))



;;;===========================================
;;; LISTENER
;;;===========================================

(defun make-listener-tab ()
  
  (let ((listener-pane (om-lisp::om-make-listener-output-pane (get-pref-value :general :listener-font))))
    
    (om-make-layout 
     'om-column-layout  :name "Listener"
     :ratios '(1 nil) :delta 0
     :subviews (list 
                
                ;; main pane
                listener-pane
                
                (om-make-layout 'om-row-layout :subviews
                                (list
                                 (om-make-di 'om-button :text "Open as separate window" 
                                             :size (omp 180 32) :font (om-def-font :font1)
                                             :di-action #'(lambda (b) 
                                                            (declare (ignore b))
                                                            (show-listener-win)
                                                            ))
                                 nil
                                 (om-make-di 'om-button :text "x" 
                                             :size (omp 40 32) :font (om-def-font :font1)
                                             :di-action #'(lambda (b) 
                                                            (declare (ignore b))
                                                            (om-lisp::om-clear-listener-output-pane listener-pane)
                                                            ))))
                ))))


(defmethod listener-view-copy ((window om-main-window))
  (let* ((view (listener-view window))
         (pane (car (om-subviews view))))
    (om-lisp::om-copy-command pane)))

(defmethod listener-view-cut ((window om-main-window))
  (let* ((view (listener-view window))
         (pane (car (om-subviews view))))
    (om-lisp::om-cut-command pane)))

(defmethod listener-view-paste ((window om-main-window))
  (let* ((view (listener-view window))
         (pane (car (om-subviews view))))
    (om-lisp::om-paste-command pane)))

(defmethod listener-view-select-all ((window om-main-window))
  (let* ((view (listener-view window))
         (pane (car (om-subviews view))))
    (om-lisp::om-select-all-command pane)))


(defun prompt-on-main-window-listener (message)
  (when (and *om-main-window*
             (equal (om-get-current-view (main-layout *om-main-window*)) (listener-view *om-main-window*)))
    (let ((listener-pane (car (om-subviews (listener-view *om-main-window*)))))
      (when listener-pane 
        (om-lisp::om-prompt-on-echo-area listener-pane message)))))

