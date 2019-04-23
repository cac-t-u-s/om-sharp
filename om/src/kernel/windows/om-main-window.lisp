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
;;;=================
;;; MAIN WINDOW
;;;=================

(defparameter *om-main-window* nil)


(defclass om-main-window (om-window)  
  ((ws-elements-view :accessor elements-view :initform nil)
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
                               :menu-items (om-menu-items nil))))
      (setf (elements-view win) (make-ws-elements-tab)
            (package-view win) (make-om-package-tab)
            (libs-view win) (make-libs-tab)
            (listener-view win) (make-listener-tab))
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

(defmethod om-window-close-event ((self om-main-window))
  (setf *om-main-window* nil))

;;;===========================================
;;; WS TAB
;;;===========================================

(defun filter-ws-elements (elements filter)
  (cond ((equal filter :all) elements)
        ((equal filter :patches) (loop for elt in elements 
                                       when (and (mypathname elt)
                                                 (string-equal (pathname-type (mypathname elt)) "omp"))
                                       collect elt))
        ((equal filter :maquettes) (loop for elt in elements 
                                       when (and (mypathname elt)
                                                 (string-equal (pathname-type (mypathname elt)) "omm"))
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
                                         :font (om-make-font "Courier" 12)
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
                                      :di-action #'(lambda (button) nil)
                                      ))
                     :ratios '(nil nil 2 1 1 1 1 1)
                     )
                                    
                    )
         ;:ratios '(nil nil)
         )))
    
    ;;; FILE VIEW
    (om-make-layout 
         'om-column-layout :name "Documents" :align :center ; :ratios '(1 nil nil)
         :subviews (list 
                    (om-make-di 
                     'om-multicol-item-list
                     :columns (gen-columns-list '(:name :type :date))
                     :items (mapcar 'doc-entry-doc *open-documents*)
                     :column-function #'(lambda (item) (gen-column-elements item '(:name :type :date)))
                     :fg-color #'(lambda (item) 
                                   (if (and (mypathname item) (probe-file (mypathname item)))
                                       (om-def-color :black) (om-make-color 0.8 0.2 0.2)))
                     :font (om-make-font "Courier" 12)
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
                     )
                     
                    ;(om-make-di 'om-multi-text :enable nil :size (om-make-point 300 20) 
                    ;            :font (om-def-font :font2)
                    ;            :fg-color (om-def-color :gray)
                    ;            :text "No Workspace has been created for this session")
                    ;(om-make-di 'om-button :enable nil :size (om-make-point nil 24)
                    ;            :font (om-def-font :font2)
                    ;            :text "Create One?")
                    ))
    ))


(defmethod update-elements-tab ((window om-main-window))
  (om-substitute-subviews (main-layout window) (elements-view window) (setf (elements-view window) (make-ws-elements-tab)))
  (om-set-current-view (main-layout window) (elements-view window)))

(defun dbclicked-item-in-list (list)
  (mapc 'open-editor (om-get-selected-item list)))

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


(defun make-om-package-tab ()
  (let* ((pack *om-package-tree*))
    (let ((pack-tree-view (om-make-tree-view (subpackages *om-package-tree*) 
                                             :size (omp 300 20)
                                             :expand-item 'get-sub-items
                                             :print-item 'get-name
                                             :font (om-def-font :font1)
                                             :bg-color (om-def-color :light-gray)
                                             :item-icon #'(lambda (item) (get-icon item))
                                             :icons (list :icon-pack :icon-fun :icon-genfun :icon-class :icon-special)
                                             ))) 
      (om-make-layout 
       'om-simple-layout :name "Class/Function Library"
       :subviews (list pack-tree-view)
       ))))

(defmethod update-packages-tab ((window om-main-window))
  (om-substitute-subviews (main-layout window) (package-view window) (setf (package-view window) (make-om-package-tab)))
  (om-set-current-view (main-layout window) (package-view window)))

;;;===========================================
;;; LIBRARIES
;;;===========================================

(defun make-libs-tab ()
  (let* ((pack *om-libs-root-package*))
    (let ((libs-tree-view (om-make-tree-view (subpackages *om-libs-root-package*) 
                                             :size (omp 300 20)
                                             :expand-item 'get-sub-items
                                             :print-item 'get-name
                                             :font (om-def-font :font1)
                                             :bg-color (om-def-color :light-gray)
                                             :item-icon #'(lambda (item) (get-icon item))
                                             :icons (list :icon-pack :icon-fun :icon-genfun :icon-class :icon-lib-loaded :icon-lib)
                                             )))
      (om-make-layout 
       'om-column-layout :name "External Libraries" :align :right
       :subviews (list libs-tree-view
                       (om-make-di 'om-button :size (om-make-point nil 24)
                                :font (om-def-font :font2) 
                                :text "Refresh list"
                                :di-action #'(lambda (b) 
                                               (update-registered-libraries)
                                               (update-libraries-tab *om-main-window*))))
       ))))


(defmethod update-libraries-tab ((window om-main-window))
  (om-substitute-subviews (main-layout window) (libs-view window) (setf (libs-view window) (make-libs-tab)))
  (om-set-current-view (main-layout window) (libs-view window)))

(defmethod om-clicked-item-from-tree-view ((self OMLib) (window om-main-window)) 
  (when (or (not (loaded? self))
            (om-y-or-n-dialog (format nil "The library '~A' is already loaded. Reload ?" (name self))))
    (load-om-library self)
    (update-libraries-tab window)
    ))


;;;===========================================
;;; LISTENER
;;;===========================================

(defun make-listener-tab ()
  
  (let ((listener-pane (om-lisp::om-make-listener-output-pane)))
    
    (om-make-layout 
     'om-column-layout  :name "Listener"
     :ratios '(1 nil) :delta 0
     :subviews (list 
                
                ;; main pane
                listener-pane
                
                (om-make-layout 'om-row-layout :subviews
                                (list
                                 nil
                                 (om-make-di 'om-button :text "x" 
                                             :size (omp 40 32) :font (om-def-font :font1)
                                             :di-action #'(lambda (b) 
                                                            (om-lisp::om-clear-listener-output-pane listener-pane)
                                                            ))))
                ))))


(defun prompt-on-main-window-listener (message)
  (when (and *om-main-window*
             (equal (om-get-current-view (main-layout *om-main-window*)) (listener-view *om-main-window*)))
    (let ((listener-pane (car (om-subviews (listener-view *om-main-window*)))))
      (when listener-pane 
        (om-lisp::om-prompt-on-echo-area listener-pane message)))))

