

;;;===========================
;;; A PACKAGE FOR OM USER
;;;===========================

(defpackage "OM-USER"
    (:use "COMMON-LISP" "CL-USER" "OM")
    (:import-from "OM"))


;;;===========================
;;; OM SESSION: STARTUP/QUIT
;;;===========================

(in-package :om)


;(defparameter *om-version* *version*)
;(defparameter *version-string* *version-str*)
;(setf *om-version* *version*)
;(setf *version-string* *version-str*)


;;;======================================
;;; OM GENERAL PREFERENCES
;;;======================================

(defvar *om-preferences* nil)

(defun om-preference-file ()
  (let* ((userpref (om-user-pref-folder)))
    (make-pathname
     :device (pathname-device userpref)
     :directory (append (pathname-directory userpref) (list "OpenMusic" 
                                                            (format nil "~A~D" *app-name* (round *om-version*))))
     :name "preferences" :type "lisp")))

;(WITH-OPEN-FILE (out "/Users/bresson/Desktop/test.txt" :direction :output 
;                         :if-does-not-exist :create :if-exists :supersede) 
;      (format out "~A~%" (list :om-version (+ 2 3))))
;     (prin1 `(:om-version ,(+ 3 3)) out))

(defmethod save-om-preferences ()
  (let ((path (om-preference-file)))
    (om-create-directory (make-pathname :directory (pathname-directory path)) :if-exists nil)
    (with-open-file (out path :direction :output 
                         :if-does-not-exist :create 
                         :if-exists :supersede) 
      (let ((*print-pretty* t))
        (print `(:info (:om-version ,*om-version*) (:saved ,(om-get-date))) out)
        (mapcar #'(lambda (item) 
                    (print (list (car item) (omng-save (cadr item))) out))
                (remove-if #'(lambda (item) (find item '(:preferences :info)))
                           *om-preferences* :key 'car))
        (unless *current-workspace*
          (print `(:preferences
                   ,.(mapcar #'(lambda (item) 
                                 (save-pref-module (car item)))
                             *user-preferences*))
                 out))
        ))
    path))

; (save-om-preferences)
;(set-om-pref :prev-ws #P"/Users/bress/")
;(list-from-file (om-preference-file))
;(cdr (find :om-preferences (list-from-file (om-preference-file)) :test 'equal :key 'car))

;(read-om-preferences)

(defmethod read-om-preferences ()
  (let* ((path (om-preference-file))
         (pr-list (and (file-exist-p path)
                       (list-from-file path))))
    (setf *om-preferences* (mapcar #'(lambda (item) 
                                       (list (car item) (omng-load (cadr item))))
                                   pr-list))
    ))

; (get-om-pref :prev-ws)

;;; preflist attached to <key>
(defun get-om-pref-list (key)
  (cdr (find key *om-preferences* :test 'equal :key 'car)))

;;; short-hand when there is only one value
(defun get-om-pref (key)
  (cadr (find key *om-preferences* :test 'equal :key 'car)))



(defun set-om-pref (key val)
  (let ((pos (position key *om-preferences* :test 'equal :key 'car)))
    (if pos 
      (setf (nth pos *om-preferences*) (list key val))
      (setf *om-preferences* (append *om-preferences* (list (list key val)))))))


;;;======================================
;;; THE 'PATCHES' FOLDER CONTAIN CHANGES TO LOAD BEFORE STARTUP
;;;======================================

(defun load-modif-patches ()
  (let ((patches-folder (om-relative-path '("init") nil :om)))
    (om-with-redefinitions
      (let ((*load-verbose* t))
        (when (probe-file patches-folder)
          (mapc #'(lambda (file)
                    (load file :verbose t)) 
                (sort (om-directory patches-folder :type (list "lisp" *om-compiled-type*) :files t :directories nil)
                      'string< :key 'pathname-name)))
        ))))



;;;======================================
;;; OM-RELATED STARTUP FUNCTIONS
;;;======================================

;;; called in delivered app init
(defun om-root-init ()
  (when (om-standalone-p)
      (om-set-root-folder 
       (make-pathname 
        :device (pathname-device (oa::om-lisp-image)) :host (pathname-host (oa::om-lisp-image))
        :directory 
        #+cocoa(butlast (pathname-directory (truename (PATHNAME-LOCATION (oa::om-lisp-image)))) 3)
        #+win32(pathname-directory (truename (PATHNAME-LOCATION  (oa::om-lisp-image))))
        ))))

;;;======================================
;;; USER-RELATED STARTUP FUNCTIONS
;;;======================================

(defvar *ws-init-funcs* nil "Functions to load after the workspace is loaded")

(defun add-ws-init-func (func-name)
  (unless (find func-name *ws-init-funcs* :test 'equal)
    (setf *ws-init-funcs* (list func-name))))

(defun ws-init-funcall ()
  (mapc #'(lambda (x) (funcall x)) *ws-init-funcs*)
  t)


;;;======================================
;;; START OM WITHOUT WORKSPACE
;;;======================================

(defun start-without-ws-file () 
  (set-om-pref :prev-ws nil)
  t)

;;;======================================
;;; MAIN START FUNCTION
;;;======================================

;;; will be printed in the Listener
(defparameter *om-startup-string* 
  (format nil 
          "==================================~%OpenMusic v. ~D~%r. ~A~%(c) IRCAM - Representations Musicales~%http://repmus.ircam.fr/openmusic/~%================================="
          *version-string* *release-date*))

; (start-openmusic)

(defun start-openmusic ()
  (push :om *features*)
  (oa::om-api-init)
  (om-fi::om-load-foreign-libs 
   #+windows (oa::om-lisp-image) 
   #+macosx (om-relative-path '("resources" "lib" "mac") nil (oa::om-root-folder))
   #+linux (om-relative-path '("resources" "lib" "linux") nil (oa::om-root-folder))
   )
  
   (load-modif-patches)
  #+cocoa(objc:make-autorelease-pool)
  ;(clos::set-clos-initarg-checking nil)
  (setf *print-case* :downcase)
  (setf *catch-errors* nil)
  (in-package :om)
  ;;(om::set-language *release-language*)
  (om-init-funcall)
    
  #+(and om-deliver mswindows)
  (define-action "Confirm when quitting image" "Prompt for confirmation" 'om::quit-om-callback)
  
  ;;; read the general OM prefs in user
  (read-om-preferences)
  ;;; stores default values for all pref modules
  (restore-default-preferences)
  ;;; applies preferences in OM prefs (if any)
  (load-saved-prefs (get-om-pref-list :preferences))
  
  ;;; start workspace (maybe)
  ;(start-workspace)
  
  (save-om-preferences)
  
  ;;;(in-package :om-user)
  (show-listener-win)

  (capi::execute-with-interface om-lisp::*om-listener* #'(lambda () (in-package :om)))
  )


;;;=================
;;; WS STARTUP DIALOG
;;;=================

; (ws-dialog)

(defun ws-dialog (&optional previous-ws)
  (let* ((font (om-def-font :font2))
         (smallfont (om-def-font :font1))
         (win (om-make-window 'om-dialog :size (om-make-point 400 350)
                              :resizable nil :maximize nil :minimize nil
                              :border 10 :title "OpenMusic - Starup"
                              :win-layout (om-make-layout 'om-column-layout :ratios '(1 nil) :delta 20 :align :right)
                              :owner nil :focus t
                              ))
         (view (om-make-view 'om-view :size (om-make-point 350 300) 
                             :bg-color (om-def-color :white) :enable nil))
         (y 10) prev exist new no-ws)
    (om-add-subviews win view)
    
    (om-add-subviews view
                     (om-make-di 'om-simple-text 
                                 :position (om-make-point 15 y) 
                                 :size (om-make-point 340 22)
                                 :text (format nil "Choose or create a workspace:")
                                 :font (om-def-font :font2))
                     (om-make-di 'om-multi-text 
                                 :position (om-make-point 25 (+ y 25)) 
                                 :size (om-make-point 350 40)
                                 :text (format nil "The workspace allows to manage your data and preferences and to store/retrieve your OM sessions.")
                                 :fg-color (om-def-color :dark-gray)
                                 :bg-color (om-def-color :white)
                                 :font smallfont))
    (incf y 60)
    (om-add-subviews view
                     (setf prev (om-make-di 'om-radio-button :position (om-make-point 20 y) :size (om-make-point 200 20)
                                            :text " Open previous workspace"
                                            :checked-p (and previous-ws (probe-file (pathname previous-ws)))
                                            :enable (and previous-ws (probe-file (pathname previous-ws)))
                                            :font font
                                             :bg-color (om-def-color :white)
                                            :radio-button-cluster 'proj
                                            ))
                     (om-make-di 'om-multi-text :position (om-make-point 40 (+ y 24)) :size (om-make-point 320 32)
                                 :text (if previous-ws 
                                           (string+ (namestring previous-ws)
                                                    (if (probe-file (pathname previous-ws)) "" " [not found]"))
                                         "...")
                                  :bg-color (om-def-color :white)
                                  :fg-color (om-def-color (if (and previous-ws (probe-file (pathname previous-ws))) :dark-gray :dark-gray))
                                  :font smallfont))
    (incf y 55)
    (om-add-subviews view
                     (setf exist (om-make-di 'om-radio-button :position (om-make-point 20 y) :size (om-make-point 200 20)
                                             :text " Open a workspace"
                                             :checked-p (and previous-ws (not (probe-file (pathname previous-ws))))
                                             :font font
                                             :bg-color (om-def-color :white)
                                             :radio-button-cluster 'proj
                                             ))
                     (om-make-di 'om-multi-text :position (om-make-point 40 (+ y 24)) :size (om-make-point 290 16)
                                 :text "[Select the main \".omws\" workspace file]"
                                 :fg-color (om-def-color :dark-gray)
                                 :bg-color (om-def-color :white)
                                 :font smallfont))
    (incf y 50)
    (om-add-subviews view
                     (setf new (om-make-di 'om-radio-button :position (om-make-point 20 y) :size (om-make-point 200 20)
                                           :text " Create a new workspace"
                                           :checked-p nil
                                           :font font
                                           :bg-color (om-def-color :white)
                                           :radio-button-cluster 'proj
                                           )))
    (incf y 40)
    (om-add-subviews view
                     (om-make-di 'om-simple-text 
                                 :position (om-make-point 15 y) 
                                 :size (om-make-point 340 22)
                                 :text (format nil "Start without workspace:")
                                 :font (om-def-font :font2)))
    (incf y 30)
    (om-add-subviews view
                     (setf no-ws (om-make-di 'om-radio-button :position (om-make-point 20 y) :size (om-make-point 300 20)
                                             :text " I will create a workspace later"
                                             :font font
                                             :checked-p (not previous-ws)
                                             :bg-color (om-def-color :white)
                                             :enable t
                                             :font font
                                             :radio-button-cluster 'proj
                                             ))
                     
                     )
 
    (om-add-subviews win
                     (setf rl (om-make-layout 'om-row-layout 
                                              :subviews (list 
                                                         (setf ok (om-make-di 'om-button :size (om-make-point 80 24) 
                                                                              :text "OK"
                                                                              :default t 
                                                                              :focus t
                                                                              :di-action #'(lambda (button)
                                                                                             (declare (ignore button))
                                                                                             (om-return-from-modal-dialog 
                                                                                              win 
                                                                                              (cond 
                                                                                               ((and prev (om-checked-p prev)) 'previous)
                                                                                               ((om-checked-p exist) 'existing)
                                                                                               ((om-checked-p new) 'new)
                                                                                               ((om-checked-p no-ws) 'no)
                                                                                               (t nil))))
                                                            ))
                                                         
                                                         (om-make-di 'om-button :size (om-make-point 80 24) :text "Quit"
                                                                     :di-action #'(lambda (button)
                                                                                    (declare (ignore button))
                                                                                    (om-return-from-modal-dialog win 'quit))))
                                              )
                           )
                     )
    
    (om-modal-dialog win)
    ))

; (ws-dialog)




;;;======================================
;;; QUIT
;;;======================================

(defun quit-om-callback () 
  (let ((rep (and (om-y-or-n-dialog "Quit OpenMusic ?")
                  (om-lisp::check-buffers-before-close)
                  (check-om-docs-before-close))))
    (when rep 
      (oa::om-api-exit))
    rep))
