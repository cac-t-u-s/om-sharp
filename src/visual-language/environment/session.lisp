;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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


;;;===========================
;;; SESSION: STARTUP/QUIT
;;;===========================

(in-package :om)

;;;======================================
;;; GENERAL PREFERENCES
;;;======================================

(defun om-preference-file ()
  (merge-pathnames "om#/preferences.om#" (om-user-pref-folder)))


(defvar *last-open-ws* nil)
(defun remember-previous-ws (&optional (path nil path-supplied-p))
  (when path-supplied-p
    (setf *last-open-ws* path))
  *last-open-ws*)


(defmethod save-om-preferences ()

  (let ((path (om-preference-file)))
    (om-create-directory (make-pathname :directory (pathname-directory path)))
    (with-open-file (out path :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (let ((*print-pretty* t))
        (pprint `(:info (:saved ,(om-get-date)) (:version ,*version*)) out)
        (pprint `(:previous-ws ,(omng-save *last-open-ws*)) out)
        (pprint `(:recent-files ,(omng-save (mapcar 'namestring *om-recent-files*))) out)
        ;;; if there is a workspace the preferences will be stored in that workspace
        (unless *current-workspace*
          (pprint `(:user-preferences
                    ,.(mapcar #'save-pref-module *user-preferences*))
                  out))
        ))
    path))


;(save-om-preferences)
;(set-om-pref :prev-ws #P"/Users/bress/")
;(list-from-file (om-preference-file))
;(cdr (find :user-preferences pr-list :test 'equal :key 'car)

;;; reads one preference in the preference file
(defmethod read-om-preference (key)
  (let* ((path (om-preference-file))
         (pref-list (and (file-exists-p path)
                         (list-from-file path))))
    (find-values-in-prop-list pref-list key)))

;;; read and loads the main prefs for a session
(defmethod load-om-preferences ()
  (let* ((path (om-preference-file))
         (pr-list (and (file-exists-p path)
                       (list-from-file path))))

    (setq *om-recent-files* (omng-load (find-value-in-kv-list pr-list :recent-files)))
    (load-saved-prefs (find-values-in-prop-list pr-list :user-preferences))
    ))


;;;======================================
;;; THE 'PATCHES' FOLDER CONTAIN CHANGES TO LOAD BEFORE STARTUP
;;;======================================


;;; pb: om-directory won't work on containing folder if the app is on quarantine
#|
(defun get-app-name ()
  #+macosx(let ((app-bundle (find ".app" (om-directory (om-root-folder) :files nil)
                                  :test 'search :key #'(lambda (dir) (car (last (pathname-directory dir)))))))
            (when app-bundle
              (car (last (pathname-directory app-bundle)))))
  #+windows(find "exe" (om-directory (om-root-folder) :files t :directories nil)
                 :test 'string-equal :key 'pathname-type))
|#

(defun get-app-name ()
  (string+ *app-name*
           #+macosx ".app"
           #+windows ".exe"
           ))

(defun get-init-patches-folder ()
  (merge-pathnames (make-pathname
                    :directory
                    (cons :relative
                          #-macosx(list "init")
                          #+macosx(if (member :om-deliver *features*)
                                      (list (get-app-name) "Contents" "Init")
                                    (list "init"))
                          ))
                   (om-root-folder)))

(defun load-modif-patches ()
  (let ((patches-folder (get-init-patches-folder)))
    (om-with-redefinitions
      (let ((*load-verbose* t))
        (when (probe-file patches-folder)
          (mapc #'(lambda (file)
                    (load file :verbose t))
                (sort (om-directory patches-folder :type (list "lisp" (om-compiled-type)) :files t :directories nil)
                      'string< :key 'pathname-name)))
        ))))


(defun load-code-in-folder (folder &optional (recursive t))
  (let ((*load-verbose* t))
    (mapc #'(lambda (file)
              (if (and recursive (om-directory-pathname-p file))
                  (load-code-in-folder file recursive)
                (load file :verbose t :print t)
                ))
          (sort (om-directory folder :type (list "lisp" (om-compiled-type)) :files t :directories t)
                'string< :key 'pathname-name))
    ))

(defun load-user-code ()
  (let ((user-code-folder (get-pref-value :general :user-code)))
    (when (and user-code-folder (probe-file user-code-folder))
      (load-code-in-folder user-code-folder))))

; (load-user-code)

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
      #+cocoa(butlast (pathname-directory (truename (lw::pathname-location (oa::om-lisp-image)))) 3)
      #+win32(pathname-directory (truename (lw::pathname-location  (oa::om-lisp-image))))
      #+linux (append (butlast (pathname-directory (truename (lw::pathname-location (oa::om-lisp-image))))) '("share" "om-sharp"))
      ))
    (om-lisp::om-set-source-tree-root-folder
     (make-pathname
      :device (pathname-device (oa::om-lisp-image)) :host (pathname-host (oa::om-lisp-image))
      :directory
      #+cocoa(append (butlast (pathname-directory (truename (lw::pathname-location (oa::om-lisp-image))))) '("Resources" "src"))
      #+win32(append (pathname-directory (truename (lw::pathname-location (oa::om-lisp-image)))) '("src"))
      #+linux (append (butlast (pathname-directory (truename (lw::pathname-location (oa::om-lisp-image))))) '("share" "om-sharp" "src"))
      ))
    ))

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
;;; START WITHOUT WORKSPACE
;;;======================================

(defun start-without-ws-file ()
  (remember-previous-ws nil)
  t)

;;;======================================
;;; MAIN START FUNCTION
;;;======================================

;;; will be printed in the Listener


(defparameter *om-startup-string*
  (format nil
          "===========================~%~A v~D~%r. ~A~%==========================="
          *app-name* *version-string* *release-date*))

; (start-omsharp)

(defvar *om-initialized* nil)

(defun start-omsharp ()
  (push :om *features*)
  (oa::om-api-init)
  (om-fi::om-load-foreign-libs
   #+windows (oa::om-lisp-image)
   #+macosx (if (oa::om-standalone-p)
                (om-make-pathname :directory (append (butlast (pathname-directory (oa::om-lisp-image))) '("Frameworks")))
              (om-relative-path '("resources" "lib" "mac") nil (oa::om-root-folder)))
   #+linux (if (oa::om-standalone-p)
               (om-make-pathname :directory (append (butlast (pathname-directory (oa::om-lisp-image))) '("lib64" "om-sharp")))
             (om-relative-path '("resources" "lib" "linux") nil (oa::om-root-folder))))

  (load-modif-patches)

  #+cocoa(objc:make-autorelease-pool)
  (editor:setup-indent "defmethod*" 2 2 2)
  (editor:setup-indent "defmethod!" 2 2 2)
  (editor:setup-indent "defclass*" 2 2 2)
  (editor:setup-indent "defclass!" 2 2 2)
  ;(clos::set-clos-initarg-checking nil)
  (setf *print-case* :downcase)

  (in-package :om)

  ;(show-listener-win)

  ;;(om::set-language *release-language*)
  (register-om-icons)

  ;; #+(or om-deliver mswindows)
  (lispworks::define-action "Confirm when quitting image" "Prompt for confirmation" 'om::quit-om-callback)

  (om-lisp::om-init-output-stream)

  ;;; read the general prefs
  (load-om-preferences)

  (when (find-om-package :midi) (midi-apply-ports-settings))

  (om-init-funcall)

  ;;; start workspace (maybe)
  ;(start-workspace)
  (register-all-libraries)

  (save-om-preferences)

  ;;;(in-package :om-user)

  (show-main-om-window :front-tab :listener)
  (capi:execute-with-interface *om-main-window* 'eval '(in-package :om))
  (om-print-format *om-startup-string*)

  (load-user-code)

  (setf *om-initialized* t)
  )


;;;==============================
;;; WS STARTUP DIALOG (not used)
;;;==============================

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
                             :bg-color (om-def-color :white)))
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
                                 :text (format nil "The workspace allows to manage your data and preferences and to store/retrieve your sessions.")
                                 :fg-color (om-def-color :dark-gray)
                                 :bg-color (om-def-color :white)
                                 :font smallfont))
    (incf y 60)
    (om-add-subviews view
                     (setf prev (om-make-di 'om-radio-button :position (om-make-point 20 y) :size (om-make-point 200 20)
                                            :text " Open previous workspace"
                                            :checked-p (and previous-ws (probe-file (pathname previous-ws)))
                                            :enabled (and previous-ws (probe-file (pathname previous-ws)))
                                            :font font
                                            :bg-color (om-def-color :white)
                                            :button-group 'proj
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
                                             :button-group 'proj
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
                                           :button-group 'proj
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
                                             :enabled t
                                             :font font
                                             :button-group 'proj
                                             ))

                     )

    (om-add-subviews win
                     (om-make-layout 'om-row-layout
                                     :subviews (list
                                                (om-make-di 'om-button :size (om-make-point 80 24)
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
                                                            )

                                                (om-make-di 'om-button :size (om-make-point 80 24) :text "Quit"
                                                            :di-action #'(lambda (button)
                                                                           (declare (ignore button))
                                                                           (om-return-from-modal-dialog win 'quit))))
                                     )
                     )

    (om-modal-dialog win)
    ))

; (ws-dialog)




;;;======================================
;;; QUIT
;;;======================================

(defvar *om-exit-actions* nil)

(defun add-om-exit-action (action)
  (pushnew action *om-exit-actions*))

(defun perform-om-exit-actions ()
  (loop for action in *om-exit-actions*
        do (funcall action)))

(defun quit-om-callback ()
  (let ((rep (and ; (om-y-or-n-dialog "Quit OpenMusic ?" :default-button :yes)
              (check-om-docs-before-close)
              (om-lisp::check-buffers-before-close) ;; handled by om-text-edit-window destroy callback
              )))
    (when rep
      (perform-om-exit-actions)
      (oa::om-api-exit))
    rep))
