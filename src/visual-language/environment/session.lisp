;============================================================================
; om#: visual programming language for computer-assisted music composition
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

(defmethod save-om-preferences ()

  (let ((path (om-preference-file)))

    (om-create-directory (make-pathname :directory (pathname-directory path)))
    (with-open-file (out path :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)

      (let ((*print-pretty* t))

        (pprint `(:info (:saved ,(om-get-date)) (:version ,*version*)) out)

        (pprint `(:recent-files ,(omng-save (mapcar 'namestring *om-recent-files*))) out)

        (pprint `(:session-window
                  (:position ,(omng-save *main-window-position*))
                  (:size ,(omng-save *main-window-size*)))
                out)

        (pprint `(:listener-window
                  (:position ,(omng-save *listener-window-position*))
                  (:size ,(omng-save *listener-window-size*)))
                out)

        (pprint `(:user-preferences
                  ,.(mapcar #'save-pref-module *user-preferences*))
                out)
        ))

    path))


;(save-om-preferences)
;(list-from-file (om-preference-file))
;(cdr (find :user-preferences pr-list :test 'equal :key 'car)

;;; reads one preference in the preference file
(defmethod read-om-preference (key)
  (let* ((path (om-preference-file))
         (pref-list (and (file-exists-p path)
                         (list-from-file path))))
    (find-values-in-prop-list pref-list key)))

;;; read and load the main prefs for a session
(defmethod load-om-preferences ()
  (let* ((path (om-preference-file))
         (pr-list (and (file-exists-p path)
                       (list-from-file path))))

    (setq *om-recent-files* (omng-load (find-value-in-kv-list pr-list :recent-files)))

    (let ((session-window-geometry (find-values-in-prop-list pr-list :session-window)))
      (when session-window-geometry
        (let ((pos (omng-load (find-value-in-kv-list session-window-geometry :position)))
              (size (omng-load (find-value-in-kv-list session-window-geometry :size))))
          (when pos (setq *main-window-position* pos))
          (when size (setq *main-window-size* size))
          )))

    (let ((listener-window-geometry (find-values-in-prop-list pr-list :listener-window)))
      (when listener-window-geometry
        (let ((pos (omng-load (find-value-in-kv-list listener-window-geometry :position)))
              (size (omng-load (find-value-in-kv-list listener-window-geometry :size))))
          (when pos (setq *listener-window-position* pos))
          (when size (setq *listener-window-size* size))
          )))

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

  (set-language *release-language*)
  (register-om-icons)

  ;; #+(or om-deliver mswindows)
  (lispworks::define-action "Confirm when quitting image" "Prompt for confirmation" 'om::quit-om-callback)

  (om-lisp::om-init-output-stream)

  (load-om-preferences)

  (when (find-om-package :midi) (midi-apply-ports-settings))

  (om-init-funcall)

  (register-all-libraries)

  (save-om-preferences)

  (show-main-window :front-tab :listener)
  (capi:execute-with-interface *main-window* 'eval '(in-package :om))
  (om-print-format *om-startup-string*)

  (load-user-code)

  (setf *om-initialized* t)
  )


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
  (let ((rep (and
              (check-om-docs-before-close)
              (om-lisp::check-buffers-before-close) ;; handled by om-text-edit-window destroy callback
              )))
    (when rep
      (perform-om-exit-actions)
      (oa::om-api-exit))
    rep))
