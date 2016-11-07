
;;;=====================
;;; PM2 PREFS
; (pushr 'pm2 *external-prefs*)
;;; (add-external-pref-module 'pm2)

(in-package :om)

(defmethod get-external-name ((module (eql 'pm2))) "PM2")
(defmethod get-external-module-vals ((module (eql 'pm2)) modulepref) (get-pref modulepref :pm2-options))
(defmethod get-external-module-path ((module (eql 'pm2)) modulepref) (get-pref modulepref :pm2-path))
(defmethod set-external-module-vals ((module (eql 'pm2)) modulepref vals) (set-pref modulepref :options vals))
(defmethod set-external-module-path ((module (eql 'pm2)) modulepref path) (set-pref modulepref :path path))

(defmethod get-external-def-vals ((module (eql 'pm2))) 
  (let ((libpath (mypathname (find-om-library "om-pm2"))))
    (list :path (om-make-pathname :directory (append (pathname-directory libpath) 
                                                     '("resources" "bin")
                                                     #+macosx '("mac" "Pm2.app" "Contents" "MacOS") 
                                                     #+win32 '("win")
                                                     #+linux '("linux")
                                                     )
                                  :host (pathname-host libpath) :device (pathname-device libpath)
                                  :name "pm2" #+win32 :type #+win32 "exe")
          :options nil)))

(defmethod save-external-prefs ((module (eql 'pm2))) 
  `(:pm2-path ,(omng-save om-pm2-lib::*PM2-PATH*) 
    :pm2-options nil))

(defmethod put-external-preferences ((module (eql 'pm2)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :pm2-options)))
    (when list-prefs 
      nil ; pas d'options...
      )
    (when (get-pref moduleprefs :pm2-path)
      (setf om-pm2-lib::*PM2-PATH* (find-true-external (get-pref moduleprefs :pm2-path))))
    ))

;;; (put-external-pref-values 'pm2)

(defun forum-authorize (exe-path)
  (let ((auth-file (om-choose-file-dialog :prompt "Pleas select the .txt file provided by the ForumNet code generator")))
    (when (and auth-file (probe-file auth-file))
      (om-cmd-line (format nil "~s -init_key_file ~s" 
                           (namestring (find-true-external exe-path))
                           (namestring auth-file))
                   t t)
      (print "Authorization... done")
      )))

(defmethod show-external-prefs-dialog ((module (eql 'pm2)) prefvals)
  (let* ((dialog (om-make-window 'om-dialog
                                 :window-title "PM2 Options"
                                 :size (om-make-point 230 160)
                                 :position :centered
                                 :resizable nil :maximize nil :close nil))
         paramstext)
    (om-add-subviews dialog
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 20) (om-make-point 200 20) 
                                          "PM2 Forum Activation" :font *om-default-font1*)
                     
                     (om-make-di 'om-button (om-make-point 160 16) (om-make-point 50 20) "Go!" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (if (probe-file *pm2-path*) (forum-authorize *pm2-path*)
                                                         (om-message-dialog "Please set PM2 path first!"))))
                  
      
                     (om-make-di 'om-button (om-make-point 130 110) (om-make-point 80 20) "Close"
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog nil)))
                 
                     )
    (om-modal-dialog dialog)))

