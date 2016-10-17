(in-package :om)


;;;================================================================================================================
;;; PM2 PATH
;;;================================================================================================================
;;; rajouter dans les prefs?

;;; si c'est dans une userlib il faut initialiser ici
(defvar *PM2-PATH* nil)

;;; ... ou a la main
(defmethod! set-pm2-path ((path string))
  :icon 186
  (setf *PM2-PATH* (pathname path)))

(defmethod! set-pm2-path ((path pathname))
  :icon 186
  (setf *PM2-PATH* path))

(defmethod! set-pm2-path ((path null))
  :icon 186
  (setf *PM2-PATH* (om-choose-file-dialog :prompt "Please locate the pm2 kernel file")))

(defmethod! pm2-path (&optional (unix nil))
  :icon 186
  (let ((pa *PM2-PATH*))
    (if unix (om-path2cmdpath pa) 
        pa)))






;;;=====================
;;; PM2 PREFS

;(pushr 'pm2 *external-prefs*)
(add-external-pref-module 'pm2)

(defmethod get-external-name ((module (eql 'pm2))) "PM2")
(defmethod get-external-icon ((module (eql 'pm2))) (and (exist-lib-p "OM-pm2") (list 953 (exist-lib-p "OM-pm2"))))

(defmethod get-external-module-vals ((module (eql 'pm2)) modulepref) (get-pref modulepref :pm2-options))
(defmethod get-external-module-path ((module (eql 'pm2)) modulepref) (get-pref modulepref :pm2-path))
(defmethod set-external-module-vals ((module (eql 'pm2)) modulepref vals) (set-pref modulepref :pm2-options vals))
(defmethod set-external-module-path ((module (eql 'pm2)) modulepref path) 
  (set-pref modulepref :pm2-path path))


;;; params folder
(defun def-pm2-options () t)

(defmethod get-external-def-vals ((module (eql 'pm2))) 
  (let ((libpath (lib-pathname (find-library "OM-pm2"))))
    (list :pm2-path (om-make-pathname :directory (append (pathname-directory libpath) 
                                                         '("resources" "bin")
                                                         #+macosx '("mac" "Pm2.app" "Contents" "MacOS") 
                                                         #+win32 '("win")
							 #+linux '("linux")
                                                         )
                                        :host (pathname-host libpath) :device (pathname-device libpath)
                                        :name "pm2" #+win32 :type #+win32 "exe")
          :pm2-options (def-pm2-options))))

(defmethod save-external-prefs ((module (eql 'pm2))) 
  `(:pm2-path ,(om-save-pathname *PM2-PATH*) 
    :pm2-options nil))

(defmethod put-external-preferences ((module (eql 'pm2)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :pm2-options)))
    (when list-prefs 
      nil ; pas d'options...
      )
    (when (get-pref moduleprefs :pm2-path)
      (setf *PM2-PATH* (find-true-external (get-pref moduleprefs :pm2-path))))
    ))

(put-external-pref-values 'pm2)

;(defmethod show-external-prefs-dialog ((module (eql 'pm2)) prefvals)
;  (om-beep-msg "NO OPTIONS FOR PM2")
;  nil)

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
                     
                     (om-make-dialog-item 'om-button (om-make-point 160 16) (om-make-point 50 20) "Go!" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (if (probe-file *pm2-path*) (forum-authorize *pm2-path*)
                                                         (om-message-dialog "Please set PM2 path first!"))))
                  
      
                     (om-make-dialog-item 'om-button (om-make-point 130 110) (om-make-point 80 20) "Close"
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog nil)))
                 
                     )
    (om-modal-dialog dialog)))

