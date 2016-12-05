
;;;=====================
;;; PM2 PREFS
; (pushr 'pm2 *external-prefs*)
;;; (add-external-pref-module 'pm2)

(in-package :om)

(defmethod default-pm2-path () 
  (let ((libpath (mypathname (find-om-library "om-pm2"))))
    (om-make-pathname :directory (append (pathname-directory libpath) 
                                         '("resources" "bin")
                                         #+macosx '("mac" "Pm2.app" "Contents" "MacOS") 
                                         #+win32 '("win")
                                         #+linux '("linux")
                                         )
                      :host (pathname-host libpath) :device (pathname-device libpath)
                      :name "pm2" #+win32 :type #+win32 "exe")))

(add-preference-section :libraries "om-pm2")
(add-preference :libraries :pm2-path "pm2 exec" :file 'default-pm2-path) 

;;; works for pm2...
(defun forum-authorize (exe-path)
  (when exe-path
  (let ((auth-file (om-choose-file-dialog :prompt "Pleas select the .txt file provided by the ForumNet code generator")))
    (when (and auth-file (probe-file auth-file))
      (om-cmd-line (format nil "~s -init_key_file ~s" 
                           (namestring (real-exec-pathname exe-path))
                           (namestring auth-file)))
      (print "Authorization... done")))))

(defmethod! authorize-pm2 ()
   (forum-authorize (get-pref-value :libraries :pm2-path)))
