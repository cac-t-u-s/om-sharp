(in-package "CL-USER")

(load-all-patches)

;(require "hqn-web")

#+win32(require "ole")

(print "==============================")
(print "LOADING SOURCES")
(print "==============================")

(load (current-pathname "build-om"))

(print "==============================")
(print "APPLICATION SETUP")
(print "==============================")

(defparameter *app-name+version* "o7")
;(defparameter *app-name+version* (concatenate 'string "o7-" (version-to-string *om-version* t nil)))

(defparameter *om-directory-folders* (butlast (pathname-directory (current-pathname))))

;;;==========================
;;; DEFAULT INTERFACE (MACOS)(defmethod osc-start-receive ((box ReceiveBox))
;;;==========================

#+cocoa
(capi:define-interface om-application (capi::cocoa-default-application-interface) ()
  (:menus
   (application-menu
      *app-name+version*
      ((:component
        (("About OM"
          :callback 'om::show-about-win
          :callback-type :none)))
       (:component
        (("Preferences..."
          :callback 'om::show-preferences-win
          :accelerator "accelerator-,"
          :callback-type :none)))

       ;(:component
       ; ()
        ;; This is a special named component where the CAPI will
        ;; attach the standard Services menu.
       ; :name :application-services)
       (:component
        (("Hide o7"
          :accelerator "accelerator-h"
          :callback-data :hidden)
         ("Hide Others"
          :accelerator "accelerator-meta-h"
          :callback-data :others-hidden)
         ("Show All"
          :callback-data :all-normal))
        :callback #'(setf capi:top-level-interface-display-state)
        :callback-type :data-interface)
       (:component
        (("Quit OM"
          :accelerator "accelerator-q"
          :callback #'(lambda (interface)
                        (capi:destroy interface))
          :callback-type :interface)))))

   (open-recent-menu 
    "Open Recent..." 
    nil
    :items-function #'(lambda (interface) 
                        (mapcar #'(lambda (file)
                                    (make-instance 'capi::menu-item :title (namestring file)
                                                   :callback #'(lambda () (om::open-om-document file))
                                                   :callback-type :none))
                                om::*om-recent-files*))
    )
  
   (file-menu
      "File"
      ((:component  
        (("New Patch" 
         :callback #'(lambda () (om::open-new-document :patch)) :callback-type :none
          :accelerator "accelerator-n")
        ("New Maquette" 
         :callback #'(lambda () (om::open-new-document :maquette)) :callback-type :none
         :accelerator "accelerator-m")
        ("New Lisp function" 
         :callback #'(lambda () (om::open-new-document :lispfun)) :callback-type :none)
        ))
        
        ("New Text/Lisp Buffer" 
         :callback #'(lambda () (om-lisp::om-open-text-editor :lisp t)) :callback-type :none
         :accelerator "accelerator-N") 
      
        (:component 
         (("Open..." 
           :accelerator "accelerator-o"
           :callback 'om::open-om-document
           :callback-type :none)
          
          open-recent-menu
          ))   
        )
      )

   (windows-menu
      "Windows"
      ((:component
        (("Workspace/Library Window"
          :callback 'om::show-main-om-window
          :accelerator "accelerator-shift-w"
          :callback-type :none)
         ))
       (:component
        (("Lisp Listener"
          :callback 'om::show-listener-win
          :callback-type :none)
         ))
       ))
   )
  (:menu-bar application-menu file-menu windows-menu)
  (:default-initargs
   :title *app-name+version*
   :application-menu 'application-menu

   ;:confirm-destroy-function 'quit-callback
   ;:destroy-callback #'(lambda (interface) (oa::om-exit-funcall))
   ;:top-level-hook 'oa::interface-handle-error
   ;:window-styles '(:internal-borderles :never-iconic :textured-background) ;; :hides-on-deactivate-window) :toolbox
   ;:display-state :normal
   ; POUR ouvrir des fichiers directement
   :message-callback 'om-application-callback
   :dock-menu 'om-dock-menu
   ))


#+cocoa
(capi:define-menu om-dock-menu (self)
  "Dock Menu"
  ((:component
        (("Workspace"
          :callback 'om::show-workspace-win
          :accelerator "accelerator-shift-w"
          :callback-type :none)
         ("Library"
          :callback 'om::show-packages-win
          :callback-type :none
          :enabled-slot nil)
         ))
       (:component
        (("Lisp Listener"
          :callback 'om::show-listener-win
          :callback-type :none)
         ("Lisp Editor"
          :callback 'oa::om-open-new-text-editor
          :callback-type :none)))
       )
  )


#+cocoa
(defun om-application-callback (self message &rest args)
  (declare (ignore self))
  (case message
    (:open-file
     (let* ((filename (pathname (car args)))
            (type (pathname-type filename)))
       (cond ((find type '("opat" "omaq" "olsp") :test 'string-equal)
              (oa::om-run-process 
               "open doc"
               #'(lambda ()
                   (sleep 1) ;; leave time to load libs etc.
                   (om::open-doc-from-file (om::extension-to-doctype type) filename))
               ))
             ((string-equal "lisp" type)
              (om::om-open-new-text-editor filename))
             (t nil))
       ))))

#+cocoa
(defun default-interface ()
  (capi:set-application-interface (make-instance 'om-application)))

; now we declare this in start-openmusic
;#+cocoa
;(defun quit-callback (interface) (om::quit-om-callback))


;;; DELIVER
(defun init-om-standalone ()
  (push :om-deliver *features*)
  #+cocoa(default-interface)
  (om::om-root-init) 
  (setf dspec::*active-finders* (append dspec::*active-finders*
                                        (list (merge-pathnames 
                                               #+macosx(concatenate 'string *app-name+version* ".app/Contents/Resources/dspec-database." (oa::om-compiled-type))
                                               #-macosx(concatenate 'string "resources/dspec-database." (oa::om-compiled-type))
                                               om-api::*om-root*))))
  #+cocoa(setf system::*stack-overflow-behaviour* nil)
  (setq om::*om-debug* nil) ;; will disable debug print messages
  (om::start-openmusic)
  )

(list (merge-pathnames 
       #+macosx(concatenate 'string "OM.app/Contents/Resources/dspec-database." (oa::om-compiled-type))
       om-api::*om-root*))

;;;==========================
;;; SOURCE DEFINITIONS
;;;==========================

; (*active-finders*)

(print "==============================")
(print "SAVE SOURCE TRACKING")
(print "==============================")


(dspec::save-tags-database (make-pathname :directory (append *om-directory-folders* '("resources"))
                                          :name "dspec-database" :type (oa::om-compiled-type)))

(dspec:discard-source-info)


;;;==========================
;;; BUILD IMAGE
;;;==========================



;(setf *debugger-hook* 'oa::om-debugger-hook)

(defun version-to-hex (n)
  (format nil "#x~4,'0X~4,'0X~4,'0X~4,'0X"
          (round n)
          (round (* (cadr (multiple-value-list (round n))) 100))
          (round (* (cadr (multiple-value-list (round (* 100 n)))) 100))
          (round (* (cadr (multiple-value-list (round (* 10000 n)))) 100))
          ))


(print "==============================")
(print "CREATING APP")
(print "==============================")

(print "================================")
(print "MOVING RESOURCES (macOS only)")
(print "================================")


(defun move-mac-resources ()
  (let* ((app-contents-folder (make-pathname 
                               :directory (append 
                                           *om-directory-folders* 
                                           (list (concatenate 'string *app-name+version* ".app") "Contents"))))
         (app-libs-folder (merge-pathnames (make-pathname :directory '(:relative "Frameworks")) app-contents-folder))
         (app-resources-folder (merge-pathnames (make-pathname :directory '(:relative "Resources")) app-contents-folder)))
  
    (print (format nil "COPYING LIBRARIES TO: ~A" app-libs-folder))
    (om::om-copy-directory 
     (merge-pathnames "lib/mac/" (make-pathname :directory (append *om-directory-folders* '("resources"))))
     app-libs-folder)
  
    (print (format nil "COPYING RESOURCES TO: ~A" app-resources-folder))
    (loop for item in (oa::om-directory (make-pathname :directory (append *om-directory-folders* '("resources"))) :files t :directories t) 
          unless (string-equal "lib" (car (last (pathname-directory item)))) do
          (if (system::directory-pathname-p item)
              (om::om-copy-directory item (make-pathname :device (pathname-device app-resources-folder) 
                                                         :directory (append (pathname-directory app-resources-folder) (last (pathname-directory item)))))
            (om::om-copy-file item (make-pathname :device (pathname-device app-resources-folder) 
                                                  :directory (pathname-directory app-resources-folder)
                                                  :name (pathname-name item) :type (pathname-type item)))))
    
    (om::om-copy-directory  (make-pathname :device (pathname-device app-resources-folder)
                                           :directory (append *om-directory-folders* '("src")))
                            (make-pathname :device (pathname-device app-resources-folder) 
                                           :directory (append (pathname-directory app-resources-folder) '("src"))))
    
    (clean-sources (make-pathname :device (pathname-device app-resources-folder) 
                                  :directory (append (pathname-directory app-resources-folder) '("src"))))
    
    (om::om-copy-directory  (make-pathname :device (pathname-device app-resources-folder)
                                           :directory (append *om-directory-folders* '("init")))
                            (make-pathname :device (pathname-device app-contents-folder) 
                                           :directory (append (pathname-directory app-contents-folder) '("Init"))))
    ))

; (version-to-hex 6.020005)
; #x0006000200000005

(let ((application-pathname 
       #+cocoa
       (when (save-argument-real-p)
         (compile-file-if-needed (sys:example-file  "configuration/macos-application-bundle") :load t)
         (create-macos-application-bundle (make-pathname :directory (butlast (pathname-directory (current-pathname)))
                                                        :name *app-name+version*)
                                         :document-types (list `("Patch" ("opat") ,(om::om-relative-path '("mac") "pat-icon.icns"))
                                                               `("Maquette" ("omaq") ,(om::om-relative-path '("mac") "maq-icon.icns"))
                                                               `("TextFun" ("olsp") ,(om::om-relative-path '("mac") "lsp-icon.icns"))
                                                               `("om Library" ("omlib") ,(om::om-relative-path '("mac") "omlib.icns")))
                                                           :application-icns (om::om-relative-path '("mac") "om.icns")
                                         :identifier "fr.ircam.repmus.o7"
                                         :version (version-to-string *om-version* t nil)
                                         ))
       #+win32
       (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2)
                      :name *app-name+version* :type "exe")))
  
  #+macosx(move-mac-resources)

  (deliver 'init-om-standalone
           application-pathname
           0 
           :split :resources
           :interface :capi
           :keep-editor t
           :keep-debug-mode t
           :keep-load-function t
           :keep-pretty-printer t
           
           ;#+win32 :editor-style #+win32 :pc
           ;:keep-complex-numbers nil
           ;:keep-conditions :all
           ;:keep-xref-info t   ;; ??
           ;:editor-style :default
           :startup-bitmap-file NIL ;; *startup-bmp*  ;; removed because of a delivery bug with menus in OM 7         
           #+win32 :keep-gc-cursor #+ win32 nil
           #+win32 :versioninfo #+win32 (list :binary-version (read-from-string (version-to-hex *om-version*))
                                              :version-string (version-to-string *om-version* t nil)
                                              :company-name "" :product-name "o7" :file-description "")
           #+win32 :console #+win32 :input
           :quit-when-no-windows #+win32 t #-win32 nil
           #+(or cocoa win32) :packages-to-keep #+cocoa '(:objc)  #+win32 '(:comm)
         ; #+win32 :icon-file #+win32 "./win/OpenMusic.ico"
           )
  )


;  :editor-commands-to-keep :all-groups
;========================

;;; MAC :
; /Applications/LispWorks\ 5.1/LispWorks.app/Contents/MacOS/lispworks-5-1-0-macos-universal -build deliver.lisp
; (save-universal-from-script "../../image/macos-i/OM 6.0" "deliver.lisp")


;;; WIN :
; lispworks-5-1-0-x86-win32.exe -build deliver.lisp
