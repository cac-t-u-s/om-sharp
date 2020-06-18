(in-package "CL-USER")

(load-all-patches)

;(require "hqn-web")

#+mswindows(require "ole")

;;; used by the "Shell" window (= useful?)
;;; see also *modules* for more modules...
(require "shell-buffer")
(require "subproc")

(print "==============================")
(print "LOADING SOURCES")
(print "==============================")

(load (current-pathname "build"))

(print "==============================")
(print "APPLICATION SETUP")
(print "==============================")

(defparameter *full-app-name* om::*app-name*)

(defparameter *om-directory-folders* (butlast (pathname-directory (current-pathname))))

(let ((version-str (concatenate 'string (format nil "~d.~d" *version-major* *version-minor*)
                                (if (and *version-patch* (plusp *version-patch*)) (format nil ".~d" *version-patch*) ""))))
  
  ;(setf *full-app-name* (concatenate 'string om::*app-name* " " version-str))

  (with-open-file (f (make-pathname :directory (butlast (pathname-directory (current-pathname)))
                                    :name "VERSION")
                     :direction :output
                     :if-exists :supersede)
    (write-string version-str f)
    ))


;;;==========================
;;; DEFAULT INTERFACE (MACOS)(defmethod osc-start-receive ((box ReceiveBox))
;;;==========================

#+cocoa
(capi:define-interface om-application (capi::cocoa-default-application-interface) ()
  (:menus
   (application-menu
      *full-app-name*
      ((:component
        (("About..."
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
        (("Hide OM#"
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
        (("Quit"
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
          :callback #'(lambda () (om::open-new-document :patch)) :callback-type :none :accelerator "accelerator-n")
         ("New Maquette" 
          :callback #'(lambda () (om::open-new-document :maquette)) :callback-type :none)
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
          :callback-type :none
          :accelerator "accelerator-shift-l")
         ))
       ))
   )
  (:menu-bar application-menu file-menu windows-menu)
  (:default-initargs
   :title *full-app-name*
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
    (("Workspace/Library Window"
      :callback 'om::show-main-om-window
      :accelerator "accelerator-shift-w"
      :callback-type :none)
     ))
   (:component
    (("Lisp Listener"
      :callback 'om::show-listener-win
      :callback-type :none
      :accelerator "accelerator-shift-l")
     ))
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
                   (loop while (not om::*om-initialized*)) ;; leave time to load libs etc.
                   (om::record-recent-file filename)
                   (capi:execute-with-interface
                    om::*om-main-window*
                    #'om::open-doc-from-file (om::extension-to-doctype type) filename))
               ))
             ((string-equal "lisp" type)
              (om::om-open-new-text-editor filename))
             (t nil))
       ))))

#+cocoa
(defun default-interface ()
  (capi:set-application-interface (make-instance 'om-application)))


;;; DELIVER
(defun init-om-standalone ()
  (push :om-deliver *features*)
  #+cocoa(default-interface)
  (om::om-root-init) 
  (setf dspec::*active-finders* 
        (append dspec::*active-finders*
                (list (merge-pathnames
                       #+macosx (concatenate 'string *full-app-name* ".app/Contents/Resources/dspec-database." (oa::om-compiled-type))
                       #-macosx (concatenate 'string "resources/dspec-database." (oa::om-compiled-type))
                       om-api::*om-root*
                       ))))
  #+cocoa(setf system::*stack-overflow-behaviour* nil)
  (setq om::*om-debug* nil) ;; will disable debug print messages
  (om::start-omsharp)
  )


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
;;; FUNCTION REFERENCE
;;;==========================

(om::gen-om-reference)

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







(defun move-mac-resources ()

  (print "================================")
  (print "MOVING RESOURCES (macOS only)")
  (print "================================")

  (let* ((app-contents-folder (make-pathname 
                               :directory (append 
                                           *om-directory-folders* 
                                           (list (concatenate 'string *full-app-name* ".app") "Contents"))))
         (app-libs-folder (merge-pathnames (make-pathname :directory '(:relative "Frameworks")) app-contents-folder))
         (app-resources-folder (merge-pathnames (make-pathname :directory '(:relative "Resources")) app-contents-folder)))
  
    (print (format nil "COPYING LIBRARIES TO: ~A" app-libs-folder))
    (om::om-copy-directory 
     (merge-pathnames "lib/mac/" (make-pathname :directory (append *om-directory-folders* '("resources"))))
     app-libs-folder)
  
    (print (format nil "COPYING RESOURCES TO: ~A" app-resources-folder))

    (loop for item in (oa::om-directory (make-pathname :directory (append *om-directory-folders* '("resources"))) :files t :directories t) 
          unless (string-equal "lib" (car (last (pathname-directory item)))) 
          unless (string-equal "ttf" (string (pathname-type item)))
          do
          (if (system::directory-pathname-p item)

              (om::om-copy-directory 
               item 
               (make-pathname :device (pathname-device app-resources-folder) 
                              :directory (append (pathname-directory app-resources-folder) (last (pathname-directory item)))))

            (om::om-copy-file item (make-pathname :device (pathname-device app-resources-folder) 
                                                  :directory (pathname-directory app-resources-folder)
                                                  :name (pathname-name item) :type (pathname-type item)))
            ))
    
    (om::om-copy-directory  (make-pathname :device (pathname-device app-resources-folder)
                                           :directory (append *om-directory-folders* '("help-patches")))
                            (make-pathname :device (pathname-device app-resources-folder) 
                                           :directory (append (pathname-directory app-resources-folder) '("help-patches"))))

    (om::om-copy-directory  (make-pathname :device (pathname-device app-resources-folder)
                                           :directory (append *om-directory-folders* '("src")))
                            (make-pathname :device (pathname-device app-resources-folder) 
                                           :directory (append (pathname-directory app-resources-folder) '("src"))))
    
    (clean-sources (make-pathname :device (pathname-device app-resources-folder) 
                                  :directory (append (pathname-directory app-resources-folder) '("src")))
                   NIL)
    
    (om::om-copy-directory  (make-pathname :device (pathname-device app-resources-folder)
                                           :directory (append *om-directory-folders* '("init")))
                            (make-pathname :device (pathname-device app-contents-folder) 
                                           :directory (append (pathname-directory app-contents-folder) '("Init"))))
    ))


(print "==============================")
(print "CREATING APP")
(print "==============================")

; (version-to-hex 6.020005)
; #x0006000200000005

(let ((application-pathname 
       #+cocoa
       (when (save-argument-real-p)
         (compile-file-if-needed (sys:example-file  "configuration/macos-application-bundle") :load t)
         (create-macos-application-bundle (make-pathname :directory (butlast (pathname-directory (current-pathname)))
                                                        :name *full-app-name*)
                                         :document-types (list `("Patch" ("opat") ,(om::om-relative-path '("mac") "opat.icns"))
                                                               `("Maquette" ("omaq") ,(om::om-relative-path '("mac") "omaq.icns"))
                                                               ;`("TextFun" ("olsp") ,(om::om-relative-path '("mac") "lsp-icon.icns"))
                                                               `("om Library" ("omlib") ,(om::om-relative-path '("mac") "omlib.icns")))
                                                           :application-icns (om::om-relative-path '("mac") "om-sharp.icns")
                                         :identifier "fr.cactus.om-sharp"
                                         :version *version-string*
                                         ))
       #+mswindows
       (make-pathname :directory (butlast (pathname-directory (current-pathname)))
                      :name *full-app-name* :type "exe")
       #+linux
       (make-pathname :directory (butlast (pathname-directory (current-pathname)))
                      :name *full-app-name*)))
  
  #+macosx(move-mac-resources)

  (deliver 'init-om-standalone
           application-pathname
           0 
            #+macosx :split  #+macosx :resources
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
           :startup-bitmap-file NIL ;; *startup-bmp*  ;; removed because of a delivery bug with menus        
           #+mswindows :keep-gc-cursor #+mswindows nil
           #+mswindows :versioninfo #+mswindows (list :binary-version (read-from-string (version-to-hex *version*))
                                              :version-string *version-string*
                                              :company-name "" :product-name "om-sharp" :file-description "")
           #+mswindows :console #+mswindows :input
           ; :quit-when-no-windows #+mswindows t #-mswindows nil
           #+(or cocoa win32) :packages-to-keep #+cocoa '(:objc)  #+mswindows '(:comm)
           #+mswindows :icon-file #+mswindows "./win/om-sharp.ico"
           )
  )


;  :editor-commands-to-keep :all-groups
