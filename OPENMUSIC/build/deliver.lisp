(in-package "CL-USER")

(load-all-patches)

;(require "hqn-web")

#+win32(require "ole")

(load (current-pathname "build-om"))


(defvar *app-name+version* "OM")
(setf *app-name+version* (concatenate 'string "OM " (version-to-string *om-version* nil *beta-release*)))



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
          :callback-type :none)))

       ;(:component
       ; ()
        ;; This is a special named component where the CAPI will
        ;; attach the standard Services menu.
       ; :name :application-services)
       (:component
        (("Hide OM"
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
   (windows-menu
      "Windows"
      ((:component
        (("Workspace"
          :callback 'om::show-workspace-win
          :accelerator "accelerator-shift-w"
          :callback-type :none)
         ("Library"
          :callback 'om::show-packages-win
          :accelerator "accelerator-shift-p"
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
       ))
   )
  (:menu-bar application-menu windows-menu)
  (:default-initargs
   :title *app-name+version*
   :application-menu 'application-menu

   :confirm-destroy-function 'quit-callback
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
     (let ((filename (pathname (car args))))
       (cond ((string-equal "omp" (pathname-type filename))
              (om::open-patch-from-file filename))
             ((string-equal "lisp" (pathname-type filename))
              (om::om-open-new-text-editor filename))
             (t nil))
       ))))

#+cocoa
(defun default-interface ()
  (capi:set-application-interface (make-instance 'om-application)))

#+cocoa
(defun quit-callback (interface)
  (om::quit-om-callback))


;;; DELIVER
(defun init-om-standalone ()
  (push :om-deliver *features*)
  #+cocoa(default-interface)
  (om::om-root-init) 
  (setf dspec::*active-finders* (append dspec::*active-finders* 
                                        (list (om::om-make-pathname 
                                               :directory (om::om-relative-path '("resources") nil :om)
                                               :name "dspec-database" :type oa::*om-compiled-type*))))  
  #+cocoa(setf system::*stack-overflow-behaviour* nil)
  (om::start-openmusic)
  )

;;;==========================
;;; SOURCE DEFINITIONS
;;;==========================

; (*active-finders*)

(dspec::save-tags-database (make-pathname :directory (append (butlast (pathname-directory (current-pathname))) (list "resources"))
                                          :name "dspec-database" :type oa::*om-compiled-type*))

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

; (version-to-hex 6.020005)
; #x0006000200000005

(let ((application-pathname 
       #+cocoa
       (when (save-argument-real-p)
         (compile-file-if-needed (sys:example-file  "configuration/macos-application-bundle") :load t)
         (write-macos-application-bundle (make-pathname :directory (butlast (pathname-directory (current-pathname)))
                                                        :name *app-name+version*)
                                         :document-types (list `("Patch" ("opat") ,(om::om-relative-path '("mac") "pat-icon.icns"))
                                                               `("Maquette" ("omaq") ,(om::om-relative-path '("mac") "maq-icon.icns")))
                                                           :application-icns (om::om-relative-path '("mac") "om.icns")
                                         :identifier "ircam.om7"
                                         :version (version-to-string *om-version* t nil)
                                         ))
       #+win32
       (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2)
                      :name *app-name+version* :type "exe")))

  (deliver 'init-om-standalone
           application-pathname
           0
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
                                              :company-name "IRCAM" :product-name "OpenMusic" :file-description "")
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
