;;;==============================
;;; OM# application bundle for MacOS 
;;; Loaded from the delivery file
;;;==============================

(in-package "CL-USER")


(capi:define-interface omsharp-application (capi::cocoa-default-application-interface) ()
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
       ("New Sequencer"
        :callback #'(lambda () (om::open-new-document :sequencer)) :callback-type :none)
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
      (("Session Window"
        :callback 'om::show-main-window
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
   :message-callback 'omsharp-application-callback
   :dock-menu 'omsharp-dock-menu
   ))


(capi:define-menu omsharp-dock-menu (self)
  "Dock Menu"
  ((:component
    (("Session Window"
      :callback 'om::show-main-window
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


(defun omsharp-application-callback (self message &rest args)
  (declare (ignore self))
  (case message
    (:open-file
     (let* ((filename (pathname (car args)))
            (type (pathname-type filename)))
       (cond ((find type '("opat" "oseq" "olsp") :test 'string-equal)
              (oa::om-run-process
               "open doc"
               #'(lambda ()
                   (loop while (not om::*om-initialized*)) ;; leave time to load libs etc.
                   (om::record-recent-file filename)
                   (if om::*main-window*
                       (capi:execute-with-interface
                        om::*main-window*
                        #'om::open-doc-from-file (om::extension-to-doctype type) filename)
                     (om::open-doc-from-file (om::extension-to-doctype type) filename))
                   )
               ))
             ((string-equal "lisp" type)
              (om::om-open-new-text-editor filename))
             (t nil))
       ))))


(defun omsharp-interface ()
  (capi:set-application-interface (make-instance 'omsharp-application)))
