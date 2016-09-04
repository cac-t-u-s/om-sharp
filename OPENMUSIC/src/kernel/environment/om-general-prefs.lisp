;;;=====================
;;; OM GENERAL PREFS
;;;=====================

(in-package :om)


(defvar *composer-name* "Guarigocha")

(defparameter *catch-errors* nil)
(defvar *eval-process* :on)

(defmethod default-prefs-for-module ((module-name (eql :general)))
  (list '(:handle-errors nil)
        '(:user-name "XXX")
        '(:eval-process :on)
        '(:listener-on-top :no)
        '(:multi-threading :yes)
        (list :out-files-dir (om-relative-path '("out-files") nil (if *current-workspace* :workspace (om-user-home))))
        (list :tmp-files-dir (om-relative-path '("out-files") nil (if *current-workspace* :workspace (om-user-home))))
        (list :in-files-dir (om-relative-path '("in-files") nil (if *current-workspace* :workspace (om-user-home))))))

(push-pref-module :general)

(defmethod apply-preferences ((module-name (eql :general)))

  (setf *catch-errors* (get-pref-value module-name :handle-errors))
  
  (when (get-pref-value module-name :eval-process) 
    (setf *eval-process* (get-pref-value module-name :eval-process))
    (om-use-eval-process (equal *eval-process* :on)))
     
  (when (get-pref-value module-name :listener-on-top) 
    (setf om-lisp::*listener-on-top* (equal (get-pref-value module-name :listener-on-top) :yes)))

  (when (get-pref-value module-name :multi-threading)
    (if (not (equal (equal (get-pref-value module-name :multi-threading) :yes) (multi-thread *general-player*)))
        (setf (multi-thread *general-player*) (equal (get-pref-value module-name :multi-threading) :yes))))
     
  (setf *composer-name* (get-pref-value module-name :user-name))

  ;(unless (probe-file (get-pref-value module-name :out-files-dir))
  ;  (om-print "Warning: preference path for 'out-files' not found. Default location will be restored")
  ;  (restore-default-pref-value module-name :out-files-dir))
  ;(unless (probe-file (get-pref-value module-name :tmp-files-dir))
  ;  (om-print "Warning: preference path for 'tmp-files' not found. Default location will be restored")
  ;  (restore-default-pref-value module-name :tmp-files-dir))
  ;(unless (probe-file (get-pref-value module-name :in-files-dir))
  ;  (om-print "Warning: preference path for 'in-files' not found. Default location will be restored")
  ;  (restore-default-pref-value module-name :in-files-dir))

  (setf *om-outfiles-folder* (get-pref-value module-name :out-files-dir))
  (setf *om-tmpfiles-folder* (get-pref-value module-name :tmp-files-dir))
  (setf *om-infiles-folder* (get-pref-value module-name :in-files-dir))
  )
    

(defmethod save-pref-module ((module-name (eql :general)))
  `(:general 
    (:handle-errors ,*catch-errors*)
    (:user-name ,*composer-name*)
    (:eval-process ,*eval-process*)
    (:listener-on-top ,om-lisp::*listener-on-top*)
    (:out-files-dir ,(omng-save *om-outfiles-folder*))
    (:tmp-files-dir ,(omng-save *om-tmpfiles-folder*))
    (:in-files-dir ,(omng-save *om-infiles-folder*))
    ))



(defmethod make-pref-panel ((id (eql :general)) modulepref)
  (let ((pane (om-make-view 'preference-pane
                            :pref-id id
                            :name "General"
                            :font (om-def-font :font1) 
                            :bg-color (om-def-color :light-gray)
                            ))
        (l1 20) 
        (posy 0)
        outtxt tmptxt intxt)
    (om-add-subviews 
     pane
     (om-make-layout 
      'om-row-layout :ratios '(1 1)
      :subviews (list 
                 (om-make-view
                  'om-view 
                  :subviews (list
                             (om-make-di 'om-simple-text :position (om-make-point l1 (incf posy 50)) 
                                         :size (om-make-point 90 24) :text "User Name"
                                         :font (om-def-font :font2))

                             (om-make-di 'om-editable-text :size (om-make-point 200 20)
                                         :position (om-make-point (+ l1 100) posy)
                                         :text (get-pref-in-module modulepref :user-name)
                                         :di-action (om-dialog-item-act item 
                                                         (set-pref-in-module modulepref :user-name (om-dialog-item-text item)))
                                         :font (om-def-font :font2)
                                         )
                 
                             (om-make-di 'om-check-box :position (om-make-point l1 (incf posy 60)) 
                                         :size (om-make-point 180 15) :text " Handle Error Messages" 
                                         :di-action (om-dialog-item-act item 
                                                      (set-pref-in-module modulepref :handle-errors (om-checked-p item)))
                                         :font (om-def-font :font2)
                                         :checked-p (get-pref-in-module modulepref :handle-errors))
      
                             (om-make-di 'om-simple-text :position (om-make-point l1 (incf posy 20)) 
                                         :size (om-make-point 330 40) 
                                         :text "(Catch Lisp erros and display a simple message window)"
                                         :font (om-def-font :font1))
                     
                             (om-make-di 'om-check-box :position (om-make-point l1 (incf posy 40)) 
                                         :size (om-make-point 200 15) :text " Enable Evaluation Process" 
                                         :di-action (om-dialog-item-act item 
                                                      (set-pref-in-module modulepref :eval-process (if (om-checked-p item) :on :off)))
                                         :font (om-def-font :font2)
                                         :checked-p (equal :on (get-pref-in-module modulepref :eval-process)))
                             
                             (om-make-di 'om-simple-text :position (om-make-point l1 (incf posy 20)) 
                                         :size (om-make-point 330 40) :text "(Evaluate visual programs on a spearate process)"
                                         :font (om-def-font :font1))

                             (om-make-di 'om-check-box :position (om-make-point l1 (incf posy 40)) 
                                         :size (om-make-point 200 15) :text " Allow Multi-Threading" 
                                         :di-action (om-dialog-item-act item
                                                      (set-pref-in-module modulepref 
                                                                          :multi-threading 
                                                                          (if (om-checked-p item) :yes :no)))
                                         :font (om-def-font :font2)
                                         :checked-p (equal (get-pref-in-module modulepref :multi-threading) :yes))

                             (om-make-di 'om-check-box :position (om-make-point l1 (incf posy 40)) 
                                         :size (om-make-point 200 15) :text " Keep Listener in Front" 
                                         :di-action (om-dialog-item-act item 
                                                      (set-pref-in-module modulepref :listener-on-top (if (om-checked-p item) :yes :no)))
                                         :font (om-def-font :font2)
                                         :checked-p (equal :yes (get-pref-in-module modulepref :listener-on-top)))

                             (om-make-di 'om-simple-text :position (om-make-point l1 (incf posy 20))
                                         :size (om-make-point 330 40) 
                                         :text "(Does not apply to the current Listener window)"
                                         :font (om-def-font :font1))))

                 (om-make-view 
                  'om-view 
                  :subviews (list
                             (om-make-di 'om-simple-text :position (om-make-point l1 (setf posy 50)) 
                                         :size (om-make-point 200 30) 
                                         :text "Default Folders"
                                         :font (om-def-font :font2b))

                             (om-make-di 'om-simple-text :position (om-make-point l1 (incf posy 30)) 
                                         :size (om-make-point 80 22) 
                                         :text "Output Files:"
                                         :font (om-def-font :font2))
                             (setf outtxt (om-make-di 'om-multi-text 
                                                      :position (om-make-point l1 (incf posy 25)) 
                                                      :size (om-make-point 320 45)
                                                      :text (namestring (get-pref-in-module modulepref :out-files-dir))
                                                      :font (om-def-font :font1)))
                     
                     ;(om-make-view 'om-icon-button 
                     ;                                 :icon "folder" :icon-pushed "folder-pushed"
                     ;                                 :position (om-make-point (+ l2 310) (- posy 5)) :size (om-make-point 26 25) 
                     ;                                 :action (om-dialog-item-act item
                     ;                                                    (declare (ignore item))
                     ;                                                    (let ((newfolder (om-choose-directory-dialog :directory
                             ;;                                                                                                 (get-pref modulepref :out-files-dir))))
                     ;                                                      (when newfolder
                     ;                                                        (om-set-dialog-item-text outtxt (namestring newfolder))
                    ;                                                         (set-pref-in-module modulepref :out-files-dir newfolder)))))
                     
                      
                             (om-make-di 'om-simple-text :position (om-make-point l1 (incf posy 40)) 
                                         :size (om-make-point 80 22) :text "Input Files:" :font (om-def-font :font2))
                             (setf intxt (om-make-di 'om-multi-text :position (om-make-point l1 (incf posy 25)) 
                                                     :size (om-make-point 320 45)
                                                     :text (namestring (get-pref-in-module modulepref :in-files-dir))
                                                     :font (om-def-font :font1)))
                     
                     ;(om-make-view 'om-icon-button 
                     ;              :icon "folder" :icon-pushed "folder-pushed"
                     ;              :position (om-make-point (+ l2 310) (- posy 5)) :size (om-make-point 26 25) 
                     ;              :action (om-dialog-item-act item
                     ;                        (declare (ignore item))
                     ;                        (let ((newfolder (om-choose-directory-dialog :directory (get-pref modulepref :in-files-dir))))
                     ;                          (when newfolder
                     ;                            (om-set-dialog-item-text intxt (namestring newfolder))
                    ;                             (set-pref-in-module modulepref :in-files-dir newfolder)))))
                      

                             (om-make-di 'om-simple-text :position (om-make-point l1 (incf posy 40)) :size (om-make-point 80 22) 
                                         :text "Temp Files:" :font (om-def-font :font2))
                             (setf tmptxt (om-make-di 'om-multi-text :position (om-make-point l1 (incf posy 25)) 
                                                      :size (om-make-point 320 45)
                                                      :text (namestring (get-pref-in-module modulepref :tmp-files-dir))
                                                      :font (om-def-font :font1)))
                     
                     ;(om-make-view 'om-icon-button 
                     ;              :icon "folder" :icon-pushed "folder-pushed"
                     ;              :position (om-make-point (+ l2 310) (- posy 5)) :size (om-make-point 26 25) 
                     ;              :action (om-dialog-item-act item
                     ;                                  (declare (ignore item))
                     ;                                  (let ((newfolder (om-choose-directory-dialog :directory (get-pref modulepref :tmp-files-dir))))
                     ;                                    (when newfolder
                     ;                                      (om-set-dialog-item-text tmptxt (namestring newfolder))
                     ;                                     (set-pref-in-module modulepref :tmp-files-dir newfolder)))))
                             ))
                 )
      ))
    
    pane))

