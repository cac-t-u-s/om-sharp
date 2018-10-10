;=========================================================================
; LW Lisp Tools 
; Lisp programming tools for LispWorks delivered applications
;=========================================================================
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed; in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Author: J. Bresson
; Contributions from N. Ellis
;;========================================================================

;=========================================================================
; Lisp Linener for delivered LispWorks appliations
;=========================================================================


(in-package :om-lisp)

;;;===================
;;; export :
;;;===================
(export '(om-init-output-stream
          om-print 
          om-print-format) 
        :om-lisp)

;;;=============================
;;; LISTENER
;;;=============================

(defvar *om-listener* nil)
(defvar *om-stream* nil)
(defparameter *om-prompt* "")

(defparameter *listener-font* 
  #+linux(gp::make-font-description :family "Liberation Mono" :size 10)
  #-linux(gp::make-font-description :family "Consolas" :size 11))


;; (defclass om-listener-pane (capi:listener-pane capi:collector-pane) ())


(defun om-init-output-stream ()
  (setq *om-stream* (capi:collector-pane-stream  (make-instance 'capi:collector-pane)))
  (setq hcl::*background-output* *om-stream* )
  (redef-print))


(defun redef-print ()
  (let ((lispworks::*HANDLE-WARN-ON-REDEFINITION* nil))
    (defun print (something &optional stream)
      (let ((output-stream (or stream *om-stream*)))
        (progn 
          (write something :stream output-stream :escape t)
          (terpri output-stream)
          ;(write-char #\space output-stream)
          ;(format *om-stream* "~D ~D~%" *om-prompt* something)
          )
      ;(when om-lisp::*om-listener* (listener-end-of-buffer om-lisp::*om-listener*))
      something
      ))
    ))


;;;=============================
;;; PRINT REDEFS
;;;=============================

(defun om-print (obj &optional prompt)  
  (format *om-stream* "~A :: ~A~%" (or prompt "") obj)
  obj)

(defun om-print-format (format-string &optional args prompt)  
  (format 
   *om-stream* 
   (concatenate 'string 
                (if prompt (concatenate 'string prompt " :: ") "")
                (apply 'format (append (list nil (concatenate 'string format-string "~%")) args)))
   ))

;;;=============================
;;; PRINT WINDOW
;;;=============================

(defclass om-listener (capi::interface) 
  ((ip :accessor ip :initarg :ip)
   (op :accessor op :initarg :op)))

(defclass om-listener-pane () ())

(defclass om-listener-in-pane (om-listener-pane capi::listener-pane) ())
(defclass om-listener-out-pane (om-listener-pane capi::collector-pane) ())

(defun om-make-listener-output-pane ()
  (make-instance 'om-listener-out-pane 
                 :stream *om-stream* 
                 :echo-area t 
                 :font *listener-font*))

(defun om-clear-listener-output-pane (pane)
  (editor::clear-buffer (capi::editor-pane-buffer pane)))


;(om-make-listener :input t)
;(setf om-lisp::*om-listener* nil)
(defun om-make-listener (&key title x y width height initial-lambda initial-prompt (input nil) (on-top nil))
  (or (and om-lisp::*om-listener* (capi::find-interface 'om-listener))
      (progn
        ;(init-output-streams)
        
        (setf om-lisp::*om-listener* 
              (let* ((in (when input (make-instance 'om-listener-in-pane
                                        ;:maximum-visible-height 0
                                        :echo-area t
                                        :font *listener-font*
                                        :stream *om-stream* 
                                        :create-callback (if initial-lambda
                                                             (lambda (window) 
                                                               (declare (ignore window)) 
                                                               (capi:execute-with-interface *om-listener* initial-lambda))
                                                           (lambda (window)
                                                             (declare (ignore window))
                                                             (capi:execute-with-interface *om-listener* (lambda () (in-package :cl-user))))))))
                     
                     (out (om-make-listener-output-pane))
                     
                     (commands (make-instance 'capi:row-layout 
                                              :description (list (make-instance 'capi::button :text "x"
                                                                                :callback-type :none
                                                                                :font *listener-font*
                                                                                :callback #'(lambda () 
                                                                                              (om-clear-listener-output-pane out))))
                                              :ratios '(nil)))
                     )
                
                (make-instance 
                           'om-listener
                           :layout (make-instance 'capi:column-layout 
                                                  :description (if in (list in :divider out commands) (list out commands))
                                                  :ratios (if in '(1 nil 5 nil) '(1 nil))
                                                  :adjust :right)
                           :window-styles (append (if on-top (list :always-on-top))
                                                  (list :no-character-palette))  ;:toolbox :shadowless :textured-background
                           :ip in :op out
                           :title (or title (concatenate 'string "Listener - " (if in "[system int/out]" "[system out]")))
                           :best-x (or x 100)
                           :best-y (or y (round (- (capi::screen-height (capi:convert-to-screen)) 250)))
                           :best-width (or width 420) :best-height (or height 200)
                           :destroy-callback (lambda (window) (setf om-lisp::*om-listener* nil))
                           :auto-menus nil
                           ;:activate-callback (lambda (window activatep) 
                           ;                     (when activatep (setf (capi::interface-menu-bar-items window)
                           ;                                           (internal-window-class-menubar window))))
                           )
                ))
        
        (editor::clear-buffer (capi::editor-pane-buffer (op om-lisp::*om-listener*)))
        
        (when initial-prompt
                  (princ initial-prompt *om-stream*) 
                  (terpri *om-stream*))
        
        (setf (capi::simple-pane-font (capi::editor-pane-echo-area (op om-lisp::*om-listener*))) *listener-font*)
        
        (when (ip om-lisp::*om-listener*)
          (setf (capi::simple-pane-font (capi::editor-pane-echo-area (ip om-lisp::*om-listener*))) *listener-font*)
          (setf (capi::editor-pane-text  (capi::editor-pane-echo-area (ip om-lisp::*om-listener*))) ""))
        
        (capi::execute-with-interface 
         om-lisp::*om-listener*
         #'(lambda (lw) 
             (let ((def-menu (listener-default-window-menus lw)))
               (setf (capi::interface-menu-bar-items lw)
                     (if (om-listener-window-menus lw)
                         (append (om-listener-window-menus lw)
                                 (list (find "Lisp" def-menu :key 'capi::menu-title :test 'string-equal)))
                       def-menu))))
         om-lisp::*om-listener*)
       
        (capi::display om-lisp::*om-listener*)
        )))


;; to be redefined
(defmethod om-listener-window-menus ((self om-listener)) nil) 

;; used if om-listener-window-menus is not redefined
;; the 'Lisp' patrt will be kept anyway
(defmethod listener-default-window-menus ((self om-listener)) 
  (append (list (make-instance 'capi::menu :title "File"
                               :items 
                               (append (list (make-instance 'capi::menu-item :title "Close Listener"
                                                            :callback-type :interface
                                                            :callback 'listener-close
                                                            :accelerator #\w))
                                     
                                       (if (handler-case (find-class 'om-lisp::om-text-editor-window) (error () nil))
                                           (list (make-instance 'capi::menu-component 
                                                                :items (list 
                                                                        (make-instance 'capi::menu-item :title "New..."
                                                                                       :callback-type :interface
                                                                                       :callback 'listener-new-text-editor
                                                                                       :accelerator #\n
                                                                                       :enabled-function 'file-operations-enabled)
                                                                        (make-instance 'capi::menu-item :title "Open..."
                                                                                       :callback-type :interface
                                                                                       :callback 'listener-open-text-file
                                                                                       :accelerator #\o
                                                                                       :enabled-function 'file-operations-enabled)))))
                                       
                                       ))
                (make-instance 'capi::menu :title "Edit"
                               :items (list (make-instance 'capi::menu-component 
                                                           :items (list 
                                                                   (make-instance 'capi::menu-item :title "Cut"
                                                                                  :callback-type :interface
                                                                                  :callback 'listener-cut
                                                                                  :accelerator #\x)
                                                                   (make-instance 'capi::menu-item :title "Copy"
                                                                                  :callback-type :interface
                                                                                  :callback 'listener-copy
                                                                                  :accelerator #\c)
                                                                   (make-instance 'capi::menu-item :title "Paste"
                                                                                  :callback-type :interface
                                                                                  :callback 'listener-paste
                                                                                  :accelerator #\v)))
                                            (make-instance 'capi::menu-component 
                                                           :items (list 
                                                                   (make-instance 'capi::menu-item :title "Select All" 
                                                                                  :callback 'listener-select-all 
                                                                                  :accelerator #\a
                                                                                  :callback-type :interface)
                                                                               
                                                                   ))
                                            
                                            ;(make-instance 'capi::menu-item :title "Text Font" 
                                            ;               :callback 'choose-listener-font 
                                            ;               :accelerator nil
                                            ;               :callback-type :interface)
                                            ))
                (make-instance 'capi::menu :title "Lisp"
                               :items (list 
                                       (make-instance 'capi::menu-component 
                                                      :items (list 
                                                              (make-instance 'capi::menu-item :title "Find Definition"
                                                                             :callback-type :interface
                                                                             :callback 'listener-find-definition
                                                                             :enabled-function 'lisp-operations-enabled
                                                                             :accelerator #\.)
                                                              ;(make-instance 'capi::menu-item :title "Abort"
                                                              ;               :callback-type :interface
                                                              ;               :callback 'listener-abort
                                                              ;               ;:enabled-function 'lisp-operations-enabled
                                                              ;               :accelerator #\A)
                                                              (make-instance 'capi::menu-item :title "Last Error Backtrace"
                                                                             :callback-type :interface
                                                                             :callback 'listener-error-backtrace
                                                                             :enabled-function 'backtrace-enabled
                                                                             :accelerator #\B)))
                                              
                                       (make-instance 'capi::menu-component 
                                                      :items (list 
                                                              (make-instance 'capi::menu-item :title "Load File..."
                                                                             :callback-type :interface
                                                                             :callback 'load-a-lisp-file
                                                                             :accelerator nil
                                                                             :enabled-function 'file-operations-enabled)
                                                              ))
                                              
                                       )))
          ))



(defun close-listener ()
  (om-close-window om-lisp::*om-listener*))

(defun listener-close (listenerwin)
  (capi::quit-interface listenerwin))

(defun listener-open-text-file (listenerwin &optional path)
  (open-text-file))

(defun listener-new-text-editor (listenerwin)
  (om-open-text-editor :lisp t))

(defun find-capi-pane-with-focus (layout)
  (capi:map-pane-descendant-children 
   layout
   #'(lambda (p)
       (when (capi:pane-has-focus-p p)
         (return-from find-capi-pane-with-focus p)))))


(defmethod om-copy-command ((self om-listener-pane))
  (call-editor self (list 'editor::copy-to-cut-buffer-command (editor-pane-buffer self))))

(defun listener-copy (listenerwin)
  (let ((pane (find-capi-pane-with-focus (pane-layout listenerwin))))
    (om-copy-command pane)))


(defmethod om-paste-command ((self om-listener-pane))
  (call-editor self (list 'editor::insert-cut-buffer-command (editor-pane-buffer self))))

(defun listener-paste (listenerwin)
  (let ((pane (find-capi-pane-with-focus (pane-layout listenerwin))))
    (om-paste-command PANE)))


(defmethod om-cut-command ((self om-listener-pane))
  (let ((buffer (editor-pane-buffer self)))
    (call-editor self (list 'editor::copy-to-cut-buffer-command buffer))
    (call-editor self (list 'editor::kill-region-command buffer))))

(defun listener-cut (listenerwin)
  (let ((pane (find-capi-pane-with-focus (pane-layout listenerwin))))
    (om-cut-command pane)))


(defmethod om-select-all-command ((self om-listener-pane))
  (let ((buffer (editor-pane-buffer self)))
    (editor::use-buffer buffer
      (editor::with-point ((p (editor::buffer-point buffer)))
        (call-editor self (list 'editor::beginning-of-buffer-cancelling-selection-command buffer))
        #+cocoa(call-editor self (list 'editor::end-of-buffer-extending-selection-command buffer))
        #-cocoa(call-editor self (list 'editor::end-of-buffer-modifying-selection-command buffer))
        ))
    ))

(defun listener-select-all (listenerwin)
  (let ((pane (find-capi-pane-with-focus (pane-layout listenerwin))))
    (om-select-all-command pane)))


(defun listener-end-of-buffer (listenerwin)
  (let* ((pane (op listenerwin))
         (buffer (editor-pane-buffer pane)))
    (capi::apply-in-pane-process pane
     #'(lambda (pa bu) 
         (editor::use-buffer bu
           (editor::with-point ((p (editor::buffer-point bu)))
             (call-editor pa (list 'editor::end-of-buffer-command bu))
             ))) 
     pane buffer)))


(defvar *fasl-extension* (pathname-type (cl-user::compile-file-pathname "")))
(defvar *last-open-directory* nil)

(defmethod load-a-lisp-file ((self t))
  (let ((filename (capi::prompt-for-file "Choose a File to Load..." 
                                         :filters (list "All files" "*.*" "Lisp File" "*.lisp" "Compiled Lisp File" 
                                                      (concatenate 'string "*." *fasl-extension*))
                                         :pathname *last-open-directory*)))
    (when filename
      (if (probe-file filename)
          (progn 
            (setf *last-open-directory* (make-pathname :directory (pathname-directory filename)))
            (load filename)
            (print (concatenate 'string "File " (namestring filename) " loaded."))
            )
        (progn 
          (beep-pane nil)
          (print (concatenate 'string "File " (namestring filename) " not found."))
          ))
      )))

(defun listener-find-definition (listenerwin)
 (with-slots (ep) listenerwin
    (let* ((pane (find-capi-pane-with-focus (pane-layout listenerwin)))
          (buffer (editor-pane-buffer pane))
          (symbol nil))
      (editor::use-buffer buffer
        (setf symbol (editor::intern-symbol-from-string (editor::read-symbol-from-point :previous t :read-package-name t)))
        (when symbol (om-lisp::om-edit-definition symbol))
      ))))


(defun om-prompt-on-echo-area (listener-pane message)
  (with-slots (editor-window) listener-pane
    (capi::apply-in-pane-process 
     listener-pane 
     'editor:process-character    
     (list 'editor:message message)
     editor-window)))

(defun om-listener-echo (str)
  (when om-lisp::*om-listener*
    (capi:execute-with-interface 
     om-lisp::*om-listener*
     #'(lambda ()
         (with-slots (op) om-lisp::*om-listener*
           (om-prompt-on-echo-area op str)
           )))))

;;; not called anymore: abort directly from the patch windows
(defun listener-abort (listenerwin)
  (declare (ignore listenerwin))
  (om-kill-eval-process)
  (om-listener-echo "Aborted Evaluation"))


(defmethod choose-listener-font ((self om-listener))
  (with-slots (ip op) self
    (let ((newfont (capi::prompt-for-font "" :font (capi::simple-pane-font op))))
      
      (when ip (setf (capi::simple-pane-font ip) *listener-font*)
        (when (capi::editor-pane-echo-area ip)
          (setf (capi::simple-pane-font (capi::editor-pane-echo-area ip)) *listener-font*)))
      
      (setf (capi::simple-pane-font op) *listener-font*)
      (setf (capi::simple-pane-font (capi::editor-pane-echo-area op)) *listener-font*)
      )))

(defmethod update-listener-font ((self om-listener))
  (with-slots (ip op) self
    (when ip (setf (capi::simple-pane-font ip) *listener-font*)
        (when (capi::editor-pane-echo-area ip)
          (setf (capi::simple-pane-font (capi::editor-pane-echo-area ip)) *listener-font*)))
    (when ip (setf (capi::simple-pane-font op) *listener-font*)
        (when (capi::editor-pane-echo-area op)
          (setf (capi::simple-pane-font (capi::editor-pane-echo-area op)) *listener-font*)))
    ))

(defun om-set-listener-font (newfont)
  (setf *listener-font* newfont)
  (mapc 'update-listener-font (capi::collect-interfaces 'om-listener)))



;;;===========================
;;; ERROR REPORTING
;;;===========================

(defparameter *error-backtrace* nil)

(defun backtrace-enabled (listenerwin) *error-backtrace*)

(defun listener-error-backtrace (listenerwin) (om-show-error-backtrace))

(defun om-show-error-backtrace ()
  (if *error-backtrace*
      (om-lisp::om-show-output-lines *error-backtrace* "Error Backtrace")
      (progn 
        (beep-pane)
        (print "no backtrace recorded")
        nil)))


;;;===========================
;;; SHELL
;;;===========================
 
;;; A Shell in LW
;;; copied from the LW tutorials

(defparameter *om-shell* nil)

(defclass om-shell-window (capi:interface) ())

;(om-make-listener :input t)
;(setf om-lisp::*om-listener* nil)
(defun om-open-shell (&key x y w h)
  (or (and om-lisp::*om-shell* (capi::find-interface 'om-shell-window))
      (let ((sp (make-instance 'capi:shell-pane)))
        (capi::display
         (setf om-lisp::*om-shell*
               (make-instance 'om-shell-window :title "SHELL"
                            :best-x x :best-y y
                            :best-width (or w 360) :best-height (or h 200)
                            :window-styles '(:no-character-palette)
                            :destroy-callback (lambda (window) (setf om-lisp::*om-shell* nil))
                            :layout (make-instance 'capi:simple-layout 
                                                   :description  (list sp)))))
        )))

; This function emulates user input on pane :
(defun send-keys-to-pane-aux (pane string newline-p)
  (loop for char across string do (capi:call-editor pane char))
  (if newline-p (capi:call-editor pane #\Return)))

; This function trampolines to send-keys-to-pane-aux on the right process:
(defun send-keys-to-pane (pane string newline-p)
  (capi:apply-in-pane-process pane 
                              'send-keys-to-pane-aux
                              pane string newline-p))

 ;This call emulates the user typing dir followed by Return :
(defun om-send-command-to-shell (cmdline)
  (when *om-shell*
    (send-keys-to-pane *om-shell* cmdline t)))


;;; (om-open-shell)



