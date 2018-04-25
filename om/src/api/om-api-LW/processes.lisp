;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
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
; Authors: J. Bresson, C. Agon
;=========================================================================

;;===========================================================================
; PROCESSES MANAGEMENT
;;===========================================================================

(in-package :om-api)

;;;===================
;;; export :
;;;===================
(export '(
          om-run-process
          om-kill-process
          om-find-process
          om-stop-process
          om-resume-process
          om-poke-process
          om-process-name
          om-process-wait
          om-process-wake-up-at

          om-eval-in-context
          om-use-eval-process
          om-eval-enqueue

          om-command-line
          om-run-application
          om-run-program
          om-select-program
          
          om-get-internal-time

          om-with-timeout
          
          ) :om-api)

;;;===================================
;;; MULTI PROCESSING
;;;===================================

(declaim (inline om-get-internal-time))
(defun om-get-internal-time ()
  #+mach (mach::mach-time) #-mach (get-internal-real-time))

(declaim (inline om-time-sleep))
(defun om-time-sleep (delay-ms)
  #+mach (mach::mach-wait-delay delay-ms)
  #-mach (sleep (/ delay-ms 1000.0)))

(declaim (inline time-sleep-until))
(defun time-sleep-until (absolute-time-ms)
  #+mach (mach::mach-wait-until absolute-time-ms)
  #-mach (sleep (max 0 (/ (- absolute-time-ms (get-internal-real-time)) 1000.0))))

(defun om-run-process (name func &key priority args)
  (apply #'mp:process-run-function (append (list name (list :priority (or priority 10)) func) args)))

(declaim (inline om-stop-process))
(defun om-stop-process (process)
  (if (eq (type-of process) 'mp::process)
      (mp:process-stop process)))

(declaim (inline om-resume-process))
(defun om-resume-process (process)
  (if (and (eq (type-of process) 'mp::process)
           (mp:process-stopped-p process))
    (mp:process-unstop process)))

(declaim (inline om-poke-process))
(defun om-poke-process (process)
  (if (eq (type-of process) 'mp::process)
      (mp:process-poke process)))

(declaim (inline om-kill-process))
(defun om-kill-process (process)
  (if (eq (type-of process) 'mp::process)
      (mp:process-kill process)))

(defun om-find-process (id)
  (mp:find-process-from-name id))

(defun om-process-name (process)
  (if (eq (type-of process) 'mp::process)
      (mp:process-name process)))

(declaim (inline om-process-wait))
(defun om-process-wait (delay-ms)
  #+mach (mach::mach-wait-delay delay-ms)
  #-mach (mp:process-wait-with-timeout "Waiting" (/ delay-ms 1000.0)))

(declaim (inline om-process-wake-up-at))
(defun om-process-wake-up-at (absolute-time-ms)
  #+mach (mach::mach-wait-until absolute-time-ms)
  #-mach (mp:process-wait-with-timeout "Waiting" (/ (- absolute-time-ms (get-internal-real-time)) 1000.0)))

(defun om-with-timeout (timeout timeout-function body-fn)
  (declare (dynamic-extent body-fn))
  (let ((process mp:*current-process*))
    
    (labels ((timeout-throw ()
               (when (find-restart 'abort-execution)
                 (invoke-restart 'abort-execution)))

             (timeout-action ()
               (mp:process-interrupt process #'timeout-throw)))
      
      (declare (dynamic-extent #'timeout-throw #'timeout-action))

      (let ((timer (mp:make-named-timer 'timer #'timeout-action)))
        (catch 'tag
          (unwind-protect
              (restart-case 
                  (progn
                    (when timeout
                      (mp:schedule-timer-relative timer timeout))
                    (return-from om-with-timeout
                      (funcall body-fn)))
                (continue-from-timeout () 
                                       (throw 'tag 
                                              (when timeout-function
                                                (funcall timeout-function))))
                (abort-execution () 
                                 (throw 'tag
                                        (when timeout-function
                                          (funcall timeout-function)))))
            (mp:unschedule-timer timer)))))))


;;;==========================
;;; EVALUATION PROCESS MANAGEMENT
;;;==========================

(defun om-eval-in-context (form &optional context)
  (capi::apply-in-pane-process context 'eval form))

(defparameter *use-eval-process* t)

;;; use a separate thread to eval with OM-EVAL-ENQUEUE
(defun om-use-eval-process (mode)
  (setq *use-eval-process* mode))


(defun om-eval-enqueue (form &key eval-context-view post-action) 
  
  (let ((eval-fun 
         #'(lambda () 
             (if eval-context-view 
                 (om-eval-in-context form eval-context-view)
               (eval form))
             (when post-action (funcall post-action)))
         ))
    
    (if *use-eval-process*
        (om-lisp::om-eval-on-process eval-fun)
      (funcall eval-fun)
      
      )))


;;;===============================
;;; RUN/MANAGE EXTERNAL PROGRAMS
;;;=============================== 

(defun om-command-line (str &optional (redirect-output nil))
  (if #+macosx (pathnamep redirect-output) #-macosx NIL
      ;(let ((tempfile "~/om-log.txt"))
      (sys:run-shell-command str :show-window t :wait t :output redirect-output :error-output redirect-output 
                             :if-output-exists :append :if-error-output-exists :append)
       ;(om-lisp::om-open-text-editor :contents (pathname tempfile)))
    (progn
      (if redirect-output
          (sys:call-system-showing-output str :wait t :output-stream om-lisp::*om-stream*)
        #-win32(sys:run-shell-command str :wait t)
        #+win32(sys:call-system str :wait t)
        ))))

;(sys::change-directory  (om-make-pathname :directory om::*om-midi-settings-app-path*))
;(hcl::get-working-directory)

; (sys:run-shell-command (format nil "~s" (namestring om::*om-midi-settings-app-path*)))
;(system::call-system (format nil "~s" (namestring om::*om-midi-settings-app-path*)) :wait t)


;;; doit retourner un ID !!
;;; path = un exe ou bien un .app dont on vet executer l'exe
(defun om-run-program (path &optional afterfun)
  (let* ((pathstr (namestring path))
         (dir (om-make-pathname :directory path))
         (name (pathname-name path)))
    (when (equal (elt pathstr (- (length pathstr) 1)) #\/)
      (setf pathstr (subseq pathstr 0 (- (length pathstr) 1)))
      (let ((appname (pathname-name (pathname (subseq pathstr 0 (- (length pathstr) 1))))))
        (setf pathstr (namestring (make-pathname :directory (append (pathname-directory path) 
                                                                    (list "Contents" "MacOS"))
                                                 :name appname)))))
        (mp:process-run-function (namestring path) nil
                                 #'(lambda ()
                                     ;;; (sys::cd dir)
                                     #-win32(system::run-shell-command pathstr :wait t)
                                     #+win32(system::call-system (format nil "~s" (namestring pathstr)) :wait t)
                                     (when afterfun (funcall afterfun))))
        (namestring path)))

(defun om-run-application (path)
  (system::call-system (format nil "open ~s" (namestring path)) :wait nil)
  (namestring path))

;;; marche pour un process créé avec la fonction om-run-program ou om-run-application
(defun om-select-program (id)
  (system::call-system (concatenate 'string "open " (namestring id))))






























