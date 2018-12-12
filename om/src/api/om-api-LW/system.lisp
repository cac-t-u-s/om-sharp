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

;===========================================================================
; GENERAL SYSTEM UTILITIES
;===========================================================================

(in-package :om-api)

;;;===================
;;; export :
;;;===================
(export '(
          om-quit
          om-standalone-p
          om-gc
          om-get-user-name
          om-get-date
          om-error-handle-funcall om-with-error-handle om-trap-errors om-with-redefinitions om-ignore&print-error
          om-set-clipboard om-get-clipboard  
          om-open-in-browser
          ) :om-api)


(defparameter *lw-version* 
  (read-from-string (subseq (lisp-implementation-version) 0 1)))

(defun om-standalone-p ()
  (if (member :om-deliver *features*) t nil))

(defun om-quit ()
  (when t ; (member :om-deliver *features*)
    (quit :confirm nil :ignore-errors-p t)))

(defun om-get-user-name ()
  (system::get-user-name))

(defun om-get-date ()
  (sys::date-string))

;;;====================
;;; MEMORY
;;;====================

;;; :error :warn nil
(setf system::*stack-overflow-behaviour* :warn)
;(hcl::current-stack-length)
;(hcl:extend-current-stack 50)

(defun om-gc () 
  (system::gc-all))

;;;====================
;;; ERROR MANAGEMENT
;;;====================

(defun om-error-handle-funcall (func)
  (handler-bind 
      ((error #'(lambda (err)
                  (capi::display-message "An error of type ~a occurred: ~%\"~a\"" (type-of err) (format nil "~A" err))
                  (abort err))))
    (funcall func)))

(defmacro om-with-error-handle (&body body)
  `(if (om-standalone-p)
      (handler-bind 
           ((error #'(lambda (err)
                       (capi::display-message "An error of type ~a occurred: ~%\"~a\"" (type-of err) (format nil "~A" err))
                       (abort err))))
         ,@body)
    (progn ,@body)))



(defmacro om-with-redefinitions (&body body)
  `(let ((lispworks::*HANDLE-WARN-ON-REDEFINITION* nil)) ,@body))

(defmacro om-ignore&print-error (&rest body)
  `(multiple-value-bind (a b) 
       (ignore-errors
         ,@body)
     (if b (print (format nil "Error: ~A" b)))
     a))

(defun om-error-handler (&rest l)
  (let ((err (car l))
        (backtrace (with-output-to-string (stream)
                    (dbg:output-backtrace t stream))))
   ;(print (car l)) (terpri) (print backtrace)
   (capi::display-message "ERROR: ~A~%" err)
   (setf om-lisp::*error-backtrace* (print (format nil "ERROR: ~A~%~%~A" err backtrace)))
   (abort)))

(defun om-trap-error-handler (condition)
   (format *error-output* "ERROR: ~A~&" condition)
   (throw 'trap-errors nil))

(defmacro om-trap-errors (&rest forms)
   `(catch 'trap-errors
      (handler-bind ((error #'om-trap-error-handler))
        ,@forms)))

(defun set-om-debugger ()
  ; (when (member :om-deliver *features*)
  (setq *debugger-hook* 'om-error-handler)
  )

(define-action "When starting image" "Init debug/backtrace tool" 'set-om-debugger)


;;;====================
;;; CLIPBOARD
;;;====================
(defun om-set-clipboard (value)
  (capi::set-clipboard (om-front-window) value))

(defun om-get-clipboard ()
  (capi::clipboard (om-front-window)))

;;;====================
;;; WEB
;;;====================
(defun om-open-in-browser (url)
  (sys::open-url url))



#|

;;; test: trying to access remote file contents via HTTP
;;; not working very well so far...

 (defun test-http ()
   (with-open-stream (http (comm:open-tcp-stream 
                            "www.lispworks.com" 
                         ;""https://github.com/openmusic-project/OM6/blob/master/README.md""
                            80
                            :errorp t))

     (format http "GET / HTTP/1.0~C~C~C~C"
             (code-char 13) (code-char 10)
             (code-char 13) (code-char 10))

     (force-output http)

     (write-string "Waiting to reply...")

     (loop for line = (read-line http nil nil);
           while line
           do (write-line line))
     )
   )
|#



