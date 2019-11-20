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
;=========================================================================

(in-package :om-lisp)

(export 
 'om-show-output-lines
 :om-lisp)

(defvar *out-buffer* nil)
(defvar *out-window* nil)

(defclass output-win (capi::interface) ())

(defun show-output-window (&optional windowtitle)
   (unless *out-window* 
     (setf *out-window* (make-instance 'output-win 
                                       :title ""
                                       :best-width 500
                                       :best-height 400
                                       :layout
                                       (make-instance 'simple-layout
                                                      :description 
                                                      (list (make-instance 'capi::collector-pane 
                                                                           :buffer *out-buffer*
                                                                           :font (gp::make-font-description :family "Verdana" :size 10)
                                                                           :width 300
                                                                           )))
                                       :destroy-callback #'(lambda (window) 
                                                             (declare (ignore window))
                                                             (setf *out-window* nil))
                                       :auto-menus nil
                                       :menu-bar-items (list (make-instance 'capi::menu :title "File"
                                                                     :items (list (make-instance 'capi::menu-item :title "Close"
                                                                                                 :callback-type :interface
                                                                                                 :callback 'quit-interface
                                                                                                 :accelerator #\w))))
                                       ))
     )
   (capi::execute-with-interface *out-window* #'(lambda () 
                                                  (display *out-window*)
                                                  (when windowtitle (setf (capi::interface-title *out-window*) windowtitle))
                                                  (capi::find-interface 'output-win)))
   )

(defun om-show-output-lines (lines &optional windowtitle)
  (unless *out-buffer* 
    (setf *out-buffer* (om-make-buffer)))
  (om-buffer-delete *out-buffer*)
  (if (consp lines)
      (mapc #'(lambda (line) (om-buffer-insert *out-buffer* (format nil "~A~%" line))) lines)
    (om-buffer-insert *out-buffer* lines))
  (show-output-window windowtitle))


;; test :
;; (om-show-output-lines '("this is a test" "for the default output-text window"))


  