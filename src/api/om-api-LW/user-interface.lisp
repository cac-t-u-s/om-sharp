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
; PREDEFINED DIALOGS AND MESSAGES
;;===========================================================================



(in-package :om-api)

;;;==========
;;; export :
;;;==========
(export '(
          om-get-user-string
          om-choose-directory-dialog
          om-choose-new-directory-dialog
          om-choose-file-dialog
          om-choose-new-file-dialog
          om-y-or-n-dialog
          om-y-n-cancel-dialog
          om-choose-color-dialog
          om-pick-color-view
          om-choose-font-dialog
          om-message-dialog
          om-beep
          ) :om-api)


;;;==========

(defvar *last-directory* nil)

(defun def-dialog-owner ()
  #-cocoa nil
  #+cocoa (capi::convert-to-screen))


(defun om-get-user-string (prompt &key (initial-string ""))
  (capi::prompt-for-string prompt :initial-value initial-string
                           :pane-args `(:visible-min-width ,(max 300 (om-string-size initial-string (om-def-font :large))))
                           :accept-null-string t))


;;; prompt does not work (macOS 10.14) ?
(defun om-choose-file-dialog (&key (prompt "Choose a File") (directory nil) (types nil))
  (let ((rep (capi::prompt-for-file prompt :filters types :filter (if types (cadr types)) :owner (def-dialog-owner)
                                    :pathname (or directory *last-directory*))))
    (when rep
      (setf *last-directory* (make-pathname :directory (pathname-directory rep))))
    rep))


;(defmacro om-with-file-dialog-process ((file &key (prompt "Choose a File") (directory nil) (button-string "OK") (types nil))
;                                       &body body)
;  `(capi:with-dialog-results (,file ok-p)
;      (capi::prompt-for-file ,prompt ;:filters ',types :filter (if ',types (cadr ',types))
;                             :owner ,(def-dialog-owner)
;                             :pathname ,(or directory *last-directory*))
;      (when ,file
;       (setf *last-directory* (make-pathname :directory (pathname-directory ,file))))
;     ,@body))


(defun om-choose-new-file-dialog (&key (prompt "Choose a new file") (directory nil) (name "") (types nil))
  (let* ((dir (or directory *last-directory*))
         (rep (capi::prompt-for-file prompt :filters types :filter (if types (cadr types)) :owner (def-dialog-owner)
                                     :pathname (make-pathname :directory (when dir (pathname-directory dir)) :name name)
                                     :operation :save)))
    (when rep
      (setf *last-directory* (make-pathname :directory (pathname-directory rep))))
    rep))


(defun om-choose-directory-dialog (&key (prompt "Choose a directory") (directory nil))
  (let ((rep (capi::prompt-for-directory prompt :owner (def-dialog-owner) :pathname (or directory *last-directory*))))
    (when rep
      (setf *last-directory* (make-pathname :directory (pathname-directory rep))))
    rep))


(defun om-choose-new-directory-dialog (&key (prompt "Choose location and name for the new directory") (directory nil) (defname nil))
  (let* ((dir (or directory *last-directory*))
         (def (if dir
                  (make-pathname :directory (pathname-directory dir) :name defname)
                defname))
         (path (capi::prompt-for-file prompt :owner (def-dialog-owner)
                                      :filter nil :filters nil :pathname def
                                      :operation :save)))
    (when path
      (setf *last-directory* (make-pathname :directory (pathname-directory path)))
      (make-pathname :device (pathname-device path) :directory (append (pathname-directory path) (list (pathname-name path))))
      )))


(defun om-y-or-n-dialog (message &key (default nil))
  (capi::prompt-for-confirmation message :default-button (if (equal default :yes) :ok nil)))


(defun om-y-n-cancel-dialog (message &key (default nil))
  (multiple-value-bind (answer successp)
      (capi:prompt-for-confirmation message :cancel-button t
                                    :default-button (if (equal default :yes) :ok nil))
    (if successp answer :cancel)))


;;; USED IN THE MAC
(defclass om-pick-color-view (om-view)
  ((color :accessor color :initarg :color :initform (om-make-color 0 0 0))
   (after-fun :accessor after-fun :initform nil :initarg :after-fun)))

(defmethod om-draw-contents ((self om-pick-color-view))
  (om-draw-rect 0 0 (om-width self) (om-height self) :fill nil :color (om-def-color :gray))
  (when (= 0 (om-color-a (color self)))
    (om-draw-line 0 0 (om-width self) (om-height self) :color (om-def-color :gray))
    (om-draw-line 0 (om-height self) (om-width self) 0 :color (om-def-color :gray))
    ))

(defmethod set-color ((self om-pick-color-view) (color omcolor))
  (om-set-bg-color self color)
  (setf (color self) color)
  (when (after-fun self) (funcall (after-fun self) self)))

(defmethod om-view-click-handler ((self om-pick-color-view) pos)
  (declare (ignore pos))
  (let ((color (om-make-color-alpha
                (make-omcolor :c (capi::prompt-for-color "Color Chooser" :color (omcolor-c (color self))))
                (om-color-a (color self)))))
    (set-color self color)))


#+cocoa
(defun om-choose-color-dialog (&key color alpha owner)
  (let* ((win (om-make-window 'om-dialog ;:size (om-make-point 240 110)
                              :resize nil :window-title "Choose a New Color..."
                              :win-layout 'om-row-layout))
         (col (or color (om-def-color :black)))
         (coloritem (om-make-view 'om-pick-color-view
                                  :position (om-make-point 40 20)
                                  :size (om-make-point 60 60)
                                  :color col
                                  :bg-color col)))
    (om-add-subviews win

                     (if alpha
                         (om-make-layout 'om-row-layout
                                         :subviews (list
                                                    coloritem
                                                    (om-make-di 'om-slider
                                                                :direction :vertical ;; does not work... ?
                                                                :range '(0 255)
                                                                :value (round (* (om-color-a (color coloritem)) 255))
                                                                :size (omp 30 nil)
                                                                :di-action #'(lambda (item)
                                                                               (let ((newcolor (make-omcolor :c (color::color-with-alpha
                                                                                                                 (omcolor-c (color coloritem))
                                                                                                                 (/ (om-slider-value item) 255.0)))))
                                                                                 (set-color coloritem newcolor)
                                                                                 ))
                                                                )))
                       coloritem)

                     (om-make-layout 'om-column-layout
                                     :subviews (list
                                                (om-make-di 'om-button :position (om-make-point 130 26) :size (om-make-point 80 24)
                                                            :text "Cancel"
                                                            :di-action #'(lambda (item)
                                                                           (declare (ignore item))
                                                                           (om-return-from-modal-dialog win nil)))
                                                (om-make-di 'om-button
                                                            :position (om-make-point 130 58)
                                                            :size (om-make-point 80 24)
                                                            :text "OK"
                                                            :focus t
                                                            :default t
                                                            :di-action #'(lambda (item)
                                                                           (declare (ignore item))
                                                                           (om-return-from-modal-dialog win (color coloritem)))))
                                     ))
    (om-modal-dialog win owner)))

#-cocoa
(defun om-choose-color-dialog (&key color alpha owner)
  (let ((rep (capi::prompt-for-color "Choose a color" :color (when color (omcolor-c color))
                                     :owner owner)))
    (when rep
      (make-omcolor :c rep))))

; (om-choose-color-dialog)


(defun om-message-dialog (message)
  (capi::display-message message))

(defun om-beep ()
  (capi::beep-pane nil))

(defun om-choose-font-dialog (&key (font (om-def-font :large)))
  (let ((font (capi::prompt-for-font "Choose a font" :font font :owner nil)))
    (and font (gp::font-description font))))
