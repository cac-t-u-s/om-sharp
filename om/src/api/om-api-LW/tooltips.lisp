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
; TOOLTIPS
;===========================================================================
;;; NOT USED....


(in-package :om-api)


;==========
; export :
;==========
(export '(
          om-tt-view
          om-show-tooltip
          om-hide-tooltip
                ) :om-api)

;======================
(in-package :oa)



#|
;;; WE DON'T USE THIS SYSTEM

(defmethod handle-tooltips-in-motion ((self t) position) nil)

(defmethod handle-tooltips-in-motion ((self om-view) position)
  (when (om-command-key-p)
    (let ((po (capi::pinboard-object-at-position self (om-point-x position) (om-point-y position))))
      (if po
          (let ((text (and 
                       (help-string po)
                       (not (string-equal (help-string po) ""))
                     ;(reduce #'(lambda (val segment) (concatenate 'string val segment (string #\Newline)))
                     ;        (oa::text-wrap-pix (help-string po) (om-def-font :font1) 200)
                     ;        :initial-value "")
                       (help-string po)
                       )))
            (when text
              (capi:display-tooltip self
                                    :x (+ (om-point-x (om-view-position po)) 10)
                                    :y (- (om-point-y (om-view-position po)) 10) 
                                    :text text)
              ))
      (capi:display-tooltip self)))))


(defmethod om-show-tooltip ((self om-graphic-object) &optional (remove nil) (short nil))
  (when (om-get-view self)
    (internal-show-tooltip self remove short)))
                     

(defmethod internal-show-tooltip ((self om-graphic-object) &optional (remove nil) (short nil))
  (if (and 
       (help-string self) 
       (not (string-equal (help-string self) ""))
       t)
      (let ((text (reduce #'(lambda (val segment) (concatenate 'string val segment (string #\Newline)))
                          (oa::text-wrap-pix (help-string self) (om-def-font :font1) 200)
                          :initial-value "")))
        (if short 
            (setf text (string-downcase (read-from-string text)))
          (setf text (subseq text 0 (- (length text) 1))))
        (multiple-value-bind (x y) (capi::current-pointer-position :relative-to (om-get-view self))     
          (capi:display-tooltip (om-get-view self) :x x :y (- y 20) :text text)
          #+cocoa(sleep (if remove 0.05 0.1))
          ))
    (capi:display-tooltip (om-get-view self))
    ))


;; protect, e.g. om-tab-layout
(defmethod om-hide-tooltip ((self t)) nil)

(defmethod om-hide-tooltip ((self capi::output-pane)) 
  (when (om-get-view self)
    (capi:display-tooltip (om-get-view self))))

(defmethod om-show-tooltip ((self om-standard-dialog-item) &optional remove short) nil) 
(defmethod om-hide-tooltip ((self om-standard-dialog-item)) nil) 

|#


;;; NOT USED
;;; WORKS ONY FOR DIALOG ITEMS
;(defmethod om-help-callback ((interface om-abstract-window) pane type key)
;  (when (and *helpon* (om-command-key-p))
;    (multiple-value-bind (x y) (capi::current-pointer-position :relative-to pane)
;      (let ((hview (or (capi::pinboard-object-at-position pane x y) pane)))
;        (case type
;          (:tooltip (help-spec hview))
;          (t nil)
;          )))))
;    (:pointer-documentation-enter
;     (when (stringp key)
;       (setf (capi:titled-object-message interface)
;             key)))
;    (:pointer-documentation-leave
;     (setf (capi:titled-object-message interface)
;           "Something else"))
;    (:help (do-detailed-help interface )))



;;;================================================
;;; CUSTOM TOOLTIP SYSTEM
;;;================================================

(defvar *tt-process* nil)
(defparameter *tt-font* (om-def-font :font1b :size 10)) ; :face "Calibri")))
  
;;; call by OM-API motion handler
(defmethod handle-tooltips-in-motion ((self t) position) nil)

;;; a mixing superclass for views that can display tooltips
(defclass om-tt-view (om-transient-drawing-view)  
  ((tt :accessor tt :initform nil)
   (ttx :accessor ttx :initform 0)
   (tty :accessor tty :initform 0)
   (ttw :accessor ttw :initform 0)
   (tth :accessor tth :initform 0)))

(defmethod om-show-tooltip ((view t) text &optional pos delay) nil)
(defmethod om-hide-tooltip ((view t)) nil)


(defun tooltip-draw (view)
  (oa::om-with-focused-view view
    (om-with-fg-color (om-make-color 0. 0. 0. 0.5)
      (om-draw-rect (ttx view) (tty view)
                    (ttw view) (tth view) :fill t))
    (om-with-fg-color (om-make-color 1 1 1)
      (om-with-font *tt-font*
                    (if (listp (tt view))
                        (loop for line in (tt view) 
                              for n = (+ (tty view) 12) then (+ n 12) 
                              do
                              (om-draw-string (+ (ttx view) 5) n line))
                      (om-draw-string (+ (ttx view) 5) (+ (tty view) 12) (tt view))
                      )
                    ))
    ))
    
(defmethod om-show-tooltip ((view om-tt-view) text &optional pos (delay 0.3))
  (when *tt-process* (om-kill-process *tt-process*)) 
  (let ((pp (or pos (om-add-points (om-mouse-position view) (om-make-point 0 -25)))))
    
    (setf *tt-process*
        (om-run-process 
         "tooltip"
         #'(lambda () 
             (sleep delay)
             (setf (tt view) text
                   (ttx view) (om-point-x pp)
                   (tty view) (om-point-y pp)
                   (ttw view) (+ 20 (if (listp text)
                                        (loop for line in text maximize (om-string-size line *tt-font*))
                                      (om-string-size text *tt-font*)))
                   (tth view) (+ 6 (* 12 (if (listp text) (length text) 1)))
                   )
             (capi::apply-in-pane-process 
              view 
              #'(lambda () 
                  (om-start-transient-drawing view 
                                              #'(lambda (view pos size) (tooltip-draw view))
                                              pp (omp (ttw view) (tth view)))))
             
             ;(capi::apply-in-pane-process view 'om-invalidate-view 'tooltip-draw view)
             )))
    ))
          
(defmethod om-hide-tooltip ((view om-tt-view))
  (when (and view (tt view))
    (om-invalidate-area view (ttx view) (tty view) (+ (ttx view) (ttw view)) (+ (tty view) (tth view)))
    (setf (tt view) nil))
  (om-stop-transient-drawing view)
  (when *tt-process* 
    (om-kill-process *tt-process*)
    (setf *tt-process* nil)))





