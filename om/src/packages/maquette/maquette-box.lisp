;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File authors: J. Bresson
;============================================================================

(in-package :om)

(defclass OMBoxMaquette (OMBoxPatch) ())

(defmethod special-box-p ((name (eql 'maquette))) t)

(defmethod get-box-class ((self OMMaquette)) 'OMBoxMaquette)

(defmethod omNG-make-special-box ((reference (eql 'maquette)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMMaquetteInternal
                  :name (if init-args (format nil "~A" (car (list! init-args))) "new-maquette"))
   pos 
   init-args ;; don't need to pass them in principle..
   ))


(defmethod next-optional-input ((self OMBoxMaquette))
  (< (length (get-optional-inputs self)) 2))

(defmethod more-optional-input ((self OMBoxMaquette) &key name (value nil val-supplied-p) doc reactive)
  (declare (ignore name doc))
   ;;; the first one is already here
   (add-optional-input self :name (if (get-optional-inputs self) "objs" "time")
                       :value (if val-supplied-p value nil) 
                       :reactive reactive)
   t)

(defmethod create-box-inputs ((self OMBoxMaquette))
  (append (call-next-method)
          (get-optional-inputs self)))

;;;=====================================
;;; BOX DISPLAY
;;;=====================================
(defmethod display-modes-for-object ((self OMMaquette)) '(:hidden :mini-view :text))

(defmethod draw-mini-view ((self OMMaquette) box x y w h &optional time)
  
  (let* ((boxes (remove-if #'(lambda (b) (not (group-id b))) (get-all-boxes self)))
         (n-tracks (apply #'max (or (mapcar #'group-id boxes) '(1))))
         (dur (get-obj-dur self))
         (box-h (/ (- h 30) n-tracks)))
    
    (flet 
        ((t-to-x (xpos) (round (* (/ xpos dur) w)))
         (id-to-y (id) (+ 8 (* box-h (1- id)))))
      
      (loop for b in boxes 
            do (om-draw-rect (+ x (t-to-x (box-x b)))
                             (+ y (id-to-y (group-id b)))
                             (t-to-x (box-w b))
                             box-h
                             :fill t
                             :color (box-draw-color b))
            (om-draw-rect (+ x (t-to-x (box-x b)))
                          (+ y (id-to-y (group-id b)))
                          (t-to-x (box-w b))
                          box-h 
                          :color (om-def-color :gray)
                          :fill nil)
            )
      )))
                             

;;;=====================================
;;; BOX DISPLAY
;;;=====================================

(defmethod omng-box-value :before ((self OMBoxMaquette) &optional numout)  
  (eval-maquette (reference self) NIL) ;;; eval the boxes in tracks but not the control-patch
  )

(defmethod compile-patch ((self OMMaquette)) 
  (setf (compiled? self) t)
  (compile-patch (ctrlpatch self)))

(defmethod compiled-fun-name ((self OMMaquette)) 
  (compiled-fun-name (ctrlpatch self)))

(defmethod compiled? ((self OMMaquette)) 
  (compiled? (ctrlpatch self)))


