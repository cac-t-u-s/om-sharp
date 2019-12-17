;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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

;(defmethod create-box-inputs ((self OMBoxMaquette))
;  (append (call-next-method)
;          (get-optional-inputs self)))

;;;=====================================
;;; BOX DISPLAY
;;;=====================================
(defmethod display-modes-for-object ((self OMMaquette)) '(:mini-view :text :hidden))

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
;;; EVAL / COMPILE
;;;=====================================

(defun default-delta (tlist)
   (cond
    ((null tlist) 0)
    ((= (length tlist) 1) (car tlist))
    (t (- (car (last tlist 1)) (car (last tlist 2))))))


(defmethod put-boxes-in-maquette ((self OMBoxMaquette))
  
  (let* ((tlist-in (find "time" (get-optional-inputs self) :key #'name :test #'string-equal))
         (tlist (and tlist-in (omng-box-value tlist-in)))
         (objs-in (find "objs" (get-optional-inputs self) :key #'name :test #'string-equal))
         (objs (and objs-in (omng-box-value objs-in))))
    
    (when objs
      (let* ((maquette (reference self))
             (timelist (if (listp tlist) tlist (list 0 tlist)))
             (objlist (list! objs))
             (deftime (default-delta timelist))
             (facty (round 100 (length objlist))))
        
        (m-flush maquette)
        (close-editor maquette)
                
        (loop with old-time = 0 
              for item in objlist 
              for i from 1 do
              (let* ((x (if timelist (pop timelist) (+ old-time deftime))))
                (let ((newbox 
                       (cond ((subtypep (type-of item) 'OMBox)
                              (let ((b (clone item)))
                                (setf (lambda-state b) nil)
                                b))
                             ((subtypep (type-of item) 'OMPatch)
                              (if (is-persistant item)
                                  (omNG-make-new-boxcall item (omp 0 0))
                                (omNG-make-new-boxcall (clone item) (omp 0 0))
                                ))
                             (t
                              (omng-make-new-boxcall (class-of item) (omp 0 0) item)))))
                  (when newbox
                    (setf (box-x newbox) x
                          (box-w newbox) 1000         ;;; (default)
                          (box-y newbox) (* i facty)
                          (box-h newbox) (- facty)
                          (group-id newbox) (mod i 4))
                     
                    (setf (display newbox) (or (find :mini-view (display-modes-for-object (get-box-value newbox))) :text))
                    (omNG-add-element maquette newbox)
                    
                    ))
                (setf old-time x)))
        
        ;;; the duration of the patch boxes is not computed yet at this point....
        (setf (range maquette)
              (list :x1 0 :x2 (or (get-obj-dur maquette) 4000)
                    :y1 -10 :y2 110))
        t
        ))))


(defmethod omng-box-value :before ((self OMBoxMaquette) &optional numout)  
  (unless
      (or (equal (lock-state self) :locked)
          (and (or (equal (lock-state self) :eval-once)
                   (get-pref-value :general :auto-ev-once-mode))
               (equal (ev-once-flag self) (get-ev-once-flag *ev-once-context*))))

    (put-boxes-in-maquette self)
    (eval-maquette (reference self) NIL) ;;; eval the boxes in tracks but not the control-patch
    ))


(defmethod eval-box-inputs ((self OMBoxMaquette))
  (loop for input in (get-standard-inputs self)
        collect (omNG-box-value input)))


(defmethod compile-patch ((self OMMaquette)) 
  (setf (compiled? self) t)
  (compile-patch (ctrlpatch self)))

(defmethod compiled-fun-name ((self OMMaquette)) 
  (compiled-fun-name (ctrlpatch self)))

(defmethod compiled? ((self OMMaquette)) 
  (compiled? (ctrlpatch self)))


