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
; File author: J. Bresson
;============================================================================


(in-package :om)

;;;============ 
;;; TIME-MAP
;;;============

#|
(position-if 
 #'(lambda (e) (and (<= (car e) 4200)
                    (not (caddr e))))
 '((0 10) (0 110) (4000 210) (8000 310)))
|#

;;; time-map is a simple BPF-like list with (time pos) pairs 

(defstruct space-point (onset) (before) (after))


;;;=====================================
;;; Rebuilding time-map
;;;=====================================
     
;;; at creating the display-cache (e.g. when the object is reevaluated)
(defmethod ensure-cache-display-draw ((box omboxeditcall) (obj chord-seq))
  (set-edit-param box :time-map (build-time-map obj)))

;;; at editing the object
(defmethod report-modifications ((self score-editor))
  (call-next-method)
  (editor-set-edit-param self :time-map (build-time-map (object-value self))))

;;; main function to build time-map
(defmethod build-time-map ((obj score-object))
  (let* ((time-space (sort 
                      (loop for sub in (inside obj)
                            append (build-object-time-space sub (tempo obj)))
                      #'< :key #'space-point-onset))
         (merged-list ()))

    ;;; build-object-time-space returns a list of (onset (spacebefore space-after))
    ;;; objects with same onsets must be grouped, and space maximized
    ;;; negative space (e.g. from measures) will affect the space of previous item 
    
    (loop for item in time-space
          do (if (and (car merged-list) 
                      (= (space-point-onset item) (space-point-onset (car merged-list))))  ;;; already something there
                 
                 (setf (space-point-before (car merged-list))  
                       (if (plusp (space-point-before item))
                           (max (space-point-before (car merged-list)) (space-point-before item))
                         ;;; negative value
                         (+ (space-point-before (car merged-list)) (- (space-point-before item))))
               
                       (space-point-after (car merged-list))  
                       (max (space-point-after (car merged-list)) (space-point-after item)))
               
               (push item merged-list)))
    
    (setf merged-list (reverse merged-list))
    
     (cons 
      (list (space-point-onset (car merged-list))
            (space-point-before (car merged-list)))
      
      (loop with curr-x = (space-point-before (car merged-list))
            for rest on merged-list
            while (cdr rest)
            do (setf curr-x (+ curr-x 
                               (space-point-after (car rest)) 
                               (space-point-before (cadr rest))))
            collect (list (space-point-onset (cadr rest)) (round curr-x)))
      )
     ))
                              


                              
;;; data for specific objects:

(defmethod build-object-time-space ((self rhythmic-object) tempo)
 (let ((space (object-space-in-units self)))
   (cons 
    (make-space-point :onset  (beat-to-time (symbolic-date self) tempo)
                      :before (first space) :after (second space))
    (loop for sub in (inside self) append  
          (build-object-time-space sub tempo))
    )))

(defmethod build-object-time-space ((self score-object) tempo)
  (let ((space (object-space-in-units self)))
    (list (make-space-point :onset  (beat-to-time (symbolic-date self) tempo)
                            :before (first space) :after (second space)))))


(defmethod object-space-in-units ((self t)) '(0 0))

(defmethod object-space-in-units ((self measure)) '(-4 4))

(defmethod object-space-in-units ((self chord)) (list 2 (+ 1 (* 10 (symbolic-dur self)))))

(defmethod object-space-in-units ((self continuation-chord)) 
  (object-space-in-units (previous-chord self)))

(defmethod object-space-in-units ((self r-rest)) (list 2 (+ 1 (* 10 (symbolic-dur self)))))



;;; Time-function:
(defmethod score-time-to-pixel (view time time-map unit)
  (let* 
      ((prev-point-pos (position time time-map :test #'>= :key #'car :from-end t))
       (prev-point (nth prev-point-pos time-map))
       (next-point (nth (1+ prev-point-pos) time-map))
         
       (units-from-0 
        (cond ((= time (car prev-point))
               ;;; the point was in the list
               (cadr prev-point))
              (next-point
               ;;; interpolate between prev and next
               (+ (cadr prev-point)
                  (* (- time (car prev-point)) 
                     (/ (- (cadr next-point) (cadr prev-point)) (- (car next-point) (car prev-point)))))
               )
              (t ;;; we are after the last: extrapolate from last
                 (let ((prev-prev-point (nth (1- prev-point-pos) time-map)))
                   (+ (cadr prev-point)
                      (* (- time (car prev-point)) 
                         (/ (- (cadr prev-point) (cadr prev-prev-point)) (- (car prev-point) (car prev-prev-point)))))
                   )))))

    (* units-from-0 unit)
    ))

;;; used by cursors
(defmethod time-to-pixel ((self score-view) time) 
  (let ((ed (editor self)))
    (score-time-to-pixel self time 
                         (editor-get-edit-param ed :time-map)
                         (font-size-to-unit (editor-get-edit-param ed :font-size)))
    ))
                             

