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

;; before is the space needed before a timed-symbol (e.g. for let accidentals etc.)
;; after is the space needed after (depends also on the duration symbol)
;; extra is some independent, extra time needed (typically for measures bars and time signature)
(defstruct space-point (onset) (before) (after) (extra))




;;;=====================================
;;; Rebuilding time-map
;;;=====================================
     
;;; at creating the display-cache (e.g. when the object is reevaluated)
(defmethod get-cache-display-for-draw  ((object voice) (box OMBoxEditCall))
  (when (equal object (get-box-value box))
    ;;; ... don't do if the object of the box is for instance a poly 
    ;;; otherwise the wrong time-map will be applied to the poly
    (set-edit-param box :time-map (build-time-map object))) 
  (call-next-method)
  t)

(defmethod get-cache-display-for-draw  ((object poly) (box OMBoxEditCall))
  (set-edit-param box :time-map (build-time-map object))
  (call-next-method)
  t)



(defmethod is-rhythmic ((obj t)) nil)
(defmethod is-rhythmic ((obj voice)) t)
(defmethod is-rhythmic ((obj poly)) t)

;;; Main function to build time-map
(defmethod build-time-map (object)
  
  ;; (print (list "build time-map for" object))

  (when (is-rhythmic object)
    
    ;; (om-print-format "Rebuild time-map for ~A" (list object))
  
    (let* ((time-space (sort (build-object-time-space object nil) ;;; no tempo: this object must be either VOICE or POLY high-level
                             #'< :key #'space-point-onset))
           (merged-list ()))

      ;;; build-object-time-space returns a list of (onset (space_nedded_before space_needed_after))
      ;;; objects with same onsets must be grouped, and space maximized
      ;;; negative space (e.g. from measures) will affect the space of previous item 
    
      (loop for item in time-space
            do (if (and (car merged-list) 
                        (= (space-point-onset item) (space-point-onset (car merged-list))))  ;;; already something there
                 
                   (setf (space-point-before (car merged-list))                         
                         (max (space-point-before (car merged-list)) (space-point-before item))

                         (space-point-after (car merged-list))  
                         (max (space-point-after (car merged-list)) (space-point-after item))

                         (space-point-extra (car merged-list))  
                         (max (space-point-extra (car merged-list)) (space-point-extra item))
                         ;;; onlty one item at a time adds extra space, otherwise it increases, 
                         ;;; e.g. when several voices have bars at the same time
                         ; (+ (space-point-extra (car merged-list)) (space-point-extra item))
                         )

               
                 ;;; new point in the time-map
                 (push item merged-list)))
      
      (when merged-list
        (setf merged-list (reverse merged-list))
     
         (cons 
     
         (list (space-point-onset (car merged-list))
               (+ (space-point-before (car merged-list))
                  (space-point-extra (car merged-list))))
      
         (loop with curr-x = (+ (space-point-before (car merged-list))
                                (space-point-extra (car merged-list)))
               for rest on merged-list
               while (cdr rest)
               do (setf curr-x (+ curr-x 
                                  (space-point-after (car rest)) 
                                  (space-point-before (cadr rest))
                                  (space-point-extra (cadr rest))))
               collect (list (space-point-onset (cadr rest)) curr-x))
         ))
        )))
                              

(defmethod build-object-time-space ((self t) tempo) 
  (declare (ignore tempo))
  nil)

(defmethod build-object-time-space ((self voice) tempo)
  (declare (ignore tempo)) ;;; voice itself holds the tempo
  
  (cons 
   ;;; for the beginning of the score...
   (make-space-point :onset -1000
                     :before 0 :after 1
                     :extra 0)
    
    (loop with prev-signature = nil 
        for m in (inside self)
        for position from 1
        append (build-measure-time-space m (tempo self) 
                                         position
                                         (not (equal (car (tree m)) prev-signature)))
        do (setf prev-signature (car (tree m))))))


(defmethod build-object-time-space ((self poly) tempo)
  (declare (ignore tempo)) ;;; voice holds the tempo
  (loop for voice in (inside self) append
        (build-object-time-space voice nil)))


;;; measure requires special arguments
(defmethod build-measure-time-space ((self measure) tempo position with-signature)
  (cons 
   (make-space-point :onset  (beat-to-time (symbolic-date self) tempo)
                     :before 0 :after 0
                     :extra (+ (if (= position 1) 0 6) ;; no bar for first measure 
                               (if with-signature 4 0) ;; space if signature
                               ))
   (loop for sub in (inside self) append  
         (build-object-time-space sub tempo))
   ))


;;;                         
;;; data for specific objects:
;;; this is for a group
(defmethod build-object-time-space ((self group) tempo)
 (let ((space (object-space-in-units self)))
   (cons 
    (make-space-point :onset (beat-to-time (symbolic-date self) tempo)
                      :before (first space) :after (second space)
                      :extra (or (third space) 0))
    (loop for sub in (inside self) append  
          (build-object-time-space sub tempo))
    )
   ))



;;; TERMINAL OBJECTS IN TIME-SPACE (no deeper inside)
;;; Units required (before after extra)
(defmethod object-space-in-units ((self t)) '(0 0 0))

(defmethod object-space-in-units ((self chord)) 
  (let ((notes (notes self)))
    (list
     ;;; space in units before:
     (+ 2 (* .5 (length (notes self)))
        ;;; number of notes with accident
        (* .8 (count-if #'(lambda (n) (pitch-to-acc (midic n))) notes)))
     ;;; space in units after:
     (+ (* .5 (length notes)) 
        (* 10 (symbolic-dur self))))))

(defmethod object-space-in-units ((self continuation-chord)) 
  (object-space-in-units (previous-chord self)))

(defmethod object-space-in-units ((self r-rest)) 
  (list 2 (+ 1 (* 10 (symbolic-dur self)))))

(defmethod build-object-time-space ((self chord) tempo)
  (let ((space (object-space-in-units self))
        (time (beat-to-time (symbolic-date self) tempo)))
    (list (make-space-point :onset time
                              :before (first space) :after (second space)
                              :extra (or (third space) 0)))
    ))

(defmethod build-object-time-space ((self continuation-chord) tempo)
  (let ((space (object-space-in-units self)))
    (list (make-space-point :onset  (beat-to-time (symbolic-date self) tempo)
                            :before (first space) :after (second space)
                            :extra (or (third space) 0)))
    ))

(defmethod build-object-time-space ((self r-rest) tempo)
   (let ((space (object-space-in-units self)))
     (list (make-space-point :onset (beat-to-time (symbolic-date self) tempo)
                       :before (first space) :after (second space)
                       :extra (or (third space) 0)))))



;;; Time-function:
;;; returns the position ins score units of a give time
(defmethod score-time-to-units (time-map time)
  
  (unless time-map (setf time-map '((0 0))))
  
  (if time-map
     
      (if (listp time)
         
          ;;; a convention when we're asking for the position of measure elements
          (let* 
              ((point-pos (or (position (car time) time-map :test #'= :key #'car)
                              (progn
                                (om-print-dbg "Error in score construction: measure doesn't start on a beat-time (t=~A)" (list (car time)))
                                (or (position (car time) time-map :test #'<= :key #'car)
                                    (1- (length time-map))))))
               (point (nth point-pos time-map))
               (prev-point (nth (max 0 (1- point-pos)) time-map)))
            (/ (+ (cadr prev-point) (cadr point)) 2.0))
           
        (let* 
            ((prev-point-pos (or (position time time-map :test #'>= :key #'car :from-end t) 0))
             (prev-point (nth prev-point-pos time-map))
             (next-point (nth (1+ prev-point-pos) time-map)))
         
          (cond 
        
           (;;; the point was in the list
            (= time (car prev-point)) 
            (cadr prev-point))
        
           (;;; interpolate between prev and next
            next-point 
            (+ (cadr prev-point)
               (* (- time (car prev-point)) 
                  (/ (- (cadr next-point) (cadr prev-point)) (- (car next-point) (car prev-point)))))
            )
        
           ((= prev-point-pos 0)  ;; only 1 point
            (cadr prev-point)
            )
        
           (t ;;; we are after the last: extrapolate from last
              (let ((prev-prev-point (nth (1- prev-point-pos) time-map)))
                (+ (cadr prev-point)
                   (* (- time (car prev-point)) 
                      (/ (- (cadr prev-point) (cadr prev-prev-point)) (- (car prev-point) (car prev-prev-point)))))
                )))
          ))
   
    (progn 
      (om-print "Error in score display: no time-map!")
      0))
  )



(defmethod score-units-to-time (time-map pos)
  
 (if time-map
  
     (let* 
      ((prev-point-pos (or (position pos time-map :test #'>= :key #'cadr :from-end t) 0))
       (prev-point (nth prev-point-pos time-map))
       (next-point (nth (1+ prev-point-pos) time-map)))
         
       (cond 
        
        (;;; the point was in the list
         (= pos (car prev-point)) 
         (car prev-point))
        
        (;;; interpolate between prev and next
         next-point 
         (+ (car prev-point)
            (* (- pos (cadr prev-point)) 
               (/ (- (car next-point) (car prev-point)) (- (cadr next-point) (cadr prev-point)))))
         )
        
        ((= prev-point-pos 0)  ;; only 1 point
         (car prev-point)
         )

        (t ;;; we are after the last: extrapolate from last
           (let ((prev-prev-point (nth (1- prev-point-pos) time-map)))
             (+ (car prev-point)
                (* (- pos (cadr prev-point)) 
                   (/ (- (car prev-point) (car prev-prev-point)) (- (cadr prev-point) (cadr prev-prev-point)))))
             )))
       )
   
   (progn 
     (om-print "Error time-mapping: no time-map!")
     0))
 )


