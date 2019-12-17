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
; Code from K.Haddad, O. Sandred and others
;============================================================================



;============================================================================
; TREE2RATIO
;============================================================================

; util: the main function is tree2ratio
(defun tree-to-ratios (tree)
  (loop for mesure in (cadr tree) collect
        (let* ((signature (car mesure))
               (values (cadr mesure))
               (ratios (mesure-ratios values)))
          (om/
           (om* ratios (car signature))
           (cadr signature)))))

(defun mesure-ratios (list)
  (let ((div (round (loop for elt in list sum (abs (if (listp elt) (car elt) elt))))))
    (flat (loop for elt in list 
                collect (if (listp elt)
                            (om* (/ (round (car elt)) div) (mesure-ratios (cadr elt)))
                          (/ (round elt) div)))
          )))

;;;-------------------------
(defun get-s-from-meas (tree)
  (if (atom tree)
      tree
    (mapcar 'get-s-from-meas (second tree))))

(defun get-pulses (tree)
  (loop for elt in (cadr tree)
        collect (flat (get-s-from-meas elt))))

;;;-------------------------
(om::defmethod! tree2ratio ((tree t))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("a rythm tree")
  :icon 'tree
  :doc "
Converts <tree> into a list of ratio where 
1/4 is a quarter note, 1/8 is an eight note etc.
"
  (let* ((res ())
         (tree-formatted (if (listp (car tree))
                             (list (length tree) tree)
                             tree))
         (pulses (flat (get-pulses tree-formatted)))
         (ratios (flat (tree-to-ratios tree-formatted))))
    
    (loop for p in pulses
          do (if (floatp p)
                 (progn 
                   (setf (car res) (+ (car res) (car ratios)))
                   (pop ratios))
               (progn 
                 (push (car ratios) res)
                 (pop ratios))))
    
    (reverse res)))



;============================================================================
; TREE2RATIO
;============================================================================
;;; by O. Sandred

;offset from 1 always!!! Because otherwise the first pause ( -0!) will dissapear.
(defun x-dx-pause-ok (x-list)
  (mapcar '*
          (mapcar #'(lambda (absdur) (if (> absdur 0) 1 -1)) x-list)
          (om::x->dx (mapcar 'abs x-list))))

(defun dx-x-pause-ok (starttime x-list)
  (mapcar '*
          (append (mapcar #'(lambda (absdur) (if (> absdur 0) 1 -1)) x-list) '(1))
          (om::dx->x starttime (mapcar 'abs x-list))))


;;;-----------------------

(defun build-local-times (global-onsets global-start)
       (mapcar #'(lambda (onset) (if (> onset 0)
                                   (- onset (1- global-start))
                                   (+ onset (1- global-start))))
               global-onsets))

;;;-----------------------

(defun make-proportional-cell (dur-list)
  (mapcar 
   #'(lambda (dur) (* dur (apply 'lcm (mapcar 'denominator dur-list))))
   dur-list))


#|
;;;-----------------------
;;; NOT USED ANYMORE... since?

; redefinition for non-interger values
(defun simplify-proportions (proportion-list)
  (let* ((list (mapcar  'round proportion-list))
         (gcdl (apply 'gcd list)))
    (mapcar #'(lambda (value) (/ value gcdl)) list)))


; all negative numbers following eachother should be fused to single negative numbers
(defun fuse-pauses (proportional-list)
  (let ((pointer-to-list 0))
    (simplify-proportions
     (loop until (>= pointer-to-list (length proportional-list))
           collect (if (< (nth pointer-to-list proportional-list) 0)
                     (let ((this-pause (nth pointer-to-list proportional-list)))
                       (incf pointer-to-list)
                       (loop until (or (>= pointer-to-list (length proportional-list))
                                       (> (nth pointer-to-list proportional-list) 0))
                             do (progn
                                  (incf this-pause (nth pointer-to-list proportional-list))
                                  (incf pointer-to-list)))
                       this-pause)
                     (progn (incf pointer-to-list)
                            (nth (1- pointer-to-list) proportional-list)))))))

;;;-----------------------
|#


(defun make-sub-tree (local-onset)
  (list 1 ; (fuse-pauses
        (cond ((and (or (= (car local-onset) 1) (= (car local-onset) -1)))
               (make-proportional-cell (x-dx-pause-ok local-onset)))
              (t
               (let ((proportional-list
                      (make-proportional-cell (x-dx-pause-ok (cons 1 local-onset)))))
                 (cons (float (first proportional-list))
                       (cdr proportional-list))))
              )))


; In this function it is possible to define more notations for special cases.
(defun better-predefined-subdiv? (sub-tree)
  (let* ((proportional-list (cadr sub-tree))
         (pauses (mapcar #'(lambda (value) (if (< value 0) -1 1)) proportional-list))
         (abs-proportional-list (mapcar #'abs proportional-list))
         (abs-answer
          (cond ((equal abs-proportional-list '(2 2 2 3 3))
                 (list (list 2 (list (first pauses)(second pauses)(third pauses)))(fourth pauses)(fifth pauses)))
                ((equal abs-proportional-list '(3 3 2 2 2))
                 (list (first pauses)(second pauses)(list 2 (list (third pauses)(fourth pauses)(fifth pauses)))))
                ((equal abs-proportional-list '(3 2 2 2 3))
                 (list (first pauses)(list 2 (list (second pauses)(third pauses)(fourth pauses)))(fifth pauses)))
                ((equal abs-proportional-list '(3.0 2 2 2 3))
                 (list (coerce (first pauses) 'float) (list 2 (list (second pauses)(third pauses)(fourth pauses)))(fifth pauses)))
                ((equal abs-proportional-list '(3 3 4 2))
                 (list (first pauses)(second pauses)(list 2 (list (* 2 (third pauses))(fourth pauses)))))
                ((equal abs-proportional-list '(4 2 3 3))
                 (list (list 2 (list (* 2 (first pauses))(second pauses)))(third pauses)(fourth pauses)))
                ((equal abs-proportional-list '(2 4 3 3))
                 (list (list 2 (list (first pauses)(* 2 (second pauses))))(third pauses)(fourth pauses)))
                ((equal abs-proportional-list '(3 3 2 4))
                 (list (first pauses)(second pauses)(list 2 (list (third pauses)(* 2 (fourth pauses))))))
                ((equal abs-proportional-list '(3 1 1 1))
                 (list (first pauses)(list 1 (list (second pauses)(third pauses)(fourth pauses)))))
                ((equal abs-proportional-list '(3.0 1 1 1))
                 (list (coerce (first pauses) 'float)(list 1 (list (second pauses)(third pauses)(fourth pauses)))))
                ((equal abs-proportional-list '(1 1 1 3))
                 (list (list 1 (list (first pauses)(second pauses)(third pauses)))(fourth pauses)))
                (t proportional-list))))
    (list 1 abs-answer)))


(defun create-beat (global-onset global-start beat-length)
  (let ((local-onset (build-local-times global-onset global-start))
        tree)
    (if (not local-onset) (setf local-onset (list (1+ beat-length))))
    (if (= (car (last local-onset)) (- -1 beat-length))
      (setf local-onset (append (butlast local-onset) (list (1+ beat-length)))))
    (if (/= (car (last local-onset)) (1+ beat-length))
      (setf local-onset (append local-onset (list (1+ beat-length)))))
    (setf tree (better-predefined-subdiv? (make-sub-tree local-onset)))
    (if (= (length (cadr tree)) 1)
      (caadr tree)
      tree)))


(defun fuse-pauses-and-tied-notes-between-beats (measure-tree no-of-beats)
  (let ((beat-nr 0))
    (loop until (>=  beat-nr no-of-beats)
          collect (cond ((and (typep (nth beat-nr measure-tree) 'number)
                              (< (nth beat-nr measure-tree) 0))
                         (let ((value (nth beat-nr measure-tree)))
                           (incf beat-nr)
                           (loop until (or (typep (nth beat-nr measure-tree) 'list)
                                           (>=  beat-nr no-of-beats)
                                           (and (typep (nth beat-nr measure-tree) 'integer)
                                                (> (nth beat-nr measure-tree) 0)))
                                 do (progn (decf value (truncate (abs (nth beat-nr measure-tree))))
                                           (incf beat-nr)))
                           value))
                        ((and (typep (nth beat-nr measure-tree) 'number))
                         (let ((value (nth beat-nr measure-tree)))
                           (incf beat-nr)
                           (loop until (not (typep (nth beat-nr measure-tree) 'float))
                                 do (progn (incf value (truncate (nth beat-nr measure-tree)))
                                           (incf beat-nr)))
                           value))
                        (t (incf beat-nr)
                           (nth (1- beat-nr) measure-tree))))
    ))

(defun build-one-measure (local-onset no-of-beats beat-length)
  (let ((beatlist (om::dx->x 1 (make-list no-of-beats :initial-element beat-length)))
        tree)
    (setf tree
          (fuse-pauses-and-tied-notes-between-beats
           (loop for beat-nr from 0 to (- (length beatlist) 2)
                 collect (let ((these-events (filter-events-between (nth beat-nr beatlist)
                                                                    (nth (1+ beat-nr) beatlist)
                                                                    local-onset)))
                           ;check if tied pause within subtree - if yes: give startpoint as pause
                           (if (and these-events
                                    (/= (abs (first these-events)) (nth beat-nr beatlist))
                                    (get-onsettime-before (nth beat-nr beatlist) local-onset)
                                    (> 0 (get-onsettime-before (nth beat-nr beatlist) local-onset)))
                             (setf these-events (append (list (- 0 (nth beat-nr beatlist)))
                                                        these-events))) 
                           ;check if tied pause within subtree - if yes: give startpoint as pause
                           (if (and (not these-events)
                                    (get-onsettime-before (nth beat-nr beatlist) local-onset)
                                    (> 0 (get-onsettime-before (nth beat-nr beatlist) local-onset)))
                             (setf these-events (list (- 0 (nth beat-nr beatlist)))))
                           
                           (create-beat these-events (nth beat-nr beatlist) beat-length)))
           no-of-beats))
    (list (list no-of-beats (/ 1 beat-length)) tree)))


(defun filter-events-between (start stop onsettimes)
  (let ((no-low-values (member start onsettimes :test #'(lambda (item value) (<= item (abs value))))))
    (reverse (member stop (reverse no-low-values) :test #'(lambda (item value) (>= item (abs value)))))))


(defun get-onsettime-before (timepoint abs-rhythm)
  (car (member timepoint (reverse abs-rhythm) :test #'(lambda (item value) (> item (abs value))))))


(defun buildmeasure-seq (abs-rhythms timesigns)
  (let ((measure-start-points (om::dx->x 1 (mapcar #'(lambda (timesign) (apply '/ timesign)) timesigns))))
    (loop for measure from 0 to (1- (length timesigns))
          collect (let ((this-seq (filter-events-between (nth measure measure-start-points)
                                                         (nth (1+ measure) measure-start-points)
                                                         abs-rhythms))
                        (this-timesign (nth measure timesigns))
                        local-onset)
                    ;check if measure starts with tied pause
                    (if (and this-seq
                             (/= (abs (first this-seq)) (nth measure measure-start-points))
                             (> 0 (get-onsettime-before (nth measure measure-start-points) abs-rhythms)))
                      (setf this-seq (append (list (- 0 (nth measure measure-start-points)))
                                             this-seq)))
                    (if (and (not this-seq) 
                             (> 0 (get-onsettime-before (nth measure measure-start-points) abs-rhythms)))
                      (setf this-seq (list (- 0 (nth measure measure-start-points)))))
                    (setf local-onset (build-local-times this-seq (nth measure measure-start-points)))
                    
                    (build-one-measure local-onset 
                                       (car this-timesign)
                                       (/ 1 (cadr this-timesign)))))))


(defun simple->tree (rhythmseq timesignseq)
  (let ((abs-rhythms (dx-x-pause-ok 1 (append rhythmseq '(-100)))))
    (list '? (buildmeasure-seq abs-rhythms timesignseq))))

;;;-----------------------

(defmethod! mktree ((rhythm list) (timesigns list)) 
  :initvals '((1/4 1/4 1/4 1/4) (4 4))
  :indoc '("list of integer ratios" "list of time signatures")
  :doc "
Builds a hierarchical rhythm tree from a simple list of note values (<rhythm>).
1/4 is the quarter note.

<timesigns> is a list of time signatures, e.g. ( (4 4) (3 4) (5 8) ... )
If a single time signature is given (e.g. (4 4)), it is extended as much as required
by the 'rhythm' length.

The output rhythm tree is intended for the <tree> input of a 'voice' factory box.
"
  :icon 'tree
  
  (if (typep (car timesigns) 'list)
      (simple->tree rhythm timesigns)
    (let* ((nbmesreal (* (/ (loop for item in rhythm sum (abs item))
                            (car timesigns))
                         (cadr timesigns)))
           (nbmes (if (integerp nbmesreal) nbmesreal (1+ (truncate nbmesreal)))))
        (simple->tree rhythm (make-list nbmes :initial-element timesigns)))
  ))

