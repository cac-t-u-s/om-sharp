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


(defclass rhythmic-object (score-element) 
  ((tree :initform '(1 (1 1 1 1)) :accessor tree :initarg :tree :type list :documentation "a rhythm tree")
   (inside :accessor inside :initform nil :documentation "internal hierarchical structure")))

(defmethod additional-slots-to-copy ((self rhythmic-object))
  (append (call-next-method) '(inside)))

(defmethod deep-search ((self rhythmic-object) obj)
  (if (find obj (inside self)) t
      (let ((found-inside nil))
        (loop for sub in (inside self)
              while (not found-inside)
              do (setf found-inside (deep-search sub obj)))
        found-inside)))

(defmethod deep-search ((self t) obj) nil)
  
(defmethod (setf inside) ((newlist list) (self score-element))
  (loop for elt in newlist do (setf (parent elt) self))
  (setf (slot-value self 'inside) newlist))

;;;===================================================
;;; VOICE IS A CHORD-SEQ WHOSE STRUCTURE AND TIMING IS RULED BY A TREE
;;;===================================================
;;; tree (text) is used to build the r-struct 
;;; .. and set dates and durations to the chords
;;; r-struct is a hierarchical structure of containers (measures/groups) whose leaves are either chords or rests
;;; the chords in r-struct are simple references to the time-sequence items

(defclass* voice (chord-seq rhythmic-object)
  ((tree :initform '(1 (((4 4) (1 1 1 1)))) :accessor tree :initarg :tree :type list 
         :documentation "a rhythm tree (list of measure-rythm-trees)")
   (Lmidic :initform '((6000)) :initarg :Lmidic :initarg :chords :type list 
           :documentation "pitches (mc)/chords: list or list of lists")
   (tempo :accessor tempo :initform 60 :initarg :tempo :documentation "a tempo value or tempo-map")
   (inside :accessor inside :initform nil :documentation "internal hierarchical structure")
   ))

(defmethod additional-class-attributes ((self voice)) '(lvel loffset lchan lport extras))

(defmethod get-object-slots-for-undo ((self voice)) 
  (append (call-next-method) '(tree tempo)))

(defmethod restore-undoable-object-state ((self voice) (state list)) 
  (call-next-method)
  (build-voice-from-tree self)
  self)


(defmethod objFromObjs ((model chord-seq) (target voice))
  (let ((grouped-chord-seq (clone model)))
    (group-chords grouped-chord-seq)
    (make-instance 'voice
                   :tree (omquantify model 60 '(4 4) 8)
                   :lmidic (get-chords grouped-chord-seq) 
                   :tempo 60)))


;;; catch-up with default behaviour
;;; => in principle this is by default for exact same type
;(defmethod objFromObjs ((model voice) (target voice))
;  (clone-object model target))


(defmethod objFromObjs ((model voice) (target chord-seq))
  (clone-object model target))


;;; SOME ADDITIONAL CLASSES TO BUILD RHYTHMIC STRUCTURES:
(defclass measure (rhythmic-object) ()
  (:default-initargs :tree '((4 4) (-1))))

(defclass group (rhythmic-object) 
  ((numdenom :accessor numdenom :initarg :numdenom :initform nil)))

(defclass continuation-chord (score-element)
  ((previous-chord :accessor previous-chord :initarg :previous-chord 
                   :initform nil :documentation "the tied previous element")))

(defmethod get-real-chord ((self continuation-chord))
  (get-real-chord (previous-chord self)))

(defmethod get-real-chord ((self chord)) self)

(defclass r-rest (score-element) ())

(defclass grace-note (score-element) ())


;;; gets chords in the rhythmic structure
;;; !! different from GET-CHORDS
(defmethod get-all-chords ((self rhythmic-object))
  (loop for obj in (inside self) append 
        (get-all-chords obj)))


(defmethod get-all-chords ((self chord)) (list self))
(defmethod get-all-chords ((self continuation-chord)) (list self))
(defmethod get-all-chords ((self r-rest)) (list self))
(defmethod get-all-chords ((self t)) nil)

(defmethod get-notes ((self r-rest)) nil)
(defmethod get-notes ((self continuation-chord)) 
  (get-notes (get-real-chord self)))

(defmethod get-notes ((self group)) 
  (loop for element in (inside self) 
        append (get-notes element)))

(defmethod get-notes ((self t)) nil)

; (format-tree '(((5 3) (1 3 (4 (3 1 (2 (8 1)) 2))))))

(defmethod get-obj-dur ((self voice))
  (let ((last-element (last-elem (get-all-chords (last-elem (inside self))))))
    (if last-element
        (beat-to-time (+ (symbolic-date last-element) (symbolic-dur last-element))
                      (tempo self))
      0)))


(defmethod initialize-instance ((self voice) &rest initargs)
  
  (call-next-method)
  
  (when (list-subtypep (tree self) '(ratio number))
    ;;; probably a list of ratios
    (setf (tree self) (mktree (tree self) '(4 4))))
   
  ;;; compat OM 6 (temp)
  (when (listp (tempo self))  ;; e.g. ((1/4 60) ...)
    (setf (tempo self) (cadr (car (tempo self)))))
  
  ;(when (atom (car (tree self)))
  ;  ;;; probably "old-formatted" RT, with "?" etc.
  ;  (setf (tree self) (cadr (tree self))))
  (unless (numberp (car (tree self)))
    (if (atom (car (tree self))) ;;; => probably "?"
        (setf (tree self) (list (length (cadr (tree self)))
                                (cadr (tree self))))
      (setf (tree self) (list (length (tree self))
                              (tree self)))))


  
  (set-tree self (slot-value self 'tree))
  
  self)

(defmethod set-tree ((self voice) (tree list))
  (setf (slot-value self 'tree) (format-tree (normalize-tree tree)))
  (build-voice-from-tree self))

(defmethod build-voice-from-tree ((self voice))
  (build-rhythm-structure self (chords self) -1)
  (set-timing-from-tempo (chords self) (tempo self)))


(defmethod om-init-instance ((self voice) &optional args)
  (let ((tempo (find-value-in-kv-list args :tempo)))
    (when tempo (set-timing-from-tempo (chords self) tempo))
    self))

(defun beat-to-time (beat tempo)
  (let ((whole-dur (* 4 (/ 60000 tempo))))
    (round (* beat whole-dur))))
  
(defun set-timing-from-tempo (chords tempo)
  (loop for c in chords do
        (setf (date c) (beat-to-time (symbolic-date c) tempo))
        (setf (ldur c) (list (- (beat-to-time (+ (symbolic-date c) (symbolic-dur c) (symbolic-dur-extent c)) tempo)
                                (beat-to-time (symbolic-date c) tempo)
                                1)))
        ))


(defmethod build-rhythm-structure ((self voice) chords n &key last-chord)

  (let ((curr-beat 0)
        (curr-n-chord n)
        (curr-last-chord last-chord))
    
    (setf (inside self)
          (loop for m-tree in (cadr (tree self))
                collect (let* ((m-dur (decode-extent (car m-tree)))
                               (mesure (make-instance 'measure :tree m-tree
                                                      :symbolic-date curr-beat
                                                      :symbolic-dur m-dur
                                                      )))
                          (setq curr-beat (+ curr-beat m-dur))
                          (multiple-value-setq
                              (curr-n-chord curr-last-chord)
                              (build-rhythm-structure mesure chords curr-n-chord :last-chord curr-last-chord))
                          mesure)))
    
    ;;; cut the end of the chords list
    (set-chords self (first-n (chords self) (1+ curr-n-chord)))

    (values curr-n-chord curr-last-chord)))


;;; the duration of a tree
(defmethod tree-extent ((tree list)) 
  (decode-extent (car tree)))

;;; the duration of a leaf
(defmethod tree-extent ((tree number)) 
  (decode-extent tree))

;;; measure or group
(defmethod build-rhythm-structure ((self rhythmic-object) chords n &key last-chord)

  (let ((total-dur (apply '+ (mapcar 'tree-extent (cadr (tree self))))) ;;; sum of subdivisions
        (curr-beat (symbolic-date self))
        (s-dur (symbolic-dur self))
        (curr-n-chord n)
        (curr-last-chord nil)
        (grace-note-offset 100)
        (current-grace-notes 0))
    
    ;(print (list "build" self (symbolic-dur self)))

    (setf (inside self) 
          
          ;;; with grace notes (subtree = 0) we can collect a NIL
          (remove 
           nil
           (loop for subtree in (cadr (tree self))
                 collect 
                 (if (listp subtree) 
                     ;;; SUBGROUP
                     (let* ((relative-dur (/ (car subtree) total-dur))
                            (sub-dur (* s-dur relative-dur))
                            (tree (list sub-dur (simplify-subtrees (cadr subtree))))
                            (group (make-instance 'group :tree tree
                                                  :symbolic-date curr-beat
                                                  :symbolic-dur sub-dur
                                                  )))
                      
                       ;;; set the "numdenom" indicator
                       ;;; direct from OM6: probably possible to simplify
                              ; (print (list "group:" subtree "=>" (tree group) (symbolic-dur group)))

                       (let* ((group-ratio (get-group-ratio (tree group)))
                              (group-s-dur (symbolic-dur group))
                              (num (or group-ratio group-s-dur))
                              (dur-for-denom (if (= group-s-dur s-dur)
                                                 group-s-dur
                                               (/ group-s-dur s-dur)))
                              (denom (find-denom num dur-for-denom)))
                        
                         (when (listp denom) 
                           (setq num (car denom))
                           (setq denom (second denom)))
                        
                         (setf (numdenom group) (cond
                                                 ((not group-ratio) nil)
                                                 ((= (/ num denom) 1) nil)
                                                 (t (reduce-num-den num denom))))
                         )
                        
                       (setq curr-beat (+ curr-beat sub-dur))
                      
                       (multiple-value-setq
                           (curr-n-chord curr-last-chord)
                           (build-rhythm-structure group chords curr-n-chord :last-chord (or curr-last-chord last-chord)))

                       group)
                  
                   ;;; ATOM (leaf)
                   ;;; subtree is a NUMBER
                   (let ((sub-dur (* (symbolic-dur self) (/ (decode-extent subtree) total-dur))))
                    
                    ; (print (list "CHORD" curr-n-chord subtree))
                    
                     (let ((object
                            (cond 
                             ;;; REST
                             ((minusp subtree)  
                              (make-instance 'r-rest
                                             :symbolic-date curr-beat
                                             :symbolic-dur sub-dur
                                             ))
                          
                             ;;; CONTINUATION CHORD
                             ((and (floatp subtree) ;;; keep current-chord in chord-list (important: same reference!)
                                   (>= curr-n-chord 0)) ;;; just to prevent error when a continuation chord has no previous chord
                             
                              (let* ((real-chord (nth curr-n-chord chords))
                                     (cont-chord (make-instance 'continuation-chord)))
                               
                                (setf 
                                 ;;; extends the duration of the main chord
                                 (symbolic-dur-extent real-chord) (+ (symbolic-dur-extent real-chord) sub-dur) 
                                
                                 (previous-chord cont-chord) (or curr-last-chord last-chord)
                                 (symbolic-date cont-chord) curr-beat
                                 (symbolic-dur cont-chord) sub-dur)
                               
                                (setq curr-last-chord cont-chord)
                                cont-chord))

                             ;;; GRACE-NOTE 
                             ((zerop subtree)
                              ;;; grace note works only if hey are less than the number of notes in the chord +1 !
                              
                              (if curr-last-chord ;;; previous chord in this group 
                                
                                  ;;; post-grace notes: add them to this chord
                                  (loop with first-n = nil 
                                        for n in (reverse (notes curr-last-chord))
                                        while (not first-n) 
                                        do (if (plusp (offset n)) ;; there's alraedy an offset
                                               (setf (offset n) (+ (offset n) grace-note-offset))
                                             (progn 
                                               (when (minusp (offset n))
                                                 ;;; this was already a grace note !!
                                                 (om-beep-msg "Warning: too many (post) grace notes for chord!"))
                                               (setf (offset n) grace-note-offset)
                                               (setf first-n t)))
                                        finally (unless first-n
                                                  (om-beep-msg "Warning: too many grace notes for chord!")))
                                
                                ;;; pre-grace notes: store them until we get the chord 
                                (setf current-grace-notes (1+ current-grace-notes))
                                )
                                NIL ;;; don't collect a grace-note
                                )
                     
                             ;;; CHORD
                             (t ;;; get the next in chord list
                               
                                (when (and (floatp subtree) (< curr-n-chord 0))
                                  (om-print "Tied chord has no previous chord. Will be converted to a normal chord." "Warning"))
                               
                                (setq curr-n-chord (1+ curr-n-chord))
                        
                                (let ((real-chord (nth curr-n-chord chords)))

                                  (unless real-chord ;;; chord-list exhausted: repeat the last one as needed to finish the tree
                                    (setq real-chord (or (clone (car (last chords))) (make-instance 'chord :lmidic '(6000))))
                                    (setf (loffset real-chord) (list 0))
                                    (pushr real-chord chords))

                                  (setf (symbolic-date real-chord) curr-beat
                                        (symbolic-dur real-chord) sub-dur
                                        (symbolic-dur-extent real-chord) 0   ;;; could have been cloned from previous
                                        )

                                  ;;; add the "pre" grace-notes
                                  (when (plusp current-grace-notes) 
                                    (when (>= current-grace-notes (length (notes real-chord)))
                                      (om-beep-msg "Warning: too many (pre) grace notes for chord!"))
                                    (loop for n in (notes real-chord) 
                                          for i = current-grace-notes then (- i 1) while (> i 0) do
                                          (setf (offset n) (- (* i grace-note-offset)))))
                                  
                                 
                                  (setq curr-last-chord real-chord)
                                  real-chord))
                             )))
                      
                       ;;; udate curr-beat for the general loop
                       (setq curr-beat (+ curr-beat sub-dur))
                      
                       object)
                     ))
                 ))
          )
    
  (values curr-n-chord (or curr-last-chord last-chord))
  ))


;;;===============================================
;;; UTILS FOR "NUMDENOM" COMPUTATION
;;; direct from OM6
;;;===============================================


(defmethod get-group-ratio (tree)
  (let ((extent (car tree))
        (addition (loop for item in (second tree) sum (floor (abs (if (listp item) (car item) item))))))
     
    (cond
      ((= (round (abs addition)) 1) nil)
      ((integerp (/ extent addition)) addition)
      ;; never happens (?)
      ((and (integerp (/ extent addition)) 
            (or (power-of-two-p (/ extent addition))
                (and (integerp (/ addition extent)) 
                     (power-of-two-p (/ addition extent)))))
       nil)
      (t addition))))

(defun bin-value-below (num)
  (let ((cp2 (next-double-of-n num 2)))
    (if (= num cp2) num (/ cp2 2))))

(defun closest-double-of (num of)
  (let* ((high (next-double-of-n num of))
         (low (/ high 2)))
    (if (< (- high num) (- num low))
        high low)))

(defun find-beat-symbol (den) (bin-value-below den))

; Find the right denom to ratio of tuplet.
(defun find-denom (num durtot)
  (cond
   
   ((or (is-binaire? durtot)
        (power-of-two-p durtot))
    (get-denom-bin num))

   ((is-ternaire? durtot) 
    (get-denom-ter num))
   
   (t 
    (get-denom-other durtot num))))



;;; is (denominator dur) a power of 2
(defun is-binaire? (dur)
  (and (= (numerator dur) 1) 
       (= (denominator dur) (next-double-of-n (denominator dur) 2))   ;;; next-pwr-of-2, or closest ?
       ))

(defun is-ternaire? (durtot)
  (and (= 3 (numerator durtot))
       (is-binaire? (/ 1 (denominator durtot)))
       ))

(defmethod get-denom-bin (num)
  (case num
    (3 2)
    (4 4) (5 4) (6 4) (7 4) (8 8) (9 8) (10 8) (11 8) (12 8) (13 8) (14 8)
    (15 16) (16 16)
    (otherwise (closest-double-of num 2))))

(defmethod get-denom-ter (num)
  (case num
    (2 3) (3 3) (4 3)
    (5 6) (6 6) (7 6) (8 6) (9 6) 
    (10 12) (11 12) (12 12) (13 12) (14 12) (15 12) (16 12) (17 12)
    (otherwise (closest-double-of num 3))))


; if the answer is a list, the num should be changed in the caller-group
(defun get-denom-other (dur num)
  (let ((durtot (numerator dur)))
    (cond
     ((= (+ num 1) durtot) durtot)
     ((= num durtot) num)
     ((< num durtot)
      (list (* num 2) durtot))
     ;((< num (- (* 2 durtot) 1)) durtot)  ;; OJO OJO ESTOS CASOS HAY QUE VERLOS CON KARIM
     (t (closest-double-of num durtot))
     )))

;; returns the smallest multiples of 2 (e.g. 14/8 => 7/4)
(defun reduce-num-den (num den)
  (if (and (evenp num) (evenp den))
      (reduce-num-den (/ num 2) (/ den 2))
    (list num den)))



;;;===============================================
;;; BUILD TREE FREOM MEASURE
;;;===============================================

(defmethod build-tree ((self voice) dur)

  (declare (ignore dur))

  (list (length (inside self))
        (loop for m in (inside self)
              collect (build-tree m (car (tree m))))
        ))

(defmethod build-tree ((self rhythmic-object) dur)
  
  (list dur
        
        (let* ((proportions (mapcar #'symbolic-dur (inside self)))
               (pgcd (reduce #'pgcd (cons 1 proportions)))
               (simple-propotions (om/ proportions pgcd)))

          (loop for element in (inside self)
                for d in simple-propotions
                collect (build-tree element d))
          )
        ))

(defmethod build-tree ((self chord) dur) dur)
(defmethod build-tree ((self continuation-chord) dur) (float dur))
(defmethod build-tree ((self r-rest) dur) (- dur))
 

;;;===============================================
;;; FOR VOICE IT IS IMPORTANT TO KEEP A REFERENCE TO THE SAME CHORDS AS THEY ARE 
;;; CONTAINED IN BOTH THE TIME-SEQUENCE, AND THE RHYTMIC STRUCTURE
;;;===============================================
;;; continue if the lists are exhausted ??

(defmethod (setf Lmidic) ((Lmidic list) (self voice))
  (loop for midics in lmidic
        for c in (chords self)
        do (setf (lmidic c) midics)
        ))

(defmethod (setf Lvel) ((Lvel list) (self voice))
  (loop for vels in lvel
        for c in (chords self)
        do (setf (lvel c) vels)
        ))

(defmethod (setf Loffset) ((Loffset list) (self voice))
  (loop for offsets in Loffset
        for c in (chords self)
        do (setf (loffset c) offsets)
        ))

(defmethod (setf Lchan) ((Lchan list) (self voice))
  (loop for chans in Lchan
        for c in (chords self)
        do (setf (lchan c) chans)
        ))

(defmethod (setf LPort) ((LPort list) (self voice))
  (loop for ports in LPort
        for c in (chords self)
        do (setf (lport c) ports)
        ))

(defmethod (setf tree) ((tree list) (self voice))
  (set-tree self tree)
  )


;;;======================================
;;; EDITION
;;;======================================

;;; REMOVE A CHORD / REPLACE WITH A REST

(defmethod remove-from-obj ((self voice) (item chord)) 
  
  ;;; turn all continuation chords into rests
  (let ((same-cont-chords nil))
    (loop for c in (get-all-chords self)
          when (typep c 'continuation-chord)
          do 
          (when (or (equal (previous-chord c) item)
                    (find (previous-chord c) same-cont-chords))
            (push c same-cont-chords)))
    
    
    (loop for c in same-cont-chords
          do (let ((rest (clone-object c (make-instance 'r-rest))))
               (setf (inside (parent c))
                     (substitute rest c (inside (parent c))))
               ))
    )
  
  ;;; change the chord itself into a rest
  ;; (change-class item 'r-rest)
  (setf (inside (parent item))
        (substitute (clone-object item (make-instance 'r-rest)) 
                    item
                    (inside (parent item))))
  
  ;;; compute new tree
  (let ((new-tree (build-tree self nil)))
    ;;; remove the chord
    (time-sequence-remove-timed-item self item)
    ;;; rebuild the structure
    (set-tree self new-tree)
    ))


(defmethod remove-from-obj ((self voice) (item group)) 
  
  ;;; turn all continuation chords into rests
  (let ((subst-rest (make-instance 'r-rest 
                                   :symbolic-dur (symbolic-dur item)
                                   :symbolic-date (symbolic-date item)
                                   )))
    
    (loop for c in (get-tpl-elements-of-type item 'chord)
          do ;;; untied continuation chords and remove the chord
          (let ((cont-c (find c (get-tpl-elements-of-type self 'continuation-chord) :key #'previous-chord)))
            (when cont-c (untie-chord self cont-c))            
            (time-sequence-remove-timed-item self c)
            ))
    
    (replace-in-obj self item subst-rest)
    (set-tree self (build-tree self nil))
    ))


(defmethod remove-from-obj ((self voice) (item measure)) 
  
  (loop for c in (get-tpl-elements-of-type item 'chord)
        do ;;; untied continuation chords and remove the chord
        (let ((cont-c (find c (get-tpl-elements-of-type self 'continuation-chord) :key #'previous-chord)))
          (when cont-c (untie-chord self cont-c))
          (time-sequence-remove-timed-item self c)
          ))
  
  (setf (inside self) (remove item (inside self)))    
  (set-tree self (build-tree self nil))
  )






;;; SUBSTITUTIONS

(defmethod replace-in-obj ((self score-element) (old t) (new t)) nil)

;;; replace an element
(defmethod replace-in-obj ((self rhythmic-object) (old score-element) (new score-element))
  
  (if (find old (inside self)) 
      
      (setf (inside self) 
            (substitute new old (inside self)))
    
    (loop for sub in (inside self) do
          (replace-in-obj sub old new))
    ))


;;; replace a list by a single element (=grouping)
(defmethod replace-in-obj ((self rhythmic-object) (old list) (new score-element))
  (let ((pos (search old (inside self))))
    (if pos 
        
        (setf (inside self) 
              (append (subseq (inside self) 0 pos)
                      (list new)
                      (subseq (inside self) (+ pos (length old)))))

      (loop for sub in (inside self) do
            (replace-in-obj sub old new))
      )))

;;; replace a single element by a list (=ungrouping)
(defmethod replace-in-obj ((self rhythmic-object) (old score-element) (new list))
  (let ((pos (position old (inside self))))
    (if pos 
        
        (setf (inside self) 
              (append (subseq (inside self) 0 pos)
                      new
                      (subseq (inside self) (1+ pos))))

      (loop for sub in (inside self) do
            (replace-in-obj sub old new))
      )))


;;; replace a list by a list (=useful?)
(defmethod replace-in-obj ((self rhythmic-object) (old list) (new list))
  (let ((pos (search old (inside self))))
    (if pos 
        
        (setf (inside self) 
              (append (subseq (inside self) 0 pos)
                      new
                      (subseq (inside self) (+ pos (length old)))))

      (loop for sub in (inside self) do
            (replace-in-obj sub old new))
      )))




;;; TIE/UNTIE

;;; converts chord into continuation-chord
(defmethod tie-chord ((self voice) (c chord) &optional rebuild)
  
  (let ((pos (position c (chords self))))
    
    (if (= pos 0) ;; can not be tied to previous
        
        c
      
      (let ((new-c (make-instance 'continuation-chord 
                                  :previous-chord (nth (1- pos) (chords self))
                                  )))
        
        (setf (symbolic-date new-c) (symbolic-date c) 
              (symbolic-dur new-c) (symbolic-dur c))
      
        (replace-in-obj self c new-c)
        
        ; remove the chord (it's not a "real" chord anymore)
        (time-sequence-remove-timed-item self c)

        ; rebuild the structure
        (when rebuild 
          (set-tree self (build-tree self nil)))
        
        ;;; we are not able to return new-c as the rhytm-structure has been re-build anyway
        (unless rebuild new-c))
      )))


;;; converts continuation-chord into chord
(defmethod untie-chord ((self voice) (c continuation-chord) &optional rebuild)
  
  (let* ((ref-chord (get-real-chord c))
         (time-pos (beat-to-time (symbolic-date c) (tempo self)))
         (new-c (clone-object ref-chord)))
    
    (setf (onset new-c) time-pos
          (symbolic-date new-c) (symbolic-date c) 
          (symbolic-dur new-c) (symbolic-dur c)) 
    
    (replace-in-obj self c new-c)
    
    ; creates anew "real" chord in teh sequence
    (time-sequence-insert-timed-item-and-update self new-c (find-position-at-time self time-pos))

    ;;; rebuild the structure
    (when rebuild 
      (set-tree self (build-tree self nil)))
    
    new-c))


;;; does a list of ties/unties 
(defmethod tie-untie ((self voice) (clist list))
  
  (let* ((cont-chords (remove-if-not #'(lambda (c) (typep c 'continuation-chord)) clist))
         (chords (remove-if-not #'(lambda (c) (typep c 'chord)) clist)))

    (loop for cc in cont-chords do
          (untie-chord self cc nil))
    (loop for c in chords do
          (tie-chord self c nil))

    ;;; rebuild the structure
    (set-tree self (build-tree self nil))
    
    nil))



;;; GROUP

(defmethod group-objects ((self t) (in-voice voice)) nil)

(defmethod group-objects ((self rhythmic-object) (in-voice voice)) 

  (let* ((chords (get-tpl-elements-of-type self 'chord))
         (cont-chords (get-tpl-elements-of-type self 'continuation-chord))
         (new-obj (if chords (make-instance 'chord)
                    (if cont-chords (make-instance 'cont-chords :previous-chord (previous-chord (car cont-chords)))
                      (make-instance 'r-rest)))))
    
    (when chords 
      (setf (notes new-obj) (remove-duplicates (get-notes chords) :key #'midic))
      (loop for c in chords do
            (time-sequence-remove-timed-item in-voice c)))
    
    (setf (symbolic-date new-obj) (symbolic-date self)
          (symbolic-dur new-obj) (symbolic-dur self))
    
    (when (typep new-obj 'chord)
      (time-sequence-insert-timed-item-and-update 
       in-voice new-obj 
       (find-position-at-time in-voice (beat-to-time (symbolic-date new-obj) (tempo in-voice)))))
    
    new-obj))


(defmethod group-objects ((self list) (in-voice voice)) 

  (let* ((chords (loop for obj in self append (get-tpl-elements-of-type obj 'chord)))
         (cont-chords (loop for obj in self append (get-tpl-elements-of-type obj 'continuation-chord)))
         (new-obj (if chords (make-instance 'chord)
                    (if cont-chords (make-instance 'continuation-chord :previous-chord (previous-chord (car cont-chords)))
                      (make-instance 'r-rest)))))
    (when chords 
      (setf (notes new-obj) (remove-duplicates (get-notes chords) :key #'midic))
      (loop for c in chords do
            (time-sequence-remove-timed-item in-voice c)))
    
    (setf (symbolic-date new-obj) (list-min (mapcar #'symbolic-date self))
          (symbolic-dur new-obj) (apply #'+ (mapcar #'symbolic-dur self)))
    
    (when (typep new-obj 'chord)
      (time-sequence-insert-timed-item-and-update 
       in-voice new-obj 
       (find-position-at-time in-voice (beat-to-time (symbolic-date new-obj) (tempo in-voice)))))
    
    new-obj))


(defmethod group-voice-elements ((self t) (clist list) (tpl-sequence voice)) nil)

(defmethod group-voice-elements ((self rhythmic-object) (clist list) (tpl-sequence voice))
  
  (setf clist (sort clist #'< :key #'symbolic-date))
  
  ;(print (list "GROUPING IN" self))
  
  ;; else: check inside
  (let* ((sublist (intersection clist (inside self)))
         (pos (and sublist (search sublist (inside self)))))
    
    (if pos  ;;; same sequence in same order exists inside: a sequence of succesive elements
        
        ;;; make a new object with sub-list
        (let ((grouped (group-objects sublist tpl-sequence)))
          ;(print (list "list to group:" sublist "=>" grouped))
          ;;; substitute the object
          (replace-in-obj self sublist grouped))
      
      ;;; go one by one
      (loop for element in (inside self) do
            
            (if (and sublist (find element sublist))
                
                ;;; group the element
                (let ((grouped (group-objects element tpl-sequence)))
                  ;(print (list "element to group:" element "=>" grouped))
                  (when grouped
                    (replace-in-obj self element grouped)))
              
              ;;; check for groups inside 
              (group-voice-elements element clist tpl-sequence)
              ))
      ))
  )
  
  
(defmethod group ((self voice) (clist list))
  (group-voice-elements self clist self)
  ;;; rebuild the structure
  (set-tree self (build-tree self nil)))


;;; SUBDIVIDE

(defmethod subdivide-in-voice ((self chord) (n integer) (in-voice voice))
  
  (let* ((new-dur (/ (symbolic-dur self) n))
         (t0 (symbolic-date self))
         (new-chords (loop for i from 0 to (1- n) collect
                           (let ((c (clone-object self)))
                             (setf (symbolic-date c) (+ t0 (* i new-dur))
                                   (symbolic-dur c) new-dur)
                             c)))
         (new-group (make-instance 'group :symbolic-date t0
                                   :symbolic-dur (symbolic-dur self))))
      
      (setf (inside new-group) new-chords)
      
      (replace-in-obj in-voice self new-group)
      
      (time-sequence-remove-timed-item in-voice self)
      
      (loop for c in new-chords do
            (time-sequence-insert-timed-item-and-update 
             in-voice c 
             (find-position-at-time in-voice (beat-to-time (symbolic-date c) (tempo in-voice)))))
      ))
    

(defmethod subdivide-in-voice ((self r-rest) (n integer) (in-voice voice))
  
  (let* ((new-dur (/ (symbolic-dur self) n))
         (t0 (symbolic-date self))
         (new-rests (loop for i from 0 to (1- n) collect
                           (let ((r (clone-object self)))
                             (setf (symbolic-date r) (+ t0 (* i new-dur))
                                   (symbolic-dur r) new-dur)
                             r))))
    
    (replace-in-obj in-voice self new-rests)
      
    ))
    

(defmethod subdivide-in-voice ((self continuation-chord) (n integer) (in-voice voice))
  
  (let* ((new-dur (/ (symbolic-dur self) n))
         (t0 (symbolic-date self))
         (new-c-chords (loop for i from 0 to (1- n) collect
                           (let ((cc (clone-object self)))
                             (setf (symbolic-date cc) (+ t0 (* i new-dur))
                                   (symbolic-dur cc) new-dur)
                             cc))))
    
    (replace-in-obj in-voice self new-c-chords)
      
    ))


                           
(defmethod subdivide ((self voice) (clist list) (n integer))

  (let ((elements (loop for obj in clist 
                        append (get-tpl-elements-of-type obj '(chord continuation-chord r-rest)))))
    
    (loop for elt in elements do
          (subdivide-in-voice elt n self))
    
    (set-tree self (build-tree self nil))))


;;; BREAK

(defmethod break-group-in-voice ((self group) (in-voice voice))
  ;(loop for elt in (inside self) do
  ;      (setf (symbolic-dur elt) (* (symbolic-dur self) (symbolic-dur elt))))  
  (replace-in-obj in-voice self (inside self))
  )

(defmethod break-groups ((self voice) (glist list))
  
  (loop for group in glist do
        (break-group-in-voice group self))
  
  ;;; rebuild the structure
  (set-tree self (build-tree self nil)))



    

