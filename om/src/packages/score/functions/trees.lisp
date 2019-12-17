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
; TREE FUNCTIONS
; by K. Haddad, 2004
;============================================================================


(in-package :om) 


;==============================================================================
; MAKE TREES
;==============================================================================

;--------------------------
; PULSEMAKER
;--------------------------

(defmethod* pulsemaker ((measures list) (beat-unit list) (n-pulses list))
  :initvals (list '(4 4) '(8 8) '(4 (1 1 1 1)))
  :indoc '("measure numerator(s)" "measure denominator(s)" "contents (pulses/subdivisions)")
  :icon :tree
  :doc "
Constructs a tree starting from a (list of) measure(s) numerator(s) <measures-num>
and a (list of) denominator(s) <beat-unit> filling these measures with <npulses>.
"
  (let* ((pulses (mapcar 'cons measures
                         (mapcar #'(lambda (x) 
                                     (if (listp x) (list x) (list (om::repeat-n 1 x)))) n-pulses)))

         (mes (mapcar #'(lambda (x y) (list x y)) measures beat-unit))
         (tree (mapcar #'(lambda (x y) (list x (list y))) mes pulses)))
    (list '? tree)))

(defmethod* pulsemaker ((measures list) (beat-unit number) (n-pulses list))
  (let* ((lght (length measures))
        (bt-unt-lst (repeat-n beat-unit lght)))
    (pulsemaker measures bt-unt-lst n-pulses)))

(defmethod* pulsemaker ((measures number) (beat-unit list) (n-pulses list))
  (let* ((lght (length beat-unit))
        (measure-lst (repeat-n measures lght)))
    (pulsemaker measure-lst beat-unit n-pulses)))

(defmethod* pulsemaker ((measures number) (beat-unit number) (n-pulses list))
  (let* ((lght (length n-pulses))
        (bt-unt-lst (repeat-n beat-unit lght))
        (measure-lst (repeat-n measures lght)))
    (pulsemaker measure-lst bt-unt-lst n-pulses)))



;--------------------------
; MAKETREEGROUPS
;--------------------------

(defun grplst (lst grp)
  (let* ((grouped (loop for a in grp
                        collect (prog1
                                    (first-n lst  a)
                                  (setf lst (nthcdr a lst))))))
    (remove nil (if lst (x-append grouped (list lst)) grouped))))


(defmethod* maketreegroups ((figures list)
                            (sequence list)
                            (measures list))
  :initvals (list '((1 1 1) (1 2 1) (3 4) (1 1 1 1))
                  '(0 3 0 2 0 1 0 0 3) 
                  '((4 4)))
  :indoc '("rhythm figures" "control sequence" "list of time signatures")
  :icon :tree
  :doc "
Builds a Rhythm Tree starting from a a list of rhythmic figures (<figures>).

<sequence> is a list of index used to pick items in <figures>. 
An inconsistent index (> than the length of <figures>) will produce a rest. 

<measures> is a list of time signatures used to build the RT.
"
  (let* ((mesures
          (let* ((dernieremesure (last-elem measures))
                 (lgtseq (length sequence))
                 (lgtbeats (apply '+ (mapcar 'car measures))))
            (if (> lgtseq lgtbeats)
                (x-append measures (repeat-n dernieremesure
                                             (ceiling (/ (- lgtseq lgtbeats) 
                                                         (car dernieremesure)))))
              measures)))
         (num (mapcar 'car mesures))
         (denom (mapcar 'cadr mesures))
         (pos (posn-match figures sequence))
         (donnes (loop for i in pos
                       collect 
                       (if (null i) -1 (list 1 i))))
         (groupment (grplst donnes num)))
    (list '? (loop
              for i in num
              for j in denom
              for a in groupment
              collect 
              (if (< (length a) i)
                  (list (list i j) (x-append  a (* -1 (- i (length a)))))
                (list (list i j) a))))))

;==============================================================================
; INSPECT TREES
;==============================================================================

;--------------------------
; GROUP-PULSES / N-PULSES
;--------------------------

(defun real-copy-list (list)
  (loop for item in list 
       collect 
       (cond
        ((listp item) (real-copy-list item))
        (t item))))

(defun give-pulse (liste)
  (let* ((n (second liste)))
    (loop for item in n append
          (if (atom item) (list item)
              (give-pulse item)))))
              
(defun pulse (mesure) 
  (om::flat (mapcar #'(lambda (x) (give-pulse x)) mesure)))

(defun pulses (mesures)
  "retourne le nombre de pulses (pas les pauses) d'une RTM"
    (om::flat (mapcar #'(lambda (x) (pulse (list x))) mesures)))

(defun om-pulses (tlist)
  (mapcar #'(lambda (x) (pulse (list (cons '() (list x))))) tlist))

(defun find-po (list)
  (remove '()
          (mapcar #'(lambda (x y)
                      (if (floatp x) nil y ))
                  list (om::arithm-ser 0 (length list) 1))))
         
(defmethod! group-pulses ((tree list))
  
  :initvals '((? (((4 4) (1 (1 (1 2.0 1.0 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("a rhythm tree")
  :icon :tree
  :doc "
Collects every pulses (expressed durations, including tied notes) from <tree>. 
"
  (let* ((tree2 
          (second 
           (om::mat-trans 
            (om::flat-once 
             (om::mat-trans (rest (real-copy-list tree)))))))
         (the-pulses (om::flat (om-pulses tree2)))
         (the-pos (om::remove-dup
                   (om::flat 
                    (list 0 (find-po the-pulses) (length the-pulses)))
                   'eq 1)))
    
    (if (null (find-po the-pulses)) nil 
    
    (om::group-list the-pulses
                    (om::x->dx the-pos)
                    'linear))))

(defmethod! n-pulses ((tree t))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("a rhythm tree")
  :icon :tree
  :doc "
Returns the numbre of pulses in <tree>.
"
  (let (( liste (if (typep tree 'voice) (tree tree) tree)))
    (length
     (remove nil
             (mapcar #'(lambda(x) (if (> (first x) 0) x nil)) (om::group-pulses liste))))))


;--------------------------
; GET-SIGNATURES
;--------------------------

(defmethod* get-signatures ((tree list))
   :icon :tree
   :indoc '("a rhythm tree, voice or poly")
   :initvals (list  '(? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
   :doc "
Returns the list of time signatures in the rhythm tree (<tree>).
"
   (mapcar #'first (cadr tree)))

(defmethod* get-signatures  ((self voice))
  (get-signatures (tree self)))

(defmethod! get-signatures  ((self poly))
  (loop for v in (inside self) collect (get-signatures v)))


;--------------------------
; GET-PULSE-PLACES
;--------------------------

(defmethod* get-pulse-places ((tree list))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("a rhythm tree or voice")
  :icon :tree
  :doc "
Returns the positions of the pulses in <tree>.
"
  (let* ((res nil) 
         (n 0) 
         (puls (group-pulses tree)))

    (loop 
     for i in puls
     do (if (plusp (car i))
            (progn 
              (push n res)
              (incf n))
          (incf n)))

    (reverse res)))

(defmethod* get-pulse-places ((self voice))
  (get-pulse-places (tree self)))


;--------------------------
; GET-REST-PLACES
;--------------------------

(defmethod* get-rest-places ((tree list))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("a rhythm tree or voice")
  :icon :tree
  :doc "
Returns the positions of the rests in <tree>.
"
  (let* ((res nil)
         (n 0) 
         (puls (group-pulses tree)))
    
    (loop 
     for i in puls
     do (if (minusp (car i))
            (progn 
              (push n res)
              (incf n))
          (incf n)))
    
    (reverse res)))

(defmethod* get-rest-places ((self voice))
  (get-rest-places (tree self)))




;==============================================================================
; TRANSFORM TREES
;==============================================================================

;The structure TREEOBJ is defined in order to access easily the tree consecutives kind:
;plusp integers, minusp integers and floats (pulse, rests and tied notes)

(defstruct treeobj
  (tvalue 1)
  (tindex 0))

(defun trans-tree (tree)
"transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes."
  (if (atom tree)
      (make-treeobj :tvalue tree)
    (list (first tree) (mapcar 'trans-tree (second tree)))))

(defun trans-obj (tree)
  (if (atom tree)
    (if (typep tree 'treeobj) (treeobj-tvalue tree) tree)
    (list (first tree) (mapcar 'trans-obj (second tree)))))


;;; used in some recursive functions
(defvar *tree-index-count* -1)

(defun trans-tree-index (tree)
  "transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes and marks the index of events."
  (if (atom tree)
      (make-treeobj :tvalue tree :tindex (incf *tree-index-count*))
    (list (first tree) (mapcar 'trans-tree-index (second tree)))))

(defun trans-note-index (treeobjlist)
  "puts index only on expressed notes and not on floats or rests (for filtering purposes)."
  (if (atom treeobjlist)
      (if (and (typep treeobjlist 'treeobj) (integerp (treeobj-tvalue treeobjlist)) (plusp (treeobj-tvalue treeobjlist)))
          (setf (treeobj-tindex treeobjlist) (incf *tree-index-count*)) treeobjlist)
    (list (first treeobjlist) (mapcar #'trans-note-index (second treeobjlist)))))


;--------------------------
; REDUCETREE
;--------------------------

(defun grouper1 (liste)
"groups succesive floats"
  (if (null liste)
    liste
    (let* ((first (car liste))
           (rest (rest liste))
           )
      (if (numberp first)
        (if (plusp first)
          (cons (+ first (loop while (and (numberp (first rest)) (floatp (first rest)))
                               sum (round (pop rest))))
                (grouper1 rest))
          (cons first (grouper1 rest)))
        (cons (grouper1 first) (grouper1 rest))))))
                
                  
(defun grouper2  (liste)
"groups succesive rests (-1) into one"
  (if (null liste)
    liste
    (let* ((first (car liste))
           (rest (rest liste)))
      (if (numberp first)
        (if (plusp first) 
          (cons first (grouper2 rest))
          (cons (+ first (loop while (and (integerp (first rest)) (minusp (first rest)))
                               sum  (pop rest)))
                (grouper2 rest)))
        (cons (grouper2 first) (grouper2 rest))))))
 

(defun grouper3 (liste)
"reduces concatenated rests in the form of (1(-3)) into -1"
  (if (atom  liste)
    liste
    (if (and (numberp (first (second liste)))
             (minusp (first (second liste)))
             (null (rest (second liste)))
             (not (listp (first liste))))
      (- (first liste))
      (list (first liste)
            (mapcar 'grouper3 (second liste))))))


(defmethod reduced-tree ((tree list))
  (let ((liste (resolve-? tree)))
    (grouper3 (grouper2 (grouper1 liste)))))



(defmethod* reducetree ((tree list))
   :initvals '( (? (((4 4) (1 (1 (1 2.0 1.0 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 -1)))) )
   :indoc '("a rhythm tree")
   :icon :tree
   :doc "
Reduces and simplifies a tree by concatenating consecutive rests and floats.
"
   (let ((liste (reduced-tree tree)))
     (loop
       while (not (equal liste tree))
       do 
       (setf tree liste)
       (setf liste (reduced-tree liste)))
     (remove nil liste)))


;--------------------------
; TIETREE
;--------------------------

(defmethod* tietree ((tree t))
   :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
   :indoc '("a rhythm tree")
   :icon :tree
   :doc "
Converts all rests in <tree> (a rhytm tree, VOICE or POLY object) into ties (i.e. float values in the RT).
"
   (cond
    ((and (atom tree) (> tree 0)) tree)
    ((atom tree) (* (* tree -1) 1.0))
    (t (list (first tree)
             (mapcar 'tietree (second tree))))))

(defmethod* tietree ((self voice))  
  (make-instance 'voice 
                 :tree (tietree (tree self)) 
                 :tempo (if (atom (tempo self)) (tempo self) (cadar (tempo self)))
                 :chords (get-chords self)))

(defmethod* tietree ((self poly))
  (make-instance 
   'poly 
   :voices (loop for v in (inside self) 
                 collect (tietree v))))


;--------------------------
; FILTERTREE
;--------------------------
(defun transform-notes-flt (list places)
  (loop while list
        for courant =  (pop list)
        do (if (and (and (integerp (treeobj-tvalue courant)) (plusp (treeobj-tvalue courant))) 
                    (member (treeobj-tindex courant) places))
               (progn
                 (setf  (treeobj-tvalue courant) (- (treeobj-tvalue courant)))
                 (loop while (and list (floatp (treeobj-tvalue (car list)))) 
                       do (setf (treeobj-tvalue (car list)) (round (- (treeobj-tvalue (car list)))))
                       (pop list))))))


(defmethod* filtertree ((tree t) (places list))
   :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))) (0 1))
   :indoc '("a rhytm tree or voice" "a list of indices")
   :icon :tree
   :doc "
Replaces expressed notes in given positions from <places> with rests.
"
   (setf *tree-index-count* -1)
   (let* ((liste (if (typep tree 'voice) (tree tree) tree))
          (tree2obj (trans-tree liste)))
     
     (trans-note-index tree2obj)
     (transform-notes-flt (remove-if 'numberp (flat tree2obj)) places)
     
     (trans-obj tree2obj)))


;--------------------------
; REVERSETREE
;--------------------------

(defun group-ties (liste)
  "liste is a liste of treeobjs"
  (let* (res)
    (loop for i in liste
          do (cond 
              ((and (plusp (treeobj-tvalue i)) (not (floatp (treeobj-tvalue i))))
               (push (list i) res))
              ((floatp (treeobj-tvalue i))
               (push i (car res)))
              (t))
          )
    (loop for i in res
          do (if (> (length i) 1)
                 (progn 
                   (setf (treeobj-tvalue (first i)) (round (treeobj-tvalue (first i))))
                   (setf (treeobj-tvalue (last-elem i)) (* 1.0 (treeobj-tvalue (last-elem i)))))))))


(defun reversedties (tree)
  (let* ((liste (if (typep tree 'voice) (tree tree) (resolve-? tree)))
         (tree2obj (trans-tree liste)))
    
    (group-ties (remove-if 'numberp (flat tree2obj)))
    
    (trans-obj tree2obj)))


(defun reversedtree (tree)
  (if (atom tree)
      tree
    (list (first tree)
          (reverse (mapcar 'reversedtree (second tree))))))

(defmethod* reversetree ((tree t))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("a rhythm tree or voice")
  :icon :tree
  :doc "
Recursively reverses <tree>.
"
  (reversedtree (reversedties tree)))

(defmethod* reversetree ((self voice))
  (reversetree (tree self)))



;--------------------------
; ROTATETREE
;--------------------------

;;rotate-by measure la ne veut rien dire:
;;example qud on a une seconde mesure commencant par une liaison 
;;donc qui appartien t a la premiere mesure on a un probleme!!!

(defmethod rotatetreepulse ((tree t) (nth integer))
  "Returns a circular permutation of a copy of <tree> durations starting from its <nth> element"
  (let* ((ratios (tree2ratio tree))
         (signatures (mapcar 'car (cadr tree)))
         (rotation (rotate ratios nth)))
    (mktree rotation signatures)))

(defun get-all-treeobj (tree)
  (remove nil      
          (mapcar 
           #'(lambda (x) (if (typep x 'treeobj) x))
           (flat tree))))

(defun permtree (list nth)
  (let* ((listobj (rotate list nth))
         (vals (mapcar 'treeobj-tvalue listobj)))
    (loop 
     for i from 0 to (1- (length listobj))
     for a in vals
     collect (setf (treeobj-tvalue (nth i list)) a))))


(defmethod rotate-tree ((tree t) (nth integer))

  (setf *tree-index-count* -1)
  
  (let* ((tree2obj (trans-tree-index tree))
         (tree2objclean (get-all-treeobj tree2obj)))

    (permtree tree2objclean nth)
    
    (trans-obj tree2obj)))


(defmethod* rotatetree (tree n &optional (mode 'pulse))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))) 1 pulse)
  :indoc '("a rhythm tree" "a number" "rotation mode")
  :menuins '((2 (("pulse" 'pulse) 
                 ("prop" 'prop))))
  
  :icon :tree
  :doc " 
Applies a rotation of <n> positions to the pulses in <tree>.

<mode> = 'pulse' : applies to pulses (expressed durations).
<mode> = 'prop' : applies to the tree values.
"
  (if (equal mode 'pulse)
      (rotatetreepulse tree n)
    (rotate-tree tree n)))



;--------------------------
; REMOVE-RESTS
;--------------------------

(defun transform-rests (list)
"traces a simple list coming from trans-tree and flattened according to:
if note encoutered, then floats are checked and transformed into rests, else
rests encoutered and either other rests or errounous floats are transformed 
into notes. From Gerard."
    (loop while list
        for courant =  (pop list)
        do (if (and (integerp (treeobj-tvalue courant)) (minusp (treeobj-tvalue courant)))
             (progn
             (setf  (treeobj-tvalue courant) (- (treeobj-tvalue courant)))
             (loop while (and list (not (and (integerp (treeobj-tvalue (first list))) (plusp (treeobj-tvalue (first list))))))
                   do (setf (treeobj-tvalue (car list)) (float (abs (treeobj-tvalue (car list)))))
                   (pop list))))))



(defmethod* remove-rests ((tree t))
   :initvals '((2 (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))))
   :indoc '("a rhythm tree")
   :icon :tree
   :doc "
Converts all rests to notes.
"
   (let* ((liste (if (typep tree 'voice) (tree tree) tree))
          (tree2obj (trans-tree liste))
          (tree2objclean (remove-if 'numberp (flat tree2obj))))
     
     (transform-rests tree2objclean)
     
     (trans-obj tree2obj)))



;--------------------------
; SUBST-RHYTHM
;--------------------------

(defun substreeall (list pos elem)
  (loop for i from 0 to (length pos)
        for a in pos
        collect (if (listp (nth i elem)) 
                    (setf (treeobj-tvalue (nth a list)) (list (abs (floor (treeobj-tvalue (nth a list)))) (nth i elem)))
                  (setf (treeobj-tvalue (nth a list)) (nth i elem)))))
           

;pour eviter les tree avec '?'
;ATTENTION!! numtree transforme les 5  en (4 1.0) etc...
;ceci pourrait fausser les transformations
; toutefois il est utlise pour regler l'histoire de ?
(defun numtree (tree)
  (let* ((newvoice (make-instance 'voice :tree tree)))
    (tree newvoice)))


; This is for both trees from measures & VOICES

(defun optimize-tree (tree)
  "this function optimizes trees modified by rotate-tree and subst-rhythm methods
by grouping each measure as a group, in order to correctly read the new rhythm 
outputed."
  
  (if (or (equal '? (first tree)) (atom (first tree))) 
    
      (let* ((header (first tree))
             (info (second tree))
             (splitree (mat-trans info))
             (signatures (first splitree))
             (measures (second splitree))
             (opt-meas (mapcar #'(lambda (x) (list (list 1 x))) measures))
             (withmes (mapcar #'(lambda (x y) (list x y)) signatures opt-meas)))
        
        (list header withmes))
    
    (let* ((signatures (first tree))
           (measures (second tree))
           (opt-meas (list (list 1 measures))))
      
      (list signatures opt-meas))))


; In OM all 5 and 7 etc.. are transformed by voice and numtree function
; in (4 1.0) as tied notes. If we need to permut trees as expressed 
; i.e 5 and not (4 1.0) use reduce mode. (here we use reducetree function!)
; and finally remove this option and put it by default in reduce mode
; for accurate computation!

(defmethod* subst-rhythm ((tree t) 
                          (pos list)
                          (elem list)
                          &optional 
                          (option 'reduce)
                          (output 'optimized))
   :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))) nil (1 2) reduce optimized)
   :indoc '("a rhythm tree" "list of positions" "list of new items" "option" "output")
   :menuins '((3 (("reduce" 'reduce) 
                 ("tied" 'tied)))
              (4 (("optimized" 'optimized) 
                  ("not optimized" 'not-optimized))))
   :icon :tree
   :doc "
Substitutes elements in <tree>.
<elem> input is a list that can accept either atoms or lists  or both.
Atoms are pulses. Lists will be proportions
creating groups . For example a list (1 1 1) substituting 2 will yield (2 (1 1 1)).
<pos> if left nil will substitute from 0 position until the end of list <elem>. 
If the positions are specified (a list) each nth elem of tree being pulses will be replaced
sequentially by elements from <elem>."

   (setf *tree-index-count* -1)

   (let* ((liste (if (typep tree 'voice) 
                   (if (equal option 'reduce) (reducetree (tree tree)) (tree tree))
                   (if (equal option 'reduce) (reducetree (numtree tree)) (numtree tree))))
          (position (if (null pos)
                      (loop for i from 0 to (1- (length elem))
                            collect i) (first-n pos (length elem) )))
          (tree2obj (trans-tree-index liste))
          (tree2objclean (remove-if 'numberp (flat tree2obj))))
          
     (substreeall tree2objclean position elem)

     (case output
       (optimized (optimize-tree (trans-obj tree2obj)))
       (not-optimized (trans-obj tree2obj)))))




;--------------------------
; INVERT-RHYTHM
;--------------------------

(defun transform (list)
  "traces a simple list coming from trans-tree and flattened according to:
if note encoutered, then floats are checked and transformed into rests, else
rests encoutered and either other rests or errounous floats are transformed 
into notes. From Gerard."
  (loop while list
        for courant = (pop list)
        do (cond
            ((and (integerp (treeobj-tvalue courant)) (plusp (treeobj-tvalue courant)))
             (setf (treeobj-tvalue courant) (- (treeobj-tvalue courant)))
             (loop while (and list (floatp (treeobj-tvalue (car list))))
                   do (setf (treeobj-tvalue (car list)) (round (- (treeobj-tvalue (car list)))))
                   (pop list)))
            ((and (integerp (treeobj-tvalue courant)) (minusp (treeobj-tvalue courant)))
             (setf (treeobj-tvalue courant) (- (treeobj-tvalue courant)))
             (loop while (and list (not (and (integerp (treeobj-tvalue (first list))) (plusp (treeobj-tvalue (first list))))))
                   do (setf (treeobj-tvalue (car list)) (float (abs (treeobj-tvalue (car list)))))
                   (pop list))))))


(defmethod* invert-rhythm ((tree t))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("a rhythm tree")
  :icon :tree
  :doc "
Inverts <tree> : every note becomes a rest and every rest becomes a note.
"
  (let* ((liste (if (typep tree 'voice) (tree tree) (resolve-? tree)))
         (tree2obj (trans-tree liste))
         (tree2objclean (remove-if 'numberp (flat tree2obj))))

    (transform tree2objclean)
    
    (trans-obj tree2obj)))






       





