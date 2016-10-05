;Calendario.lisp / March 2011
;by Marc Chemillier

(in-package :om)

;WARNING:
;You need to load library ImproteK (with file Hermeto.lsip)
;It contains a variable 'Calendario-do-Som'


;DISPLAY A TRANSITION TREE OF THE CHORD PROGRESSIONS FROM THE 'CALENDRIO DO SOM'
;-------------------------------------------------------------------------------

#|  
Evaluate the following expressions and look at the Listener window:

(DeltaRoot 'c 'd)
(DeltaRoot 'd 'c)
(DeltaRoot 'c 'f#)

(setf l (list (make-instance 'jazzchord :root 'c :quality '(- 4 7 9))
              (make-instance 'jazzchord :root 'd :quality '(6 7+ 9))
              (make-instance 'jazzchord :root 'e :quality '(- 4 7 9))
              (make-instance 'jazzchord :root 'f :quality '(- 4 7 9))
              (make-instance 'jazzchord :root 'c :quality '(6 7+ 9))))

(make-transition l)

(print-transition (sort-transition (make-transition l)))

(print-transition (sort-transition (make-transition (transition-calendario Calendario-do-Som))))

;The result should look like this, thus giving a kind of 'transition tree' of the tunes from Calendario:
'CALENDRIO DO SOM' HARMONIC TRANSITION TREE
============================================
23-junho-1997 + 02-dezembro-1996 + 23-outubro-1996 

transitions from (- 4 7 9) = 57 times
---> to (- 4 7 9) with intervals:  2 [8] 1 [3] 3 [2] 4 [2] -3 [2] -2 [2] -1 [2] -5 [1] -4 [1] 
---> to (7 9+ 13-) with intervals:  -1 [14] 
---> to (7+) with intervals:  3 [2] -1 [2] 2 [1] 6 [1] -5 [1] -4 [1] 
---> to (6 7+ 9) with intervals:  1 [2] -4 [2] -2 [1] -1 [1] 
---> to (6 7 9) with intervals:  5 [3] -4 [1] 
---> to (6 7 9 11+) with intervals:  5 [1] 
---> to (- 5- 9) with intervals:  -3 [1] 
etc.

|#

(defclass* jazzchord ()
  (
   (root :initform 'c :initarg :root :accessor root)
   (quality :initform '(- 4 7 9) :initarg :quality :accessor quality)
   (nbeats :initform 4 :initarg :nbeats :accessor nbeats)

))

(defun transition-calendario (liste)
  (format *om-stream* "~%'CALENDRIO DO SOM' HARMONIC TRANSITION TREE~%============================================~%")  
  (loop for x in liste for i = 1 then (1+ i) do (format *om-stream* "~a " (first x))
        when (< i (length liste)) do (format *om-stream* "+ ")
        when (and (< i (length liste)) (= (mod i 5) 0)) do (format *om-stream* "~%"))
   (loop for x in liste
         append (loop for y in (rest x) 
                      collect (make-instance 'jazzchord :root (first y) :quality (second y) :nbeats (third y)))))

(defun make-transition (liste-ch)
  (let ((res (make-hash-table :test 'equal)))
    (loop for x in liste-ch
          for y in (cdr liste-ch)
          do (progn (when (null (gethash (quality x) res)) (setf (gethash (quality x) res) (make-hash-table :test 'equal)))
               (let ((hash-x (gethash (quality x) res)))
                 (when (null (gethash (quality y) hash-x)) 
                   (setf (gethash (quality y) hash-x) (make-hash-table :test 'equal)))
                 (let* ((hash-y (gethash (quality y) hash-x))
                        (i (DeltaRoot (root x) (root y)))
                        (nb (gethash i hash-y)))
                   (setf (gethash i hash-y) 
                         (if (null nb) 1 (incf nb))))))
          finally return res)))
  
(defun sort-transition (h-table)
  (sort (loop for x being the hash-key of h-table using (hash-value q) 
              collect (let ((tmp0 (sort (loop for y being the hash-key of q using (hash-value r) 
                                              collect (let ((tmp (sort (loop for z being the hash-key of r using (hash-value s) 
                                                                             collect (list z s))
                                                                       #'> :key #'cadr)))
                                                        (cons y (cons (apply '+ (mapcar 'cadr tmp)) tmp))))
                                        #'> :key #'cadr)))
                        (cons x (cons (apply '+ (mapcar 'cadr tmp0)) tmp0))))
        #'> :key #'cadr))

(defun print-transition (transition-tree)
  (loop for x in transition-tree
        do (progn (format *om-stream* "~%~%transitions from ~a = ~a times" (first x) (second x))
             (loop for y in (cddr x)
                   do (progn (format *om-stream* "~%---> to ~a with intervals:  " (first y))
                        (loop for z in (cddr y) do (format *om-stream* "~a [~a] " (first z) (second z)))))))
  (format *om-stream* "~%~%"))

(defmacro DeltaRoot (root1 root2)
  `(let ((r1 (case ,root1 (db 'c#) (d# 'eb) (gb 'f#) (ab 'g#) (a# 'bb) (t ,root1)))
         (r2 (case ,root2 (db 'c#) (d# 'eb) (gb 'f#) (ab 'g#) (a# 'bb) (t ,root2))))
     (- (mod (+ 5 (- (length (member r1 '(c c# d eb e f f# g g# a bb b)))
                     (length (member r2 '(c c# d eb e f f# g g# a bb b))))) 12)
        5)))




;VOICE LEADING AFTER DMITRI TYMOCZKO (Barbados, February 2011)
;-------------------------------------------------------------

(defun TransposePCList (pclist int) (mapcar #'(lambda (pc) (mod (+ pc int) 12)) pclist))
(defun PCNote (pc) (nth pc '(c c# d eb e f f# g g# a bb b c c# d eb e f f# g g# a bb b)))

(defun display-three-best-transpo (acc1 acc2)
  (let* ((all-transpo (find-min-transpositions acc1 acc2)) 
         (first-three (extract-npairs 3 (sort (copy-list all-transpo) '<= :key 'car))))
    (loop for pair in all-transpo
          do (let* ((min (first pair)) (i (second pair)) (permute-acc2 (choose-permute (TransposePCList acc2 i))))
               (format *om-stream* "--------------------~%Transposition i = ~a -> minimal distance = ~a~%" (- (mod (+ i 5) 12) 5) min)
               (loop for x in permute-acc2
                     when (and (member pair first-three) (= (distance-sum acc1 x) min) (crossing-free x))
                     do (format *om-stream* "~a->~a~%" (mapcar 'PCNote acc1) (mapcar 'PCNote x)))))))

;Find best voice leadings between acc1 and various permutations of the transpositions of acc2:
(defun find-min-transpositions (acc1 acc2) 
  (loop for i from 0 to 11 collect (list (min-voice-leading acc1 (TransposePCList acc2 i)) i)))

(defun extract-npairs (n pair-list)
  (loop for x in pair-list with lastval = 0 with k = 3 when (> (first x) lastval) do (setf lastval (first x) k (1- k)) while (> k 0) collect x))
   
;Compute minimal voice leadings between acc1 and all permutations of acc2:
(defun min-voice-leading (acc1 acc2) (loop for x in (choose-permute acc2) minimize (distance-sum acc1 x)))

(defun distance-sum (acc1 acc2) (loop for u in acc1 for v in acc2 sum (min (abs (- u v)) (- 12 (abs (- u v))))))

(defun all-permute (list)
  (if (null (cdr list)) (list list) (loop for l in (all-permute (cdr list)) append (insert-elt (car list) l))))

(defun choose-prefered (liste)
  (let ((hermeto-table (make-hash-table :test 'equal)))
    (loop for i from 0 to 11 
          do (setf (gethash (TransposePCList '(0 2 7 11) i) hermeto-table) 
                   (loop for x in (list '(0 2 7 11) '(7 11 0 2)) collect (TransposePCList x i))))
    (or (gethash liste hermeto-table) (all-permute liste))))

(defun choose-permute (liste) (all-permute liste))
;(defun choose-permute (liste) (choose-prefered liste))

(defun insert-elt (x list)
  (append (loop for l = list then (cdr l) until (null l) with pref = nil
                collect (append (reverse pref) (list x) l)
                do (push (car l) pref))
          (list (append list (list x)))))

(defun crossing-free (acc)
  (let ((acc-8va (cons (car acc) (loop for u in acc for v in (cdr acc) with bool = t
                                       when (< v u) do (setf bool nil)
                                       if bool collect v else collect (+ v 12)))))
    (loop for u in acc-8va for v in (cdr acc-8va) never (< v u))))

#|

(choose-prefered '(0 2 7 11))
(all-permute '(7 9 2 6))
(extract-npairs 3 '((0 a) (1 d) (1 e) (2 d) (3 c) (4 z) (4 z) (5 q)))

(loop for x in '(1 2 3 4 5 6) for k = 1 when (= x 5) do (setf k (1- k)) while (> k 0) do (print x))

(TransposePCList '(1 2 3) 5)
(sort '((1 a) (2 b) (1 c)) '>= :key 'car)

(distance-sum '(4 7 9 2 6) '(4 7 9 2 6))
(permute '(4 7 9 2 6))
(min-voice-leading '(4 7 9 2 6) '(4 7 9 2 6))
(min-voice-leading '(4 7 9 2 6) (TransposePCList '(4 7 9 2 6) 10))
(find-min-transpositions '(4 7 9 2 6) '(4 7 9 2 6))
(extract-npairs 3 (sort (find-best-transpositions '(4 7 9 2 6) '(4 7 9 2 6)) '<= :key 'car))
(display-three-best-transpo  '(4 7 9 2 6) '(4 7 9 2 6))


(crossing-free '(2 11 9 0 7))
(crossing-free '(2 9 0))






PERFECT MAJOR TRIADS (T-SYMMETRY)

(display-three-best-transpo '(0 4 7) '(0 4 7))
--------------------
Transposition i = 0 -> minimal distance = 0
(c e g)->(c e g)
--------------------
Transposition i = 1 -> minimal distance = 3
(c e g)->(c# f g#)
--------------------
Transposition i = 2 -> minimal distance = 6
--------------------
Transposition i = 3 -> minimal distance = 3
(c e g)->(bb eb g)
--------------------
Transposition i = 4 -> minimal distance = 2
(c e g)->(b e g#)
--------------------
Transposition i = 5 -> minimal distance = 3       ;;;;;;;;;;; Dmitri, Chapter 6, Figure 6.3.8
(c e g)->(c f a)
--------------------
Transposition i = 6 -> minimal distance = 6
--------------------
Transposition i = -5 -> minimal distance = 3
(c e g)->(b d g)
--------------------
Transposition i = -4 -> minimal distance = 2
(c e g)->(c eb g#)
--------------------
Transposition i = -3 -> minimal distance = 3
(c e g)->(c# e a)
--------------------
Transposition i = -2 -> minimal distance = 6
--------------------
Transposition i = -1 -> minimal distance = 3
(c e g)->(b eb f#)


Dmitri, Chapter 6, 12 numbered voice leadings on Figure 6.3.1, all but one takes the shortest path along the triadic circle
The exception is:

(let* ((acc1 '(2 5 9))
       (acc2 '(4 8 11))
       (permute-acc2 (permute acc2)))
  (loop for x in permute-acc2
        do (format *om-stream* "~a->~a -> distance = ~a~%" (mapcar 'PCNote acc1) (mapcar 'PCNote x) (distance-sum acc1 x))))

(d f a)->(e g# b) -> distance = 7   ;;;minimal distance = 7, but most efficient in the triad circle
(d f a)->(g# e b) -> distance = 9
(d f a)->(g# b e) -> distance = 17
(d f a)->(e b g#) -> distance = 9
(d f a)->(b e g#) -> distance = 5   ;;;used by Josquin, minimal distance = 5, but less efficient in the triad circle
(d f a)->(b g# e) -> distance = 11



CHROMATIC SETS (P-SYMMETRY)

(display-three-best-transpo '(0 1 2) '(0 1 2))
--------------------
Transposition i = 0 -> minimal distance = 0
(c c# d)->(c c# d)
--------------------
Transposition i = 1 -> minimal distance = 3
(c c# d)->(c# d eb)
(c c# d)->(d c# eb)
(c c# d)->(c# eb d)
(c c# d)->(eb c# d)
--------------------
Transposition i = 2 -> minimal distance = 6
(c c# d)->(d eb e)
(c c# d)->(eb d e)
(c c# d)->(eb e d)
(c c# d)->(d e eb)
(c c# d)->(e d eb)
--------------------
Transposition i = 3 -> minimal distance = 9
--------------------
Transposition i = 4 -> minimal distance = 12
--------------------
Transposition i = 5 -> minimal distance = 13
--------------------
Transposition i = 6 -> minimal distance = 14
--------------------
Transposition i = -5 -> minimal distance = 13
--------------------
Transposition i = -4 -> minimal distance = 12
--------------------
Transposition i = -3 -> minimal distance = 9
--------------------
Transposition i = -2 -> minimal distance = 6
(c c# d)->(bb b c)
(c c# d)->(b c bb)
(c c# d)->(bb c b)
(c c# d)->(c bb b)
(c c# d)->(c b bb)
--------------------
Transposition i = -1 -> minimal distance = 3
(c c# d)->(b c c#)
(c c# d)->(c b c#)
(c c# d)->(c c# b)


DOMINANT-SEVENTH AND TRITONE SUBSTITUTION

(display-three-best-transpo '(0 3 6 8) '(0 3 6 8))
--------------------
Transposition i = 0 -> minimal distance = 0
(c eb f# g#)->(c eb f# g#)
--------------------
Transposition i = 1 -> minimal distance = 4
(c eb f# g#)->(c# e g a)
--------------------
Transposition i = 2 -> minimal distance = 4
(c eb f# g#)->(bb d f g#)
--------------------
Transposition i = 3 -> minimal distance = 2
(c eb f# g#)->(b eb f# a)
--------------------
Transposition i = 4 -> minimal distance = 4
(c eb f# g#)->(c e g bb)
--------------------
Transposition i = 5 -> minimal distance = 4
(c eb f# g#)->(b c# f g#)
--------------------
Transposition i = 6 -> minimal distance = 2
(c eb f# g#)->(c d f# a)
--------------------
Transposition i = -5 -> minimal distance = 4
(c eb f# g#)->(c# eb g bb)
--------------------
Transposition i = -4 -> minimal distance = 4
(c eb f# g#)->(b d e g#)
--------------------
Transposition i = -3 -> minimal distance = 2     ;;;;;;;;;Dmitri, Supporting Online Material (Materials and Methods), p. 22
(c eb f# g#)->(c eb f a)
--------------------
Transposition i = -2 -> minimal distance = 4
(c eb f# g#)->(c# e f# bb)
--------------------
Transposition i = -1 -> minimal distance = 4
(c eb f# g#)->(b d f g)


(display-three-best-transpo '(2 7 5 11) '(2 7 5 11))
--------------------
Transposition i = 0 -> minimal distance = 0
(d g f b)->(d g f b)
--------------------
Transposition i = 1 -> minimal distance = 4
--------------------
Transposition i = 2 -> minimal distance = 4
(d g f b)->(c# g e a)
--------------------
Transposition i = 3 -> minimal distance = 2
(d g f b)->(d g# f bb)
--------------------
Transposition i = 4 -> minimal distance = 4
(d g f b)->(eb a f# b)
--------------------
Transposition i = 5 -> minimal distance = 4
(d g f b)->(c g e bb)
--------------------
Transposition i = 6 -> minimal distance = 2   ;;;Dmitri; tritone substitution, Chapter 10, Figure 10.3.6
(d g f b)->(c# g# f b)
--------------------
Transposition i = -5 -> minimal distance = 4
--------------------
Transposition i = -4 -> minimal distance = 4
(d g f b)->(c# g eb bb)
--------------------
Transposition i = -3 -> minimal distance = 2
(d g f b)->(d g# e b)
--------------------
Transposition i = -2 -> minimal distance = 4
--------------------
Transposition i = -1 -> minimal distance = 4
(d g f b)->(c# f# e bb)



MINOR-SEVENTH CHORDS

(display-three-best-transpo '(0 3 5 8) '(0 3 5 8))
--------------------
Transposition i = 0 -> minimal distance = 0
(c eb f g#)->(c eb f g#)
--------------------
Transposition i = 1 -> minimal distance = 4
(c eb f g#)->(c# e f# a)
--------------------
Transposition i = 2 -> minimal distance = 4
(c eb f g#)->(bb d f g)
--------------------
Transposition i = 3 -> minimal distance = 2
(c eb f g#)->(b eb f# g#)
--------------------
Transposition i = 4 -> minimal distance = 4
(c eb f g#)->(c e g a)
--------------------
Transposition i = 5 -> minimal distance = 4
(c eb f g#)->(bb c# f g#)
--------------------
Transposition i = 6 -> minimal distance = 4
(c eb f g#)->(b d f# a)
--------------------
Transposition i = -5 -> minimal distance = 4
(c eb f g#)->(c eb g bb)
--------------------
Transposition i = -4 -> minimal distance = 4
(c eb f g#)->(b c# e g#)
--------------------
Transposition i = -3 -> minimal distance = 2    ;;;;;;;;;Dmitri, Supporting Online Material (Materials and Methods), p. 22
(c eb f g#)->(c d f a)
--------------------
Transposition i = -2 -> minimal distance = 4
(c eb f g#)->(c# eb f# bb)
--------------------
Transposition i = -1 -> minimal distance = 4
(c eb f g#)->(b d e g)




EXAMPLES FROM HERMETO'S CALENDARIO
----------------------------------

transitions from (- 4 7 9) = 57 times
---> to (- 4 7 9) with intervalles:  2 [8] 1 [3] 3 [2] 4 [2] -3 [2] -2 [2] -1 [2] -5 [1] -4 [1] 
---> to (7 9+ 13-) with intervalles:  -1 [14] 

transitions from (7+) = 43 times
---> to (- 4 7 9) with intervals:  -3 [8] -1 [5] 0 [1] 1 [1] 6 [1] -4 [1] -2 [1] 
---> to (7+) with intervals:  3 [5] 5 [3] -4 [3] -1 [3] 2 [1] 4 [1] -2 [1] 

transitions from (7 9+ 13-) = 32 times
---> to (7+) with intervals:  5 [5] -1 [5] 1 [1] -3 [1] 
---> to (- 4 7 9) with intervals:  1 [4] -3 [2] -1 [2] 


Voicings preferes d'Hermeto pour (- 4 7 9)
E + (7 9 2 6)
E + (2 6 7 9) 

AU LIEU DE CHERCHER LES INTERVALLES DE BASSE POUR LESQUELS ON A UNE DISTANCE MIN PAR RAPPORT A L'UNE DES PERMUT
-> CHERCHER CEUX POUR LESQUELS ON A UNE DISTANCE MIN PAR RAPPORT A L'UN DES VOICINGS PREFERES D'HEMETO!!!!!!!!!!!!!!!
===> VOIR AVEC JOVINO LES "VOICINGS PREFERES"

INCLUDING BASS NOTE:

(display-three-best-transpo '(4 7 9 2 6) '(4 7 9 2 6))
--------------------
Transposition i = 0 -> minimal distance = 0
(e g a d f#)->(e g a d f#) 
--------------------
Transposition i = 1 -> minimal distance = 5
(e g a d f#)->(f g# bb eb g) 
(e g a d f#)->(f g bb eb g#)
--------------------
Transposition i = 2 -> minimal distance = 4       ;;;;Hermeto [7]
(e g a d f#)->(e g# a b f#);;;;;;;;;;;;;;;;;;;;;bass on top
--------------------
Transposition i = 3 -> minimal distance = 7
--------------------
Transposition i = 4 -> minimal distance = 8
--------------------
Transposition i = 5 -> minimal distance = 9
--------------------
Transposition i = 6 -> minimal distance = 10
--------------------
Transposition i = -5 -> minimal distance = 9
--------------------
Transposition i = -4 -> minimal distance = 8
--------------------
Transposition i = -3 -> minimal distance = 7
--------------------
Transposition i = -2 -> minimal distance = 4
(e g a d f#)->(e g c d f), distance=4
--------------------
Transposition i = -1 -> minimal distance = 5
(e g a d f#)->(eb f# g# c# f), distance=5
(e g a d f#)->(eb f g# c# f#), distance=5




(display-three-best-transpo '(4 7 9 2 6) '(4 8 0 2 7))
--------------------
Transposition i = 0 -> minimal distance = 5
--------------------
Transposition i = 1 -> minimal distance = 4
(e g a d f#)->(eb g# a c# f)
--------------------
Transposition i = 2 -> minimal distance = 3;;;;MINIMAL: but not used by Hermeto
(e g a d f#)->(e a bb d f#)
--------------------
Transposition i = 3 -> minimal distance = 6 
--------------------
Transposition i = 4 -> minimal distance = 5
--------------------
Transposition i = 5 -> minimal distance = 6       ;;;;Hermeto [1]
--------------------
Transposition i = 6 -> minimal distance = 5
--------------------
Transposition i = -5 -> minimal distance = 6
--------------------
Transposition i = -4 -> minimal distance = 7
--------------------
Transposition i = -3 -> minimal distance = 6
--------------------
Transposition i = -2 -> minimal distance = 7
--------------------
Transposition i = -1 -> minimal distance = 4      ;;;;Hermeto [11]
(e g a d f#)->(eb g b c# f#)




JUST UPPER VOICES WITHOUT BASS:

(display-three-best-transpo '(7 9 2 6) '(7 9 2 6))
--------------------
Transposition i = 0 -> minimal distance = 0
(g a d f#)->(g a d f#)
--------------------
Transposition i = 1 -> minimal distance = 4       ;;;;Hermeto [2]
(g a d f#)->(g# bb eb g)
(g a d f#)->(g bb eb g#)
--------------------
Transposition i = 2 -> minimal distance = 6       ;;;;Hermeto [7]
(g a d f#)->(g# a b e)
--------------------
Transposition i = 3 -> minimal distance = 6       ;;;;Hermeto [2]
(g a d f#)->(a bb c f)
--------------------
Transposition i = 4 -> minimal distance = 6       ;;;;Hermeto [2]
(g a d f#)->(bb b c# f#)
--------------------
Transposition i = 5 -> minimal distance = 8
--------------------
Transposition i = 6 -> minimal distance = 8
--------------------
Transposition i = -5 -> minimal distance = 8       ;;;;Hermeto [2]
--------------------
Transposition i = -4 -> minimal distance = 6       ;;;;Hermeto [2]
(g a d f#)->(f bb d eb)
(g a d f#)->(eb bb d f)
--------------------
Transposition i = -3 -> minimal distance = 6       ;;;;Hermeto [2]
(g a d f#)->(f# b eb e)
(g a d f#)->(e b eb f#)
--------------------
Transposition i = -2 -> minimal distance = 6      ;;;;Hermeto [1]
(g a d f#)->(g c e f)
--------------------
Transposition i = -1 -> minimal distance = 4      ;;;;Hermeto [2]
(g a d f#)->(f# g# c# f)
(g a d f#)->(f g# c# f#)



(display-three-best-transpo '(7 9 2 6) '(8 0 2 7))
--------------------
Transposition i = 0 -> minimal distance = 5
(g a d f#)->(g# c d g)
(g a d f#)->(g c d g#)
--------------------
Transposition i = 1 -> minimal distance = 5
(g a d f#)->(g# a c# eb)
--------------------
Transposition i = 2 -> minimal distance = 5
(g a d f#)->(a bb d e)
--------------------
Transposition i = 3 -> minimal distance = 7
--------------------
Transposition i = 4 -> minimal distance = 7
--------------------
Transposition i = 5 -> minimal distance = 5       ;;;;Hermeto [1]    -> ASCENDING 4TH
(g a d f#)->(g c c# f)
--------------------
Transposition i = 6 -> minimal distance = 5
(g a d f#)->(g# c# d f#)
--------------------
Transposition i = -5 -> minimal distance = 3
(g a d f#)->(g a d eb)
--------------------
Transposition i = -4 -> minimal distance = 5
(g a d f#)->(g# bb eb e)
--------------------
Transposition i = -3 -> minimal distance = 7
--------------------
Transposition i = -2 -> minimal distance = 5
(g a d f#)->(f# bb c f)
(g a d f#)->(f bb c f#)
--------------------
Transposition i = -1 -> minimal distance = 3       ;;;;Hermeto [11]    -> DESCENDING HALF STEP
(g a d f#)->(g b c# f#)



(display-three-best-transpo '(8 11 3 6) '(7 9 2 6))
--------------------
Transposition i = 0 -> minimal distance = 4       ;;;;Hermeto [1]
(g# b eb f#)->(g a d f#)
--------------------
Transposition i = 1 -> minimal distance = 2       ;;;MINIMAL: mais seulement Hermeto [1]
(g# b eb f#)->(g# bb eb g)
--------------------
Transposition i = 2 -> minimal distance = 4
(g# b eb f#)->(a b e g#)
(g# b eb f#)->(g# b e a)
--------------------
Transposition i = 3 -> minimal distance = 6
--------------------
Transposition i = 4 -> minimal distance = 4
(g# b eb f#)->(bb b c# f#)
--------------------
Transposition i = 5 -> minimal distance = 6
--------------------
Transposition i = 6 -> minimal distance = 6       ;;;;Hermeto [1]
--------------------
Transposition i = -5 -> minimal distance = 6
--------------------
Transposition i = -4 -> minimal distance = 6       ;;;;Hermeto [1]
--------------------
Transposition i = -3 -> minimal distance = 4       ;;;;Hermeto [8]
(g# b eb f#)->(f# b eb e)
(g# b eb f#)->(e b eb f#)
--------------------
Transposition i = -2 -> minimal distance = 4       ;;;;Hermeto [1]
(g# b eb f#)->(g c e f)
--------------------
Transposition i = -1 -> minimal distance = 4       ;;;;Hermeto [5]
(g# b eb f#)->(g# c# f f#)




|#

