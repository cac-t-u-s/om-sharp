;Substitution.lisp
;by Marc Chemillier, last revised May 2013
;From  M.J. Steedman, "A Generative Grammar for Jazz Chord Sequences", Music Perception, vol. 2, no1, 1984, 52-77. 
(in-package :om)


(setf multibeatgrid '((b m7 1) (e 7 1) (bb m7 1) (eb 7 1) (ab m7 2) (c# 7 2) (f# maj7 2) (ab 7 2) (b 7 2) (bb 7 2)))

#|
;Play different voicings on the same chord progression 'multibeatgrid':

(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; basic voicings        
(play (setf harmo (beats->chseq (PlayVoicings basicvoicings multibeatgrid 500) 500 0))))

(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; voicings from Hermeto Pascoal            
(play (setf harmo (beats->chseq (PlayVoicings hermetovoicings multibeatgrid 500) 500 0))))

(save-as-midi-with-tempo harmo 500)

(progn (pgmout 4 1) (ctrlchg 7 120 1)               
(setf harmo (beats->chseq (PlayVoicings hermetovoicings (GridLabel (Rewrite bluesgrammar (InitMultiplebeatGrid multibeatgrid) 2)) 
                                        500) 500 0))    ; voicings from Hermeto Pascoal
(play harmo))
(Stop-Player *general-player*)

;--------------
;BASIC EXAMPLES
;-------------

(setf r (make-instance 'rule :leftpart '(b l o n d e) :rightpart '(b r u n e)))
(ApplyRule r '(u n e b l o n d e a u x y e u x e n a m e n d e s))

(setf r1 (make-instance 'rule :leftpart '(a) :rightpart '(x)))
(setf r2 (make-instance 'rule :leftpart '(a b) :rightpart '(y)))
(setf g (make-instance 'grammar :rulelist (list r1 r2)))

(ApplyRule r1 '(a a b))

(CompileLanguage g '(a b))
(CompileLanguage g '(a a b))
(CompileLanguage g '(a b a b))
(progn (mapcar 'PrintWord (CompileLanguage g '(a b a b))) 'ok)
;...avec quelques secondes de calcul:
(length (CompileLanguage g '(a a a a a a a a a a b)))                ;-> 1536 mots derives

;----------------
;MUSICAL EXAMPLES
;----------------

;???????????????????????????? 'CompileLanguage' ne marche pas

(progn (mapcar 'PrintWord (CompileLanguage bluesgrammar (MultibeatLabels->Jazzchordlist '((c maj7 1) (g 7 1))) 1)) 'ok)

(TimeCompile bluesgrammar (MultibeatLabels->Jazzchordlist '((g 7 2))) 1)             ;---> (5 nonterminals 4 terminals time 0.096 secondes)
(TimeCompile bluesgrammar (MultibeatLabels->Jazzchordlist '((c maj7 2) (g 7 2))) 1)  ;---> (301 nonterminals 163 terminals time 168.295 secondes)

(TimeCompile bluesgrammar (MultibeatLabels->Jazzchordlist '((g 7 1))) 1)   ;---> sequences (c maj7 1)^n (g 7 1) = temps exponentiel (0.01)^n
;((g 7 1))                                                                 ---> (1 nonterminals 1 terminals time 0.007 secondes)
;((c maj7 1) (g 7 1))                                                      ---> (4 nonterminals 4 terminals time 0.078 secondes)           
;((c maj7 1) (c maj7 1) (g 7 1))                                           ---> (15 nonterminals 15 terminals time 0.934 secondes)           
;((c maj7 1) (c maj7 1) (c maj7 1) (g 7 1))                                ---> (64 nonterminals 64 terminals time 10.505 secondes)           
;((c maj7 1) (c maj7 1) (c maj7 1) (c maj7 1) (g 7 1))                     ---> (293 nonterminals 293 terminals time 143.236 secondes)

(setf res1 (CompileLanguage bluesgrammar (MultibeatLabels->Jazzchordlist '((c maj7 1) (c maj7 1) (c maj7 1) (g 7 1))) 1))
(setf res2 (CompileLanguage bluesgrammar (MultibeatLabels->Jazzchordlist '((eb m7 1) (a m7 1) (d m7 1) (g 7 1))) 1))
(length res1) ;---> 64
(length res2) ;---> 67, differences = prefixes reprenant l'ancienne sequence, mais les suffixes substitues sont les memes

;Pbs a etudier (cf. fichier "Tests IMPROTEK juil2012 Harmonie.odt")
;---> mauvais enchainements lies a jonction entre accords d'origine et debut d'un section substituee
|#



(defclass* rule ()
  ((leftpart :initform nil :initarg :leftpart :accessor leftpart)         ; = object list (symbols, chords, etc.)
   (rightpart :initform nil :initarg :rightpart :accessor rightpart)))

(defclass* grammar ()
  ((rulelist :initform nil :initarg :rulelist :accessor rulelist)))


(defclass* substitution (rule)
  (
   (rulename :initform 'myrule :initarg :rulename :accessor rulename)
   (dividing :initform nil :initarg :dividing :accessor dividing)
   (marking :initform t :initarg :marking :accessor marking)))

(defclass* steedmangrammar (grammar)
  ((threshold :initform 1 :initarg :threshold :accessor threshold)))   ;control the deepness of rule application


(defclass* jazzchord ()
  (
   (root :initform 'c :initarg :root :accessor root)
   (quality :initform 'maj7 :initarg :quality :accessor quality)
   (nbeats :initform 4 :initarg :nbeats :accessor nbeats)
   (marked :initform nil :initarg :marked :accessor marked)
   (nonterminal :initform nil :initarg :nonterminal :accessor nonterminal)
))


;Steedman's grammar
;- dividing rules can only have 1 chord in the left part
;- marking rules can only have 2 chords in the left part
(setf rule-I
   (make-instance 'substitution :leftpart '((x maj7)) :rightpart '((x maj7) (x maj7)) :dividing t :marking nil :rulename 'I))
(setf rule-I7 
   (make-instance 'substitution :leftpart '((x 7)) :rightpart '((x maj7) (x 7)) :dividing t :marking nil :rulename 'I7))
(setf rule-Im7                                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'((x maj7) (x m7)) pourquoi "majorer" l'accord ?????

   (make-instance 'substitution :leftpart '((x m7)) :rightpart '((x m7) (x m7)) :dividing t :marking nil :rulename 'Im7))

(setf rule-V7-I7 (make-instance 'substitution :leftpart '(w (x 7)) :rightpart '(((D x) 7) (x 7)) :rulename 'V7-I7))
(setf rule-V7-Im7 (make-instance 'substitution :leftpart '(w (x m7)) :rightpart '(((D x) 7) (x m7)) :rulename 'V7-Im7))
(setf rule-IIm7-V7 (make-instance 'substitution :leftpart '(w (x 7)) :rightpart '(((D x) m7) (x 7)) :rulename 'IIm7-V7)) ;;;;;;;;;;;;;;si w est deja Dx m7????
(setf rule-IIb7-I7 (make-instance 'substitution :leftpart '(((D x) 7) (x 7)) :rightpart '(((Stb x) 7) (x 7)) :rulename 'IIb7-I7))
(setf rule-IIb-I (make-instance 'substitution :leftpart '(((D x) 7) (x maj7)) :rightpart '(((Stb x)) (x maj7)) :rulename 'IIb-I))
(setf rule-IIb-Im7 (make-instance 'substitution :leftpart '(((D x) 7) (x m7)) :rightpart '(((Stb x)) (x m7)) :rulename 'IIb-Im7))  ;;;;;verifier ((Stb x)) avec maj7???

(setf bluesgrammar (make-instance 'steedmangrammar 
          :rulelist (list rule-I rule-I7 rule-Im7 
                          rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 
                          rule-IIb7-I7 rule-IIb-I rule-IIb-Im7)))



;AUTOMATON: TRANSITION FUNCTION FROM RIGHT TO LEFT
;-------------------------------------------------
;Steedman's rules may be applied from right to left, the resulting transition function representing a finite automaton.
;X7     ---> may be preceded by Dx7 or Dxm7 or Stbx7     [rule-V7-I7, rule-IIm7-V7, rule-IIb7-I7]
;Xm7    ---> may be preceded by Dx7 or Stbxmaj7          [rule-V7-Im7, rule-IIb-Im7]
;Xmaj7  ---> may be preceded by Dx7 or Stbxmaj7          [rule-V7-Im7, rule-IIb-Im7]

;PROBLEME: les regles donnent les enchainements possibles (V-I ou IIb-I)
;mais ce qui pose pb, c'est la condition d'applicabilite par rapport a la sequence d'origine (acc. non marque? acc non 7?)
;---> il faudrait donner une demonstration formelle de l'equivalence: 
;                  applicabilite des regles <===> acceptabilite de la sequence obtenue par transitions

#|
(PrintWord (substitute-by-transition (MultibeatLabels->Jazzchordlist '((a m7 4) (b m7 4) (g m7 4) (a m7 4)))))


(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; voicings from Hermeto Pascoal   
(setf subst (substitute-by-transition (MultibeatLabels->Jazzchordlist 
                                       '((a m7 4) (b m7 4) (g m7 4) (a m7 4)))))
(PrintWord subst)         
(play (setf harmo (beats->chseq (PlayVoicings hermetovoicings (Jazzchordlist->MultibeatLabels subst) 500) 500 0))))

(PrintWord revegrid)
(rewrite raingrammar revegrid 5)
(PrintWord (substitute-by-transition revegrid 1))

(setf revegrid (MultibeatLabels->Jazzchordlist '((a m7 2))))

;From Bkup 9aout11 ImprotekInprogress
;Essai de substitution sur les accords de 'Reve eveille':

(setf *reve-changes* '((a m7) (a m7) / (b m7) (b m7)  / (g m7) (g m7) / (a m7) (a m7)))
(setf revegrid (InitGrid *reve-changes*))

(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; voicings from Hermeto Pascoal   
(setf subst (substitute-by-transition (MultibeatLabels->Jazzchordlist (grid Jaime_tune))))
(PrintWord subst)         
(play (setf harmo (beats->chseq (PlayVoicings basicvoicings (Jazzchordlist->MultibeatLabels subst) 500) 500 0))))

(Stop-Player *general-player*)
|#

(defun substitute-by-transition (jazzchordlist &optional (threshold 1))
  (loop for x in (random-partition jazzchordlist)
        append (if (> (random 10) 7)   ;;(= (random 10) 1)   ;;;;;;;;;;Marc 9/7/13
                   x 
                 (substitute-by-transition-on-segment x threshold)
)
))
;;;;;;;;;;;;;;;;;;;;;;;;IL FAUT EVITER LES REDITES !!!!!!!! donc pas de subst. independantes sur les segments

(defun random-partition (l)
  (if (null l) nil
    (let ((n (min 1                ;;;;;;;;Marc Uzeste 9/7/13
                  (1+ (random (length l))))))  
      (cons (nthcar n l) (random-partition (nthcdr n l))))))

(defun substitute-by-transition-on-segment (jazzchordlist &optional (threshold 1))
  (if (null jazzchordlist) nil
    (loop with rlist = (reverse jazzchordlist)    ;substitutions are generated from right to left
          with res = nil
          for l = (cdr rlist) then (cdr l)
          with y = (clone (first rlist))
          for possibly-reduced-y = (clone y)

          when (and (not (null y)) (> (nbeats y) threshold) )  ;(= (random 2) 1))      ;;;;;Marc Uzeste 9/7/13
          do (divide-chord-durations (list y possibly-reduced-y) (nbeats y))
             (setf (quality y) 'maj7)
             (push y l)

          do (push possibly-reduced-y res)                    
          when (not (null l)) do (setf y (choose-backward-chord (first l) possibly-reduced-y))
          when (or (null l) (null y)) return (append (reverse l) res)

)))
           
(defun choose-backward-chord (originalchord destinationchord)       
  (if (steedman-backward-condition originalchord destinationchord) 
      (let ((newchord (nth-random (steedman-backward-transition destinationchord))))      ;nth-random defined in "combinatorial.lisp"
        (setf (nbeats newchord) (nbeats originalchord))
        newchord)
    (format *om-stream* "Terminal state: first chord is a dominant one ~a ~a~%" (Jazzchord->Label originalchord) (Jazzchord->Label destinationchord))))

(defun steedman-backward-transition (destinationchord)
  (case (quality destinationchord)
    (7 (list (make-instance 'jazzchord :root (TransposeRoot (root destinationchord) 7) :quality '7)
             (make-instance 'jazzchord :root (TransposeRoot (root destinationchord) 7) :quality 'm7)
             (make-instance 'jazzchord :root (TransposeRoot (root destinationchord) 1) :quality '7)))
    (m7 (list (make-instance 'jazzchord :root (TransposeRoot (root destinationchord) 7) :quality '7)
              (make-instance 'jazzchord :root (TransposeRoot (root destinationchord) 1) :quality 'maj7)))
    (maj7 (list (make-instance 'jazzchord :root (TransposeRoot (root destinationchord) 7) :quality '7)  
                (make-instance 'jazzchord :root (TransposeRoot (root destinationchord) 1) :quality 'maj7)))))
                                                 

(defun steedman-backward-condition (originalchord destinationchord)
  (or t;(not (eq (quality originalchord) '7))                                          ;rules V7-I7, V7-Im7, IIm7-V7    ;;;;;;Marc 9/7/13
      (= 7 (+ (DeltaRoot (root destinationchord) (root originalchord)) 12))))      ;rules IIb7-I7, IIb-I, IIb-Im7

(defun divide-chord-durations (jazzchordlist duration)
  (loop for l = jazzchordlist then (cdr l) until (null l)
        for x in (divide-durations duration) for ch = (first l)
        ;when (cdr l) do (setf (quality ch) 'maj7)
        do (setf (nbeats ch) x)))

(defun divide-durations (duration)
  (case duration (4 (list 2 2)) (3 (list 2 1)) (2 (list 1 1))
    (t (format *om-stream* "Error: Trying to divide an out of range duration~%" duration))))

#|
ESSAIS SUBSTITUTION REVE EVEILLE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

grille :  Am7(4) Bm7(4) Gm7(4) Am7(4)
harmo10 : E7(2) A7(2) D7M(2) Ab7(2) Gm7(4) Am7(4) 
harmo9 :  A7(4) D7(4) D7(2) Gm7(2) E7(2) Am7(2)
harmo8 :  Am7(2) C7M(2) Bm7(4) Bm7(2) E7(2) Am7(4)



(PrintWord revegrid)
(rewrite raingrammar revegrid 5)
(PrintWord (substitute-by-transition revegrid 1))
(play (Jazzchordlist->MultibeatLabels

(setf revegrid (MultibeatLabels->Jazzchordlist '((a m7 2))))

;From Bkup 9aout11 ImprotekInprogress
;Essai de substitution sur les accords de 'Reve eveille':

(setf *reve-changes* '((a m7) (a m7) / (b m7) (b m7)  / (g m7) (g m7) / (a m7) (a m7)))
(setf revegrid (InitGrid *reve-changes*))
(setf revegrammar (make-instance 'steedmangrammar 
          :rulelist (list rule-I7 rule-Im7 
                          rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 
                          rule-IIb7-I7)))

(progn (ctrlchg 7 0 10) (ctrlchg 7 60 2)
(pgmout 4 1)      ; channel 1 = "4 - Electric Piano 1", for a complete list evaluate the variable *midi-programs*
(pgmout 4 2)     ; channel 2 = "33 - Electric Fingered Bass"
(setf testlabels (GridLabel (rewrite raingrammar revegrid 5))
      harmo (ImprovizeOnHarmGrid oracle6 (length testlabels) testlabels))           ;oracle6 cf. "Rain" in 'ImprotekTutorial.lisp'
(loop for beat in harmo do (setf (MidiSet beat) (timestretch (MidiSet beat) (/ 400 484))))
(play (beats->chseq harmo 400 0))
)

(mapcar 'HarmLabel harmo)

(mapcar 'HarmLabel (multiple-beat->single harmo 400))


(save-for-antescofo (thread-Beats harmo) 400 "/Users/marc/Documents/RECHERCHE/TUTORIAL MAX/Antescofo~_Max_UB/CACA.txt")


|#

;OLD VERSION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defmethod rewrite ((self grammar) (word list) (n integer))
   (loop for i from n downto 1
         do (let* ((PossibleRules (loop for r in (rulelist self)
                                        when (loop for l = word then (cdr l) until (null l)
                                                   when (= (mismatch (leftpart r) l) (length (leftpart r)))
                                                   return t)
                                        collect r))
                   (ChosenRule (nth (random (length PossibleRules)) PossibleRules))
                   (res (ApplyRule ChosenRule word)))
              (setf word (nth (random (length res)) res)))
         finally return word))


;Applies one not dividing rule, and eventually one dividing rule
(defmethod rewrite ((self steedmangrammar) (grid list) (n integer))
  (format *om-stream* "------------------------------------------------------------~%") (PrintLabelList (GridLabel grid))
  (format *om-stream* "------------------------------------------------------------~%")
   (loop for i from n downto 1
         do (progn (when (> (random 3) 0) (setf grid (rewrite-once self grid t)))
                   (setf grid (rewrite-once self grid nil)))
         finally return (progn (format *om-stream* "------------------------------------------------------------~%")
                          grid))) 

;Applies a dividing rule if true, and a not dividing one if false
(defmethod rewrite-once ((self steedmangrammar) (grid list) (dividingbool t))
   (let* ((PossibleRules (loop for r in (funcall (if dividingbool 'remove-if-not 'remove-if) 'dividing (rulelist self))
                               when (loop for l = grid then (cdr l) until (null l)
                                          when (eligible-rule? r l (threshold self))
                                          return t)
                               collect r))
          (ChosenRule (nth (random (length PossibleRules)) PossibleRules)))
     (when ChosenRule 
       (let* ((res (ApplyRule ChosenRule grid (threshold self)))
              (newgrid (nth (random (length res)) res)))
;         (pushr (list ChosenRule res) (lastderivation self))
         (when (not dividingbool) (PrintLabelList (GridLabel newgrid)))
         newgrid))))

(defun InitGrid (labellist)                 ;InitGridWithBars
  (loop for x in labellist with beat = nil with beatlist = nil
        do (if (not (eq x '/)) (push (make-instance 'jazzchord :root (first x) 
                                                    :quality (if (null (cdr x)) 'maj7 (second x)))
                                     beat)
               (progn (push (reverse beat) beatlist) (setf beat nil)))
        finally return (loop for chordsinbeat in (reverse (push (reverse beat) beatlist))
                             append (loop for x in chordsinbeat for n = (length chordsinbeat)
                                          do (setf (nbeats x) (/ 4 n)) 
                                          collect x))))

;old versions kept for compatibility:
(defun GridLabel (grid) (Jazzchordlist->MultibeatLabels grid))
(defun GridLabelBeat (grid) (Jazzchordlist->Beats grid)) 


;APPLYING GRAMMARS'S RULES
;-------------------------

;Gives ALL the sequences obtained by applying a rule in all possible positions
(defmethod ApplyRule ((r rule) (word list) &optional (threshold 1))   ;return nil if no match, 'threshold' specific to musical grammars
   (loop for suffix = word then (cdr suffix) until (null suffix)
         for i from 0
         when (prefix? (leftpart r) suffix :test 'equal) 
         collect (append (nthcar i word) 
                         (rightpart r) 
                         (nthcdr (+ i (length (leftpart r))) word))))

(defun prefix? (l1 l &key test) 
  (let ((nomatch (mismatch l1 l :test test)))       ;(mismatch l1 l2) = nil when l1 and l2 nil, 
    (or (null nomatch)                              ;otherwise = length of the longest common prefix of l1 and l2
        (= nomatch (length l1))))) 


;Gives ALL the sequences obtained by applying a rule in all possible positions
;'threshold' used to control the deepness of rule application (optional since it is not used for general grammars)
(defmethod ApplyRule ((r substitution) (jazzchordlist list) &optional (threshold 1))
  (when (check-rule-for-dividing-and-marking r)
    (loop for suffixe = jazzchordlist then (cdr suffixe) until (null suffixe) 
          for i from 0
          with root = nil
          do (setf root (FindRoot (leftpart r) suffixe))
          when (eligible-rule? root r suffixe threshold)
          collect (append (nthcar i jazzchordlist) 
                          (create-jazzchordlist-from-rightpart root r (subseq jazzchordlist i (+ i (length (leftpart r))))) 
                          (nthcdr (+ i (length (leftpart r))) jazzchordlist)) into res
          finally return res)))


(defmethod check-rule-for-dividing-and-marking ((r substitution))
  (cond ((and (dividing r) (not (= (length (leftpart r)) 1))) (format *om-stream* "Error: Dividing rule ~a with more than one chord~%" (rulename r)) nil)
        ((and (marking r) (not (= (length (leftpart r)) 2))) (format *om-stream* "Error: Marking rule ~a with less or more than two chords~%" (rulename r)) nil)
        (t t)))

(defmethod eligible-rule? (root (r substitution) suffixe threshold)
;(print (list 'regle (rulename r) 'seq (GridLabel suffixe)))
  (and (prefix? (FixRoot root (leftpart r)) suffixe :test 'equal-up-to-w)
       (or (not (dividing r)) (all-above-threshold? (nthcar (length (leftpart r)) suffixe) threshold))
        ;(and nomatchright (< nomatchright (length (rightpart r))))
))

(defmethod create-jazzchordlist-from-rightpart (root (r substitution) original-jazzchordlist)
  (let ((new-jazzchordlist (loop for x in (FixRoot root (rightpart r))
                                 collect (make-instance 'jazzchord :root (first x) :quality (if (null (cdr x)) 'maj7 (second x)))))) 
    (when (and (dividing r) (= (length (leftpart r)) 1)) (divide-chord-durations new-jazzchordlist (nbeats (first original-jazzchordlist)))
      (setf (nonterminal (first new-jazzchordlist)) t))  ;first chord obtained after dividing is a non terminal (cannot stay in a terminal sequence)
    (when (and (marking r) (= (length (leftpart r)) 2)) (set-chord-durations new-jazzchordlist original-jazzchordlist)
      (mark-changed-chord-root new-jazzchordlist))
    new-jazzchordlist))

;divide-chord-durations ---> see above

(defun set-chord-durations (new-jazzchordlist original-jazzchordlist)
  (if (not (= (length new-jazzchordlist) (length original-jazzchordlist))) (format *om-stream* "Error: Original and substituted sequences differ in length~%")
    (loop for x in new-jazzchordlist for y in original-jazzchordlist do (setf (nbeats x) (nbeats y)))))      

(defun mark-changed-chord-root (jazzchordlist) (setf (marked (first jazzchordlist)) t))


;utilities for Steedman's musical rules
(defun all-above-threshold? (jazzchordlist threshold)          ;1 is the lowest chord duration, no duration <= 1 can be divided
  (loop for x in jazzchordlist when (<= (nbeats x) threshold) return nil finally return t))

;In Steedman's rules, the variable 'w' stands for a chord
;1- that is not marked (its root has not been changed)
;2- THAT IS NOT A DOMINANT CHORD 7
;DJHarm 1.0 version in 2001 only took into account the first condition
(defmethod equal-up-to-w (label (chord jazzchord)) 
  (if (equal label 'w)    ;be careful that if label=w, one cannot go to the else part because of (second w)
      (and (not (equal (quality chord) '7))  ;;;;;;;;; THIS CONDITION WAS NOT IN THE ORIGINAL 1999-2001 PROGRAM
           (not (marked chord))  
       )
      (and (eq (first label) (root chord)) (eq (second label) (quality chord)))))

(defun FindRoot (rulepart grid)
  (loop for label in rulepart for chord in grid 
        when (and (listp label) (eq (first label) 'x)) return (root chord)))

(defun FixRoot (root rulepart) 
   (loop for label in rulepart
         if (atom label) collect label
         else collect (TransposeLabel (cons root (cdr label))
                                      (if (atom (first label)) 0
                                          (case (caar label) (D 7) (Stb 1))))))

;-------------------------------------------------------------------
;COMPILING ALL SEQUENCES GENERATED BY THE GRAMMAR FROM A GIVEN 'word'


;!!!!!!!!!!!!!!!!!!!!! FAIRE COMPILATION JUSQU'A UN NOMBRE D'APPLICATION DE REGLES FIXE -> PEUT REMPACER 'rewrite


(defmethod CompileLanguage ((self grammar) word &optional (threshold 1)) 
  (ScanTree self (list word) (rulelist self) threshold))

(defmethod CompileLanguage ((self steedmangrammar) word &optional (threshold 1)) 
  (remove nil (ScanTree self (list word) (rulelist self) threshold) 
          :test #'(lambda (x y) (member nil y :test #'(lambda (u v) (nonterminal v))))))

(defmethod ScanTree ((self grammar) wordlist rulelisttmp &optional (threshold 1))
  (if (or (null rulelisttmp) (> (length wordlist) 5000)) wordlist
      (let ((new-wordlist (loop for w in wordlist collect (clone w))))
        (loop for word in wordlist (Jazzchordlist->MultibeatLabels reduced-y)
              do (loop for x in (ApplyRule (first rulelisttmp) word threshold)
                       when (NotAlreadyWaiting x new-wordlist) do (pushr x new-wordlist)))
        (if (= (length new-wordlist) (length wordlist)) 
          (ScanTree self wordlist (cdr rulelisttmp) threshold)
          (ScanTree self new-wordlist (rulelist self) threshold)))))

(defun NotAlreadyWaiting (word waiting-word-list) 
  (not (member word waiting-word-list :test 
               (if (equal (class-of (first word)) (class-of (make-instance 'jazzchord)))
                 #'(lambda (w1 w2) 
                     (loop for x in w1 for y in w2
                           when (not (and (eq (root x) (root y)) (eq (quality x) (quality y)) (eq (nbeats x) (nbeats y)))) 
                           return nil 
                           finally return t))
                 'equal))))

(defun PrintWord (word)
  (if (equal (class-of (first word)) (class-of (make-instance 'jazzchord))) 
    (PrintLabelList (Jazzchordlist->MultibeatLabels word))
    (format *om-stream* "~a~%" word)))

(defmethod TimeCompile ((self steedmangrammar) word &optional (threshold 1)) 
  (let* ((t1 (get-internal-real-time))
         (nontermlist (ScanTree self (list word) (rulelist self) threshold))
         (t2 (get-internal-real-time))
         (res (remove nil nontermlist :test #'(lambda (x y) (member nil y :test #'(lambda (u v) (nonterminal v)))))))
    (print (list  (length nontermlist) 'nonterminals (length res) 'terminals 'time (float (/ (- t2 t1) 1000)) 'secondes)) 'ok))



;----------------------------------------------------------------
;FORMATING DATA

(defun MultibeatLabels->Jazzchordlist (multiplebeat-labellist)
  (loop for x in multiplebeat-labellist
        collect (make-instance 'jazzchord :root (first x) :quality (second x) :nbeats (third x))))

(defun Jazzchordlist->MultibeatLabels (grid) (loop for x in grid collect (Jazzchord->Label x)))
(defun Jazzchord->Label (jazzchord) (list (root jazzchord) (quality jazzchord) (nbeats jazzchord)))

(defun Jazzchordlist->Beats (grid) 
   (loop for x in grid 
         append (make-list (nbeats x) :initial-element (list (root x) (quality x)))))

;http://www.gigamonkeys.com/book/a-few-format-recipes.html           
;An "at" sign can modify ~( to make it capitalize the first word in a section of text:
(defun PrintLabel (y) (format *om-stream* "~@(~a~)~a(~a) " (first y) (if (eq (second y) 'maj7) "" (second y)) (third y)))
(defun PrintLabelList (list) (loop for x in list do (PrintLabel x)) (format *om-stream* "~%"))

(defun Bluesify (labels) 
   (loop for x in (copy-list labels) do (when (not (eq (second x) 'm7)) (setf (second x) '7)) collect x))


;----------------------------------------------------------------
;PLAYING VOICINGS

(defmethod PlayVoicings ((voicinglist list) (multibeatgrid list) beatdur)
  (let* ((voicingoracle (make-Improvizer-from-voicings voicinglist beatdur))
         (labels (mapcar #'(lambda (x) (list (first x) (second x))) multibeatgrid))
         (labels (MakeLabelsFromList labels 'harmlabel))
         (durlist (mapcar #'third multibeatgrid))     ;nb beats
         (harmomultibeats (ImprovizeOnHarmGrid voicingoracle (length labels) labels)))
    (loop for x in harmomultibeats for z in durlist 
          do (setf (MidiSet x) (timestretch (MidiSet x) z))       ; durations are stretched to the number of beats
          append (cons x                                       ;1rst beat of a chord: MidiSet with the whole duration 
                       (make-list (- z 1)   ;+ adding as many beats as the chord duration with empty MidiSet
                                  :initial-element (make-instance 'midiharmbeat :label (Newharmlabel (root (label x)) (chordtype (label x)) )))))))

(defmethod make-Improvizer-from-voicings ((voicinglist list) beatdur)
  (let* ((timedvoicings (loop for x in voicinglist for y = (nth-random (second x))  ;chose one voicing if many for each chord
                              collect (list (first x) (loop for z in y 
                                                            collect (list z 0 (- beatdur 50) 80 5)))))  ;duration = 1 beat for each voicing
         (beats (mapcar 'make-midiharmbeat timedvoicings)))
    (NewImprovizer beats beatdur)))


;!!!!!!!!!!!!!!!!!! PROBLEME POUR UTILISER UNE SIMPLE NAVIGATION D'ORACLE POUR CALCULER LES VOICINGS:
;chaque accord = une attaque sur 1 beat X + prolongation sur 7 beats suivants X' (midi <=0)
;PROBLEME: si l'on a dans l'oracle l'enchainement Cm7 Cm7' Cm7' Cm7' Cm7' Cm7' Cm7' Cm7' G7 G7' G7' G7' G7' G7' G7' G7' 
;et si la grille est Cm7 Cm7 G7 G7, il cherche un prefixe max
;=> il trouve Cm7 Cm7' Cm7' Cm7' Cm7' Cm7' [ Cm7' Cm7' G7 G7' ] G7' G7' G7' G7' G7' G7' donc Cm7 demarre sur une prolongation
;=> 'thread-Beats' supprime les prolongations non precedees par une attaque
;=> il faut demarrer chaque accord sur une occurrence d'attaque, donc LA PLUS A GAUCHE (qu'elle soit reelle ou transposee)

;EN ATTENDANT: les voicings sont calcules sur 2 canaux speciaux (13 et 14)
;---> dans 'generate-improbeatlist-from-oraclelist', on utilise 'PlayVoicings' au lieu de 'ImprovizeOnHarmGrid'

;AUTRE SOLUTION: demultiplier les voicings sur plusieurs longueurs pour avoir TOUS les enchainements de 1, 2, 3, 4, 8 beats


(defmethod PlayVoicingsNEW ((voicinglist list) (multibeatgrid list) beatdur)
  (let* ((voicingoracle (make-Improvizer-from-voicingsNEW voicinglist beatdur))
         (labels (multibeatgrid->labels multibeatgrid)))
    (setf (max-continuity voicingoracle) 1000)
    (setf (randomPrefixOccurrenceMode voicingoracle) nil) ;?????? essais parametrage pour demarrer les accords sur des attaques (plus a gauche)
    (setf (bestTranspoMode voicingoracle) nil)
    (thread-Beats (ImprovizeOnHarmGrid voicingoracle (length labels) labels) beatdur)))

(defmethod make-Improvizer-from-voicingsNEW ((voicinglist list) beatdur)
  (let* ((timedvoicings (loop for x in voicinglist for y = (nth-random (second x))  ;chose one voicing if many for each chord
                              for midiroot = (MidiRoot (first (first x))) for midiquality = (MidiQuality (second (first x)))
                              with deltatime = (* 8 beatdur)               ;duration = 8 beats for each voicing
                              for onset = 0 then (+ onset deltatime)
                              append (append (list (list 12 onset 10 100 16)                          ; clock dur=10 vel=100
                                                   (list midiroot (+ onset 50) (- beatdur 50) midiquality 16))  ; label
                                             (loop for z in y 
                                                   collect (list z (+ onset 10) (- deltatime 50) 80 5))
                                             (loop for i from 1 to 7 for onset1 = (+ onset (* i beatdur))
                                                   append (list (list 12 onset1 10 100 16)
                                                                (list midiroot (+ onset1 50) (- beatdur 50) midiquality 16))))))
         (beats (mapcar 'make-midiharmbeat (clocked-evts->beats timedvoicings))))
    (NewImprovizer beats beatdur)))



#|
(setf (max-continuity o) 1000)
(setf (randomPrefixOccurrenceMode o) nil)
(setf (bestTranspoMode o) nil)
(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; basic voicings        
(play (setf harmo (beats->chseq (PlayVoicings basicvoicings multibeatgrid 500) 500 0))))

(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; basic voicings        
(play (setf harmo (beats->chseq (PlayVoicingsOld basicvoicings multibeatgrid 500) 500 0))))

(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; basic voicings        
(play (setf harmo (beats->chseq (PlayVoicings (voicings *current-tune*) (grid *current-tune*) (beatduration *current-tune*)) 
                                (beatduration *current-tune*) 0))))


(save-as-midi-with-tempo harmo 500)

(setf labels (multibeatgrid->labels multibeatgrid))
(play (setf harmo (beats->chseq (thread-Beats (ImprovizeOnHarmGrid o (length labels) labels) 500) 500 0)))
(play harmo)

(setf *current-tune* Allthethingsyouare_tune)

(setf o (make-Improvizer-from-voicings hermetovoicings (beatduration *current-tune*)))
(loop with vect = (vectext o) for i from 1 to (1- (maxetat o)) do (print (list (HarmLabel (aref vect i)) (MidiSet (aref vect i)))))
(om-inspect o)

(progn (pgmout 4 1) (ctrlchg 7 120 1)                       
(play (setf harmo (beats->chseq (PlayVoicings basicvoicings (grid *current-tune*) (beatduration *current-tune*)) (beatduration *current-tune*) 0))))

(Stop-Player *general-player*)
|#



(setf basicvoicings '(
((b m7)     ((47 57 61 62 66)))     
((e 7)      ((40 56 61 62 66)))
((a maj7)   ((45 56 59 61 64))) 

((e m7)     ((40 55 59 62 66))) 
((a 7)      ((45 55 59 61 66)))
((d maj7)   ((38 54 57 61 64)))
))

(setf hermetovoicings '(
((b m7)     ((47 57 61 62 64)))            ; (- 4 7 9)
((e 7)      ((40 56 60 62 67)))            ; (7 9+ 13-)
((a maj7)   ((45 56 59 61 66)))            ; (6 7+ 9)

((e m7)     ((40 55 57 62 66)))      ; (- 4 7 9)
((a 7)      ((45 55 59 61 63 66)))    ;11+
((d maj7)   ((38 54 57 61 64)))
))













#|


;DICIDENBAS

;Open a MIDI file with chords on channel 16, and then give a couple: (list of pairs label+mididata, beatdur)
(let ((tmp (midi-to-beats)))
(setf multbeatigrid Dicidenbas_grid)
(setf DiciChiff_beatdur (second tmp)
      DiciChiff_beatsfromfile (first tmp)
      DiciChifflist (make-midiharmbeat-list DiciChiff_beatsfromfile DiciChiff_beatdur)
      oracleDiciChiff (NewImprovizer DiciChifflist DiciChiff_beatdur)
(setf labelDiciChiff (GridLabelBeat (Rewrite bluesgrammar (InitMultiplebeatGrid multibeatgrid) 5))
      ;labelDiciChiff (GridLabelBeat (InitMultiplebeatGrid multibeatgrid))
)
))

(progn (pgmout 4 1) (ctrlchg 7 0 15) (setf (max-continuity oracleDiciChiff) 100)
(setf ;labelDiciChiff (GridLabelBeat (Rewrite bluesgrammar (InitMultiplebeatGrid multibeatgrid) 3))
      labelDiciChiff (GridLabelBeat (InitMultiplebeatGrid multibeatgrid))
)
(setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid oracleDiciChiff (length labelDiciChiff) labelDiciChiff) 
                                                DiciChiff_beatdur)
                                  DiciChiff_beatdur 0)
                    (beats->chseq (make-clocks (length labelDiciChiff) DiciChiff_beatdur 2) 
                                  DiciChiff_beatdur 0)))
(play impro))



;(my-save-as-midi impro DiciChiff_beatdur)
;(om-inspect Zistebeatlist)









;!!!!!!!!!!!!!!!!!!!!!!!!  Faire disparaitre definitivement InitGridWithBars 
(GridLabel (AdaptRule rule-V7-I7 (InitGridWithBars '((c m7) (g 7)))))

(mapcar 'GridLabel (ApplyRule rule-V7-I7 (InitGridWithBars '((c m7) (g 7) / (c 7))) 1))

(mapcar 'GridLabel (ApplyRule rule-V7-I7 raingrid 1))

(GridLabel (rewrite-once raingrammar raingrid t))    ;dividing
(GridLabel (rewrite-once raingrammar raingrid nil))  ;not dividing

(mapcar 'GridLabel (ApplyRule rule-V7-I7 (InitGridWithBars '((c m7) (g 7) / (c 7))) 1))

(mapcar 'GridLabel (ApplyRule rule-V7-I7 raingrid 1))

(GridLabel (rewrite raingrammar raingrid 3))

(GridLabel (rewrite raingrammar raingrid 4))


|#




;------------------------------------------
;MORE EXAMPLES FROM 'ImprotekTutorial.lisp'
;------------------------------------------
;!!!!!!!!!!!!!!!!!!!!!!!!VIRER LES GRILLES AVEC DES BARRES DE MESURES / ENTRE LES ACCORDS !!!!!!!!!!!!!!!!!!!!!!!

(defun InitGridWithBars (labellist)
  (loop for x in labellist with beat = nil with beatlist = nil
        do (if (not (eq x '/)) (push (make-instance 'jazzchord :root (first x) 
                                                    :quality (if (null (cdr x)) 'maj7 (second x)))
                                     beat)
               (progn (push (reverse beat) beatlist) (setf beat nil)))
        finally return (loop for chordsinbeat in (reverse (push (reverse beat) beatlist))
                             append (loop for x in chordsinbeat for n = (length chordsinbeat)
                                          do (setf (nbeats x) (/ 4 n)) 
                                          collect x))))


(setf *rain-changes*  '((b m7) (e 7) (bb m7) (eb 7) / (g# m7) (c# 7) / (f#) (g# 7) / (b 7) (bb 7)))

#|
;EXAMPLE # 06 "Rain" 
;=====================

(progn (ctrlchg 7 100 10)
(pgmout 4 1)      ; channel 1 = "4 - Electric Piano 1", for a complete list evaluate the variable *midi-programs*
(pgmout 33 2)     ; channel 2 = "33 - Electric Fingered Bass"
(let* ((newlabels6 (GridLabel (rewrite bluesgrammar (InitGridWithBars *rain-changes*) 4)))
       (nbgrids (/ (loop for x in newlabels6 sum (third x)) 16))
       (drumpart (make-drumpart nbgrids)))
  (play (merger (beats->chseq (ImprovizeOnHarmGrid oracle6 (length newlabels6) newlabels6) 484 0)
                (beats->chseq (make-midiharmbeat-list drumpart) 484 0))))
)

;'oracle6' and 'make-drumpart' see ImprotekTutorial.lisp

|#



(setf *boogie-changes*               ; a blues in E
'((e) / (e) / (e) / (e 7) / (a) / (a) / (e) / (e) / (b) / (b 7) / (e) / (e)))
(setf boogiegrammar (make-instance 'steedmangrammar :threshold 2
                      :rulelist (list rule-I rule-I7 
                                      rule-V7-I7 rule-V7-Im7 rule-IIm7-V7
                                      rule-IIb7-I7 rule-IIb-I)))

(defun GridLabelBoogie (grid)  ;4th elt in labels = pos in the bar (1st or 3rd beat) + all qualities = 7
   (let ((BeatList
          (loop for x in grid with boolstrongbeat = t
                when (>= (nbeats x) 4) collect (list (root x) 7 (nbeats x) 1)
                when (= (nbeats x) 2) 
                collect (if boolstrongbeat
                          (progn (setf boolstrongbeat nil) (list (root x) 7 (nbeats x) 1))
                          (progn (setf boolstrongbeat t) (list (root x) 7 (nbeats x) 
                                                               3))))))   ;position=3rd beat
     (GroupChordPairs (GroupChordPairs BeatList 8) 8)))

(defun GroupChordPairs (labellist nbeatmax)   ;WARNING: does not check the position of the barlines
  (loop for l = labellist then l until (null l)
        collect (if (or (null (cdr l)) 
                        (not (eq (first (car l)) (first (cadr l))))
                        (not (eq (second (car l)) (second (cadr l))))
                        (/= (third (car l)) (third (cadr l)))
                        (/= (fourth (car l)) 1)
                        (= (third (car l)) nbeatmax))
                  (pop l)
                  (let* ((x1 (pop l)) (x2 (pop l))) (setf (third x1) (+ (third x1) (third x2))) x1))))


#|
;The grid used has a fourth element for each chord indicating the position in the bar (1 or 3)
;The following function groups chords having the same label:
(GroupChordPairs '((e 7 4 1) (e 7 4 1) (c 7 4 1) (b 7 4 1) (e 7 4 1) (a 7 4 1) (a 7 4 1) (e 7 4 1) (e 7 4 1) (b 7 4 1) (f 7 4 1) (e 7 4 1) (e 7 4 1))
8)

;'Ad hoc' function which fits the requirements of the associated voicing in the oracle for the "Boogie" example:
(GridLabelBoogie (rewrite boogiegrammar (InitGridWithBars *boogie-changes*) 5))

;EXAMPLE # 07 "Boogie" 
;=====================

(progn (setf impro nil)
(pgmout 17 1)      ; channel 1 = "17 - Percussive Organ" 
(pgmout 52 2)      ; channel 2 = "52 - Choir Aahs", channel 1 duplicated for a more sustained sound
(pgmout 78 3)      ; channel 3 = "78 - Whistle", channel 1 duplicated for the bass part
(ctrlchg 7 110 1) (ctrlchg 7 92 2) (ctrlchg 7 127 3)
(let* ((newlabels7 (GridLabelBoogie (rewrite boogiegrammar (InitGridWithBars *boogie-changes*) 5)))
       (impro1 (beats->chseq (ImprovizeOnHarmGrid oracle7 (length newlabels7) newlabels7) 414 0))
       (impro2 (clone impro1)) (impro3 (clone impro1)))
  (setf (Lchan impro2) (change-element 2 1 (Lchan impro2)) (Lchan impro3) (change-element 3 1 (Lchan impro3))
        (Lmidic impro3) (mapcar #'(lambda (l) (remove-if #'(lambda (x) (> x 6000)) l)) (Lmidic impro3))    ; bass part
        impro (merger (merger impro1 impro2) impro3))
  (play impro)
))


|#

(setf *ziste-changes* '(
(bb) / (bb) / (bb) / (bb 7) / (eb) / (eb) / (bb) / (bb) / (f 7) / (eb) / (bb) / (f 7)))
;(setf zistegrammar (make-instance 'steedmangrammar        ;"Ziste" whithout maj7
;          :rulelist (list rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 rule-IIb7-I7)))
(setf zistegrammar (make-instance 'steedmangrammar 
          :rulelist (list rule-I rule-I7 rule-Im7 
                          rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 
                          rule-IIb7-I7 rule-IIb-I rule-IIb-Im7)))

(setf beats9comp (make-midiharmbeat-list zistecomp))

(setf oracle9comp 
      (let ((o (NewImprovizer)))      
        (loop for i from 0 to (1- (length beats9comp)) do (learn-event o (nth i beats9comp))) o))

(setf (max-continuity oracle9comp) 1)

#|

(Bluesify (GridLabelBeat (InitGridWithBars *ziste-changes*))))

;EXAMPLE # 09 "Ziste" 
;====================

;Plays solo on the blues chord changes with voicings on substituted chord changes
(progn  (setf impro nil)               
(pgmout 4 1) (pgmout 4 3)     ; channel 4 = sustained voicings, channel 3 = rhythmic voicings
(pgmout 36 4)     ; "36 - Slap Bass 1", fixed bass pattern
(pgmout 36 2)     ; "36 - Slap Bass 1", bass related to chords
(pgmout 4 11)     ; channel 11 = "4" (solo by Bernard Lubat)
(ctrlchg 7 100 1) (ctrlchg 7 80 3) (ctrlchg 7 50 4) (ctrlchg 7 127 11) (ctrlchg 7 90 10) (ctrlchg 7 50 2) 
(let* ((labels9comp (Bluesify (GridLabel (rewrite zistegrammar (InitGridWithBars *ziste-changes*) 5)))))
  (setf impro (merger (merger (beats->chseq (ImprovizeOnHarmGrid oracle9 (length labels9) labels9) 536 0)
                              (beats->chseq (ImprovizeOnHarmGrid oracle9comp (length labels9comp) labels9comp) 536 0))
                      (beats->chseq (make-midiharmbeat-list (list zistefunk)) 536 0)))
  (format *om-stream* "------------------------------------------------------------~%") (PrintLabelList labels9comp)
  (format *om-stream* "------------------------------------------------------------~%")
  (play impro))
)
|#



(setf *goodbye-changes*      '(          ;"Goodbye Pork Pie Hat" by Mingus
        (f 7) (c# 7) / (f#) (b 7) / (eb 7) (c# 7) / (eb 7) (f 7) /
        (bb m7) (g# 7) / (g m7) (c 7) / (d 7) (g 7) / (c# 7) (f#) /
        (bb 7) (c# 7) / (c 7) (eb 7) / (f 7) (c# 7) / (f#) (b 7)))
(setf *goodbye-changes1* '(              ;substitutions applied to "Goodbye Pork Pie Hat" (see grid below)
(g# 7) (g 7) / (f# 7) (f 7) (e 7) (e 7) / (e 7) (eb 7) (bb 7) (bb 7) / (c 7) (f# 7) (f# 7) (f 7) /
(b maj7) (bb m7) (a 7) (a 7) / (g# maj7) (d 7) (c# 7) (eb 7) / (g# 7) / (g 7) (g 7) (f# 7) (f 7) /
(b 7) (eb 7) (d 7) (d 7) / (c# 7) (g 7) (f# 7) (f# 7) / (d 7) (d 7) (g maj7) (c# 7) / (c 7) (b 7)))
#|
_____________________________________________________________________________________
|G#7       G7        |F#7  F7   E7        |E7   Eb7  Bb7       |C    F#7  F#7  F7   |
|____________________|____________________|____________________|____________________|
|B    Bbm7 A7        |G#   D7   C#7  Eb7  |G#7       G#7       |G7        F#7  F7   |
|____________________|____________________|____________________|____________________|
|B7   Eb7  D7        |C#7  G7   F#7       |D7        G    C#7  |C7        B7        |
_____________________________________________________________________________________
|#
(setf goodbyegrammar (make-instance 'steedmangrammar 
          :rulelist (list rule-I rule-I7 rule-Im7 
                          rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 
                          rule-IIb7-I7 rule-IIb-I rule-IIb-Im7)))


#|

(GridLabel (InitGridWithBars *goodbye-changes1*))

;In this grid, every label has the same 1 beat duration, and contains two elements: root + quality of the chord
(GridLabelBeat (InitGridWithBars *goodbye-changes1*))
(GridLabelBeat (rewrite goodbyegrammar (InitGridWithBars *goodbye-changes*) 10))

;EXAMPLE # 10 "Goodbye" 
;======================

(progn (setf impro nil)
(pgmout 4 1)       ; channel 1 = piano voicings "4 - Electric Piano 1"
(pgmout 78 2)      ; channel 2 = bass "78 - Whistle"      
(pgmout 75 11)     ; channel 11 = flute "75" (solo by Bernard Lubat)
(ctrlchg 7 110 1) (ctrlchg 7 127 2) (ctrlchg 7 127 11)
(let* ((newlabels10 (GridLabelBeat (rewrite goodbyegrammar (InitGridWithBars *goodbye-changes1*) 3)))
       (drumpart (loop for i from 1 to (floor (length newlabels10) 8) collect roof)))             ; roof duration = 8 beats (2 bars)
  (setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid oracle10 (length newlabels10) newlabels10)) 779 0)
                      (beats->chseq (make-midiharmbeat-list drumpart) 779 0)))
  (play impro))
)

(Stop-Player *general-player*)

|#


;From EXAMPLE # 11:
(setf *israel-changes* '(
(d m7) / (d m7) / (d m7) / (d 7) / (g m7) / (c 7) / (f maj7) / (bb maj7) / (e m7) / (a 7) / (d m7) / (a 7)))







