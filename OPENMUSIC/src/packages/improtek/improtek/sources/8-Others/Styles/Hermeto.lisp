;Hermeto.lisp / July 19th 2010
;by Marc Chemillier
;Improvize chord progressions based on Hermeto Pascoal's "Calendario Do Som"
;and make an harmonization of a melody in the style of Hermeto (see Harmonization.lisp for a complete description):
;http://www.hermetopascoal.com.br/partituras_calendario.asp

(in-package :om)

(defclass* relativechord (jazzchord)
  ((relativeroot :initform 0 :initarg :relativeroot :accessor relativeroot)
))

;for 'jazzchord' class, see "Substitution.lisp"

(setf list-relativechord (list (make-instance 'relativechord :relativeroot 0 :quality '(- 4 7 9) :nbeats 1)
                               (make-instance 'relativechord :relativeroot 2 :quality '(6 7+ 9) :nbeats 1)
                               (make-instance 'relativechord :relativeroot 2 :quality '(- 4 7 9) :nbeats 1)
                               (make-instance 'relativechord :relativeroot 2 :quality '(- 4 7 9) :nbeats 1)
                               (make-instance 'relativechord :relativeroot -4 :quality '(6 7+ 9) :nbeats 1)))

(setf oraclechord 
   (loop with o = (NewImprovizer) for i from 0 to (1- (length list-relativechord)) do (learn-event o (nth i list-relativechord)) finally return o))

(setf labelstesthermeto '((e (- 4 7 9)) (f# (- 4 7 9)) (g (- 4 7 9)) (eb (6 7+ 9)) (a (7 9+ 13-))))

#|
Free improvisation of chord sequences based on Hermeto's "Calendario":
(progn (pgmout 4 1) (ctrlchg 7 120 1)
(setf chprog (Improvize oraclecalendario 20) 
      labels (FixRoot chprog))
(play (beats->chseq (ImprovizeOnHarmGrid oraclevoicings (length labels) labels) 500 0))
)

(Stop-Player *general-player*)


;With two different durations
(progn (pgmout 4 1) (ctrlchg 7 120 1)
(setf chprog (Improvize oraclecalendario 20) labels (FixRoot chprog) 
      durlist (loop for i from 1 to (length labels) collect (case (random 5) (0 4) (1 1) (t 2))))
(setf impro (ImprovizeOnHarmGrid oraclevoicings (length labels) labels))
(setf impro (loop for x in impro for z in durlist 
                  do (setf (HarmLabel x) (append (HarmLabel x) (list z))
                           (MidiSet x) (timestretch (MidiSet x) z))
                  collect x))
(play (beats->chseq impro 500 0))
)

(my-save-as-midi (beats->chseq impro 500 0) 500)

;First oracle 'oraclechord': generate a simple chord progression

(setf labelstesthermeto (FixRoot (Improvize oraclechord 5)))

;Second oracle 'oraclevoicings': generate voicings for the previous chord progression

(setf impro (ImprovizeOnHarmGrid oraclevoicings (length labelstesthermeto) labelstesthermeto))

(progn (pgmout 4 1) (ctrlchg 7 120 1)
(play (beats->chseq impro 500 0)))


;These oracles have different classes of objects:

(class-of (otext oraclechord 1))
(class-of (otext oraclecalendario 1))
(class-of (otext oraclevoicings 1))


|#


;ORACLE FOR GENERATING CHORD PROGRESSIONS IN THE STYLE OF HERMETO
;----------------------------------------------------------------       

(defun make-chordlist (labellist)
  (let ((flatlist (loop for x in labellist append (rest x))))
    (loop for x in flatlist with previousroot = nil 
          collect (let ((newchord (make-instance 'relativechord :quality (second x) :nbeats (third x))))
                    (setf (relativeroot newchord) (if (null previousroot) 0 (DeltaRoot previousroot (first x))) previousroot (first x))
                    newchord))))
 
(defun FixRoot (chordprogression) 
  (loop for x in chordprogression with baseroot = 'c collect (list (setf baseroot (TransposeRoot baseroot (relativeroot x))) (quality x))))

;Function from the Improtek code that needs to be extended to 'Relativechord' objects for genericity:
(defmethod CompareEvents ((Event1 relativechord) (event2 relativechord))
  (and (equal (relativeroot event1) (relativeroot event2)) (equal (quality event1) (quality event2))))



;ORACLE FOR PLAYING A CHORD SEQUENCE USING PREDEFINED VOICINGS
;-------------------------------------------------------------
;A direct function PlayVoicings ((voicinglist list) (multiplebeat-harmGrid list) beatdur) is defined in Substitution.lisp
;for translating directly a 'grid' into 'timedvoicings' = a list of beats  according to a reference list of voicings

(defun make-beats-from-voicings (list-of-voicings)    ;beat duration fixed = 500 ms
   (mapcar 'make-beat (loop for x in list-of-voicings
                            append (loop for y in (second x)
                                         collect (list (first x) (loop for z in y collect (list z 0 500 100 1)))))))

(setf voicingshermeto '(   
                      
((e (- 4 7 9))       ((40 55 57 62 66)))
((bb (- 4 7 9))      ((46 56 60 61 63)))
((db (- 4 7 9))      ((49 59 63 64 67)))
((e (- 4))           ((40 55 57 62 66)))
((bb (- 4))          ((46 56 60 61 63)))

((e (7+))            ((40 56 59 61 63 66)))
((e (6 7+ 9))        ((40 56 59 61 63 66)))
((bb (7+))           ((46 57 60 62 65)))
((bb (6 7+ 9))       ((46 57 60 62 67) (46 55 57 60 62)))
((e (7+ 9))          ((40 56 59 61 63 66)))
((bb (7+ 9))         ((46 57 60 62 67) (46 55 57 60 62)))

((e (7 9+ 13-))      ((40 56 60 62 67) (40 55 56 60 62)))
((bb (7 9+ 13-))     ((46 56 61 62 66)))
((e (6 7 9))         ((40 61 62 67 69)))
((bb (6 7 9))        ((46 55 56 60 62)))
((e (6 7 9 11+))     ((40 56 58 62 66)))
((bb (6 7 9 11+))    ((46 56 60 62 64 67)))


((e (- 5-))          ((40 58 62 67 70)))
((bb (- 5-))         ((46 56 61 64 68)))
((e (- 5- 9))        ((40 58 62 67 70)))
((bb (- 5- 9))       ((46 56 61 64 68)))

((e (5-))            ((40 58 62 68 70)))
((bb (5-))           ((46 56 62 64 68)))

((e (- 7+ 9))        ((40 55 59 63 66)))
((bb (- 7+ 9))       ((46 57 61 65 69)))

((e (5+ 7+ 9))       ((40 56 60 63 66)))
((bb (5+ 7+ 9))      ((46 57 62 66 69)))

; triad superposition
((e (/ 4 (7)))       ((40 56 60 63 66)))
((bb (/ 4 (7)))      ((46 57 60 62 66)))


))

(setf beatshermeto (make-beats-from-voicings voicingshermeto))

(setf oraclevoicings 
      (loop for i from 0 to (1- (length beatshermeto)) with o = (NewImprovizer) do (learn-event o (nth i beatshermeto)) finally return o))


;ORACLE FOR HARMONIZING A GIVEN MELODY
;-------------------------------------
;See Hamonization.lisp for a complete description

;ATTENTION : MODIFICATION POUR CONSERVER LES LABELS (28 JUIN) ???????????????????????????????????????????????
(defun harmonize-hermeto (solobeatlist beatdur)
  (let* ((melolist (mapcar 'MeloSignature (beats->melobeats (thread-Beats solobeatlist))))
         (melobeatres (ImprovizeOnHarmGrid oraclemelocalendario (length melolist) melolist))
         (labels (loop for x in (mapcar 'HarmLabel melobeatres) collect (if (numberp (first x)) nil x)))
         (harmo (ImprovizeOnHarmGrid oraclevoicings (length labels) labels))
         (harmochseq nil))
    (setf harmochseq (beats->chseq harmo 500 0))
    (setf (Lchan harmochseq) (mapcar #'(lambda (x) (substitute 2 1 x)) (Lchan harmochseq)))
;    (mf-info->chord-seq (timestretch (chord-seq->mf-info harmochseq) (/ beatdur 500)))))
    (list (mf-info->chord-seq (timestretch (chord-seq->mf-info harmochseq) (/ beatdur 500))) labels)))



(setf roundmelo   ; for "Round Midnight" (8 first bars)
'(((eb m7) nil) ((eb m7) ((58 -2 269 110 1) (63 285 251 110 1) (65 567 269 110 1) (70 828 265 110 1))) ((eb m7) ((66 -5 1085 110 1))) ((eb m7) ((-66 0 520 110 1) (58 534 555 110 1))) ((eb m7) ((63 -3 1088 110 1))) ((eb m7) ((65 368 177 110 1) (63 535 178 110 1) (62 717 202 110 1) (63 912 178 110 1))) ((eb m7) ((-63 0 7 110 1) (70 259 244 110 1) (70 749 146 110 1) (71 916 136 110 1))) ((eb m7) ((70 15 240 110 1) (68 296 230 110 1) (68 558 538 110 1))) ((eb m7) nil) ((eb m7) ((63 5 282 110 1) (66 311 306 110 1) (70 570 270 110 1) (73 816 271 110 1))) ((eb m7) ((72 0 624 110 1))) ((eb m7) ((63 650 447 110 1))) ((eb m7) ((-63 0 29 110 1) (69 5 278 110 1) (69 284 248 110 1))) ((eb m7) ((68 2 283 110 1) (68 300 290 110 1))) ((eb m7) ((68 504 305 110 1) (68 817 263 110 1))) ((eb m7) ((-68 0 19 110 1) (67 12 619 110 1))) ((eb m7) nil) ((eb m7) ((68 0 308 110 1) (71 308 289 110 1) (75 551 274 110 1) (78 832 251 110 1))) ((eb m7) ((77 8 1072 110 1))) ((eb m7) ((-77 0 615 110 1) (71 858 239 110 1))) ((eb m7) ((-71 0 4 110 1) (70 4 1084 110 1))) ((eb m7) ((-70 0 1091 110 1))) ((eb m7) ((-70 0 1080 110 1))) ((eb m7) ((-70 0 24 110 1) (63 572 283 110 1) (65 847 260 110 1))) ((eb m7) ((66 -18 165 110 1) (68 139 141 110 1) (65 312 447 110 1) (66 774 260 110 1))) ((eb m7) ((66 1 745 110 1) (65 765 155 110 1) (66 902 195 110 1))) ((eb m7) ((65 -12 1092 110 1))) ((eb m7) ((-65 0 715 110 1) (63 725 373 110 1))) ((eb m7) ((-63 0 6 110 1) (62 -3 644 110 1) (74 680 214 110 1) (71 894 193 110 1))) ((eb m7) ((-71 0 12 110 1) (70 12 371 110 1) (67 383 363 110 1) (63 738 347 110 1))) ((eb m7) ((60 1 756 110 1) (56 753 182 110 1) (53 939 161 110 1)))))

(setf roundbeatlist (make-beat-list roundmelo))

(setf roundbeatdur 1090)



#|
;Evaluate the following example to get an harmonization of the melody of "Round Midnight" (8 first bars):

(progn (pgmout 4 1) (pgmout 4 2) 
(setf impro (merger (beats->chseq (thread-Beats roundbeatlist) roundbeatdur 0)     ;melody
                    (first (harmonize-hermeto roundbeatlist roundbeatdur))))       ;harmonisation
(play impro)
)

(Stop-Player *general-player*)
(my-save-as-midi impro roundbeatdur)

;Another example harmonizing a solo phrase by Bernard Lubat on "Zieste zeste":

(progn (pgmout 4 1) (pgmout 4 2) 
(setf impro (merger (beats->chseq (thread-Beats zistebeatlist) zistebeatdur 0)     ;melody
                    (first (harmonize-hermeto zistebeatlist zistebeatdur))))       ;harmonisation
(play impro)
)

;'harmonize-hermeto' calls for 'ImprovizeOnHarmGrid' with a specific sequence 'melolist':

(setf melolist '((71 70) (76 68) (87 82)))
(MeloSignature (otext oraclemelocalendario 1))
(eligible-beat? (otext oraclemelocalendario 1) '(72 73))
(eligible-beat? (otext oraclemelocalendario 1) '(72 74))

(setf melobeatres (ImprovizeOnHarmGrid oraclemelocalendario (length melolist) melolist))
(mapcar 'HarmLabel melobeatres)
(mapcar 'HarmLabel melobeatcalendario)

;Use ImprotekTutorial.lisp (EXAMPLE #12) to load a MIDI file containing a melody and to define 'beatlist' and 'beatdur':
(setf midifromfile (evts-from-midifile)) 
(progn
(setf res1 (check-clocks midifromfile) 
      clocksfromfile (first res1) quintuplesfromfile (second res1) 
      defaultbeatdur (round (om-mean (x->dx (mapcar 'first clocksfromfile)))))
(setf beatsfromfile2 (quintuples->beats clocksfromfile quintuplesfromfile))
(setf beatsfromfile3 (cut-beat-events clocksfromfile beatsfromfile2))           
(setf beatsfromfile4 (set-relative-time-beat clocksfromfile beatsfromfile3))
(setf beatsfromfile (label-chord-beat beatsfromfile4))
)

(setf beatlist (make-beat-list beatsfromfile) beatdur defaultbeatdur)

(setf melolist (mapcar 'MeloSignature (beats->melobeats (thread-Beats beatlist))))


(setf melobeatres (ImprovizeOnHarmGrid oraclemelocalendario (length melolist) melolist))
(setf labels (loop for x in (mapcar 'HarmLabel melobeatres) collect (if (numberp (first x)) nil x)))

(play (beats->chseq (ImprovizeOnHarmGrid oraclevoicings (length labels) labels) beatdur 0))


|#


(setf zistemelo
'(((eb m7) ((60 -5 99 1 1) (58 189 76 98 1) (64 308 146 86 1))) ((eb m7) ((65 46 100 76 1) (58 296 100 83 1))) ((eb m7) ((64 9 182 103 1) (65 226 93 68 1))) ((eb m7) ((64 -80 212 88 1))) ((eb m7) ((70 305 123 123 1))) ((eb m7) ((70 296 70 97 1))) ((eb m7) ((76 18 223 121 1) (77 295 64 106 1))) ((eb m7) ((76 29 305 118 1) (77 319 70 117 1))) ((eb m7) ((68 36 111 65 1) (56 288 147 85 1) (62 295 200 100 1) (77 301 123 113 1))) ((eb m7) ((67 40 164 45 1) (68 222 118 74 1) (77 447 102 113 1))) ((eb m7) ((-77 0 15 113 1) (191 -8 29 113 1) (62 134 111 68 1) (56 134 129 71 1) (67 146 93 93 1) (68 258 152 77 1))) ((eb m7) ((56 15 134 61 1) (62 15 140 64 1) (77 22 128 108 1) (67 281 177 88 1) (68 433 75 83 1))) ((eb m7) ((77 6 134 100 1) (67 171 134 72 1) (68 265 140 78 1))) ((eb m7) ((77 17 140 113 1) (56 23 140 63 1) (62 23 118 66 1) (67 295 105 75 1))) ((eb m7) ((68 8 176 113 1) (56 8 181 63 1) (62 8 253 68 1) (77 213 124 103 1))) ((eb m7) nil) ((eb m7) ((67 196 70 61 1) (73 333 134 89 1))) ((eb m7) ((67 -39 24 60 1) (77 32 140 100 1) (79 321 164 114 1))) ((eb m7) ((73 44 164 98 1) (77 227 129 82 1))) ((eb m7) ((73 -86 88 101 1) (61 -86 140 74 1) (55 -86 129 72 1) (75 180 118 108 1) (72 391 81 78 1) (73 444 101 71 1))) ((eb m7) ((-73 0 64 71 1) (55 52 479 52 1) (61 58 473 72 1) (72 100 431 72 1))) ((eb m7) ((-55 0 28 52 1) (-61 0 27 72 1) (-72 0 28 72 1) (213 -1 540 52 1) (207 -1 540 72 1) (196 -1 540 72 1))) ((eb m7) ((-213 0 32 52 1) (-207 0 32 72 1) (-196 0 32 72 1) (213 -4 535 52 1) (207 -4 535 72 1) (196 -4 535 72 1))) ((eb m7) ((-213 0 31 52 1) (-207 0 31 72 1) (-196 0 31 72 1) (213 1 330 52 1) (207 1 501 72 1) (196 1 283 72 1))) ((eb m7) ((63 52 134 71 1) (57 57 123 82 1) (62 57 111 93 1) (67 57 170 86 1) (68 57 123 108 1) (69 181 135 70 1) (72 359 88 91 1))) ((eb m7) ((62 283 134 101 1) (77 289 105 105 1) (67 301 151 72 1) (56 301 129 86 1))) ((eb m7) ((67 24 93 74 1) (73 140 124 93 1) (74 266 117 117 1) (79 429 101 103 1))) ((eb m7) ((-79 0 16 103 1) (74 -1 129 97 1) (77 176 117 108 1) (76 288 123 91 1))) ((eb m7) ((75 -95 105 55 1) (70 306 123 123 1))) ((eb m7) ((58 -72 159 76 1) (67 -60 177 81 1) (62 -60 152 74 1) (56 -60 147 76 1))) ((eb m7) ((67 19 93 74 1) (73 135 124 93 1) (74 261 117 117 1) (79 424 107 103 1))) ((eb m7) ((-79 0 10 103 1) (74 -7 129 97 1) (77 170 117 108 1) (76 282 123 91 1))) ((eb m7) ((75 -99 105 55 1) (71 7 170 78 1) (73 154 111 93 1) (74 272 146 108 1))) ((eb m7) ((76 -87 75 98 1) (77 13 188 106 1) (72 201 70 69 1) (75 314 182 114 1))) ((eb m7) ((73 -74 41 63 1) (74 44 129 108 1) (73 174 140 93 1) (72 322 105 82 1))) ((eb m7) ((70 -83 100 120 1) (68 28 118 91 1) (65 181 81 64 1) (63 282 123 111 1))) ((eb m7) ((61 -95 105 77 1) (72 -1 111 91 1) (82 134 147 121 1) (83 258 135 95 1) (79 394 137 88 1))) ((eb m7) ((-79 0 3 88 1) (80 22 253 103 1))) ((eb m7) ((213 -1 532 65 1) (208 -1 532 64 1) (203 -1 532 63 1) (207 -1 532 63 1))) ((eb m7) ((-213 0 35 65 1) (-208 0 35 64 1) (-203 0 35 63 1) (-207 0 35 63 1) (213 4 246 65 1) (208 4 236 64 1) (203 4 260 63 1) (207 4 260 63 1))) ((eb m7) ((58 173 140 91 1) (74 296 164 89 1))) ((eb m7) ((68 -70 88 54 1) (82 30 164 120 1) (68 207 83 45 1) (80 302 216 117 1))) ((eb m7) ((79 33 100 110 1) (80 162 93 113 1) (79 174 29 81 1) (79 286 140 86 1))) ((eb m7) ((77 -92 81 81 1) (79 14 170 92 1) (77 150 158 95 1) (76 298 100 89 1)))))

(setf zistebeatlist (make-beat-list zistemelo))

(setf zistebeatdur 536)




;DATA FROM HERMETO PASCOAL'S "CALENDARIO DO SOM"
;-----------------------------------------------
;Without barlines, durations indicated as numbers of beats
;Notes of the melody as MIDI codes

(setf Calendario-do-Som   
'(

(23-Outubro-1996    ; 4 beats

 (bb (- 4 7 9) 2 (72 73 75)) (a (7 9+ 13-) 2 (77 75 73))
 (f# (- 4 7 9) 2 (69 76 75)) (f (7 9+ 13-) 2 (73 71 63))
 (e (7+) 2 (71 70 71 78)) (c# (- 4 7 9) 2 (76 74 68 71))
 (bb (- 4 7 9) 2 (72 73 75 80)) (a (7+) 2 (68 76 78 73))
 (a (- 4 7 9) 2 (74 79 78 76 71 74)) (ab (7 9+ 13-) 2 (76 74 83 86))
 (g (7 9+ 13-) 2 (87 82 75 77 73)) (c (7+) 2 (78 79 81 79 74 76 69 71))
 (g# (- 4 7 9) 2 (73 75 82 78)) (bb (- 4 7 9) 1 (85)) (a (7 9+ 13-) 1 (84 77))     ; 13 or 13- ???
 (d (7+) 2 (80 73)) (b (- 4 7 9) 2 (76 81))
 (ab (- 5- 9) 2 (77 74 71 70 65 66 68 73)) (b (- 5- 9) 2 (76 74 80 81 73 79 76 77))
 (bb (7+) 2 (69 76 74)) (ab (7+) 2 (67 74 72))
 (a (5-) 2 (77 75 74 71)) (ab (5-) 2 (73 66))
 (g (- 5-) 2 (70 69 65 72)) (f# (- 5-) 2 (71 74 72 80))
 (a (- 4 7 9) 2 (79 83 76 74 72 71 67 74)) (e (7+) 2 (71 80 73 78 75 70 68 66))
 (d# (- 4) 2 (73 78 77)) (d (7+) 2 (76 73 69))
 (db (7+) 1 (68 79 76 77)) (bb (- 4 7 9) 1 (65 67 75 72)) (c (7+) 1 (74 71 78 76)) (c (/ 4 (7)) 1 (75 74 64 67)) ;superposition of triades C / E7    
 (c# (- 5-) 2 (71 81 78 79)) (c (- 5-) 1 (80 77)) (f (7 9+ 13-) 1 (78 71))  ; repeat

 (bb (7+) 2 (77)) (g (- 4 7 9) 2 (72 69 64 65 74))
 (eb (- 4 7 9) 2 (73 70 68 66)) (b (7+) 2 (77 80 75 65 66 74))
 (ab (- 4 7 9) 2 (70 73)) (g (- 4 7 9) 2 (72 77))
 (a (- 4 7 9) 1 (74 71)) (b (- 4 7 9) 1 (76 73))
 (bb (- 4 7 9) 4 (75))
)
 

;))

;'((
(02-Dezembro-1996    ; 4 beats

 (d (- 4 7 9) 2) (g (6 7 9) 2)

; begin repeat
 (d (- 4 7 9) 4)
 (db (7 9+ 13-) 4)
 (c (- 4 7 9) 4) 
 (b (7 9+ 13-) 4)
 (bb (- 4 7 9) 2) (a (7 9+ 13-) 2)
 (ab (7 9+ 13-) 2) (f (7 9+ 13-) 2)
 (e (7+) 2) (eb (7 9+ 13-) 2)
 (c (7+) 2) (b (- 4 7 9) 2)
 (a (- 4 7 9) 2) (ab (7 9+ 13-) 2)
 (g (7+) 2) (eb (7+) 1) (c (- 4 7 9) 1)
 (e (- 4 7 9) 2) (f# (- 4 7 9) 2)
 (g (- 4 7 9) 2) (bb (7+) 1) (eb (7+) 1)

; 1st time
 (d (- 4 7 9) 2) (g (6 7 9 11+) 1) (c# (7 9+ 13-) 1)  ; repeat

; 2nd time
 (d (7+) 2) (c# (7 9+ 13-) 2)
 (c (7+) 2) (f (7+) 2)
 (e (- 4 7 9) 2) (eb (7+) 2)
 (d (- 4 7 9) 2) (ab (7+) 2)
 (c (7+) 2) (eb (7+) 2)
 (f (7+) 2) (db (7+) 2)
 (e (7+) 4)
)


(26-Dezembro-1996    ; 2 beats

 (g (7+) 1) (c (7+) 1)
 (b (- 4 7 9) 1) (e (7 9+ 13-) 1)
 (a (6 7 9) 1) (ab (7 9+ 13-) 1)
 (f (6 7 9) 1) (e (7 9+ 13-) 1)
 (c# (7 9+ 13-) 1) (c (6 7+ 9) 1/2) (c (5+ 7+ 9) 1/2)
 (a (- 4 7 9) 1) (f (- 4 7 9) 1/2) (e (7 9+ 13-) 1/2)
 (eb (7+) 1) (d (7+) 1)
 (c# (7+) 1) (e (7+) 1)
 (eb (- 7+ 9) 1) (c (- 7+ 9) 1)
 (ab (7+) 1) (a (- 4 7 9) 1)
 (e (- 4 7 9) 1) (c (7+) 1/2) (a (- 4 7 9) 1/2)
 (ab (7+) 1/2) (f (- 4 7 9) 1/2) (d (- 5-) 1/2) (g (7 9+ 13-) 1/2)
 (c (7+) 1) (ab (7+) 1)
 (a (- 5- 9) 1) (d (7 9+ 13-) 1)
 (g (7+) 1) (c# (- 4 7 9) 1/2) (f# (6 7 9 11+) 1/2)   ; repeat
 
 (c (7+) 1) (eb (7+) 1)
 (d (7+ 9) 1) (c# (7+ 9) 1)
(c (7+) 2)
)


(23-Junho-1997     ; 2 beats

 (bb (- 4 7 9) 1) (gb (6 7 9) 1)

; begin repeat
 (eb (- 4 7 9) 2)     
 (db (6 7+ 9) 2)
 (eb (6 7 9) 1) (f# (6 7+ 9) 1)
 (g (7 9+ 13-) 1) (ab (- 4 7 9) 1)
 (bb (- 4 7 9) 1) (f# (6 7+ 9) 1/2) (eb (- 4 7 9) 1/2)
 (bb (- 4 7 9) 1) (f# (6 7+ 9) 1)
 (bb (- 4 7 9) 1/2) (eb (6 7 9) 1/2) (c (7 9+ 13-) 1/2) (b (6 7+ 9) 1/2)
 (db (6 7+ 9) 1) (e (7 9+ 13-) 1/2) (a (7 9+ 13-) 1/2)
 (ab (6 7+ 9) 1) (db (6 7+ 9) 1)
 (a (7 9+ 13-) 1) (bb (- 4 7 9) 1)
 (g (- 4 7 9) 1) (f# (6 7+ 9) 1/2) (a (7 9+ 13-) 1/2)
 (bb (- 4 7 9) 1) (c (- 4 7 9) 1)
 (db (6 7+ 9) 1) (d (7 9+ 13-) 1)    ; repeat

 (b (- 4 7 9) 1) (c (- 4 7 9) 1)
 (db (6 7+ 9) 1) (d (- 4 7 9) 1)
 (e (- 4 7 9) 1) (g (- 4 7 9) 1)
 (bb (- 4 7 9) 1) (a (7 9+ 13-) 1)
 (bb (- 4 7 9) 1) (a (7 9+ 13-) 1/2) (d (7 9+ 13-) 1/2)
 (db (6 7+ 9) 1) (c (6 7+ 9) 1)
)


))


;for improvising chord sequences:
(setf chordcalendario (make-chordlist Calendario-do-Som))

(setf oraclecalendario
      (loop with o = (NewImprovizer) for i from 0 to (1- (length chordcalendario)) do (learn-event o (nth i chordcalendario)) finally return o))

;for harmonizing a given melody:
(setf melobeatcalendario (make-melobeatlist Calendario-do-Som))

(setf oraclemelocalendario
      (loop with o = (NewImprovizer) for i from 0 to (1- (length melobeatcalendario)) do (learn-event o (nth i melobeatcalendario)) finally return o))








