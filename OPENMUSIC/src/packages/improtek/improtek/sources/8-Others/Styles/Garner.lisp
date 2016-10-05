(in-package :om)

;Garner.lisp
;-----------
;Marc Chemillier
;December 10th 2010, last modified January 2012

;Garner's dotted quarter note accompaniments are computed at about BPM=250, but on a double grid,
;thus the resulting beat list must be slowed down to half tempo BPM=125 (beats are grouped by 2)

(defun doublegrid (grid) (loop for x in grid append (list x x)))

(defun halftempo (beatlist beatdur) 
  (loop for l = beatlist then (cddr l) until (null l) 
        collect (let ((newbeat (clone-object (first l))) (shiftedmidiset nil))
                  (when (second l) 
                    (progn (setf shiftedmidiset (loop for x in (Midiset (second l)) 
                                                      collect (append (list (first x) (+ (second x) beatdur)) (cddr x))))
                      (setf (Midiset newbeat) (append (Midiset newbeat) shiftedmidiset))))
                  newbeat)))

(setf labels-garner 
  (doublegrid '((d m7) (d m7) (d m7) (d m7) (g 7) (g 7) (g 7) (g 7) (c maj7) (c maj7) (c maj7) (c maj7) (a 7) (a 7) (a 7) (a 7))))


(setf beatdur 236)  ;BPM=254 Garner dotted quarter left hand is better dealt with at a BPM around 250         
;(setf beatdur 472) ;BPM=117 --> but in Max, it should be played half tempo at a BPM around 125


(defun make-clocks (nbeats beatdur &optional d)
  (let ((q (if (null d) 1 d)))
    (make-beat-list (make-list (div nbeats q) :initial-element (list (list 'nolabel 'nolabel q) (list (list 56 0 beatdur 100 10)))))))

(setf beatlistdrums (make-clocks (* 2 (length labels-garner)) beatdur 2))  ; percu: 1=quarter notes, 2=half notes

#|
;Improvisation with 2 oracles (1 for the left hand + 1 for the right hand)
;Left hand oracle can recombine freely with a long continuity so that dotted quarter notes are regular

(progn (Stop-Player *general-player*) (pgmout 4 1) (pgmout 4 2) 
(setf (max-continuity garnerrightoracle) 5
      (max-continuity garnerleftoracle) 1000)
(set-start-point garnerrightoracle 50)             ; --;MARC 10/2/2012 generic function that works with 'beats', but also with objects of specific classes ('melobeats', 'relativechords', ...)
;you need to redefine the following functions: 
;- TransposeClonedBeat ((self beat) int)
;- eligible-beat? ((self beat) (label list))
;- CompareEvents ((Event1 beat) (event2 beat))
;- clone-object ((self beat))
;when using label objects instead of simple lists ('garnerlabel'...):
;- TransposeCloneLabel ((label list) int)  
;- FormatLabel ((label list))
;- undefined-label? ((label list))
;MARC 10/2/2012 generic function that works with 'beats', but also with objects of specific classes ('melobeats', 'relativechords', ...)
;you need to redefine the following functions: 
;- TransposeClonedBeat ((self beat) int)
;- eligible-beat? ((self beat) (label list))
;- CompareEvents ((Event1 beat) (event2 beat))
;- clone-object ((self beat))
;when using label objects instead of simple lists ('garnerlabel'...):
;- TransposeCloneLabel ((label list) int)  
;- FormatLabel ((label list))
;- undefined-label? ((label list))

;--> continuations-by-suppleance, find-beat-label-match, choose-factor-link uses these functions only
;--> continuations-by-suppleance, find-beat-label-match, choose-factor-link uses these functions only-----> change this value to obtain various improvisations
(setf labels1 (append labels-garner labels-garner)) ; labels-garner labels-garner)) 
(format *om-stream* "-----------------------~%LEFT HAND~%")
(setf leftbeatlist (thread-Beats (ImprovizeOnHarmGrid garnerleftoracle (length labels1) labels1)))
(setf impro (merger (beats->chseq leftbeatlist beatdur 0) 
                    (beats->chseq beatlistdrums beatdur 0))) 
(format *om-stream* "-----------------------~%RIGHT HAND~%")
(setf rightbeatlist (thread-Beats (ImprovizeOnHarmGrid garnerrightoracle (length labels1) labels1)))
(setf impro (merger (beats->chseq rightbeatlist beatdur 0)
                    impro))
(play impro)
)


(Stop-Player *general-player*)
(my-save-as-midi impro beatdur)
;MARC 10/2/2012 generic function that works with 'beats', but also with objects of specific classes ('melobeats', 'relativechords', ...)
;you need to redefine the following functions: 
;- TransposeClonedBeat ((self beat) int)
;- eligible-beat? ((self beat) (label list))
;- CompareEvents ((Event1 beat) (event2 beat))
;- clone-object ((self beat))
;when using label objects instead of simple lists ('garnerlabel'...):
;- TransposeCloneLabel ((label list) int)  
;- FormatLabel ((label list))
;- undefined-label? ((label list))

;--> continuations-by-suppleance, find-beat-label-match, choose-factor-link uses these functions only
(maxetat garnerrightoracle)

(setf labels-garner '((ab 7) (ab 7) (ab 7) (ab 7) (ab 7) (ab 7) (ab 7) (ab 7)
                      (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7)
                      (c m7) (c m7) (b m7) (b m7) (bb m7) (bb m7) (eb 7) (eb 7)
                      (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7)))

(setf labels-garner '((ab 7) (ab 7) (ab 7) (ab 7) (ab 7) (ab 7) (ab 7) (ab 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7)
                      (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (b m7) (b m7) (bb m7) (bb m7) (eb 7) (eb 7)
                      (ab maj7) (ab maj7) (ab maj7) (ab maj7) (g m7) (g m7) (g m7) (g m7) (f m7) (f m7) (f m7) (f m7) (bb 7) (bb 7) (bb 7) (bb 7)
                      (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7)))


;Save impros into the default file given by 'path_bufferAntescofo':
;/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/buffer-for-antescofo.txt
;Beats are grouped by two using 'halftempo' in order to fit the double beat duration (BPM around 125 instead of 250) 

(save-for-antescofo (loop for x1 in (halftempo rightbeatlist) for x2 in (halftempo leftbeatlist)     ;;;;;; mix of both hands
                          for beat = (clone-object x1)
                          do (setf (MidiSet beat) (sort (append (MidiSet beat) (MidiSet x2)) #'<= :key #'second))
                          collect beat)
                    (* 2 beatdur)) 


(setf beatlistdrums   ;beatdur=472, BPM=117
  (let* ((2beats 
         '((nolabel nolabel 2) ((36 0 229 80 10) (42 236 229 80 10) (36 472 229 80 10) (40 472 229 80 10) (42 708 229 80 10))))
         (8bars (list 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats 2beats)))
    (make-beat-list (append 8bars 8bars) 472)))

|#




(defclass* garnerbeat (beat)
  (
   (Garnerposition :initform 0 :initarg :Garnerposition :accessor Garnerposition) 
   ))            ;Garnerposition = 0 ("downbeat"), 1 ("syncopated"), 2 ("silence")    [X -] [- X] [- -]


(defclass* garnerlabel ()
  (
   (HarmLabel :initform () :initarg :HarmLabel :accessor HarmLabel)
   (Garnerposition :initform 0 :initarg :Garnerposition :accessor Garnerposition) 
   ))            ;Garnerposition = 0 ("downbeat"), 1 ("syncopated"), 2 ("silence")    [X -] [- X] [- -]


(defmethod TransposeCloneLabel ((label garnerlabel) int)  
  (let ((Clonedlabel (clone label)))
    (setf (harmlabel Clonedlabel) (cons (TransposeRoot (first (harmlabel Clonedlabel)) int) (rest (harmlabel Clonedlabel))))
    Clonedlabel))

(defmethod FormatLabel ((label garnerlabel)) (harmlabel label))


; for learning
(defmethod CompareEvents ((Event1 garnerbeat) (Event2 garnerbeat)) 
  (and (equal (Garnerposition event1) (Garnerposition event1))
       (or (equal (harmlabel event1) (harmlabel event2))   ;;(RefHarmlabel event1) (RefHarmlabel event2))
           (equalLabel (harmlabel event1) (harmlabel event2)) )))

; for improvising
(defmethod eligible-beat? ((self garnerbeat) (label garnerlabel))   ;label = (root quality garnerposition) Ex: (d m7 0)
  (or (null label) 
      (and (equal (garnerposition self) (garnerposition label))
           (equalLabel (harmlabel self) (harmlabel label) ))))

; using general 'beat' methods applied to 'garnerbeat':
;(defmethod TransposeClonedBeat ((self garnerbeat) int)


;For Garner's data, rhythmic positions are marked on channel 11 (MIDI code 60=downbeat, 61=syncopated, 62=silence)
(defun transform-to-garnerbeat (beatlist)
  (loop for beat in beatlist 
        for newbeat = (make-instance 'garnerbeat :harmlabel (harmlabel beat) :duration (duration beat))
        do (setf (Midiset newbeat) (loop for x in (Midiset beat)
                                         if (= (fifth x) 11) do (setf (Garnerposition newbeat) (mod (first x) 12))
                                         else collect x))
        collect newbeat))



#|
;Test with 1 double oracle (2 voice data, each hand on 1 channel)
;PROBLEM: the left hand appears to be quite irregular (the dotted quarter notes are disturbed by the recombination process)
;-> left hand must be recombined in a separate way, with a long continuity

(setf garnerdoubleoracle (NewImprovizer (remove-channels garner-beatlist '(11)) refbeatdur))


(progn (pgmout 2 1) (setf (max-continuity garnerdoubleoracle) 5 
                          labels1 (append labels-garner labels-garner labels-garner labels-garner)) 
(setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid garnerdoubleoracle (length labels1) labels1)) beatdur 0) 
                    (beats->chseq (make-clocks (length labels1) beatdur 2)  ; percu: 1=quarter notes, 2=half notes 
                                  beatdur 0)))
(play impro)
)

(Stop-Player *general-player*)
(my-save-as-midi impro beatdur)


;Test with 2 oracles (1 for each hand, but the left hand oracle takes care of the positions of the dotted quarter notes)
;-> left hand follows the succession of rhythmic positions: "downbeat" -> "syncopated" -> "silence"  [X -] [- X] [- -]
;-> labels are converted into a specific format taking into account the positions
;-> a specific subclass 'garnerbeat' of 'beat' is created to adapt the 'CompareEvents' and 'eligible-beat?' functions
;PROBLEM: one cannot have a long continuity because the original data do not follow a strict succession [X -] [- X] [- -]

(setf garnermarkedleftoracle (NewImprovizer (transform-to-garnerbeat (remove-channels garner-beatlist '(1))) refbeatdur))


(progn (pgmout 2 1) (pgmout 2 2) (setf (max-continuity garnerrightoracle) 10) (setf (max-continuity garnermarkedleftoracle) 1000)
(set-start-point garnerrightoracle 200)
(setf labels1 (append labels-garner labels-garner labels-garner labels-garner)) (setf labels2 (make-garnerlabels labels1))
(setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid garnermarkedleftoracle (length labels2) labels2)) beatdur 0) 
                    (beats->chseq (make-clocks (length labels2) beatdur 2)  ; percu: 1=quarter notes, 2=half notes 
                                  beatdur 0)))
(setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid garnerrightoracle (length labels1) labels1)) beatdur 0)
                    impro))
(play impro)
)

(Stop-Player *general-player*)
(my-save-as-midi impro beatdur)


(harmlabel (nth 0 labels2))
(setf l '((c 7) (g 7)))
(setf g (make-garnerlabels l))
(harmlabel (nth 1 g))
(harmlabel (TransposeCloneLabel (nth 0 labels2) -1))


;With an audio percussion accompaniement (bass note = C):
(setf percu (get-sound))         ; choose the file name: '127.32.TeaForTwoLoop.wav'
(play percu)
(Stop-Player *general-player*)

;WARNING: You need to load the soundfile '127.32.TeaForTwoLoop.wav' before playing this example
(progn (Stop-Player *general-player*) (pgmout 2 1) (setf (max-continuity garnerdoubleoracle) 200) (setf labels (append labels-garner labels-garner))
(setf impro (beats->chseq (thread-Beats (ImprovizeOnHarmGrid garnerdoubleoracle (length labels) labels)) beatdur 0))
(play (list percu impro))
)

|#


;Transformation of the grid into a list of 'garnerlabels':
(defun make-garnerlabels (grid)
  (let ((garnergrid (loop for x in grid for i = 0 then (incf i) 
                          collect (make-instance 'garnerlabel :harmlabel x :garnerposition (mod i 3)))))
    (when (= (garnerposition (car (last garnergrid))) 1) (setf (garnerposition (car (last garnergrid))) 0))
    garnergrid))



;DATA FROM TRANSCRIPTIONS OF ERROLL GARNER'S IMPROVISATIONS
;----------------------------------------------------------

#|
;Opens a MIDI file and gives a pair with 
;- a list of pairs labels+mididata 
;- a beatduration

(midi-to-beats)


;MIDI data are taken from transcriptions of Erroll Garner's solos, and stored in variables 
;'beatsfromfile-twohands', 'beatsfromfile-righthand', 'beatsfromfile-lefthand' all adapted to BPM = 254, beatdur = 236 (half BPM = 127, beatdur = 472)
;+ in 'beatsfromfile-lefthand' additional informations on the type of beats: "downbeat"=0, "syncopated"=1, "silence"=2  [X -] [- X] [- -]
;coded as MIDI notes 60, 61, 62, on channel 11

;You can play the resulting beatlist in order to check the midi data, and test 'halftempo':
(progn (pgmout 1 1) (ctrlchg 7 127 10)
(setf beatlist (make-beat-list beatsfromfile-twohands))
(setf halfbeatlist (halftempo beatlist beatdur))
(play (merger (beats->chseq (thread-Beats halfbeatlist) (* 2 beatdur) 0)  
              (beats->chseq (make-clocks (length halfbeatlist) (* 2 beatdur) 1)  ; percu: 1=quarter notes, 2=half notes 
                            (* 2 beatdur) 0))))

(Stop-Player *general-player*)

(mapcar 'HarmLabel beatlist)

(om-inspect garnermarkedleftoracle)
(loop for x being the hash-key of (hashtransition garnermarkedleftoracle) using (hash-value q) 
      do (format *om-stream* "~a ---> ~a~%" x (reverse q)))
(loop for i from 1 to 100 do (format *om-stream* "~a " (append (Harmlabel (aref (vectext garnermarkedleftoracle) i)) (list (Garnerposition (aref (vectext garnermarkedleftoracle) i))))))
(loop for i from 1 to 100 do (format *om-stream* "~a " (Midiset (aref (vectext garnermarkedleftoracle) i))))

(Midiset (aref (vectext garnermarkedleftoracle) 2))

|#

;REMARK: anacrusis must be measured otherwise its notes are getting a negative MIDI code and are played as a chord

; right hand = channel 1
; left hand = channel 2
; 'mambo arithmetic' = channel 11 
; ---> gives the type of the beat: 0=onbeat, 1=syncopated, 2=silence
;double tempo around BPM=250 
;played in ImproteK with half tempo around BPM=125


(defun remove-channels (beatlist list-of-channels)
  (loop for beat in beatlist for newbeat = (clone beat)
        do (setf (MidiSet newbeat) 
                 (remove list-of-channels (MidiSet newbeat) 
                         :test #'(lambda (x y) (member y x)) :key 'fifth))
        collect newbeat))



(setf Garner-TeaForTwo_beatdur 236       ;December 2, 1971 (album 'Gemini'), part A B  (original BPM=254)
      Garner-TeaForTwo_beatsfromfile
'(
((d m7) ((64 1 106 27 2) (60 1 106 27 2) (53 1 106 27 2) (57 1 106 27 2) (60 30 179 100 11) (72 115 75 64 1))) ((d m7) ((61 26 184 100 11) (64 118 95 27 2) (53 118 95 27 2) (60 118 95 27 2) (57 118 95 27 2))) ((d m7) ((62 28 184 100 11))) ((d m7) ((64 2 82 27 2) (53 2 82 27 2) (60 2 82 27 2) (57 2 82 27 2) (60 31 179 100 11))) ((d m7) ((61 27 184 100 11) (64 111 79 27 2) (53 111 79 27 2) (60 111 79 27 2) (57 111 79 27 2))) ((d m7) ((62 28 184 100 11))) ((d m7) ((69 1 79 64 1) (60 1 83 27 2) (53 1 83 27 2) (64 1 83 27 2) (57 1 83 27 2) (60 31 179 100 11))) ((d m7) ((72 -3 87 64 1) (61 29 183 100 11) (64 116 98 27 2) (59 116 98 27 2) (57 116 98 27 2) (53 116 98 27 2))) ((g 7) ((71 2 78 64 1) (62 29 184 100 11))) ((g 7) ((64 2 94 27 2) (59 2 94 27 2) (57 2 94 27 2) (53 2 94 27 2) (60 32 180 100 11))) ((g 7) ((61 31 183 100 11) (64 121 98 27 2) (59 121 98 27 2) (57 121 98 27 2) (53 121 98 27 2))) ((g 7) ((62 30 184 100 11))) ((g 7) ((64 -2 106 27 2) (59 -2 106 27 2) (57 -2 106 27 2) (53 -2 106 27 2) (60 27 180 100 11))) ((g 7) ((67 5 71 64 1) (61 24 183 100 11) (64 111 102 36 2) (59 111 102 36 2) (57 111 102 36 2) (53 111 102 36 2))) ((g 7) ((68 -3 75 64 1) (62 27 184 100 11) (69 111 67 64 1))) ((g 7) ((64 -1 106 36 2) (59 -1 106 36 2) (57 -1 106 36 2) (53 -1 106 36 2) (60 29 180 100 11) (72 126 75 64 1))) ((d m7) ((61 28 183 100 11) (53 119 106 36 2) (57 122 107 36 2) (64 122 107 36 2) (60 122 107 36 2))) ((d m7) ((62 27 184 100 11))) ((d m7) ((53 1 102 36 2) (57 5 102 36 2) (64 5 102 36 2) (60 5 102 36 2) (60 32 179 100 11))) ((d m7) ((61 29 183 100 11) (53 115 102 36 2) (57 119 102 36 2) (64 119 102 36 2) (60 119 102 36 2))) ((d m7) ((62 4 118 64 1) (62 28 184 100 11))) ((d m7) ((65 1 118 64 1) (53 1 98 36 2) (57 4 99 36 2) (64 4 99 36 2) (60 4 99 36 2) (60 32 179 100 11))) ((d m7) ((69 1 118 64 1) (61 29 184 100 11) (53 119 102 36 2) (72 123 113 64 1) (60 123 102 36 2) (57 123 102 36 2) (64 123 102 36 2))) ((d m7) ((-72 0 5 64 1) (62 30 183 100 11) (74 115 118 64 1))) ((g 7) ((53 1 98 36 2) (59 1 98 36 2) (64 5 98 36 2) (57 5 98 36 2) (60 32 179 100 11) (69 92 75 64 1))) ((g 7) ((70 -74 75 64 1) (71 -7 201 64 1) (61 32 183 100 11) (59 123 99 36 2) (53 123 99 36 2) (64 127 99 36 2) (57 127 99 36 2))) ((g 7) ((62 33 184 100 11))) ((g 7) ((53 2 98 36 2) (59 2 98 36 2) (64 6 98 36 2) (57 6 98 36 2) (60 33 179 100 11))) ((g 7) ((67 -2 119 64 1) (61 24 184 100 11) (53 118 98 36 2) (59 118 98 36 2) (57 122 98 36 2) (63 122 98 36 2))) ((g 7) ((62 24 184 100 11))) ((g 7) ((53 -2 99 36 2) (59 -2 99 36 2) (66 1 118 64 1) (57 2 99 36 2) (63 2 99 36 2) (60 30 179 100 11) (67 115 118 64 1))) ((g 7) ((66 0 118 64 1) (61 27 183 100 11) (67 114 118 64 1) (52 124 98 36 2) (59 124 98 36 2) (62 128 98 36 2) (55 128 98 36 2))) ((c maj7) ((62 28 184 100 11))) ((c maj7) ((52 2 98 36 2) (59 2 98 36 2) (62 6 98 36 2) (55 6 98 36 2) (60 27 180 100 11))) ((c maj7) ((67 -3 189 60 1) (70 1 87 64 1) (61 30 183 100 11) (71 92 102 64 1) (52 126 99 36 2) (59 126 99 36 2) (62 126 99 36 2) (55 126 99 36 2))) ((c maj7) ((62 29 184 100 11) (67 111 125 60 1) (70 115 86 64 1))) ((c maj7) ((-67 0 99 60 1) (71 -31 110 64 1) (52 0 98 36 2) (59 0 98 36 2) (62 0 98 36 2) (55 0 98 36 2) (60 28 180 100 11))) ((c maj7) ((67 -8 243 60 1) (70 -4 87 64 1) (61 28 183 100 11) (71 86 149 64 1) (52 113 99 36 2) (59 113 99 36 2) (62 113 99 36 2) (55 113 99 36 2))) ((c maj7) ((-67 0 92 60 1) (-71 0 96 64 1) (62 26 184 100 11))) ((c maj7) ((67 -4 63 64 1) (52 -1 99 36 2) (59 -1 99 36 2) (62 -1 99 36 2) (55 -1 99 36 2) (60 29 179 100 11))) ((f 7) ((61 27 184 100 11) (69 57 59 64 1) (62 123 98 36 2) (59 123 98 36 2) (55 123 98 36 2) (52 123 98 36 2))) ((f 7) ((62 27 183 100 11))) ((f 7) ((55 5 98 36 2) (52 5 98 36 2) (60 5 98 36 2) (60 28 179 100 11))) ((f 7) ((61 28 184 100 11) (55 115 99 36 2) (53 115 99 36 2) (63 115 99 20 2) (57 115 99 36 2))) ((f 7) ((62 17 184 100 11))) ((f 7) ((55 -5 98 36 2) (60 -5 98 36 2) (57 -5 98 36 2) (53 -5 98 36 2) (60 22 179 100 11) (62 106 118 64 1))) ((f 7) ((63 -5 118 64 1) (61 28 184 100 11) (64 113 118 64 1) (53 116 98 36 2) (60 116 98 36 2) (55 116 98 36 2) (57 116 98 36 2))) ((f 7) ((65 -10 118 64 1) (62 35 184 100 11) (66 104 119 64 1))) ((c maj7) ((67 -5 118 64 1) (52 2 99 36 2) (59 2 99 36 2) (62 2 99 36 2) (55 2 99 36 2) (60 38 179 100 11) (68 109 119 64 1))) ((c maj7) ((69 -5 118 64 1) (61 34 184 100 11) (70 109 118 64 1) (52 110 99 36 2) (59 116 98 36 2) (62 116 98 36 2) (55 116 98 36 2))) ((c maj7) ((71 -3 118 64 1) (62 36 184 100 11) (74 118 118 64 1))) ((c maj7) ((70 -1 118 64 1) (55 3 98 36 2) (59 3 98 36 2) (62 3 98 36 2) (52 3 98 36 2) (60 39 179 100 11) (71 94 142 64 1))) ((c maj7) ((-71 0 35 64 1) (74 -1 118 64 1) (61 36 184 100 11) (62 129 99 36 2) (59 129 99 36 2) (55 129 99 36 2) (52 129 99 36 2))) ((c maj7) ((70 -1 118 64 1) (62 36 183 100 11) (71 93 142 64 1))) ((c maj7) ((-71 0 27 64 1) (55 -4 98 36 2) (59 -4 98 36 2) (62 -4 98 36 2) (52 -4 98 36 2) (74 0 118 64 1) (60 40 179 100 11))) ((c maj7) ((70 -1 118 64 1) (61 36 184 100 11) (71 78 157 64 1) (55 109 99 36 2) (59 109 99 36 2) (62 109 99 36 2) (52 109 99 36 2))) ((a 7) ((-71 0 20 64 1) (62 38 184 100 11) (67 63 63 64 1))) ((a 7) ((69 -1 75 64 1) (55 -1 99 36 2) (65 -1 99 36 2) (61 -1 99 36 2) (57 -1 99 36 2) (60 40 179 100 11))) ((a 7) ((61 38 184 100 11) (55 119 98 36 2) (65 119 98 36 2) (61 119 98 36 2) (57 119 98 36 2))) ((a 7) ((62 39 184 100 11) (67 123 115 64 1))) ((a 7) ((-67 0 3 64 1) (68 -1 118 64 1) (61 -1 98 36 2) (57 -1 98 36 2) (65 -1 98 36 2) (55 -1 98 36 2) (60 30 179 100 11))) ((a 7) ((68 -2 118 64 1) (61 26 184 100 11) (55 120 99 36 2) (65 120 99 36 2) (61 120 99 36 2) (57 120 99 36 2))) ((a 7) ((69 2 118 64 1) (62 30 184 100 11) (70 116 118 64 1))) ((a 7) ((55 -94 99 68 2) (71 -3 118 64 1) (65 1 98 36 2) (61 1 98 36 2) (57 1 98 36 2) (72 34 179 100 11) (60 34 179 100 11))) ((d m7) ((72 2 118 64 1) (60 6 106 27 2) (53 6 106 27 2) (64 6 106 27 2) (57 6 106 27 2) (60 26 179 100 11))) ((d m7) ((61 22 184 100 11) (64 115 107 27 2) (53 115 107 27 2) (60 115 107 27 2) (57 115 107 27 2))) ((d m7) ((62 2 119 64 1) (62 23 184 100 11) (65 121 115 64 1))) ((d m7) ((-65 0 3 64 1) (60 3 106 27 2) (57 3 106 27 2) (53 3 106 27 2) (64 3 106 27 2) (60 27 179 100 11) (69 117 118 64 1))) ((d m7) ((61 23 184 100 11) (72 120 116 64 1) (60 128 106 27 2) (53 128 106 27 2) (64 128 106 27 2) (57 128 106 27 2))) ((d m7) ((-72 0 2 64 1) (62 24 183 100 11) (69 124 112 64 1))) ((d m7) ((-69 0 6 64 1) (72 2 118 64 1) (64 2 107 27 2) (57 2 107 27 2) (53 2 107 27 2) (60 2 107 27 2) (60 27 179 100 11))) ((d m7) ((74 2 119 64 1) (61 24 184 100 11) (57 117 106 27 2) (59 117 106 27 2) (64 117 106 27 2) (53 117 106 27 2))) ((g 7) ((70 3 118 64 1) (62 25 183 100 11) (71 66 170 64 1))) ((g 7) ((-71 0 3 64 1) (64 7 106 27 2) (59 7 106 27 2) (57 7 106 27 2) (53 7 106 27 2) (60 28 179 100 11) (67 129 106 64 1))) ((g 7) ((61 26 184 100 11) (64 126 106 27 2) (59 126 106 27 2) (57 126 106 27 2) (53 126 106 27 2))) ((g 7) ((79 1 118 64 1) (76 1 118 64 1) (62 26 184 100 11))) ((g 7) ((64 2 107 27 2) (59 2 107 27 2) (57 2 107 27 2) (53 2 107 27 2) (60 14 179 100 11))) ((g 7) ((67 -5 118 64 1) (61 10 184 100 11) (64 120 106 27 2) (59 120 106 27 2) (57 120 106 27 2) (53 120 106 27 2))) ((g 7) ((68 -2 119 64 1) (62 14 184 100 11) (69 113 118 64 1))) ((g 7) ((53 -71 107 46 2) (59 0 107 27 2) (64 0 107 27 2) (57 0 107 27 2) (60 18 179 100 11) (72 18 179 100 11) (72 120 115 64 1))) ((d 7) ((-72 0 3 64 1) (52 1 107 13 2) (54 1 107 27 2) (57 1 107 27 2) (60 5 107 27 2) (60 32 179 100 11))) ((d 7) ((61 28 184 100 11) (54 123 106 27 2) (57 123 106 27 2) (52 123 106 13 2) (60 127 106 27 2))) ((d 7) ((61 2 118 64 1) (62 30 184 100 11) (62 124 112 64 1))) ((d 7) ((-62 0 6 64 1) (63 6 118 64 1) (57 6 106 27 2) (54 6 106 27 2) (52 6 106 13 2) (60 10 106 27 2) (60 33 179 100 11) (64 124 113 64 1))) ((d 7) ((-64 0 5 64 1) (65 5 118 64 1) (61 29 184 100 11) (57 121 106 27 2) (54 121 106 27 2) (52 121 106 13 2) (66 123 113 64 1) (60 125 106 27 2))) ((d 7) ((-66 0 5 64 1) (67 5 118 64 1) (62 30 184 100 11) (68 123 113 64 1))) ((d 7) ((-68 0 6 64 1) (69 6 118 64 1) (52 7 106 13 2) (54 7 106 27 2) (57 7 106 27 2) (60 11 106 27 2) (60 33 180 100 11) (70 124 112 64 1))) ((d 7) ((-70 0 6 64 1) (71 6 118 64 1) (61 31 183 100 11) (53 120 106 27 2) (57 120 106 27 2) (60 120 106 27 2) (72 124 112 64 1) (64 124 106 27 2))) ((g 7) ((-72 0 6 64 1) (62 31 184 100 11) (71 122 78 64 1))) ((g 7) ((67 -4 63 64 1) (64 -2 107 27 2) (59 2 107 27 2) (57 2 107 27 2) (53 2 107 27 2) (60 34 180 100 11))) ((g 7) ((69 1 71 64 1) (61 33 183 100 11) (57 122 106 27 2) (64 122 106 27 2) (53 122 106 27 2) (59 122 106 27 2))) ((g 7) ((62 32 184 100 11) (67 123 118 64 1))) ((g 7) ((69 -5 119 64 1) (59 -3 106 27 2) (64 -3 106 27 2) (57 -3 106 27 2) (53 -3 106 27 2) (60 19 180 100 11) (67 114 118 64 1))) ((g 7) ((69 -5 118 64 1) (61 16 183 100 11) (67 109 118 64 1) (59 114 107 27 2) (64 114 107 27 2) (57 114 107 27 2) (53 114 107 27 2))) ((g 7) ((69 -2 118 64 1) (62 19 184 100 11) (67 112 118 64 1))) ((g 7) ((53 -96 107 55 2) (66 -3 118 64 1) (64 -1 106 27 2) (57 -1 106 27 2) (59 -1 106 27 2) (60 23 180 100 11) (72 23 180 100 11) (67 119 116 64 1))) ((c maj7) ((-67 0 2 64 1) (76 -2 118 64 1) (62 0 106 27 2) (59 0 106 27 2) (52 0 106 27 2) (55 0 106 27 2) (60 27 179 100 11))) ((c maj7) ((77 -2 118 64 1) (61 23 184 100 11) (52 117 107 27 2) (59 117 107 27 2) (55 117 107 27 2) (62 117 107 27 2))) ((c maj7) ((78 -4 118 64 1) (62 26 184 100 11) (81 114 118 64 1))) ((c maj7) ((52 1 106 27 2) (59 1 106 27 2) (55 1 106 27 2) (62 1 106 27 2) (60 28 179 100 11) (79 121 115 64 1))) ((c maj7) ((-79 0 3 64 1) (61 25 184 100 11) (67 123 114 64 1) (52 123 106 27 2) (59 123 106 27 2) (55 123 106 27 2) (62 123 106 27 2))) ((c maj7) ((-67 0 4 64 1) (62 25 184 100 11) (77 118 117 64 1))) ((c maj7) ((-77 0 1 64 1) (52 1 107 27 2) (59 1 107 27 2) (55 1 107 27 2) (62 1 107 27 2) (60 29 179 100 11) (76 127 110 64 1))) ((c maj7) ((-76 0 181 64 1) (61 26 183 100 11) (55 122 107 27 2) (59 122 107 27 2) (52 122 107 27 2) (62 122 107 27 2))) ((c maj7) ((62 27 184 100 11))) ((c maj7) ((71 -3 118 64 1) (55 1 106 27 2) (59 1 106 27 2) (52 1 106 27 2) (62 1 106 27 2) (60 29 180 100 11))) ((c maj7) ((74 6 118 64 1) (61 28 183 100 11) (52 124 106 27 2) (59 124 106 27 2) (55 124 106 27 2) (62 124 106 27 2) (72 128 109 64 1))) ((c maj7) ((-72 0 104 64 1) (62 27 184 100 11) (67 123 118 64 1))) ((c maj7) ((71 -3 118 64 1) (55 0 107 27 2) (59 0 107 27 2) (52 0 107 27 2) (62 0 107 27 2) (60 24 180 100 11))) ((c maj7) ((69 -4 118 64 1) (61 21 183 100 11) (52 118 106 27 2) (59 118 106 27 2) (55 118 106 27 2) (62 118 106 27 2))) ((c maj7) ((67 -1 118 64 1) (62 24 184 100 11))) ((c maj7) ((65 2 118 64 1) (52 2 106 27 2) (59 2 106 27 2) (55 2 106 27 2) (62 2 106 27 2) (60 26 180 100 11))) ((c maj7) ((62 -32 114 64 1) (61 25 183 100 11) (63 31 98 64 1) (64 82 155 64 1) (62 121 107 27 2) (59 121 107 27 2) (52 121 107 27 2) (55 121 107 27 2))) ((c maj7) ((-64 0 3 64 1) (67 -5 118 64 1) (62 24 184 100 11))) ((c maj7) ((62 -3 119 64 1) (59 1 106 27 2) (52 1 106 27 2) (55 1 106 27 2) (60 30 179 100 11))) ((c maj7) ((60 -3 118 64 1) (61 26 183 100 11) (52 118 107 27 2) (55 118 107 27 2) (59 118 107 27 2))) ((c maj7) ((62 5 118 64 1) (62 26 184 100 11))) ((c maj7) ((60 -4 118 64 1) (59 0 106 27 2) (52 0 106 27 2) (55 0 106 27 2) (60 29 179 100 11))) ((c maj7) ((62 -3 118 64 1) (61 27 184 100 11) (52 119 106 27 2) (55 119 106 27 2) (59 119 106 27 2))) ((c maj7) ((60 -3 118 64 1) (62 27 183 100 11) (63 113 71 64 1))) ((c maj7) ((64 -69 248 64 1) (62 1 107 27 2) (59 1 107 27 2) (52 1 107 27 2) (55 1 107 27 2) (60 30 179 100 11))) ((c maj7) ((61 29 183 100 11) (52 122 107 27 2) (59 122 107 27 2) (55 122 107 27 2) (62 122 107 27 2))) ((c maj7) ((62 30 184 100 11))) ((c maj7) ((76 -1 118 64 1) (55 1 106 27 2) (59 1 106 27 2) (52 1 106 27 2) (62 1 106 27 2) (60 30 179 100 11) (75 117 119 64 1))) ((c maj7) ((74 -4 118 64 1) (61 19 184 100 11) (73 110 118 64 1) (55 115 106 27 2) (59 115 106 27 2) (52 115 106 27 2) (62 115 106 27 2))) ((c maj7) ((72 -8 98 64 1) (62 20 184 100 11) (72 114 118 64 1))) ((c maj7) ((55 -4 106 27 2) (59 -4 106 27 2) (52 -4 106 27 2) (62 -4 106 27 2) (71 -2 118 64 1) (60 25 179 100 11) (70 120 116 64 1))) ((c maj7) ((-70 0 2 64 1) (69 -6 118 64 1) (61 23 183 100 11) (68 113 118 64 1) (57 126 107 27 2) (54 126 107 27 2) (61 126 107 27 2) (52 126 107 27 2))) ((f# m7) ((69 -4 118 64 1) (62 23 184 100 11) (76 115 118 64 1))) ((f# m7) ((57 -3 106 27 2) (54 -3 106 27 2) (61 -3 106 27 2) (52 -3 106 27 2) (60 23 180 100 11))) ((f# m7) ((76 -9 118 64 1) (61 25 183 100 11) (61 133 104 27 2) (54 133 104 27 2) (57 133 104 27 2) (52 133 104 27 2))) ((f# m7) ((-61 0 2 27 2) (-54 0 2 27 2) (-57 0 2 27 2) (-52 0 2 27 2) (62 24 184 100 11) (76 111 125 64 1))) ((f# m7) ((-76 0 98 64 1) (52 -2 106 27 2) (61 -2 106 27 2) (57 -2 106 27 2) (54 -2 106 27 2) (60 23 180 100 11))) ((f# m7) ((63 -10 118 64 1) (61 23 183 100 11) (64 109 126 64 1) (54 127 107 27 2) (61 127 107 27 2) (57 127 107 27 2) (52 127 107 27 2))) ((f# m7) ((-64 0 40 64 1) (62 21 184 100 11) (76 117 118 64 1))) ((f# m7) ((52 -2 106 27 2) (61 -2 106 27 2) (57 -2 106 27 2) (54 -2 106 27 2) (60 24 179 100 11) (74 118 117 64 1))) ((b 7) ((-74 0 1 64 1) (75 2 118 64 1) (61 22 184 100 11) (54 129 106 27 2) (61 129 106 27 2) (51 129 106 27 2) (57 129 106 27 2))) ((b 7) ((62 22 183 100 11))) ((b 7) ((51 1 106 27 2) (54 1 106 27 2) (57 1 106 27 2) (61 1 106 27 2) (60 24 179 100 11) (75 121 116 90 1))) ((b 7) ((-75 0 150 90 1) (61 23 184 100 11) (57 130 103 27 2) (54 130 103 27 2) (51 130 103 27 2) (61 130 103 27 2))) ((b 7) ((-57 0 4 27 2) (-54 0 4 27 2) (-51 0 4 27 2) (-61 0 4 27 2) (62 21 184 100 11))) ((b 7) ((51 -2 106 27 2) (54 -2 106 27 2) (57 -2 106 27 2) (61 -2 106 27 2) (60 26 179 100 11) (71 108 118 64 1))) ((b 7) ((72 -1 118 64 1) (61 32 184 100 11) (73 117 98 64 1) (57 136 101 27 2) (54 136 101 27 2) (51 136 101 27 2) (61 136 101 27 2))) ((b 7) ((-57 0 5 27 2) (-54 0 5 27 2) (-51 0 5 27 2) (-61 0 5 27 2) (74 7 118 64 1) (62 39 184 100 11) (75 120 115 64 1))) ((f# m7) ((-75 0 3 64 1) (61 2 106 27 2) (52 2 106 27 2) (57 2 106 27 2) (54 2 106 27 2) (76 7 118 64 1) (60 42 179 100 11))) ((f# m7) ((63 3 204 64 1) (61 38 184 100 11))) ((f# m7) ((52 -99 106 27 2) (57 -99 106 27 2) (61 -99 106 27 2) (54 -99 106 27 2) (64 1 118 64 1) (62 39 184 100 11) (76 134 102 64 1))) ((f# m7) ((-76 0 17 64 1) (54 3 106 27 2) (57 3 106 27 2) (61 3 106 27 2) (52 3 106 27 2) (60 43 179 100 11) (63 132 104 64 1))) ((f# m7) ((-63 0 61 64 1) (61 40 184 100 11) (64 129 108 64 1))) ((f# m7) ((-64 0 43 64 1) (57 -95 107 27 2) (61 -95 107 27 2) (54 -95 107 27 2) (52 -95 107 27 2) (62 40 183 100 11) (76 125 118 64 1))) ((f# m7) ((52 -5 106 27 2) (61 -5 106 27 2) (54 -5 106 27 2) (57 -5 106 27 2) (60 35 179 100 11) (78 114 118 64 1))) ((f# m7) ((61 31 184 100 11) (74 119 116 64 1) (52 124 106 27 2) (61 124 106 27 2) (54 124 106 27 2) (57 124 106 27 2))) ((b 7) ((-74 0 40 64 1) (75 -40 223 64 1) (62 33 184 100 11))) ((b 7) ((57 -5 106 27 2) (51 -5 106 27 2) (61 -5 106 27 2) (54 -5 106 27 2) (60 35 179 100 11) (71 113 121 64 1))) ((b 7) ((-71 0 55 64 1) (61 34 184 100 11) (61 127 106 27 2) (57 127 106 27 2) (54 127 106 27 2) (51 127 106 27 2))) ((b 7) ((62 34 184 100 11) (71 113 123 64 1))) ((b 7) ((-71 0 5 64 1) (54 -3 106 27 2) (57 -3 106 27 2) (61 -3 106 27 2) (51 -3 106 27 2) (70 1 128 64 1) (60 27 179 100 11) (71 115 122 64 1))) ((b 7) ((-71 0 6 64 1) (70 2 128 64 1) (61 23 184 100 11) (71 116 117 64 1) (54 126 106 27 2) (57 126 106 27 2) (61 126 106 27 2) (51 126 106 27 2))) ((b 7) ((-71 0 11 64 1) (70 7 128 64 1) (70 7 128 64 1) (62 27 184 100 11) (71 121 115 64 1))) ((b 7) ((-71 0 13 64 1) (51 -62 144 60 2) (61 -1 107 27 2) (54 -1 107 27 2) (57 -1 107 27 2) (72 9 113 64 1) (60 32 179 100 11) (72 32 179 100 11) (73 80 109 64 1) (74 123 113 64 1))) ((e maj7) ((-74 0 6 64 1) (63 -4 106 27 2) (75 -3 99 64 1) (59 2 106 27 2) (56 2 106 27 2) (54 2 106 27 2) (60 21 179 100 11) (75 116 104 64 1))) ((e maj7) ((71 -2 100 64 1) (61 19 183 100 11) (71 122 104 64 1) (54 132 103 27 2) (56 132 103 27 2) (63 132 103 27 2) (59 132 103 27 2))) ((e maj7) ((-54 0 3 27 2) (-56 0 3 27 2) (-63 0 3 27 2) (-59 0 3 27 2) (75 1 95 64 1) (62 20 184 100 11) (75 120 99 64 1))) ((e maj7) ((63 -2 106 27 2) (56 -2 106 27 2) (54 -2 106 27 2) (59 -2 106 27 2) (71 2 90 64 1) (60 22 180 100 11) (71 116 100 64 1))) ((e maj7) ((75 -8 100 64 1) (61 13 183 100 11) (75 111 109 64 1) (54 126 106 27 2) (56 126 106 27 2) (63 126 106 27 2) (59 126 106 27 2))) ((e maj7) ((71 -6 104 64 1) (62 13 184 100 11) (71 118 99 64 1))) ((e maj7) ((75 -4 95 64 1) (54 -4 107 27 2) (56 -4 107 27 2) (63 -4 107 27 2) (59 -4 107 27 2) (60 16 180 100 11) (75 115 99 64 1))) ((e maj7) ((71 -2 85 64 1) (61 14 183 100 11) (73 131 105 80 1) (55 131 105 27 2) (52 131 105 27 2) (59 131 105 27 2) (49 131 105 27 2))) ((a 7) ((-73 0 61 80 1) (-55 0 2 27 2) (-52 0 2 27 2) (-59 0 2 27 2) (-49 0 2 27 2) (62 14 184 100 11))) ((a 7) ((55 -4 106 27 2) (59 -4 106 27 2) (52 -4 106 27 2) (49 -4 106 27 2) (60 18 179 100 11))) ((a 7) ((61 16 183 100 11) (49 126 106 27 2) (59 130 107 27 2) (55 130 107 27 2) (52 130 107 27 2))) ((a 7) ((62 15 184 100 11) (61 113 119 64 1))) ((a 7) ((-61 0 51 64 1) (55 -7 106 27 2) (52 -7 106 27 2) (59 -7 106 27 2) (49 -2 106 27 2) (60 13 179 100 11) (64 132 105 64 1))) ((a 7) ((-64 0 60 64 1) (61 9 183 100 11) (55 130 103 27 2) (59 130 103 27 2) (52 130 103 27 2) (49 130 103 27 2) (69 132 101 64 1))) ((a 7) ((-55 0 3 27 2) (-59 0 3 27 2) (-52 0 3 27 2) (-49 0 3 27 2) (-69 0 60 64 1) (62 12 184 100 11) (71 131 106 64 1))) ((a 7) ((-71 0 45 64 1) (52 1 106 27 2) (59 1 106 27 2) (55 1 106 27 2) (49 1 106 27 2) (72 17 179 100 11) (60 17 179 100 11) (73 131 104 64 1))) ((g# m7) ((51 1 107 27 2) (59 1 107 27 2) (54 1 107 27 2) (56 1 107 27 2) (60 43 179 100 11) (74 133 104 64 1))) ((g# m7) ((-74 0 9 64 1) (75 4 109 64 1) (61 39 184 100 11) (76 123 111 64 1) (59 133 101 27 2) (54 133 101 27 2) (56 133 101 27 2) (51 133 101 27 2))) ((g# m7) ((-76 0 2 64 1) (-59 0 6 27 2) (-54 0 6 27 2) (-56 0 6 27 2) (-51 0 6 27 2) (77 3 113 64 1) (62 41 184 100 11) (78 126 111 64 1))) ((g# m7) ((-78 0 2 64 1) (75 2 109 64 1) (51 4 106 27 2) (56 4 106 27 2) (59 4 106 27 2) (54 4 106 27 2) (60 44 179 100 11) (76 121 113 64 1))) ((g# m7) ((77 -1 113 64 1) (61 41 184 100 11) (78 122 113 64 1))) ((g# m7) ((51 -100 106 27 2) (56 -100 106 27 2) (59 -100 106 27 2) (54 -100 106 27 2) (75 8 109 64 1) (62 41 183 100 11) (76 126 109 64 1))) ((g# m7) ((-76 0 4 64 1) (77 5 113 64 1) (51 6 106 27 2) (56 6 106 27 2) (59 6 106 27 2) (54 6 106 27 2) (60 45 179 100 11) (78 128 109 64 1))) ((g# m7) ((-78 0 4 64 1) (74 5 113 64 1) (61 41 184 100 11) (75 123 113 64 1) (50 134 106 27 2) (55 134 106 27 2) (58 134 106 27 2) (53 134 106 27 2))))
)



(setf Garner-CloseToYou_beatdur 231     ;October 30, 1973 (album 'Magician'), part A A B (original BPM=260)
      Garner-CloseToYou_beatsfromfile                                        ;transcribed by Fred Moyer
'(
((g# 7) ((82 -1 230 91 1) (65 -1 98 32 2) (60 -1 96 32 2) (54 0 98 40 2) (54 0 98 40 2) (60 37 175 100 11))) ((g# 7) ((61 12 179 100 11) (60 108 96 32 2) (54 110 98 40 2) (65 110 98 32 2) (54 110 98 40 2))) ((g# 7) ((62 16 179 100 11))) ((g# 7) ((60 -1 96 32 2) (54 1 98 40 2) (65 1 98 32 2) (54 1 98 40 2) (60 18 175 100 11))) ((g# 7) ((61 17 179 100 11) (60 120 96 32 2) (54 122 98 40 2) (65 122 98 32 2) (54 122 98 40 2))) ((g# 7) ((62 16 179 100 11) (80 114 115 64 1))) ((g# 7) ((82 -4 116 64 1) (60 2 96 32 2) (54 3 99 40 2) (65 3 99 32 2) (54 3 99 40 2) (60 20 175 100 11))) ((g# 7) ((84 -4 115 64 1) (61 17 180 100 11) (57 113 98 40 2) (60 113 96 40 2) (64 113 100 40 2) (53 113 98 40 2))) ((g 7) ((79 1 116 64 1) (62 17 180 100 11))) ((g 7) ((57 5 98 40 2) (60 5 96 40 2) (64 5 100 40 2) (53 5 98 40 2) (60 20 175 100 11))) ((g 7) ((61 18 179 100 11) (57 121 98 40 2) (60 121 96 40 2) (64 121 100 40 2) (53 121 98 40 2))) ((g 7) ((62 17 180 100 11))) ((g 7) ((57 2 98 40 2) (60 2 96 40 2) (64 2 100 40 2) (53 2 98 40 2) (60 21 175 100 11))) ((g 7) ((61 17 179 100 11) (57 116 98 40 2) (60 116 96 40 2) (64 116 100 40 2) (53 116 98 40 2) (79 118 81 44 1))) ((g 7) ((82 5 77 44 1) (62 19 179 100 11) (82 124 89 44 1))) ((g 7) ((79 1 73 44 1) (60 6 96 40 2) (57 6 98 40 2) (64 6 100 40 2) (53 6 98 40 2) (60 21 176 100 11))) ((g 7) ((85 -3 116 100 1) (59 -3 96 40 2) (64 -3 100 40 2) (53 -3 98 40 2) (60 21 175 100 11) (86 109 115 100 1))) ((g 7) ((85 -8 116 64 1) (61 17 179 100 11) (86 104 115 64 1) (59 104 96 40 2) (64 104 100 40 2) (53 104 98 40 2))) ((g 7) ((82 -4 115 64 1) (62 21 180 100 11) (84 111 116 64 1))) ((g 7) ((59 -9 96 40 2) (64 -9 100 40 2) (53 -9 98 40 2) (82 -1 115 64 1) (60 23 175 100 11) (83 114 115 64 1))) ((g 7) ((78 0 116 64 1) (61 22 179 100 11) (79 112 115 64 1) (64 116 100 40 2) (59 116 96 40 2) (53 116 98 40 2))) ((g 7) ((73 -1 116 64 1) (62 21 179 100 11))) ((g 7) ((74 -3 115 64 1) (64 1 100 40 2) (59 1 96 40 2) (53 1 98 40 2) (60 25 175 100 11))) ((g 7) ((71 -3 115 64 1) (61 24 180 100 11) (58 107 115 40 2) (55 107 115 40 2) (60 107 115 40 2) (63 107 115 40 2))) ((c m7) ((87 0 116 64 1) (62 24 180 100 11) (89 116 115 64 1))) ((c m7) ((55 -1 115 40 2) (63 -1 115 40 2) (58 -1 115 40 2) (62 -1 115 40 2) (87 0 116 64 1) (60 24 176 100 11) (84 116 114 64 1))) ((b m7) ((-84 0 1 64 1) (86 1 115 64 1) (61 22 180 100 11) (62 119 112 40 2) (54 119 112 40 2) (57 119 112 40 2) (61 119 112 40 2) (88 120 111 64 1))) ((b m7) ((-62 0 3 40 2) (-54 0 3 40 2) (-57 0 3 40 2) (-61 0 3 40 2) (-88 0 4 64 1) (86 -3 115 64 1) (62 22 180 100 11) (83 116 114 64 1))) ((bb m7) ((-83 0 2 64 1) (53 0 115 40 2) (61 0 115 40 2) (56 0 115 40 2) (60 0 115 40 2) (85 2 115 64 1) (60 26 175 100 11) (87 117 114 64 1))) ((bb m7) ((-87 0 1 64 1) (85 13 115 64 1) (61 23 180 100 11) (82 117 113 64 1) (60 121 109 40 2) (58 121 109 40 2) (52 121 109 40 2) (55 121 109 40 2))) ((eb 7) ((-82 0 2 64 1) (-60 0 6 40 2) (-58 0 6 40 2) (-52 0 6 40 2) (-55 0 6 40 2) (84 2 115 64 1) (62 24 180 100 11) (87 117 114 64 1))) ((eb 7) ((-87 0 2 64 1) (49 -94 116 70 2) (60 -2 115 40 2) (52 -2 115 40 2) (55 -2 115 40 2) (84 2 115 64 1) (60 35 175 100 11))) ((g# maj7) ((60 -10 116 40 2) (63 -10 116 40 2) (67 -10 116 40 2) (56 -10 116 40 2) (84 -7 115 64 1) (80 -4 115 64 1) (60 27 175 100 11))) ((g# maj7) ((61 27 179 100 11) (63 112 115 40 2) (67 112 115 40 2) (56 112 115 40 2) (60 112 115 40 2))) ((g# maj7) ((62 30 179 100 11))) ((g# maj7) ((56 -2 116 40 2) (67 -2 116 40 2) (63 -2 116 40 2) (60 -2 116 40 2) (60 28 176 100 11) (72 120 110 64 1))) ((g m7) ((-72 0 6 64 1) (61 29 179 100 11) (65 121 110 40 2) (62 121 110 40 2) (55 121 110 40 2) (58 121 110 40 2) (75 130 101 64 1))) ((g m7) ((-65 0 5 40 2) (-62 0 5 40 2) (-55 0 5 40 2) (-58 0 5 40 2) (-75 0 14 64 1) (62 27 179 100 11) (84 117 112 64 1))) ((g m7) ((-84 0 11 64 1) (62 -1 115 40 2) (65 -1 115 40 2) (55 -1 115 40 2) (58 -1 115 40 2) (82 1 116 64 1) (60 31 176 100 11))) ((g m7) ((62 19 180 100 11))) ((f m7) ((53 -5 116 40 2) (63 -4 116 40 2) (56 -4 116 40 2) (60 -4 116 40 2) (60 20 175 100 11))) ((f m7) ((61 19 179 100 11) (60 107 115 40 2) (63 107 115 40 2) (56 107 115 40 2) (53 108 116 40 2))) ((f m7) ((62 16 179 100 11) (72 126 106 64 1))) ((f m7) ((-72 0 2 64 1) (75 -3 133 64 1) (53 -3 115 40 2) (63 -2 115 40 2) (60 -2 115 40 2) (56 -2 115 40 2) (60 20 175 100 11) (84 125 104 64 1))) ((bb 7) ((-84 0 7 64 1) (82 -2 115 64 1) (61 24 179 100 11) (67 126 106 40 2) (56 126 106 40 2) (60 126 106 40 2) (63 126 106 40 2))) ((bb 7) ((-67 0 10 40 2) (-56 0 10 40 2) (-60 0 10 40 2) (-63 0 10 40 2) (62 31 180 100 11))) ((bb 7) ((70 -93 100 64 1) (72 -6 133 64 1) (55 0 115 40 2) (62 1 115 40 2) (58 1 115 40 2) (65 1 115 40 2) (60 23 175 100 11) (75 130 100 64 1))) ((bb 7) ((56 -85 124 70 2) (77 -4 130 64 1) (62 -2 115 40 2) (59 -2 115 40 2) (66 -2 115 40 2) (60 31 175 100 11) (78 131 104 64 1))) ((eb 7) ((55 -7 115 40 2) (58 -7 115 40 2) (61 -6 115 40 2) (65 -6 115 40 2) (79 -5 133 64 1) (60 24 175 100 11) (80 131 100 64 1))) ((eb 7) ((81 -4 130 64 1) (61 20 179 100 11) (58 111 116 40 2) (55 111 116 40 2) (65 112 115 40 2) (61 112 115 40 2) (82 122 104 64 1))) ((eb 7) ((-65 0 1 40 2) (-61 0 1 40 2) (81 -4 136 64 1) (62 24 179 100 11))) ((eb 7) ((82 -97 98 64 1) (81 -3 136 64 1) (58 -3 116 40 2) (55 -3 116 40 2) (65 -2 116 40 2) (61 -2 116 40 2) (60 25 176 100 11))) ((eb 7) ((82 -90 95 64 1) (78 -2 133 64 1) (61 24 180 100 11) (55 119 113 40 2) (58 119 113 40 2) (61 120 112 40 2) (65 120 112 40 2))) ((eb 7) ((-55 0 3 40 2) (-58 0 3 40 2) (-61 0 4 40 2) (-65 0 4 40 2) (79 -98 100 64 1) (75 -1 133 64 1) (62 23 180 100 11))) ((eb 7) ((73 -97 100 64 1) (55 -3 116 40 2) (58 -3 116 40 2) (61 -2 116 40 2) (65 -2 116 40 2) (75 0 133 64 1) (60 28 175 100 11))) ((eb 7) ((75 -95 100 64 1) (72 4 134 64 1) (61 25 179 100 11) (58 117 114 40 2) (55 117 114 40 2) (65 118 113 40 2) (61 118 113 40 2))) ((eb 7) ((-58 0 2 40 2) (-55 0 2 40 2) (-65 0 3 40 2) (-61 0 3 40 2) (75 0 134 64 1) (62 25 180 100 11))) ((eb 7) ((72 -3 133 64 1) (55 -3 116 40 2) (58 -3 116 40 2) (61 -2 116 40 2) (65 -2 116 40 2) (60 28 175 100 11))) ((eb 7) ((84 -4 134 64 1) (61 26 179 100 11) (55 112 116 40 2) (58 112 116 40 2) (61 113 116 40 2) (65 113 116 40 2))) ((eb 7) ((78 -96 100 64 1) (62 25 179 100 11))) ((eb 7) ((55 -4 116 40 2) (59 -2 115 40 2) (61 -1 116 40 2) (66 0 116 40 2) (60 20 175 100 11))) ((eb 7) ((61 16 179 100 11) (55 115 114 40 2) (59 117 112 40 2) (61 118 111 40 2) (66 119 110 40 2))) ((eb 7) ((-55 0 2 40 2) (-59 0 3 40 2) (-61 0 5 40 2) (-66 0 6 40 2) (72 0 133 64 1) (59 10 112 40 2) (62 18 179 100 11))) ((eb 7) ((55 -82 115 70 2) (75 -4 133 64 1) (59 -1 115 40 2) (61 0 116 40 2) (66 1 116 40 2) (72 22 176 100 11) (60 22 176 100 11))) ((g# 7) ((54 -7 116 40 2) (58 -6 116 40 2) (78 -4 134 64 1) (60 -4 116 40 2) (65 -3 115 40 2) (60 11 175 100 11))) ((g# 7) ((78 -99 101 64 1) (79 -6 130 64 1) (61 8 179 100 11) (54 115 113 40 2) (58 116 112 40 2) (60 118 110 40 2) (65 118 110 40 2) (79 125 103 64 1))) ((g# 7) ((-54 0 2 40 2) (-58 0 3 40 2) (-60 0 5 40 2) (-65 0 6 40 2) (80 -2 136 64 1) (62 11 179 100 11))) ((g# 7) ((80 -94 97 64 1) (54 -6 116 40 2) (58 -5 116 40 2) (60 -3 116 40 2) (65 -2 116 40 2) (81 -1 136 64 1) (60 13 176 100 11))) ((g# 7) ((81 -89 95 64 1) (82 0 136 64 1) (61 11 180 100 11) (54 116 115 40 2) (58 118 113 40 2) (60 119 112 40 2) (65 120 111 40 2))) ((g# 7) ((-54 0 1 40 2) (-58 0 2 40 2) (-60 0 4 40 2) (-65 0 5 40 2) (82 -92 98 64 1) (80 2 136 64 1) (62 11 180 100 11))) ((g# 7) ((82 -89 94 64 1) (78 0 105 64 1) (54 0 115 40 2) (58 1 115 40 2) (60 3 115 40 2) (65 4 115 40 2) (60 16 175 100 11) (79 36 136 64 1))) ((g# 7) ((75 5 136 64 1) (61 12 180 100 11) (53 120 110 40 2) (57 121 109 40 2) (60 123 107 40 2) (65 124 106 40 2))) ((g 7) ((-53 0 6 40 2) (-57 0 7 40 2) (-60 0 9 40 2) (-65 0 10 40 2) (79 2 136 64 1) (62 13 180 100 11))) ((g 7) ((77 -88 95 64 1) (53 -2 116 40 2) (60 0 115 40 2) (74 1 122 64 1) (65 1 115 40 2) (57 3 115 40 2) (60 15 175 100 11) (72 109 120 64 1))) ((g 7) ((70 0 105 64 1) (61 14 179 100 11) (71 36 136 64 1) (60 114 116 40 2) (65 115 115 40 2) (53 116 115 40 2) (57 117 115 40 2))) ((g 7) ((70 -5 105 64 1) (62 13 180 100 11) (71 31 136 64 1))) ((g 7) ((53 0 115 40 2) (60 1 115 40 2) (57 1 115 40 2) (65 2 115 40 2) (60 8 175 100 11))) ((g 7) ((61 4 179 100 11) (53 111 115 40 2) (60 112 115 40 2) (57 112 115 40 2) (65 113 115 40 2))) ((g 7) ((62 6 179 100 11))) ((g 7) ((53 -84 142 70 2) (57 -3 116 40 2) (60 -3 116 40 2) (65 -2 116 40 2) (60 9 176 100 11) (72 9 176 100 11))) ((g 7) ((79 -97 95 64 1) (53 -5 115 40 2) (59 -4 115 40 2) (82 -3 136 64 1) (64 -3 115 40 2) (60 18 175 100 11))) ((g 7) ((82 -96 97 64 1) (79 -2 135 64 1) (61 14 179 100 11) (59 108 115 40 2) (64 109 115 40 2) (53 110 115 40 2))) ((g 7) ((88 -1 95 64 1) (62 18 179 100 11))) ((g 7) ((82 -2 95 64 1) (59 -1 115 40 2) (64 0 115 40 2) (53 1 115 40 2) (60 19 176 100 11))) ((g 7) ((82 1 95 64 1) (61 18 180 100 11) (64 115 116 40 2) (59 117 115 40 2) (53 119 113 40 2))) ((g 7) ((-53 0 2 40 2) (79 -3 95 64 1) (62 17 180 100 11))) ((g 7) ((91 0 95 64 1) (59 0 116 40 2) (64 1 116 40 2) (53 2 115 40 2) (60 22 175 100 11))) ((g 7) ((61 18 180 100 11) (58 116 114 40 2) (55 116 114 40 2) (51 117 113 40 2) (62 119 111 40 2))) ((c m7) ((-58 0 1 40 2) (-55 0 1 40 2) (-51 0 3 40 2) (-62 0 5 40 2) (62 19 180 100 11))) ((c m7) ((55 -5 116 40 2) (58 -5 116 40 2) (51 -3 115 40 2) (90 -2 136 64 1) (62 -1 115 40 2) (60 21 175 100 11))) ((b m7) ((91 -92 97 64 1) (90 2 135 64 1) (61 20 179 100 11) (54 116 116 40 2) (57 116 116 40 2) (50 118 114 40 2) (61 120 112 40 2))) ((b m7) ((-50 0 1 40 2) (-61 0 3 40 2) (91 -89 95 64 1) (90 2 133 64 1) (62 19 180 100 11))) ((bb m7) ((91 -91 100 64 1) (53 -3 115 40 2) (56 -3 115 40 2) (49 -2 116 40 2) (60 0 116 40 2) (87 3 134 64 1) (60 14 175 100 11))) ((bb m7) ((85 -89 94 64 1) (82 -1 133 64 1) (61 10 179 100 11) (55 115 114 40 2) (49 116 113 40 2) (60 118 111 40 2))) ((eb 7) ((-55 0 1 40 2) (-49 0 3 40 2) (-60 0 5 40 2) (80 -88 95 64 1) (78 3 104 64 1) (62 12 179 100 11) (79 67 136 64 1))) ((eb 7) ((49 -77 115 60 2) (55 -3 116 40 2) (75 0 133 64 1) (60 1 115 40 2) (60 16 176 100 11) (72 16 176 100 11))) ((g# 7) ((75 -68 289 90 1) (60 -5 115 40 2) (54 -5 115 40 2) (65 -1 115 40 2) (60 13 175 100 11))) ((g# 7) ((77 -3 130 64 1) (61 10 179 100 11) (60 110 115 40 2) (54 110 116 40 2) (65 114 114 40 2))) ((g# 7) ((-65 0 1 40 2) (78 -96 104 64 1) (79 4 133 64 1) (62 13 179 100 11))) ((g# 7) ((80 -91 100 64 1) (60 -7 116 40 2) (54 -6 115 40 2) (81 -4 134 64 1) (65 -2 115 40 2) (60 15 175 100 11))) ((g m7) ((82 -98 101 64 1) (81 -1 135 64 1) (61 13 180 100 11) (55 113 116 40 2) (58 114 116 40 2) (62 116 115 40 2) (53 117 114 40 2))) ((g m7) ((-62 0 1 40 2) (-53 0 2 40 2) (82 -91 95 64 1) (81 0 133 64 1) (62 13 180 100 11))) ((g m7) ((82 -94 100 64 1) (62 -6 116 40 2) (55 -5 115 40 2) (58 -4 115 40 2) (78 -1 105 64 1) (53 -1 115 40 2) (60 17 175 100 11) (79 64 136 64 1))) ((g m7) ((75 -3 133 64 1) (61 14 179 100 11) (60 113 115 40 2) (53 114 115 40 2) (56 115 115 40 2) (51 117 114 40 2))) ((f m7) ((-51 0 2 40 2) (70 4 133 64 1) (62 14 180 100 11))) ((f m7) ((60 -3 115 40 2) (53 -3 116 40 2) (56 -1 115 40 2) (51 1 116 40 2) (60 17 175 100 11))) ((f m7) ((82 -3 133 90 1) (61 15 179 100 11) (53 114 115 40 2) (60 114 115 40 2) (56 115 115 40 2) (51 116 115 40 2))) ((f m7) ((-51 0 1 40 2) (62 15 179 100 11) (84 76 133 90 1))) ((bb 7) ((54 -5 116 40 2) (56 -4 116 40 2) (82 -3 134 90 1) (50 -2 115 40 2) (60 -1 115 40 2) (60 17 176 100 11))) ((bb 7) ((61 14 180 100 11) (54 114 115 40 2) (56 115 114 40 2) (50 116 113 40 2) (60 118 111 40 2))) ((bb 7) ((-56 0 1 40 2) (-50 0 3 40 2) (-60 0 4 40 2) (62 16 180 100 11))) ((bb 7) ((48 -87 180 70 2) (50 -2 115 40 2) (56 -2 115 40 2) (54 -1 115 40 2) (60 0 115 40 2) (60 18 175 100 11))) ((eb 7) ((70 -11 130 64 1) (61 -8 115 40 2) (55 -7 115 40 2) (51 -7 116 40 2) (58 -6 115 40 2) (60 18 175 100 11) (72 128 103 64 1))) ((eb 7) ((-72 0 1 64 1) (75 -3 133 64 1) (61 15 179 100 11) (61 113 115 40 2) (55 114 114 40 2) (51 114 114 40 2) (58 115 113 40 2) (75 126 100 64 1))) ((eb 7) ((-61 0 1 40 2) (-55 0 1 40 2) (-51 0 2 40 2) (-58 0 2 40 2) (75 -1 111 64 1) (62 18 179 100 11) (75 113 115 64 1))) ((eb 7) ((61 -4 115 40 2) (55 -4 116 40 2) (51 -3 115 40 2) (75 -2 112 64 1) (58 -2 115 40 2) (60 21 175 100 11) (75 112 115 64 1))) ((eb 7) ((75 -2 112 64 1) (61 19 179 100 11) (61 111 115 40 2) (55 111 116 40 2) (51 112 115 40 2) (58 113 115 40 2))) ((eb 7) ((62 19 179 100 11))) ((eb 7) ((73 -100 101 64 1) (61 3 116 40 2) (55 4 115 40 2) (51 4 116 40 2) (75 5 112 64 1) (58 5 116 40 2) (60 22 176 100 11))) ((eb 7) ((78 -4 130 64 1) (72 -4 177 64 1) (61 21 179 100 11) (79 50 154 50 1) (61 113 115 40 2) (55 113 115 40 2) (51 114 115 40 2) (58 114 116 40 2))) ((eb 7) ((62 22 180 100 11))) ((eb 7) ((72 -3 101 64 1) (61 -1 116 40 2) (55 0 115 40 2) (51 0 116 40 2) (58 1 116 40 2) (60 21 175 100 11))) ((eb 7) ((75 5 119 64 1) (61 20 180 100 11) (61 119 113 40 2) (55 119 113 40 2) (51 120 112 40 2) (58 121 111 40 2))) ((eb 7) ((-61 0 2 40 2) (-55 0 3 40 2) (-51 0 4 40 2) (-58 0 4 40 2) (75 -100 104 64 1) (72 -3 111 64 1) (62 19 180 100 11))) ((eb 7) ((61 -1 116 40 2) (55 0 115 40 2) (51 0 116 40 2) (58 1 116 40 2) (75 2 111 64 1) (60 24 175 100 11))) ((eb 7) ((72 0 111 64 1) (61 20 179 100 11) (61 119 110 40 2) (55 120 109 40 2) (51 121 108 40 2) (58 121 108 40 2))) ((eb 7) ((-61 0 6 40 2) (-55 0 6 40 2) (-51 0 7 40 2) (-58 0 8 40 2) (78 -2 180 64 1) (62 22 180 100 11) (79 58 173 70 1))) ((eb 7) ((55 -50 126 70 2) (61 0 116 40 2) (66 1 116 40 2) (60 30 179 100 11))) ((g# 7) ((60 -11 115 40 2) (72 -10 180 64 1) (54 -10 115 40 2) (65 -10 116 40 2) (60 23 175 100 11))) ((g# 7) ((61 22 180 100 11) (60 111 115 40 2) (54 111 116 40 2) (65 112 115 40 2))) ((g# 7) ((62 25 180 100 11))) ((g# 7) ((75 -100 104 64 1) (60 -4 116 40 2) (77 -3 138 64 1) (54 -3 115 40 2) (65 -2 115 40 2) (60 24 175 100 11))) ((g# 7) ((75 -99 105 64 1) (78 -6 138 64 1) (61 25 179 100 11) (60 112 115 40 2) (54 112 116 40 2) (65 113 115 40 2) (79 121 105 64 1))) ((g# 7) ((80 -9 138 64 1) (62 22 180 100 11) (81 118 104 64 1))) ((g# 7) ((82 -11 130 64 1) (82 -11 130 64 1) (60 -4 115 40 2) (54 -4 116 40 2) (65 -3 115 40 2) (60 26 175 100 11) (82 123 104 64 1) (82 123 104 64 1))) ((g# 7) ((80 -4 138 64 1) (61 23 179 100 11) (60 113 116 40 2) (54 114 115 40 2) (65 115 115 40 2) (82 127 104 64 1))) ((g# 7) ((-82 0 1 64 1) (77 -9 51 64 1) (78 17 59 64 1) (62 23 180 100 11) (79 50 138 64 1))) ((g# 7) ((75 -3 138 64 1) (60 -3 115 40 2) (54 -3 115 40 2) (65 -2 115 40 2) (60 23 175 100 11))) ((g# 7) ((77 -6 138 64 1) (61 24 179 100 11) (60 112 115 40 2) (54 112 115 40 2) (65 113 115 40 2))) ((g# 7) ((72 -2 138 64 1) (62 19 179 100 11))) ((g# 7) ((60 -5 116 40 2) (54 -4 115 40 2) (65 -3 115 40 2) (78 0 138 64 1) (60 25 175 100 11))) ((g# 7) ((75 -8 138 64 1) (61 27 180 100 11) (60 110 115 40 2) (54 110 115 40 2) (65 111 115 40 2))) ((g# 7) ((84 -5 138 64 1) (62 37 180 100 11))) ((g# 7) ((54 -44 130 60 2) (60 -6 115 40 2) (65 -5 115 40 2) (79 -2 138 64 1) (60 37 175 100 11))) ((g 7) ((59 -11 115 40 2) (53 -11 115 40 2) (63 -10 115 40 2) (60 29 175 100 11))) ((g 7) ((79 -37 153 64 1) (61 15 180 100 11) (59 106 116 40 2) (53 107 115 40 2) (63 108 115 40 2))) ((g 7) ((82 -4 131 64 1) (82 -4 131 64 1) (62 18 180 100 11) (79 127 104 64 1))) ((g 7) ((59 -8 116 40 2) (53 -7 115 40 2) (63 -6 115 40 2) (60 21 175 100 11))) ((g 7) ((84 -97 324 64 1) (61 19 179 100 11) (59 111 116 40 2) (53 112 115 40 2) (63 113 115 40 2))) ((g 7) ((62 19 180 100 11))) ((g 7) ((82 -100 104 64 1) (59 -4 116 40 2) (53 -3 115 40 2) (63 -2 115 40 2) (85 1 130 64 1) (60 24 175 100 11) (86 131 101 64 1))) ((g 7) ((-86 0 4 64 1) (85 -4 130 64 1) (61 20 179 100 11) (58 113 116 40 2) (52 114 115 40 2) (62 115 115 40 2) (86 127 103 64 1))) ((c 7) ((-86 0 1 64 1) (82 -3 131 64 1) (62 21 179 100 11) (79 124 104 64 1))) ((c 7) ((52 -11 115 40 2) (58 -8 116 40 2) (77 -7 130 64 1) (62 -6 115 40 2) (60 23 175 100 11))) ((c 7) ((79 -5 130 64 1) (61 22 179 100 11) (58 116 115 40 2) (52 117 115 40 2) (62 117 115 40 2))) ((c 7) ((-62 0 1 40 2) (77 -2 130 64 1) (62 21 179 100 11) (79 129 101 64 1))) ((c 7) ((-79 0 3 64 1) (74 -18 51 64 1) (58 -5 116 40 2) (52 -4 115 40 2) (62 -3 115 40 2) (75 8 59 64 1) (60 15 175 100 11) (76 41 138 64 1))) ((c 7) ((75 -1 138 64 1) (61 12 179 100 11) (58 111 116 40 2) (52 112 115 40 2) (62 113 115 40 2))) ((c 7) ((72 -4 138 64 1) (62 14 179 100 11) (52 128 104 70 2))) ((c 7) ((-52 0 57 70 2) (58 -6 115 40 2) (62 -5 116 40 2) (60 17 175 100 11) (72 17 175 100 11))) ((f 7) ((53 -8 115 40 2) (57 -6 115 40 2) (63 -6 116 40 2) (60 7 175 100 11))) ((f 7) ((84 -2 131 64 1) (61 4 179 100 11) (53 112 115 40 2) (57 114 114 40 2) (63 114 114 40 2))) ((f 7) ((-57 0 1 40 2) (-63 0 2 40 2) (86 -95 104 64 1) (87 0 131 64 1) (62 7 180 100 11))) ((f 7) ((89 -100 105 64 1) (53 -7 115 40 2) (57 -6 116 40 2) (63 -5 115 40 2) (86 3 130 64 1) (60 10 175 100 11))) ((f 7) ((87 -96 104 64 1) (84 3 131 64 1) (61 8 179 100 11) (53 113 116 40 2) (57 115 116 40 2) (63 116 115 40 2))) ((f 7) ((86 -93 104 64 1) (83 6 130 64 1) (62 8 179 100 11))) ((f 7) ((85 -89 105 64 1) (53 -4 116 40 2) (57 -2 116 40 2) (63 -1 115 40 2) (81 7 130 64 1) (60 13 175 100 11))) ((f 7) ((83 -90 104 64 1) (80 5 130 64 1) (61 9 179 100 11) (53 115 116 40 2) (57 117 115 40 2) (63 117 116 40 2))) ((f 7) ((81 -93 105 64 1) (79 -2 131 64 1) (62 3 179 100 11))) ((f 7) ((81 -98 104 64 1) (53 -13 115 40 2) (57 -11 115 40 2) (63 -11 116 40 2) (78 -3 131 64 1) (60 5 176 100 11))) ((f 7) ((80 -98 104 64 1) (76 -3 131 64 1) (61 3 180 100 11) (53 108 115 40 2) (57 109 116 40 2) (63 110 115 40 2))) ((f 7) ((77 -99 104 64 1) (74 1 130 64 1) (62 3 180 100 11) (75 126 104 64 1))) ((f 7) ((-75 0 1 64 1) (53 -10 115 40 2) (57 -8 115 40 2) (63 -8 116 40 2) (60 -3 175 100 11) (72 1 130 64 1))) ((f 7) ((73 -100 104 64 1) (61 -6 179 100 11) (71 0 131 64 1) (53 110 115 40 2) (57 111 116 40 2) (63 112 115 40 2))) ((f 7) ((72 -99 104 64 1) (62 -5 180 100 11) (73 4 131 64 1))) ((f 7) ((74 -96 104 64 1) (53 -12 116 40 2) (57 -10 115 40 2) (63 -9 115 40 2) (72 0 175 100 11) (60 0 175 100 11) (75 3 131 64 1))) ((f m7) ((76 -93 104 64 1) (77 10 130 64 1) (61 38 179 100 11) (51 115 116 40 2) (53 116 115 40 2) (60 118 113 40 2) (56 118 113 40 2))) ((f m7) ((-60 0 2 40 2) (-56 0 2 40 2) (62 38 180 100 11))) ((f m7) ((89 -92 97 64 1) (51 -4 115 40 2) (53 -4 116 40 2) (60 -2 115 40 2) (56 -2 116 40 2) (60 44 175 100 11))) ((f m7) ((61 41 179 100 11) (51 123 107 40 2) (53 124 106 40 2) (60 126 104 40 2) (56 126 104 40 2))) ((f m7) ((-51 0 9 40 2) (-53 0 9 40 2) (-60 0 11 40 2) (-56 0 11 40 2) (62 42 179 100 11))) ((f m7) ((51 2 115 40 2) (53 2 116 40 2) (60 4 115 40 2) (56 4 116 40 2) (60 45 175 100 11))) ((f m7) ((87 -94 105 64 1) (84 2 130 64 1) (61 44 179 100 11) (51 120 112 40 2) (53 120 112 40 2) (60 122 110 40 2) (56 122 110 40 2))) ((f m7) ((-51 0 3 40 2) (-53 0 4 40 2) (-60 0 6 40 2) (-56 0 6 40 2) (80 7 131 64 1) (62 43 179 100 11))) ((bb 7) ((50 -3 116 40 2) (60 0 115 40 2) (56 0 115 40 2) (77 4 228 64 1) (60 46 176 100 11))) ((bb 7) ((-77 0 229 64 1) (61 42 180 100 11) (50 124 105 40 2) (60 126 103 40 2) (56 126 103 40 2))) ((bb 7) ((-77 0 9 64 1) (-50 0 10 40 2) (-60 0 13 40 2) (-56 0 13 40 2) (62 44 180 100 11))) ((bb 7) ((50 -88 138 70 2) (75 1 153 64 1) (60 5 115 40 2) (56 5 115 40 2) (60 36 175 100 11))) ((bb 7) ((78 -8 161 64 1) (62 25 180 100 11))) ((bb 7) ((48 -75 134 50 2) (60 -9 116 40 2) (56 -9 116 40 2) (80 -5 119 64 1) (60 25 180 100 11))) ((bb 7) ((80 -1 123 64 1) (62 35 175 100 11))) ((bb 7) ((55 -62 116 50 2) (80 -3 123 64 1) (66 -3 115 40 2) (61 -1 115 40 2) (60 21 175 100 11) (72 21 175 100 11))) ((g# 7) ((65 -5 115 40 2) (58 -5 116 40 2) (80 -4 133 64 1) (54 -4 116 40 2) (60 -3 115 40 2) (60 23 175 100 11))) ((g# 7) ((80 -6 129 64 1) (61 19 179 100 11) (54 117 110 40 2) (82 118 109 64 1) (65 118 109 40 2) (58 118 109 40 2) (60 119 108 40 2))) ((g# 7) ((-54 0 5 40 2) (-82 0 1 64 1) (-65 0 6 40 2) (-58 0 6 40 2) (-60 0 8 40 2) (78 2 111 64 1) (62 23 180 100 11))) ((g# 7) ((76 -13 98 30 1) (54 -3 115 40 2) (60 -3 115 40 2) (58 -2 115 40 2) (65 0 115 40 2) (60 25 175 100 11))) ((g# 7) ((61 24 179 100 11) (54 119 113 40 2) (65 120 112 40 2) (58 120 112 40 2) (60 122 110 40 2))) ((g# 7) ((-54 0 2 40 2) (-65 0 3 40 2) (-58 0 4 40 2) (-60 0 5 40 2) (62 23 179 100 11))) ((g# 7) ((80 -84 207 64 1) (54 -3 115 40 2) (65 -2 115 40 2) (58 -2 116 40 2) (60 0 115 40 2) (60 28 175 100 11) (82 127 104 64 1))) ((g# 7) ((-82 0 6 64 1) (78 7 124 64 1) (61 25 179 100 11) (64 118 113 40 2) (53 118 113 40 2) (60 118 113 40 2) (57 120 111 40 2))) ((g 7) ((-64 0 2 40 2) (-53 0 3 40 2) (-60 0 3 40 2) (-57 0 4 40 2) (75 -98 112 64 1) (62 25 179 100 11))) ((g 7) ((64 0 115 40 2) (53 0 116 40 2) (60 1 115 40 2) (57 2 115 40 2) (60 27 175 100 11))) ((g 7) ((79 4 141 64 1) (61 25 180 100 11) (64 119 112 40 2) (53 119 112 40 2) (60 119 112 40 2) (57 121 110 40 2))) ((g 7) ((-64 0 3 40 2) (-53 0 4 40 2) (-60 0 4 40 2) (-57 0 5 40 2) (81 -92 98 64 1) (77 2 134 64 1) (62 25 180 100 11))) ((g 7) ((74 -93 100 64 1) (64 -1 115 40 2) (53 -1 116 40 2) (60 0 115 40 2) (57 1 115 40 2) (60 16 175 100 11))) ((g 7) ((72 1 117 64 1) (61 13 179 100 11) (64 117 113 40 2) (53 117 113 40 2) (60 117 113 40 2) (57 119 111 40 2))) ((g 7) ((-64 0 2 40 2) (-53 0 3 40 2) (-60 0 3 40 2) (-57 0 4 40 2) (70 -9 107 64 1) (74 13 218 64 1) (62 14 180 100 11) (71 16 160 64 1))) ((g 7) ((-74 0 80 64 1) (64 -3 115 40 2) (53 -2 115 40 2) (60 -2 115 40 2) (57 -1 116 40 2) (67 0 134 64 1) (60 19 175 100 11))) ((g 7) ((64 -11 115 40 2) (53 -10 115 40 2) (59 -10 115 40 2) (57 -9 116 40 2) (60 28 175 100 11))) ((g 7) ((67 -7 134 64 1) (61 25 179 100 11) (64 115 113 40 2) (53 116 112 40 2) (59 116 112 40 2) (57 117 111 40 2))) ((g 7) ((-64 0 2 40 2) (-53 0 3 40 2) (-59 0 3 40 2) (-57 0 5 40 2) (68 -7 69 64 1) (69 -3 134 64 1) (62 28 179 100 11) (70 96 134 64 1))) ((g 7) ((71 -6 129 64 1) (64 -4 115 40 2) (53 -4 116 40 2) (59 -4 116 40 2) (57 -2 115 40 2) (60 30 176 100 11) (73 106 121 64 1))) ((g 7) ((74 -8 134 64 1) (61 28 180 100 11) (76 99 129 64 1) (64 118 113 40 2) (53 119 112 40 2) (59 119 112 40 2) (57 120 111 40 2))) ((g 7) ((-64 0 2 40 2) (-53 0 3 40 2) (-59 0 3 40 2) (-57 0 5 40 2) (77 -7 133 64 1) (62 28 180 100 11) (79 109 121 64 1))) ((g 7) ((-79 0 4 64 1) (80 -5 133 64 1) (64 -3 115 40 2) (53 -3 116 40 2) (59 -2 115 40 2) (57 -1 115 40 2) (60 32 175 100 11) (82 111 120 64 1))) ((g 7) ((-82 0 5 64 1) (83 -4 125 64 1) (61 29 179 100 11) (84 104 124 64 1) (55 114 114 40 2) (51 118 110 40 2) (62 118 110 40 2) (58 119 109 40 2))) ((c m7) ((-84 0 1 64 1) (-55 0 1 40 2) (-51 0 5 40 2) (-62 0 6 40 2) (-58 0 6 40 2) (87 -4 125 64 1) (62 32 180 100 11) (89 117 114 64 1))) ((c m7) ((-89 0 10 64 1) (51 -9 115 40 2) (55 -9 115 40 2) (62 -5 115 40 2) (58 -4 115 40 2) (87 2 125 64 1) (60 35 175 100 11) (84 113 113 64 1))) ((b m7) ((-84 0 12 64 1) (86 8 124 64 1) (61 37 179 100 11) (61 118 114 40 2) (50 119 113 40 2) (54 119 113 40 2) (57 119 113 40 2) (88 128 104 64 1))) ((b m7) ((-61 0 2 40 2) (-50 0 2 40 2) (-54 0 2 40 2) (-57 0 2 40 2) (-88 0 21 64 1) (86 12 125 64 1) (62 36 179 100 11) (83 124 109 64 1))) ((bb m7) ((-83 0 16 64 1) (56 -5 115 40 2) (49 -2 116 40 2) (53 -1 115 40 2) (60 -1 115 40 2) (85 3 124 64 1) (60 27 175 100 11) (87 123 108 64 1))) ((bb m7) ((-87 0 17 64 1) (85 8 125 64 1) (61 24 179 100 11) (49 117 112 40 2) (55 118 111 40 2) (82 120 109 64 1) (52 122 107 40 2) (60 122 107 40 2))) ((eb 7) ((-49 0 3 40 2) (-55 0 4 40 2) (-82 0 16 64 1) (-52 0 8 40 2) (-60 0 8 40 2) (87 -3 125 64 1) (62 26 179 100 11))) ((eb 7) ((49 -74 186 70 2) (52 -3 115 40 2) (60 -3 115 40 2) (87 -1 125 64 1) (55 2 115 40 2) (60 29 175 100 11))) ((g# 7) ((87 -7 63 64 1) (65 -7 115 40 2) (56 -7 116 40 2) (60 -5 115 40 2) (54 -3 115 40 2) (99 20 50 90 1) (60 37 175 100 11) (87 73 63 64 1) (99 99 50 90 1))) ((g# 7) ((87 -80 63 64 1) (99 -53 50 90 1) (87 0 63 64 1) (99 27 49 90 1) (61 33 179 100 11) (54 110 115 40 2) (65 115 112 40 2) (56 115 112 40 2) (60 117 110 40 2))) ((g# 7) ((-65 0 3 40 2) (-56 0 3 40 2) (-60 0 5 40 2) (62 37 180 100 11))) ((g# 7) ((54 -4 115 40 2) (56 -3 115 40 2) (60 -2 115 40 2) (65 1 111 40 2) (60 39 175 100 11))) ((g m7) ((95 -15 63 64 1) (84 -3 125 64 1) (96 -2 125 64 1) (61 38 179 100 11) (53 116 115 40 2) (55 117 115 40 2) (58 118 114 40 2) (62 121 111 40 2))) ((g m7) ((-58 0 2 40 2) (82 0 125 64 1) (94 0 125 64 1) (62 37 179 100 11))) ((g m7) ((62 -3 116 40 2) (53 -2 116 40 2) (58 -1 115 40 2) (75 1 125 64 1) (87 1 125 64 1) (55 3 115 40 2) (60 42 175 100 11))) ((g m7) ((61 39 179 100 11) (60 111 116 40 2) (53 112 115 40 2) (56 112 116 40 2) (51 113 115 40 2))) ((f m7) ((82 -1 125 64 1) (94 -1 125 64 1) (62 39 179 100 11))) ((f m7) ((87 -7 125 64 1) (99 -6 124 64 1) (60 -1 115 40 2) (51 -1 116 40 2) (53 0 115 40 2) (56 0 116 40 2) (60 41 176 100 11))) ((f m7) ((102 -15 63 64 1) (91 -2 107 64 1) (103 -2 107 64 1) (61 39 180 100 11) (60 122 109 40 2) (51 123 108 40 2) (53 124 107 40 2) (56 124 107 40 2))) ((f m7) ((-60 0 7 40 2) (-51 0 8 40 2) (-53 0 8 40 2) (-56 0 8 40 2) (102 -21 63 64 1) (91 -8 125 64 1) (103 -7 124 64 1) (62 39 180 100 11))) ((bb 7) ((87 -3 124 64 1) (99 -3 125 64 1) (60 -2 115 40 2) (50 -2 116 40 2) (54 -1 115 40 2) (56 -1 116 40 2) (60 33 175 100 11))) ((bb 7) ((84 0 125 64 1) (96 0 125 64 1) (61 30 180 100 11) (60 120 110 40 2) (50 121 109 40 2) (54 122 108 40 2) (56 122 108 40 2))) ((bb 7) ((-60 0 6 40 2) (-50 0 7 40 2) (-54 0 7 40 2) (-56 0 7 40 2) (80 0 125 64 1) (92 0 125 64 1) (62 31 180 100 11) (82 128 103 64 1) (94 128 103 64 1))) ((bb 7) ((50 -70 115 60 2) (60 -9 115 40 2) (54 -8 116 40 2) (56 -7 115 40 2) (80 -1 125 64 1) (92 -1 125 64 1) (60 36 175 100 11) (82 127 103 64 1) (94 127 103 64 1))) ((eb 7) ((90 -16 103 64 1) (55 -8 116 40 2) (51 -7 116 40 2) (61 -7 111 40 2) (58 -6 116 40 2) (79 -3 125 64 1) (91 -2 124 64 1) (60 28 175 100 11))) ((eb 7) ((75 1 124 64 1) (87 1 125 64 1) (61 25 179 100 11) (55 115 113 40 2) (61 116 111 40 2) (51 116 112 40 2) (58 117 111 40 2))) ((eb 7) ((-55 0 2 40 2) (-51 0 3 40 2) (-58 0 5 40 2) (75 -6 125 64 1) (87 -6 125 64 1) (62 28 179 100 11))) ((eb 7) ((55 -8 115 40 2) (51 -7 116 40 2) (61 -7 111 40 2) (58 -6 116 40 2) (72 2 125 64 1) (84 2 125 64 1) (60 30 176 100 11))) ((eb 7) ((75 -6 125 64 1) (87 -6 125 64 1) (61 25 180 100 11) (55 113 115 40 2) (61 114 111 40 2) (51 114 115 40 2) (58 115 116 40 2))) ((eb 7) ((72 -3 125 64 1) (84 -3 125 64 1) (62 24 180 100 11))) ((eb 7) ((90 -11 103 64 1) (79 -7 125 30 1) (58 -5 115 40 2) (55 -3 115 40 2) (85 -2 124 30 1) (91 -2 125 64 1) (61 -2 111 40 2) (51 -2 115 40 2) (60 29 175 100 11))) ((eb 7) ((79 -3 124 50 1) (85 -3 124 30 1) (91 -3 125 64 1) (62 26 179 100 11))) ((eb 7) ((58 -7 115 40 2) (55 -5 115 40 2) (61 -4 111 40 2) (51 -4 115 40 2) (60 31 175 100 11))) ((eb 7) ((72 -5 124 64 1) (84 -5 125 64 1) (61 28 180 100 11) (58 102 115 40 2) (55 104 116 40 2) (61 105 111 40 2) (51 105 116 40 2))) ((eb 7) ((75 -10 125 64 1) (87 -10 125 64 1) (62 29 180 100 11) (75 122 103 64 1) (87 123 102 64 1))) ((eb 7) ((58 -5 115 40 2) (55 -3 116 40 2) (72 -2 125 64 1) (84 -2 125 64 1) (61 -2 111 40 2) (51 -2 116 40 2) (60 22 175 100 11))) ((eb 7) ((90 -11 103 64 1) (79 -6 124 30 1) (85 -2 125 30 1) (91 -2 125 64 1) (61 20 179 100 11) (58 110 111 40 2) (55 112 111 40 2) (51 113 111 40 2) (61 113 107 40 2))) ((eb 7) ((62 20 180 100 11))) ((eb 7) ((72 -8 125 64 1) (84 -7 125 64 1) (58 -6 115 40 2) (55 -4 115 40 2) (51 -3 115 40 2) (61 -3 111 40 2) (60 25 176 100 11))) ((eb 7) ((75 -13 125 64 1) (87 -13 125 64 1)))) 
)



(setf refbeatdur Garner-TeaForTwo_beatdur)

(setf garner-beatlist (make-beat-list Garner-TeaForTwo_beatsfromfile Garner-TeaForTwo_beatdur))
(setf garner-beatlist1 (make-beat-list Garner-CloseToYou_beatsfromfile Garner-CloseToYou_beatdur))

(setf garner-beatlist (add-beat-list garner-beatlist refbeatdur garner-beatlist1 Garner-CloseToYou_beatdur))

(setf test (nthcar 3 garner-beatlist))
(length test)

;Simple beat oracles:
(setf garnerrightoracle (NewImprovizer (remove-channels garner-beatlist '(2 11)) refbeatdur))
(setf garnerleftoracle (NewImprovizer (remove-channels garner-beatlist '(1 11)) refbeatdur))


#|
; Problème (?) oracle garner
;-------------------------
;APRES AVOIR EXECUTE LE FICHIER GARNER.LISP

;Sauvegarde sur le disque
;-------------------------
(let ((l (cdddr (reverse (multiple-value-list (get-decoded-time)))))) (save-improvizer 
               garnerrightoracle
               (format nil "~a/GarnerTest.or" 
                       path_dir_live_oracles)))

;Grille sur laquelle jouer
;-------------------------
(setf tune Dicidenbas_tune)
(setf beatduration (beatduration tune))
(setf grid (expand_grid (grid tune)))

;Recharger l'oracle
;-------------------------
(setf oracle_solo (load-improvizer (format nil "~a/GarnerTest.or" path_dir_live_oracles)))
(setf (max-continuity oracle_solo) 1000)

;Calcul impro
;-------------
(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))

;Jouer
;------------------
;(setf impro (beats->chseq impro_solo beatduration 0))
;(pgmout 4 1)
;(pgmout 5 2)
;(Stop-Player *general-player*)
;(play impro)
|#


