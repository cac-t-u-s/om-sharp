;ISSU DU DECOUPAGE DE BEATLIST.LISP
;COMMENTAIRES ET TUTORIALS
;-----------------------------------------

(in-package :om)



;Open a MIDI file with chords on channel 16, and then give a couple: (list of pairs label+mididata, beatdur)
(midi-to-MidiHarmBeats)

(mf-info (load-midi-file (om-choose-file-dialog)))

;Example of such a list of pairs label+mididata, with its corresponding 'beatdur':

(setf Zisteinit_beatdur 536
      Zisteinit_MidiHarmBeatsfromfile
      '(      ;"Ziste zeste", BPM = 112, beat duration = 60000/BPM = 536 ms, Bernard's solo recorded in Uzeste, April 4th 2003
((bb m7) nil) 
((bb m7) nil) 
((bb m7) nil) 
((bb m7) ((70 190 82 111 11) (76 306 141 100 11))) 
((f 7) ((74 35 153 111 11) (68 212 52 55 11) (73 343 147 72 11))) 
((f 7) nil) 
((f 7) nil) 
((f 7) nil) 
))

;MidiHarmBeatLIST:
(setf ZisteMidiHarmBeatlist (make-MidiHarmBeat-list Zisteinit_MidiHarmBeatsfromfile Zisteinit_beatdur))

(setf DrumsMidiHarmBeatlist   ;beatdur=536, BPM=112
  (let ((2beatpoumchiBPM112 
         '((nolabel nolabel 2) ((36 0 229 80 10) (42 268 229 80 10) (36 536 229 80 10) (40 536 229 80 10) (42 804 229 80 10)))))
    (make-MidiHarmBeat-list (list 2beatpoumchiBPM112 2beatpoumchiBPM112 2beatpoumchiBPM112 2beatpoumchiBPM112) Zisteinit_beatdur)))

(play (merger (MidiHarmBeats->chseq ZisteMidiHarmBeatlist Zisteinit_beatdur 0) (MidiHarmBeats->chseq DrumsMidiHarmBeatlist Zisteinit_beatdur 0)))
(Stop-Player *general-player*)

;ORACLE:
(setf oracleziste (NewImprovizer ZisteMidiHarmBeatlist Zisteinit_beatdur))

(setf labels0 '((bb m7) (bb m7) (bb m7) (bb m7) (f 7) (f 7) (f 7) (f 7)) labels (append labels0 labels0 labels0 labels0))

(progn (pgmout 4 11) (ctrlchg 7 127 11) (setf (max-continuity oracleziste) 1)
(play (setf impro (merger (MidiHarmBeats->chseq (ImprovizeOnHarmGrid oracleziste (length labels) labels) Zisteinit_beatdur 0) 
                          (MidiHarmBeats->chseq (loop for i from 1 to (floor (/ (length labels) 8)) append DrumsMidiHarmBeatlist) 
                                        Zisteinit_beatdur 0)))))

(my-save-as-midi impro Zisteinit_beatdur)
(om-inspect ZisteMidiHarmBeatlist)


;Tests 29/4/2012 ---> MISSING NoteOff  ????????????????

(loop for i from 0 for x in (thread-MidiHarmBeats (oracle->MidiHarmBeatlist (oracle1 *current-tune*)) (RefTempo (oracle1 *current-tune*)))
      do (format *om-stream* "MidiHarmBeat=~a chord=~a midiset=~a~%" i (HarmLabel x) (MidiSet x)))

(loop for i from 0 for x in (first (midi-to-MidiHarmBeats))
      do (format *om-stream* "MidiHarmBeat=~a data=~a ~%" i x))        

(loop for i from 0 with res = (midi-to-MidiHarmBeats) for x in (thread-MidiHarmBeats (make-MidiHarmBeat-list (first res) (second res)) (second res))
      do (format *om-stream* "MidiHarmBeat=~a chord=~a midiset=~a~%" i (HarmLabel x) (MidiSet x)))

(load-midi-file (om-choose-file-dialog))


;SAVING MIDIFILES BY ADJUSTING BARLINES TO THE CORRECT TEMPO:
;1) Calculate the mean MidiHarmBeat duration given by clocks on channel 16 as loaded by OM (= defaultbeatdur), and then stretch durations 
;   with the coef. = 1000/defaultbeatdur so that durations are adapted to OM default tempo BPM=60 and adjusted to the barlines 
;2) Then indicate the real tempo of the original sequence by a metaevent at the beginning of the file for playing notes at the correct speed,
;   (= defaultbeatdur)

;REMARK: Max creates MIDI files thanks to 'seq' with correct durations of the notes, but without tempo metaevent,
;so that an implicit wrong tempo is fixed with value BPM=120 (beat duration = 500 ms).
;When OM loads these files, it does not find any tempo, so another wrong tempo is fixed (BPM=60, beat duration = 1000 ms),
;thus BPM=120 being replaced by BPM=60, durations of the notes are multiplied by 2 (= played half tempo). 
;Consequently, before applying 'save-as-midi-with-tempo', one must divide durations by 2 using 'timestretch' 

;SIMPLE TEST: Write a simple MIDI file with the correct tempo:
(setf chsq700 
      (make-instance 'chord-seq :lmidic '(1200 1200 1200 1200 1200) :lonset '(0 700 1400 2100 2800) :ldur '(10 10 10 10 10) :lchan '(16 16 16 16 16)))

(save-as-midi-with-tempo chsq700 700)
;---> Intuem: barlines are adjusted + tempo is correct BPM=85.71 (=60000/700)

(dump-straight (om-choose-file-dialog))
;---> ((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) 4D 54 72 6B 0 0 0 13 0 FF 58 4 4 2 0 0 0 FF 51 3 A AE 60    etc.
;---> gives the correct tempo meta event = (0 FF 51 3 A AE 60) last three bytes A AE 60 give beat duration in microseconds
(write #x0AAE60 :base 10)     ;---> 700000 = 700 ms BPM=85.71




;REMARK  (22/4/2012): 'save-as-midi-with-tempo' is not intended to change the tempo !!!!!!!!!
;It works well only if 'beatdur' is the correct tempo according to the data on channel 16



;===========================================================================================
;This will load a MIDI file and resave it with the correct tempo  corresponding to the MidiHarmBeats on channel 16

(update-midifile-tempo-from-max)        ;-> from Max = WITHOUT A TEMPO META EVENT, but an implicit tempo equal to BPM=120,
                                        ;              thus when loaded in OM with default tempo BPM=60, durations are doubled       

(update-midifile-tempo-from-intuem)     ;-> from Intuem = with a tempo meta event, OM takes the correct durations        

;Notice that in Intuem, the correct tempo is written at the BEGINNING of the MIDI file
;Thus time should not be deleted at the beginning, otherwise the tempo will be deleted too
;(and Intuem default tempo BPM=120 will be inserted instead)
;===========================================================================================

;EXAMPLE IN MAX: Make a MIDI file in Max with simple data: 5 notes with pitch=12, dur=10, velo=100 played by a 'metro' with deltatime=700
(dump-straight (om-choose-file-dialog))
;---> ((4D 54 68 64 0 0 0 6 0 0 0 1 0 60) 4D 54 72 6B 0 0 0 30 0 9F C 64 1 9F C 0 81 5 9F C 64 2 9F C 0   etc.
;---> THERE IS NO META EVENT (0 FF 51 ...) !!!!!!!!!!!!!!!!!!!!

;Re-save it with Inutem (after deleting unused tracks):
;---> ((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) 4D 54 72 6B 0 0 0 19 0 FF 51 3 7 A1 20  etc.
;---> tempo meta event = (0 FF 51 3 7 A1 20) last three bytes 7 A1 20 give beat duration in microseconds
(write #x07A120 :base 10)     ;---> 500000 = 500 ms BPM=120





;///////////// A FAIRE: FILTRAGE DES EVENEMENTS D'UN MIDIFILE /////////////


-> le beat7 de l'impro "Jaime-new11.9.2011-9-21.5.txt" semble correspondre à l'état 243 de l'oracle

(start-region or)
(midiset (aref (vectext or) 242))
(mapcar 'midiset (list (aref (vectext or) 240) (aref (vectext or) 241) (aref (vectext or) 242) (aref (vectext or) 243)))
(loop for i from 1 to 250
      do (format *om-stream* "etat=~a midiset=~a~%" i (midiset (aref (vectext or) i))))
etat=179 midiset=((54 -42 58 111 2) (55 47 167 105 2) (56 224 63 113 2) (57 334 260 113 2))
etat=180 midiset=((-57 0 584 113 2) (53 -52 36 85 2) (54 36 141 113 2) (53 234 74 106 2) (50 338 94 103 2))
etat=181 midiset=((-57 0 588 113 2) (54 -74 110 123 2) (53 41 141 121 2) (50 343 245 127 2))

-> L'erreur est dans l'oracle lui-meme. A partir de l'etat 179, le prolongement -57 est present dans tous les etats jusqu'a la fin

(setf midifromfile (evts-from-midifile (MidiFixedBuff *current-tune*)))

(setf MidiHarmBeatlist (make-MidiHarmBeat-list (clocked-evts->MidiHarmBeats midifromfile)))

(loop for i from 175 to 185
      do (format *om-stream* "etat=~a midiset=~a~%" i (midiset (nth i MidiHarmBeatlist))))
etat=178 midiset=((54 -42 58 111 2) (55 47 167 105 2) (56 224 63 113 2) (57 334 260 113 2))
etat=179 midiset=((-57 0 584 113 2) (53 -52 36 85 2) (54 36 141 113 2) (53 234 74 106 2) (50 338 94 103 2))
etat=180 midiset=((-57 0 588 113 2) (54 -74 110 123 2) (53 41 141 121 2) (50 343 245 127 2))
etat=181 midiset=((-57 0 594 113 2) (-50 0 594 127 2))
etat=182 midiset=((-57 0 588 113 2) (-50 0 234 127 2))
etat=183 midiset=((-57 0 594 113 2))
etat=184 midiset=((-57 0 594 113 2))

(length midifromfile)

(loop for i from 1 to 1393
      do (format *om-stream* "etat=~a midiset=~a~%" i (nth i midifromfile)))  ; puis on cherche un repere: velocite 113 de (57 334 260 113 2)
etat=965 midiset=(12 106088 47 100 16)
etat=966 midiset=(55 47 167 105 2)
etat=967 midiset=(2 47 500 102 16)
etat=968 midiset=(56 224 63 113 2)
etat=969 midiset=(57 334 260 113 2)     -> on le trouve la, duree=260
etat=970 midiset=(53 -52 36 85 2)
etat=971 midiset=(12 106682 42 100 16)
etat=972 midiset=(54 36 141 113 2)
etat=973 midiset=(2 42 494 102 16)
etat=974 midiset=(53 234 74 106 2)
etat=975 midiset=(50 338 94 103 2)
etat=976 midiset=(54 -74 110 123 2)
etat=977 midiset=(12 107266 46 100 16)
etat=978 midiset=(53 41 141 121 2)
etat=979 midiset=(2 46 495 102 16)
etat=980 midiset=(50 343 245 127 2)
etat=981 midiset=(12 107854 42 100 16)

-> on detaille les etapes du calcul de MidiHarmBeatlist:

(setf midifromfile (evts-from-midifile (MidiFixedBuff *current-tune*)))

-> on voit bien les pbs de "modifs physiques", car apres avoir recharge 'midifromfile', l'affichage devient:
etat=965 midiset=(12 106088 47 100 16)
etat=966 midiset=(55 106135 167 105 2)
etat=967 midiset=(2 106135 500 102 16)
etat=968 midiset=(56 106312 63 113 2)
etat=969 midiset=(57 106422 111296 113 2)  -> ici le 57 a une duree ANORMALE = 111296 ms

-> donc le BUG est present des l'ouverture du fichier dans OM avec 'evts-from-midifile'


;Jean Bresson, 10/11/2011 (+ mars 2006)
;========================
;Fonction 'get-midievents' definies dans "midifile.lisp" pour les MIDIfiles:
;   - prend en entrée un MIDIfile (obtenu en ouvrant un fichier avec 'load-midi-file' et 'om-choose-file-dialog'
;   - retourne la liste des evenements MIDI sous forme de crochets
;[MIDIEVENT date-ms type-evt / track X / port X / chan X / VALUE=X]
;Exemple: pour un note on/off:
;[MIDIEVENT 3016 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(66 113)]     -> couple de valeurs (pitch velo)


(loop for x in (get-midievents (load-midi-file (om-choose-file-dialog))) 
      do (format *om-stream* "~a~%" x))

(setq tmp (get-midievents (load-midi-file (om-choose-file-dialog))))
([MIDIEVENT 0 Tempo / track 0 / port 0 / chan 1 / VALUE=(60)] [MIDIEVENT 0 TimeSign / track 0 / port 0 / chan 1 / VALUE=(4 2 0 0)] [MIDIEVENT 0 Tempo / track 0 / port 0 / chan 1 / VALUE=(60)] [MIDIEVENT 0 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(60 100)] [MIDIEVENT 611 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(66 100)] [MIDIEVENT 998 KeyOff / track 1 / port 0 / chan 1 / VALUE=(60 64)] [MIDIEVENT 1250 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(66 100)] [MIDIEVENT 1609 KeyOff / track 1 / port 0 / chan 1 / VALUE=(66 64)] [MIDIEVENT 1694 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(61 100)] [MIDIEVENT 2248 KeyOff / track 1 / port 0 / chan 1 / VALUE=(66 64)] [MIDIEVENT 2692 KeyOff / track 1 / port 0 / chan 1 / VALUE=(61 64)] [MIDIEVENT 2692 EndTrack / track 1 / port 0 / chan 1 / VALUE=nil])


(ev-date (nth 10 tmp))

;Dans le fichier problematique, on a autour de l'onset 106000 ms une note anormalement longue:

[MIDIEVENT 106135 KeyOff / track 2 / port 0 / chan 16 / VALUE=(12 0)]
[MIDIEVENT 106135 KeyOn  / track 2 / port 0 / chan 16 / VALUE=(2 102)]
[MIDIEVENT 106302 KeyOff / track 1 / port 0 / chan 2 / VALUE=(55 0)]
[MIDIEVENT 106312 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(56 113)]
[MIDIEVENT 106375 KeyOff / track 1 / port 0 / chan 2 / VALUE=(56 0)]
[MIDIEVENT 106422 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(57 113)]
[MIDIEVENT 106422 KeyOff / track 1 / port 0 / chan 2 / VALUE=(57 1)]     ===> PROBLEME: KeyOn et KeyOff SIMULTANES a la date = 106422 ms
[MIDIEVENT 106630 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(53 85)]
[MIDIEVENT 106635 KeyOff / track 2 / port 0 / chan 16 / VALUE=(2 0)]
[MIDIEVENT 106666 KeyOff / track 1 / port 0 / chan 2 / VALUE=(53 0)]

;POUR PASSER D'UNE LISTE DE 'MidiEvents' A UNE LISTE DE 5UPLES (mail de Jean 14/11/2011):

(setf tmp-EventMidi-seq (objfromobjs tmp (make-instance 'EventMidi-seq)))

(get-midi-notes tmp-EventMidi-seq)
(((60 0 998 100 1) (66 611 998 100 1) (66 1250 998 100 1) (61 1694 998 100 1)))

-> avec 'objfromobjs' et 'get-midi-notes':
on passe d'une liste de 'MidiEvents' à une liste de 5uples
Donc il faut:
- filtrer la liste de 'MidiEvents' récupérée à partir du fichier MIDI
- après filtrage, la transformer en liste de 5uples pour créer une MidiHarmBeatlist







;==================
;dump of a midifile


(dump-midifile (om-choose-file-dialog))

;The meta event to indicate a tempo change is: FF 51 03 tt tt tt 	
;The 3 data bytes of tt tt tt are the tempo in MICROseconds (not MILLIseconds) per quarter note. 
;If tt tt tt = 07 A1 20, then each quarter note should be 07 A1 20 = 500 000 microseconds long (= 500 ms).
;For example, a tempo of 120 BPM = 07 A1 20 microseconds per quarter note.

(write #xFF :stream *om-stream* :base 10)
(write '(#x00 #xFF #x51 #x03 #x07 #xA1 #x20) :stream *om-stream* :base 10)    -> (0 255 81 3 7 161 32)
(write #x07A120 :stream *om-stream* :base 10)                                 -> 500000 = 500 ms -> BPM=60000/500=120
(write '(#x00 #xFF #x51 #x03 #x09 #x27 #xC0) :stream *om-stream* :base 10)    -> (0 255 81 3 9 39 192)
(write #x0927C0 :stream *om-stream* :base 10)                                 -> 600000 = 600 ms -> BPM=60000/600=100

(dump-straight "/Users/marc/Documents/OpenMusic/MyWS/out-files/tempo100.mid")    ; File "tempo100.mid" created by sequencer INTUEM
((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) 
4D 54 72 6B 0 0 0 19 
0 FF 51 3 9 27 C0   ; FF 51 (tempo meta event) + nb bytes= (3) + 3 bytes 9 27 C0 (beatduration in microseconds=600000, BPM=100)
0 FF 58 4 4 2 8 8 
0 FF 59 2 0 0 
0 FF 2F 0 
4D 54 72 6B 0 0 0 4D 
0 FF 3 8 54 72 61 63 6B 3A 20 31 
0 FF 4 1D 51 75 69 63 6B 74 69 6D 65 20 4D 75 73 69 63 61 6C 20 49 6E 73 74 72 75 6D 65 6E 74 73 0 B0 0 0 0 B0 20 0 0 C0 0 0 B0 7 64 0 B0 A 
40 0 90 3C 40  ; note Do3 = 3C noteon (90)
9D A 80 3C 40  : note Do3 = 3C noteoff (80)
0 FF 2F 0 nil)

;There seems to be a bug in the multitrack recording of MIDI files by 'detonate' in Max,
;since the Track #O devoted to meta events is not taken into account in the number of tracks of the header.
;Here is a dump of a MIDI file created by 'detonate' in Max:
(dump-straight "/Users/marc/Documents/OpenMusic/MyWS/out-files/a.mid")  
((4D 54 68 64 0 0 0 6 0 1 0 1 0 60) ; format 1 (0 1), ERROR: nb track=1 (0 1) whereas the file contains two tracks
4D 54 72 6B 0 0 0 B ; = Track 1
0 FF 51 3 7 A1 20   ; FF 51 (tempo meta event) + nb bytes= (3) + 3 bytes 7 A1 20 (beatduration in microseconds=500000, BPM=120)
0 FF 2F 0 
4D 54 72 6B 0 0 0 14 ; = Track 2
0 99 43 78 0 99 38 78 13 99 38 0 0 99 43 0 
0 FF 2F 0 nil)

;It suffices to load the file into INTUEM and resave it, then the number of tracks will be updated to its correct value including Track #0:
(dump-straight "/Users/marc/Documents/OpenMusic/MyWS/out-files/aINTUEM.mid")  
((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) ; format 1 (0 1), nb track=2 (0 2)
4D 54 72 6B 0 0 0 19 
0 FF 51 3 7 A1 20 
0 FF 58 4 4 2 8 8 
0 FF 59 2 0 0 0 FF 2F 0 
4D 54 72 6B 0 0 0 4A 
0 FF 3 8 54 72 61 63 6B 3A 20 31 
0 FF 4 1D 51 75 69 63 6B 74 69 6D 65 20 4D 75 73 69 63 61 6C 20 49 6E 73 74 72 75 6D 65 6E 74 73 0 B9 7 40 0 B9 A 40 
0 99 43 78 0 99 38 78 81 45 89 38 0 0 89 43 0 
0 FF 2F 0 nil)

