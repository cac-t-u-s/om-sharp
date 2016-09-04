; Fichier Auramusic.lisp / 24 juilet 2010

(in-package :om)



;(if (om-standalone-p)            ; need 'my-save-as-midi'
;    (load "/Applications/OM 6.2.2/libraries/OMax 2.0/OMax")
;    (compile&load "/Applications/OM 6.2.2/libraries/OMax 2.0/OMax"))

(if (om-standalone-p)           ; Need remus.lisp
    (load "/Applications/OM 6.2.2/libraries/repmus/repmus")
    (compile&load "/Applications/OM 6.2.2/libraries/repmus/repmus"))

;------------------------------------------
;Conversion of a SDIF File into a MIDI File
;------------------------------------------

#|
AudioSculpt analysis proceeds as follows from an audio file:
- Analysis/Sonagram Analysis for visualizing the signal and placing the markers
- Analysis/Generate markers: 
  for the spoken voice, choose 'Spectral differencing' instead of 'Transient detection'
- Edit the markers and delete selected markers to fit the spoken words
- Analysis/Chord Sequence Analysis: 
  display horizontal red lines corresponding to the partials 
- File/Save Analysis/Save Chord Sequence Analysis to produce a SDIF File

OpenMusic can open a SDIF File and save it as a MIDI File with the partials as notes:

(progn
(setf as-chsq (as->om (get-sdif) 
                      1 100                  ;velo min et max
                      5 
                      0100 12700 8            ;midicents min et max
                      ;1          ;npoly = nb partiels simultanes
                      ;2
                      ;3
                      4
))
(my-save-as-midi as-chsq 500)                      ;beatdur=500 for default tempo BPM=120
)


(chord-seq->mf-info as-chsq)


Essais avec la voix de Montand:
- montand.cs.mkauto-1p
Markers places automatiquement (pas sur 'Transient', mais sur 'Spectral differencing')
Chord Sequence Analysis avec nb max de partiels = 1

- montand.cs.mksyllabe-1p
- montand.cs.mksyllabe-2p
- montand.cs.mksyllabe-4p
Markers places a la main en reperant les syllabes dans le signal
Chord Sequence Analysis avec nb max de partiels = 1, 2 ou 4

Probleme: quand on fait l'analyse de la partie d'Hermeto en solo, ca ne coincide pas avec la transcription
La mauvaise qualité de l'enregistrement de la video est peut-être responsable des erreurs
(le souffle notamment renforce le spectre dans l'aigu)
N.B.: si on fait l'analyse de la transcription elle-meme, ce n'est pas non plus absolument exact (1 erreur de note),
donc la 'Chord Sequence Analysis' d'Audiosculpt n'est pas fiable à 100%

Essais avec 'Quando As Aves Se Encontram, Nasce O Som', oiseau a 0'17





|#