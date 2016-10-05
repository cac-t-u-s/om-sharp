;Tsigane.lisp
;by Marc Chemillier 2011

(in-package :om)


(setf horadelabotosani '((c m7 4) (c m7 4) (bb 7 4) (eb maj7 4) 
                         (eb maj7 4) (eb maj7 4) (eb maj7 4) (eb maj7 4) (g 7 4) (c m7 4) 
                         (c m7 4) (c m7 4) (c m7 4) (c m7 4)))

(setf hora (make-instance 'tune :grid horadelabotosani :tunename "Horadelabotosani"))
(setf *current-tune* hora)

#|
(setf midifromfile (evts-from-midifile)) 
(setf defaultbeatdur (round (om-mean (x->dx (mapcar 'first (first (check-clocks midifromfile)))))))  
                                                        ;'check-clocks' does not make physical modifications on 'midifromfile'

(progn
(setf horabeatlist (make-beat-list (clocked-evts->beats (clone midifromfile))))    ;WARNING: 'clocked-evts->beats' makes physical modifications
(setf (oracle hora) (NewImprovizer horabeatlist))
(setf solobeatlist (make-beat-list (clocked-evts->beats (remove 2 (clone midifromfile) :key #'fifth))))
(setf bassbeatlist (make-beat-list (clocked-evts->beats (remove 1 (clone midifromfile) :key #'fifth))))
(setf (liveoracle hora) (NewImprovizer solobeatlist))
)

(remove 16 '((74 373 108 64 1) (12 25910 25 100 16) (48 1 114 64 1) (72 10 119 64 1) (12 26390 26 100 16)) :key #'fifth)


(play (horaimpro))

(Stop-Player *general-player*)
(save-as-midi-with-tempo impro defaultbeatdur)


(play (horaimprowithbass))

(update-midifile-tempo)

(setf (max-continuity (liveoracle hora)) 100)
(setf (max-continuity (liveoracle hora)) 1)

|#


(defun horaimpro ()
  (progn  (setf impro nil) (pgmout 56 1) (pgmout 58 2) (ctrlchg 7 0 16)
    (setf impro (beats->chseq (thread-Beats (ImprovizeOnHarmGrid (oracle hora) (length (simplegrid hora)) (simplegrid hora)) defaultbeatdur) 
                              defaultbeatdur 0))      ;defaultbeatdur = 480 (BPM=125)
))


(defun horaimprowithbass ()
  (progn  (setf impro nil) (pgmout 56 1) (pgmout 58 2) (ctrlchg 7 0 16) 
    (setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid (liveoracle hora) (length (simplegrid hora)) (simplegrid hora)) defaultbeatdur) 
                                      defaultbeatdur 0)      ;defaultbeatdur = 480 (BPM=125)
                        (beats->chseq bassbeatlist defaultbeatdur 0)))
))


(defun horaaddgrid (impro) (setf impro (merger impro (beats->chseq (grid->beatlist (simplegrid hora) defaultbeatdur) defaultbeatdur 0))))

(defun grid->beatlist (labels beatdur)
  (let* ((emptybeatfromfile '(((d maj7) nil) ((a maj7) nil) ((d m7) nil) ((a m7) nil) ((d 7) nil) ((a 7) nil)))
         (emptybeatlist (make-beat-list emptybeatfromfile)) 
         (o (NewImprovizer emptybeatlist))
         (beatlist (thread-Beats (ImprovizeOnHarmGrid o (length labels) labels))))
    (loop for beat in beatlist 
          do (setf (MidiSet beat) 
                   (append (list (list 12 0 50 100 16)  ; clock
                                 (list (MidiRoot (first (HarmLabel beat))) 50 (- beatdur 100) 
                                       (case (second (HarmLabel beat)) (maj7 100) (m7 101) (7 102)) 16))
                           (MidiSet beat))))
    beatlist))


(defun horaimprowithbassMIDIfile ()
  (horaimprowithbass)
  (let ((name "/Users/marc/Desktop/CR Victor Stoichita/Essais Improtek/essai.mid"))
    (save-as-midi-with-tempo impro (* defaultbeatdur 2) name)
    name))
  