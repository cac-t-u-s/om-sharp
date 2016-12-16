;;;=================
;;; SOUND OBJECT
;;;=================


(in-package :om)

(omNG-make-package 
 "Audio"
 :container-pack *om-package-tree*
 :doc "Sound/DSP objects and support."
 :classes '(sound)
 :functions '(sound-dur sound-dur-ms sound-points save-sound)
 :subpackages (list (omNG-make-package 
                     "Processing"
                     :functions '(sound-silence 
                                  sound-fade sound-loop sound-reverse sound-cut
                                  sound-mix sound-seq
                                  sound-normalize sound-vol 
                                  sound-mono-to-stereo sound-stereo-to-mono sound-stereo-pan
                                  sound-resample 
                                  ))
                    (omNG-make-package 
                     "Conversions"
                     :functions '(lin->db db->lin samples->sec sec->samples ms->sec sec->ms))
                    (omNG-make-package 
                     "Tools"
                     :functions '(adsr)))
 )


