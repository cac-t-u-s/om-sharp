;;;=================
;;; SOUND OBJECT
;;;=================


(in-package :om)

(let ((soundpack (omNG-make-package "Audio"
                   :container-pack *om-package-tree*
                   :doc "Sound/DSP objects and support."
                   :classes '(sound)
                   :functions '(sound-dur sound-dur-ms sound-points)
                   :subpackages (list (omNG-make-package 
                                       "Conversions"
                                       :functions '(lin->db db->lin samples->sec sec->samples ms->sec sec->ms))
                                      (omNG-make-package 
                                       "Tools"
                                       :functions '(adsr)))
                                      
                 )))
                                          
  ;(add-ref-section (gen-ref-entries kernelpack))
  )


