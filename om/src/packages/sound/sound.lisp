;;;=================
;;; SOUND PROJECT
;;;=================


(in-package :om)

(load (decode-local-path "audio-api/load-audio-api.lisp"))
        
(mapc #'(lambda (filename) (compile&load (decode-local-path filename))) 
      '(
        "sound/sound"
        "sound/audio-tools"
        "sound/juce-player"
        "sound/buffer-player"
        "sound/sound-processing"
        "sound-pack"
        )
      )