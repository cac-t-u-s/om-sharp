;;;=================
;;; SOUND PROJECT
;;;=================


(in-package :om)

(load (decode-local-path "audio-api/load-audio-libs.lisp"))
        
(mapc #'(lambda (filename) (compile&load (decode-local-path filename))) 
      '(
        "sound/sound"
        "sound/audio-tools"
        ;"sound/las-player"
        "sound/juce-player"
        "sound-pack"
        "sound/buffer-player"
        "sound/sound-processing"
        )
      )

#+macosx(load (decode-local-path "IAE/load-iae.lisp"))
