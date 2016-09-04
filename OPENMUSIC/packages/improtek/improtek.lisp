(in-package :om)

(defvar *omimpro-files* nil)

(setq *omimpro-files*
      '("improtek/sources/LoadImprotek"
        "database/scenarios/scenarios"
        "database/oracles/oracles"
        "handler/improvisation-handler"
        "handler/improvisation-renderer"
        "handler/improvisation-query"
        "handler/DemoMidi"
        "handler/DemoAudio"
        ))

(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename))) 
      *omimpro-files*)