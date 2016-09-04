(in-package :om)

(let ((soundpack (omNG-make-package "Spat"
                   :container-pack *om-package-tree*
                   :doc "Spatialization tools and connection to the Spat framework."
                   :classes '(spat-scene 3DC)
                   :functions '(#+sdif sdif-export)
                   )))
                                          
  ;(add-ref-section (gen-ref-entries kernelpack))
  )