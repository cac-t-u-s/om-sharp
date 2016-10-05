(in-package :om)

;;;=================================================================================;;;
;;;====================================SCENARIOS====================================;;;
;;;=================================================================================;;;
(setq *scenario-original* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                            (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                            (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4)
                            (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
      *scenario-degeu* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) 
                         (bb maj7 4) (eb maj7 4) (c maj7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) 
                         (c m7 4) (f 7 4) (bb maj7 4) (b 7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
      *scenario-subst1* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                          (c m7 4) (f 7 4) (b m7 2) (e 7 2) (bb m7 2) (eb 7 2) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                          (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (b m7 4) (e 7 4)
                          (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (a m7 2) (d 7 2) (g m7 4) (g m7 4))
      *scenario-subst2* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                          (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                          (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4)
                          (a m7 4) (d 7 4) (g 7 2) (gb 7 2) (f 7 2) (e 7 2) (eb 7 4) (d 7 4) (g m7 4) (g m7 4)))