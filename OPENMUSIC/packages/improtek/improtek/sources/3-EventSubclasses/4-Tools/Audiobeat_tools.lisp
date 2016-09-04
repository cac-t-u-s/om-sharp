(in-package :om)


(defun NewSymbolicImprovizer_AudioHarmbeats (memory_tune duration mult)
  (let* ((listofemptyevents '()) (i 0) (labels '()))
    (loop for l from 1 to mult do
          (setf labels (append labels (expand_grid (grid memory_tune)))))
    (loop for label in labels do 
          (progn
            (setf listofemptyevents 
                  (append listofemptyevents 
                          (list (NewAudioHarmbeat (nth 0 label) (nth 1 label) i duration))))
            (setf i (+ i 1))))
    (NewImprovizer listofemptyevents duration)))


(defun NewSymbolicRealtimeImprovizer_AudioHarmbeats (memory_tune duration mult)
  (let* ((listofemptyevents '()) (i 0) (labels '()))
    (loop for l from 1 to mult do
          (setf labels (append labels (expand_grid (grid memory_tune)))))
    (loop for label in labels do 
          (progn
            (setf listofemptyevents 
                  (append listofemptyevents 
                          (list (NewAudioHarmbeat (nth 0 label) (nth 1 label) i duration))))
            (setf i (+ i 1))))
    (NewRealTimeImprovizer listofemptyevents duration)))


(defun NewSymbolicImprovizer_AudioDescrBeats (memory_tune duration mult)
  (let* ((listofemptyevents '()) (i 0) (labels '()))
    (loop for l from 1 to mult do
          (setf labels (append labels (expand_grid (grid memory_tune)))))
    (loop for label in labels do 
          (progn
            (setf listofemptyevents 
                  (append listofemptyevents 
                          (list (NewAudioDescrBeat
                                 (nth 0 label) (nth 1 label) i duration (datafromanalysis memory_tune) nil nil))))
            (setf i (+ i 1))))
    (NewImprovizer listofemptyevents duration)))


(defun NewSymbolicRealtimeImprovizer_AudioDescrBeats (memory_tune duration mult)
  (let* ((listofemptyevents '()) (i 0) (labels '()))
    (loop for l from 1 to mult do
          (setf labels (append labels (expand_grid (grid memory_tune)))))
    (loop for label in labels do 
          (progn
            (setf listofemptyevents 
                  (append listofemptyevents 
                          (list (NewAudioDescrBeat 
                                 (nth 0 label) (nth 1 label) i duration (datafromanalysis memory_tune) nil nil))))
            (setf i (+ i 1))))
    (NewRealTimeImprovizer listofemptyevents duration)))


(defun NewSymbolicImprovizer_AudioHarmbeats_from_list (l mult)
  (let* ((listofemptyevents '()) (i 0) (labels '()))
    (loop for memory_tune in l do
          (loop for l from 1 to mult do
                (setf labels (append labels (expand_grid (grid memory_tune))))))
    (loop for label in labels do 
          (progn
            (setf listofemptyevents 
                  (append listofemptyevents 
                          (list (NewAudioHarmbeat (nth 0 label) (nth 1 label) i duration))))
            (setf i (+ i 1))))
    (NewImprovizer listofemptyevents duration)))

