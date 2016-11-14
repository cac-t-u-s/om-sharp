(in-package :om)


;;;==========================
;;; READ CHORDS AND PARTIALS
;;;==========================

(defstruct partial (t-list) (f-list) (a-list) (ph-list))

(defmethod chord-seq-raw-data ((self sdiffile) &optional (stream nil))
   (let ((frames (GetSDIFFrames self :sid stream))
         (mrk-partials (make-hash-table))
         (trc-partials (make-hash-table))
         bmat emat pmat partial-list)
    
     (loop for fr in frames do
           (cond ((string-equal "1MRK" (frametype fr))
                  (loop for mat in (lmatrices fr) do
                        (cond ((string-equal "1BEG" (matrixtype mat)) (setf bmat mat))
                              ((string-equal "1END" (matrixtype mat)) (setf emat mat))
                              ((string-equal "1TRC" (matrixtype mat)) (setf pmat mat))
                              (t nil)))
                  (when bmat 
                    ;;; a begin matrix : set time info
                    (loop for i in (get-col bmat "Index") do
                          (sethash mrk-partials i (make-partial :t-list (list (date fr) (date fr))
                                                                :f-list nil
                                                                :a-list nil))))
                  (when pmat 
                    ;;; a parameter matrix : add data in partials
                    (loop for i in (get-col bmat "Index") 
                          for f in (get-col bmat "Frequency") 
                          for a in (get-col bmat "Amplitude") 
                          do (let ((p (gethash i mrk-partials)))
                               (when p
                                 (setf (partial-f-list p) (list f f))
                                 (setf (partial-a-list p) (list a a))
                                 ))))
                  (when emat 
                    ;;; a end matrix: find the partial, set duration and put in the final list 
                    (loop for i in (get-col bmat "Index") do
                          (let ((p (gethash i mrk-partials)))
                            (when p
                              (setf (partial-t-list p) 
                                    (list (car (partial-t-list p)) (date fr)))))))
             
                  (setf bmat nil)
                  (setf emat nil)
                  (setf pmat nil))
                 
                 ((or (string-equal "1TRC" (frametype fr)) (string-equal "1HRM" (frametype fr)))
                  (loop for mat in (lmatrices fr) do
                     (when (or (string-equal "1TRC" (matrixtype mat)) (string-equal "1TRC" (matrixtype mat)))
                       (loop for i in (get-col mat "Index") 
                             for f in (get-col mat "Frequency") 
                             for a in (get-col mat "Amplitude") 
                             for ph in (get-col mat "Phase") 
                             do (let ((p (gethash i trc-partials)))
                                  (if p
                                      (setf (partial-t-list p) (append (partial-t-list p) (list (date fr)))
                                            (partial-f-list p) (append (partial-f-list p) (list f))
                                            (partial-a-list p) (append (partial-a-list p) (list a))
                                            (partial-ph-list p) (append (partial-ph-list p) (list ph)))
                                    
                                    (sethash trc-partials i 
                                             (make-partial :t-list (list (date fr))
                                                           :f-list (list f) :a-list (list a) :ph-list (list ph))))
                                  )))))
                 ))
          
     
     (maphash #'(lambda (key p) (push p partial-list)) mrk-partials)
     (maphash #'(lambda (key p) (push p partial-list)) trc-partials)
     (sort (reverse partial-list) '< :key #'(lambda (p) (car (partial-t-list p))))
     
     ))


(defmethod* GetSDIFPartials ((self sdiffile) &optional (stream nil))
   :indoc '("an SDIF file")
   :doc "Return a list of partial structures from an sdif file (using 1TRC or 1MRK frames)

<stream> selects a specific SDIF stream (usually corresponding to a channel in audio analysis files.
"
   (chord-seq-raw-data self stream))


(defmethod* GetSDIFChords ((self sdiffile) &optional (stream nil))
   :indoc '("an SDIF file")
   :doc "Returns a list of chords data from an sdif file (using 1MRK / 1TRC frames).

Chords are formatted as (pitch [Hz]  onset [s]  duration [s]  velocity [lin]).

<stream> selects a specific SDIF stream (usually corresponding to a channel in audio analysis files.
"
   :icon 639
   
   (mapcar #'(lambda (p)
               (let ((t1 (list-min (partial-t-list p))) 
                     (t2 (list-max (partial-t-list p))))
                 (list (om-mean (partial-f-list p)) 
                       t1 (- t2 t1)
                       (om-mean (partial-a-list p)))))
           (chord-seq-raw-data self stream)))
                   
   
(defmethod* SDIF->chord-seq ((self sdiffile) &optional (stream nil))
   :indoc '("an SDIF file")
   :doc "Generates a CHORD-SEQ instance from the 1TRC or 1MRK frame data in <self>.

Internally calls and formats data from GetSDIFChords.

<stream> selects a specific SDIF stream (usually corresponding to a channel in audio analysis files.
"
   :icon 639
   (let* ((chord-data (sort (GetSDIFChords self stream) '< :key 'cadr))
          (chords nil) (cseqdata nil))
     (loop for note in chord-data do
           ;;; note = (pitch onset dur vel)
           ;;; (car chords) = (onset (pitches) (durs) (vels)) 
           (if (and (car chords) 
                    (= (second note) (car (car chords))))
               ;;; add note to chord
               (setf (car chords)
                     (list (first (car chords))
                           (append (second (car chords)) (list (first note)))
                           (append (third (car chords)) (list (third note)))
                           (append (fourth (car chords)) (list (fourth note)))))
             ;;; else create new chord
             (push (list (second note) (list (first note)) (list (third note)) (list (fourth note)))
                   chords)))
     (setf cseqdata (mat-trans chords))
     (make-instance 'chord-seq
                    :lonset (om-round (om* (first cseqdata) 1000))
                    :lmidic (om-round (f->mc (second cseqdata)))
                    :ldur (om-round (om* (third cseqdata) 1000))
                    :lvel (om-round (om-scale (fourth cseqdata) 50 127)))))

