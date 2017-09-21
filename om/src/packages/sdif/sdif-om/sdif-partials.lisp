(in-package :om)


;;;============================================
;;; READ/WRITE CHORDS AND PARTIALS
;;;============================================

;;; A utility structure to deal with partials
(defstruct partial (t-list) (f-list) (a-list) (ph-list))

;;; the default struct accessors can not (yet?) be instaniated as boxes in OM
(defun mk-partial (&key t-list f-list a-list ph-list)
  (make-partial :t-list t-list :f-list f-list :a-list a-list :ph-list ph-list))

(defun partial-times (p) (partial-t-list p))
(defun partial-freqs (p) (partial-f-list p))
(defun partial-amps (p) (partial-a-list p))


;;;============================================
;;; READ...
;;;============================================

;;; GetSDIFFrames will handle the dispatch wrt. type of self
(defmethod chord-seq-raw-data ((self t) &optional (stream nil))
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
                              (t nil))
                  
                        (when bmat 
                          ;;; a begin matrix : set time info
                          (loop for i in (get-col bmat "Id") do
                                (sethash mrk-partials i (make-partial :t-list (list (frametime fr) (frametime fr))
                                                                      :f-list nil
                                                                      :a-list nil))))
                        (when pmat 
                          ;;; a parameter matrix : add data in partials
                          (loop for i in (get-col pmat "Index") 
                                for f in (get-col pmat "Frequency") 
                                for a in (get-col pmat "Amplitude") 
                                do (let ((p (gethash i mrk-partials)))
                                     (when p
                                       (setf (partial-f-list p) (list f f))
                                       (setf (partial-a-list p) (list a a))
                                       ))))
                        (when emat 
                          ;;; a end matrix: find the partial, set duration and put in the final list 
                          (loop for i in (get-col emat "Id") do
                                (let ((p (gethash i mrk-partials)))
                                  (when p
                                    (setf (partial-t-list p) 
                                          (list (car (partial-t-list p)) (frametime fr)))))))
             
                        (setf bmat nil)
                        (setf emat nil)
                        (setf pmat nil)))
                 
                 ((or (string-equal "1TRC" (frametype fr)) (string-equal "1HRM" (frametype fr)))
                  (loop for mat in (lmatrices fr) do
                     (when (or (string-equal "1TRC" (matrixtype mat)) (string-equal "1TRC" (matrixtype mat)))
                       (loop for i in (get-col mat "Index") 
                             for f in (get-col mat "Frequency") 
                             for a in (get-col mat "Amplitude") 
                             for ph in (get-col mat "Phase") 
                             do (let ((p (gethash i trc-partials)))
                                  (if p
                                      (setf (partial-t-list p) (append (partial-t-list p) (list (frametime fr)))
                                            (partial-f-list p) (append (partial-f-list p) (list f))
                                            (partial-a-list p) (append (partial-a-list p) (list a))
                                            (partial-ph-list p) (append (partial-ph-list p) (list ph)))
                                    
                                    (sethash trc-partials i 
                                             (make-partial :t-list (list (frametime fr))
                                                           :f-list (list f) :a-list (list a) :ph-list (list ph))))
                                  )))))
                 ))
           
     (maphash #'(lambda (key p) (declare (ignore key)) (push p partial-list)) mrk-partials)
     (maphash #'(lambda (key p) (declare (ignore key)) (push p partial-list)) trc-partials)
     (sort (reverse partial-list) '< :key #'(lambda (p) (car (partial-t-list p))))
     
     ))


(defmethod* GetSDIFPartials ((self t) &optional (stream nil))
   :indoc '("an SDIF file")
   :doc "Return a list of partial structures from an sdif file (using 1TRC or 1MRK frames)

<stream> selects a specific SDIF stream (usually corresponding to a channel in audio analysis files.
"
   (chord-seq-raw-data self stream))


(defmethod* GetSDIFChords ((self t) &optional (stream nil))
   :indoc '("an SDIF file")
   :doc "Returns a list of chords data from an sdif file (using 1MRK / 1TRC frames).

Chords are formatted as (pitch [Hz]  onset [s]  duration [s]  velocity [lin]).

<stream> selects a specific SDIF stream (usually corresponding to a channel in audio analysis files.
" 
   (mapcar #'(lambda (p)
               (let ((t1 (list-min (partial-t-list p))) 
                     (t2 (list-max (partial-t-list p))))
                 (list (om-mean (partial-f-list p)) 
                       t1 (- t2 t1)
                       (om-mean (partial-a-list p)))))
           (chord-seq-raw-data self stream)))
                   
   
(defmethod* SDIF->chord-seq ((self t) &optional (stream nil))
   :indoc '("an SDIF file")
   :doc "Generates a CHORD-SEQ instance from the 1TRC or 1MRK frame data in <self>.

Internally calls and formats data from GetSDIFChords.

<stream> selects a specific SDIF stream (usually corresponding to a channel in audio analysis files.
"
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


;;;============================================
;;; WRITE...
;;;============================================

(defun make-1TRC-frames (partials &optioanl separate-streams)
  (let ((frames 
         (sort 
          (loop for partial in (sort partials '< :key #'(lambda (p) (car (partial-t-list p)))) 
                for i = 0 then (+ i 1) append
                (let ((1-partial-frames 
                       (loop for time in (partial-t-list partial)
                             for n from 0 collect
                             (make-instance 'SDIFFrame :frametime time :frametype "1TRC"
                                            :streamid (if separate-streams i 0) 
                                            :lmatrices (list (make-instance 'SDIFMatrix :matrixtype "1TRC"
                                                                            ;:num-rows 1 :num-cols 4
                                                                            :data (list (list (1+ i))
                                                                                        (list (nth n (partial-f-list partial)))
                                                                                        (list (or (nth n (partial-a-list partial)) 1.0))
                                                                                        (list (or (nth n (partial-ph-list partial)) 0)))
                                                                            ))))))
                  (setf (nth 2 (data (car (lmatrices (car (last 1-partial-frames)))))) (list 0.0))
                  1-partial-frames))
          '< :key 'frametime)))
    (if separate-streams 
        frames
      (merge-frame-data frames))
    ))

(defun get-partial-f-a-ph-at-time (partial time)
  (let ((pos (position time (partial-t-list partial) :test '=)))
    (if pos 
        (list (nth pos (partial-f-list partial))
              (if (partial-a-list partial) (nth pos (partial-a-list partial)) 1.0)
              (if (partial-ph-list partial) (nth pos (partial-ph-list partial)) 0))
      (let* ((pos-before (position time (partial-t-list partial) :test '>= :from-end t))
             (t1 (nth pos-before (partial-t-list partial)))
             (t2 (nth (1+ pos-before) (partial-t-list partial))))
             
        (list (linear-interpol t1 t2
                               (nth pos-before (partial-f-list partial))
                               (nth (1+ pos-before) (partial-f-list partial))
                               time)
              (if (partial-a-list partial) 
                  (linear-interpol t1 t2
                                   (nth pos-before (partial-a-list partial))
                                   (nth (1+ pos-before) (partial-a-list partial))
                                   time)
                1.0)
              (if (partial-ph-list partial) 
                  (linear-interpol t1 t2 
                                   (nth pos-before (partial-ph-list partial))
                                   (nth (1+ pos-before) (partial-ph-list partial))
                                   time)
                0)))
      )))

(defun make-1TRC-frames-synchronous (partials &optional sr)
  (flet 
      ((make-frame-at-time (p-list time)
         (let ((frame (make-instance 'SDIFFrame :frametime time :streamid 0 
                                     :frametype "1TRC"))
               (data (sort 
                      (loop for partial in p-list 
                            for i = 1 then (+ i 1)
                            when (and (>= time (car (partial-t-list partial))) 
                                      (<= time (car (last (partial-t-list partial)))))
                            collect (cons i (get-partial-f-a-ph-at-time partial time)))
                      '< :key 'car)))
              
              (setf (lmatrices frame)
                    (list (make-instance 'SDIFMatrix :matrixtype "1TRC"
                                         ;:num-cols 4
                                         ;:num-rows (length data)
                                         :data (mat-trans data))))
              frame)))
    (if sr 
        
        (let* ((t-lists (mapcar 'partial-t-list partials))
               (tmin (reduce 'min (mapcar 'list-min t-lists)))
               (tmax (reduce 'max (mapcar 'list-max t-lists))))
          (loop for frametime from tmin to (+ tmax .2) by sr collect
                (make-frame-at-time partials frametime)))
      
      (let ((timeslist (sort (remove-duplicates 
                              (loop for partial in partials 
                                    append (partial-t-list partial))
                              :test '=)
                             '<)))
        (loop for frametime in timeslist collect
              (make-frame-at-time partials frametime)))

      )))


(defmethod* partials->sdif ((partials list) &key (outpath "partials.sdif") (frame-rate 0.01))
  :indoc '("a list of partials" "output pathname")
  :initvals '(nil "partials.sdif" 0.01)
  :doc "Saves the contents of <partials> as an SDIF file in <outpath>.

Data is stored as a sequence of 1TRC frames containing 1TRC matrices.

SDIF partials are resampled in synchronous frames at <frame-rate>
"
  (let ((out-path (cond ((pathnamep outpath) outpath)
                         ((stringp outpath) (outfile outpath))
                         (t (om-choose-new-file-dialog)))))
    (when out-path
      (let ((sdiffileptr (sdif::sdif-open-file out-path sdif::eWriteFile)))
        (if sdiffileptr
          (unwind-protect 
              (progn (sdif::SdifFWriteGeneralHeader sdiffileptr)
                (sdif-write (default-om-NVT) sdiffileptr)
                (sdif::SdifFWriteAllASCIIChunks sdiffileptr)
                (when partials
                  (loop for frame in (make-1trc-frames-synchronous partials frame-rate)
                        do (sdif-write frame sdiffileptr))))
            (sdif::SDIFFClose sdiffileptr))
          (om-beep-msg "Could not open file for writing: ~A" out-path))
        (probe-file out-path)
        ))))


#|


(defmethod! chordseq-to-datalist ((self chord-seq))
  (let* ((ind -1)
         (chord-data nil)
         (enddata nil)
         newendlist)
    ;;; recup les donnees
    (setf chord-data 
          (loop for chord in (inside self) 
                collect (list (offset->ms chord)
                              (loop for note in (inside chord) 
                                    do (pushr (list (+ (offset->ms chord) (dur note)) (incf ind)) enddata)
                                    collect (list ind (mc->f (midic note)) (/ (vel note) 1270.0) 0)))))
    ;;; mettre tous les end simulatnes dans une meme frame
    (setf enddata (sort enddata '< :key 'car))
    (let* (tmptime)
      (setf newendlist (loop while enddata do
                             (setf tmptime (car (car enddata)))
                             collect (list tmptime (loop while (equal (car (car enddata)) tmptime) collect (second (pop enddata))))) 
            ))
    (sort (append chord-data newendlist) '< :key 'car)
    ))

(defun write-mrk-frame (fileptr data) 
   (let (beg end)
     (loop for elt in (second data) do 
           (if (consp elt) 
             (pushr elt beg)
             (pushr elt end)))
  (let* ((time (/ (car data) 1000.0))
         (datatype 4)
         (numlinesb (length beg))
         (numlinese (length end))
         (framesize (+ 32 (calc-pad (* 4 datatype numlinesb)) (calc-pad (* 1 datatype (+ numlinesb numlinese)))))
         (beg-values (om-make-pointer (* 1 datatype numlinesb)))
         (trc-values (om-make-pointer (* 4 datatype numlinesb)))
         (end-values (om-make-pointer (* 1 datatype numlinese)))
         (nbmat 0))
    (when beg (setf nbmat (+ nbmat 2))) 
    (when end (setf nbmat (+ nbmat 1)))
    (sdif::SdifFSetCurrFrameHeader fileptr (sdif::SdifStringToSignature  "1MRK") framesize nbmat 0 (coerce time 'double-float))
    (sdif::SdifFWriteFrameHeader fileptr)
    (loop for i from 0 to (- numlinesb 1)
          for note in beg do
          (om-write-ptr beg-values (* i datatype) :float (coerce (car note) 'single-float))
          (om-write-ptr trc-values (* (* i 4) datatype) :float (coerce (car note) 'single-float) )
          (om-write-ptr trc-values (* (+ (* i 4) 1) datatype) :float (coerce (second note) 'single-float) )
          (om-write-ptr trc-values (* (+ (* i 4) 2) datatype) :float (coerce (third note) 'single-float) )
          (om-write-ptr trc-values (* (+ (* i 4) 3) datatype) :float (coerce (fourth note) 'single-float) )
          )
    (loop for i from 0 to (- numlinese 1)
          for note in end do
          (om-write-ptr end-values (* i datatype) :float (coerce note 'single-float))
          )
    (when beg
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1BEG") datatype numlinesb 1 beg-values)
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1TRC") datatype numlinesb 4 trc-values))
    (when end
      (sdif::SdifFWriteMatrix fileptr (sdif::SdifStringToSignature "1END") datatype numlinese 1 end-values))
    (om-free-pointer trc-values)
    (om-free-pointer beg-values)
    (om-free-pointer end-values)
  )))
     
 

(defun make-1MRK-frames (datalist)
  (let ((last-time nil)
        (frameslist nil))
    (loop for data in datalist do
          (if (and last-time (= (car data) last-time))
            (setf (cadr (last-elem frameslist))
                  (append (cadr (last-elem frameslist)) (cadr data)))
            (pushr data frameslist))
          (setf last-time (car data)))
    frameslist
    ))


(defmethod! chord-seq->sdif ((self chord-seq) &optional (outpath "cseq.sdif"))
  :icon 264
  :indoc '("a CHORD-SEQ" "output pathname")
  :doc "Saves the contents of <self> (a CHORD-SEQ) as an SDIF file in <outpath>.

Data is stored as a sequence of 1MRK frames containing 1BEG and 1END matrices for begin and end times, and 1TRC matrices for chords values.
"
  (let* ((error nil) time outfile
         (out-path (cond ((pathnamep outpath) outpath)
                         ((stringp outpath) (outfile outpath))
                         (t (om-choose-new-file-dialog)))))
    (when out-path
      (setf outfile (sdif-open-file (om-path2cmdpath out-path) 1))
      (sdif::SdifFWriteGeneralHeader outfile)
      (write-nvt-tables outfile (list (default-om-NVT)))
      (write-sdif-types outfile  "{1FTD 1MRK {1TRC chord_seq_partials;}}")
      (sdif::SdifFWriteAllASCIIChunks outfile)
      (let ((datalist (chordseq-to-datalist self)))
        (setf datalist (make-1MRK-frames datalist)) 
        (loop for data in datalist
              while (not error) do
          (write-mrk-frame outfile data)
          ))
  (sdif-close-file outfile)
  (probe-file out-path)
  )))




|#