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
                          (sethash mrk-partials i (make-partial :t-list (list (frametime fr) (frametime fr))
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
                                    (list (car (partial-t-list p)) (frametime fr)))))))
             
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



;;;============================================
;;; WRITE...
;;;============================================

(defun make-1TRC-frames (partials)
  (merge-frame-data 
   (sort 
    (loop for partial in partials 
          for i = 1 then (+ i 1) append
          (loop for time in (partial-t-list partial)
                for n from 0 collect
                (make-instance 'SDIFFrame :frametime time :streamid 0 
                               :frametype "1TRC"
                               :lmatrices (list (make-instance 'SDIFMatrix :matrixtype "1TRC"
                                                             :num-elts 1 :num-fields 4
                                                             :data (list (list i)
                                                                         (list (nth n (partial-f-list partial)))
                                                                         (list (or (nth n (partial-a-list partial)) 1.0))
                                                                         (list (or (nth n (partial-ph-list partial)) 0)))
                                                             )))
                ))
    '< :key 'frametime)))

(defmethod* partials->sdif ((partials list) &optional (outpath "partials.sdif"))
  :icon 264
  :indoc '("a list of partials" "output pathname")
  :doc "Saves the contents of <partials> as an SDIF file in <outpath>.

Data is stored as a sequence of 1TRC frames containing 1TRC matrices.
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
                (let ((frames (make-1TRC-frames partials)))
                  (loop for frame in frames do
                        (sdif-write frame sdiffileptr))))
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