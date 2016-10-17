;;;================================================================================================================
;;; PM2 Additive Sound Analysis/Systhesis
;;; jb - 2005 
;;;================================================================================================================


(in-package :om)

;;; AUTO UPDATE COMPATIBILITY

(defun cdr-inputs-to-keywords (fun inputs)  
  (cons (car inputs) 
        (loop for input in (cdr inputs)
              for argname in (cdr (ordered-arg-list fun))
              collect (if (keyword-input-p input) input
                        (make-instance 'input-keyword
                                       :name (name input) 
                                       :value argname
                                       :def-value (value input)
                                       :doc-string (doc-string input)
                                       :val-menu (if (menu-input-p input) (thepopup input) nil)
                                       :box-ref (box-ref input)
                                       :connected? (connected? input)
                                       )))))

(defmethod update-inputs ((self (eql 'chord-seq-analysis)) inputs)
  (cdr-inputs-to-keywords self inputs))

(defmethod update-inputs ((self (eql 'partial-tracking)) inputs)
  (cdr-inputs-to-keywords self inputs))

(defmethod update-inputs ((self (eql 'pm2-f0)) inputs)
  (cdr-inputs-to-keywords self inputs))

(defmethod update-inputs ((self (eql 'pm2-add-synth)) inputs)
  (cdr-inputs-to-keywords self inputs))

;;;================================================================================================================
;;; PM2 TOOLS
;;;================================================================================================================

(defmethod make-pm2-chord-file ((self list) &optional (outfile "tmpchords"))
  (let ((file (if (pathnamep outfile) outfile (tmpfile outfile))))
    (with-open-file (out file :direction :output 
                         :if-does-not-exist :create :if-exists :supersede)
      (if (listp (car self))
          (loop for p in self do 
                (format out "~F~%~F " (car p) (cadr p)))
        (progn
          (format out "~F " (car self))
          (loop for p in (butlast (cdr self)) do 
                (format out "~F~%~F " p p))    
          (format out "~F~%" (car (last self))))))
        file))
  
(defmethod make-pm2-chord-file ((self sound) &optional (outfile "tmpchords"))
  (let ((file (if (pathnamep outfile) outfile (tmpfile outfile)))
        (mrk (if (markers self) (markers self) (list 0.0))))
    (make-pm2-chord-file (append mrk (list (sound-dur self))) file)))

(defun trunc (val n) 
  (/ (round (* val (expt 10 n))) (float (expt 10 n))))



;;;================================================================================================================
;;; PM2 CHORD SEQ ANALYSIS
;;;================================================================================================================

(defmethod! chord-seq-analysis ((self string) &key
                                begin-t end-t
                                markers
                                (max-partials 12) (amp-treshold -40)
                                (analysis-type "averaged-spectrum")
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                                (out "chordseqs.sdif"))
  :initvals '(nil nil nil nil 12 -40 "averaged-spectrum" 4096 4096 256 "hanning" "chordseqs.sdif")
  :menuins (list
            (list 6 '(("Averaged Spectrum" "averaged-spectrum") ("Inharmonic Partial Averaging" "inharmonic-partial-averaging")))
            (list 10 '(("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :icon 954
  :doc "Tracks sinusoidal chords in an audio file and returns an SDIF sound description file.

- FileName : the pathname of the sound file (Aiff) to be analysed

- begin-t : begin time of analysis (s)

- end-t : end time of analysis (s)

- markers : markers for analysis (text file). If nil, markers of filename are used.

- max-partials : the maximum number of simultaneous partials

- amp-treshold : Amplitude treshold of analysis (-120 -> 0 dB)

- analysis-type : type of analysis ('averaged-spectrum or 'inharmonic-partial-averaging).
                  for inharmonic partial averaging, connect partial connection parameters if needed : 
                           - Relative frequency derivation (mc)
                           - Constant frequency derivation (Hz)
                           - Relative amplitude derivation (%)
                           - Source partial neighbors
                           - Target partial neighbors (>= source partial neighbors)
                           - Time gap to connect over (s)
                           - Frequency gap to connect over (mc)
                           - Minimum partial fragment length (s)
                           - Relative Min. partial length [0.0-1.0]

- windowsize : the number of samples of the analysis window

- fftsize : the number of points of fft

- step : the number of samples between two successive analysis windows

- windowtype : the shape of the analysis window

"

;;;pm2  -S"/Applications/AudioSculpt 2.4.2/Sounds/africa.aiff" -Aseqs  -N2048 -M2048 -I256  -Wblackman  
;;;-Os -p0 -q15 -m40.0 --chords="$USERHOME/Temp/chords"  
;;;"/Applications/AudioSculpt 2.4.2/ChordSeqs/tempChord-YMS3.cs.sdif"
;;;
;;;pm2  -S"/Applications/AudioSculpt 2.4.2/Sounds/africa.aiff" -Aseqp  -N2048 -M2048 -I256  -Wblackman  
;;;-Os -p0 -q15 -m40.0 -a0 -r0 -Ct0.017 -Cf0.029 --devFR=0.012 --devFC=0.0 --devA=0.5 --devM=1 --devK=3 -L0.009 --chords="$USERHOME/Temp/chords"  
;;;"/Applications/AudioSculpt 2.4.2/ChordSeqs/tempChord-iGG4.cs.sdif"

  (if (and *PM2-PATH* (probe-file *PM2-PATH*))
    (let ((outname (if out 
                      (if (pathnamep out) out (outfile out))
                      (om-choose-new-file-dialog :prompt "Choose a SDIF output file for Chord Sequence Analysis"
                                                 :directory (def-save-directory)))))
       (when outname
         (setf *last-saved-dir* (make-pathname :directory (pathname-directory outname)))
         (let* ((unix-outname (om-path2cmdpath outname))
                (typestr (if (and (stringp analysis-type) (string-equal analysis-type "averaged-spectrum")) "-Aseqs" "-Aseqp"))
                (timestr (string+ "" (if begin-t (format nil "-B~D " begin-t) "")
                                  (if end-t (format nil "-E~D " end-t) "")))
                (chordsfile (cond ((pathnamep markers) markers)
                                  ((stringp markers) (if (probe-file markers) markers (tmpfile markers)))
                                  (t (push (make-pm2-chord-file markers (tmpfile "tmpchords")) *tmpparfiles*)
                                     (tmpfile "tmpchords"))))
                (fftstr (format nil "-N~D -M~D -W~D -I~D " fftsize windowsize windowtype step))
                (cmd (format nil "~s -v -S~s ~A ~A ~A -OS -p0 -q~D -m~D -a0 -r0 ~A --chords=~s ~s" 
                             (om-path2cmdpath *PM2-PATH*)
                             (om-path2cmdpath self)
                             typestr
                             timestr
                             fftstr 
                             max-partials
                             (float (- amp-treshold))
                    ;(car smoothing-enveloppe)
                    ;(cadr smoothing-enveloppe)
                             (if (and (stringp analysis-type) (string-equal analysis-type "averaged-spectrum")) ""
                               (let ((data (if (consp analysis-type) analysis-type '(20 0.0 50 1 3 0.017 50 0.009 0.5))))
                                 (format nil "-Ct~D -Cf~D --devFR=~D --devFC=~D --devA=~D --devM=~D --devK=~D -L~D -l~D"
                                         (nth 5 data)
                                         (trunc (- (expt 2 (/ (nth 6 data) 1200.0)) 1) 3)
                                         (trunc (- (expt 2 (/ (nth 0 data) 1200.0)) 1) 3)
                                         (float (nth 1 data))
                                         (/ (nth 2 data) 100.0)
                                         (nth 3 data)
                                         (if (< (nth 4 data) (nth 3 data)) (nth3 data) (nth 4 data))
                                         (nth 7 data)
                                         (nth 8 data)
                                         )))
                             (om-path2cmdpath chordsfile)
                             unix-outname))
                )
           (print (string+ "PM2 PROCESS : " cmd))
           (om-cmd-line cmd *sys-console*)
           ;(when *delete-inter-file* (clean-tmp-files))
           outname)))
         (om-beep-msg "PM2 not found! Set path to pm2 in the OM preferences.")))

(defmethod! chord-seq-analysis ((self pathname) &key
                                begin-t end-t
                                markers
                                (max-partials 12) (amp-treshold -40)
                                (analysis-type "averaged-spectrum")
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                                (out "chordseqs.sdif"))
  (chord-seq-analysis (namestring self) :begin-t begin-t :end-t end-t
                                :markers markers
                                :max-partials max-partials :amp-treshold amp-treshold
                                :analysis-type analysis-type
                                :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
                                :out out))

(defmethod! chord-seq-analysis ((self sound) &key
                                begin-t end-t
                                markers
                                (max-partials 12) (amp-treshold -40)
                                (analysis-type "averaged-spectrum")
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                                (out "chordseqs.sdif"))
  (let* ((markers-file (if markers markers
                          (let ((f (make-pm2-chord-file self)))
                            (push f *tmpparfiles*)
                            f)))
         (sdiffile (chord-seq-analysis (om-sound-file-name self) 
                                       :begin-t begin-t :end-t end-t
                                       :markers markers-file
                                       :max-partials max-partials :amp-treshold amp-treshold
                                       :analysis-type analysis-type
                                       :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
                                       :out out)))
    sdiffile))
  
;;;================================================================================================================
;;; PM2 PARTIAL TRACKING
;;;================================================================================================================

(defmethod! partial-tracking ((self string) &key
                                begin-t end-t
                                (max-partials 12) (amp-treshold -40)
                                (analysis-type "inharmonic") (analysis-params '(20 0.0 50 1 3 0.017 50 0.009))
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning") (smoothing-enveloppe  '(0.0 0.0))
                                (out "partials.sdif"))
  :initvals '(nil nil nil 12 -40 "inharmonic" (20 0.0 50 1 3 0.017 50 0.009) 4096 4096 256 "hanning" (0.0 0.0) "partials.sdif")
  :menuins (list
            (list 5 '(("Harmonic" "harmonic") ("Inharmonic" "inharmonic")))
            (list 10 '(("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :icon 953
  :doc "Tracks sinusoidal partial in an audio file and returns an SDIF sound description file.

- FileName : the pathname of the sound file (Aiff) to be analysed

- begin-t : begin time of analysis (s)

- end-t : end time of analysis (s)

- max-partials : the maximum number of simultaneous partials

- amp-treshold : Amplitude treshold of analysis (-120 -> 0 dB)

- analysis-type : type of analysis (harmonic or inharmonic).

- analysis-params : parameters for tracking                 
                    - for inharmonic partial tracking : 
                           - Relative frequency derivation (mc) (default 20)
                           - Constant frequency derivation (Hz) (default 0.0)
                           - Relative amplitude derivation (%) (default 50)
                           - Source partial neighbors (default 1)
                           - target partial neighbors (>= source partial neighbors) (default 3)
                           - time gap to connect over (s) (default 0.017)
                           - frequency gap to connect over (mc) (default 50)
                           - minimum partial length (s) (default 0.009)
                    - for harmonic partial tracking : an SDIF f0 analysis file (automatically generated if nil)

- windowsize : the number of samples of the analysis window

- fftsize : the number of points of fft

- step : the number of samples between two successive analysis windows

- windowtype : the shape of the analysis window

- smoothing-enveloppe : smoothing enveloppe attack and release times (s) (default '(0.0 0.0))

"

;;;pm2  -S"/Applications/AudioSculpt 2.4.2/Sounds/africa.aiff" -Apar  -N2048 -M2048 -I256  -Wblackman  
;;;-Os -P"/Applications/AudioSculpt 2.4.2/Fundamental/tempf0-ZnPI.f0.sdif" 
;;;-q15 -m68.0 -c0.83 -a0.02 -r0.03 -Ct0.017 -Cf0.029 --devFR=0.012 --devFC=0.1 --devA=0.51 --devM=5 --devK=5 -L0.009  
;;;"/Applications/AudioSculpt 2.4.2/Partials/temppart-Swtx.hrm.sdif"
;;;
;;;pm2  -S"/Applications/AudioSculpt 2.4.2/Sounds/africa.aiff" -Apar  -B-0.0 -E0.0 -N2048 -M2048 -I256  -Wblackman  
;;;-Os -p0 -q15 -m68.0 -a0.02 -r0.03 -Ct0.017 -Cf0.029 --devFR=0.012 --devFC=0.1 --devA=0.51 --devM=5 --devK=5 -L0.009  
;;;"/Applications/AudioSculpt 2.4.2/Partials/temppart-LbBy.trc.sdif"

  (if (and *PM2-PATH* (probe-file *PM2-PATH*))

  (let ((outname (if out 
                    (if (pathnamep out) out (outfile out))
                    (om-choose-new-file-dialog :prompt "Choose a SDIF output file for Partial Tracking Analysis"
                                               :directory (def-save-directory)))))
    (when outname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory outname)))
      (let* ((unix-outname (om-path2cmdpath outname))
             (timestr (string+ "" (if begin-t (format nil "-B~D " begin-t) "")
                               (if end-t (format nil "-E~D " end-t) "")))
             (fftstr (format nil "-N~D -M~D -W~D -I~D " fftsize windowsize windowtype step))
             (cmd (format nil "~s -v -S~s -Apar ~A ~A ~A -p1 --mode=0 -q~D -m~D -a~D -r~D ~A ~s" 
                          (om-path2cmdpath *PM2-PATH*)
                          (om-path2cmdpath self)
                          timestr
                          fftstr 
                          (if (string-equal analysis-type "harmonic") 
                              (let ((f0file (cond ((pathnamep analysis-params) analysis-params)
                                                  ((stringp analysis-params) (if (probe-file analysis-params) (pathname analysis-params) (tmpfile analysis-params)))
                                                  ((typep analysis-params 'sdiffile) (filepathname analysis-params))
                                                  (t (let ((tmpfile (tmpfile "ptrack-f0.sdif")))
                                                       (pm2-f0 self :fund-minfreq 50.0 :fund-maxfreq 2000.0 :spectrum-maxfreq 4000.0 :out tmpfile)
                                               (push tmpfile *tmpparfiles*)
                                               tmpfile)))))
                                (format nil " -P~s " (om-path2cmdpath f0file)))
                            "")
                    max-partials
                    (float (- amp-treshold))
                    (car smoothing-enveloppe)
                    (cadr smoothing-enveloppe)
                    (let ((data (if (consp analysis-params) analysis-params '(20 0.0 50 1 3 0.017 50 0.009))))
                      (format nil "-Ct~D -Cf~D --devFR=~D --devFC=~D --devA=~D --devM=~D --devK=~D -L~D"
                              (nth 5 data)
                              (trunc (- (expt 2 (/ (nth 6 data) 1200.0)) 1) 3)
                              (trunc (- (expt 2 (/ (nth 0 data) 1200.0)) 1) 3)
                              (float (nth 1 data))
                              (/ (nth 2 data) 100.0)
                              (nth 3 data)
                              (if (< (nth 4 data) (nth 3 data)) (nth3 data) (nth 4 data))
                              (nth 7 data)
                              ))
                    unix-outname))
             
             )
        (print (string+ "PM2 PROCESS : " cmd))
        (om-cmd-line cmd *sys-console*)
        (when *delete-inter-file* (clean-tmp-files))
        
        outname)))
      (om-beep-msg "PM2 not found! Set path to pm2 in the OM preferences.")))


(defmethod! partial-tracking ((self pathname) &key
                                begin-t end-t
                                (max-partials 12) (amp-treshold -40)
                                (analysis-type "inharmonic") (analysis-params '(20 0.0 50 1 3 0.017 50 0.009))
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning") (smoothing-enveloppe  '(0.0 0.0))
                                (out "partials.sdif"))
  (partial-tracking (namestring self) 
                    :begin-t begin-t :end-t end-t
                    :max-partials max-partials :amp-treshold amp-treshold
                    :analysis-type analysis-type :analysis-params analysis-params
                    :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype :smoothing-enveloppe smoothing-enveloppe 
                    :out out))

(defmethod! partial-tracking ((self sound) &key
                                begin-t end-t
                                (max-partials 12) (amp-treshold -40)
                                (analysis-type "inharmonic") (analysis-params '(20 0.0 50 1 3 0.017 50 0.009))
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning") (smoothing-enveloppe  '(0.0 0.0))
                                (out "partials.sdif"))
  (partial-tracking (om-sound-file-name self) 
                    :begin-t begin-t :end-t end-t
                    :max-partials max-partials :amp-treshold amp-treshold
                    :analysis-type analysis-type :analysis-params analysis-params
                    :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype :smoothing-enveloppe smoothing-enveloppe 
                    :out out))


;;;================================================================================================================
;;; PM2 SYNTHESIS
;;;================================================================================================================

(defun pm2-synthesis (partiels &key (attack 0.01) (release 0.01) sr res (outfile "pm2-out.aiff") nchannels)
  ;;;./pm2 -Asyn -R44100 -Stest.sdif out.aiff
  (if (and *PM2-PATH* (probe-file *PM2-PATH*))
      (let ((outname (if outfile 
                         (if (pathname-directory (pathname outfile)) outfile (outfile outfile))
                       (om-choose-new-file-dialog :prompt "Choose an output file"
                                                  :directory (def-save-directory))))
            (sr (or sr *audio-sr*))
            (res (or res *audio-res*)))
        (when outname
          (setf *last-saved-dir* (make-pathname :directory (pathname-directory outname)))
          (let* ((unix-outname (om-path2cmdpath outname))
                 (cmd (format nil "~s -Asyn -S~s ~A -a~D -r~D -R~D -Osa~D ~s" 
                              (om-path2cmdpath *PM2-PATH*)
                              (om-path2cmdpath partiels)
                              (if nchannels (format nil "--numchannels=~D" nchannels) "")
                              attack
                              release
                              sr
                              res
                              unix-outname)))
            
            (print (string+ "PM2 PROCESS : " cmd))
            (om-cmd-line cmd *sys-console*)
            (when *delete-inter-file* (clean-tmp-files))
            outname)))
    (progn
      (when *delete-inter-file* (clean-tmp-files))
      (om-beep-msg "PM2 not found! Set path to pm2 in the OM preferences."))))


(defmethod! pm2-add-synth ((partiels pathname) &key
                           (attack 0.01) (release 0.01)
                           sr res (outfile "pm2-out.aiff"))
  :initvals '(nil 0.01 0.01 nil nil "pm2-out.aiff")
  :indoc '("partials" "partials attack time (s)" "partials release time (s)" "sample rate" "resolution" "output pathname")
  :icon 953
  :doc "Realizes additive synthesis using pm2.

The input is a simple list of partials provided as an SDIFFILE or SDIF-BUFFER with 1MRK/1TRC frames.

CHORD-SEQ can also be connected and are iternally converted to an SDIF file as well.
"
  (pm2-synthesis partiels :attack attack :release release :sr sr :res res :outfile outfile))


(defmethod! pm2-add-synth ((partiels sdiffile) &key
                           (attack 0.01) (release 0.01)
                           sr res (outfile "pm2-out.aiff"))
 (pm2-synthesis (filepathname partiels) :attack attack :release release :sr sr :res res :outfile outfile
                :nchannels (length (sdifstreams partiels))))

(defmethod! pm2-add-synth ((seq sdif-buffer) &key
                           (attack 0.01) (release 0.01)
                           sr res (outfile "pm2-out.aiff"))
  (let* ((sdiftmp (tmpfile "chords.sdif"))
         (sdiffile (save-sdif-file seq :out sdiftmp)))
    (push sdiftmp *tmpparfiles*)
    (pm2-synthesis sdiffile :attack attack :release release :sr sr :res res :outfile outfile
                   :nchannels (length (remove-duplicates (mapcar 'streamid (lframes seq)))))
    ))


(defmethod! pm2-add-synth ((seq chord-seq) &key
                           (attack 0.01) (release 0.01)
                           sr res (outfile "pm2-out.aiff"))
  (let* ((sdiftmp (tmpfile "chords.sdif"))
         (sdiffile (chordseq->sdif seq sdiftmp)))
    (when sdiffile
      (push sdiftmp *tmpparfiles*)
      (pm2-add-synth sdiffile :attack attack :release release :sr sr :res res :outfile outfile)
      )))

(defmethod! pm2-add-synth ((seq score-element) &key
                           (attack 0.01) (release 0.01)
                           sr res (outfile "pm2-out.aiff"))
  (pm2-add-synth (objfromobjs seq (make-instance 'chord-seq))
                 :attack attack :release release :sr sr :res res :outfile outfile))



;;;================================================================================================================
;;; PM2 F0
;;;================================================================================================================

(defmethod! pm2-f0 ((self string) &key
                                begin-t end-t
                                (fund-minfreq 100) (fund-maxfreq 300) (spectrum-maxfreq 3000)
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                                (out "f0.sdif"))
  :initvals '(nil nil nil 100 300 3000 4096 4096 256 "hanning" "f0.sdif")
  :menuins (list
            (list 5 '(("Harmonic" "harmonic") ("Inharmonic" "inharmonic")))
            (list 10 '(("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :icon 955
  :doc "Fundamental frequency estimation.

- FileName : the pathname of the sound file (Aiff) to be analysed

- begin-t : begin time of analysis (s)

- end-t : end time of analysis (s)

- fund-minfreq : min F0 

- fund-maxfreq : max F0  

- spectrum-maxfreq : max analysis frequency

- windowsize : the number of samples of the analysis window

- fftsize : the number of points of fft

- step : the number of samples between two successive analysis windows

- windowtype : the shape of the analysis window

- smoothing-enveloppe : smoothing enveloppe attack and release times (s) (default '(0.0 0.0))

"

;;; 
;;;pm2 -Af0 --f0min=100 --f0max=300 --f0ana=3000 --f0use  -Sfile.wav -M1000 -I250 -N1024-m40 result.f0.sdif

  (if (and *PM2-PATH* (probe-file *PM2-PATH*))

      (let ((outname (if out 
                         (if (pathnamep out) out (outfile out))
                       (om-choose-new-file-dialog :prompt "Choose a SDIF output file for F0 analysis"
                                                  :directory (def-save-directory)))))
        (when outname
          (if (probe-file outname) (om-delete-file outname))
          (setf *last-saved-dir* (make-pathname :directory (pathname-directory outname)))
          (let* ((unix-outname (om-path2cmdpath outname))
             (beginstr (if begin-t (format nil "-B~D " begin-t) ""))
             (endstr (if end-t (format nil "-E~D " end-t) ""))
             (fftstr (format nil "-M~D -I~D -N~D -m40 -W~D" 
                              (if windowsize windowsize 4096) 
                              (if step step 250)
                              (if fftsize fftsize 4096)
                              (if windowtype windowtype "hanning")))
             (f0params (format nil "--f0min=~D --f0max=~D --f0ana=~D --f0use" 
                               fund-minfreq fund-maxfreq spectrum-maxfreq))
             (cmd (format nil "~s -Af0 ~A -S~s ~A ~A ~A ~s" 
                          (om-path2cmdpath *PM2-PATH*)
                          f0params
                          (om-path2cmdpath self)
                          beginstr endstr
                          fftstr  
                          (om-path2cmdpath outname)))
             )
        (show-message-win "F0 analysis...")
        (om-cmd-line cmd *sys-console*)
        (hide-message-win)
        (when *delete-inter-file* (clean-tmp-files))
        (if (probe-file outname)
          outname
          (om-message-dialog "Error in pm2 F0 analysis")))))
    (om-beep-msg "PM2 not found! Set path to pm2 in the OM preferences.")))


(defmethod! pm2-f0 ((self pathname) &key
                              begin-t end-t
                                (fund-minfreq 100) (fund-maxfreq 300) (spectrum-maxfreq 3000)
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                                (out "f0.sdif"))
  (pm2-f0 (namestring self) 
          :begin-t begin-t :end-t end-t
          :fund-minfreq fund-minfreq :fund-maxfreq fund-maxfreq :spectrum-maxfreq spectrum-maxfreq
          :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
          :out out))

(defmethod! pm2-f0 ((self sound) &key
                              begin-t end-t
                                (fund-minfreq 100) (fund-maxfreq 300) (spectrum-maxfreq 3000)
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                                (out "f0.sdif"))
  (pm2-f0 (om-sound-file-name self)  
          :begin-t begin-t :end-t end-t
          :fund-minfreq fund-minfreq :fund-maxfreq fund-maxfreq :spectrum-maxfreq spectrum-maxfreq
          :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
          :out out))


;;;================================================================================================================

#|

PM2 Help (vers. 1.0.2)
------------------------
pm2 -[Options...] <Output File>
-A<Action>                       (see below)
-S<Input Filename>               (either sound file or partial parameters in sdif format)
-P<Fundamental  Filename>        (.f0 or .sdif)
-k<Peaks Output Filename>        (SDIF)
-K<Peaks Input  Filename>        (SDIF)
-F<InData File Format>           (a:ASCII, b:Binary, S:SDIF,(default: S))
-O<Output File Format>           (a:ASCII, b:Binary, S:SDIF  (default: S))
-Os<Sound File Format>           (a[8|16|24|32|float|double]:AIFF, w[8|16|24|32|float|double]:WAV, i[16|float]:ircam, r[8|16|24|32|float|double]:RAW (default: aiff 16bit) )
-q<Number Of Partials>           (all by default)
-M<Window Analysis Size>         (1024 samples)
-N<Window FFT Size>              (1024 samples)
-I<Window Analysis Step>         (256 samples)
-W<Window Type>                  (rectangle, triangle, hanning (default), hanning2, hamming, blackman)
-m<maximum amplitude att>         (default: Inf dB compared to normalized maximum)
                                  (f0 analysis: 0.003162 dB)
-p<Use Phase>                     (default: 0 for synthesis/1 for analysis)
-R<Sampling Rate>                 (44100 Hz)
-B<Begin>                         (time)
-E<End>                           (time)
-C<num> (first channel == 1)      (1)
-c<Bandwidth Partial Seeve>       (0.5, coeff 0<c<1)
-a<Smoothing Envelope Attack>     (0.005 sec)
-r<Smoothing Envelope Release>    (0.005 sec)
 Partial connection options
-Ct<time gap to connect over>     (0 sec)
-Cf<rel. freq gap to connect over>  (0, Cf> 0)
--devFR=<rel. freq dev>[c]   (2e+01 cents),
                   given as factor or in cents with c suffix attached)
--devFC=<const freq dev partial>  (50 Hz)
--devA=<rel. ampl dev in partial> (0.5)
--devM=<source partial neighbors> (1)
--devK=<traget partial neighbors> (3)
-L<minimum partial length>        (0 sec)
-l<required partial dur in chord> (1 0<l<=1)
-j<F0 smoothing order>            (5 frames)
--f0min=<min F0 in Hz>            (50 Hz)
--f0max=<max F0 in Hz>            (1000 Hz)
--f0ana=<max analysis freq>       (3000 Hz)
--f0score                         (false)
--f0use=<weiEnv,weiMatch>         (0.140000,0.260000)
      if f0use is given the model scoring f0 algorithm will
      be used with the default score factors for spectral match,
      spectral envelope smoothness and spectral centroid.
      If weighting parameters are given then the first wil adjust
      the spectral envelope weighting the second the spectral
      match weighting, and the spectral centroid weighting will
      be set to 1-weiEnv-weiMatch.
--mixto=<insndfile>               (nothing)
--mixfac=<factor>                 (mixing ->-1, synth -> 1)
--chords=<ch1start,ch1end/...>  or 
--chords=<filename containing start and end time one pair per line>   (empty)
-v                                verbose
-V                                print version
-h                                this help

PM Actions
----------
smo:  F0 Smooth
f0:   F0 Analysis
par:  Partial Follow
seqs: Chord Seq (average spectrum)
seqp: Chord Seq (average partials)
pic:  Extract Peaks
syn:  Synthesis
fad:  Fade Harmonics

|#
;;;================================================================================================================