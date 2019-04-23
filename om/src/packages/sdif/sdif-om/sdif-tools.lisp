;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

;============================================================================
; From OM6 SDIF Analysis / Processing Tools
; jb - 2005 
;============================================================================

(in-package :om)

;;;==========================================================================
;;; FFT TOOLS
;;;==========================================================================


;;; "re-sample" frequencies of an fft of type 1GB4 
;;; this function is not hyper-useful but can be used as a model for other SDIF file processing operation
(defmethod resample-sdif-fft-freqs ((self SDIFFile) &key out-file (stream-num 0) (f-min 0) (f-max 22050) (f-step 100))
  
  (when (or (find "1GB4" (file-map self) :key 'fstream-desc-fsig :test 'string-equal) ;;; the file contains FFT data 
            (om-beep-msg "Sorry this SDIF file does not contain a valid SVP fft (1GB4)"))
    
    (let* ((out-path (or (and out-file (handle-new-file-exists out-file))
                         (om-choose-new-file-dialog)))
           (outptr (and out-path (sdif::sdif-open-file out-path sdif::eWriteFile))))
      
      (when outptr 
        
        (unwind-protect 
          
            (flet ((find-amplitude-in-f-a-list (freq f-a-list)
                     (let ((min-dist 44000) (val 0) dist)
                       (loop for f-a in f-a-list do
                             (setf dist (abs (- freq (first f-a))))
                             (if (<= dist min-dist) (setf min-dist dist val (second f-a))))
                       val)))
        
              (sdif::SdifFWriteGeneralHeader outptr)
              (sdif-write-nvt outptr `(("Author" ,(string+ "OM " *version-string*))))
              (sdif-write-types-string outptr "{1MTD 1GB4 {frequency, amplitude} 1FTD XFFT {1GB4  fftata;}}")
              (sdif::SdifFWriteAllASCIIChunks outptr)
          
              (get-sdif-frames self stream-num "1GB4" nil nil
                                 :apply-fun #'(lambda (frame)
                                                (let ((mat (find "1GB4" (lmatrices frame) :key 'matrixtype :test 'string-equal)))
                                                  (om-print-format "Sampling SDIF frame at ~D" (list (frametime frame)))
                                                  (when mat 
                                                    (let* ((f-a-list (mat-trans (get-array-data mat)))
                                                           (sampled-f-a-list 
                                                            (loop for f from f-min to f-max by f-step collect
                                                                  (list f (find-amplitude-in-f-a-list f f-a-list)))))
                                                      (setf (data mat) (mat-trans sampled-f-a-list))
                                                      (om-init-instance mat) ;; updates the elts and fields slots
                                                      ))
                                                  (sdif-write frame outptr)
                                                  )))
              (namestring out-path)
              )
          
          ;;; cleanup/close
          (sdif::sdiffclose outptr)
          ))
      )
    )
  )


(defmethod* fftdata->sdif ((fftdata list) &key timelist out-file)

  (let* ((out-path (or (and out-file (handle-new-file-exists out-file))
                       (om-choose-new-file-dialog)))
         (outptr (and out-path (sdif::sdif-open-file out-path sdif::eWriteFile))))
     
    (when outptr 
        
      (unwind-protect 
          (let ()
            (sdif::SdifFWriteGeneralHeader outptr)
            (sdif-write-nvt outptr `(("Author" ,(string+ "OM " *version-string*))))
            (sdif-write-types-string outptr "{1MTD 1GB1 {amplitude} 1FTD XFFT {1GB1  fftata;}}")
            (sdif::SdifFWriteAllASCIIChunks outptr)
          
            (loop for framedata in fftdata
                  for i = 0 then (+ i 1) do
                  (let* ((mat (om-init-instance 
                               (make-instance 'SDIFMatrix :matrixtype "1GB1" 
                                              :data (list framedata))))
                         (frame (make-instance 'SDIFFrame :frametype "XFFT" 
                                               :frametime (if timelist (nth i timelist) i)
                                               :streamid 0
                                               :lmatrices (list mat))))
                    (sdif-write frame outptr)
                    ))
            (namestring out-path))
            
        ;;; cleanup
        (sdif::sdiffclose outptr))
      )))


;;;==========================================================================
;;; BPF/F0 TOOLS
;;;==========================================================================

(defmethod* sdif->bpf ((self sdiffile) &key (frametype "1FQ0") (matrixtype "1FQ0") (stream 0) (field 0) (tmin nil) (tmax nil))
    :icon 608
    :indoc '("SDIF file" "frame type (string)" "matrix type (string)" "stream ID (int)" "field number" "min time (s)" "max time (s)")
    :initvals '(nil "1FQ0" "1FQ0" 0 0 nil nil)
    :doc "Reads SDIF data and formats results as a BPF.

Default values are suited to read and convert 1FQ0 frame and matrix types, typically resulting from fundamental frequency analysis.
Other type of data can be extracted by setting the <stream>, <frame>, <matrix> and <field> arguments accordingly.

<tmin> and <tmax> allow to bound the extracted data in a time interval.
"
  (when (and stream frametype frametype field)
      (multiple-value-bind (y x) (getsdifdata self stream frametype matrixtype field nil nil tmin tmax)
        (make-instance 'bpf :x-points x :y-points (flat y)))))


(defmethod* bpf->sdif ((self bpf) ftype mtype &key (scope 'time) (typedefs nil) (out-file "mybpf.sdif"))
  :icon 608
  :initvals '(nil "1FQ0" "1FQ0" 'time nil nil "mybpf.sdif")
  :indoc '("a BPF" "frame type (string)" "matrix type (string)" "x = time or elements" "custom types declaration" "output file")
  :menuins '((3 (("Time" time) ("Elements" elts))))
  :doc "Saves the contents of <self> (a BPF) as an SDIF file in <outfile>.

<ftype> and <mtype> allow to determine the SDIF type to enclose the data in (default = 1FQ0, i.e. fundamental frequency).
If these types are not standard, they must be declared and given as a list of SDIFType objects in <typedefs>

If <outfile> is just a filename (not a pathname) the file is written in the default OM 'out-files' folder.

<scope> allows to choose whether the x-dimension of the BPF should be considered as time (default) or as the elements in a single matrix.
"
   (let* ((out-path (or (and out-file (handle-new-file-exists out-file))
                       (om-choose-new-file-dialog)))
          (outptr (and out-path (sdif::sdif-open-file out-path sdif::eWriteFile))))
     
    (when outptr 
        
      (unwind-protect 
          (let ()
            (sdif::SdifFWriteGeneralHeader outptr)
            (sdif-write-nvt outptr `(("Author" ,(string+ "OM " *version-string*))))
            
            (when typedefs (sdif-write-types file (list! typedefs)))
            
            (sdif::SdifFWriteAllASCIIChunks outptr)
    
            (if (equal scope 'time)
                
                ;;; write a sequence of frames
                (loop for time in (x-points self)
                      for val in (y-points self) do
                      (let* ((mat (om-init-instance 
                                   (make-instance 'SDIFMatrix :matrixtype mtype 
                                                  :data (list (list val)))))
                             (frame (make-instance 'SDIFFrame :frametype ftype 
                                                   :frametime time
                                                   :streamid 0
                                                   :lmatrices (list mat))))
                        (sdif-write frame outptr)
                        ))
              
              ;;; write a single big frame/matrix
              (let* ((mat (om-init-instance 
                           (make-instance 'SDIFMatrix :matrixtype mtype 
                                          :data (list (y-points self)))))
                     (frame (make-instance 'SDIFFrame :frametype ftype
                                           :frametime 0.0
                                           :streamid 0
                                           :lmatrices (list mat))))
                (sdif-write frame outptr)
                )
              )

            (namestring out-path))
            
        ;;; cleanup
        (sdif::sdiffclose outptr))
      
      )))
            

;;;==========================================================================
;;; MARKERS TOOLS
;;;==========================================================================

(defmethod* sdif->markers ((self sdiffile) &key (frame "1MRK") (matrix nil) (stream 0) (tmin nil) (tmax))
    :icon 608
        :indoc '("SDIF file" "frame type (string)" "matrix type (string)" "stream ID (int)" "min time (s)" "max time (s)")
    :initvals '(nil "1MRK" nil 0 nil nil)
    :doc "Reads SDIF data and formats results as a list of time lmarkers (in s).

Default values are suited to read 1MRK frames, typically resulting from markers or transient detection analysis.
Other more specific type of data can be extracted by setting the <stream>, <frame>, <matrix> arguments accordingly.

<tmin> and <tmax> allow to bound the extracted data in a time interval.
"
    (when (and stream frame)
      (GetSDIFTimes self stream frame matrix tmin tmax)))


(defmethod* markers->sdif ((self list) &key (ftype "1MRK") (typedefs nil) (out-file "markers.sdif"))
  :icon 608
  :initvals '(nil "1MRK" nil "mybpf.sdif")
  :indoc '("onset list (s)" "SDIF frame type" "custom types declaration" "output file")
  :doc "Saves <self> (a list of onsets) as an SDIF file in <outfile>.

<ftype> allows to determine the SDIF frame type to use (default = 1MRK, the standard SDIF type for time markers).
If this type is not standard, it must be declared and given as an SDIFType object in <typedefs>

If <outfile> is just a filename (not a pathname) the file is written in the default OM 'out-files' folder.

"
  
  (let* ((out-path (or (and out-file (handle-new-file-exists out-file))
                       (om-choose-new-file-dialog)))
          (outptr (and out-path (sdif::sdif-open-file out-path sdif::eWriteFile))))
     
    (when outptr 
        
      (unwind-protect 
          (let ()
            (sdif::SdifFWriteGeneralHeader outptr)
            (sdif-write-nvt outptr `(("Author" ,(string+ "OM " *version-string*))))
            
            (when typedefs (sdif-write-types file (list! typedefs)))
            
            (sdif::SdifFWriteAllASCIIChunks outptr)
    
            ;;; write a sequence of frames
            (loop for time in self do
                  (let* ((frame (make-instance 'SDIFFrame :frametype ftype 
                                               :frametime time
                                               :streamid 0)))
                    (sdif-write frame outptr)
                    ))
                
            (namestring out-path))
            
        ;;; cleanup
        (sdif::sdiffclose outptr))
      
      )))



