
(in-package :om)

(defvar *spat-sdiftypes* nil)

(setf *spat-sdiftypes* 
      (reduce #'(lambda (s1 s2) (string+ s1 " " s2))
              (list "{"
                    "1MTD XCAR {x, y, z}" 
                    "1MTD XPOS {x, y, z}" 
                    "1MTD XAED {azimuth, elevation, distance}"
                    "1MTD XORI {yaw, pitch, roll}" 
                    "1MTD XAPE {aperture}" 
                    "1MTD PRES {presence}"
                    "1MTD WARM {warmth}"
                    "1MTD BRIL {brillance}"
                    "1MTD PRER {room_presence}"
                    "1MTD REVP {running_reverberance}"
                    "1MTD ENVP {envelopment}"
                    "1MTD OMNI {global_gain, gain_low, gain_mid, gain_high, freq_low, freq_high}"
                    "1MTD AXIS {global_gain, gain_low, gain_mid, gain_high, freq_low, freq_high}"
                    "1MTD XRID {room_index}"
                    
                    "1MTD XDEC {rt_global, rt_low, rt_mid, rt_high, freq_low, freq_High}"	
                    "1MTD XRIR {early_start, early_end, cluster_start, cluster_end, reverb_start, modal_density, early_distr, cluster_distr}"
                    
                    "1FTD XSRC {XCAR cartesian_coordinates; XAED navigational_coordinates; XORI orientation; XAPE aperture; PRES presence; WARM warmth; BRIL brillance; PRER room_presence; REVP running_reverberance; ENVP envelopment; OMNI omni_filter; AXIS axis_filter; XRID room_index;}"

                    "1FTD XRFX {XDEC decay_times; XRIR time_response;}"
                    
                    "}")))

(defmethod get-3D-pairs ((self BPC))
  (mapcar #'(lambda (pt) (list (car pt) (cadr pt) 0.0)) (point-pairs self)))

(defmethod get-3D-pairs ((self 3DC))
  (point-pairs self))

(defun gen-trajectories-frames (trajectories)
  (let (xmin xmax ymin ymax zmin zmax data) 
    (setf data
          (sort (loop for tr in trajectories
                      for i = 1 then (+ i 1) 
                      when tr append
                      (multiple-value-bind (list xn xx yn yx zn zx)
                          (loop for 3Dp in (get-3D-pairs tr)
                                for tim in (get-all-times tr)
                                minimize (car 3Dp) into xn
                                maximize (car 3Dp) into xx
                                minimize (cadr 3Dp) into yn
                                maximize (cadr 3Dp) into yx
                                minimize (caddr 3Dp) into zn
                                maximize (caddr 3Dp) into zx
                                collect (make-instance 'SDIFFrame :date tim :streamid i 
                                                         :frametype "XSRC"
                                                         :lmatrices (make-instance 'SDIFMatrix :matrixtype "XCAR"
                                                                                   :num-elts 1 :num-fields 3
                                                                                   :data 3Dp))
                                into list
                                finally (return (values list xn xx yn yx zn zx )))
                        (setf xmin (if xmin (min xmin xn) xn))
                        (setf xmax (if xmax (max xmax xx) xx))
                        (setf ymin (if ymin (min ymin yn) yn))
                        (setf ymax (if ymax (max ymax yx) yx))
                        (setf zmin (if zmin (min zmin zn) zn))
                        (setf zmax (if zmax (max zmax zx) zx))
                        list))
                '< :key 'date))
    (values data xmin xmax ymin ymax zmin zmax)))

  
(defmethod* sdif-export ((self spat-scene) &key out-filename (stream-mode 'sep) export-sounds)
            :icon '(638)
            :menuins '((2 (("separate streams" sep) ("merge streams" merge))))
            :indoc '("a SPAT-MATRIX" "output SDIF file name" "export stream format" "sources export mode" "room(s) descriptions")
            :outdoc '("sdif file pathname")
            :initvals '(nil nil sep nil)
            :doc "Saves a SPAT-MATRIX into an SDIF File.

- If <out> is not specified, a file chooser opens to choose the output file.
- <stream mode> determines if source trajectories are stored in separate SDIF streams ('sep) or in a single stream ('merge)
- If <export-sounds> is T, the source files (if provided in the SPAT-MATRIX) are copied and exported to the SDIF file location.
  If <export-sound> is :temp, they are exported and registered as 'temporary files' to delete after synthesis.
"
            (let* ((error nil) time
                   (filepath (or (and out-filename (handle-new-file-exists out-filename))
                                 (om-choose-new-file-dialog :name "spat-scene" :types (list (format nil (om-str :file-format) "SDIF") "*.sdif" ))))
                   (spat-descriptors '("trajecories"))
                   (nstreams (length spat-descriptors)))
              (when filepath
                (let* ((sources (loop for src in (sources self) collect (get-sound src)))
                       (srcnames (loop for src in (sources self) collect (get-sound-name src)))
                       (srcfiles (loop for src in (sources self) collect (get-sound-file src)))
                       (sndtable (loop for file in srcfiles 
                                       for i = 1 then (+ i 1) do 
                                       (when (and file export-sounds)
                                         (let ((tempfile (unique-pathname filepath (pathname-name soundpath) (pathname-type soundpath))))
                                           (om-copy-file soundpath tempfile)
                                           (when (equal export-sounds :temp) (add-tmp-file tempfile))
                                           ))
                                       collect  
                                       (cons (number-to-string i)
                                             (list (if file (string+ (pathname-name file) "." (pathname-type file)) "unknown-source")))))
                       (nametable (loop for name in srcnames 
                                        for i = 1 then (+ i 1)
                                        collect (list (number-to-string i) (or name (format nil "src~A" name)))))
                       )
       
                  (multiple-value-bind (spatframes xmin xmax ymin ymax zmin zmax) 
                      (gen-trajectories-frames (trajectories self))
           
                    (when (equal stream-mode 'merge)
                      (setf spatframes (merge-frame-data spatframes)))
           
                    (let* ((nvts (list (default-om-NVT)
                                       (make-instance 'SDIFNVT :tablename "Sources" :ID 0 :NV-pairs sndtable)
                                       (make-instance 'SDIFNVT :tablename "Dimensions" :ID 0
                                                      :NV-pairs (list (list "Xmin" (format nil "~D" xmin))
                                                                      (list "Xmax" (format nil "~D" xmax))
                                                                      (list "Ymin" (format nil "~D" ymin))
                                                                      (list "Ymax" (format nil "~D" ymax))
                                                                      (list "Zmin" (format nil "~D" zmin))
                                                                      (list "Zmax" (format nil "~D" zmax))
                                                                      ))
                                       (make-instance 'SDIFNVT :tablename "SourceNames" :ID 0 :NV-pairs nametable)))
                           (sdiff (sdif::SDIFFOpen (namestring filepath) sdif::eWriteFile))
                           (datatype 4))
                      (sdif::SdifFWriteGeneralHeader sdiff)
                      (sdif-write nvts sdiff)   
                      (sdif-write-types-string sdiff *spat-sdiftypes*)
                      (sdif::SdifFWriteAllASCIIChunks sdiff)
                      (sdif-write (merge-frames spatframes) sdiff)
                      (sdif::SDIFFClose sdiff))
                    (probe-file (namestring filepath))
                    )))))


