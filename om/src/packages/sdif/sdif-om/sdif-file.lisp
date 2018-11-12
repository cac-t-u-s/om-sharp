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

(in-package :om)


;;;================================================================================
;;; SDIFFILE is a pointer to a file on the disk
;;; It loads a filemap for quick inspect 
;;; It can be edited to the extend of this map (streams, frames etc.)
;;; Frames can be extracted to a data-stream for further manipulations
;;;================================================================================
(defclass* SDIFFile (sdif-object)   
   ((file-pathname  :initform nil :accessor file-pathname)
    (file-map :initform nil :accessor file-map))
   (:documentation "
SDIFFILE represents an SDIF file stored somewhere in your hard drive.

SDIF is a generic format for the storage and transfer of sound description data between applications.
It is used in particular by softwares like SuperVP, pm2, AudioSculpt, Spear, etc. to store and export sound analyses.

See http://www.ircam.fr/sdif for more inforamtion about SDIF

Connect a pathname to an SDIF file, or the output of a function returning such a pathname, to initialize the SDIFFILE.
If not connected, the evaluation of the SDIFFILE box will open a file chooser dialog.

Lock the box ('b') to keep the current file.
"))


(defmethod additional-slots-to-save ((self SDIFFile)) '(file-pathname))
(defmethod additional-slots-to-copy ((self SDIFFile)) '(file-pathname))
 

;;; INIT METHODS
(defmethod box-def-self-in ((self (eql 'SDIFFile))) :choose-file)

(defmethod objFromObjs ((model (eql :choose-file)) (target SDIFFile))
  (let ((file (om-choose-file-dialog :prompt "Choose an SDIF file..."
                                     :types '("SDIF files" "*.sdif"))))
    (if file (objFromObjs file target)
      (om-abort))))

(defmethod objfromobjs ((model pathname) (target SDIFFile))
  (when (and (probe-file model)
             (sdif::sdif-check-file model))
    (setf (file-pathname target) model) 
    target))

(defmethod objfromobjs ((model string) (target SDIFFile))
  (objfromobjs (pathname model) target))


(defmethod om-init-instance ((self SDIFFile) &optional initargs)
  (call-next-method)
  (when (file-pathname self) 
    (load-sdif-file self))
  self)


;;; CONNECT SDIFFILE TO TEXTBUFFER
(defmethod objfromobjs ((self SDIFFile) (target TextBuffer))
   (objfromobjs (sdif->text self) target))

;;; DISPLAY BOX
(defmethod display-modes-for-object ((self sdiffile)) '(:hidden :text :mini-view))

(defmethod get-cache-display-for-text ((self sdiffile))
  `((:file-pathname ,(file-pathname self))
    (:file-contents 
     ,(loop for stream in (file-map self) collect 
            (format nil "~D:~A ~A" (fstream-desc-id stream) (fstream-desc-fsig stream)
                    (mapcar 'mstream-desc-msig (fstream-desc-matrices stream)))
            )
     )))

(defmethod default-name ((self SDIFFILE)) 
  (when (file-pathname self)
    (string+ (pathname-name (file-pathname self)) "." (pathname-type (file-pathname self)))))

(defmethod draw-mini-view ((self SDIFFIle) (box t) x y w h &optional time)
  (let* ((n-streams (length (file-map self)))
         (stream-h (round (- h 8) (max n-streams 1)))
         (max-t (list-max (mapcar 'fstream-desc-tmax (file-map self))))
         (font (om-def-font :font1 :size 10)))
    (om-with-font 
     (om-def-font :font1 :face "arial" :size 36 :style '(:bold))
     (om-with-fg-color (om-make-color 0.6 0.6 0.6 0.5)
       (om-draw-string (- (round w 2) 50) (+ y (max 30 (+ 14 (/ h 2)))) "SDIF")))
    (om-with-font 
     font 
     (loop for stream in (file-map self) 
           for ypos = 8 then (+ ypos stream-h) 
           do 
           (when (> max-t 0)
             (om-draw-rect (* w (/ (fstream-desc-tmin stream) max-t)) 
                           (+ ypos 2) 
                           (* w (/ (fstream-desc-tmax stream) max-t)) 
                           (- stream-h 4) 
                           :color (om-make-color-alpha (om-def-color :dark-blue) 0.6) :fill t))
           (om-with-fg-color (om-def-color :white) 
             (om-draw-string 2 (+ ypos 12) 
                             (format nil "~D:~A ~A" (fstream-desc-id stream) (fstream-desc-fsig stream)
                                     (mapcar 'mstream-desc-msig (fstream-desc-matrices stream))))))
     )))


;;;========================
;;; FILL FILE-MAP from file
;;;========================

(defstruct fstream-desc id fsig tmin tmax nf matrices)
(defstruct mstream-desc msig fields rmax tmin tmax nf)

(defun check-current-singnature (ptr)
  (sdif::sdif-check-signature (sdif::SdifSignatureToString (sdif::SdifFCurrSignature ptr))))

(defmethod get-frame-from-sdif (ptr &optional (with-data t))
  (sdif::SdifFReadFrameHeader ptr)
  (let ((sig (sdif::SdifSignatureToString (sdif::SdifFCurrFrameSignature ptr)))
        (time (sdif::SdifFCurrTime ptr))
        (sid (sdif::SdifFCurrId ptr)))
    (make-instance 'SDIFFrame :frametime time :frametype sig :streamid sid
                   :lmatrices (let ((nummatrix (sdif::SdifFCurrNbMatrix ptr)))
                                (loop for i from 1 to nummatrix 
                                      collect (get-matrix-from-sdif ptr with-data))))))


(defmethod get-matrix-from-sdif (ptr &optional (with-data t))
  
  (sdif::SdifFReadMatrixHeader ptr)
  
  (let* ((sig (sdif::SdifSignatureToString (sdif::SdifFCurrMatrixSignature ptr)))
         (ne (sdif::SdifFCurrNbRow ptr))
         (nf (sdif::SdifFCurrNbCol ptr))
         (matrix (make-instance 'SDIFMatrix :matrixtype sig)))
    
    (setf (field-names matrix)
          (let ((mtype (sdif::SdifTestMatrixType ptr (sdif::SdifStringToSignature sig))))
            (if (om-null-pointer-p mtype)
                (loop for i from 1 to nf collect (format nil "c~D" i))
              (loop for i from 1 to nf collect (sdif::SdifMatrixTypeGetColumnName mtype i)))))
    
    (if with-data

        (let ((bytesread 0))
          
          (setf (data matrix) (loop for f from 1 to nf collect (make-list ne)))
          
          (loop for r from 0 to (1- ne) do
                (progn (setf bytesread (+ bytesread (sdif::SdifFReadOneRow ptr)))
                  (loop for n from 1 to nf do
                        (setf (nth r (nth (1- n) (data matrix)))
                              (sdif::SdifFCurrOneRowCol ptr n)))))
          
          (om-init-instance matrix)
          (sdif::SdifFReadPadding ptr (sdif::sdif-calculate-padding bytesread))
          )

      (progn
        (setf (fields matrix) nf)
        (setf (elts matrix) ne)
        (sdif::SdifFSkipMatrixData ptr))
      )
    
    matrix))


(defmethod load-sdif-file ((self SDIFFile))
  (cond ((not (file-pathname self))
         (om-beep-msg "Error loading SDIF file: -- no file"))
        ((not (probe-file (file-pathname self)))
         (om-beep-msg "Error loading SDIF file -- file does not exist: ~D" (file-pathname self)))
        ((not (sdif::sdif-check-file (file-pathname self)))
         (om-beep-msg "Error loading SDIF file -- wrong format: ~D" (file-pathname self)))
        (t 
         (let ((sdiffileptr (sdif::sdif-open-file (file-pathname self) sdif::eReadWriteFile)))
           (om-print-format "Loading SDIF file : ~A" (list (file-pathname self)) "SDIF")
           (if sdiffileptr
               (unwind-protect 
                   (progn (sdif::SdifFReadGeneralHeader sdiffileptr)
                     (sdif::SdifFReadAllASCIIChunks sdiffileptr)
                     ;;; set the file-map
                     (loop while (check-current-singnature sdiffileptr) do
                           (let ((f (get-frame-from-sdif sdiffileptr NIL)))
                             (record-in-streams self f)
                             (sdif::sdif-read-next-signature sdiffileptr))))
                 (sdif::SDIFFClose sdiffileptr))
             (om-beep-msg "Error loading SDIF file -- bad pointer: ~D" (file-pathname self)))
           )))
  (setf (file-map self) (sort (file-map self) '< :key 'fstream-desc-id))  
  self)

(defun record-in-streams (self frame)
  (let ((streamdesc (find frame (file-map self) 
                          :test #'(lambda (frame stream) 
                                    (and (string-equal (frametype frame) (fstream-desc-fsig stream))
                                         (= (streamid frame) (fstream-desc-id stream)))))))
    (if streamdesc
        (let ((frame-time (frametime frame))) ;;; EXISTING FRAME STREAM
          (setf (fstream-desc-nf streamdesc) (1+ (fstream-desc-nf streamdesc)))
          (when (< frame-time (fstream-desc-tmin streamdesc))
            (setf (fstream-desc-tmin streamdesc) frame-time))
          (when (> frame-time (fstream-desc-tmax streamdesc))
            (setf (fstream-desc-tmax streamdesc) frame-time))
          (loop for mat in (lmatrices frame) do
                (let ((mstreamdesc (find mat (fstream-desc-matrices streamdesc)
                                         :test #'(lambda (mat mstreamdesc) 
                                                   (string-equal (matrixtype mat) (mstream-desc-msig mstreamdesc))))))
                  (if mstreamdesc 
                      (let ()  ;;; EXISTING MATRIX STREAM
                        (setf (mstream-desc-nf mstreamdesc) (1+ (mstream-desc-nf mstreamdesc)))
                        (when (< frame-time (mstream-desc-tmin mstreamdesc))
                          (setf (mstream-desc-tmin mstreamdesc) frame-time))
                        (when (> frame-time (mstream-desc-tmax mstreamdesc))
                          (setf (mstream-desc-tmax mstreamdesc) frame-time))
                        (when (> (elts mat) (mstream-desc-rmax mstreamdesc))
                          (setf (mstream-desc-rmax mstreamdesc) (elts mat)))
                        ;(when (> (fields mat) (mstream-desc-fields mstreamdesc))
                        ;  (setf (mstream-desc-fields mstreamdesc) (fields mat)))
                        )
                    ;;; NEW MATRIX STREAM
                    (pushr (make-mstream-desc :msig (matrixtype mat) 
                                              :fields (first-n (SDIFTypeDescription self (matrixtype mat) 'm) (fields mat))
                                              :rmax (elts mat) :tmin frame-time :tmax frame-time
                                              :nf 1) 
                           (fstream-desc-matrices streamdesc)))
                  ))
          )
      ;;; NEW FRAME STREAM
      (pushr (make-fstream-desc 
              :fsig (frametype frame) :id (streamid frame)
              :tmin (frametime frame) :tmax (frametime frame) :nf 1
              :matrices  (loop for mat in (lmatrices frame) collect 
                               (make-mstream-desc 
                                :msig (matrixtype mat) 
                                :fields (first-n (SDIFTypeDescription self (matrixtype mat) 'm) (fields mat))
                                :rmax (elts mat) :tmin (frametime frame) :tmax (frametime frame)
                                :nf 1)))
             (file-map self))
      )))


(defmethod* SDIFInfo ((self sdifFile) &optional (print t))
   :doc "Prints/returns information about the SDIF data in <self>.
Returns an advanced stream description with every FrameType-MatrixType pair in the file.
"
   :indoc '("SDIF file")
   (let ((rep-list nil))
     (when print 
       (om-print-format "----------------------------------------------------------~%")
       (om-print-format  "SDIF file description for ~D~%" (list (namestring (file-pathname self))))
       (om-print-format "----------------------------------------------------------~%")
       (om-print-format  "NUMBER OF SDIF STREAMS: ~D~%"  (list (length (file-map self)))))
     (loop for st in (file-map self) do
           (when print 
             (om-print-format "   STREAM ID ~D - ~D Frames type = ~A ~%"  (list (fstream-desc-id st) (fstream-desc-nf st) (fstream-desc-fsig st)))
             (om-print-format "      Tmin= ~D   -   Tmax= ~D~%" (list (fstream-desc-tmin st) (fstream-desc-tmax st)))
             (om-print-format "      Matrices :  "))
           (loop for ma in (fstream-desc-matrices st) do 
                 (pushr (list (fstream-desc-id st) (fstream-desc-fsig st) (mstream-desc-msig ma)) rep-list)
                 (when print (om-print-format " ~D" (list (mstream-desc-msig ma))))
                 )
           (when print (om-print-format "~%"))
           )
   (when print 
     (om-print-format "----------------------------------------------------------~%")
     (om-print-format "End file description~%")
     (om-print-format "----------------------------------------------------------~%"))
   rep-list
   ))


;;;===================================
;;; INFO / META DATA FROM SDIF HEADERS
;;;===================================

(defun matrixinfo-from-signature (file msig)
  (let* ((sig (sdif::SdifStringToSignature msig))
         (mtype (sdif::SdifTestMatrixType file sig)))
    (if (om-null-pointer-p mtype)
        (progn 
          (print (string+ "Matrix Type " msig " not found."))
          NIL)
      (let ((mnumcol (sdif::SdifMatrixTypeGetNbColumns mtype)))
        (loop for i = 1 then (+ i 1) while (<= i mnumcol)
              collect (sdif::SdifMatrixTypeGetColumnName mtype i))
        )
      )))

(defun frameinfo-from-signature (file fsig)
   (let* ((sig (sdif::SdifStringToSignature fsig))
          (ftype (sdif::SdifTestFrameType file sig)))
    (if (om-null-pointer-p ftype)
      (progn 
        (print (string+ "Frame Type " fsig " not found."))
        NIL)
      (let ((fnumcomp (sdif::SdifFrameTypeGetNbComponents ftype)))
         (loop for i = 1 then (+ i 1) while (<= i fnumcomp) collect 
               (let ((fcomp (sdif::SdifFrameTypeGetNthComponent ftype i)))
                 (if (om-null-pointer-p fcomp) NIL
                   (let ((msig (sdif::SdifFrameTypeGetComponentSignature fcomp)))
                     (list (sdif::SdifSignaturetoString msig)
                           (matrixinfo-from-signature file (sdif::SdifSignatureToString msig))
                           ))
                   ))))
      )))
      
(defmethod* SDIFTypeDescription ((self SDIFFile) (signature string) &optional (type 'm))
            
            :indoc '("SDIF file" "SDIF type Signature" "Frame / Matrix")
            :initvals '(nil "1TYP" m)
            :menuins '((2 (("Matrix" m) ("Frame" f))))
            :doc "Returns a description of type <signature>.

This function must be connected to an SDIF file (<self>) containing this data type.
<type> (m/f) allows to specify if the type correspond to matrices (default) or to frames.

Matrix type description is a list of the different field (columns) names.
Frame type description is a list of lists containing the internal matrix signatures and their respective type descriptions.
"

            (let ((sdifptr (sdif::sdif-open-file (file-pathname self) sdif::eReadWriteFile)))
              (if sdifptr 
                  (unwind-protect 
                      (progn
                        (sdif::SdifFReadGeneralHeader sdifptr)
                        (sdif::SdifFReadAllASCIIChunks sdifptr)
                        (case type 
                          ('m (matrixinfo-from-signature sdifptr signature))
                          ('f (frameinfo-from-signature sdifptr signature))))
                    (sdif::SDIFFClose sdifptr)))))


;;;=====================
;;; NVT Utils
;;;=====================

(defmethod get-nvt-list ((self string))
  (let ((fileptr (sdif::sdif-open-file self sdif::eReadWriteFile)))
    (if fileptr 
        (unwind-protect
            (progn
              (sdif::SdifFReadGeneralHeader fileptr)
              (sdif::SdifFReadAllASCIIChunks fileptr)
              (let* ((nvtlist (sdif::SdifFNameValueList fileptr))
                     (nvtl (sdif::SdifNameValueTableList nvtlist))
                     (numnvt (sdif::SdifListGetNbData nvtl))
                     (nvtiter (sdif::SdifCreateHashTableIterator nil)))
                (unwind-protect
                    (progn (sdif::SdifListInitLoop nvtl)
                      (loop for i = 0 then (+ i 1)
                            while (< i 2000)  ;; just in case :)
                            while (let ((next (sdif::SdifListIsNext nvtl)))
                                    (and next (> next 0)))
                            collect (let* ((curnvt (sdif::SdifListGetNext nvtl))
                                           (nvht (sdif::SdifNameValueTableGetHashTable curnvt))
                                           (nvnum (sdif::SdifNameValueTableGetNumTable curnvt))
                                           (nvstream (sdif::SdifNameValueTableGetStreamID curnvt))
                                           (nvtdata (make-instance 'SDIFNVT :id nvstream)))
                                      (setf (tnum nvtdata) nvnum)
                                      (sdif::SdifHashTableIteratorInitLoop nvtiter nvht)
                                      (setf (nv-pairs nvtdata)
                                            (loop for i = 0 then (+ i 1)
                                                  while (< i 2000)
                                                  while (let ((nextit (sdif::SdifHashTableIteratorIsNext nvtiter)))
                                                          (and nextit (> nextit 0)))
                                                  collect 
                                                  (let* ((nv (sdif::SdifHashTableIteratorGetNext nvtiter))
                                                         (name (sdif::SdifNameValueGetName nv))
                                                         (value (sdif::SdifNameValueGetValue nv)))
                                                    (when (string-equal name "TableName")
                                                      (setf (tablename nvtdata) value))
                                                    (list name value))))
                                      nvtdata)
                            ))
                  (sdif::SdifKillHashTableIterator nvtiter))
                ))
          (sdif::SDIFFClose fileptr)))))

(defmethod get-nvt-list ((self SDIFFile))
   (get-nvt-list (file-pathname self)))

(defmethod get-nvt-list ((self pathname))
   (get-nvt-list (namestring self)))


(defmethod* GetNVTList ((self t))
            
            :indoc '("SDIF file")
            :initvals '(nil)
            :doc "Returns the list of Name/Value tables in <self> (SDIFFile object or path to an SDIF file).

Name/Value tables are formatted as SDIFNVT objects.
"
            (get-nvt-list self))

(defmethod* GetNVTList ((self sdiffile)) (call-next-method))


(defmethod* find-in-nvtlist ((nvtlist list) (entry string) &optional table-num)
            
            :indoc '("list of SDIFNVT" "A name entry in the NameValue table" "Table Number")
            :initvals '(nil "" nil)
            :doc "Finds value corresponding to name <entry> in the name/value tables <nvtlist>.

<table-num> allows to look specifically in table number <table-num> as internally assigned to SDIF NVTs.
"
    (if (and table-num (numberp table-num))
        (let ((nvt (find table-num nvtlist :key 'tnum)))
          (if nvt 
              (find-in-nvt nvt entry)
            (om-beep-msg (string+ "There is no table number " (integer-to-string table-num)))))
      (let ((rep nil))
        (loop for nvt in nvtlist while (not rep) do
              (setf rep (find-in-nvt nvt entry))) fstream-desc
        rep)))
         
(defmethod* find-in-nvt ((nvt SDIFNVT) (entry string))
            
            :indoc '("a SDIFNVT object" "A name entry in the Name/Value table")
            :initvals '(nil "")
            :doc "Finds value corresponding to name <entry> in the name/value table <nvt>."
    (cadr (find entry (nv-pairs nvt) :test 'string-equal :key 'car)))


;;;=============================
;;; READ DATA
;;;=============================

(defmethod get-sdif-data ((self pathname) streamNum frameT matT colNum rmin rmax tmin tmax &key (with-data t))
  (get-sdif-data (namestring self) streamNum frameT matT colNum rmin rmax tmin tmax :with-data with-data))

(defmethod get-sdif-data ((self SDIFFIle) streamNum frameT matT colNum rmin rmax tmin tmax &key (with-data t))
  (get-sdif-data (file-pathname self) streamNum frameT matT colNum rmin rmax tmin tmax :with-data with-data))

(defmethod get-sdif-data ((self string) streamNum frameT matT colNum rmin rmax tmin tmax &key (with-data t))
  (cond ((or (and rmin rmax (> rmin rmax)) (and tmin tmax (> tmin tmax))) 
         (om-beep-msg "GET-SDIF-DATA: Wrong parameters (tmin > tmax) or (rmin > rmax)..." ))
        ((not (probe-file self))
         (om-beep-msg (format nil "FILE: ~A NOT FOUND..." self)))
        (t 
         (let ((sdiffileptr (sdif::sdif-open-file self sdif::eReadWriteFile))
               (error nil) (sdifdata nil) (sdiftimes nil))
           (om-print "extracting data..." "SDIF")
           (if sdiffileptr
               (unwind-protect 
                   (let ((curr-time nil))
                     (sdif::SdifFReadGeneralHeader sdiffileptr)
                     (sdif::SdifFReadAllASCIIChunks sdiffileptr)
                     ;;; HERE
                     (loop while (and 
                                  (check-current-singnature sdiffileptr) ;;; more frames in the file
                                  (not error) ;;; so far so good
                                  (or (not curr-time) (not tmax) (not (>= curr-time tmax)))) ;;; did not ran over the tmax already 
                           do
                           (sdif::SdifFReadFrameHeader sdiffileptr)
                           (let ((fsig (sdif::SdifSignatureToString (sdif::SdifFCurrFrameSignature sdiffileptr)))
                                 (sid (sdif::SdifFCurrId sdiffileptr)))
                             (setq curr-time (sdif::SdifFCurrTime sdiffileptr))
                       
                             (if (and (or (not streamNum) (= streamNum sid))
                                      (string-equal frameT fsig) 
                                      (or (not tmin) (>= curr-time tmin)))
                                 
                                 ;;; we're in a candidate frame
                                 (dotimes (m (sdif::SdifFCurrNbMatrix sdiffileptr))
                                   
                                   (sdif::SdifFReadMatrixHeader sdiffileptr)
                                   (let ((msig (sdif::SdifSignatureToString (sdif::SdifFCurrMatrixSignature sdiffileptr)))
                                         (ne (sdif::SdifFCurrNbRow sdiffileptr))
                                         (nf (sdif::SdifFCurrNbCol sdiffileptr))
                                         (size (sdif::SdifSizeofDataType (sdif::SdifFCurrDataType sdiffileptr))))
                                     
                                     (if (or (null matT)
                                             (string-equal msig matT))
                                         
                                         ;;; we're in a candidate matrix
                                         (if with-data
                                             (if (and (numberp colNum) (<= nf colNum))
                                                 (progn 
                                                   (om-beep-msg (format nil "Error the matrix ~A has only ~D fields" msig nf))
                                                   (setf error t))
                                               (let ((r1 0) (r2 (1- ne)) 
                                                     (bytesread 0))
                                                 (when (and rmin (> ne rmin)) (setf r1 rmin))
                                                 (when (and rmax (> ne rmax)) (setf r1 rmax))
                                      ;(print (list msig ne nf size))
                                                 ;;; go to r1
                                                 (loop for k from 0 to (1- r1) 
                                                       do (setf bytesread (+ bytesread (sdif::SdifFSkipOneRow sdiffileptr))))
                                                 ;;; read
                                                 (let ((data
                                                        (loop for k from r1 to r2 
                                                              do (setf bytesread (+ bytesread (sdif::SdifFReadOneRow sdiffileptr)))
                                                              collect 
                                                              (cond
                                                               ((numberp colNum) 
                                                                (sdif::SdifFCurrOneRowCol sdiffileptr (1+ colNum)))
                                                               ((consp colNum) 
                                                                (loop for n in colNum collect (sdif::SdifFCurrOneRowCol sdiffileptr (1+ n))))
                                                               ((null colNum) (loop for n from 1 to nf collect 
                                                                                    (coerce (sdif::SdifFCurrOneRowCol sdiffileptr n) 'single-float)))))
                                                        ))
                                                   (loop for k from (1+ r2) to (1- ne) 
                                                         do (setf bytesread (+ bytesread (sdif::SdifFSkipOneRow sdiffileptr))))
                                        ;(print (list "read" bytesread "pad" (sdif::sdif-calculate-padding bytesread)))
                                                   (sdif::SdifFReadPadding sdiffileptr (sdif::sdif-calculate-padding bytesread))
                                                   (when data 
                                                     (push data sdifdata)
                                                     (push curr-time sdiftimes)))
                                                 ))
                                           ;;; no data (just times)
                                           (progn (push curr-time sdiftimes)
                                             (sdif::SdifFSkipMatrixData sdiffileptr))
                                           )
                                       (sdif::SdifFSkipMatrixData sdiffileptr)
                                       ))
                                   )
                               ;;; skip the frame
                               (sdif::sdiffskipframedata sdiffileptr)
                          
                               )
                      
                             (sdif::sdif-read-next-signature sdiffileptr)
                        
                             ))
                     (if (or sdifdata sdiftimes) 
                         (values (reverse sdifdata) (reverse sdiftimes))
                       (progn (om-print-format "No data found for Frame ~A / Matrix ~A in stream #~D (t1=~D t2=~D r1=~D r2=~D)" 
                                               (list frameT matT streamNum tmin tmax rmin rmax) "SDIF")
                         nil))
                     )
                 (sdif::SDIFFClose sdiffileptr))
             (om-beep-msg "Error loading SDIF file -- bad pointer: ~D" self))
           ))))


(defmethod* GetSDIFData ((self t) sID (frameType string) (matType string) Cnum rmin rmax tmin tmax)
   
   :indoc '("SDIF file" "stream number (int)" "frame type (string)" "matrix type (string)" "field number (int or list)" "min row" "max row" "min time (s)" "max time (s)")
   :outdoc '("matrix values" "times")
   :initvals '(nil 0 "" "" 0 nil nil nil nil)
   :doc "Extracts and returns a data array (<rmin>-<rmax>, <tmin>-<tmax>) from the <cnum> field of the <matType> matrix from the Stream <sid> of <frameType> frames from <self>.

<self> can be an SDIFFile object or a pathname to a valid SDIF file.

<cnum> can be a single value or a list of values (for multiple dimentional descriptions)
Unspecified arguments mean all SDIF data (i.e. for instance any time, all rows, fields, etc.) will be considered and returned.

<frameType> and <matType> MUST be specified (4-character SDIF signatures for frame and matrix types)

Use SDIFINFO for information about the Frame and Matrix types contained in a <self>.

Ex. (GETSDIFDATA <SDIFFile> 0 \"1MRK\" \"1TRC\" (0 1) nil nil 0.0 2.0)
means : Get all data from Stream number 1, frames of type 1MRK, matrices of type 1TRC, columns (i.e. fields) 0 and 1, all matrox rows, between 0.0s and 2.0s.

The second oulet returns all corresponding frame TIMES.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.

"
   :numouts 2
   (get-sdif-data self sID frameType matType Cnum rmin rmax tmin tmax :with-data t))



;;;=============================
;;; READ DATA
;;;=============================

(defmethod get-sdif-times ((self pathname) streamNum frameT matT tmin tmax)
  (get-sdif-times (namestring self) streamNum frameT matT tmin tmax))

(defmethod get-sdif-times ((self SDIFFIle) streamNum frameT matT tmin tmax)
  (get-sdif-times (file-pathname self) streamNum frameT matT tmin tmax))

(defmethod get-sdif-times ((self string) streamNum frameT matT tmin tmax)

  (cond ((and tmin tmax (> tmin tmax))
         (om-beep-msg "GET-SDIF-TIMES: Wrong parameters (tmin > tmax) ..." ))
        ((not (probe-file self))
         (om-beep-msg "FILE: ~A NOT FOUND..." self))
        (t 
         (let ((sdiffileptr (sdif::sdif-open-file self sdif::eReadWriteFile))
               (error nil) (sdiftimes nil))
           (om-print "rxtracting frame times..." "SDIF")
           
           (if sdiffileptr
               (unwind-protect 
                   
                   (let ((curr-time nil))
                     (sdif::SdifFReadGeneralHeader sdiffileptr)
                     (sdif::SdifFReadAllASCIIChunks sdiffileptr)
                     ;;; HERE
                     (loop while (and 
                                  (check-current-singnature sdiffileptr) ;;; more frames in the file
                                  (not error) ;;; so far so good
                                  (or (not curr-time) (not tmax) (not (>= curr-time tmax)))) ;;; did not ran over the tmax already 
                           do
                           (sdif::SdifFReadFrameHeader sdiffileptr)
                           (let ((fsig (sdif::SdifSignatureToString (sdif::SdifFCurrFrameSignature sdiffileptr)))
                                 (sid (sdif::SdifFCurrId sdiffileptr)))
                             
                             (setq curr-time (sdif::SdifFCurrTime sdiffileptr))
                       
                             (if (and (or (not streamNum) (= streamNum sid))
                                      (or (not frameT) (string-equal frameT fsig))
                                      (or (not tmin) (>= time tmin)))
                                 ;;; we're in a candidate frame..
                                
                                 (if (null matT) 

                                     ;;; OK: keep this one
                                     (progn 
                                       (push curr-time sdiftimes)
                                       (sdif::sdiffskipframedata sdiffileptr))

                                   ;;; search matrices
                                   (dotimes (m (sdif::SdifFCurrNbMatrix sdiffileptr))
                                     (sdif::SdifFReadMatrixHeader sdiffileptr)
                                     (let ((msig (sdif::SdifSignatureToString (sdif::SdifFCurrMatrixSignature sdiffileptr))))
                                       (when (string-equal msig matT)
                                         ;;; OK: keep this one
                                         (push curr-time sdiftimes))
                                       )
                                     (sdif::SdifFSkipMatrixData sdiffileptr)
                                     )
                                   )

                               ;;; not a candidate frame: just skip the frame
                               (sdif::sdiffskipframedata sdiffileptr)
                               
                               )
                             
                             )
                           
                           (sdif::sdif-read-next-signature sdiffileptr)
                        
                           )
                 
                     (if sdiftimes 
                         (reverse sdiftimes)
                       (progn (om-print-format "No data found for Frame ~A / Matrix ~A in stream #~D (t1=~D t2=~D)" 
                                               (list frameT matT streamNum tmin tmax) "SDIF")
                         nil))
                     )
                 (sdif::SDIFFClose sdiffileptr))
             (om-beep-msg "Error loading SDIF file -- bad pointer: ~D" self))
           )))
  )


(defmethod* GetSDIFTimes ((self sdifFile) sID frameType matType tmin tmax)
   
   :indoc '("SDIF file" "stream number (integer)" "frame type" "matrix type" "min time (s)" "max time (s)")
   :initvals '(nil 0 "" "" nil nil)
   :doc "Returns a list of times (s) between <tmin> and <tmax> for frames of type <frameType> from the stream <sID> in <self>, containing a matrix of type <matType>.

Unspecified arguments mean respectively that all streamms (for <sID>), frames (<frameType>), matrices (<matType>) and no time boundaries (<tmin> and <tmax>) are considered.

Use SDIFINFO for information about the Frame and Matrix types contained in a <self>.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.

"
   (get-sdif-times self sID frameType matType tmin tmax))


(defmethod* GetSDIFFrames ((self t) &key sID frameType tmin tmax)
   
   :indoc '("SDIF file" "frame type (string)" "matrix type (string)" "stream number"  "min time (s)" "max time (s)")
   :initvals '(nil nil nil 0 nil nil)
   :doc "Creates and returns an SDIFStream instance from SDIF data in stream <sid> of <self>.

<self> can be an SDIFFile object or a pathname to a valid SDIF file.

<sid> can be a list of IDs (intergers)
 
<tmin> and <tmax> determine a time window in the stream(s).
<frameType> and <matType> allow to select frames and/or matrices of a specific type.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.
"
   (get-sdif-frames self sID frameType tmin tmax))

(defmethod get-sdif-frames ((self sdifFile) streamNum frameT tmin tmax &key apply-fun)
  (if (file-pathname self)
      (get-sdif-frames (file-pathname self) streamNum frameT tmin tmax :apply-fun apply-fun)
    (om-beep-msg "SDIFFILE: no file loaded!" self)))

(defmethod get-sdif-frames ((self pathname) streamNum frameT tmin tmax &key apply-fun)
  (get-sdif-frames (namestring self) streamNum frameT tmin tmax :apply-fun apply-fun))

(defmethod get-sdif-frames ((self string) streamNum frameT tmin tmax &key apply-fun)
  (cond ((not (probe-file self))
         (om-beep-msg (format nil "FILE: ~A NOT FOUND..." self)))
        (t
         (let ((sdiffileptr (sdif::sdif-open-file self sdif::eReadWriteFile))
               (frame-list nil))
           (if sdiffileptr
               (unwind-protect 
                   (let ((curr-time nil))
                     
                     (sdif::SdifFReadGeneralHeader sdiffileptr)
                     (sdif::SdifFReadAllASCIIChunks sdiffileptr)
                     
                     (loop while (and 
                                  (check-current-singnature sdiffileptr) ;;; more frames in the file
                                  (or (not curr-time) (not tmax) (not (>= curr-time tmax)))) 
                           
                           do (sdif::SdifFReadFrameHeader sdiffileptr)
                           
                           (let ((sig (sdif::SdifSignatureToString (sdif::SdifFCurrFrameSignature sdiffileptr)))
                                 (sid (sdif::SdifFCurrId sdiffileptr)))
                             (setq curr-time (sdif::SdifFCurrTime sdiffileptr))
                             
                             (if (and (or (not streamNum) (and (numberp streamnum) (= streamNum sid))
                                            (and (listp streamnum) (find sid streamNum :test '=)))
                                        (or (not frameT) (string-equal frameT sig))
                                        (or (not tmin) (>= curr-time tmin))
                                        (or (not tmax) (<= curr-time tmax)))
                                 
                                 (let ((frame (make-instance 'SDIFFrame :frametime curr-time :frametype sig :streamid sid
                                                      :lmatrices (let ((nummatrix (sdif::SdifFCurrNbMatrix sdiffileptr)))
                                                                   (loop for i from 1 to nummatrix 
                                                                         collect (get-matrix-from-sdif sdiffileptr t))))))
                                   (when apply-fun (funcall apply-fun frame))
                                   (push frame frame-list)
                                   )
                               
                               (sdif::sdiffskipframedata sdiffileptr))
                                  
                             (sdif::sdif-read-next-signature sdiffileptr))))
                 (sdif::SDIFFClose sdiffileptr))
             (om-beep-msg "Error loading SDIF file -- bad pointer: ~D" self))
           (reverse frame-list)
           ))))

;;;==========================
;;; TEXT CONVERSION
;;;==========================

(defmethod* SDIF->text ((self string) &optional out-filename)
   
   :indoc '("SDIF file" "text file pathname")
   :doc "Converts <self> to text-SDIF in <out-filename>."
   (let ((outfile (if out-filename 
                      (or (handle-new-file-exists out-filename)
                          (om-choose-new-file-dialog :types (list (format nil (om-str :file-format) "Text") "*.txt" )))
                    (outfile "tmp_sdif_to_text_convert.txt"))))
     (when outfile 
       (let ((SDIFF (sdif::sdif-open-file self)))
         (when SDIFF 
           (sdif::SdifToText SDIFF (namestring outfile))
           (sdif::SDIFFClose SDIFF)
           outfile)))))
     
(defmethod* SDIF->text ((self pathname) &optional out-filename)
   (SDIF->text (namestring self) out-filename))
  
(defmethod* SDIF->text ((self SDIFFile) &optional out-filename)
   (SDIF->text (file-pathname self) out-filename))


