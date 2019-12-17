;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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

;==============================================
;writing sdif data
;==============================================

(defmethod sdif-write ((self t) ptr) nil)
(defmethod sdif-write ((self list) ptr) (mapcar #'(lambda (elt) (sdif-write elt ptr)) self))

(defun align-bytes (n align)
  (* align (ceiling n align)))

(defmethod sdif-header-size ((self sdifmatrix)) 16)
(defmethod sdif-header-size ((self sdifframe)) 16)

(defmethod sdif-size ((self sdifmatrix))
  (+ (sdif-header-size self)
     (align-bytes (* 4 (elts self) (fields self)) 8)))

(defmethod sdif-size ((self sdifframe))
  (reduce 
   '+ (cons
       (sdif-header-size self)
       (mapcar 'sdif-size (lmatrix self)))))

(defmethod sdif-write ((matrix sdifmatrix) file-ptr)
  (let* ((data-type-size 4)
         ;;; flat data list
         (data (cond ((listp (car (data matrix))) 
                      (flat (mat-trans (data matrix))))
                     ((array-field-p (car (data matrix)))
                      (flat (mat-trans (mapcar 'array-field-data (data matrix)))))
                     (t (data matrix))))
         (data-ptr (om-alloc-memory (* data-type-size (fields matrix) (elts matrix)))))
    
    (loop for val in data 
          for i from 0
          do (om-write-ptr data-ptr (* i data-type-size) 'single-float (coerce val 'single-float))
          )
    (sdif::SdifFWriteMatrix file-ptr 
                            (sdif::SdifStringToSignature (matrixtype matrix))
                            data-type-size (elts matrix) (fields matrix) data-ptr)
    (om-free-memory data-ptr)
    ))

(defmethod sdif-write ((self sdifframe) file-ptr)
   (let ((framesize (sdif-size self)))
     (sdif::SdifFSetCurrFrameHeader file-ptr 
                                    (sdif::SdifStringToSignature (frametype self))
                                    framesize 
                                    (length (lmatrix self))
                                    (streamID self) 
                                    (coerce (ftime self) 'double-float))
     (sdif::SdifFWriteFrameHeader file-ptr)
     (loop for item in (lmatrix self) do (sdif-write item file-ptr))
     ))


;;;======================================
;;; TYPES / NVT / IDS
;;; to do BEFORE SdifWriteAllASCIIChunks
;;;======================================
;;; TYPES
;;;======================================
(defmethod format-type-string ((self sdiftype))
   (let ((str "") (desc ""))
     (cond ((equal 'F (struct self)) 
               (loop for item in (description self) do
                       (when (listp item) (setf desc (string+ desc (car item) " " (second item) "; "))))
               (setf desc (subseq desc 0 (max 0 (- (length desc) 1))))
               (setf str (string+ str " 1FTD " (signature self) " {" desc "}")))
              ((equal 'M (struct self)) 
               (loop for item in (description self) do
                       (when (stringp item) (setf desc (string+ desc item ", "))))
               (setf desc (subseq desc 0 (- (length desc) 2)))
               (setf str (string+ str " 1MTD " (signature self) " {" desc "}")))
              (t nil))
     str))

(defun sdif-write-types-string (fileptr types-string)
  (let ((sstr (sdif::SdifStringNew)))
    (sdif::SdifStringAppend sstr types-string)
    (sdif::SdifStringGetC sstr)
    (sdif::SdifFGetAllTypefromSdifString fileptr sstr)
    ))
  
;;; write a list of SDIFType objects 
(defun sdif-write-types (fileptr typeslist)
  (let ((str "{") (FList nil) (MList nil))
    (loop for typedef in typeslist do
           (if (typep typedef 'sdiftype) 
               (cond ((equal 'F (struct typedef)) (push typedef Flist))
                     ((equal 'M (struct typedef)) (push typedef Mlist)))
             (om-beep-msg "NOT AN SDIFTYPE: ~A" typedef)))
    (loop for mdef in Mlist do (setf str (string+ str (format-type-string mdef))))
    (loop for fdef in Flist do (setf str (string+ str (format-type-string fdef))))
    (setf str (string+ str "}"))
    (sdif-write-types-string fileptr str)))
 
;;;======================================
;;; NVT
;;;======================================
(defun sdif-write-nvt (fileptr name-values &key id tablename)
   (let ((nvtlist (sdif::SdifFNameValueList fileptr)))
     (sdif::SdifNameValuesLNewTable nvtlist (or id #xffff))
     (when tablename (sdif::SdifNameValuesLPutCurrNVT nvtlist "TableName" tablename))
     (loop for nv-pair in name-values do
           (sdif::SdifNameValuesLPutCurrNVT nvtlist (car nv-pair) (cadr nv-pair)))))

(defmethod sdif-write ((self SDIFNVT) fileptr)
  (sdif-write-nvt fileptr (nv-pairs self) :id (ID self) :tablename (tablename self)))
                
;;;======================================
;;; IDS
;;;======================================
(defun sdif-write-IDS (file id str tree)
   (let ((idstable (sdif::SdifFStreamIDTable file)))
     (sdif::SdifStreamIDTablePutSID idstable id str tree)))



;;;======================================
;;; GENERAL / TOP-LEVEL
;;;======================================
(defmethod* write-sdif-file ((frames list) &key (outpath "out.sdif") types nvts)
  
  :indoc '("a list of SDIFFrame objects" "SDIF file pathname" "list of SDIFType obvjects" "list of SDIFNVT objects")
  :outdoc '("pathname of written SDIF file")
  :initvals '(nil "out.sdif" nil nil)
  :icon 'sdif
  :doc "Writes a list of SDIFFrame objects as a new SDIF file."
  
  (let ((out-path (cond ((pathnamep outpath) outpath)
                         ((stringp outpath) (outfile outpath))
                         (t (om-choose-new-file-dialog)))))
    (when out-path
      (let ((sdiffileptr (sdif::sdif-open-file out-path sdif::eWriteFile)))
        (if sdiffileptr
          (unwind-protect 
              (progn (sdif::SdifFWriteGeneralHeader sdiffileptr)
                
                (loop for nvt in (cons (default-om-NVT) (list! nvts)) 
                      do (sdif-write nvt sdiffileptr))
                
                (when types (sdif-write-types sdiffileptr (list! types)))
            
                (sdif::SdifFWriteAllASCIIChunks sdiffileptr)
                
                (loop for frame in frames
                      do (sdif-write frame sdiffileptr))
                )
            (sdif::SDIFFClose sdiffileptr))
          (om-beep-msg "Could not open file for writing: ~A" out-path))
        (probe-file out-path)
        ))))



;;;==================================
;;; USING DYNAMIC STREAM POINTER (IN A VISUAL PROGRAM)
;;; EQUIVALENT TO OM6's FILE-BOX
;;; ==================================

(defclass sdif-fstream (fstream) ())

(defmethod om-cleanup ((self sdif-fstream))
  (when (open? self)
    (sdif::sdiffclose (fs self))
    (setf (open? self) nil)))



(defmethod* open-SDIF-stream (path &key (direction :io))
  :initvals '(nil :io :supersede)
  :indoc '("a valid pathname" "stream direction (read/write)" "behaviour if the file exists")
  :menuins '((1 (("read" :input) ("write" :output) ("read/write" :io))) 
             (2 (("rename" :rename) ("supersede" :supersede) ("overwrite" :overwrite) ("append" :append))))
  :icon :file
  :doc "Opens an SDIF file stream where other functions can read and write.

Open FILE-STREAMs are automatically closed when they are not used anymore by the Common Lisp garbage collection system, however, it is recommended to close them explicitely with CLOSE-FILE-STREAM as soon as they are not needed in order to limit the number of streams open
" 
  (let ((SDIFF (sdif::sdif-open-file path
                                     (case direction
                                       (:input sdif::eReadFile)
                                       (:output sdif::eWriteFile)
                                       (otherwise sdif::eReadWriteFile)))))
    
    (unless SDIFF
      (om-beep-msg "ERROR SDIF stream could not be open in mode ~D: ~A" direction path))
    
    (make-instance 'sdif-fstream 
                   :fs SDIFF
                   :fpath path
                   :open? (if SDIFF t nil))
    ))



(defmethod* sdif-write-header ((fstream sdif-fstream) &optional (types nil) (nvts nil) (sids nil))
  :icon :write
  :indoc '("an SDIF file pointer" "list of SDIFType" "list of SDIFNVT" "list of stream ID list")
  :doc "Writes the header of the SDIF file in <fstream>.

This is a compulsory operation before to start writing SDIF frames in the file.
<fstream> is a file pointer created by open-SDIF-stream.

<types>, <nvts> and <sids> are SDIF types, name/value tables to declare and write in the file header.
" 
  (if (fs fstream)
      (progn
        (sdif::SdifFWriteGeneralHeader (fs fstream))
        (loop for NVT in (cons (default-om-NVT) (list! nvts))
              do (sdif-write NVT (fs fstream)))
        (when types (sdif-write-types (fs fstream) (list! types)))
        (when sids 
          (loop for SID in sids do
                (apply #'sdif-write-IDS (cons (fs fstream) SID))))
        (sdif::SdifFWriteAllASCIIChunks (fs fstream)))
    (progn 
      (om-beep-msg "ERROR No valid open SDIF file stream!!")
      (abort))
    ))



(defmethod* sdif-write-frame ((self sdifframe) (fstream sdif-fstream))
  :icon :write
  :indoc '("an SDIFFrame to write" "an SDIF file pointer")
  :doc "Writes the SDIF frame <self> in <fstream>.

<fstream> is a file pointer created by open-SDIF-stream." 
  (sdif-write self (fs fstream)))

(defmethod* sdif-write-frame ((self t) (fstream sdif-fstream))
  (print "Error: only write frames in SDIF files!") 
  nil)

