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
       (mapcar 'sdif-size (LMatrices self)))))

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
          for i from 0 do
          (om-write-ptr data-ptr (* i data-type-size) 'single-float (coerce val 'single-float)))
    (sdif::SdifFWriteMatrix file-ptr 
                            (sdif::SdifStringToSignature (matrixtype matrix))
                            data-type-size (elts matrix) (fields matrix) data-ptr)
    (om-free-memory data-ptr)
    ))

 

(defmethod sdif-write ((self sdifframe) file-ptr)
   (let ((framesize (sdif-size self)))
     (sdif::SdifFSetCurrFrameHeader file-ptr 
                                    (sdif::SdifStringToSignature (frametype self))
                                    framesize (length (Lmatrices self)) 
                                    (streamID self) (coerce (frametime self) 'double-float))
     (sdif::SdifFWriteFrameHeader file-ptr)
     (loop for item in (LMatrices self) do (sdif-write item file-ptr))
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
    (sdif::SdifFGetAllTypefromSdifString fileptr sstr)))
  
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



