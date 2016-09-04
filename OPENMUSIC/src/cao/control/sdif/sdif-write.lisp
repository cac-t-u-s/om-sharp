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
     (align-bytes (* 4 (num-elts self) (num-fields self)) 8)))

(defmethod sdif-size ((self sdifframe))
  (reduce 
   '+ (cons
       (sdif-header-size self)
       (mapcar 'sdif-size (LMatrices self)))))

(defmethod sdif-write ((matrix sdifmatrix) file-ptr)
  (let* ((datatype 4)
         (data (if (listp (car (data matrix))) (flat (mat-trans (data matrix))) (data matrix)))
         (data-ptr (om-alloc-memory (* datatype (num-fields matrix) (num-elts matrix)))))
    (loop for val in data 
          for i = 0 then (+ i 1) do
          (om-write-ptr data-ptr (* i datatype) :float (coerce val 'single-float)))
    (sdif::SdifFWriteMatrix file-ptr 
                            (sdif::SdifStringToSignature (matrixtype matrix))
                            datatype (num-elts matrix) (num-fields matrix) data-ptr)
    (om-free-memory data-ptr)
    ))

(defmethod sdif-write ((self sdifframe) file-ptr)
   (let ((framesize (sdif-size self)))
     (sdif::SdifFSetCurrFrameHeader file-ptr 
                                    (sdif::SdifStringToSignature (frametype self))
                                    framesize (length (Lmatrices self)) 
                                    (streamID self) (coerce (date self) 'double-float))
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


#|
;;; SDIF-Buffer
(defmethod! save-sdif-file ((self sdif-buffer) &key out options)
   :icon 639
   :indoc '("an SDIF-buffer" "format options" "output pathname")
   :initvals '(nil nil t)
   :doc "Saves the contents of <self> as an SDIF file in <outpath>.

<self> is an SDIF-Buffer object or some other object having the SAVE-SDIF-FILE method implemented.
<options> are specific options depending on <self>.

If <outpath> is not specified, a pop-up dialog will open and allow to choose a destination pathname.
"
   (declare (ignore options))
   (let* ((outfile (or (and out 
                            (handle-new-file-exists out))
                       (om-choose-new-file-dialog)))
          (dir (om-make-pathname :directory outfile)))
       (when outfile 
         (unless (probe-file dir)
           (om-create-directory dir))
         (let ((thefile (sdif-open-file (namestring outfile) :eWriteFile)))
           (sdif::SdifFWriteGeneralHeader thefile)
           (write-types-table thefile (list! (types self)))
           (write-nvt-tables thefile (cons (default-om-NVT) (list! (NVTs self))))
           (sdif::SdifFWriteAllASCIIChunks thefile)
           (loop for item in (LFrames self) do
                 (save-sdif item thefile))
           (sdif-close-file thefile))
         outfile)))
|#
