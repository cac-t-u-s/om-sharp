(in-package :om)

;;;=========================
;;; dummy class for SDIF lib initialization
;;;=========================
(defclass sdif-object () ())

(defmethod initialize-instance :after ((self sdif-object) &rest initargs)
  (sdif::sdif-init-cond))


;;;=========================
;;; SDIF TYPE
;;;=========================
(defclass* SDIFType ()
   ((Struct :initform 'F :initarg :Struct :accessor struct :documentation "frame (= f) or matrix (= m)")
    (Signature :initform "" :initarg :Signature :accessor signature :documentation "SDIF type signature")
    (Description :initform nil :initarg :description :accessor description :documentation "type description"))
   (:documentation "An SDIF type declaration to be written in an SDIF file or buffer.

SDIF types define data structures in the SDIF frameworks.
They are identified by a 4 ASCII characters (e.g. 1TRC, 1FQ0, ETC..)

See http://sdif.sourceforge.net/ for more inforamtion about SDIF TYPES.

Matrix type description is a list with the names of the different description fields in this matrix (e.g. '(\"freq\" \"amp\" \"phase\"))
Frame type description is a list with the types and names of the different matrices allowed in the frame (e.g. '((\"TYP1\" \"matrix1\") (\"TYP2\" \"matrix2\"))

"
    ))

;;; NEEDS SUPPORT FOR MENUINS IN CLASSES !!
;; (("Frame" 'F)  ("Matrix" 'M))

;;;=========================
;;; SDIF NAME-VALUE TABLE
;;;=========================
(defclass* SDIFNVT ()
   ((NV-pairs :initform nil :initarg :NV-pairs :accessor NV-pairs :documentation "list of (name value) pairs")
    (TableName :initform nil :initarg :TableName :accessor TableName :documentation "name (string)")
    (ID :initform 0 :initarg :ID :accessor ID :documentation "table ID (integer)")
    (tnum :initform 0 :accessor tnum))
   (:documentation "An SDIF Name/Value table to be written in an SDIF file or buffer.

SDIF NVTs gibe additional info on the data in an SDIF file in the form of name/value pairs
They have an ID and ususally a TableName name/value.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF TYPES.

"
    ))

(defun default-om-NVT ()
  (make-instance 'SDIFNVT 
                 :tablename "FileInfo"
                 :ID 0
                 :NV-pairs (list 
                            (list "Author" (string+ "OM " *version-string*))
                            (list "Date" (om-get-date)))))

;;;=========================
;;; SDIF MATRIX
;;;=========================

(defclass* SDIFMatrix (OMArray)
   ((matrixtype :initform nil :initarg :matrixtype :accessor matrixtype :documentation "SDIF matrix type signature"))
   (:documentation "SDIF data stored as a 2D array.

SDIF Matrix define multidimensional sound description data at a given moment (no temporal dimension).
The lines of the matrix define the different fields of the description data (e.g. frequencies, amplitudes, etc.)
The number of fields usually depends on the SDIF type specification corresponding to <signature>.
The number of elements is variable.

SDIFMatrix is a virtual 2D array of <num-fields> x <num-elts>.
All data is contained in <data> 
- as a list of lists ((freqs) (amps) (phases) ...)
or
- as flat list, containing the successive field values for each element (e.g. '(freq1 amp1 phase1 frezq2 amp2 phase2 ...)).
This case optimizes file writing operations.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF

 "))

;;;=========================
;;; SDIF FRAME
;;;=========================
;;; Ensemble de matrices correspondant a un instant d'echantillonnage 
;;; Unite minimum pour ecrire dans un fichier SDIF
(defclass* sdifframe (sdif-object data-frame)
   ((date :accessor date :initarg :date :initform 0 :documentation "time of the frame")
    (frametype :initform nil :initarg :frametype :accessor frametype :documentation "4-char signature of the SDIF frame type")
    (streamID :initform 0 :initarg :streamID :accessor streamID :documentation "SDIF stream ID (integer)")
    (lMatrices :initform nil :initarg :LMatrices :accessor LMatrices :documentation "list of SDIFMatrix objects"))
   (:documentation "An SDIF data chunk.

SDIF frames are data chunk containing one or more SDIF matrices (<lMatrices>) and precisely localized in time (<fTime>).
Frames can be grouped in streams identified by an ID (<streamID>) in order to describe parallele data.  
The number and type of matrices allowed in a given frame depends on the SDIF frame type specification corresponding to <signature>.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.
"
))

;;; in case there is just 1 matrix -- avoids using list
(defmethod initialize-instance :after ((self sdifframe) &rest initargs)
   (setf (LMatrices self) (list! (LMatrices self))))

;;; merge frames for same kind of (single) matrices (adds data in the matrix)
(defun merge-frame-data (frames)
  (let ((newframes nil))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (date (car newframes)) (date fr))
                     (string-equal (frametype (car newframes)) (frametype fr)))
                (loop for matrix in (lmatrices fr) do
                      (let ((fmat (find (matrixtype matrix) (lmatrices (car newframes)) :test 'string-equal :key 'matrixtype)))
                        (if fmat 
                            (setf (data fmat) (append (data fmat) (data matrix))
                                  (num-elts fmat) (1+ (num-elts fmat)))
                          (setf (lmatrices fr) (append (lmatrices fr) (list matrix))))))
              (push (make-instance 'SDIFFrame :date (date fr) :frametype (frametype fr)
                                   :streamID 0 :lmatrices (lmatrices fr))
                    newframes))))
    (reverse newframes)))

;;; merge frames for same kind of frames (appends matrices)
(defun merge-frames (frames)
  (let ((newframes nil))
    (setf frames (sort frames '< :key 'date))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (date (car newframes)) (date fr))
                     (= (streamID (car newframes)) (streamID fr))
                     (string-equal (frametype (car newframes)) (frametype fr)))
                (setf (lmatrix (car newframes))
                      (append (lmatrices (car newframes)) (lmatrices fr)))
              (push fr newframes))))
    (reverse newframes)))



