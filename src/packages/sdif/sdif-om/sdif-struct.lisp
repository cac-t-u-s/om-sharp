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

;;;=========================
;;; dummy class for SDIF lib initialization
;;;=========================
(defclass sdif-object () ())

;(defmethod initialize-instance :after ((self sdif-object) &rest initargs)
;  (sdif::sdif-init-cond))


;;;=========================
;;; SDIF TYPE
;;;=========================
(defclass* SDIFType ()
   ((struct :initform 'F :initarg :struct :accessor struct :documentation "frame (= f) or matrix (= m)")
    (signature :initform "" :initarg :Signature :accessor signature :documentation "SDIF type signature")
    (description :initform nil :initarg :description :accessor description :documentation "type description"))
   (:documentation "An SDIF type declaration to be written in an SDIF file or buffer.

SDIF types define data structures in the SDIF frameworks.
They are identified by a 4 ASCII characters (e.g. 1TRC, 1FQ0, ETC..)

See http://sdif.sourceforge.net/ for more inforamtion about SDIF TYPES.

Matrix type description is a list with the names of the different description fields in this matrix (e.g. '(\"freq\" \"amp\" \"phase\"))
Frame type description is a list with the types and names of the different matrices allowed in the frame (e.g. '((\"TYP1\" \"matrix1\") (\"TYP2\" \"matrix2\"))

"
    ))

;;; we don't really need this as a class...
(defmethod* make-sdif-m-type (sign fieldnames)
  (make-instance 'SDIFType :struct 'm :signature sign :description fieldnames))

(defmethod* make-sdif-f-type (sign matrixtypes &optional matrixnames)
  (make-instance 'SDIFType :struct 'f :signature sign 
                 :description 
                 (loop for type in (list! matrixtypes)
                       for i = 0 then (+ i 1) collect
                       (list type (or (nth i matrixnames) (format nil "matrix~D" (1+ i)))))))

(defmethod sdif-types-from-field-names (field-names &key mat-type f-type)
  (let ((ft (or f-type "XFRA"))
        (mt (or mat-type "XMAT")))
    (list (make-sdif-f-type ft mt)
          (make-sdif-m-type mt field-names))
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
                            (list "Author" (string+ *app-name* " " *version-string*))
                            (list "Date" (om-get-date)))))

;;;=========================
;;; SDIF MATRIX
;;;=========================

(defclass* SDIFMatrix (2D-array)
   ((matrixtype :initform nil :initarg :matrixtype :accessor matrixtype :documentation "SDIF matrix type signature")
    ;(elts :initform 0 :initarg :elts  :accessor elts :documentation "number of elements (a.k.a lines)")
    ;(fields :initform nil :initarg :fields :accessor fields :documentation "Name of SDIF fields")
    (data :initform nil :initarg :data :accessor data :documentation "data matrix / list of lists : (field1 field2 ...)")
    )
   (:documentation "SDIF data stored as a 2D array.

SDIF Matrix define multidimensional sound description data at a given moment (no temporal dimension).
The lines of the matrix define the different fields of the description data (e.g. frequencies, amplitudes, etc.)
The number of fields usually depends on the SDIF type specification corresponding to <signature>.
The number of elements is variable.

SDIFMatrix is a virtual 2D array of <fields> x <elts>.
All data is contained in <data> 
- as a list of lists ((freqs) (amps) (phases) ...)
or
- as flat list, containing the successive field values for each element (e.g. '(freq1 amp1 phase1 frezq2 amp2 phase2 ...)).
This case optimizes file writing operations.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF

 "))


; (defmethod allow-extra-controls ((self SDIFMatrix)) t)

(defun merge-matrix-data (data1 data2)
  (loop for c1 in data1
        for c2 in data2 collect
        (append c1 c2)))


;;; TODO:
;;; matrixtype could be a SDIFtype and do something at initialization to initialize the fields...


;;;=========================
;;; SDIF FRAME
;;;=========================
;;; Ensemble de matrices correspondant a un instant d'echantillonnage 
;;; Unite minimum pour ecrire dans un fichier SDIF
(defclass* SDIFFrame (sdif-object data-frame)
  ((frametype :initform nil :initarg :frametype :initarg :signature :accessor frametype :documentation "4-char signature of the SDIF frame type")
   (ftime :accessor ftime :initarg :ftime :initform 0.0 :documentation "time of the frame (s)")
   (streamid :initform 0 :initarg :streamid :accessor streamid :documentation "SDIF stream ID (integer)")
   (lmatrix :initform nil :initarg :lmatrix :accessor lmatrix :documentation "list of SDIFMatrix objects"))
   (:documentation "An SDIF data chunk.

SDIF frames are data chunk containing one or more SDIF matrices (<lmatrix>) and precisely localized in time (<fTime>).
Frames can be grouped in streams identified by an ID (<streamid>) in order to describe parallele data.  
The number and type of matrices allowed in a given frame depends on the SDIF frame type specification corresponding to <signature>.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.
"
))

;;; in case there is just 1 matrix -- avoids using list
(defmethod initialize-instance :after ((self sdifframe) &rest initargs)
   (setf (lmatrix self) (list! (lmatrix self)))
   (setf (slot-value self 'onset) (sec->ms (ftime self))))

;;; the onset from data-frame/timed-object is computed from ftime
(defmethod onset ((self SDIFFrame)) 
  (sec->ms (ftime self)))

(defmethod (setf onset) (date (self SDIFFrame)) 
  (call-next-method)
  (setf (ftime self) (ms->sec date)))

;;; merge frames for same kind of (single) matrices (adds data in the matrix)
(defun merge-frame-data (frames)
  (let ((newframes nil))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (ftime (car newframes)) (ftime fr))
                     (string-equal (frametype (car newframes)) (frametype fr)))
                (loop for matrix in (lmatrix fr) do
                      (let ((fmat (find (matrixtype matrix) (lmatrix (car newframes)) :test 'string-equal :key 'matrixtype)))
                        (if fmat 
                            (setf (data fmat) (merge-matrix-data (data fmat) (data matrix))
                                  (elts fmat) (1+ (elts fmat)))
                          (setf (lmatrix fr) (append (lmatrix fr) (list matrix))))))
              (push (make-instance 'SDIFFrame :ftime (ftime fr) :frametype (frametype fr)
                                   :streamID 0 :lmatrix (lmatrix fr))
                    newframes))))
    (reverse newframes)))


;;; merge frames for same kind of frames (appends matrices)
(defun merge-frames (frames)
  (let ((newframes nil))
    (setf frames (sort frames '< :key 'ftime))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (ftime (car newframes)) (ftime fr))
                     (= (streamID (car newframes)) (streamID fr))
                     (string-equal (frametype (car newframes)) (frametype fr)))
                (setf (lmatrix (car newframes))
                      (append (lmatrix (car newframes)) (lmatrix fr)))
              (push fr newframes))))
    (reverse newframes)))


;;; use in a DATA-STREAM
(defmethod data-frame-text-description ((self SDIFFrame))
  (cons (string+ "SDIF " (frametype self))
        (flat (mapcar #'(lambda (m) 
                          (format nil "~A ~A" (matrixtype m) (data m)))
                      (lmatrix self))))
  )
