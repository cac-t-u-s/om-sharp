;/************************************************************************************/
;/*  FILE DESCRIPTION							             */
;/*----------------------------------------------------------------------------------*/
;/*!
; *   @file       omspat-utils.lisp
; *   @brief      lisp-interface to OMSpat.framework Utilities
; *   @author     Thibaut Carpentier
; *   @version    $(PRODUCT_VERSION)
; *   @date       05/11/2013
; *
; */
;/************************************************************************************/

(in-package :spat)

;;;=========
;;; GENERAL
;;;=========

;;;======================================================================
(cffi:defcfun ("OmSpatInitialize" OmSpatInitialize) :boolean)
(cffi:defcfun ("OmSpatIsInitialized" OmSpatIsInitialized) :boolean)
(cffi:defcfun ("OmSpatGetVersion" OmSpatGetVersion) :string)
(cffi:defcfun ("OmSpatGetLastError" OmSpatGetLastError) :string)
(cffi:defcfun ("OmSpatClearLastError" OmSpatClearLastError) :pointer)
(cffi:defcfun ("OmSpatGetGlobalSamplingRate" OmSpatGetGlobalSamplingRate) :float)
(cffi:defcfun ("OmSpatSetGlobalSamplingRate" OmSpatSetGlobalSamplingRate) :void (samplerate :float))
(cffi:defcfun ("OmSpatDebugOSCPacket" OmSpatDebugOSCPacket) :boolean (bundle :pointer) (size :unsigned-long)) ;
;;;======================================================================

;(OmSpatIsInitialized)
;(OmSpatInitialze)
;(OmSpatGetVersion)
;(OmSpatGetLastError)

(defun test-osc-command (messages)
  (let ((ob (om::make-o.bundle (make-instance 'om::osc-bundle :messages (print messages)))))
    (spat::OmSpatDebugOSCPacket (om::o.bundle-ptr ob) (om::o.bundle-size ob))))
; (test-osc-command '(("/numsources" 4)))


;;;=========
;;; UTILS
;;;=========

(defun number-to-double (i) (coerce i 'double-float))
(defun number-to-float (i) (coerce i 'float))

(defun coerce-list (liste type)
  (case type 
    (:double (mapcar #'number-to-double liste))
    (:float (mapcar #'number-to-float liste))
    (otherwise nil)))

; Takes a list from OM and converts it into a C array. 
; The C-array is allocated here, but not freed
(defun lisp-list-to-c-array (liste &optional (type :float)) 
  (cffi::foreign-alloc :float :initial-contents (coerce-list liste type)))

; Takes a C-array and converts it into a lisp list
; The pointer is not freed here.
(defun c-array-to-lisp-list (ptr size &optional (type :float))
  (loop for i from 0 to (- size 1) collect (cffi::mem-aref ptr type i)))


;;;====================
;;; POINTS
;;;====================

(defconstant FORMAT_XYZ 0)
(defconstant FORMAT_AED 1)

(cffi:defcstruct omspat-points 
  (format :int) (numPoints :unsigned-int) (data :pointer))

(cffi::defctype spat-points omspat-points)

;;;======================================================================
;;;======================================================================

(cffi:defcfun ("OmSpatCoordinatesFormatName" OmSpatCoordinatesFormatName) :string (format :int))
(cffi:defcfun ("OmSpatResizePoints" OmSpatAlloOmSpatResizePoints) :boolean (points omspat-points) (numPoints :unsigned-int))
(cffi:defcfun ("OmSpatFreePoints" OmSpatFreePoints) :boolean (points omspat-points))
(cffi:defcfun ("OmSpatPrintPoints" OmSpatPrintPoints) :boolean (src omspat-points) (printData :boolean))

(cffi:defcfun ("OmSpatConvertPoints" OmSpatConvertPoints) :boolean 
  (dest omspat-points) (new-format :int) (src omspat-points))
(cffi:defcfun ("OmSpatTranslatePoints" OmSpatTranslatePoints) :boolean 
  (dest omspat-points) (offsetx :float) (offsety :float) (offsetz :float) (src omspat-points))
(cffi:defcfun ("OmSpatRotatePoints" OmSpatRotatePoints) :boolean 
  (dest omspat-points) (yawInDegrees :float) (pitchInDegrees :float) (rollInDegrees :float) (src omspat-points))
(cffi:defcfun ("OmSpatScalePoints" OmSpatScalePoints) :boolean (dest omspat-points) 
  (scalex :float) (scaley :float) (scalez :float) (src omspat-points))

;;;======================================================================
;;;======================================================================


(defun 2D-to-aed (p) (if (= 2 (length p)) (list (car p) 0 (cadr p)) p))
(defun 2D-to-xyz (p) (if (= 2 (length p)) (list (car p) (cadr p) 0) p))

;;; input = list of 2D or 3D points ((x1 y1 z1) (x2 y2 z2) ...)
;;; output = pointer on a flat C array 
(defun points-list-to-buffer (list &optional (format 0))
  (lisp-list-to-c-array (reduce 'append (mapcar (if (= format FORMAT_AED) #'2D-to-aed #'2D-to-xyz) list))))

(defun allocate-spat-points (&key (format nil format-supplied-p) 
                                  (size nil size-supplied-p) 
                                  (points nil points-supplied-p))
  (let ((pts (cffi::foreign-alloc 'spat-points))
        (npts (or size (length points))))
    (when format (setf (cffi:foreign-slot-value pts 'omspat-points 'format) format))
    (setf (cffi:foreign-slot-value pts 'omspat-points 'numPoints) npts)
    (if points 
        (setf (cffi:foreign-slot-value pts 'omspat-points 'data) (points-list-to-buffer points format))
      (setf (cffi:foreign-slot-value pts 'omspat-points 'data) (fli:allocate-foreign-object :type :float :nelems (* 3 npts))))
    pts))

(defun free-spat-points (spat-points)
  (cffi::foreign-free (cffi:foreign-slot-value spat-points 'omspat-points 'data))
  (cffi::foreign-free spat-points))

(defun get-spat-points (spatpoints) 
  (c-array-to-lisp-list (cffi:foreign-slot-value spatpoints 'omspat-points 'data) 
                        (* 3 (cffi:foreign-slot-value spatpoints 'omspat-points 'numPoints))))

(defun group-list (list groupsize)
  (let ((segmentation 
         (loop for n from 1 to (ceiling (length list) groupsize) collect groupsize)))
    (let ((list2 list) (res nil))
      (catch 'gl
        (loop for segment in segmentation
              while list2
              do (push (loop for i from 1 to segment
                             when (null list2)
                             do (progn (push sublist res) (throw 'gl 0))
                             end
                             collect (pop list2) into sublist
                             finally (return sublist))
                       res)))
      (nreverse res)
      )))

(defun spat-aed-to-xyz (pos)
  (let* ((positions (allocate-spat-points :format FORMAT_AED :points pos))
         (newpositions (allocate-spat-points :format FORMAT_XYZ :size (length pos)))
         (rep nil))
    ;(om-print-points positions t)
    (OmSpatConvertPoints newpositions FORMAT_XYZ positions)
    ;(om-print-points newpositions t)
    (setf rep (group-list (get-spat-points newpositions) 3))
    (free-spat-points positions)
    (free-spat-points newpositions)
    rep))

(defun spat-xyz-to-aed (pos)
  (let* ((positions (allocate-spat-points :format FORMAT_XYZ :points pos))
         (newpositions (allocate-spat-points :format FORMAT_AED :size (length pos)))
         (rep nil))   
    ;(om-print-points positions t)
    (OmSpatConvertPoints newpositions FORMAT_AED positions)
    ;(om-print-points newpositions t)
    (setf rep (group-list (get-spat-points newpositions) 3))
    (free-spat-points positions)
    (free-spat-points newpositions)
    rep))

