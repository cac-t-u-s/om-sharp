;============================================================================
; o.OM : odot OSC interface in OM
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


;;;============================
;;; A WRAPPER AROUND O. BUNDLE
;;;============================

(defclass! o.bundle (data-frame om-cleanup-mixin)
  ((bundle_s :accessor bundle_s :initform nil :initarg :bundle_s)))

(defstruct (o.pointer (:include oa::om-pointer)))

(defmethod free-foreign-pointer ((self o.bundle))
  (when (bundle_s self)
    (oa::om-release (bundle_s self))))

(defmethod assign-foreign-pointer ((self o.bundle) ptr)
  (setf (bundle_s self) (make-o.pointer :ptr ptr))
  (oa::om-retain (bundle_s self)))

;; (gc-all)

(defmethod oa::om-release ((bundle_s o.pointer))
  (when (<= (decf (oa::om-pointer-count bundle_s)) 0)
    (unless (om-null-pointer-p (oa::om-pointer-ptr bundle_s))
      ;(om-print (format nil "Free OSC bundle ~A" bundle_s) "O.DEBUG")
      (odot::osc_bundle_s_deepFree (oa::om-pointer-ptr bundle_s)))))

(defmethod om-cleanup ((self o.bundle))
  ;(free-foreign-pointer self)
  (when (bundle_s self)
    (oa::om-release (bundle_s self))))

(defmethod clone-object ((self o.bundle) &optional clone)
  (let ((o.bndl (call-next-method)))
    (setf (bundle_s o.bndl) (bundle_s self))
    (when (bundle_s o.bndl) (oa::om-retain (bundle_s o.bndl)))
    o.bndl))


(defmethod get-timetag ((self o.bundle))
  (when (bundle_s self)
    (odot::decode_osc_timetag
     (odot::osc_bundle_s_getTimetag 
      (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self)))
      (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self)))))))
  

(defmethod onset ((self o.bundle)) (get-timetag self))
(defmethod (setf onset) (date (self o.bundle)) (update-bundle-pointer-timetag self date))


(defmethod get-messages ((self o.bundle))
  (when (bundle_s self)
    (odot::osc_decode_bundle_s_data 
     (oa::om-pointer-ptr (bundle_s self)))))

;;; change bundle
(defmethod update-bundle-pointer ((self o.bundle) data timetag)
  (free-foreign-pointer self)
  (assign-foreign-pointer self (odot::osc_make_foreign_bundle_s data timetag))
  self)

;;; change the time tag (same bundle)
(defmethod update-bundle-pointer-timetag ((self o.bundle) timetag)
  (when (and (bundle_s self) timetag)
    (odot::osc_bundle_s_setTimetag 
     (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self))) 
     (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self))) 
     (odot::make_osc_timetag timetag))))

;;; change the bundle but restore existing timetag (if any)
(defmethod update-bundle-pointer-data ((self o.bundle) data)
  (let ((timetag nil))
    (when (bundle_s self)
      (setq timetag (odot::osc_bundle_s_getTimetag (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self))) 
                                                   (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self)))))
      (free-foreign-pointer self))
    (assign-foreign-pointer self (odot::osc_make_foreign_bundle_s data))
    (when timetag (update-bundle-pointer-timetag self timetag))
    self))

(defmethod data-frame-text-description ((self o.bundle))
  (cons "O. BUNDLE" (flat (mapcar 'format-message (get-messages self)))))

(defmethod data-size ((self o.bundle))
  (length (flat (get-messages self))))

(defmethod get-frame-action ((self o.bundle))
  #'(lambda () 
      (when (bundle_s self)
        (odot::osc_send_bundle_s 3000 "localhost" (oa::om-pointer-ptr (bundle_s self))))))

(defmethod make-o.bundle ((self osc-bundle))
  (let ((b (make-instance 'o.bundle)))
    (objfromobjs self b)
    b))

(defmethod make-o.bundle ((messages list))
  (let ((ob (make-instance 'o.bundle)))
    (assign-foreign-pointer ob (odot::osc_make_foreign_bundle_s messages))
    ob))

(defmethod o.bundle-size ((self o.bundle)) (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self))))
(defmethod o.bundle-ptr ((self o.bundle)) (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self))))

(defmethod objfromobjs ((model osc-bundle) (target o.bundle))
  (update-bundle-pointer target (messages model) (date model))
  target)

(defmethod objfromobjs ((model o.bundle) (target osc-bundle))
  (setf (date target) (get-timetag model)
        (messages target) (get-messages model))
  target)

    

