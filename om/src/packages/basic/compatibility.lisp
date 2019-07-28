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

;;; SPECIAL HACKS TO LOAD OBJECTS AND CODE FROM OM6

(in-package :om)

;;;================
;;; BPF
;;;================

(defun simple-bpf-from-list (x-points y-points &optional (class 'bpf) (decimals 0))
  (make-instance class :x-points x-points :y-points y-points :decimals decimals))

(defun 3Dc-from-list (xlist ylist zlist &optional (class '3DC) (decimals 0))
  (make-instance class :x-points xlist :y-points ylist :z-points zlist :decimals decimals))


;;;================
;;; CLASS-ARRAY
;;;================

(defun get-init-slots-of-class (class)
  (mapcar #'(lambda (slot)
              (list (slot-definition-name slot) (slot-definition-type slot))) 
          (class-slots (find-class class))))

(defmethod (setf lcontrols) (val obj) nil)


(defmethod (setf data) (data (array class-array))
  
  ;; init-array-from-csound-instr
  (om-init-instance array)
 
  ;(cons array
  ;             (loop for field in (cr::cs-description-params (cr::cs-instr array))
  ;                   for field-data in data collect
  ;                   (make-array-field :name field-name :data field-data :decimals 4))

  (setf (slot-value array 'data)
        (if (every #'array-field-p data) data
          (loop for array-field in (data array)
                for field-data in data collect
                (make-array-field :name (array-field-name array-field) 
                                  :decimals (array-field-decimals array-field)
                                  :default (array-field-default array-field)
                                  :type (array-field-type array-field)
                                  :doc (array-field-doc array-field)
                                  :data field-data)
                )))
  )



       
     
 
     
  
