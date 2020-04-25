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

;;; LOAD OBJECTS AND CODE FROM OM6

(in-package :om)


;==================
; Functions
;==================
;;; compat API:
(defmethod update-arg-names ((reference (eql 'om-sample)))
  '(("sample-rate" "nbs-sr")))

;;; => do it for other arithmetic functions...
(defmethod update-arg-names ((reference (eql 'om*)))
  '(("self" "arg1") ("num" "arg2")))
(defmethod update-arg-names ((reference (eql 'om/)))
  '(("self" "arg1") ("num" "arg2")))
(defmethod update-arg-names ((reference (eql 'om+)))
  '(("self" "arg1") ("num" "arg2")))
(defmethod update-arg-names ((reference (eql 'om-)))
  '(("self" "arg1") ("num" "arg2")))



;;;================
;;; BPF
;;;================

(defun simple-bpf-from-list (x-points y-points &optional (class 'bpf) (decimals 0))
  (make-instance class :x-points x-points :y-points y-points :decimals decimals))

(defmethod (setf bpfcolor) ((c t) (self bpf))
  (when c (setf (slot-value self 'color) (om-correct-color c))))

(defmethod bpfcolor ((self bpf))
  (color self))

(defun 3Dc-from-list (xlist ylist zlist &optional (class '3DC) (decimals 0))
  (make-instance class :x-points xlist :y-points ylist :z-points zlist :decimals decimals))

;;; xxx-LIBs
(defclass bpf-lib () ((bpf-list :accessor bpf-list :initarg :bpf-list :initform nil)))
(defclass bpc-lib (bpf-lib) ())
(defclass 3DC-lib (bpc-lib) ())

;;; compat API:
(defmethod update-reference ((ref (eql 'bpf-lib))) 'collection)
(defmethod update-reference ((ref (eql 'bpc-lib))) 'collection)
(defmethod update-reference ((ref (eql '3dc-lib))) 'collection)

(defmethod update-value ((self bpf-lib))
  (make-instance 'collection :obj-list (bpf-list self)))

(defmethod update-reference ((ref (eql '3D-trajectory))) '3DC)

;;;===================
;;; TEXTFILE/TEXTBUFER
;;;===================

;;; text contents
(defun load-buffer-textfile (listline class edmode &optional (evmode "text"))
  (declare (ignore class edmode evmode))
  (omng-load 
   `(:object
     (:class textbuffer)
     (:slots ((:contents ,(omng-save listline)))))))

;;; linked to a file
;;; !! pathname can be relative ("infile/...")
(defun load-textfile (pathname class edmode &optional (evmode "text"))
  (declare (ignore class edmode evmode))
  (omng-load 
   `(:object
     (:class textbuffer)
     (:slots ((:contents ,(omng-save (lines-from-file pathname))))))))


(defmethod om-load-editor-box1 (name (reference (eql 'textfile)) 
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore reference fname editparams meditor pictlist))

  (let* ((eval-mode-str (find-value-in-kv-list (cdr (eval (fourth inputs))) :value))
         (eval-mode (cond ((string-equal eval-mode-str "text") :text-list)
                          ((string-equal eval-mode-str "data list") :lines-cols)
                          ((string-equal eval-mode-str "list") :list)
                          ((string-equal eval-mode-str "value") :value)
                          )))
    `(:box
      (:type :object)
      (:reference textbuffer)
      (:name ,name)
      (:value ,value)
      (:x ,(om-point-x position))
      (:y ,(om-point-y position))
      (:w ,(om-point-x size))
      (:h ,(om-point-y size))
      (:lock ,(if lock (cond ((string-equal lock "x") :locked)
                             ((string-equal lock "&") :eval-once))))
      (:showname ,show-name)
      (:display ,(if spict :mini-view :hidden))
      (:edition-params (:output-mode ,eval-mode))
      (:inputs 
       (:input (:type :standard) (:name "self") (:value nil))
       (:input (:type :standard) (:name "contents") (:value nil))
       (:input (:type :key) (:name "output-mode") (:value ,eval-mode))
       )
      )))



;;;================
;;; CLASS-ARRAY
;;;================

;;; refefines the base method here... (not good)
(defmethod update-arg-names (reference)
  (when (find-class reference nil)
    (cond ((subtypep reference 'class-array)
           '(("numcols" "elts")))
          (t nil))))


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



       
     
 
     
  
