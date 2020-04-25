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


;;; compat API:
(defmethod update-arg-names ((reference (eql 'chord-seq)))
  '(("legato" "llegato")))

;;; Redefinitions of OM6 load utilities
(defun load-obj-list-from-save (list)
  (loop for item in list collect (eval item)))

;;; SCORE OBJECTS
;;; => TODO
(defmethod set-patch-pairs ((self t) list) )
(defmethod load-port-info ((self t) port) )
(defmethod init-mus-color ((self t) color) )
(defmethod set-extra-pairs ((self t) extras) )
(defmethod set-tonalite ((self t) tonalite) )
(defmethod set-object-analysis ((self t) analyse) )

;;; SCORE EDITOR PARAMS

(defun load-score-edition-params (editparams)
  (let ((staff-symb (cdr (find 'staff editparams :key #'car))))
    (when staff-symb
      `((:edition-params (:staff ,(intern-k (symbol-name staff-symb)))))
      )))


(defmethod om-load-editor-box1 (name (reference (eql 'note)) inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore fname meditor pictlist))
  (append (call-next-method)
          (load-score-edition-params editparams)))

(defmethod om-load-editor-box1 (name (reference (eql 'chord)) inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore fname meditor pictlist))
  (append (call-next-method)
          (load-score-edition-params editparams)))

(defmethod om-load-editor-box1 (name (reference (eql 'chord-seq)) inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore fname meditor pictlist))
  (append (call-next-method)
          (load-score-edition-params editparams)))

(defmethod om-load-editor-box1 (name (reference (eql 'voice)) inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore fname meditor pictlist))
  (append (call-next-method)
          (load-score-edition-params editparams)))


;;; => hack / to(re)do when score editors are operational
(defclass edition-values () 
  ((paper-size :accessor paper-size)
   (top-margin :accessor top-margin)
   (left-margin :accessor left-margin)
   (right-margin :accessor right-margin)
   (bottom-margin :accessor bottom-margin)
   (orientation :accessor orientation)
   (scale :accessor scale)
   (system-space :accessor system-space)
   (system-color :accessor system-color)
   (line-space :accessor line-space)
   (title :accessor title)
   (show-title? :accessor show-title?)
   (show-page? :accessor show-page?)
   (sheet-id :accessor sheet-id)
   (page-mode :accessor page-mode)))
   

;;; SHEET
(defclass sheet-track-obj () 
  ((end-t :accessor end-t)
   (obj-size :accessor obj-size)
   (obj-margin :accessor obj-margin)
   (obj-staff :accessor obj-staff)  
   ))

