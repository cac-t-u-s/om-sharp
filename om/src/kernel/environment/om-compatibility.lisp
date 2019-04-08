;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2019 J. Bresson et al., IRCAM.
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

(defvar *om-current-persistent*)

(defun load-om6-patch (path)
  
  (om-print-format "Opening document : ~A" (list path) "Compatibility")

  (clos::set-clos-initarg-checking nil)
  
  (unwind-protect 
  
  (with-relative-ref-path path
    
    (with-open-file (in path :direction :input :if-does-not-exist nil)
      
      (unless (equal (read-line in nil :eof) :eof) ;; not empty
        
        ;; line #2 contains meta-data about the patch
        (let ((metadata (read-from-string (subseq (om-read-line in) 1))))
      
          (if metadata 
              (om-print-format "Converting from OM ~D (type '~A')" (list (car metadata) (cadr metadata)) "Compatibility")
            (om-print-format "No header found in document..." nil "Compatibility"))
          
          (setq *om-current-persistent* (make-new-om-doc 
                                         (if (equal (cadr metadata) :maqt) :maquette :patch) 
                                         (pathname-name path)))
          
          (handler-case  
              (load path)
            (error (err) 
              (om-print err "Compatibility")
              ;(abort)
              (error err)
              ))

            (let ((object (om-copy *om-current-persistent*)))
              
            (setf (omversion object) *om-version*
                  (create-info object) (list (om-get-date) (om-get-date))
                  (saved? object) nil)
            
            (register-document object)
            
            object)
            )
          )))
    

    (clos::set-clos-initarg-checking t)
    ))



;============================================================================
; CONVERT ALL OM6 FUNCTIONS TO OM7 CALLS...
;============================================================================

;============================================================================
; REQUIRED LIBS
;============================================================================
;;; called to lod libs prior to the patch
(defun load-lib-for (list)
  (loop for libname in list do (require-library libname)))


;============================================================================
; PATCH (MAIN SECTION)
;============================================================================
;;; main patch file
(defun om-load-patch1 (name boxes connections &rest ignore)
  (declare (ignore ignore))
  (let ((patch (make-instance 'OMPatchFile :name name)))
  
    (loop for box-code in boxes do
          (let ((box (eval box-code)))
            (when box 
              (omng-add-element patch box))))
          
    patch))
    
#|
(omng-load
   `(:patch
     (:boxes ,.(loop for box-code in boxes collect (eval box-code)))
     (:connections nil))
   )
|#

;;; internal sub-patch
(defun om-load-patch-abs1 (name boxes connections &rest ignore)
  (declare (ignore ignore))
  
  (let ((patch (make-instance 'OMPatchInternal :name name)))
  
    (loop for box-code in boxes do
          (let ((box (eval box-code)))
            (when box 
              (omng-add-element patch box))))
          
    patch))




;============================================================================
; BOXES
;============================================================================


(defmethod om-load-boxcall ((class t) name reference inputs position size value lock &rest rest) 
  (omng-load `(:box 
               (:reference ,reference) 
               (:x ,(om-point-x position))
               (:y ,(om-point-y position)))
             ))
               

(defmethod om-load-boxcall ((self (eql 'abstraction)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'maqabs)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'editor)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'slot)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'lispfun)) name reference inputs position size value lock &rest rest) )



;;; Object boxes
(defun om-load-editor-box1 (name reference inputs position size value lock &rest rest)
  (omng-load `(:box 
               (:type :object)
               (:reference ,reference) 
               (:name ,name)
               (:x ,(om-point-x position))
               (:y ,(om-point-y position)))
             ))

;;; This was used to save OMLoops
;;; In om7 OMloop is just an normal patch
(defmethod om-load-boxwithed1 ((class t) name reference inputs position size value lock boxes conec numouts) 
  (declare (ignore numouts))
  (let ((patch (om-load-patch-abs1 name boxes conec)))
    (om-load-boxcall 'abstraction name reference inputs position size patch lock)))


;;; Value boxes
(defmethod om-load-boxcall ((self (eql 'bastype)) name reference inputs position size value lock &rest rest)
  (omng-load `(:box 
               (:type :value)
               (:reference ,reference) 
               (:name ,name)
               (:x ,(om-point-x position))
               (:y ,(om-point-y position)))
             ))


(defun om-load-boxin (name indice position docu &optional fname val fsize) )
(defun om-load-boxout (name indice position inputs &optional fname fsize) )

(defun om-load-boxcomment (name size reference value position fname color style) )

(defmethod om-load-seqbox (name reference inputs position sizeload value lock numouts) )

(defun om-load-boxinstance (name instance inputs position &optional fname size) )
(defun om-load-ominstance1 (class name icon instance edparams &optional pictlist doc &rest rest) )


;;; a box input
(defun om-load-inputfun (class doc name value) ) 
;;; a box input "keyword" 
(defun om-load-inputkeyword (class doc name value defval menu) )
;;; a box input with menu for different values
(defun om-load-inputfunmenu1 (class doc name value items) )


(defun om-load-boxtypein (name type indice position docu keys defval &optional fname fsize) )
(defun om-load-initin  (name type indice posi self? class &optional fsize) )

;============================================================================
; MAQUETTE
;============================================================================

(defun om-load-maq1 (name boxes connections range markers &rest ignore) )
(defun om-load-maq2 (name boxes connections range markers &rest ignore) )

(defun om-load-maq-abs1 (name boxes connections range markers) )

(defun om-load-maq-boxin (name indice position docu &optional fname val fsize) )
(defun om-load-maq-boxout (name indice position inputs &optional fname fsize) )
(defun om-load-boxmaqselfin (name  position  &optional fsize) )

(defun om-load-temp-patch (name boxes connections &optional (version nil)) )
(defun om-load-temp-patch1 (name boxes connections &optional (version nil) pictlist) )

(defun om-load-tempobj1 (name inputs refer numouts posx sizex clorf value ignorepict) )

(defun om-load-boxselfin (name  position  &optional fsize) )
(defun om-load-tempboxout (name position inputs &optional fname fsize) )

;============================================================================
; LISP-PATCH
;============================================================================

(defun om-load-lisp-patch (name version expression) )
(defun om-load-lisp-abspatch (name version expression) )

;============================================================================
; DATA
;============================================================================

(defun load-obj-list-from-save (list)
  (loop for item in list collect (eval item)))

;; TODO
(defmethod set-patch-pairs ((self t) list) )
(defmethod load-port-info ((self t) port) )
(defmethod init-mus-color ((self t) color) )
(defmethod set-extra-pairs ((self t) extras) )
(defmethod set-tonalite ((self t) tonalite) )

;; dirty hacks
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
   
;;; BPF
(defun simple-bpf-from-list (x-points y-points &optional (class 'bpf) (decimals 0))
  (make-instance class :x-points x-points :y-points y-points :decimals decimals))
 


