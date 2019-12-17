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
; File authors: J. Bresson
;============================================================================

(in-package :om)


;;; load the Maquette (file)
(defmacro om-load-maq2 (name boxes connections range markers &rest ignore)
  
  (declare (ignore range markers ignore))
  
  `(let ((name-loaded ,name)
         (boxes-loaded ,boxes)
         (connections-loaded ,connections))
     
     ;;; only top-level patch acually load their contents:
     (omng-load
      `(:maquette
        (:name ,name-loaded)
        (:boxes .,(remove 'temp-marker 
                          (loop for box-code in boxes-loaded collect (eval box-code))
                          :key #'type-of))
        (:connections .,(loop for c in connections-loaded collect (format-imported-connection c)))
        )
      )
     ))


(defclass temp-marker () ())
(defun (setf doc) (doc self) nil)


;;; load the Maquette (internal)
(defun om-load-maq-abs1 (name boxes connections range markers &rest ignore)
  (declare (ignore range markers ignore))
  `(:maquette
    (:name ,name) 
    (:boxes .,(loop for box-code in boxes collect (eval box-code)))
    (:connections .,(loop for c in connections collect (format-imported-connection c)))
    )
  )

; (:input (:type :standard) (:name "time") (:value 1000)) 
(defun convert-maq-input (input) 
  `(:input (:type :optional) ,(cddr input)))

;;; OMBoxMaquette (internal)
(defmethod om-load-boxcall ((self (eql 'maqabs)) name reference inputs position size value lock &rest rest)

  (declare (ignore value rest))
  
  `(:box 
    (:type :abstraction)
    (:reference ,reference)   ;; contains the actual contents
    (:name ,name)
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:h ,(if size (om-point-y size) 48))
    (:lock ,(if lock (cond ((string-equal lock "x") :locked) 
                           ((string-equal lock "&") :eval-once))))
    (:lambda ,(if lock (cond ((string-equal lock "l") :lambda) 
                             ((string-equal lock "o") :reference))))
    (:inputs .,(mapcar #'(lambda (i) (convert-maq-input (eval i))) inputs))
    (:display :mini-view)
    )
  )


;;; OMBoxMaquette (external)
(defmethod om-load-boxcall ((self (eql 'maquette)) name reference inputs position size value lock &rest rest)
  
  (declare (ignore value rest))
  
  (let ((loaded-reference (load-om6-patch-from-relative-path reference)))
    
    (om-load-boxcall 'abstraction name loaded-reference inputs position size value lock)
    
    ))



;;; TEMPOUT in patches = normal out
(defun om-load-tempboxout (name position inputs &optional fname fsize)
  (om-load-boxout name 0 position inputs fname fsize))


;;; => A BOX IN THE MAQUETTE
(defun om-load-tempobj1 (name inputs refer numouts posx sizex clorf value ignorepict 
                              sizey posy strechfact 
                              &optional (store nil) (params nil) (lock nil) pict (showpict nil) (mute nil) (pos-locked nil)
                              (showname nil) (doc "") &rest rest)
 
  (declare (ignore numouts ignorepict strechfact store params  pict showpict mute pos-locked doc rest))

  (multiple-value-bind (ref-type ref-val)
 
      (case (first refer)
        ;;; external abstractions
        (patchb (values 'patch-box (cadr refer)))
        (maq (values 'maquette (cadr refer)))
        ;;; internal abstractions
        (patch (values 'abstraction (cadr refer)))
        ;;; a bug (?) in OM6: the value is quotes only fr internalm maquettes... => eval
        (absmaq (values 'abstraction (eval (cadr refer))))  
        (yourobj (values NIL (cadr refer))))
    
    (if ref-type  ;;; => abstraction box
        
        (print (append 
         (om-load-boxcall ref-type name ref-val 
                          inputs 
                          (om-make-point posx posy)
                          (om-make-point sizex sizey)
                          value lock)
         `((:color ,(omng-save clorf))
           (:show-name ,showname)
           (:group-id ,(om-random 1 4))))
         )

      (append 
       (om-load-boxinstance name ref-val 
                            inputs 
                            (om-make-point posx posy))
       `((:w ,sizex)
         (:h ,sizey)
         (:color ,(omng-save clorf))
         (:group-id ,(om-random 1 4))
         ))
      
      )
    ))


;;; META IN/OUTS: need some conversion
(defun om-load-boxselfin (name position  &optional fsize) 
  (declare (ignore fsize name))
  (om-print "Warning: 'self' input box converted to 'MYBOX': consider reconnect output using an 'OMBOX' SLOTS box."  
            "Import/Compatibility")
  `(:box
    (:type :io)
    (:reference (:in (:type omselfin) (:index 0) (:name "BOX")))
    (:name "BOX")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:outputs (:output (:name "out")))
    ))


(defun om-load-boxmaqselfin (name position  &optional fsize) 
  (declare (ignore name fsize))
  (om-print "Warning: 'self-maquette' input box converted to 'MYMAQUETTE': consider reconect outputs using GET-OBJ-DUR / GET-ALL-BOXES / M-OBJECTS."  
            "Import/Compatibility")
  `(:box
    (:type :io)
    (:reference (:in (:type ommaqin) (:index 0) (:name "MAQUETTE")))
    (:name "MAQUETTE")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:outputs (:output (:name "out")))
    )
  )


;;; IN/OUT in the maquette: no more supported
(defun om-load-maq-boxin (name indice position docu &optional fname val fsize) 
  (declare (ignore name indice position docu fname val fsize))
  (om-print "Warning: Inputs no more supported in the maquette. Use control-patch." "Import/Compatibility")
  NIL)

(defun om-load-maq-boxout (name indice position inputs &optional fname fsize) 
  (declare (ignore name indice position inputs fname fsize))
  (om-print "Warning: Outputs no more supported in the maquette. Use control-patch." "Import/Compatibility")
  NIL)




;======================================
; old forms not supported: 
;======================================
; old-old: not exported by OM6
; (defun om-load-maq1 (name boxes connections range markers &rest ignore) )
; (defun om-load-temp-patch (name boxes connections &optional version) )
; (defun om-load-temp-patch1 (name boxes connections &optional version pictlist) )


