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
; File authors: J. Bresson
;============================================================================

(in-package :om)


;;; load the Maquette (file)
(defmacro  om-load-maq2 (name boxes connections range markers &rest ignore)
  
  (declare (ignore ignore))

  (when boxes 
    (om-beep-msg "WARNING: Maquette compatibility is not implemented yet!"))
 
  `(let ((name-loaded ,name)
         (boxes-loaded ,boxes)
         (connections-loaded ,connections))
     
     ;;; only top-level patch acually load their contents:
     (omng-load
      `(:maquette
        (:name ,name-loaded)
        (:boxes .,(loop for box-code in boxes-loaded collect (eval box-code)))
        (:connections .,(loop for c in connections-loaded collect (format-imported-connection c)))
        )
      )
     ))


;;; load the Maquette (internal)
(defun om-load-maq-abs1 (name boxes connections range markers &rest ignore)
  (declare (ignore ignore))
  (when boxes 
    (om-beep-msg "WARNING: Maquette compatibility is not implemented yet!"))  
  `(:maquette
    (:name ,name) 
    (:boxes .,(loop for box-code in boxes collect (eval box-code)))
    (:connections .,(loop for c in connections collect (format-imported-connection c)))
    )
  )



;;; OMBoxMaquette
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
    (:inputs .,(mapcar #'eval inputs))
    (:display :mini-view)
    )
  )


;;; TEMPOUT in patches = normal out
(defun om-load-tempboxout (name position inputs &optional fname fsize)
  (let ((newbox (make-new-temp-output  name (om-correct-point position))))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))


;;; => A BOX IN THE MAQUETTE
(defun om-load-tempobj1 (name inputs refer numouts posx sizex clorf value ignorepict 
                              sizey posy strechfact 
                              &optional (store nil) (params nil) (lock nil) pict (showpict nil) (mute nil) (pos-locked nil)
                              (showname nil) (doc "") &rest rest)
  nil)
  

;;; IN/OUT in the maquette: no more supported
(defun om-load-maq-boxin (name indice position docu &optional fname val fsize) NIL)
(defun om-load-maq-boxout (name indice position inputs &optional fname fsize) NIL)


;;; META IN/OUTS: need some conversion work
(defun om-load-boxmaqselfin (name position  &optional fsize) )
(defun om-load-boxselfin (name position  &optional fsize) )


#|
;;; OM6 code:
(defun om-load-tempobj1 (name inputs refer numouts posx sizex clorf value ignorepict 
                             sizey posy strechfact 
                             &optional (store nil) (params nil) (lock nil) pict (showpict nil) (mute nil) (pos-locked nil)
                             (showname nil) (doc "") &rest rest)
  (let (reference newtempob maqpos)
    (cond
     ((equal (first refer) 'maq) 
      (setf reference (mk-object-refer 'maquette (second refer))))
     ((equal (first refer) 'patchb) 
      (setf reference (mk-object-refer 'patch-box (second refer))))
     (t (setf reference (eval (second refer)))))
    (when reference
      (setf maqpos (om-make-big-point posx posy))
      (setf newtempob (omNG-make-tempobj reference maqpos name))   
      (setf (numouts newtempob) numouts)
      (setf (inputs newtempob) (mapcar #'(lambda (input) (eval input)) inputs))
      (set-box-to-inputs (inputs newtempob) newtempob)
      (setf (extend newtempob) sizex)
      (setf (colorframe newtempob) (om-correct-color clorf))
      (setf (free-store newtempob) store)
      (when value
        (setf (value newtempob) (list! value))
        (if (maquette-p reference) (setf (value reference) (car (value newtempob)))))
      (setf (slot-value newtempob 'strech-fact)  (or strechfact 1))
      (setf (colorframe newtempob) (om-correct-color clorf))
      (setf (slot-value newtempob 'sizey) sizey)
      (setf (allow-lock newtempob) lock)
      (setf (lock newtempob) pos-locked)
      (setf (mute newtempob) mute)
      (setf (showpict newtempob) showpict)
      (setf (name newtempob) name)
      (setf (doc newtempob) (str-with-nl doc))
      (setf (show-name newtempob) showname)
      (when pict (setf (pictu newtempob) pict))   ;;; .
      (setf (edition-params newtempob) (corrige-edition-params (car (value newtempob)) params))
      newtempob)))
|#


;======================================
; old forms not supported: 
;======================================
; old-old: not exported by OM6
; (defun om-load-maq1 (name boxes connections range markers &rest ignore) )
; (defun om-load-temp-patch (name boxes connections &optional version) )
; (defun om-load-temp-patch1 (name boxes connections &optional version pictlist) )


