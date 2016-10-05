(in-package :om)

(defclass object-in-box ()
  ((associated-box :initform nil :accessor associated-box)))
  
(defmethod (setf value) :after ((value object-in-box) (self OMBoxEditCall)) 
  (setf (associated-box value) self))

(defmethod notify-change ((self object-in-box))
  (when (associated-box self)
    (update-if-editor (associated-box self))))


; REDEF
 (defclass named-object (object-in-box) ((name :initform nil :initarg :name :accessor name)))
; !!! ne pas oublier de re-evaluer les fichiers chord-seq, bppf etc. ensuite !!!
