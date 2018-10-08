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


(in-package :om)

;;;=============================
; INPUTS/OUTPUTS OF A BOX
;;;=============================

;; reference can be an actual in/out box (e.g. in an abstraction box) 
;; or just a symbol (never used) corresponding to the name of the input
(defclass OMBoxIO (OMVPObject) 
  ((reference :initform nil :initarg :reference :accessor reference)
   (doc-string :initform nil :initarg :doc-string :accessor doc-string)
   (value :initform nil :initarg :value :accessor value)
   (connections :initform nil :accessor connections :initarg :connections)
   (box :initform nil :initarg :box :accessor box)
   (area :initform nil :initarg :area :accessor area)
   (reactive :initform nil :initarg :reactive :accessor reactive)
   ))

(defmethod copy-io ((self OMBoxIO))
  (let ((new-io (make-instance (type-of self)
                               :value (value self)
                               :reference (reference self)
                               :name (name self)
                               :box (box self)
                               :reactive (reactive self)
                               :doc-string (doc-string self))))
    (setf (connections new-io)
          (mapcar 
           #'(lambda (c) (adopt-connection new-io c))
           (connections self)))
    new-io))
  
  
(defclass box-input (OMBoxIO) ())
(defclass box-output (OMBoxIO) ())

;;; just for display in tooltips etc.
(defmethod io-prefix ((self OMBoxIO)) "")

(defmethod set-value ((self OMBoxIO) value) 
  (setf (value self) value))

;;; the doc-string can be either hard-coded, or collected from the class/method definition
(defmethod get-input-doc-string ((self OMBoxIO))
  (let ((doc (or (doc-string self)
                 (get-input-doc (box self) (name self)))))
    (and (stringp doc) (not (string-equal doc "")) doc)))

(defmethod get-input-doc ((self OMBox) name) nil)
(defmethod get-input-menu ((self OMBox) name) nil)
(defmethod get-input-def-value ((self OMBox) name) nil)
(defmethod get-output-doc ((self OMBox) i) nil) ; (format nil "out~D" i))

;----------------------------------------------------
; BOXES CAN HAVE SPECIAL OPTIONAL / KEYWORD INPUTS
;----------------------------------------------------

(defclass box-optional-input (box-input) ())
(defclass box-keyword-input (box-input) ())

(defmethod keyword-input-p ((self t)) nil)
(defmethod keyword-input-p ((self box-keyword-input)) t)

(defmethod get-optional-inputs ((box OMBox))
  (remove-if-not #'(lambda (item) (subtypep item 'box-optional-input)) (inputs box) :key 'type-of))

(defmethod get-keyword-inputs ((box OMBox))
  (remove-if-not #'(lambda (item) (subtypep item 'box-keyword-input)) (inputs box) :key 'type-of))

;;; used for subclasses such as patch boxes, loop boxes, sequence...
(defmethod do-delete-one-input-extra ((self OMBox)) nil)


(defmethod next-optional-input ((self OMBox)) nil)
(defmethod more-optional-input ((self t) &key name (value nil val-supplied-p) doc reactive) nil)

(defmethod add-optional-input ((self OMBox) &key name (value nil val-supplied-p) doc reactive) 
  (set-box-inputs 
   self 
   (append (inputs self)
           (list (make-instance 'box-optional-input
                                :name (string-downcase name)
                                :value value
                                :box self
                                :doc-string (or doc "optional input")
                                :reactive reactive)))))


(defmethod remove-one-optional-input ((self OMBox))
   (let ((optionals (get-optional-inputs self)))
     (when optionals 
       (let ((last-in (car (last optionals))))
         (mapcar #'(lambda (c) (omng-remove-element (container self) c)) (connections last-in))
         (set-box-inputs self (remove last-in (inputs self) :test 'equal))
         (do-delete-one-input-extra self)
         t))))



(defmethod get-all-keywords ((self t)) nil)

(defmethod next-keyword-input ((self OMBox))
  (let ((keywordlist (apply 'append (get-all-keywords self)))
        (usedkeywords (mapcar #'(lambda (in) (intern-k (name in))) (get-keyword-inputs self))))
    (if keywordlist 
        (or (find-if-not #'(lambda (elt) (member elt usedkeywords)) keywordlist)
            (values nil "All keywords are already used.."))
      (values nil (string+ "No keyword for box '" (name self) "'.")))))

(defmethod io-prefix ((self box-keyword-input)) ":")

;(defmethod more-keyword-input ((self t) &key name (value nil val-supplied-p) doc reactive) nil)

(defmethod def-reactive ((self OMBox) key) nil)

(defmethod more-keyword-input ((self OMBox) &key key (value nil val-supplied-p) doc (reactive nil reactive-supplied-p))
  (multiple-value-bind (def-next err-message) 
      (next-keyword-input self)
    (if def-next ;;; a kyword exist/is available
      (let ((keyname 
             (if key ;;; a specific name is asked for
                 (let ((keywordlist (mapcar 'intern-k (apply 'append (get-all-keywords self))))
                       (usedkeywords (mapcar #'(lambda (in) (intern-k (name in))) (get-keyword-inputs self))))
                   (and (or (find (intern-k key) keywordlist) 
                            (find (intern-k (box-free-keyword-name self)) keywordlist)
                            (om-beep-msg (string+ "Wrong keyword name: " (string key))))
                        (or (not (find (intern-k key) usedkeywords)) 
                            (om-beep-msg (string+ "Keyword name already used: " (string key))))
                        (intern-k key))) ;;; key is ok
               def-next)))
        (when keyname
          (add-keyword-input self :key keyname
                             :value (if val-supplied-p value (get-input-def-value self keyname))
                             :doc (or doc (get-input-doc self (string-downcase keyname)))
                             :reactive (if reactive-supplied-p reactive (def-reactive self keyname)))
          t))
      (om-beep-msg err-message)
      )))

(defmethod add-keyword-input ((self OMBox) &key key (value nil val-supplied-p) doc reactive)
    (set-box-inputs self (append (inputs self)
                                (list (make-instance 'box-keyword-input
                                                     :name (string-downcase key) ;; string-downcase
                                                     :value value
                                                     :box self
                                                     :doc-string (or doc "keyword input")
                                                     :reactive reactive
                                                   )))))

(defmethod remove-one-keyword-input ((self OMBox))
  (let ((keywords (get-keyword-inputs self)))
    (when keywords
      (let ((last-in (car (last keywords))))
        (mapcar #'(lambda (c) 
                    (omng-remove-element (container self) c)) 
                (connections last-in))
        (set-box-inputs self (remove last-in (inputs self) :test 'equal))
       t))))

;;; :++ is a special keyword allowing to set a personalized keyword name
;;; used for instance in class-array
(defmethod box-free-keyword-name ((self t)) :++)

(defmethod change-keyword ((input box-keyword-input) key)
  (let ((old-name (name input))
        (new-key (if (equal key (box-free-keyword-name (box input))) 
                    (let ((new-name (om-get-user-string "type a new name" :initial-string (name input))))
                      (and new-name (intern-k new-name)))
                   key)))
    (when new-key
      (setf (name input) (string-downcase new-key)
            (value input) (get-input-def-value (box input) new-key)
            (doc-string input) (get-input-doc (box input) (string-downcase new-key))
            (reactive input) (def-reactive (box input) new-key))
      (update-output-from-new-in (box input) old-name input)
    )))

(defmethod update-output-from-new-in (box name in) nil)

(defmethod smart-copy-additional-inputs ((self OMBox) newbox)
  
  ;;; if boxes have common inputs (in principle, they do!) => copy the values
  (loop for in in (inputs self) do
        (let ((newin (find (name in) (inputs newbox) :key 'name :test 'string-equal)))
          (when newin 
            (setf (value newin) (om-copy (value in)))
            (setf (reactive newin) (reactive in)))
          ))
  
  (loop for out in (outputs self) do
        (let ((newout (find (name out) (outputs newbox) :key 'name :test 'string-equal)))
          (when newout 
            (setf (reactive newout) (reactive out)))
          ))

  ;;; add relevant optional and keyword inputs
  (mapcar 
     #'(lambda (in) 
         (more-optional-input newbox :name (name in) :value (value in) :doc (doc-string in) :reactive (reactive in)))
     (get-optional-inputs self))
  (mapcar 
   #'(lambda (in) 
       (more-keyword-input newbox :key (intern-k (name in)) :value (value in) :doc (doc-string in) :reactive (reactive in)))
   (get-keyword-inputs self)))

(defmethod om-copy ((self OMBox)) 
  (let ((newbox (call-next-method)))
    ;;; add the optional/keywords + copy input values
    (smart-copy-additional-inputs self newbox) 
    newbox))


(defmethod optional-input++ ((self OMBox))
  (more-optional-input self)
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))))

(defmethod optional-input-- ((self OMBox))
  (remove-one-optional-input self)
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))
    (om-invalidate-view (om-view-container (frame self))) ;; in case there were connections...
    ))

(defmethod keyword-input++ ((self OMBox))
  (more-keyword-input self)
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))))

(defmethod keyword-input-- ((self OMBox))
  (remove-one-keyword-input self)
  (when (frame self) 
    (set-frame-areas (frame self))
    (om-invalidate-view (om-view-container (frame self))) ;; in case there were connections...
    ))

;-------------------------------------------
; SPECIAL OUTPUTS CORRESPONDING TO KEYWORD/OPTIONAL INPUTS
;-------------------------------------------

(defclass box-keyword-output (box-output) ())
(defclass box-optional-output (box-output) ())

(defun get-keyword-outputs (box)
  (remove-if-not #'(lambda (item) (subtypep item 'box-keyword-output)) (outputs box) :key 'type-of))


