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

;;;==============================================
;;; A superclass for 'playable' synthesis objects
;;;==============================================

(defclass SynthesisEvt (schedulable-object) ())

(defun filter-events (evt-list fun-s) 
  (loop for item in evt-list
        when (let ((rep t))
               (loop for fun in (list! fun-s)
                     while rep do
                     (setf rep (funcall fun ev)))
               rep)
        collect item))

(defmethod synthesize-method ((self t)) nil)

(defun collect-events-by-synth (evt-list)
  (let ((rep nil))
    (loop for evt in evt-list do
          (let ((synthesis-fun (synthesize-method evt)))
            (if synthesis-fun
                (let ((thislist (find synthesis-fun rep :key 'car)))
                  (if thislist (setf (cadr thislist) (append (cadr thislist) (list evt)))
                    (setf rep (append rep (list (list synthesis-fun (list evt)))))))
              (om-beep-msg "No synthesis method for events of type ~A" (type-of evt))))
          )
    rep))
    

;;;==============================================
;;; A generic synthesizer function 
;;; (calls appropriate specific method for the type of input)
;;;==============================================


(defmethod* synthesize ((self synthesisevt) &key (name "my-synth") (run t) (format "aiff") filters inits tables sr kr)

  :indoc '("a synthesis even (or list)" "name of output file" "run synthesis or generate params?" 
           "audio output format" "filter function for synthesis events" "synth initializers" "wave/gen tables" "sample rate" "control rate")
  :initvals '(nil "my-synth" t "aiff")

  (when (synthesize-method self)
    (if (or (null filters)
            (filter-events (list self) filters))
        (funcall (synthesize-method self) 
                 self
                 :name name :run run :format format
                 :inits inits :tables tables 
                 :sr sr :kr kr)
      (om-beep-msg "SYNTHESIZE: event did not pass filter(s) !"))
    ))

(defmethod* synthesize ((self list) &key (name "my-synth") (run t) (format "aiff") filters inits tables sr kr)
  
  (let* ((evt-list (if filters (filter-events self filters) self))
         (grouped-list (collect-events-by-synth evt-list)))
    (when grouped-list
      
      (cond 
       
       ((= 1 (length grouped-list)) ;; only one kind of synthesis (most frequent case...)
        
        (funcall (car (car grouped-list)) 
                 (cadr (car grouped-list))
                 :run run :name name :format format
                 :inits inits :tables tables
                 :sr sr :kr kr))
            
       (t (let ((rep-list (loop for elt in grouped-list  ;; several synthesis processes to mix
                                for i = 1 then (+ i 1) collect
                                (funcall (car elt) 
                                         (cadr elt)
                                         :run run 
                                         :name (string+ name "-temp-" (integer-to-string i))
                                         :format format
                                         :inits inits :tables tables
                                         :sr sr :kr kr))))
            (if run
                ;;; mix all results
                (save-sound 
                 (reduce 'sound-mix rep) 
                 (if (pathnamep name) name (outfile name :type format)))
              
                rep-list)))
       )
      )))


