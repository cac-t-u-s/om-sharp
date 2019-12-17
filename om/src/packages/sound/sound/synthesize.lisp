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
                     (setf rep (funcall fun item)))
               rep)
        collect item))

;;; the function pointed by synthesize method must accept the following keyword arguments
;;; (even if it ignores them) :
;;; :name :run :format :inits :tables :filters :resolution :normalize :sr :kr

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


(defmethod* synthesize ((obj synthesisevt) 
                        &key (name "my-synth") (run t) 
                        format resolution 
                        (normalize nil normalize-supplied-p) 
                        inits tables filters
                        sr kr)

  :indoc '("a synthesis even (or list)" "name of output file" "run synthesis or generate params?" 
           "audio output format" "filter function for synthesis events" "synth initializers" "wave/gen tables" "sample rate" "control rate")
  :initvals '(nil "my-synth" t "aiff")
  
  (when (synthesize-method obj)
    (if (or (null filters)
            (filter-events (list obj) filters))
        (funcall (synthesize-method obj) 
                 obj
                 :name name :run run 
                 :format (or format (get-pref-value :audio :format))
                 :resolution resolution
                 :normalize (if normalize-supplied-p normalize (get-pref-value :audio :normalize))
                 :inits inits :tables tables
                 :sr sr :kr kr)
      (om-beep-msg "SYNTHESIZE: event did not pass filter(s) !"))
    ))

(defmethod* synthesize ((obj list)                      
                        &key (name "my-synth") (run t) 
                        format resolution 
                        (normalize nil normalize-supplied-p)
                        inits tables filters
                        sr kr)
  
  (let* ((evt-list (if filters (filter-events obj filters) obj))
         (grouped-list (collect-events-by-synth evt-list))
         (out-normalize (if normalize-supplied-p normalize (get-pref-value :audio :normalize))))
    
    (when grouped-list
      
      (cond 
       
       ((= 1 (length grouped-list)) ;; only one kind of synthesis (most frequent case...)
        
        (funcall (car (car grouped-list)) 
                 (cadr (car grouped-list))
                 :name name :run run :format format
                 :inits inits :tables tables
                 :resolution resolution 
                 :normalize out-normalize 
                 :sr sr :kr kr))
            
       (t (let ((rep-list (loop for elt in grouped-list  ;; several synthesis processes to mix
                                for i = 1 then (+ i 1) collect
                                (funcall (car elt) 
                                         (cadr elt)
                                         :name (string+ name "-temp-" (number-to-string i)) :run run :format format
                 :inits inits :tables tables
                 :resolution resolution 
                 :normalize out-normalize
                 :sr sr :kr kr))))
            (if run
                ;;; mix all results
                (save-sound 
                 (reduce 'sound-mix rep-list) 
                 (if (pathnamep name) name (outfile name :type format)))
              
                rep-list)))
       )
      )))


