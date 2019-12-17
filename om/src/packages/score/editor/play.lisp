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

;;;===================================
;;; PLAY (MIDI)
;;; Specific actions for score-objects playback 
;;;===================================

;;; chord-seq/voice already play (from data-stream)

(defmethod play-obj? ((self multi-seq)) t)
(defmethod play-obj? ((self chord)) t)
(defmethod play-obj? ((self note)) t)

;;;===================================================
;;; PLAY-ACTIONS
;;;===================================================

;;; from a chord-editor (or a maquette...)
;;; !! negative offsets shift the chord
(defmethod get-action-list-for-play ((c chord) interval &optional parent)

  (let ((chan-shift (and (get-pref-value :score :auto-bend)
                         (micro-channel-on (pitch-approx c)))))
    
    (let ((negative-offset (max 0 (- (loop for n in (notes c) minimize (offset n))))))
      
      (remove 
       nil 
       (loop for n in (notes c) append
             (let ((channel (+ (or (chan n) 1) 
                               (if chan-shift (micro-channel (midic n) (pitch-approx c)) 0))))
                           
               (list 
                (when (in-interval (+ (offset n) negative-offset) interval :exclude-high-bound t)
                       

                  (list (+ (offset n) negative-offset)
                        #'(lambda (note) (om-midi::midi-send-evt 
                                          (om-midi:make-midi-evt 
                                           :type :keyOn
                                           :chan channel :port (or (port note) (get-pref-value :midi :out-port))
                                           :fields (list (truncate (midic note) 100) (vel note)))))
                        (list n)))
      
                (when (in-interval (+ (offset n) (dur n) negative-offset) interval :exclude-high-bound t) 
     
   
                  (list (+ (offset n) (dur n) negative-offset)
                        #'(lambda (note) (om-midi::midi-send-evt 
                                          (om-midi:make-midi-evt 
                                           :type :keyOff
                                           :chan channel :port (or (port note) (get-pref-value :midi :out-port))
                                           :fields (list (truncate (midic note) 100) 0))))
                        (list n)))
                ))
             ))
      )))

 
(defmethod get-action-list-for-play ((n note) interval &optional parent)
  (let ((channel (+ (or (chan n) 1)
                    (if (and (get-pref-value :score :auto-bend)
                             (micro-channel-on (pitch-approx n)))
                        (micro-channel (midic n) (pitch-approx n))
                      0))))
    (remove 
     nil
     (list 
      (when (in-interval 0 interval :exclude-high-bound t) 
        (list (offset n)
              #'(lambda (note) (om-midi::midi-send-evt 
                                (om-midi:make-midi-evt 
                                 :type :keyOn
                                 :chan channel :port (or (port note) (get-pref-value :midi :out-port))
                                 :fields (list (truncate (midic note) 100) (vel note)))))
              (list n)))
      
      (when (in-interval (dur n) interval :exclude-high-bound t) 
     
   
        (list (+ (offset n) (dur n))
              #'(lambda (note) (om-midi::midi-send-evt 
                                (om-midi:make-midi-evt 
                                 :type :keyOff
                                 :chan channel :port (or (port note) (get-pref-value :midi :out-port))
                                 :fields (list (truncate (midic note) 100) 0))))
              (list n)))
      ))))



(defmethod get-action-list-for-play ((object chord-seq) interval &optional parent)
  (let ((chan-shift (and (get-pref-value :score :auto-bend)
                         (micro-channel-on (pitch-approx object)))))

    (sort 
     (loop for c in (remove-if #'(lambda (chord) 
                                   (let ((t1 (+ (date chord) (list-min (loffset chord))))
                                         (t2 (+ (date chord) (loop for n in (notes chord) maximize (+ (offset n) (dur n))))))
                                     (or (< t2 (car interval))
                                         (> t1 (cadr interval)))
                                     ))
                               (chords object))
    
           append 
           (loop for n in (notes c) append
                 (let ((channel (+ (or (chan n) 1) 
                                   (if chan-shift (micro-channel (midic n) (pitch-approx object)) 0))))
                   (remove nil 
                           (list 
                            (if (or (in-interval (+ (date c) (offset n)) interval :exclude-high-bound t) 
                                    ;; (minusp (offset n)) ;;; pre-schedule it ?
                                    )

                                (list (+ (date c) (offset n))
                                        
                                      #'(lambda (note) (om-midi::midi-send-evt 
                                                        (om-midi:make-midi-evt 
                                                         :type :keyOn
                                                         :chan channel :port (or (port note) (get-pref-value :midi :out-port))
                                                         :fields (list (truncate (midic note) 100) (vel note)))))
                                      (list n)))

                            (if (in-interval (+ (date c) (offset n) (dur n)) interval :exclude-high-bound t)
                                
                                (list (+ (date c) (offset n) (dur n))
                                      
                                      #'(lambda (note) (om-midi::midi-send-evt 
                                                        (om-midi:make-midi-evt 
                                                         :type :keyOff
                                                         :chan channel :port (or (port note) (get-pref-value :midi :out-port))
                                                         :fields (list (truncate (midic note) 100) 0))))
                                      (list n)))
                      
                            ))))
           )
     '< :key 'car)
    ))



(defmethod get-action-list-for-play ((object multi-seq) interval &optional parent)
  
  (loop for voice in (obj-list object)
        append (get-action-list-for-play voice interval parent))
  
  )



;;;===================================================
;;; MICROTONES
;;;===================================================

(add-preference :score :auto-bend "Auto microtone bend" :bool t 
                '("Applies 1/8th pitch-bend to MIDI channels 1-4 during playback of score-objects"
                  "and shift MIDI channels of micro-tonal note when scale is 1/4 or 1/8th tone."))

;;; split note on channels in case of microtonal setup (4 or 8)
;;; tone = 0, 1/8 = 1, 1/4 = 2, 3/8 = 3
;;; default bend channel 1 = 0, channel 2 = 25 mc, channel 3 = 50 mc, channel 4 = 75mc

(defparameter *micro-channel-approx* 8)

(defun micro-bend-messages (&optional port)
  (loop for chan from 1 to (/ *micro-channel-approx* 2)
        for pb from 8192 by (/ 8192 *micro-channel-approx*) ;;; 8192 = 1 tone pitch-bend
        collect (om-midi::make-midi-evt
                 :type :PitchBend
                 :date 0
                 :chan chan
                 :port (or port (get-pref-value :midi :out-port))
                 :fields (val2lsbmsb pb))))


(defun micro-reset-messages (&optional port)
  (loop for chan from 1 to 4
        collect (om-midi::make-midi-evt
                 :type :PitchBend
                 :date 0
                 :chan chan
                 :port (or port (get-pref-value :midi :out-port))
                 :fields (val2lsbmsb 8192))))

(defun micro-bend (&optional port)
  (loop for pb-evt in (micro-bend-messages port)
        do (om-midi:midi-send-evt pb-evt)))

(defun micro-reset (&optional port)
  (loop for pb-evt in (micro-reset-messages port)
        do (om-midi:midi-send-evt pb-evt)))



;; t / nil / list of approx where it must be applied
(defparameter *micro-channel-mode-on* '(4 8))

(defun micro-channel-on (approx)
  (and 
   approx
   (if (consp *micro-channel-mode-on*) 
       (find approx *micro-channel-mode-on* :test '=)
     *micro-channel-mode-on*)))

; channel offset from midic 
(defun micro-channel (midic &optional approx)
  (if (micro-channel-on approx)
      (let ((scale-mod (/ 200 approx))  ;;; *-mod = 25 or 50
            (channels-mod (/ 200 *micro-channel-approx*)))
        (round (* (floor (mod midic 100) scale-mod) scale-mod) channels-mod))
    0))

(defmethod collec-ports-from-object ((self t))
  (remove-duplicates (mapcar #'port (get-notes self))))

;;; chord-seq and multi-seq don't work with get-notes
(defmethod collec-ports-from-object ((self chord-seq))
  (remove-duplicates 
   (loop for c in (inside self) append (collec-ports-from-object c))))

(defmethod collec-ports-from-object ((self multi-seq))
  (remove-duplicates 
   (loop for c in (inside self) append (collec-ports-from-object c))))


;;; HOOK ON PLAYER METHODS:

(defmethod player-play-object ((self scheduler) (object score-element) (caller score-editor) &key parent interval)
  
  (declare (ignore parent interval))

  (let ((approx (/ 200 (step-from-scale (editor-get-edit-param caller :scale)))))
   (setf (pitch-approx object) approx)
   (when (and (get-pref-value :score :auto-bend)
              (micro-channel-on approx))
     (loop for p in (collec-ports-from-object object) do (micro-bend p))))
  
  (call-next-method))


(defmethod player-play-object ((self scheduler) (object score-element) (caller ScoreBoxEditCall) &key parent interval)
  
  (declare (ignore parent interval))
  
  (let ((approx (/ 200 (step-from-scale (get-edit-param caller :scale)))))
    (setf (pitch-approx object) approx)
    (when (and (get-pref-value :score :auto-bend)
               (micro-channel-on approx))
      (loop for p in (collec-ports-from-object object) do (micro-bend p))
      ))
  
  (call-next-method))

    
(defmethod player-play-object :before ((self scheduler) (object ommaquette) caller &key parent interval)
  ;;;Ajouter ici la task begin : (mp:mailbox-send (taskqueue *engine*) *taskbegin*)
  (declare (ignore parent interval))
  
  (let ((micro-play-ports nil))
    
    (loop for box in (get-boxes-of-type object 'ScoreBoxEditCall)
          do 
          (let ((approx (/ 200 (step-from-scale (get-edit-param box :scale))))
                (object (car (value box))))
             
            (setf (pitch-approx object) approx)
            
            (when (and (get-pref-value :score :auto-bend) (micro-channel-on approx))
               (setf micro-play-ports
                     (append micro-play-ports (collec-ports-from-object object))))))
    
    (loop for p in (remove-duplicates micro-play-ports)
          do (micro-bend p))
    ))


(defmethod player-pause-object ((self scheduler) (object score-element))
  (om-midi::midi-all-keys-off)
  (call-next-method))

(defmethod player-stop-object ((self scheduler) (object score-element))
  (om-midi::midi-all-keys-off)
  (when *micro-channel-mode-on*
    (loop for p in (collec-ports-from-object object) do (micro-reset p)))
  (call-next-method))


