(in-package :om)






;=============================================================
; WRITE AND SEND OBJECTS AND LISTS AS ANTESCOFO STRUCTURES
;=============================================================

(defmethod write-obj-as-antescofo-struct ((ev event) &optional idx) "")

;(defmethod write-obj-as-antescofo-struct ((l list) &optional idx) (format nil "TAB [~a, \"~{~a~^ ~}\"]" idx l))
(defmethod write-obj-as-antescofo-struct ((l list) &optional idx) (format nil "\"~{~a~^ ~}\"" l))


(defmethod write-obj-as-antescofo-struct ((l label) &optional idx) (format nil "\"~{~a~^ ~}\"" (formatlabel l)))


(defmethod write-list-as-antescofo-map ((seq list) (startidx integer) &optional linearidxs)
  (let (( s "MAP {") (i 0))
    (loop for el in seq do
          (progn
            (setf s 
                  (concatenate 'string s 
                               (format nil "(~a, ~a)" (+ i startidx) 
                                       (if linearidxs 
                                           (write-obj-as-antescofo-struct el (+ i startidx)) 
                                         (write-obj-as-antescofo-struct el))
                                       ))) 
            (setf i (+ i 1))
            (if (nth i seq)
                (setf s (concatenate 'string s (format nil ", ") )) 
              (setf s (concatenate 'string s (format nil " }") )) )
            ))s))

;(write-list-as-antescofo-map (expand_grid (grid autumnleavesdomin_tune)) 0 t)
;(write-list-as-antescofo-map (makelabelsfromlist (expand_grid (grid autumnleavesdomin_tune)) 'harmlabel) 0 t)




(defmethod osc-send-list-as-antescofo-map ((sequence list) startidx hostsend portsend adresssend oscheadmess &optional linearidxs)
  (let ((pos startidx) (seq sequence) (sizemess 40)
        (fragseq nil) (s nil) (i 0) (j 0))
    (loop while seq do
          (progn
            (setf i 0)
            (setf fragseq (loop while (and seq (< (incf i) (+ sizemess 1))) collect (pop seq)))
            (setf s (write-list-as-antescofo-map fragseq pos linearidxs))
            (osc-send (list adresssend oscheadmess s) hostsend portsend)
            (incf j)
            (setf pos (+ pos (- i 1)))
            (if seq (sleep 0.051))))))



;=============================================================
; OVERLOADED METHODS
;=============================================================
(defmethod write-obj-as-antescofo-struct ((ev AudioHarmBeat) &optional idx) (format nil "TAB [~a, ~a]" (IdxInBuffer (data ev)) (CurrentTransfo (data ev))))

(defmethod write-obj-as-antescofo-struct ((ev AudioDescrBeat) &optional idx) (format nil "TAB [~a, ~a]" (IdxInBuffer (data ev)) (CurrentTransfo (data ev))))

(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent audioharmbeat) startidx hostsend portsend adresssend oscheadmess)
  (osc-send-list-as-antescofo-map sequence startidx hostsend portsend adresssend oscheadmess))

(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent audiodescrbeat) startidx hostsend portsend adresssend oscheadmess)
  (osc-send-list-as-antescofo-map sequence startidx hostsend portsend adresssend oscheadmess))






;===============
; WRITE ON DISK
;===============

(defmethod save-list ((self list) (name t))
  (WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))
  t)

(defmethod load-list ((name t))
  (WITH-OPEN-FILE (in name :direction :input  );:if-does-not-exist) ;:nil)
    (eval (read in)) (eval (read in))))

(defun osc-send-saved-list (path_list oscheadmess shift)
  (let ((l (load-list path_list)))
       (loop for j from 0 to (- (list-length l) 1) do
             
             (osc-send-sequence-fragment (list (nth j l)) (+ j shift) "127.0.0.1" 7657 "/modify" oscheadmess)
             (sleep 0.1)
             ;(format *om-stream* "Pos impro ~a : Idx ~a ; Label ~a ; RMStransf ~a ~%" j 
             ;        (IdxInBuffer (nth j impro))
             ;        (Label (nth j impro))
             ;        (TranspoRMSratio (nth j impro))
             ;        )
             )))











;;; ----- A REPRENDRE ------

;MidiHarmBeat
;--------------

(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent midiharmbeat) startidxinimpro hostsend portsend adresssend oscheadmess)
  sequence)

(defmethod write-midiharmbeat-as-group-for-antescofo ((self midiharmbeat) (beatduration integer) (numAntescofo integer))
  (let ((c-beat (clone-object self))
        (midicode nil)
        (midicodes ""))
    (setf (MidiSet c-beat) (sort (MidiSet c-beat) #'<= :key #'second)) 
    
    (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet c-beat)) for event in (MidiSet c-beat)
          do 
          (progn 
            ; 06/08/13 : RAJOUTÉ "@local" pour synchronisation évènements arrivés potentiellement trop tard
            ; (cf Delainegatif.asco.txt)
            (setf midicode (format nil "    ~a   mnote~D ~a ~a \"0.0.~a\" ~a @local~%" 
                                   (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))
                                   numAntescofo
                                   (MEPitch event)     
                                   (MEVel event) 
                                   (round (* (/ (MEDur event) beatduration) 480))
                                   (MEChannel event)))
            (setf midicodes (concatenate 'string midicodes midicode))))
    (format nil "group{~%~a}" midicodes)))








