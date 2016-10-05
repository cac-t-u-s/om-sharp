;;===========================================================================
;Copyright (C) 2015 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Author: Dimitri Bouche
;;===========================================================================

;;===========================================================================
;DocFile
;  Metronome
;;===========================================================================
(in-package :om)

;(defconstant *metronome-up-path* (namestring (current-pathname "./Metronome-up.wav")))
;(defconstant *metronome-down-path* (namestring (current-pathname "./Metronome-down.wav")))
;(om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil nil))

;;===========================================================================
;;;Metronome
;;===========================================================================
;;;Structure
(defclass metronome (schedulable-object)
  ((editor :initform nil :initarg :editor :accessor editor)
   (time-signature :initform '(4 4) :initarg :time-signature :accessor time-signature)
   (click :initform '(nil nil) :initarg :click :accessor click)))

(defmethod initialize-instance :after ((self metronome) &rest initargs)
  (setf (click self)
        (list (om-midi:make-midi-evt 
                               :type :keyOn
                               :chan 10 :port 0
                               :fields (list 32 127)) ;44
              (om-midi:make-midi-evt 
                               :type :keyOff
                               :chan 10 :port 0
                               :fields (list 32 127)))))

(defmethod get-action-list-for-play ((self metronome) time-interval &optional parent)
  (filter-list (loop for beat in (get-beat-grid (get-tempo-automation (editor self)) (car time-interval) (cadr time-interval))
                     collect
                     (list
                      (car beat)
                      #'(lambda ()
                          (mapcar 'om-midi::midi-send-evt (click self)))))
               (car time-interval) (cadr time-interval)
               :key 'car))

(defmethod get-obj-dur ((self metronome))
  *positive-infinity*)

