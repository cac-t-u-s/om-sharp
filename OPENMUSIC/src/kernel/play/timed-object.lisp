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
;Author: Jean Bresson, Dimitri Bouche & Jérémie Garcia
;;===========================================================================



;This class facillitates time manipulations in Maquette and the use of time markers with time rulers.

(in-package :om)

(defclass timed-object () 
  ((onset :accessor onset :initform 0 :initarg :onset)))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-time-markers ((self timed-object))
  "returns a list of time markers"
  (list 0))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-elements-for-marker ((self timed-object) marker)
  "returns a list of elements matching the marker"
  (list nil))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod translate-elements-from-time-marker ((self timed-object) elems dt)
  "translates elements from a time marker with dt"
  (setf (onset self) (max 0 (+ (onset self) dt))))

(defmethod set-object-onset ((self timed-object) onset)
  (setf (onset self) onset))
(defmethod set-object-onset ((self t) onset)
  onset)



