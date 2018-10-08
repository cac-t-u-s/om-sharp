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


(defclass multi-view-editor () 
  ((selected-view :accessor selected-view :initform nil)))

(defclass multi-view-editor-view ()
  ((editor :accessor editor :initarg :editor :initform nil)))

;;; the container editor can be the editor itself if if has several views inside
;;; or it can be another editor in case of objects embedded in others


(defmethod handle-multi-editor-click ((self om-view) (editor t)) nil)
(defmethod handle-multi-editor-click ((self om-view) (editor multi-view-editor))
  (setf (selected-view editor) self))

(defmethod om-view-click-handler :around ((self multi-view-editor-view) pos)
  (declare (ignore position))
  (when (and (editor self)
             (container-editor (editor self)))
    (handle-multi-editor-click self (container-editor (editor self))))
  (call-next-method))

;;; this method is also defined for OMBoxFrame
(defmethod dispatch-key-action ((self multi-view-editor) key) 
  (if (and (selected-view self) 
           (not (equal (editor (selected-view self)) self)))
      (editor-key-action (editor (selected-view self)) key)
    (call-next-method)))
