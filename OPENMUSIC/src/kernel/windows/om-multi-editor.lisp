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

(defmethod om-view-click-handler :around ((self OMBoxFrame) position)
  (declare (ignore position))
  (when (and (editor (om-view-container self))
             (container-editor (editor (om-view-container self))))
    (handle-multi-editor-click (om-view-container self) (container-editor (editor (om-view-container self)))))
  (call-next-method))

(defmethod dispatch-key-action ((self multi-view-editor) key) 
  (if (and (selected-view self) 
           (not (equal (editor (selected-view self)) self)))
      (editor-key-action (editor (selected-view self)) key)
    (call-next-method)))
