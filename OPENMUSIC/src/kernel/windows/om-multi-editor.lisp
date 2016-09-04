(in-package :om)


(defclass multi-view-editor () 
  ((selected-view :accessor selected-view :initform nil)))

(defclass multi-view-editor-view ()
  ((editor :accessor editor :initarg :editor :initform nil)))

(defmethod om-view-click-handler :around ((self multi-view-editor-view) pos)
  (let ((ed (and (editor self) 
                 (or (container-editor (editor self)) (editor self)))))
    (when ed (setf (selected-view ed) self)))
  (call-next-method))

(defmethod editor-key-action ((self multi-view-editor) key) 
  (or (when (and (selected-view self) (not (equal (editor (selected-view self)) self)))
        (editor-key-action (editor (selected-view self)) key))
      (call-next-method)))
