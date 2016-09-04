

(in-package :om)

(defclass container ()
  ((inside :accessor inside :initarg :inside :initform nil :documentation "the contents of the container...")))

;;; symbolic date and symbolic-dur make sense only if the object is in a context with tempo
(defclass score-object (data-frame)
  ((symbolic-date :accessor symbolic-date :initarg :symbolic-date :initform nil :documentation "date in symbolic musical time (ratio of beat)")
   (symbolic-dur :accessor symbolic-dur :initarg :symbolic-dur :initform nil :documentation "duration in symbolic musical time (ratio of beat)")))


