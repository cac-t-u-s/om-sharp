(in-package :om)

;;;======================================
;;; DIFFERENT KIND OF DATA (FRAMES/BUNDLES)
;;;======================================
(defclass* data-frame (timed-item)
  ((date :accessor date :initarg :date :initform 0 :documentation "date/time of the frame")
   (attributes :accessor attributes :initarg :attributes :initform nil :documentation "some additional attributes for drawing etc.")))

;;; TIME-SEQUENCE API
;(defmethod date ((self dataframe)) (date self))
;(defmethod (setf date) (date (self dataframe)) (setf (item-time self) date))
(defmethod item-get-time ((self data-frame)) (date self))
(defmethod item-set-time ((self data-frame) time) (setf (date self) time))

(defmethod data-size ((self data-frame)) 1)
(defmethod data-frame-text-description ((self data-frame)) '("DATA FRAME"))

(defmethod get-frame-action ((self data-frame)) 
  #'(lambda () (print "EMPTY ACTION")))


;;; SIMPLEST DATA FRAME
(defclass act-bundle (data-frame)
  ((date :accessor date :initarg :dateg :initform 0 :documentation "date/time of the frame")
   (actions :accessor actions :initarg :actions :initform nil)))

(defmethod get-frame-action ((self act-bundle)) 
  #'(lambda () (mapcar 'funcall (actions self))))

(defun make-act-bundle (date actions)
  (make-instance 'act-bundle
                 :date date
                 :actions actions))

;;;======================================
;;; MAIN CLASS
;;;======================================
(defclass* data-stream (named-object time-sequence schedulable-object)
  ((default-frame-type :accessor default-frame-type :initarg :default-frame-type :initform 'act-bundle)
   (frames :accessor frames :initarg :frames :initform nil :documentation "a list of timed data chunks")
   (slice-duration :accessor slice-duration :initform nil)  ;;; what is it for ?
   ))

(defmethod om-init-instance ((self data-stream) &optional initargs)
  (call-next-method)
  (setf (frames self) (sort (remove nil (frames self)) '< :key 'item-get-time)) 
  (let ((frames (find-value-in-kv-list initargs :frames)))
    (when frames (setf (default-frame-type self) (type-of (car frames))))
    (mapc #'(lambda (f) (setf (attributes f) nil)) frames))
  self)

;; redefine for other slots
(defmethod data-stream-frames-slot ((self data-stream)) 'frames)

(defmethod frames ((self data-stream)) (slot-value self (data-stream-frames-slot self)))
(defmethod (setf frames) (frames (self data-stream)) (setf (slot-value self (data-stream-frames-slot self)) frames))

(defmethod data-stream-get-frames ((self data-stream)) (frames self))
(defmethod data-stream-set-frames ((self data-stream) frames) (setf (frames self) frames))
 
;;; TIME-SEQUENCE API
(defmethod time-sequence-get-timed-item-list ((self data-stream)) (data-stream-get-frames self))
(defmethod time-sequence-set-timed-item-list ((self data-stream) list) (data-stream-set-frames self list))

(defmethod time-sequence-make-timed-item-at ((self data-stream) at)
  (make-instance (default-frame-type self) :date at))

(defmethod display-modes-for-object ((self data-stream))
  '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self data-stream) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (om-with-fg-color (om-def-color :dark-blue)
      (multiple-value-bind (fx ox)
          (conversion-factor-and-offset 0 (get-obj-dur self) w x)
        (multiple-value-bind (fy oy) 
            (conversion-factor-and-offset 100 -100 (- h 20) (+ y 10))
          (loop for frame in (data-stream-get-frames self) do
                (om-draw-circle (+ ox (* fx (or (date frame) 0))) (+ oy (* fy (getf (attributes frame) :posy 0))) 
                                2 :fill t)))))))


;;;======================================
;;; OBJECT PROPERTIES
;;;======================================
(defmethod play-obj? ((self data-stream)) t)

(defmethod get-obj-dur ((self data-stream)) 
  (or (slice-duration self) ;; ???
      (call-next-method)))

(defmethod get-action-list-for-play ((object data-stream) interval &optional parent)
  (mapcar 
   #'(lambda (frame) 
       (list (date frame)
             #'(lambda () (funcall (get-frame-action frame)))))
   (remove-if #'(lambda (date) (or (< date (car interval)) (> date (cadr interval)))) 
              (data-stream-get-frames object) 
              :key 'date)))

(defmethod prune-object ((self data-stream) t1-ms t2-ms)
  (let ((t1 (max 0 (or t1-ms 0)))
        (t2 (min (get-obj-dur self) (or t2-ms *positive-infinity*))))
    (data-stream-set-frames self (filter-list (data-stream-get-frames self)
                                     t1
                                     t2
                                     :key 'date)
          (slice-duration self) (- t2 t1))
    (om-invalidate-view self)))


;;;======================================
;;; OMMETHOD FOR PATCHES
;;;======================================

(defmethod* add-frame-in-data-stream ((self data-stream) frame) 
   (insert-timed-point-in-time-sequence self frame)
   frame)

(defmethod* add-frame-in-data-stream ((self t) frame) 
  (om-beep-msg "ERROR: ~A is not a valid DATA-STREAM" self))

;;; when editing in mode "box" => allows to update editor
(defmethod* add-frame-in-data-stream ((self omboxeditcall) frame) 
   (insert-timed-point-in-time-sequence (get-box-value self) frame)
   (update-after-eval self)
   frame)



(defmethod* clear-data-stream ((self data-stream))
 (time-sequence-set-timed-item-list self nil))

(defmethod* clear-data-stream ((self t))
 (om-beep-msg "ERROR: ~A is not a valid DATA-STREAM" self))

;;; when editing in mode "box" => allows to update editor
(defmethod* clear-data-stream ((self omboxeditcall)) 
   (clear-data-stream (get-box-value self))
   (update-after-eval self))






