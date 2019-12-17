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

;;;============================
;;; OSC BUNDLE OBJECT
;;;============================
(defclass* osc-bundle (data-frame)
  ((onset :accessor onset :initform 0 
          :initarg :onset :initarg :date  ;;; two possible initargs (for compatibility)
          :documentation "date/time of the object")
   (messages :accessor messages :initarg :messages :initform nil :documentation "list of osc messages")))

;(defmethod om-init-instance ((self osc-bundle) args)
;  (let* ((pointer-in (find-value-in-kv-list args :bundle-pointer))
;         (data-in (find-value-in-kv-list args :messages))
;         (tt-in (find-value-in-kv-list args :date)))
;    (when pointer-in
;      (when data-in 
;        (update-bundle-pointer-data pointer-in data-in))
;      (when tt-in 
;        (update-bundle-pointer-timetag pointer-in tt-in)))
;    self))

(defmethod initialize-instance :after ((self osc-bundle) &rest args)
  
  (unless (and (listp (messages self))
               (listp (car (messages self))))

    (setf (messages self) (if (listp (messages self))
                              (list (messages self))
                            nil))
    )
  
  (loop for msg in (messages self) 
        unless (stringp (car msg))
        do (setf (car msg) (string-downcase (format nil "~A" (car msg)))))
  )

;;; DATA-STREAM METHODS

(defmethod data-size ((self osc-bundle))
  (length (flat (messages self))))

(defmethod get-frame-action ((self osc-bundle))
  #'(lambda () (osc-send self "localhost" 3000)))

;;;=========================================================
;;; SEND
;;;=========================================================

(defmethod* osc-send ((self osc-bundle) host port)
  (osc-send (messages self) host port))


;;;=========================================================
;;; UTILITIES AND R/W ACCESSORS
;;;=========================================================

(defmethod osc-msg (address data)
  (cons (if (stringp address) address (format nil "~A" address))
        (if (listp data) data (list data))))

(defmethod osc-msg (address (data textbuffer))
  (osc-msg address (textbuffer-read data :text)))


(defmethod* osc-set ((self osc-bundle) address value)
  (let* ((copy (om-copy self))
         (mess (find address (messages copy) :key 'car :test 'string-equal)))
    (if mess 
        (setf (cdr mess) (list! value))
      (setf (messages copy) (append (messages copy) (list (cons address (list! value))))))
    copy))
    
(defmethod* osc-delete ((self osc-bundle) address)
  (let ((copy (om-copy self)))
    (setf (messages copy) (remove address (messages copy) :key 'car :test 'string-equal))
    copy))

(defmethod* osc-get ((self osc-bundle) address)
  (let* ((mess (find address (messages self) :key 'car :test 'string-equal)))
    (when mess (cdr mess))))


(defmethod* osc-timetag ((self osc-bundle) time)
  (let ((copy (om-copy self)))
    (setf (onset copy) time)
    copy))


;;;=========================================================
;;; BOX/DISPLAY
;;;=========================================================

;(defun format-message (message &optional (indent 0))
;  (if (stringp (car message))
;      (list (concatenate 'string 
;                   (make-sequence 'string (* indent 4) :initial-element #\Tab)
;                   (car message) " "
;                   (format nil "~A" (cdr message))))
;    (loop for sub in message append 
;          (format-message sub (1+ indent)))
;    ))

;(defun format-message (message &optional (indent 0))
;  (let ((tab (make-sequence 'string (* indent 4) :initial-element #\Space)))
;  (append 
;   (list (concatenate 'string tab (car message) " [ "))
;   (loop for arg in (cdr message) append (format-argument arg (1+ indent)))
;   (list (concatenate 'string tab " ] "))
;  )))

;;; simpler: just on 1 line
(defun format-message (message &optional (indent 0))
  (declare (ignore indent))
  (format nil "~{~a~^ ~}" message))

(defmethod format-argument (arg indent) 
  (let ((tab (make-sequence 'string (* indent 4) :initial-element #\Space)))
    (list (concatenate 'string tab (format nil "~A" arg)))))

(defmethod format-argument ((arg list) indent) 
  (loop for msg in arg collect (format-message msg indent)))

(defmethod data-frame-text-description ((self osc-bundle))
  (cons "OSC BUNDLE" 
        (flat (mapcar 'format-message (messages self))))
  )

(defmethod display-modes-for-object ((self osc-bundle))
  '(:mini-view :text :hidden))

(defmethod draw-mini-view ((self osc-bundle) (box t) x y w h &optional time)
  (let ((display-cache (ensure-cache-display-text box self)))
    (om-draw-rect x y w h :fill t :color (om-gray-color 0.48))
    (om-with-fg-color (om-def-color :white)
      (om-with-font 
       (om-def-font :font1)
       (loop for msg in (find-value-in-kv-list display-cache 'messages)
             for yy = 20 then (+ yy 12) while (< yy h) do
             (om-draw-string 6 yy (format nil "~{~a~^ ~}" msg))
             )
       ))
    ))


(defun find-osc-values (osc-bundle address)
  (cdr (find address (messages osc-bundle) :test 'string-equal :key 'car)))

(defmethod get-frame-color ((self osc-bundle)) 
  (let ((colorvals (find-osc-values self "/color")))
    (if colorvals (apply 'om-make-color colorvals)
      (call-next-method))))

(defmethod get-frame-posy ((self osc-bundle)) 
  (or (car (find-osc-values self "/y")) (call-next-method)))

(defmethod get-frame-sizey ((self osc-bundle)) 
  (or (car (find-osc-values self "/size")) (call-next-method)))




;;;=========================================================
;;; EDITOR
;;;=========================================================

(defmethod object-has-editor ((self osc-bundle)) t)
(defmethod get-editor-class ((self osc-bundle)) 'osc-editor)

(defclass osc-editor (OMEditor) ())
  
(defmethod get-properties-list ((self osc-bundle)) nil)

(defun bundle-to-text (oscb)
  (apply #'concatenate 
         (cons 'string
               (loop for msg in (messages oscb)
                     append 
                     (list (message-to-text msg) (string #\Newline))))))

(defun message-to-text (msg)
  (apply #'concatenate 
         (cons 'string 
               (append (list (car msg) " ")
                       (loop for arg in (cdr msg)
                             collect (if (stringp arg) 
                                         (format nil "~s " arg)
                                       (format nil "~a " arg)))
                       )
               )))
                                     
             
(defmethod make-editor-window-contents ((self osc-editor))
  (let ((oscb (object-value self)))
    (om-make-layout
     'om-simple-layout
     :subviews (list 
              (om-make-di 
               'om-text-edit-view
               :size (omp 200 200)
               :text (bundle-to-text oscb) 
               :edit-action #'(lambda (item)
                                (let ((messages (remove nil
                                                        (loop for line in (om-text-to-lines (om-dialog-item-text item))
                                                              when (> (length (delete-spaces line)) 0)
                                                              collect 
                                                              (multiple-value-bind (address data)
                                                                  (string-until-char (delete-spaces line) " ")
                                                                (cons address
                                                                      (when data 
                                                                        (om-read-list-from-string data))))))))
                                  (setf (messages oscb) messages)
                                  (report-modifications self)))
               )))
    ))

(defmethod update-to-editor ((self osc-editor) from)
  (declare (ignore from))
  (om-set-dialog-item-text (car (om-subviews (main-view self)))
                           (bundle-to-text (object-value self))))
  


;;;======================================
;;; TEMP: GENERATE RANDOM BUNDLES
;;;======================================
(defun gen-bundles (n)
  (and n
       (sort 
        (loop for i from 1 to n collect 
              (make-instance 'osc-bundle 
                             :date (om-random 0.0 60000.0)
                             :messages (loop for i from 0 to (random 6) append
                                             (list (list "/foo/b/c" (random 20) (random 20.0) (random 20.0))
                                                   (list "/a/param" 
                                                    (random 60.0)
                                                    ;(loop for i from 0 to (random 3) collect 
                                                    ;      (list "/control/a" (random 20)))
                                                    )))
                             ))
        '< :key #'date)))


