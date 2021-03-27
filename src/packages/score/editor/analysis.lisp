;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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
; TOOLS FOR MUSIC ANALYSIS
; J. Bresson 2021
;============================================================================


(in-package :om)


(defclass abstract-analysis () ())

;;; API
(defmethod update-analysis ((self abstract-analysis) (editor score-editor)) nil)
(defmethod draw-analysis-for-group ((self abstract-analysis) (editor score-editor) group-id x1 y1 x2 y2) nil)

(defparameter *registered-analyses* nil)


(defmethod editor-update-analysis ((editor score-editor))

  (when (and (editor-get-edit-param editor :analysis)
             (null (analysis editor)))
    (let ((analysis-type (editor-get-edit-param editor :selected-analysis)))
      (when analysis-type
        (setf (analysis editor) (make-instance analysis-type)))))

  (when (analysis editor)
    (update-analysis (analysis editor) editor)
    (editor-invalidate-views editor)))


(defmethod init-editor :after ((self score-editor))
  (editor-update-analysis self))


(defmethod editor-analysis-controls ((editor chord-seq-editor))

  (om-make-layout
   'om-column-layout
   :gap 0
   :subviews
   (list

    (om-make-layout
     'om-row-layout
     :align :bottom
     :subviews
     (list
      (om-make-di 'om-simple-text :size (om-make-point 72 18)
                  :text "Analysis"
                  :font (om-def-font :font1b))

      (om-make-di 'om-check-box :text "" :font (om-def-font :font1)
                  :size (omp 20 20)
                  :checked-p (editor-get-edit-param editor :analysis)
                  :di-action #'(lambda (item)
                                 (editor-set-edit-param editor :analysis (om-checked-p item))
                                 (om-enable-dialog-item (get-g-component editor :analysis-menu) (om-checked-p item))
                                 (if (om-checked-p item)
                                     (editor-update-analysis editor)
                                   (editor-invalidate-views editor))
                                 ))
      ))

    (om-make-layout
     'om-row-layout
     :align :bottom
     :subviews
     (list
      (set-g-component
       editor :analysis-menu
       (om-make-di 'om-popup-list :items *registered-analyses*
                   :size (omp 120 22) :font (om-def-font :font1)
                   :enabled (editor-get-edit-param editor :analysis)
                   :value (editor-get-edit-param editor :selected-analysis)
                   :di-action #'(lambda (list)
                                  (let ((analysis-type (om-get-selected-item list)))
                                    (editor-set-edit-param editor :selected-analysis analysis-type)
                                    (setf (analysis editor) (make-instance analysis-type))
                                    (editor-update-analysis editor)
                                    ))))
      ))
    ))
  )


;;;=========================
;;; Example: PCSET ANALYSIS
;;;=========================

(defclass pcset-analysis (abstract-analysis)
  ((pcsets :accessor pcsets :initform nil :type list)))

(pushr 'pcset-analysis *registered-analyses*)


(defmethod update-analysis ((self pcset-analysis) (editor score-editor))
  (setf (pcsets self)
        (loop for group-id in (collect-group-ids editor)
              collect (let ((chords (get-group-elements editor group-id)))
                        (list
                         group-id
                         (chord2c
                          (make-instance 'chord :lmidic (apply 'append (mapcar 'lmidic chords)))
                          2))
                        ))
        ))


(defmethod draw-analysis-for-group ((self pcset-analysis) (editor score-editor) group-id x1 y1 x2 y2)
  (declare (ignore editor))
  (let ((cercle (cadr (find group-id (pcsets self) :key 'car :test 'string-equal))))
    (when cercle
      (draw-cercle cercle x1 (+ y2 20) (- x2 x1) 120))))


