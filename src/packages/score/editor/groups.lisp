;============================================================================
; om#: visual programming language for computer-assisted music composition
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
; GROUPS in score editors.
; Uses the GROUP-IDS list of score-element objects
; File author: J. Bresson, 2021
;============================================================================


(in-package :om)


(defmethod collect-group-ids ((self score-editor))
  (let ((groups ()))
    (loop for c in (chords (object-value self))
          do (loop for id in (group-ids c)
                   unless (find id groups :test 'string-equal)
                   do (pushr id groups)))
    groups))


(defmethod get-group-elements ((self score-editor) group-id)
  (loop for c in (chords (object-value self))
        when (find group-id (group-ids c) :test 'string-equal)
        collect c))


(defmethod generate-group-id ((self score-editor))
  (let* ((other-ids (collect-group-ids self))
         (index (1+ (length other-ids)))
         (group-id (format nil "group~D" index)))

    (loop while (find group-id other-ids :test 'string-equal)
          do (setf group-id (format nil "group~D" (incf index))))

    group-id))


;;; add to the current group if a group is selected
;;; create a new group if :all-groups
(defmethod add-selection-to-group ((self score-editor))

  (let ((group (if (equal (editor-get-edit-param self :selected-group) :all)
                   (generate-group-id self)
                 (editor-get-edit-param self :selected-group))))

    (loop for item in (selection self)
          do (setf (group-ids item)
                   (append (group-ids item) (list group))))

    (editor-update-analysis self)

    (update-group-controls self)))


;;; deleted the group if a specific group is selected
;;; or all groups (with warning)
(defmethod delete-current-group ((self score-editor))

  (let* ((group-id (editor-get-edit-param self :selected-group))
         (remove-all (equal group-id :all)))

    (unless (and remove-all
                 (not (om-y-or-n-dialog "This will delete all groups in the score. Do it ?")))

      (loop for c in (chords (object-value self))
            do (setf (group-ids c)
                     (if remove-all
                         nil
                       (remove group-id (group-ids c) :test 'string-equal))))

      (update-group-controls self))))


(defmethod rename-current-group ((self score-editor))

  (let* ((group-id (editor-get-edit-param self :selected-group)))

    (unless (equal group-id :all)

      (let ((new-id (om-get-user-string "Enter a new name for the group" :initial-string (string group-id))))
        (when new-id

          (loop for c in (chords (object-value self))
                do (setf (group-ids c)
                         (substitute new-id group-id (group-ids c) :test 'string-equal)))

          (editor-set-edit-param self :selected-group new-id)
          (update-group-controls self)))
      )))


;;; delete the group(s) of selected items if :all-groups
;;; or remove the selected item from a specific group
(defmethod delete-selection-group ((self score-editor))

  (let* ((group (editor-get-edit-param self :selected-group))
         (remove-all (equal group :all)))

    (loop for item in (selection self)
          do (setf (group-ids item)
                   (if remove-all
                       nil
                     (remove group (group-ids item) :test 'string-equal))))

    (editor-update-analysis self)

    (update-group-controls self)))


(defmethod draw-groups-on-score-editor (editor)

  (let ((color (om-make-color .4 .4 .4))
        (fill (om-make-color .4 .4 .4 .2))
        (draw-name (editor-get-edit-param editor :group-names))
        (margin-x 10)
        (margin-y 5))

    (loop for group-id in (if (equal (editor-get-edit-param editor :selected-group) :all)
                              (collect-group-ids editor)
                            (list (editor-get-edit-param editor :selected-group)))
          do (loop for c in (chords (object-value editor))
                   when (and (b-box c) (find group-id (group-ids c) :test 'string-equal))
                   minimize (b-box-x1 (b-box c)) into x1
                   and minimize (b-box-y1 (b-box c)) into y1
                   and maximize (b-box-x2 (b-box c)) into x2
                   and maximize (b-box-y2 (b-box c)) into y2
                   finally

                   (om-draw-rounded-rect
                    (- x1 margin-x) (- y1 margin-y) (- x2 x1 (* -2 margin-x)) (- y2 y1 (* -2 margin-y))
                    :color color :round 8 :line 2)
                   (om-draw-rounded-rect
                    (- x1 margin-x) (- y1 margin-y) (- x2 x1 (* -2 margin-x)) (- y2 y1 (* -2 margin-y))
                    :color fill :fill t :round 8)
                   (when draw-name
                     (om-draw-string (- x1 margin-x) (+ y2 12 margin-y) group-id :color color))

                   (when (and (editor-get-edit-param editor :analysis)  (analysis editor))
                     (draw-analysis-for-group (analysis editor) editor group-id x1 y1 x2 y2))
                   ))
    ))


(defmethod update-group-controls ((editor score-editor))

  (let ((groups (collect-group-ids editor))
        (current-group-id (editor-get-edit-param editor :selected-group))
        (menu (get-g-component editor :groups-menu))
        (delete-button (get-g-component editor :delete-group-button))
        (rename-button (get-g-component editor :rename-group-button))
        (group-names-box (get-g-component editor :group-names-box))
        (group-names-label (get-g-component editor :group-names-label)))


    (unless (find current-group-id groups :test 'string-equal)
      (editor-set-edit-param editor :selected-group :all)
      (setf current-group-id :all))

    (when menu
      (om-set-item-list menu (append '(:all "-") (collect-group-ids editor)))
      (om-set-selected-item menu current-group-id)
      (om-enable-dialog-item menu (editor-get-edit-param editor :groups)))

    (when delete-button
      (om-enable-dialog-item
       delete-button
       (and (editor-get-edit-param editor :groups) groups)))

    (when rename-button
      (om-enable-dialog-item
       rename-button
       (and (editor-get-edit-param editor :groups) groups
            (not (equal current-group-id :all)))))

    (when group-names-box
      (om-enable-dialog-item
       group-names-box
       (editor-get-edit-param editor :groups)))

    (when group-names-label
      (om-set-fg-color
       group-names-label
       (if (editor-get-edit-param editor :groups)
           (om-def-color :black) (om-def-color :gray))))

    (editor-invalidate-views editor)
    ))


(defmethod editor-groups-controls ((editor chord-seq-editor))

  (let ((groups (collect-group-ids editor)))

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
                    :text "Groups"
                    :font (om-def-font :font1b))

        (om-make-di 'om-check-box :text "" :font (om-def-font :font1)
                    :size (omp 20 20)
                    :checked-p (editor-get-edit-param editor :groups)
                    :di-action #'(lambda (item)
                                   (editor-set-edit-param editor :groups (om-checked-p item))
                                   (update-group-controls editor)))
        ))

      (om-make-layout
       'om-row-layout
       :align :bottom
       :subviews
       (list

        (set-g-component
         editor :groups-menu
         (om-make-di 'om-popup-list :items (append '(:all "-") groups)
                     :size (omp 95 22) :font (om-def-font :font1)
                     :enabled (editor-get-edit-param editor :groups)
                     :value (editor-get-edit-param editor :selected-group)
                     :di-action #'(lambda (list)
                                    (editor-set-edit-param editor :selected-group (om-get-selected-item list))
                                    (update-group-controls editor)
                                    )))


        (set-g-component
         editor :delete-group-button
         (om-make-di 'om-button
                     :text "x"
                     :size (omp 34 24) :font (om-def-font :font1)
                     :enabled (and (editor-get-edit-param editor :groups) groups)
                     :di-action #'(lambda (b)
                                    (declare (ignore b))
                                    (delete-current-group editor))))
        ))

      (om-make-layout
       'om-row-layout
       :align :bottom
       :subviews
       (list
        ; (om-make-graphic-object 'om-item-view :size (omp 78 18))

        (set-g-component
         editor :group-names-label
         (om-make-di 'om-simple-text :size (om-make-point 72 18)
                     :text "Names"
                     :fg-color (if (editor-get-edit-param editor :groups)
                                   (om-def-color :black) (om-def-color :gray))
                     :font (om-def-font :font1)))

        (set-g-component
         editor :group-names-box
         (om-make-di 'om-check-box :text "" :font (om-def-font :font1)
                     :size (omp 20 20)
                     :checked-p (editor-get-edit-param editor :group-names)
                     :enabled (editor-get-edit-param editor :groups)
                     :di-action #'(lambda (item)
                                    (editor-set-edit-param editor :group-names (om-checked-p item))
                                    (update-group-controls editor))))

        (set-g-component
         editor :rename-group-button
         (om-make-di 'om-button
                     :text "Rename"
                     :size (omp 70 24) :font (om-def-font :font1)
                     :enabled (and (editor-get-edit-param editor :groups)
                                   groups
                                   (not (equal (editor-get-edit-param editor :selected-group) :all)))
                     :di-action #'(lambda (b)
                                    (declare (ignore b))
                                    (rename-current-group editor))))
        ))
      ))
    ))

