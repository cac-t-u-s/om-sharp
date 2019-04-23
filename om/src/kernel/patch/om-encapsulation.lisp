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
; File authors: J. Bresson, G. Holbrook
;============================================================================

;=================================================================
; ENCAPSULATION
; Patch encapsulation / de-encapsulation by Geof Holbrook in OM 6
; Adaptation OM7 J. Bresson - 2016
;=================================================================

(in-package :om)

(defmethod normalize-positions ((self ompatch))  
  (let ((minx (apply 'min (mapcar 'box-x (boxes self))))
        (miny (apply 'min (mapcar 'box-y (boxes self)))))
    (loop for box in (boxes self)
          do (move-box box (- 50 minx) (- 50 miny)))))
   
(defmethod shrink-patch-window-size ((self ompatch))
  (let ((maxx (apply 'max (mapcar #'(lambda (b) (+ (box-x b) (box-w b))) (boxes self))))
        (maxy (apply 'max (mapcar #'(lambda (b) (+ (box-y b) (box-h b))) (boxes self)))))
    (setf (window-size self) (om-make-point (+ maxx 100) (+ maxy 100)))))

;;; a slightly modified version of 'save-connections-from-boxes'
;;; where source boxes and dest boxes are two different sets 
(defmethod save-connections-from-boxes-2 (source-boxes dest-boxes)
  (loop for box in source-boxes 
        for b = 0 then (+ b 1) append
        (loop for out-c in (get-out-connections box)
              for c = 0 then (+ c 1)
              when (find (box (to out-c)) dest-boxes)
              collect (list :connection
                            (list :from (list :box b :out (position (from out-c) (outputs box)))) ;; (box out) 
                            (list :to (list :box (position (box (to out-c)) dest-boxes) 
                                            :in (position (to out-c) (inputs (box (to out-c)))))) ;;; (box in)
                            (list :attributes (list :color (omng-save (color out-c))
                                                    :style (style out-c) :modif (modif out-c)))
                            ))))


(defmethod make-incoming-connections ((patch ompatch) bridges outside-boxes inside-boxes)
  (loop for connection in bridges
        for origin = (find-value-in-kv-list (cdr connection) :from) 
        for destination = (find-value-in-kv-list (cdr connection) :to) 
        for index = 0 then (+ index 1) collect
        
        ;with entries  ;; keep track of new input boxes, with entries of the form (input-obj inactives-ordinal output-ordinal input-ordinal)
        ;with input-ord
        (let ((dest-box (nth (getf destination :box) inside-boxes))
              (dest-in (getf destination :in))
              (origin-box (nth (getf origin :box) outside-boxes))
              (origin-out (getf origin :out)))
          (let ((new-in (omng-make-new-boxcall 
                          (make-instance 'OMIn :name (string+ "input " (prin1-to-string (1+ index))))
                          (om-make-point (+ (box-x dest-box) (* dest-in (round (box-w dest-box) (length (inputs dest-box)))))
                                         (- (box-y dest-box) 50))
                          nil)))
            
            (omng-add-element patch new-in)
            
            ;; connection inside the patch
            (omng-add-element patch 
                              (omng-make-new-connection 
                               (car (outputs new-in))
                               (nth dest-in (inputs dest-box))
                               `(:style :curved :color ,(om-def-color :dark-blue))
                               ))

            ;; collect the outputs from outside
            (nth origin-out (outputs origin-box))
            ))))
            

(defmethod make-outgoing-connections ((patch ompatch) bridges outside-boxes inside-boxes)
  (let ((grouped-bridges nil))
    
    ;; pre-process to group the outputs when possible
    (loop for connection in bridges do
          (let ((existpos (position (find-value-in-kv-list (cdr connection) :from) 
                                    grouped-bridges
                                    :key 'cadar :test 'equal)))
            (if existpos 
                (setf (nth existpos grouped-bridges)
                      (append (nth existpos grouped-bridges)
                              `((:to ,(find-value-in-kv-list (cdr connection) :to)))))
              (push `((:from ,(find-value-in-kv-list (cdr connection) :from))
                      (:to ,(find-value-in-kv-list (cdr connection) :to)))
                    grouped-bridges))))
    
    (loop for connection in (reverse grouped-bridges)
          for index = 0 then (+ index 1) collect
          (let ((origin-box (nth (getf (cadr (car connection)) :box) inside-boxes))
                (origin-out (getf (cadr (car connection)) :out)))
          (let ((new-out (omng-make-new-boxcall 
                          (make-instance 'OMOut :name (string+ "output " (prin1-to-string (1+ index))))
                          (om-make-point (+ (box-x origin-box) (* origin-out 35))
                                         (+ (box-y origin-box) (box-h origin-box) 25))
                          nil)))
          
            (omng-add-element patch new-out)
            
            ;; connection inside the patch
            (omng-add-element patch 
                              (omng-make-new-connection 
                               (nth origin-out (outputs origin-box))
                               (car (inputs new-out))
                               `(:style :curved :color ,(om-def-color :dark-blue))))
            
            ;; collect the destination input(s)
            (loop for destination in (cdr connection) collect
                  (let ((dest-box (nth (getf (cadr destination) :box) outside-boxes))
                        (dest-in (getf (cadr destination) :in)))
                    (nth dest-in (inputs dest-box))))
            )))
    ))
            

(defmethod encapsulate-patchboxes ((editor patch-editor) (view patch-editor-view) boxes)
  (let* ((thispatch (object editor))
         (inactive-boxes (remove-if 'selected (boxes thispatch)))
         (newpatch (make-instance 'OMPatchInternal :name "my-patch"))
         (patchbox (omng-make-new-boxcall 
                    newpatch 
                    (om-make-point (round (average (mapcar 'box-x boxes) nil))
                                   (round (average (mapcar 'box-y boxes) nil)))
                    nil)))       
    ;insert new patch in current window
    (add-box-in-patch-editor patchbox view)

    (let ((copies (mapcar 'om-copy boxes))
          (connections (save-connections-from-boxes boxes)) 
          (coming-in (sort (save-connections-from-boxes-2 inactive-boxes boxes)
                           #'(lambda (b1 b2) 
                               (if (= (getf b1 :box) (getf b2 :box))
                                   (< (getf b1 :out) (getf b2 :out))
                                 (< (box-x (nth (getf b1 :box) inactive-boxes))
                                    (box-x (nth (getf b2 :box) inactive-boxes))))) 
                           :key #'(lambda (bridge) (find-value-in-kv-list (cdr bridge) :from))))
          (going-out (sort (save-connections-from-boxes-2 boxes inactive-boxes)
                           #'(lambda (b1 b2) 
                               (if (= (getf b1 :box) (getf b2 :box))
                                   (< (getf b1 :in) (getf b2 :in))
                                 (< (box-x (nth (getf b1 :box) inactive-boxes))
                                    (box-x (nth (getf b2 :box) inactive-boxes)))))
                           :key #'(lambda (bridge) (find-value-in-kv-list (cdr bridge) :to)))))
                
      ;add the copies to the new patch
      (loop for copy in copies do (omng-add-element newpatch copy))
      (loop for c in (restore-connections-to-boxes connections copies) do (omng-add-element newpatch c))
            
      ;MAKE THRESHOLD CONNECTIONS IN
      (let ((dest-outs (make-incoming-connections newpatch coming-in inactive-boxes copies)))
        (loop for dest-output in dest-outs 
              for i = 0 then (+ i 1) do
              (let ((connection (omng-make-new-connection 
                                 dest-output
                                 (nth i (inputs patchbox))
                                 `(:color ,(om-def-color :dark-blue))
                                 )))
                (omng-add-element thispatch connection)
                (add-connection-in-view view connection))))

      ;MAKE THRESHOLD CONNECTIONS OUT
      (let ((dest-ins (make-outgoing-connections newpatch going-out inactive-boxes copies)))
        (loop for dest-input-set in dest-ins 
              for i = 0 then (+ i 1) do
              (loop for dest-input in dest-input-set do
                    (let ((connection (omng-make-new-connection 
                                       (nth i (outputs patchbox))
                                       dest-input
                                       `(:color ,(om-def-color :dark-blue)))))
                      (omng-add-element thispatch connection)
                      (add-connection-in-view view connection)))))
 
      (normalize-positions newpatch)
      (shrink-patch-window-size newpatch)

      ;remove the original boxes
      (remove-boxes editor boxes)
      
      (select-box patchbox t)
      (om-invalidate-view view)
      (report-modifications editor)
      
      )))


;===============================
; UN-ENCAPSULATE
;===============================

(defmethod unencapsulate-patchboxes ((editor patch-editor) view boxes) 
  (mapc #'(lambda (b) (unencapsulate-box b editor view)) boxes))

(defmethod unencapsulate-box ((box t) (editor patch-editor) view) (om-beep))

(defmethod center-positions-around (boxes position)
  (when boxes 
    (let ((move-x (- (om-point-x position) (average (mapcar 'box-x boxes) nil)))
          (move-y (- (om-point-y position) (average (mapcar 'box-y boxes) nil))))
      (loop for box in boxes
            do (move-box box move-x move-y)))))

(defmethod decapsulable ((self OMPatch)) t)

(defmethod unencapsulate-box ((box omboxpatch) (editor patch-editor) view)  
  (when (decapsulable (object editor))
    (let* ((patch (object editor))
           (sub-patch (reference box))
           (all-boxes (boxes sub-patch))
           (all-connections (save-connections-from-boxes all-boxes))
           (internal-boxes (remove-if #'(lambda (box) (subtypep (type-of box) 'OMInOutBox)) all-boxes))
           (internal-connections (save-connections-from-boxes internal-boxes))
           (copies (mapcar 'om-copy all-boxes))
           (internal-copies (remove-if #'(lambda (box) (subtypep (type-of box) 'OMInOutBox)) copies)))
    
    ; reposition the new boxes
      (center-positions-around internal-copies (omp (box-x box) (box-y box)))
     
    ; add boxes in the main patch (except the io boxes)
      (loop for box in internal-copies
            do (add-box-in-patch-editor box view))
    
    ; restore to in/out connections
      (loop for ext-c in all-connections do
            (let* ((from (find-value-in-kv-list (cdr ext-c) :from))
                   (from-box (nth (getf from :box) copies))
                   (from-out (nth (getf from :out) (outputs from-box)))
                   (to (find-value-in-kv-list (cdr ext-c) :to))
                   (to-box (nth (getf to :box) copies))
                   (to-in (nth (getf to :in) (inputs to-box))))
            
              (cond ((and (typep from-box 'ominbox) (typep to-box 'omoutbox))
                     (let* ((index-i (index (reference (nth (getf from :box) all-boxes))))
                            (index-o (index (reference (nth (getf to :box) all-boxes))))
                            (connections-i (connections (nth (1- index-i) (inputs box)))) ;; in principle there can be no more than one
                            (connections-o (connections (nth (1- index-o) (outputs box)))))
                       (when connections-i
                         (loop for c in connections-o do
                               (let ((new-connection (omng-make-new-connection 
                                                      (from (car connections-i))
                                                      (to c)
                                                      `(:color ,(om-def-color :dark-blue)))))
                                 (omng-add-element patch new-connection)
                                 (add-connection-in-view view new-connection)))
                         )))
                  
                    ((typep from-box 'ominbox)
                     (let* ((index (index (reference (nth (getf from :box) all-boxes)))) ;; the original one (copies don't keep the id)
                            (connections (connections (nth (1- index) (inputs box))))) ;; in principle there can be no more than one
                       (loop for c in connections do
                             (let ((new-connection (omng-make-new-connection 
                                                    (from c)  ; who is connected to the index-th inlet of the box patch
                                                    to-in
                                                    `(:color ,(om-def-color :dark-blue)))))
                               (omng-add-element patch new-connection)
                               (add-connection-in-view view new-connection)))
                       ))
                  
                    ((typep to-box 'omoutbox)
                     (let* ((index (index (reference (nth (getf to :box) all-boxes)))) ;; the original one (copies don't keep the id)
                            (connections (connections (nth (1- index) (outputs box)))))
                       (loop for c in connections do
                             (let ((new-connection (omng-make-new-connection 
                                                    from-out  
                                                    (to c) ; who is connected to the index-th inlet of the box patch
                                                    `(:color ,(om-def-color :dark-blue)))))
                               (omng-add-element patch new-connection)
                               (add-connection-in-view view new-connection))
                             )))
                  
                    )))
     
      ; restore the rest of internal connections
      (loop for c in (restore-connections-to-boxes internal-connections internal-copies)
            do (omng-add-element patch c)
            do (add-connection-in-view view c))

      ; remove the original patch box
      (remove-boxes editor (list box))
      
      ; set the new boxes selected
      (loop for b in copies do (select-box b t)) 
      (om-invalidate-view view)
      (report-modifications editor)
      )))



