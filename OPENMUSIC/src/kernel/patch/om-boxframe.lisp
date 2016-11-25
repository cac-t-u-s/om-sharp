;;;================================
;;; Basic frames for OM boxes
;;;================================

(in-package :om)

;;; A Frame whose object is an OMBox
(defclass OMBoxFrame (OMSimpleFrame) 
  ((icon-id :accessor icon-id :initform nil :initarg :icon-id)
   (icon-size :accessor icon-size :initform nil)))
 
(defmethod get-help ((self OMBoxFrame)) (get-box-help (object self)))
(defmethod get-box-help ((self OMBox)) nil)

;;; calculates "non graphic" coordinates in view from pixel pos
(defmethod omng-x ((container t) pix-x) pix-x)
(defmethod omng-y ((container t) pix-y) pix-y)
(defmethod omng-w ((container t) pix-w) pix-w)
(defmethod omng-h ((container t) pix-h) pix-h)

(defmethod omng-position ((container t) pix-position)
  (omp (omng-x container (om-point-x pix-position))
       (omng-y container (om-point-y pix-position))))

(defmethod omng-size ((container t) pix-size)
  (omp (omng-w container (om-point-x pix-size))
       (omng-h container (om-point-y pix-size))))

;;; calculates "graphic" coordinates in view from symbolic values
(defmethod omg-x ((container t) s-x) s-x)
(defmethod omg-y ((container t) s-y) s-y)
(defmethod omg-w ((container t) s-w) s-w)
(defmethod omg-h ((container t) s-h) s-h)

(defmethod omg-position ((container t) s-position) 
  (omp (omg-x container (om-point-x s-position))
       (omg-y container (om-point-y s-position))))

(defmethod omg-size ((container t) s-size) 
  (omp (omg-w container (om-point-x s-size))
       (omg-h container (om-point-y s-size))))

(defmethod omg-add-element ((container t) (frame OMFrame))
  (let ((scaledsize (omg-size container (omp (box-w (object frame)) (box-h (object frame)))))
        (scaledpos (omg-position container (omp (box-x (object frame)) (box-y (object frame))))))
    (om-set-view-position frame scaledpos)
    (om-set-view-size frame (omp (if (scale-in-x-? (object frame)) (om-point-x scaledsize) (box-w (object frame)))
                                 (if (scale-in-y-? (object frame)) (om-point-y scaledsize) (box-h (object frame)))))
    (om-add-subviews container frame)))


;;;================================
;;; I/O areas
;;;================================

(defclass io-area (frame-area) 
  ((temp-locked :initform nil :accessor temp-locked)))

(defmethod io-position-in-patch ((self io-area))
  (om-add-points (om-view-position (frame self))
                 (get-position self)))

(defclass input-area (io-area) ())
(defclass output-area (io-area) ())

(defmethod io-color ((self OMBoxIO)) (om-def-color :dark-gray))
(defmethod io-color ((self box-optional-input)) (om-def-color :gray))
(defmethod io-color ((self box-keyword-input)) (om-make-color 0.2 0.4 0.5))
(defmethod io-color ((self box-keyword-output)) (om-make-color 0.2 0.4 0.5))

(defmethod area-tt-text ((self io-area)) 
  (remove 
   nil 
   (list (string+ 
          (format nil "~A~A" 
                 (io-prefix (object self))
                 (or (name (object self)) ""))
          (if (connections (object self)) ""
            (format nil " [~s]" (value (object self)))))
         (get-input-doc-string (object self))
         )))

(defmethod area-tt-pos ((self output-area)) 
  (om-add-points 
   (call-next-method)
   (om-make-point 30 30)))

(defmethod om-enter-area ((area io-area)) 
  (unless (temp-locked area)
    (call-next-method)))
  
(defparameter +active-r+ 5)
(defparameter +inactive-r+ 2.5)

(defmethod om-draw-area ((area io-area))
  (let ((p (get-position area))
        (r (if (active area) +active-r+ +inactive-r+)))
    (when (reactive (object area))
      (om-with-fg-color (om-def-color :dark-red)
        (om-draw-circle (om-point-x p) (om-point-y p) (1+ r) :fill t)))
    (om-with-fg-color (io-color (object area))
      (om-draw-circle (om-point-x p) (om-point-y p) r :fill t))))

;;;=============================
;;; BOX I/O EDITS
;;;=============================

(defclass input-edit-area (frame-area) ())
(defclass ++input-area (input-edit-area) ())
(defclass --input-area (input-edit-area) ())

(defmethod disabled-area ((area input-edit-area))
  (container-frames-locked (om-view-container (frame area))))

(defmethod area-tt-text ((self ++input-area)) 
  "add optional/keyword input")

(defmethod area-tt-text ((self --input-area)) 
  "remove optional/keyword input")

(defmethod allow-remove-inputs ((self OMBox))  
  (or (get-optional-inputs self)
      (get-keyword-inputs self)))

(defmethod allow-add-inputs ((self OMBox))  
  (or (next-optional-input self)
      (next-keyword-input self)))

(defmethod input-edit-areas ((self OMBoxFrame))
  (let ((S 5))
    (remove nil 
            (list 
             (when (allow-add-inputs (object self))
               (make-instance '++input-area :object self :frame self
                              :pos #'(lambda (f) (om-make-point (- (w f) (* 2 S)) S))
                              :pick #'(lambda (f) (list (- S) (- S) S S))))
             (when (allow-remove-inputs (object self))
               (make-instance '--input-area :object self :frame self
                            :pos #'(lambda (f) (om-make-point (- (w f) S) (* 2 S)))
                            :pick #'(lambda (f) (list (- S) (- S) S S))))))))

(defmethod om-draw-area ((area input-edit-area))
  (let ((p (get-position area)))
    (om-with-fg-color (om-def-color :light-gray)
      (om-draw-circle (om-point-x p) (om-point-y p) 5 :fill t))))

(defmethod om-draw-area ((area ++input-area))
  (unless (disabled-area area)
    (let ((p (get-position area)))
      (call-next-method)
      (om-with-fg-color (if (active area) (om-def-color :dark-red) (om-def-color :gray))
        (om-with-line-size (if (active area) 2 1)
          (om-draw-line (om-point-x p) (- (om-point-y p) 2.5)
                        (om-point-x p) (+ (om-point-y p) 2.5))
          (om-draw-line (- (om-point-x p) 2.5) (om-point-y p)
                        (+ (om-point-x p) 2.5) (om-point-y p)))))))

(defmethod om-draw-area ((area --input-area))
  (unless (disabled-area area)
    (let ((p (get-position area)))
      (call-next-method) 
      (om-with-fg-color (if (active area) (om-def-color :dark-red) (om-def-color :gray))
        (om-with-line-size (if (active area) 2 1)
          (om-draw-line (- (om-point-x p) 2.5) (om-point-y p)
                        (+ (om-point-x p) 2.5) (om-point-y p)))))))
  
(defmethod add-next-input ((boxframe OMBoxFrame))
  (let ((box (object boxframe)))
    (cond ((next-optional-input box)
           (optional-input++ box))
          ((next-keyword-input box)
           (keyword-input++ box))
          (t nil))))

(defmethod click-in-area ((self ++input-area) boxframe)
  (add-next-input boxframe) t)
      
(defmethod click-in-area ((self --input-area) boxframe)
  (let ((box (object boxframe)))
    (if (get-optional-inputs box)
        (optional-input-- box)
      (if (get-keyword-inputs box)
          (keyword-input-- box)))
    t))

;;;=============================
;;; RESIZE 
;;;=============================

(defclass resize-area (frame-area) ((p0 :accessor p0 :initform nil)))
(defclass h-resize-area (resize-area) ())
(defclass v-resize-area (resize-area) ())

(defmethod resize-areas ((self OMBoxFrame))
  (list 
   (make-instance 'resize-area :object self :frame self
                  :pos #'(lambda (f) (om-make-point (- (w f) 8) (- (h f) 8)))
                  :pick #'(lambda (f) (list 0 0 12 12)))
   (make-instance 'h-resize-area :object self :frame self
                  :pos #'(lambda (f) (om-make-point (- (w f) 8) 16))
                  :pick #'(lambda (f) (list 0 0 12 (- (h f) 16))))
   (make-instance 'v-resize-area :object self :frame self
                  :pos #'(lambda (f) (om-make-point 0 (- (h f) 8)))
                  :pick #'(lambda (f) (list 0 0 (- (w f) 8) 12)))
   ))

(defparameter *resize-handler* nil)

(defun get-cursor-if-allowed (area cursor)
  (unless (container-frames-locked (om-view-container (frame area)))
    (om-get-cursor cursor)))

(defmethod om-view-cursor ((self resize-area)) (get-cursor-if-allowed self :resize))
(defmethod om-view-cursor ((self h-resize-area)) (get-cursor-if-allowed self :h-size))
(defmethod om-view-cursor ((self v-resize-area)) (get-cursor-if-allowed self :v-size))

(defmethod resize-handle ((self resize-area) container frame pos) 
  (let ((pp (om-add-points (p0 self) pos)))
      (om-set-view-size frame 
       (om-borne-point 
        (resize-frame-size self frame pp)
        (minimum-size (object frame)) 
        (maximum-size (object frame))
        ))))

(defmethod resize-frame-size ((self resize-area) frame pos) pos)
(defmethod resize-frame-size ((self h-resize-area) frame pos) (omp (om-point-x pos) (h frame)))
(defmethod resize-frame-size ((self v-resize-area) frame pos) (omp (w frame) (om-point-y pos)))


(defmethod click-in-area ((self resize-area) boxframe) 
  (unless (container-frames-locked (om-view-container boxframe))
    (setf (p0 self) (om-subtract-points (om-view-size boxframe) (om-mouse-position boxframe)))
    (select-box (object boxframe) t)
    (setf *resize-handler* self)))

(defmethod om-click-release-handler ((self OMBoxFrame) pos)
  (when *resize-handler*
    (setf *resize-handler* nil)
    (let* ((box (object self))
           (view (om-view-container self))
           (size (om-view-size self)))
      (omng-resize box 
                   (omp (if (scale-in-x-? box) (omng-w view (om-point-x size)) (om-point-x size))
                        (if (scale-in-y-? box) (omng-h view (om-point-y size)) (om-point-y size))))
      )
    (redraw-connections self)))

(defmethod om-click-motion-handler ((self OMBoxFrame) pos)
  (if *resize-handler*
      (progn (resize-handle *resize-handler* (om-view-container self) self pos)
        (mapcar 'update-points (get-box-connections (object self)))
        (redraw-connections self))
        ;;; drag and drop
  (call-next-method)))


;;;=============================
;;; SHOW INSPECTOR BUTTON
;;;=============================

(defclass get-info-area (frame-area) ())

(defmethod area-tt-text ((self get-info-area)) nil) ;"open inspector")

(defmethod om-draw-area ((area get-info-area))
  (let ((p (get-position area)))
    (when (and (active area) (> (w (frame area)) 40))
      (om-with-fg-color (om-def-color :gray)
        (om-draw-circle (+ 7 (om-point-x p)) (om-point-y p) 6 :fill t))
      (om-with-fg-color (om-def-color :white)
        (om-draw-circle (+ 7 (om-point-x p)) (om-point-y p) 6 :fill nil))
      (om-with-fg-color (om-def-color :white)
        (om-with-font 
         (om-def-font :font2b)
         (om-draw-string (+ (om-point-x p) 5) (+ (om-point-y p) 4) "i"))))))

(defmethod click-in-area ((self get-info-area) boxframe)
  (when (> (w boxframe) 40)
    (if *inspector-window*
        (update-inspector (object boxframe) boxframe)
      (show-inspector (object boxframe) boxframe))))
      

(defmethod info-area ((self OMBoxFrame))
  (list 
   (make-instance 'get-info-area :object self :frame self
                  :pos #'(lambda (f) (om-make-point 0 (/ (h f) 2)))
                  :pick #'(lambda (f) (list 0 -8 16 8)))
   ))



;;;=============================
;;; General Methods (OMBOXFRAME)
;;;=============================

(defmethod set-frame-areas ((self t)) nil)

(defmethod set-frame-areas ((self OMBoxFrame))
  (let* ((box (object self))
         (nin (length (inputs box)))
         (nout (length (outputs box)))
         (eia (input-edit-areas self))
         (statesign-w 5)
         (extra-w (+ (* statesign-w 2) (if eia 10 0))) 
         (n -1))
    (setf (areas self)
          (append (mapcar #'(lambda (in)
                              (setf (area in)
                                    (make-instance 'input-area :object in :frame self
                                                   :pos #'(lambda (f) 
                                                            (om-make-point 
                                                             (+ statesign-w 
                                                                (* (- (w f) extra-w) 
                                                                   (/ (1+ (* 2 (or (position in (inputs box)) 0))) (* 2 nin))))
                                                             +active-r+))
                                                   :pick '(-6 -15 6 4))))
                          (inputs (object self)))
                  (mapcar #'(lambda (out)
                              (setf (area out) 
                                    (make-instance 'output-area :object out :frame self
                                                   :pos #'(lambda (f) 
                                                            (om-make-point 
                                                             (+ statesign-w 
                                                                (* (- (w f) extra-w)
                                                                   (/ (1+ (* 2 (or (position out (outputs box)) 0))) (* 2 nout))))
                                                             (- (round (h f)) +active-r+) ;;; (would be better if H was already an integer...)
                                                             ))
                                                   :pick '(-6 -6 6 6))))
                          (outputs (object self)))
                  (resize-areas self)
                  (info-area self)
                  eia
                  ))
    (mapcar 'update-points (get-box-connections (object self)))
    (redraw-connections self)
    ))

(defmethod make-frame-from-callobj ((self OMBox))
  (let ((view (om-make-graphic-object (get-box-frame-class self) 
                :position (omp (box-x self) (box-y self))
                :font (text-font self)
                :object self
                :icon-id (and (get-icon-id-from-reference self)
                              (or (find (get-icon-id-from-reference self) *om-loaded-picts*)
                                         'not-found)))))
    (om-set-view-size view (om-def-point (omp (box-w self) (box-h self))
                                                (default-size self)))
    (setf (frame self) view)
    (set-frame-areas view)
    view))

(defmethod update-frame-connections-display ((self OMBoxFrame))
;  (print (mapcan 'connections (append (inputs (object self)) (outputs (object self)))))
  (mapc 
   #'(lambda (c) (update-connection-display c (om-view-container self)))
   ;;; MAPCAN hangs here I don't know why.
   ;;; on Windows at least, when there are more than 1 connections
   ;;; works better with LOOP + APPEND
   (loop for io in (append (inputs (object self)) (outputs (object self))) append (connections io))
   ))


(defmethod update-view ((self OMBoxFrame) (object OMBox))
  (when (text-font object) (om-set-font self (text-font object)))
  (let ((best-size (om-borne-point (omp (box-w object) (box-h object)) 
                                   (minimum-size object) (maximum-size object))))
    (setf (box-w object) (om-point-x best-size)
          (box-h object) (om-point-y best-size))
    
    (om-set-view-size self (omp (box-w object) (box-h object))))
  (om-invalidate-view self)
  (update-frame-connections-display self)
  )

(defmethod update-view ((self OMBoxFrame) (object OMBoxCall))
  (when (text-font object) (om-set-font self (text-font object)))
  (let ((adjusted-size (om-borne-point 
                        (omp (box-w object) (box-h object))
                        (minimum-size object) (maximum-size object))))
    
    ;; adjust the size only if the box doesn't scale according to rulers
    (unless (scale-in-x-? object) (setf (box-w object) (om-point-x adjusted-size)))
    
    (unless (scale-in-y-? object) 
      (setf (box-h object) (om-point-y adjusted-size))  
      (when (and (or (lambda-state object) (lock-state object)) (< (box-h object) 36))
        (setf (box-h object) (+ (box-h object) 8))))

    (om-set-view-size self (omp 
                            (if (scale-in-x-? object) (omg-w (om-view-container self) (box-w object)) (box-w object))
                            (if (scale-in-y-? object) (omg-h (om-view-container self) (box-h object)) (box-h object))))
    
    (om-invalidate-view self)
    (update-frame-connections-display self)
    ))

(defmethod update-to-editor ((self OMEditor) (from OMBox)) 
  ;(print (list "update" self "from BOX" from))
  (update-default-view self))


;;;===========================
;;; DRAW
;;;===========================

(defmethod box-draw ((self t) (frame OMBoxFrame)) nil)

(defmethod object-for-miniview ((self OMBox)) (get-box-value self))

(defmethod display-text-and-area ((self OMBoxFrame))
  (let ((text (and (show-name (object self)) (name (object self))))
        (icon-size (get-icon-size (object self))))
    (when text
      (let ((font (or (text-font (object self)) (om-get-font self)))
            (shift (if (equal :left (icon-pos (object self))) icon-size 0)))
        (multiple-value-bind (w h) (om-string-size text font)
          (values text
                  (case (text-align (object self))
                    (:center (+ shift (round (- (/ (- (w self) shift) 2) (/ w 2)))))
                    (:right (- (w self) w 4))
                    (otherwise (+ shift 4)))
                  (if (equal :left (icon-pos (object self))) 8 (- (h self) 10 h))
                  w h)
          )))))

(defmethod om-draw-contents ((self OMBoxFrame))
  (om-with-clip-rect self 0 0 (w self) (h self) 
    (boxframe-draw-contents self (object self))))

(defmethod draw-border ((self OMBox) x y w h style)
  (om-draw-rect x y w h :line (if (numberp style) style 1.5) :color (om-def-color :gray) :angles :round))

(defmethod box-draw-color ((self OMBox)) (color self))
(defmethod box-draw-text-color ((self OMBox)) (text-color self))

(defmethod boxframe-draw-contents ((self OMBoxFrame) (box OMBox))
  (let ((icon-size (get-icon-size box))
        (io-hspace 4))
    (om-with-fg-color (om-def-color :dark-gray)
      ;;; interior
      (when (box-draw-color box)
        (om-draw-rect 0 io-hspace (w self) (- (h self) (* 2 io-hspace)) 
                      :color (box-draw-color box)
                      :angles :round
                      :fill t))
    
      (when (selected box)
        (om-draw-rect 0  io-hspace (w self) (- (h self) (* 2 io-hspace)) 
                      :color (om-make-color-alpha (om-def-color :gray) 0.3)
                      :angles :round
                      :fill t))
    
      ;;; icon
      (or (box-draw box self)
          (when (icon-id self)
            (case (icon-pos box)
              (:left (om-draw-picture (icon-id self) :x 0 :y (- (h self) icon-size io-hspace) :w icon-size :h icon-size))
              (:top (let ((smaller (min (w self) (- (h self) icon-size io-hspace io-hspace))))
                      (om-draw-picture (icon-id self) 
                                       :x (round (- (w self) smaller) 2) 
                                       :y (* io-hspace 1.5) :w smaller :h smaller)))
              (otherwise nil))))

      ;;; name
      (om-with-clip-rect self 0 0 (w self) (- (h self) 8) 
        (multiple-value-bind (text x y w h)
            (display-text-and-area self)
          (when text
            (om-with-fg-color (box-draw-text-color box)
            (om-with-font
             (or (text-font box) (om-def-font :font1))
             ;(om-draw-rect x y w h)
             (om-draw-string (max 2 x) (+ y (om-font-size (or (text-font box) (om-get-font self)))) text nil (max 10 (- (w self) 2)))
             )))))

      ;;; border
      (when (border box) 
        (draw-border box 0 io-hspace (w self) (- (h self) (* 2 io-hspace)) (border box)))
      ))
  
  ;;; in/outs etc.
  (mapcar #'(lambda (a) (om-draw-area a)) (areas self))
   
  )


(defmethod boxframe-draw-contents ((self OMBoxFrame) (box OMBoxCall))
  (call-next-method)
  ;;; lambda button
  (when (lambda-state box)
    (om-draw-rect 0 (- (h self) 15) 11 11 ;(- (w self) 10) 4 12 11 
                  :color (om-def-color :dark-gray) :angles :round :fill t)
    (om-with-fg-color (om-def-color :white)
      (case (lambda-state box)
        (:lambda (multiple-value-bind (char font) (om-font-lambda 10)
                   (om-with-font font
                                 (om-draw-string 3 (- (h self) 6) ;(- (w self) 7) 13 
                                                 (string char)))))
        (:reference 
         (om-draw-line 5 (- (h self) 13) 5 (- (h self) 6))
         (om-draw-line 5 (- (h self) 6) 2 (- (h self) 9))
         (om-draw-line 5 (- (h self) 6) 8 (- (h self) 9)))
        (:box (om-draw-rect 2 (- (h self) 13) 7 7 :fill t))
        )
      ))
    
  ;;; lock button
  (when (lock-state box)
    (om-draw-rect 0 4 11 11 
                  :color (om-def-color :dark-gray) :angles :round :fill t)
    (om-with-fg-color (om-def-color :white)
      (om-with-font (om-def-font :font1 :size 9)
                    (om-draw-string 2 13 (if (equal (lock-state (object self)) :eval-once) "1" "X")))
      ))
  )

                                        
;;;=============================
;;; CLIC&ACTIONS 
;;;=============================

(defmethod clickable-box ((self OMBoxFrame)) t)

(defmethod allow-text-input ((self t)) nil)

(defmethod edit-area ((self OMBoxFrame) position)
  (multiple-value-bind (text x y w h)
      (display-text-and-area self)
    (if (and text (om-point-in-rect-p position x y w h)
             (allow-text-input (object self)))
        ;;; EDIT THE NAME ?
        (multiple-value-bind (edittext action)
            (allow-text-input (object self))
          (when text 
            (let* ((container-view (om-view-container self)))
              (edit-text-in-patch edittext self container-view action (omp x y) (omp w h))
              t)))
      )))


(defmethod om-view-click-handler ((self OMBoxFrame) position)
  (when (clickable-box self)
    ;;; if we're in multiple selection (SHIFT) or if the box is already selected: do not unselect all
    (editor-box-selection (editor (om-view-container self)) (object self))
    (apply-in-area self 'click-in-area position)
    ;(and (selected (object self)) (not (om-command-key-p)) (edit-area self position))
    self))

;;; handle boxframe click in multi-editor-view
(defmethod om-view-click-handler :around ((self OMBoxFrame) position)
  (declare (ignore position))
  (when (and (editor (om-view-container self))
             (container-editor (editor (om-view-container self))))
    (handle-multi-editor-click (om-view-container self) (container-editor (editor (om-view-container self)))))
  (call-next-method))
  
(defmethod om-view-cursor ((self OMBoxFrame))
  (let ((aa (active-area-at-pos self (om-mouse-position self))))
    (when aa (om-view-cursor aa))))

(defmethod click-in-area ((self frame-area) boxframe) self)

(defmethod om-view-doubleclick-handler ((self OMBoxFrame) position)
  (or ;; edit area is a simple click on a selected box
      (and (selected (object self)) (not (om-command-key-p)) (edit-area self position)) 
      (open-editor (object self))))

(defun edit-text-in-patch (edittext frame container-view action pos size)
  (let* ((box (object frame))
         (textinput (om-make-di 'text-input-item
                               :text edittext
                               :focus t
                               :fg-color (om-def-color :dark-gray)
                               :di-action #'(lambda (item) 
                                              (let ((newtext (om-dialog-item-text item)))
                                                (om-end-text-edit item)
                                                (om-remove-subviews container-view item)
                                                (when action
                                                  (funcall action box newtext))
                                                (om-set-focus container-view)
                                                (let ((newsize (om-min-point (om-max-point (om-view-size frame)
                                                                                           (default-size box))
                                                                             (maximum-size box))))
                                                       
                                                              (om-set-view-size frame newsize) 
                                                              (setf (box-w box) (om-point-x newsize)
                                                                    (box-h box) (om-point-y newsize)))
                                                     
                                                (mapcar 'update-points (get-box-connections box))
                                                (redraw-connections frame)
                                                (om-invalidate-view frame)
                                                (report-modifications (editor container-view))
                                                ))
                               :begin-edit-action #'(lambda (item)
                                                      (om-set-fg-color item (om-make-color 0.4 0 0))
                                                      )
                               :edit-action #'(lambda (item)
                                                (let ((textsize (length (om-dialog-item-text item))))
                                                  (om-set-view-size item (om-make-point (list :character (+ 1 textsize)) (h item)))
                                                  ))
                               :font (om-def-font :font1)
                               :size (om-point-mv size :y 20 :x -10)
                               :position (om-point-mv (om-add-points (om-view-position frame) pos) :x -4 :y -5)
                               )))
    (om-add-subviews container-view textinput)
    (om-set-text-focus textinput t)))
    

;;;=============================
;;; CONNECT
;;;=============================

(defmethod click-in-area ((self output-area) boxframe)
  (if (om-command-key-p) 
      (output-eval-command self)
    (start-connection boxframe self)))

(defvar *connection-handler* nil)

(defmethod start-connection ((self omboxframe) oa &optional after-fun)
  (let* ((patchpanel (om-view-container self))
         (init-pos-in-patch (om-convert-coordinates (get-position oa) self patchpanel))
         (connection (om-make-graphic-object 'drag-line :position init-pos-in-patch 
                                             :size (om-make-point 4 4))))
    (om-init-temp-graphics-motion patchpanel 
                           init-pos-in-patch
                           connection
                           :motion #'(lambda (panel pos)
                                       (setf *connection-handler* t)
                                       (om-set-view-size connection (om-subtract-points pos init-pos-in-patch))
                                       (let ((target (om-find-view-containing-point panel pos)))
                                         (if (and target (not (equal target self))) 
                                             (handle-connect-move target 
                                                                  (om-convert-coordinates pos panel target) 
                                                                  self oa)
                                           )))
                           :release #'(lambda (panel pos)
                                        (setf *connection-handler* nil)
                                        (let ((target (om-find-view-containing-point panel pos)))
                                          (when (and target (not (equal target self)))
                                              (let ((connected? (handle-connect-release target 
                                                                                        (om-convert-coordinates pos panel target) 
                                                                                        self oa patchpanel)))
                                                   (when after-fun (funcall after-fun connected?)) 
                                                   ;;; will not be called in handle-connect-release returns nil
                                                   )
                                            )))
                           )
   ))


(defparameter *last-active-frame* nil)

(defun flush-active-area ()
  (when (and *last-active-frame* (active-area *last-active-frame*))
    (om-leave-area (active-area *last-active-frame*)))
  (setf *last-active-frame* nil))

(defmethod handle-connect-move ((self t) pos originframe output) (flush-active-area))

(defmethod handle-connect-move ((self omboxframe) pos originframe output)
  (let ((aa (active-area-at-pos self pos)))
    (when (and (active-area self) (not (equal aa (active-area self))))
      (om-leave-area (active-area self)))
    (when (and aa (find aa '(input-area ++input-area) :test #'(lambda (x elt) (subtypep (type-of x) elt))))
      (setf *last-active-frame* self)
      (om-enter-area aa)
      (setf (active-area self) aa))))

(defmethod handle-connect-release ((self t) pos originframe output get-out-connected-boxes) nil)

;;; CALLED WHEN THE CONNECTION ACTION IS RELEASED
;;; returns the new connection if ok
(defmethod handle-connect-release ((self omboxframe) pos originframe output patchpanel)
  (let ((aa (active-area-at-pos self pos))
        (editor (editor patchpanel)))
    (when aa 
      (let ((new-connection
             (cond ((subtypep (type-of aa) 'input-area)
                    (io-connect (object aa) (object output) (object editor) patchpanel))
                   ((subtypep (type-of aa) '++input-area)
                    (add-next-input self)
                    (io-connect (car (last (inputs (object self))))
                                (object output) 
                                (object editor) patchpanel))
                   )))
        (when new-connection
          (report-modifications editor)
          (om-invalidate-view patchpanel)
          (redraw-connections self)
          new-connection)
        ))))

;;; tries to connect two boxes
;;; returns the new connection if OK
(defmethod io-connect ((in box-input) (out box-output) patch panel)
  (let ((new-connection (omng-make-new-connection out in)))
    (when new-connection
      (loop for c in (connections in) do 
            (omng-remove-element patch c))
      (omng-add-element patch new-connection)
      (add-connection-in-view panel new-connection)
      new-connection)))

(defmethod redraw-connections ((self OMBoxFrame))
  (let ((connections (get-box-connections (object self)))
        (ed-view (om-view-container self)))
    (when ed-view
      (mapcar #'(lambda (c) 
                  (when (graphic-connection c)
                    (setf (view (graphic-connection c)) ed-view)))
              connections))
    (let ((all-points (loop for c in connections append 
                            (append (points c)
                                    (get-draw-points c)))))
    
    (mapcar #'update-graphic-connection connections)    

    (when (and all-points (om-view-container self))
      (om-invalidate-area (om-view-container self)
                          (- (apply 'min (mapcar 'om-point-x all-points)) 4) ;;; replace with list-min etc. pb when too many points
                          (- (apply 'min (mapcar 'om-point-y all-points)) 4)
                          (+ (apply 'max (mapcar 'om-point-x all-points)) 4)
                          (+ (apply 'max (mapcar 'om-point-y all-points)) 4)
                          ))
    )))


;;;=============================
;;; SPECIAL MOVE
;;;=============================

;;; this is for move actions to apply only on boxframes
;;; position is a symbolic position
(defmethod move-frame-to-position ((self OMBoxFrame) (container-view om-view) position)
  (om-set-view-position self (om-round-point (omg-position container-view position)))
  (redraw-connections self))

;;;=============================
;;; MAGIC: AUTO CONNECT
;;;=============================
;;; Several boxes are selected: the lower box's inputs are connected
;;; with the ouher boxes' outputs
(defun auto-connect-box (list-of-boxes editor view)
  (when list-of-boxes
    (let* ((y-sorted (sort list-of-boxes '> :key 'box-y))
           (x-sorted (sort (cdr y-sorted) '< :key 'box-x))
           (lower-box (car y-sorted))
           ;(sorted-outputs (apply 'append (mapcar #'(lambda (frame) (outputs (object frame))) x-sorted)))
           (sorted-outputs (if (= 1 (length x-sorted)) (outputs (car x-sorted))
                             (mapcar #'(lambda (b) (car (outputs b))) x-sorted)))
           (patch (object editor)))
      (when x-sorted ;; more than 1 box in the game
      (loop for i in (inputs lower-box)
            for o in sorted-outputs
            ;while sorted-outputs
            do      
            (let* (;(o (pop sorted-outputs))
                   (new-connection (omng-make-new-connection o i)))
              (when new-connection
                (loop for c in (connections i) do (omng-remove-element patch c))
                (omng-add-element patch new-connection)
                (add-connection-in-view view new-connection)
                )))
      
      (mapc #'(lambda (box) (redraw-connections (frame box))) list-of-boxes)
      (report-modifications editor)
      (om-invalidate-view view)))
      t))

;;; Auto connects vertically (first in/outs) 
(defun auto-connect-seq (list-of-boxes editor view)
  (when list-of-boxes
    (let* ((y-sorted (sort list-of-boxes '> :key 'box-y))
           (patch (object editor)))
      (loop for rest on y-sorted
            while (cadr rest) do      
            (let* ((i (car (inputs (car rest))))
                   (o (car (outputs (cadr rest)))))
              (when (and i o)
                (let ((new-connection (omng-make-new-connection o i)))
                  (when new-connection
                    (loop for c in (connections i) do (omng-remove-element patch c))
                    (omng-add-element patch new-connection)
                    (add-connection-in-view view new-connection))))))
      (mapc #'(lambda (box) (redraw-connections (frame box))) list-of-boxes)
      (report-modifications editor)
      (om-invalidate-view view)))
  t)



;;;=============================
;;; EDIT INPUT OR MOVE A CONNECTION
;;;=============================

;(defmethod* test (a b &key c (d 9) e)
;  :menuins '((1 (("b1" 4) ("b2" 5)))
;             (3 (("d1" 8) ("d2" 9))))
;  :initvals '(1 2 3 8 5)
;   d)
  
(defmethod additional-box-attributes-names ((self OMBox)) nil)

(defun key-to-menu (key box input)
  (if (listp key)
      (om-make-menu-comp 
       (when key (append (if (find (car key) (additional-box-attributes-names box)) 
                             (list (om-make-menu-item "-- box attributes --" nil :enabled nil :selected nil)))
                         (loop for k in key collect (key-to-menu k box input))
                         )))
    (let ((selected (string-equal (name input) (string key))))
      ;(print (list (name input) key selected))
      (if (and selected (get-input-menu box (name input)))
          ;;; it's a keyword which has a menu also for values
          (input-values-menu (string+ ":" (string-downcase key) " [select]") box input)
        ;;; just a normal menu item
        (om-make-menu-item (string+ ":" (string-downcase key))
                         (let ((currentkey key))
                           #'(lambda () 
                               (change-keyword input currentkey)))
                         :enabled (or selected
                                   (not (find (string key)
                                              (get-keyword-inputs box) 
                                              :test 'string-equal :key 'name)))
                         :selected selected ;; will not wok if all items are enabled... ?
                         )))))

;;; Displays a menu for the keyword arguments of a function
(defun show-keywords-menu (input box view)
;; IF ALL THE ITEMS ARE ENABLED THE SELECTION DOESN'T WORK: MYSTERIOUS...
;(let ((fakeitem (when (= 1 (length (get-keyword-inputs box)))
;                    (list (om-make-menu-item "" nil :enabled nil :selected nil)))))                               
  (let ((menu (om-make-menu 
               "keyword arguments"                           
               (loop for key in (get-all-keywords box)
                     collect (key-to-menu key box input))
               ;(list (key-to-menu (get-all-keywords box) box input))
               )))
    (om-open-pop-up-menu menu view)
    ))

(defun input-values-menu (name box input)
  (om-make-menu name 
                (loop for item in (get-input-menu box (name input)) 
                      ;;(cons '("" nil) (get-input-menu box (name input)))
                      for i = 0 then (+ i 1)
                      collect (om-make-menu-item 
                               (car item)
                               (let ((val (cadr item)))
                                 #'(lambda () 
                                     (set-value input val)))
                               :enabled t
                               :selected (equal (value input) (cadr item))
                               ))))
  
;;; Displays a menu for the values of an input
(defun show-input-val-menu (input box view)
  (let ((menu (input-values-menu "input val menu" box input)))
    (om-open-pop-up-menu menu view)))
   

;;; creates a new box with the current value of an input
(defmethod popup-value-as-new-box ((self input-area) view &optional (connect t))
  (let* ((new-box (omNG-make-new-boxcall 'value
                                         (om-add-points 
                                          (om-convert-coordinates (get-position self) (frame self) view)
                                          (om-make-point -10 -40))
                                         (value (object self))))
         (frame (add-box-in-patch-editor new-box view))
         (patch (object (editor view))))
    
    (move-box new-box
                (- (om-point-x (io-position-in-patch self))
                   (om-point-x (io-position-in-patch (area (first (outputs new-box))))))
                0)

    (when connect 
      (io-connect (object self) (first (outputs new-box)) patch view))
    ))

(defmethod popup-value-as-new-box ((self output-area) view &optional (connect t))
  (let* ((box (object (frame self)))
         (pos (position (object self) (outputs box))))
    (output-value-as-new-box (if pos (nth pos (value box)) (value (object self)))
                             view 
                             (om-add-points 
                              (om-convert-coordinates (get-position self) (frame self) view)
                              (om-make-point 0 20))
                             (when connect (object self)))))

(defun output-value-as-new-box (value view pos &optional (connect-to nil))
  (let* ((new-box (make-new-box-with-instance value pos))               
         (frame (add-box-in-patch-editor new-box view)))
    (move-box new-box
              (- (om-point-x pos)
                 (om-point-x (io-position-in-patch (area (first (outputs new-box))))))
              0)
    (if connect-to
        (let ()
          (unless (inputs new-box)
            (optional-input++ new-box))
          (let ((c (omng-make-new-connection connect-to (car (inputs new-box)))))
            (omng-add-element (object (editor view)) c)
            (add-connection-in-view view c)
            (om-invalidate-view view)) ;(graphic-connection c)))
          )
      (when (inputs new-box) (setf (lock-state new-box) :locked)))
    frame))


;;; disconnects an input and reconnect somewhere else
(defun reconnect-input-connection (connection patch)
  (when (graphic-connection connection) 
    ;;; !!! the connection stays disabled even if there is no movement 
    (setf (state (graphic-connection connection)) :disabled)
    (start-connection (frame (area (from connection))) (area (from connection))
                                 #'(lambda (connected?)
                                     ;;; connected? is the new connection (if any)
                                     (if connected?
                                         (let ()
                                           (setf (style connected?) (style connection)
                                                 (color connected?) (color connection))
                                           (update-graphic-connection connected?)
                                           (omng-remove-element patch connection)
                                           )
                                       (setf (state (graphic-connection connection)) nil))
                                     ))))

(defmethod enter-input-value ((self input-area) view)
  (om-hide-tooltip view)
  (setf (temp-locked self) t)
  (let ((textinput 
         (om-make-di 'text-input-item
                     :text (format nil "~s" (value (object self)))
                     :focus t
                     :fg-color (om-def-color :dark-gray)
                     :di-action #'(lambda (item) 
                                    (let ((text (om-dialog-item-text item)))
                                      (om-end-text-edit item)
                                      (om-remove-subviews view item)
                                      (let ((val (ignore-errors (read-from-string text))))
                                        (when (quoted-form-p val) 
                                          (setf val (eval val)))
                                        (set-value (object self) val))
                                      (setf (temp-locked self) nil)
                                      (report-modifications (editor view))
                                      (om-set-focus view)))
                     :begin-edit-action #'(lambda (item)
                                            (om-set-fg-color item (om-make-color 0.4 0 0))
                                            )
                     :edit-action #'(lambda (item)
                                      (let ((textsize (length (om-dialog-item-text item))))
                                        (om-set-view-size item (om-make-point (list :character (1+ textsize)) 20))
                                        ))
                     :font (om-def-font :font1)
                     :size (om-make-point 60 30)  ; 100 12
                     :position (om-add-points 
                                (om-convert-coordinates (get-position self) (frame self) view)
                                (om-make-point -10 -12)))))  ;-10 - 34
    (om-add-subviews view textinput)
    (om-set-text-focus textinput t)
    t))


;;;=====================
;;; MAIN FUNCTION: CLICK ON INPUT
;;;=====================

(defmethod click-in-area ((self input-area) boxframe)
  (let* ((input (object self))
         (box (object boxframe))
         (c (car (connections input)))
         (editorview (om-view-container boxframe)))

    (if (subtypep (type-of input) 'box-keyword-input)

        ;;; KEYWORD INPUTS = SPECIAL
        (if (om-shift-key-p)
            (if (get-input-menu box (name input))
                (show-input-val-menu (object self) (object boxframe) editorview)
              (popup-value-as-new-box self editorview)
              )
          ;;; NO SHIFT
           (show-keywords-menu (object self) (object boxframe) editorview))
      

      ;;; ELSE (NO KEYWORD) 
      (if c
          ;;; IF CONNECTED
          (cond          
           ((om-shift-key-p) ;;; shift + click = unconnect
            (let ((patch (object (editor editorview))))
              (omng-remove-element patch c)
              (apply 'om-invalidate-area (cons editorview (graphic-area c)))))
           (t  ;;; click = reconnect
               (let ((patch (object (editor editorview))))
                 (reconnect-input-connection c patch)))
           )
        ;;; ELSE: NOT CONNECTED
        (cond 
         ((om-shift-key-p) ;;; shift + click = show value as a box
          (popup-value-as-new-box self editorview))
         ((get-input-menu box (name input)) ;;; there is a menu for this input: display
          (show-input-val-menu (object self) (object boxframe) editorview))
         ;;; click = open a text edit to enter a value
         (t (enter-input-value self editorview))
         )
        ))))


(defmethod om-get-menu-context ((self OMBoxFrame))
  (let* ((position (om-mouse-position self))
         (aa (active-area-at-pos self position)))
    (if aa (menu-in-area aa self)
      (boxframe-menu-context self))))

(defmethod menu-in-area ((self t) boxframe) nil)
(defmethod boxframe-menu-context ((self OMBoxFrame)) nil)

(defmethod menu-in-area ((self io-area) boxframe)
  (let ((io (object self))
        (editorview (om-view-container boxframe)))
    (list (list 
           (om-make-menu-item "pop & connect value" 
                              #'(lambda () (popup-value-as-new-box self editorview t))
                              :enabled (not (connections io)))
           (om-make-menu-item "disconnect" 
                              #'(lambda () 
                                  (mapc #'(lambda (c) 
                                            (omng-remove-element (object (editor editorview)) c)
                                            (apply 'om-invalidate-area (cons editorview (graphic-area c)))) (connections io)))
                              :enabled (connections io))
           (om-make-menu-item (if (reactive (object self)) "set not reactive" "set reactive")
                              #'(lambda () 
                                  (setf (reactive io) (not (reactive io)))
                                  (mapc #'(lambda (c) (apply 'om-invalidate-area (cons editorview (graphic-area c)))) (connections io))
                                  ))
           ))
    ))
       

       
;       (om-new-leafmenu "Picture" #'(lambda ()  (make-bg-pict self posi)))



