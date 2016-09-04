;-----------------------------------------------------------------
;COMMENTS 
;-----------------------------------------------------------------

(in-package :om)

(defclass OMComment (OMBox) ())

(defmethod get-properties-list ((self OMComment))
  '(("Appearance" ;;; category
               (:bgcolor "Background color" :color color)
               (:border "Border" :bool border)
               (:font "Text font" :font text-font) ;;; id text type slot-name
               (:fgcolor "Text color" :color text-color)
               ;(:align "Text align" (:left :center :right) text-align)
               )))

(defmethod omNG-make-new-comment (text pos)
  (let* ((size (omp 100 100))
         (newcomment (make-instance 'OMComment 
                                    :text-font (om-def-font :font1 :style '(:italic))
                                    :border nil :color nil :icon-pos nil
                                    :box-x (om-point-x pos) :box-y (om-point-y pos)
                                    :box-w (om-point-x size) :box-h (om-point-y size)   
                                    )))
    (setf (value newcomment) text)
    newcomment))

(defmethod om-copy ((self OMComment)) 
  (let ((newbox (call-next-method)))
    (setf (value newbox) (value self))
    newbox))

(defmethod get-box-value ((self OMComment))
  nil)


;(defun str-without-nl (str)
;  (map 'string  #'(lambda (x) (if (equal x #\Newline) #\$ x)) str))
;(defun str-with-nl (str)
;  (map 'string  #'(lambda (x) (if (equal x #\$) #\Newline  x)) str))
;(defmethod save-reference ((self OMBoxcomment))
;  (str-without-nl (str-check (reference self))))


(defclass CommentFrame (OMBoxFrame) ())
(defmethod get-box-frame-class ((self OMComment)) 'CommentFrame)

(defmethod make-frame-from-callobj ((self OMComment))
  (let ((view (om-make-graphic-object 'CommentFrame 
                :position (omp (box-x self) (box-y self))
                :size (omp (box-w self) (box-h self))
                :help "comment"
                ;:bg-color (om-def-color :white)
                :font (text-font self)
                :object self)))
    (setf (frame self) view)
    (set-frame-areas view)
    view))


(defmethod clickable-box ((self CommentFrame)) 
  (let ((container-view (om-view-container self)))
    (and (editor container-view)
         (not (bg-lock (editor container-view))))))

(defmethod set-frame-areas ((self CommentFrame))
  (setf (areas self) (resize-areas self)))

(defmethod display-text-and-area ((self CommentFrame))
  (let ((font (or (text-font (object self)) (om-get-font self)))
        (lines (om-text-to-lines (value (object self)))))
    (multiple-value-bind (w h) (om-string-size (car lines) font)
      (loop for l in (cdr lines) do (setf w (max w (om-string-size l font))))
      (values (value (object self)) 3 8 w (* h (length lines))))))

(defmethod draw-border ((self OMComment) x y w h style)  
  (om-with-line '(1 2)
    (om-draw-rect x y w h :line (if (numberp style) style 1) :color (om-def-color :gray) :angles :round)))

(defmethod enter-new-comment ((self om-view) position)
  (let ((textinput 
         (om-make-di 'om-text-edit-view
                     :text "type your comment here"
                     :fg-color (om-def-color :gray)
                     :di-action #'(lambda (item) 
                                    (let ((text (om-dialog-item-text item)))
                                      (om-end-text-edit item)
                                      (om-remove-subviews self item)
                                      (unless (or (string-equal text "type your comment here")
                                                  (<= (length text) 0))
                                        (new-comment-in-patch-editor self text position))
                                      (om-set-focus self)
                                      ))
                     :begin-edit-action #'(lambda (item) (om-set-fg-color item (om-def-color :dark-gray)))
                     ;:edit-action #'(lambda (item) (print "edit"))
                     :font (om-def-font :font1)
                     :size (om-make-point 140 60)
                     :position position
                     )))
    (om-add-subviews self textinput)
    (om-set-text-focus textinput t)
    t))

(defmethod new-comment-in-patch-editor ((self patch-editor-view) text pos)
  (let ((comment (omNG-make-new-comment text pos)))
    (add-box-in-patch-editor comment self)))

(defmethod om-view-doubleclick-handler ((self CommentFrame) position)
  (let* ((box (object self))
         (container-view (om-view-container self))
         (textinput (om-make-di 'om-text-edit-view
                               :text (value box)
                               :focus t
                               ;:fg-color (text-color box)
                               :di-action #'(lambda (item) 
                                              (let ((newtext (om-dialog-item-text item)))
                                                (om-end-text-edit item)
                                                (om-remove-subviews container-view item)
                                                (set-value box newtext)
                                                (om-set-focus container-view)
                                                ))
                               :font (text-font box)
                               :size (om-add-points (omp (box-w box) (box-h box)) (omp 4 4))
                               :position (omp (box-x box) (box-y box))
                               )))
    (om-add-subviews container-view textinput)
    (om-set-text-focus textinput t)))


    
