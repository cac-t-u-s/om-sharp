;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; OM-GRAPHIC OBJECTS DISPLAY CALLBACKS
;DocFile
;;===========================================================================


(export '(
          
          om-with-focused-view
          om-with-fg-color
          om-with-bg-color
          om-with-font
          om-with-line-size
          om-with-line
          om-with-translation
          om-with-alpha
          
          om-with-delayed-redraw
          om-with-clip-rect
          om-draw-contents
          om-draw-contents-area
          om-invalidate-view
          om-invalidate-area
          
          om-draw-string
          om-draw-line
          om-draw-lines
          om-draw-dashed-line
          om-draw-rect
          om-draw-ellipse
          om-draw-arc
          om-draw-circle
          om-draw-polygon
          om-draw
          ) :om-api)

(in-package :om-api)



;;;======================
;;; SPECIFIC OPERATIONS ON GRAPHIC STATE 
;;;======================
(defvar *curstream* nil)
(defvar *alpha* nil)

(defmacro om-with-focused-view (view &body body)
  `(if (om-item-view-p ,view)
       (let ((*curstream* (om-get-view ,view)))
         (when *curstream*
           (multiple-value-bind (*pox* *poy*) (capi::pinboard-pane-position ,view)
             ,@body)
           ))
     (let ((*curstream* (om-get-view ,view)))
       (when *curstream*
         ,@body))))

(defmacro om-with-alpha (a &body body)
  `(let ((*alpha* ,a))
     ,@body))

(defun get-real-color (omcolor)
  (when omcolor 
    (if *alpha* (color::color-with-alpha (omcolor-c omcolor) *alpha*)
      (omcolor-c omcolor))))

(defmacro om-with-fg-color (color &body body)
  `(if ,color 
       (gp::with-graphics-state (*curstream* :foreground (get-real-color ,color)) ,@body) 
     (progn ,@body)))

(defmacro om-with-bg-color (color &body body)
  `(gp::with-graphics-state (*curstream* :background (get-real-color ,color)) ,@body))

(defmacro om-with-font (font &rest body)
  `(gp::with-graphics-state (*curstream* :font (if (gp::font-description-p ,font)
                                                   (gp::find-best-font *curstream* ,font)
                                                 ,font))
     ,@body))

(defmacro om-with-line (style &body body)
  `(cond ((consp ,style)
          (gp::with-graphics-state (*curstream* :dashed t :dash  (list (car ,style) (cadr ,style))) ,@body))
         ((equal ,style :dash)
          (gp::with-graphics-state (*curstream* :dashed t :dash '(2 2)) ,@body))
         (t (progn ,@body))))

(defmacro om-with-line-size (size &body body)
  `(let ((siz #+cocoa ,size #-cocoa (max 1 (round ,size))))
     (gp::with-graphics-state (*curstream* :thickness siz
                                           :line-joint-style :miter   ; :bevel :round
                                           :line-end-style :butt)    ; :butt :projecting 
    ,@body)))

(defmacro om-with-translation (x y &body body)
  `(gp::with-graphics-transform (*curstream* 
                                 (gp::apply-translation (gp::make-transform) ,x ,y)) 
     ,@body))



;;;=====================
;;; CALLBACKS / INVALIDATE
;;;=====================

;;; do not define both of them !
(defmethod om-draw-contents ((self om-graphic-object)) t)
(defmethod om-draw-contents-area ((self om-graphic-object) x y w h)
  (om-draw-contents self))

(defmethod om-draw-contents-callback ((self t) x y w h) nil)

(defmethod om-draw-contents-callback ((self om-graphic-object) x y w h)
  (call-next-method)
  (om-with-error-handle 
    (set-graphics-port-coordinates (om-get-view self) :left 0 :top 0)
    (om-with-focused-view self
      ;(oa::om-with-clip-rect self 0 0 (vw self) (vh self)   ;;; removed from draw-contents for windows d&d...
        ;(capi::apply-in-pane-process (om-get-view self) 'om-draw-contents self))
      ;(gp::with-graphics-state ((om-get-view self) :mask (list 0 0 (vw self) (vh self)))
      (om-draw-contents-area self x y w h)
      ;(om-draw-contents self)
      
      (mapcar #'(lambda (po) 
                ;(print (list po x y w h))
                  (when (capi::pinboard-object-overlap-p po x y (+ x w) (+ y h))
                    (capi::draw-pinboard-object (om-get-view self) po
                                                :x (item-x po) :y (item-y po) :width (vw po) :height (vh po))
                    ))
              (item-subviews (om-get-view self)))
      )
    ))

;;; ONLY FOR WINDOWS
;;; draws a pinboard-object on top of the layout
;(defmethod capi::draw-pinboard-object (pane (po om-view-pinboard-object) &key x y w h)
;  (declare (ignore x y w h))
;  (capi::apply-in-pane-process pane #'(lambda (pa) 
;                                      ;(let ((posx (or (capi::get-horizontal-scroll-parameters pa :slug-position) 0))
;                                      ;      (posy (or (capi::get-vertical-scroll-parameters pa :slug-position) 0)))
;                                        ;(om-with-delayed-redraw 
;                                            (om-with-focused-view pa (om-draw-contents pa))
;                                        ;  ))
;                                      ) pane))


(defmethod om-invalidate-view ((self t)) nil)

(defmethod om-invalidate-view ((self om-graphic-object))
  (when (and (interface-visible-p self) (om-get-view self))
    (capi::with-atomic-redisplay ((om-get-view self))
      (capi::apply-in-pane-process (om-get-view self) 'internal-invalidate-view (om-get-view self)))))

(defmethod internal-invalidate-view ((self om-view))
  (gp::invalidate-rectangle (om-get-view self))
  #+windows(mapc 'internal-invalidate-view (om-subviews self)))

(defmethod om-invalidate-view ((self om-abstract-window))
  (capi::with-atomic-redisplay ((om-get-view self))
    (capi::execute-with-interface self  'internal-invalidate-view (om-get-view self))))

(defmethod internal-invalidate-view ((self om-abstract-layout))
  (mapc 'internal-invalidate-view (capi::layout-description self)))

(defmethod internal-invalidate-view ((self t))  nil)


(defmethod om-invalidate-area ((self om-graphic-object) x1 y1 x2 y2)
  (when (and (interface-visible-p self) (om-get-view self))
    (capi::with-atomic-redisplay ((om-get-view self))
      (capi::apply-in-pane-process (om-get-view self) 'gp::invalidate-rectangle (om-get-view self)
                                   (min x1 x2) (min y1 y2) (abs (- x2 x1)) (abs (- y2 y1)))
      ;#+win32(mapcar 'om-invalidate-view (om-subviews self))
      )))

(defmacro om-with-delayed-redraw (view &body body)
   `(capi:with-atomic-redisplay (,view) ,@body))

;;; nouveau.. 
(defmethod om-redraw-view ((self om-graphic-object))
  (capi:redraw-pinboard-layout (om-get-view self) 0 0 (om-width self) (om-height self) t))


;;;======================
;;; DRAW SHAPES
;;;======================

(defun format-line-style (style)
  (cond ((consp style) `(:dashed t :dash  (,(car style) ,(cadr style))))
        ((equal style :dash) '(:dashed t :dash (2 2)))
        (t '(:dashed nil))))

(defun format-graphic-args (&key fcolor bcolor line style)
  (reduce 'append
          (remove nil
                  (list (when fcolor (list :foreground (get-real-color fcolor)))
                        (when bcolor (list :background (get-real-color bcolor)))
                        (when line (list :thickness line))
                        (when style (format-line-style style))
                        ))
          ))

(defmacro om-draw-string (x y str &key selected wrap)
   `(if ,wrap
        (multiple-value-bind (left top right bottom)
            (gp::get-string-extent *curstream* ,str)
          (let ((text-list (wrap-text-for-pane *curstream* (substitute #\Space #\Tab ,str) :visible-width ,wrap))
                (text-h (- bottom top)))
            (loop for line in text-list for yy = ,y then (+ yy text-h) do
                  (gp:draw-string *curstream* line ,x yy :text-mode :default
                                  ,.(if selected '(:block t :foreground :color_highlighttext :background :color_highlight) '(:block nil))))
            ))
      (gp:draw-string *curstream* (substitute #\Space #\Tab ,str) ,x ,y :text-mode :default
                      ,.(if selected '(:block t :foreground :color_highlighttext :background :color_highlight) '(:block nil))
                      )
      ))

;; #-cocoa :operation #-cocoa (if erasable boole-eqv boole-1)
(defun om-draw-line (x1 y1 x2 y2 &key (style :round) color line)
  ;(gp:draw-line *curstream* (+ x1 0.5) (+ y1 0.5) (+ x2 0.5) (+ y2 0.5))
  (gp:draw-line *curstream* x1 y1 x2 y2 :line-end-style style ; :round or :projecting
                :foreground (get-real-color color)
                :thickness line)
  )

(defun om-draw-dashed-line (x1 y1 x2 y2)
  ;(gp:draw-line *curstream* (+ x1 0.5) (+ y1 0.5) (+ x2 0.5) (+ y2 0.5))
  (gp:draw-line *curstream* x1 y1 x2 y2 :dashed t) ;, :round or :projecting)
  )

(defun om-draw-lines (list-of-x-y)
  (gp:draw-lines *curstream* list-of-x-y))   ; (mapcar #'(lambda (v) (+ v 0.5)) list-of-x-y)

(defun convert-rect (x y w h)
  (values 
   (if (minusp w) (+ x w) x)
   (if (minusp h) (+ y h) y)
   (abs w)
   (abs h)))

(defun om-draw-rect (x y w h &key erasable angles line color fill style)
  (multiple-value-bind (xx yy ww hh) (convert-rect x y w h)
  (apply 'gp:draw-rectangle 
         (append (list *curstream* (+ xx 0.5) (+ yy 0.5) (- ww 1) (- hh 1)
               ;(+ x *pox*) (+ y *poy*) w h 
                       :filled fill
                       :line-joint-style angles     ; :bevel :miter :round
                       :thickness line :foreground (get-real-color color))
                 (format-line-style style)
                 ))))

(defun om-draw-ellipse (x y rx ry &key fill)  
  (gp:draw-ellipse *curstream* x y rx ry :filled fill))

(defun om-draw-arc (x y width height start-angle sweep-angle) 
  (gp::draw-arc *curstream* x y  width height start-angle sweep-angle))

(defun om-draw-circle (x y r &key fill) 
  (gp::draw-circle *curstream* x y r :filled fill))

;(mapc #'(lambda (x) (list (car x) (cadr x))) '((1 2 3) (4 5 6) (7 8 9)))
;(apply 'append '((1 2 3) (4 5 6) (7 8 9)))

(defun om-draw-polygon (points &key fill)  
  (gp:draw-polygon *curstream* 
                   (cond ((om-point-p (car points))
                          (mapcan #'(lambda (p) (list (om-point-x p) (om-point-y p))) points)) 
                         ((listp (car points))
                          (apply #'append points))
                         (t points))
                   :closed t :filled fill))


(defun om-draw (points &key color line style)  
  (apply 'gp:draw-polygon 
         (append 
          (list *curstream* 
                (cond ((om-point-p (car points))
                       (loop for item in points append (list (om-point-x item) (om-point-y item))))
                      ((listp (car points))
                       (loop for item in points append item))
                      (t points))
                :closed nil :filled nil
                   ;:shape-mode :plain
                )
          (format-graphic-args :fcolor color :line line :style style))))


(defmethod position-in-view ((self om-view)) (omp 0 0))

(defmacro om-with-clip-rect (view x y w h &body body)
  `(gp::with-graphics-state ((om-get-view ,view) :mask (list (+ (om-point-x (position-in-view ,view)) ,x) 
                                                             (+ (om-point-y (position-in-view ,view)) ,y) 
                                                             ,w ,h))
     ,@body))




