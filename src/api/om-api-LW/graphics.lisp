;=========================================================================
; OM API
; Multiplatform API for OpenMusic
; LispWorks Implementation
;=========================================================================
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed; in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Authors: J. Bresson, C. Agon
;=========================================================================

;;===========================================================================
; GRAPHIC STRUCTURES (points, colors, fonts)
;;===========================================================================

(in-package :om-api)

;;;=========================
;;; export :
;;;=========================
(export '(
          ompoint
          om-make-point omp
          om-point-p
          om-points-equal-p
          om-add-points
          om-subtract-points
          om-point-x
          om-point-y
          om-point-*
          om-point-mv
          om-point-set
          om-point-set-values-from-point
          om-max-point
          om-min-point
          om-borne-point
          om-round-point
          om-def-point

          om-make-color
          om-gray-color
          om-make-color-alpha
          om-color-p
          om-color-r
          om-color-g
          om-color-b
          om-color-a
          om-color-null-p
          om-def-color
          om-color-equal

          om-make-font
          om-font-p
          om-font-face
          om-font-size
          om-font-style
          om-def-font
          om-font-lambda
          om-font-equal
          om-list-all-fonts

          om-string-size
          om-string-wrap

          om-correct-point
          om-correct-font
          om-correct-color

          ) :om-api)


;;;=========================
;;; DUMMY VIEW
;;; THIS VIEW IS USED BY SEVERAL CAPI FUNCTIONS TO INITIALIZE GRAPHICS COMPONENTS
;;;=========================

(defvar *dummy-view* nil)

(defun init-dummy-view ()
  (let* ((pl (make-instance 'capi:pinboard-layout)))
    (capi:display (make-instance 'capi:interface
                                 :display-state :hidden
                                 :layout pl))
    (setf *dummy-view* pl)))

(om-api-add-init-fun 'init-dummy-view)

;;;=========================
;;; COMPATIBILITY
;;;=========================
(defun om-correct-color (color)
  (if (om-color-p color) color (om-def-color :gray)))

(defun om-correct-point (point)
  (cond ((om-point-p point) point)
        ((null point) point)
        ((numberp point) (om-make-point (- point (ash (ash point -16) 16)) (ash point -16)))
        ((consp point) (om-make-point (car point) (cadr point)))
        (t nil)))

(defun om-correct-font (font)
  (if (om-font-p font) font (om-def-font :font1)))

;;;=========================
;;;POINTS
;;;=========================

(defstruct ompoint (x 0) (y 0))

;;; LW facilities are allowed (e.g. NIL, (:character 4), etc.)
(defun om-make-point (x y)
  (make-ompoint :x x :y y))

(defmacro omp (x y) `(om-make-point ,x ,y))

(defmethod make-load-form ((self ompoint) &optional env)
  (declare (ignore env))
  `(make-ompoint :x ,(ompoint-x self) :y ,(ompoint-y self)))

(defmethod om-point-p ((self t)) (ompoint-p self))

;;; COMPAT
(defmethod om-point-h ((point ompoint)) (ompoint-x point))
(defmethod om-point-v ((point ompoint)) (ompoint-y point))

(defmethod om-point-x ((point ompoint))
  (ompoint-x point))

(defmethod om-point-y ((point ompoint))
  (ompoint-y point))

(defmethod om-add-points (point1 point2)
  (make-ompoint :x (+ (ompoint-x point1) (ompoint-x point2))
                :y (+ (ompoint-y point1) (ompoint-y point2))))

(defmethod om-subtract-points (point1 point2)
  (make-ompoint :x (- (ompoint-x point1) (ompoint-x point2))
                :y (- (ompoint-y point1) (ompoint-y point2))))

(defmethod om-points-equal-p (point1 point2) nil)

(defmethod om-points-equal-p ((point1 ompoint) (point2 ompoint))
  (and (= (ompoint-x point1) (ompoint-x point2))
       (= (ompoint-y point1) (ompoint-y point2))))

(defmethod om-point-* ((point ompoint) fact)
  (make-ompoint :x (* (ompoint-x point) fact)
                :y (* (om-point-y point) fact)))

(defmethod om-point-mv ((point ompoint) &key x y)
  (if x (setf (ompoint-x point) (+ (ompoint-x point) x)))
  (if y (setf (ompoint-y point) (+ (ompoint-y point) y)))
  point)

(defmethod om-point-set ((point ompoint) &key x y)
  (if x (setf (ompoint-x point) x))
  (if y (setf (ompoint-y point) y))
  point)


(defmethod om-point-set-values-from-point ((point ompoint) (from ompoint))
  (setf (ompoint-x point) (ompoint-x from))
  (setf (ompoint-y point) (ompoint-y from))
  point)

(defun max-null (a b)
  (cond ((and a b) (max a b))
        ((null a) b)
        ((null b) a)))

(defun min-null (a b)
  (cond ((and a b) (min a b))
        ((null a) b)
        ((null b) a)))

(defmethod om-max-point ((p1 ompoint) (p2 ompoint))
  (make-ompoint :x (max-null (ompoint-x p1) (ompoint-x p2))
                :y (max-null (ompoint-y p1) (ompoint-y p2))))

(defmethod om-max-point ((p1 ompoint) (p2 null)) p1)
(defmethod om-max-point ((p1 null) (p2 ompoint)) p2)

(defmethod om-min-point ((p1 ompoint) (p2 ompoint))
  (make-ompoint :x (min-null (ompoint-x p1) (ompoint-x p2))
                :y (min-null (ompoint-y p1) (ompoint-y p2))))

(defmethod om-min-point ((p1 ompoint) (p2 null)) p1)
(defmethod om-min-point ((p1 null) (p2 ompoint)) p2)

(defun om-borne-point (p pmin pmax)
  (om-min-point (om-max-point p pmin) pmax))

(defun om-round-point (p)
  (make-ompoint :x (round (ompoint-x p)) :y (round (ompoint-y p))))

(defun om-def-point (p defp)
  (make-ompoint :x (or (ompoint-x p) (ompoint-x defp)) :y (or (ompoint-y p) (ompoint-y defp))))


;;;=========================
;;;COLORS
;;;=========================

(defstruct omcolor
  (c (color:make-rgb 0 0 0)))

(defun om-make-color (r g b &optional a)
  (make-omcolor :c (color:make-rgb r g b a)))

(defun om-gray-color (val &optional a)
  (make-omcolor :c (color:make-rgb val val val a)))

(defmethod om-make-color-alpha ((color omcolor) alpha)
  (make-omcolor :c (color::color-with-alpha (omcolor-c color) alpha)))

(defmethod make-load-form ((self omcolor) &optional env)
  (declare (ignore env))
  `(make-omcolor :c ,(omcolor-c self)))

(defmethod om-color-p ((self t)) nil)
(defmethod om-color-p ((self omcolor)) t)

(defun om-color-r (color)
  (color::color-red (omcolor-c color)))

(defun om-color-g (color)
  (color::color-green (omcolor-c color)))

(defun om-color-b (color)
  (color::color-blue (omcolor-c color)))

(defun om-color-a (color)
  (color::color-alpha (omcolor-c color)))

(defun om-color-null-p (color)
  (or (null color)
      (= (color::color-alpha (omcolor-c color)) 0)))

(defun om-color-equal (c1 c2)
  (and (= (om-color-r c1) (om-color-r c2))
       (= (om-color-g c1) (om-color-g c2))
       (= (om-color-b c1) (om-color-b c2))
       (= (om-color-a c1) (om-color-a c2))))


(defun om-def-color (c)
  (case c
    (:light-gray (make-omcolor :c (color:make-rgb 0.9 0.9 0.9)))
    (:gray (make-omcolor :c (color:make-rgb 0.6 0.6 0.6)))
    (:dark-gray (make-omcolor :c (color:make-rgb 0.3 0.3 0.3)))
    (:dark-red (make-omcolor :c (color:make-rgb 0.9 0.3 0.3)))
    (:dark-blue (make-omcolor :c (color:make-rgb 0.2 0.4 0.5)))
    ;; (:window (make-omcolor :c (color::get-color-spec #+cocoa :transparent #-cocoa :gray90)))
    (:window (make-omcolor :c (color::get-color-spec #+cocoa :transparent #-cocoa :background)))
    (:selection (make-omcolor :c #+win32 (color::make-rgb 0.87058825 0.87058825 0.87058825 1)
                              #-win32 (color::make-rgb 0.5 0.5 0.5 1)))
    (:selection-inv (make-omcolor :c (color::make-rgb 0.9 0.9 0.9)))
    (:selection-a (make-omcolor :c (color::make-rgb 0.7 0.7 0.7 0.2)))
    (:toolbar-color (make-omcolor :c (color:make-rgb 0.85 0.85 0.85)))
    (:text-selection (let ((selectcolor (om-def-color :selection)))
                       (make-omcolor :c (color:make-rgb (/ (om-color-r selectcolor) 2)
                                                        (/ (om-color-g selectcolor) 2)
                                                        (/ (om-color-b selectcolor) 2)
                                                        0.7))))
    ;;; supported symbols = :black :wite :red ... :transparent
    (otherwise (make-omcolor :c (color::get-color-spec c)))
    ))

;;;=========================
;;;FONTS
;;;=========================

(defmethod om-font-p ((self t)) (gp::font-description-p self))

;; &allow-other-keys is for compatibility with OM6 patches
(defun om-make-font (face size &key (style nil) &allow-other-keys)
  (gp::make-font-description
   ;:name face   ; --> name is not portable for find-best-font process
   :family face
   :size (round size)
   :slant (if (member :italic style) :italic :roman)
   :weight (if (member :bold style) :bold :normal)
   :charset :ansi
   ))

(defun om-font-face (font)
  (gp::font-description-attribute-value font :family))

(defun om-font-size (font)
  (gp::font-description-attribute-value font :size))

(defun om-font-style (font)
  (cond ((and (equal (gp::font-description-attribute-value font :weight) :bold)
              (equal (gp::font-description-attribute-value font :slant) :italic))
         '(:bold :italic))
        ((equal (gp::font-description-attribute-value font :slant) :italic)
         '(:italic))
        ((equal (gp::font-description-attribute-value font :weight) :bold)
         '(:bold))
        (t '(:plain))))


(defun om-font-equal (f1 f2)
  (and (string-equal (om-font-face f1) (om-font-face f2))
       (= (om-font-size f2) (om-font-size f2))
       (equal (om-font-style f1) (om-font-style f2))))


(defun om-string-size (str font)
  (if str
      (multiple-value-bind (left top right bottom)
          (gp::get-string-extent
           *dummy-view* str
           (gp::find-best-font *dummy-view* font))
        (values (round (- right left)) (- bottom top)))
    (values 0 0)))

; (om-string-size "--" (om-def-font :font2))
; (om-string-wrap "azertyuiop qsdfghjklm wxcvbn" 10 (om-def-font :font2))

(defun om-string-wrap (str width font)
  (declare (special *curstream* *dummy-view*))
  (let* ((view (or *curstream* *dummy-view*))
         (w (max width (om-string-size "--" font))))
    (capi::wrap-text-for-pane
     view str
     :visible-width w
     :font (gp::find-best-font view font)
     )))


(defparameter *def-font*
  #+macosx '("Lucida Grande" (11 12 14 16 20)) ;72 ppi
  #+mswindows '("Segoe UI" (8 9 10 12 14))    ;96 ppi
  #+linux '("Liberation Sans" (9 10 11 12 15)) ;96 ppi
  )

(defparameter *mono-font*
  #+macosx '("Courier New" 12)
  #+mswindows '("Courier New" 8)
  #+linux '("Courier" 10)
  )

(defparameter *gui-font*
  #+macosx '("Lucida Grande" 13)
  #+mswindows '("Segoe UI" 8)
  #+linux '("Bistream Vera Sans" 10)
  )

(defparameter *score-font*
  '("Times New Roman" 10))

(defun om-def-font (font-id &key face size style)
  (let ((def-face (car *def-font*))
        (sizes (cadr *def-font*)))
    (let ((font
           (case font-id
             (:font1 (om-make-font def-face (nth 0 sizes)))
             (:font2 (om-make-font def-face (nth 1 sizes)))
             (:font3 (om-make-font def-face (nth 2 sizes)))
             (:font4 (om-make-font def-face (nth 3 sizes)))
             (:font1b (om-make-font def-face (nth 0 sizes) :style '(:bold)))
             (:font2b (om-make-font def-face (nth 1 sizes) :style '(:bold)))
             (:font3b (om-make-font def-face (nth 2 sizes) :style '(:bold)))
             (:font4b (om-make-font def-face (nth 3 sizes) :style '(:bold)))
             (:gui (apply #'om-make-font *gui-font*))
             (:score (apply #'om-make-font *score-font*))
             (:mono (apply #'om-make-font *mono-font*))
             (otherwise (om-make-font def-face (nth 0 sizes))))))
      (when face (setf font (gp::augment-font-description font :family face)))
      (when size (setf font (gp::augment-font-description font :size size)))
      (when style (setf font (gp::augment-font-description
                              font
                              :slant (if (member :italic style) :italic :roman)
                              :weight (if (member :bold style) :bold :normal))))
      font)))


;;; a special font / char code to write a lambda :)
(defun om-font-lambda (&optional size)
  (values (code-char 955)
          (om-make-font "Times" (or size 12))))

;;; #+win32 (gp::font-description capi-win32-lib::*win32-default-gui-font*))


(defun om-list-all-fonts ()
  (mapcar #'(lambda (font)
              (gp::font-description-attribute-value font :family))
          (gp:list-all-font-names *dummy-view*)))
