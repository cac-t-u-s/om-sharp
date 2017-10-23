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
; Authors: J. Bresson
;=========================================================================

;;;==============================
;;; Manipulation of pictures using LW
;;;==============================

(export '(om-save-picture) :om-api)

(in-packahge :oa)


;;; !!! ARRAY EN BGRA
(defun image-to-arrayvector (img)
  (ensure-pict-win)
  (let* ((intimg (om-internalize-image img))
         (ia (gp::make-image-access *temp-pictlayout* intimg))
         (w (gp:image-access-width ia))
         (h (gp:image-access-height ia))
         (bgra-vector (make-array (* h w 4)
                                  :element-type '(unsigned-byte 8)))
         (color-array (make-array (list h w 4)
                                  :element-type '(unsigned-byte 8)
                                  :displaced-to bgra-vector))
         (pix nil) (color nil))
  (gp::image-access-transfer-from-image ia)
  (gp:image-access-pixels-to-bgra ia bgra-vector)
  (gp::free-image-access ia)
  color-array))


(defmethod image-to-arraylist (img)
  (ensure-pict-win)

  (let* ((intimg (om-internalize-image img))
         (ia (gp::make-image-access *temp-pictlayout* intimg))
         (w (gp:image-access-width ia))
         (h (gp:image-access-height ia))
         color-array
         (pix nil) (color nil))
    (gp::image-access-transfer-from-image ia)
    (setf color-array
          (loop for j from 0 to (- h 1) collect
                (loop for i from 0 to (- w 1) 
                      do (setf color (color::get-color-spec (color::ensure-rgb (color::unconvert-color *temp-pictlayout* (gp::image-access-pixel ia i j)))))
                      collect
                      (list (aref color 1) (aref color 2) (aref color 3) (if (> (length color) 4) (aref color 4) 1.0)))))
    (gp::free-image-access ia)
      ;(capi:destroy win)
    color-array))

(defun pix-list-to-image (color-array)   
  (ensure-pict-win)
  (capi::hide-interface *temp-pictlayout* nil)
  (let* ((img (gp::make-image *temp-pictlayout* (length (car color-array)) (length color-array) :alpha t))
         (ia (gp::make-image-access *temp-pictlayout* img)))
    (loop for line in color-array 
          for j = 0 then (+ j 1) do
          (loop for pix in line 
                for i = 0 then (+ i 1) do 
                (setf (gp::image-access-pixel ia i j) 
                      (color::convert-color *temp-pictlayout* (if (consp pix)
                                                                  (color::make-rgb (nth 0 pix) (nth 1 pix) (nth 2 pix) (nth 3 pix))
                                                                (color::make-rgb pix pix pix 1))))))
    (gp::image-access-transfer-to-image ia)
    (gp::free-image-access ia)
      ;(capi:destroy win)
    img))

;;; !!! ARRAY EN BGRA
(defun pix-array-to-image (array)   
  (ensure-pict-win)
  (let ((color-array array)
        (h (array-dimension array 0))
        (w (array-dimension array 1)))
    (when (= 2 (array-rank array))
      (setf color-array (make-array (list h w 4)
                                    :element-type '(unsigned-byte 8)))
      (dolist (i h)
        (dolist (j w)
          (let ((pixel (aref array i j)))
            (cond ((numberp pixel)
                   (setf (aref color-array i j 0) pixel)
                   (setf (aref color-array i j 1) pixel)
                   (setf (aref color-array i j 2) pixel)
                   (setf (aref color-array i j 3) 1))
                  ((consp pixel)
                   (setf (aref color-array i j 0) (car pixel))
                   (setf (aref color-array i j 1) (cadr pixel))
                   (setf (aref color-array i j 2) (caddr pixel))
                   (setf (aref color-array i j 3) (or (cadddr pixel) 1)))))))
      )
    
    (let ((bgra-vector (make-array (* h w 4)
                                   :element-type '(unsigned-byte 8)
                                   :displaced-to color-array)))
      (or (and *temp-pictwin* *temp-pictlayout*) (init-pictwin))
      (capi::hide-interface *temp-pictlayout* nil)
      (let* ((img (gp::make-image *temp-pictlayout* w h :alpha t))
             (ia (gp::make-image-access *temp-pictlayout* img)))
        (gp:image-access-pixels-from-bgra ia bgra-vector)
        (gp::image-access-transfer-to-image ia)
        (gp::free-image-access ia)
        img)
      )))

(defun om-create-picture (array)
  (cond ((arrayp array) (pix-array-to-image array))
        ((consp array) (pix-list-to-image array))
        (t nil)))

(defun om-picture-values (pict)
  (let ((w (car (capi::collect-interfaces 'om-abstract-window :screen :any :sort-by :visible)))
        (pictarray (image-to-arraylist pict)))
    (om-select-window w)
    pictarray))

(defun om-picture-array (pict)
  (let ((w (car (capi::collect-interfaces 'om-abstract-window :screen :any :sort-by :visible)))
        (pictarray (image-to-arrayvector pict)))
    (om-select-window w)
    pictarray))

(defmethod om-save-picture (pict path)
  (gp::write-external-image (om-externalize-image pict) path :if-exists :supersede)
  path)


;;; (gp::externalize-and-write-image  

(defmethod om-save-picture ((pict internal-picture) path)
  (unwind-protect
      (gp:externalize-and-write-image 
       *record-view*
       (om-internal-picture-to-pict pict *record-view*) 
       path :if-exists :supersede :errorp nil)
    (gp:free-image *record-view* image))
  (probe-file path))

