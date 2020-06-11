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
; MENUS AND POPUP MENUS
;;===========================================================================

(in-package :om-api)

(export '(
                om-make-menu
                om-make-menu-comp
                om-make-menu-item
                om-set-menu-bar
                om-get-menu-bar
                
                om-pop-up-menu
                om-open-pop-up-menu
                
                om-get-menu-context
                om-popup-menu-context
                
                ) :om-api)


(defun om-set-menu-bar (window menus) 
  (capi::execute-with-interface 
   window
   #'(lambda () (setf (capi::interface-menu-bar-items window) menus))
   ))

(defun om-get-menu-bar (window) 
  (capi::interface-menu-bar-items window))

;;; Inherits: title - items
(defclass om-menu (capi::menu) ())

;;; Inherits: items
;;; Allows selection
(defclass om-menu-group (capi:menu-component) ())

;;; Inherits: title - accelerator
(defclass om-menu-item (capi::menu-item) ())

;;;Creates a new menu with the given <title> and the list of <menus>.
(defun om-make-menu (title items &key (enabled t))
  (let ((enablefun (cond ((functionp enabled) #'(lambda (item) (declare (ignore item)) (funcall enabled)))
                         ((and (symbolp enabled) (fboundp enabled)) #'(lambda (item) (declare (ignore item)) (funcall enabled)))
                         (enabled #'(lambda (item) (declare (ignore item)) t))
                         (t #'(lambda (item) (declare (ignore item)) nil)))))
        (make-instance 'om-menu 
                 :title title 
                 :callback-type :none
                 :items (list (om-make-menu-comp items :selection t))
                 :enabled-function enablefun
                 )))
                     
;Creates a new leaf menu with the given <title> and <action>.
(defun om-make-menu-item (title action &key (key nil) (key-mod :default) (enabled t) selected )
  ;(print (list enabled selected))
  (let ((enablefun (cond ((functionp enabled) #'(lambda (item) (declare (ignore item)) (funcall enabled)))
                         ((and (symbolp enabled) (fboundp enabled)) #'(lambda (item) (declare (ignore item)) (funcall enabled)))
                         (enabled #'(lambda (item) (declare (ignore item)) t))
                         (t #'(lambda (item) (declare (ignore item)) nil))))
        (selectfun (cond ((functionp selected) #'(lambda (item) (declare (ignore item)) (funcall selected)))
                         ((and (symbolp selected) (fboundp selected)) #'(lambda (item) (declare (ignore item)) (funcall selected)))
                         (selected #'(lambda (item) (declare (ignore item)) t))
                         (t #'(lambda (item) (declare (ignore item)) nil)))))
    (make-instance 'om-menu-item
                   :title title 
                   :accelerator (if (and enabled key)
				    (concatenate 'string 
						 (cond ((equal key-mod :default)
							"accelerator-")
						       (key-mod (concatenate 'string key-mod "-"))
						       (t ""))
						 key)
                                  nil)
                   :enabled-function enablefun
                   :callback-type :none
                   :selected-function selectfun
                   :callback action)
    ))

(defmethod om-make-menu-comp ((items list) &key selection)
  (make-instance 'om-menu-group
                 :items items
                 :callback-type :item
                 :interaction (if selection :multiple-selection :none)))

(defmethod om-make-menu-comp ((items function) &key selection)
  (declare (ignore selection))
  (make-instance 'om-menu-group
                 :items-function items
                 :callback-type :item
                 :interaction :none))


;;;;===================
;;;; POP UP / CONTEXT MENU
;;;;===================

(defun list-to-menu (menu)
  (if (listp menu)
      (loop for elt in menu collect 
            (if (listp elt)
                (make-instance 
                 'capi:menu-component
                 :items (list-to-menu elt)
                 :interaction :multiple-selection)
              (list-to-menu elt)
              ))
    menu))

(defmethod om-get-menu-context ((self om-graphic-object)) nil)

;;; DEFAULT BEHAVIOR
;;; right click -> open menu context
(defvar *menu-context-open* nil)

(defmethod om-context-menu-callback ((self om-graphic-object) x y)
  
  ;;; cancel d&d init
  ;;; ;;; => why ? temporarily removed because it tended to raise other windows on right clicks..
  ;(let ((win (capi::top-level-interface self)))
  ;  (capi::find-interface (type-of win) :name (capi::capi-object-name win)))
  
  (om-activate-callback self t)

  (let ((clicked (om-find-view-containing-point self (om-make-point x y))))
    ;(om-view-click-handler clicked (om-convert-coordinates (om-make-point x y) self clicked))
    (when (om-get-menu-context clicked)
      (setf *menu-context-open* t)
      (om-open-pop-up-menu (capi::make-menu-for-pane self (list-to-menu (om-get-menu-context clicked))) self)
      (setf *menu-context-open* nil))
    ))


(defmethod om-popup-menu-context ((self om-graphic-object) &optional container-view)
  (let ((themenu (capi::make-menu-for-pane self (om-get-menu-context self))))
    (om-open-pop-up-menu themenu self)))


(defmethod om-popup-menu-context ((self t) &optional container-view)
  (let ((themenu (capi::make-menu-for-pane (om-get-menu-context self) self))
        (container (if container-view container-view self)))
    (om-open-pop-up-menu themenu container)))


;;;=========================
;;; POP-UP AS AN OBJECT
;;; Explicit call for open
;;;=========================

(defun om-open-pop-up-menu (themenu self)
  (capi::display-popup-menu themenu :owner self))

