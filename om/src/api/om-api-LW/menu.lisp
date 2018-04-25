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

#|
(defmethod menu-item-title ((self om-menu)) (capi::menu-title self))
(defmethod menu-item-title ((self om-menu-group)) "")
(defmethod menu-item-title ((self om-menu-item)) (capi::item-title menuitem))


(defmethod om-find-menu-item ((menu om-menu) item)
  (let ((founditem nil))
    (loop for mitem in (capi::menu-items menu) while (not founditem) do
          (setf founditem (om-find-menu-item mitem item)))))

(defmethod om-find-menu-item ((menu om-menu-group) item)
  (let ((founditem nil))
    (loop for mitem in (capi::collection-items menu) while (not founditem) do
          (setf founditem (om-find-menu-item mitem item)))))

(defmethod om-find-menu-item ((menu om-menu-item) (item string))
  (string-equal (capi::menu-item-title menu) item))

(defmethod om-find-menu-item ((menu om-menu-item) (item om-menu-item))
  (equal menu item))
|#

;;;Creates a new menu with the given <title> and the list of <menus>.
(defun om-make-menu (title items &key (enabled t))
  (let ((enablefun (cond ((functionp enabled) #'(lambda (win) (funcall enabled)))
                         ((and (symbolp enabled) (fboundp enabled)) #'(lambda (win) (funcall enabled)))
                         (enabled #'(lambda (win) t))
                         (t #'(lambda (win) nil)))))
        (make-instance 'om-menu 
                 :title title 
                 :callback-type :none
                 :items (list (om-make-menu-comp items :selection t))
                 :enabled-function enablefun
                 )))

;(capi:contain 
; (make-instance 'capi::menu :items 
;                (list (make-instance 'capi:menu-component 
;                                     :interaction :none
;                                     :callback-type :item
;                                     :items (list 
;                                             (make-instance 'capi::menu-item :title "A"
;                                                            :enabled-function #'(lambda (x) t)
;                                                            :selected-function #'(lambda (x) nil))
;                                             (make-instance 'capi::menu-item :title "B"
;                                                            :enabled-function #'(lambda (x) t)
;                                                            :selected-function #'(lambda (x) nil))
;                                             (make-instance 'capi::menu-item :title "C" 
;                                                            :enabled-function #'(lambda (x) nil)
;                                                            :selected-function #'(lambda (x) t)))))))

                     
;Creates a new leaf menu with the given <title> and <action>.
(defun om-make-menu-item (title action &key (key nil) (key-mod :default) (enabled t) selected )
  ;(print (list enabled selected))
  (let ((enablefun (cond ((functionp enabled) #'(lambda (win) (funcall enabled)))
                         ((and (symbolp enabled) (fboundp enabled)) #'(lambda (win) (funcall enabled)))
                         (enabled #'(lambda (win) t))
                         (t #'(lambda (win) nil))))
        (selectfun (cond ((functionp selected) #'(lambda (win) (funcall selected)))
                         ((and (symbolp selected) (fboundp selected)) #'(lambda (win) (funcall selected)))
                         (selected #'(lambda (win) t))
                         (t #'(lambda (win) nil)))))
    (make-instance 'om-menu-item
                   :title title 
                   :accelerator (if key (concatenate 'string 
                                                     (case key-mod 
                                                       (:default "accelerator-")
                                                       (otherwise "alt-"))
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
  (make-instance 'om-menu-group
                 :items-function items
                 :callback-type :item
                 :interaction :none))


#|
(defun om-select-menu-item (menu item)
  (let ((select-item (om-find-menu-item menu item)))
    (when select-item
      (setf (capi::item-selected select-item) t))))

(defun om-enable-menu-item (menu-item enabled?)
  (setf (enabled menu-item)  enabled?))
|#



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

;;; pour un objet graphique
(defmethod om-popup-menu-context ((self om-graphic-object) &optional container-view)
  (let ((themenu (capi::make-menu-for-pane self (om-get-menu-context self))))
    (om-open-pop-up-menu themenu self)))

;;; pour un objet non graphique
(defmethod om-popup-menu-context ((self t) &optional container-view)
  (let ((themenu (capi::make-menu-for-pane (om-get-menu-context self) self))
        (container (if container-view container-view self)))
    (om-open-pop-up-menu themenu container)))


;;;=========================
;;; POP-UP AS AN OBJECT
;;; Explicit call for open
;;;=========================

;;; class
; (defclass om-pop-up-menu (om-standard-dialog-item capi::menu) ())

(defun om-open-pop-up-menu (themenu self)
  (capi::display-popup-menu themenu :owner self))

;(defun om-create-menu (class itemlist)
;  (make-instance class :items (list-to-menu itemlist)))

;(let* ((pane (make-instance 'capi::simple-pane))
;       (menu (capi::make-menu-for-pane 
;              pane                                       
;              (list (make-instance 'capi::menu-item :title "test"
;                                   :callback-type :none :font (om-make-font "Arial" 8)
;                                   :callback #'(lambda () (print "hello")))))))
;  (capi::contain pane)
;  (setf (capi::menu-item-font menu) (om-make-font "Arial" 8))
;  (capi::display-popup-menu menu :owner pane)  
;  )






