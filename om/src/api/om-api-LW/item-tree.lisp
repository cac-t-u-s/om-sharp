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

(in-package :om-api)


(export '(om-item-tree
          om-make-tree-view
          om-clicked-item-from-tree-view) :om-api)

;;; the capi::extended-selection-tree-view may be better (?)
;;; but the simple one looks better...
(defclass om-item-tree (om-graphic-object capi:tree-view) ())

(defun om-make-tree-view (base-items &key expand-item position size bg-color item-icon icons print-item font)
  (let ((ilist (when icons 
                 (make-instance 'capi:image-list
                                :image-sets icons
                                :image-width 16
                                :image-height 16))))
    (make-instance 'om-item-tree 
                   :visible-min-width (and size (om-point-x size))
                   :visible-min-height (and size (om-point-y size))
                   :roots base-items
                   :children-function expand-item
                   :background (omcolor-c bg-color)
                   :retain-expanded-nodes t
                   :font font
                   :accepts-focus-p nil
                 ;:image-width 16
                   :checkbox-status nil
                 ;:pane-menu 'extend-tree-view-test-menu
                   :image-lists (list :normal ilist)
                   :image-function 
                   (cond 
                    ((or (functionp item-icon) 
                         (and (symbolp item-icon) (fboundp item-icon)))
                     (if icons 
                         #'(lambda (item) (position (funcall item-icon item) icons))
                       item-icon))
                     ((null item-icon) item-icon)
                     (t #'(lambda (icon) item-icon)))
                   :callback-type :interface-data
                   :print-function (or print-item #'(lambda(x) (format nil "~a"  x )))
                 ;:selection-callback #'(lambda (self item) (add-a-message self  "~&Selected item ~S" item))
                 ;:extend-callback #'(lambda (self item)(add-a-message self  "~&Extended item ~S" item))
                 ;:retract-callback  #'(lambda (self item) (add-a-message self "~&Retracted item ~S" item))
                   :action-callback 'test-extend-tree-view-action-function 
                 ;:delete-item-callback 'test-extend-tree-view-delete-callback
                   )))




(defun add-a-message (interface format-string &rest args)
  (let* ((message-pane (slot-value interface  'message-pane))
         (stream (capi:collector-pane-stream message-pane)))
    (apply 'format stream format-string args)))
    

(defmethod om-clicked-item-from-tree-view (item window) nil)

(defun test-extend-tree-view-action-function (window item)
  (om-clicked-item-from-tree-view item window))
  ;(print (list self item))
  ;(with-slots (tree) self
  ;  (capi:tree-view-update-item tree item t))
  ;(add-a-message self "~&Action item ~S" item)


;;; The undocumented interface in 6.0 for :delete-item-callback
;;; changed in 6.1, and this code show a way of coding
;;; to cope with both interfaces. It works because the items
;;; themselves are not never lists in this example. 

(defun test-extend-tree-view-delete-callback (tree item)
  (if (listp item)   ;; true since 6.1, false until 6.0
      (progn 
        (setq *extend-tree-view-test-deleted-items*
              (union *extend-tree-view-test-deleted-items* item))
        (capi:with-atomic-redisplay (tree)
          (dolist (i-item item)
            (capi:tree-view-update-item tree i-item t))))
    (progn 
      (pushnew item *extend-tree-view-test-deleted-items*)
      (capi:tree-view-update-item tree item t))))

(defun extend-tree-view-test-menu (self data x y)
  (declare (ignorable data x y))
  (in-extend-tree-view-test-menu (capi:top-level-interface self)))

(capi:define-menu in-extend-tree-view-test-menu (self)
  :menu
  (("Delete"
    :callback  #'(lambda (interface)
                   (with-slots (tree) interface
                     (test-extend-tree-view-delete-callback
                      tree (capi:choice-selected-items tree))))
    :enabled-function #'(lambda (interface)
                          (with-slots (tree) interface
                            (capi:choice-selection tree))))
   (:component
    (("Undelete all"
      :callback #'(lambda(interface)
                    (setq *extend-tree-view-test-deleted-items* nil)
                    (with-slots (tree) interface
                      ;;redo the tree
                      (setf (capi:tree-view-roots tree) (capi:tree-view-roots tree))))))))
   :callback-type :interface)