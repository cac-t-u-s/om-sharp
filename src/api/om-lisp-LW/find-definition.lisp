;=========================================================================
; LW Lisp Tools
; Lisp programming tools for LispWorks delivered applications
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
; Author: J. Bresson
;;========================================================================

;===========================
; find and edit source code
;===========================

(in-package :om-lisp)


(defun open-definition (def-spec)
  (let ((file (cadr def-spec)))

    (if (pathnamep file)

        (if (probe-file file)
            (om-open-text-editor-at file (car def-spec))

          (progn (beep-pane nil)
            (print (format nil "File ~A not found." file))))

      (progn (beep-pane nil)
        (print (format nil "Unknown location for symbol definition (~A)." (second (car def-spec))))))
    ))


(defun show-definitions-dialog (symb deflist)
  (let* ((w 400) (h 300)
         (win (make-instance
               'capi::interface
               :title (concatenate 'string "Definitions of " (string-upcase (string symb)))
               :name (gensym)
               :width w :height h
               :window-styles nil
               :auto-menus nil
               :menu-bar-items (list (make-instance
                                      'capi::menu
                                      :title "File"
                                      :items
                                      (list (make-instance
                                             'capi::menu-item
                                             :title "Close"
                                             :callback-type :interface
                                             :callback #'(lambda (win) (destroy win))
                                             :accelerator #\w))))
               )))

    (set-hint-table win (list :external-min-width w :external-max-width w
                              :external-min-height h :external-max-height h))

    (setf (pane-layout win)
          (make-instance
           'pinboard-layout
           :internal-border nil
           :visible-border nil
           #+cocoa :background #+cocoa :transparent
           ))

    (apply-in-pane-process
     (pane-layout win)
     (lambda ()
       (setf (capi:layout-description (pane-layout win))
             (list
              (make-instance
               'capi::list-panel
               :x 10 :y 10
               :width 370 :height 260
               :interaction :single-selection
               :retract-callback nil
               :callback-type '(:collection)
               :test-function 'string-equal
               :items (loop for item in deflist collect (format nil "~A" (car item)))
               :action-callback #'(lambda (list)
                                    (open-definition (nth (capi::choice-selection list) deflist)))
               )))))

    (capi::display win)))


;;; SPECIAL FOR OM-LISP FIND-DEFINITION
(defvar *recorded-src-root* nil)
(defvar *new-src-root* nil)

(defun om-set-source-tree-root-folder (path)
  (setq *new-src-root* path)
  (unless (member :om-deliver *features*)
    (setq *recorded-src-root* path)))


(defun om-restore-source-path (path)
  (let ((rec-root-dir (pathname-directory *recorded-src-root*))
        (path-dir (pathname-directory (translate-logical-pathname path))))   ; truename ?
    (if (and (>= (length path-dir) (length rec-root-dir))
             (equal rec-root-dir (butlast path-dir (- (length path-dir) (length rec-root-dir)))))
        ;;; => path is recorded in the original rec-root-dir
        (merge-pathnames (make-pathname :name (pathname-name path)
                                        :type (pathname-type path)
                                        :directory (append (pathname-directory *new-src-root*)
                                                           (nthcdr (length rec-root-dir) path-dir))) *new-src-root*)
      path)))


(defun restore-definitions-pathnames (def-list)
  (loop for def in def-list collect
        (list (car def)
              (if (and (or (stringp (cadr def)) (pathnamep (cadr def))))
                  (let ((restored (om-restore-source-path (cadr def))))
                    (if (and (pathnamep restored) (probe-file restored))
                        (truename restored)
                      (cadr def)))
                (cadr def)))))


(defun om-edit-definition (symb)

  (if (symbolp symb)

      (let ((definitions
             (restore-definitions-pathnames
              (catch 'dspec-err
                (handler-bind ((error #'(lambda (err)
                                          (print (format nil "Error retrieving symbol definition: ~A" err))
                                          (throw 'dspec-err nil))))
                  (dspec:find-name-locations dspec:*dspec-classes* symb))))))

        (if definitions

            (if (= (length definitions) 1)
                (open-definition (car definitions))
              (show-definitions-dialog symb definitions))

          (progn (beep-pane nil)
            (print (format nil "No definition found for ~A." symb)))))

    (progn (beep-pane nil)
      (print (format nil "Error: ~A is not a valid symbol." symb)))))


; (om-edit-definition 'om::om+)
; (setf dspec::*active-finders* (list :internal dspec::*active-finders*))
; (dspec:find-name-locations dspec:*dspec-classes* 'om::om+)
; (dspec:find-name-locations dspec:*dspec-classes* 'om::bpf)
; (dspec:name-definition-locations dspec:*dspec-classes* 'om::om+)
