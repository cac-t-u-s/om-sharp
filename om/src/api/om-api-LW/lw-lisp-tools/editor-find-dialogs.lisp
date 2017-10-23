;;===========================================================================
; LW Lisp Tools 
; Lisp programming tools for LispWorks delivered applications
;;===========================================================================
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
; Authors: Jean Bresson, Sheldon Ball, Nicholas Ellis
;=========================================================================
;
; Find and Replace dialogs from S. Ball's ANVITA EDITOR
;
;===========================================================================


(in-package :om-lisp)

(defun print-capitalized (keyword)
  (substitute #\space #\- (string-capitalize keyword)))

(define-interface anvita-dialog ()
  ((ae-interface :initform nil :initarg :ae-interface :reader anvita-dialog-interface)))

(define-interface find-dialog (anvita-dialog)
  ((replace-pane :initform nil))
  (:panes
   (find-pane text-input-choice
              :visible-min-width '(:character 30)
              :items (:initarg :find-items))
   (direction-pane radio-button-panel
                   :title "Direction"
                   :title-position :frame
                   :items '(:up :down)
                   :print-function 'print-capitalized
                   :selected-item :down)
   (action-buttons push-button-panel
                   :items (:initarg :find-replace-actions)
                   :print-function 'print-capitalized
                   :default-button :find-next
                   :cancel-button :cancel
                   :selection-callback 'find-replace-callback
                   :callback-type :interface-data
                   :layout-class 'column-layout
                   :layout-args '(:uniform-size-p t)))
  (:layouts
   (find-replace-layout grid-layout
                        '("Find what:" find-pane)
                        :columns 2
                        :y-adjust :center)
   (option-layout row-layout
                  '(nil direction-pane)
                  :y-adjust :center)
   (find-replace-option-layout column-layout
                               '(find-replace-layout option-layout))
   (main-layout row-layout
                '(find-replace-option-layout action-buttons)
                :gap 10
                :x-adjust (:initarg ((:find-replace-actions-adjust
                                      find-replace-actions-adjust)
                                     :center))))
  (:default-initargs
   :layout 'main-layout
   :auto-menus nil
   :internal-border 10
   :title "Find"
   :visible-max-width t
   :visible-max-height t))

(define-interface find-replace-dialog (find-dialog)
  ()
  (:panes
   (replace-pane text-input-choice
                 :visible-min-width '(:character 30)
                 :items (:initarg :replace-items)))
  (:layouts
   (find-replace-layout grid-layout
                        '("Find what:" find-pane "Replace with:" replace-pane)
                        :columns 2
                        :y-adjust :center))
  (:default-initargs
   :title "Replace"))

(defvar *find-strings* nil)
(defvar *replace-strings* nil)

(defun find-replace-callback (self data)
  (with-slots (find-pane replace-pane direction-pane) self
    (if (eq data :cancel)
        (destroy self)
      (let* ((find-text (text-input-pane-text find-pane))
             (direction (if (eq (choice-selected-item direction-pane) :up)
                            :backward
                          :forward))
             (replace-text (and replace-pane
                                (text-input-pane-text replace-pane))))
        (pushnew find-text *find-strings* :test 'string-equal)
        (when replace-text
          (pushnew replace-text *replace-strings* :test 'string-equal))
        (let ((interface (anvita-dialog-interface self)))
          (with-slots (ep) interface
            (call-editor ep
                         (list 'do-editor-find-replace-callback
                               find-text
                               replace-text
                               direction
                               data
                               #'(lambda ()
                                   (execute-with-interface 
                                    self
                                    #'(lambda ()
                                        (display-message-for-pane self "No more ~S" find-text))))))))))))

(defun move-point-to-found (found buffer point direction)
  (let* ((mark (or (editor:buffer-mark buffer t)
                   (progn
                     (editor:set-current-mark point)
                     (editor:buffer-mark buffer)))))
    (editor:move-point mark point)
    (editor:character-offset (if (eq direction :backward)
                                 mark
                               point)
                             found)
    (editor::set-highlight-buffer-region t buffer)))

(defun do-editor-find-replace-callback (find-text replace-text direction
                                                  data limit-callback)
  (block done
    (let* ((buffer (editor:current-buffer))
           (point (editor:buffer-point buffer)))
      (when (eq data :replace-all)
        (when-let (start (looking-at-editor-string-p find-text point direction))
          (editor:move-point point start))
        (editor::query-replace-string :point point
                                      :direction direction
                                      :target find-text
                                      :replacement replace-text
                                      :do-all t)
        (return-from done))
      (when (eq data :replace)
        (when-let (start (looking-at-editor-string-p find-text point direction))
          (editor::query-replace-string :point start
                                        :direction direction
                                        :target find-text
                                        :replacement replace-text
                                        :count 1
                                        :do-all t)))
      (loop (when-let (found (find-editor-string find-text point direction))
              (move-point-to-found found buffer point direction)
              (return))
            (editor:redisplay)
            (funcall limit-callback)
            (return)))))

(defun find-editor-string (string point direction &optional limit)
  (let ((pattern (editor::get-search-pattern string direction nil)))
    (editor::find-pattern point pattern limit)))

(defun looking-at-editor-string-p (string point direction)
  (let* ((buffer (editor:point-buffer point))
         (start (editor:buffer-mark buffer t))
         (end point))
    (and start
         (editor:with-point ((seek start :temporary))
           (let ((found (find-editor-string string seek direction end)))
             (and found
                  (if (eq direction :forward)
                      (and (editor:point= seek start)
                           (progn
                             (editor:character-offset seek found)
                             (editor:point= seek end)))
                    (and (editor:point= seek end)
                         (progn
                           (editor:character-offset seek found)
                           (editor:point= seek start)))))))
         start)))

(defun find-dialog-find-next (interface)
  (find-replace-callback interface :find-next))

;;;==================
;;; ENTRY POINTS
;;;==================

(defun display-anvita-dialog (interface class &rest args)
  (display (apply 'make-instance class :ae-interface interface args)
           :owner interface))

(defun find-in-file (interface)
  (display-anvita-dialog interface 'find-dialog
                          :find-replace-actions '(:find-next :cancel)
                          :find-replace-actions-adjust :center
                          :find-items *find-strings*))

(defun replace-in-file (interface)
  (display-anvita-dialog interface 'find-replace-dialog
                          :find-replace-actions '(:find-next :replace :replace-all :cancel)
                          :find-replace-actions-adjust :center
                          :find-items *find-strings*
                          :replace-items *replace-strings*))

