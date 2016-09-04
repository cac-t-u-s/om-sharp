;;===========================================================================
;LW Lisp Tools 
;Lisp programming tools for LispWorks delivered applications
;;===========================================================================

;;===========================================================================
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Jean Bresson, Sheldon Ball, Nicholas Ellis
;Initial code from S. Ball's ANVITA editor
;;===========================================================================


;;;======================================
;;; OM TEXT EDITOR
;;;======================================

(in-package :om-lisp)

(export 
 '(om-text-editor
   om-open-text-editor
   om-get-text-editor-text
   om-set-text-editor-text
   om-set-text-editor-file
   om-open-text-editor-at
   om-text-editor-modified
   om-window-class-menubar
   om-set-window-title
   om-get-window-title
   om-text-editor-destroy-callback
   om-text-editor-activate-callback)
 :om-lisp)

;;;============
;;; UTIL
;;;============

(defun echo-string (interface string)
  (with-slots (ep) interface
    (ignore-errors 
      (when (editor-window ep)
        (editor:process-character 
         (list 'editor:message string) 
         (editor-window ep))))))
  
(defun report-file-operation (interface op file)
  (when op (echo-string interface (format nil "~A file ~A" op file))))

(defun report-failed-file-operation (interface op file)
  (declare (ignore interface))
  (display-message "Failed to ~A file ~A" op file))

(defmacro with-safe-file-operation (interface path present past &body forms)
 `(restart-case
      (prog1 ,@forms (report-file-operation ,interface ,past ,path))
    (abort ()
      (report-failed-file-operation ,interface ,present ,path))))

;;;==========================
;;; MAIN CLASS 
;;;==========================

(defvar *om-text-editor-initial-xy*)
(setq *om-text-editor-initial-xy* #(100 100))
(setq *om-text-editor-count* 0)

(defclass om-text-editor (capi::interface)
  ((ep :initform nil :accessor ep :initarg :ep)
   (file :initform nil :accessor file :initarg :file)
   (lisp? :initform nil :accessor lisp? :initarg :lisp?))
  (:default-initargs
   :best-x (+ (aref *om-text-editor-initial-xy* 0) (* 15 (mod (incf *om-text-editor-count*) 10))
              (mod (* 200 (floor (/ *om-text-editor-count* 10))) 800))
   :best-y (+ (aref *om-text-editor-initial-xy* 1) (* 15 (mod *om-text-editor-count* 10)))
   :best-height 400 :best-width 500
   :external-min-width 150 :external-min-height 150
   :layout (make-instance 'capi:simple-layout)
   ;:message-area t  
   ;:create-callback 'init-text-editor
   :destroy-callback 'destroy-text-editor
   :confirm-destroy-function 'check-close-buffer
   :title "Text Editor"))

(defmethod om-set-window-title ((self om-text-editor) (title string))
  (setf (capi::interface-title self) title))

(defmethod text-edit-window-activate-callback (win activate-p)
  (when activate-p
    (setf (capi::interface-menu-bar-items win) 
          (append (internal-window-class-menubar win)
                  (om-window-class-menubar win)))))

;; used for finding windows by name
(defmethod capi::interface-match-p ((self om-text-editor) &rest initargs  &key name)
  (string-equal (capi::capi-object-name self) name))

;;; buffer has been modified and not saved
(defmethod buffer-modified-p ((self om-text-editor))
  (and (ep self)
       (editor:buffer-modified (capi::editor-pane-buffer (ep self)))))

;;; callback of the editor pane
(defmethod om-text-editor-modified ((self om-text-editor))
  (set-editor-window-title self t))

(defmethod om-text-editor-activate-callback ((self om-text-editor) activate) nil)

(defmethod set-editor-window-title ((self om-text-editor) &optional (modified nil modified-supplied-p))
  (let ((base (if (file self) (namestring (file self)) "Text Buffer"))
        (modified (if modified-supplied-p modified (buffer-modified-p self))))
    (om-set-window-title self (concatenate 'string (if modified "* " "") base))))

(defmethod lisp-operations-enabled ((self t)) t)
(defmethod lisp-operations-enabled ((self om-text-editor)) (lisp? self))

(defmethod file-operations-enabled ((self t)) t)
(defmethod file-operations-enabled ((self om-text-editor)) t)

(defmethod save-operation-enabled ((self om-text-editor)) (buffer-modified-p self))

(defmethod capi:interface-display :after ((win om-text-editor))
  (capi::execute-with-interface 
   win
   #'(lambda ()
       (when (lisp? win) 
         (call-editor (ep win) 
                      (list 'editor:lisp-mode-command (capi::editor-pane-buffer (ep win))))
         (echo-string win "")))))


(defmethod (setf file) :after (path (self om-text-editor))
  (if path
      (setf (capi::interface-title self) (namestring path))
    (setf (capi::interface-title self) "Text Buffer")))


;;;=====================
;;; CREATE-WINDOW
;;; MAIN (EXPORTED) FUNCTIONS
;;;===========================

;;; A record for open files
(defvar *editor-files-open* nil)

(defun find-open-file (path)
  (find (namestring path) *editor-files-open*
        :test 'string-equal 
        :key #'(lambda (win)
                 (namestring 
                  (or (and (ep win) (capi::editor-pane-buffer (ep win))
                           (editor::buffer-pathname (capi::editor-pane-buffer (ep win))))
                      "")))))

; (setf *editor-files-open* nil)

(defun om-open-text-editor (&key contents class (lisp t) title)
  (let* ((path (and (pathnamep contents) contents))
         (window (when path (find-open-file path))))
    (if window 
        ;;; the file is already open ! (get the window)
        (capi::find-interface (type-of window) :name (capi::capi-object-name window))      
      (progn
        (setf window (make-instance (or class 'om-text-editor) 
                                    :name (concatenate 'string "TextEditor_" (string (gensym)))
                                    :x 200 :y 200 :width 800 :height 800
                                    :title (or title (if path (namestring path) "New Text Buffer"))
                                    :parent (capi:convert-to-screen)
                                    :internal-border 5 :external-border 0
                                    :internal-min-height 200 :internal-min-width 300
                                    :display-state :normal
                                    :file path :lisp? lisp
                                    :activate-callback 'om-text-editor-activate-callback
                                    ))
        (let* ((buffer (if path (editor:find-file-buffer path) :temp)) ; (om-make-buffer)))
               (text (cond ((consp contents)
                            (format nil "~{~a~^~%~}" contents))
                           ((stringp contents) contents) 
                           (contents (format nil "~a" contents))
                           (t " ")))
               (ep (make-instance 'capi::editor-pane :echo-area t 
                                  :buffer buffer 
                                  :text text
                                  ;; :destroy-callback #'(lambda (ep) (print (capi::editor-pane-buffer ep)))
                                  :change-callback #'(lambda (pane point old-length new-length) 
                                                       (om-text-editor-modified window))
                                  :font *def-text-edit-font*)))
          (setf (capi::layout-description (capi::pane-layout window))
                (list (setf (ep window) ep))))
        (push window *editor-files-open*)
        (capi::display window)
        window))))

(defmethod om-get-text-editor-text ((self om-text-editor))
  (and (ep self) (capi::editor-pane-buffer (ep self))
       (om-buffer-lines (capi::editor-pane-buffer (ep self)))))

(defmethod om-set-text-editor-text ((self om-text-editor) (text string))
  (and (ep self) (capi::editor-pane-buffer (ep self))
       (om-buffer-set (capi::editor-pane-buffer (ep self)) text)))

(defmethod om-set-text-editor-text ((self om-text-editor) (text list))
  (and (ep self) (capi::editor-pane-buffer (ep self)) text
       (om-buffer-set (capi::editor-pane-buffer (ep self)) 
                      (reduce #'(lambda (s1 s2) (concatenate 'string s1 (string #\Newline) s2)) text))
       ))

  
;;;========================
;;; OPEN FILE
;;;========================

(defvar *last-open-directory* nil)

;;; called from the menu
(defun open-text-file ()
  (when-let (path (capi::prompt-for-file "Open File:" :owner (capi::convert-to-screen)
                                         :pathname *last-open-directory*
                                         :if-does-not-exist :error :operation :open  
                                         :filter "*.*" 
                                         :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*")))
    (setf *last-open-directory* (make-pathname :directory (pathname-directory path)))
    (om-open-text-editor :contents path)))

(defmethod import-text-from-file ((self om-text-editor) &optional path)
  (with-slots (ep) self
    (when-let* ((path (or path (prompt-for-file "Select File:" 
                                                :if-does-not-exist :error 
                                                :filter "*.*"
                                                :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
                (current (capi::editor-pane-buffer ep))
                (newbuffer (with-safe-file-operation self path "open" nil 
                             (editor:find-file-buffer path)))
                (newtext (editor::use-buffer newbuffer
                           (editor:points-to-string 
                            (editor:buffers-start newbuffer) 
                            (editor:buffers-end newbuffer)))))
      (editor::use-buffer current
        (editor::clear-buffer current)
        (editor::insert-string (editor::buffers-start current) newtext))
      (setf (editor:buffer-modified current) t)
      (editor::kill-buffer-no-confirm newbuffer))))


;;;===========================
;;; UTIL FOR SOURCE TRACKING
;;;===========================

; cherche def in path
(defun om-open-text-editor-at (path def)
  (let ((edwin (om-open-text-editor :contents path))
        (string-to-search (concatenate 'string (string (car def))
                                       "[!\*]* "
                                       (string (cadr def)))
                          ))
    ;(print string-to-search)
    (when edwin
      (capi::execute-with-interface 
       edwin
       #'(lambda () 
           (with-slots (ep) edwin
             (let ((buffer (editor-pane-buffer ep)))
               (editor::use-buffer buffer
                 (call-editor ep (list 'editor::beginning-of-buffer-cancelling-selection-command buffer))
                 (call-editor ep (list 'EDITOR::REGEXP-FORWARD-SEARCH-COMMAND buffer string-to-search))
                 ))))))
    edwin))


;;;================================
;;; CLOSE / DESTROY
;;;================================

;;; Called by the 'close' menu
(defmethod close-text-editor-window ((self om-text-editor))
  (capi::execute-with-interface self 'quit-interface self))


;;; destroy callback
(defmethod destroy-text-editor ((self om-text-editor))
  (om-text-editor-destroy-callback self)
  (when (and (ep self) (capi::editor-pane-buffer (ep self)))
    (om-kill-buffer (capi::editor-pane-buffer (ep self))))
  (setf *editor-files-open* (remove self *editor-files-open*)))

(defmethod om-text-editor-destroy-callback  ((self om-text-editor)) nil)

;;; OLD STYLE : "EXIT ANYWAY"
;(defmethod check-close-buffer ((self om-text-editor))
;   (if (save-operation-enabled self)
;       (multiple-value-bind (answer successp)
;           (capi:prompt-for-confirmation
;            (format nil "Changes on ~A were not saved. Close anyway?"
;                    (if (editor-file self) (pathname-name (editor-file self)) "this text buffer"))
;            :cancel-button nil)
;         answer)
;    t))
  
;;; SAVE-operations-enabled
;;; NEW STYLE : "SAVE BEFORE CLOSE?"
;;; check if : Buffer modified 
;;; called by the destroy callback
(defmethod check-close-buffer ((self om-text-editor))
  (if (and (om-lisp::buffer-modified-p self) 
           (om-lisp::save-operation-enabled self))
      (multiple-value-bind (answer successp)
          (capi:prompt-for-confirmation
           (format nil "Changes on ~A were not saved.~% Save before closing?"
                   (if (file self) (pathname-name (file self)) "this text buffer"))
           :cancel-button t :default-button :ok)
        (when answer
          (with-slots (ep) self
              (let ((buffer (capi::editor-pane-buffer ep)))
                (if (file self)
                    (call-editor ep (list 'editor:save-file-command buffer))
                  (or (save-as-text-file self)
                      (setf successp nil))))))
        successp)
    t))

;;; checks all not-saved text windows
;;; ask for save each time and retruns t or nil if cancel
(defun check-buffers-before-close ()
  (let ((editors (capi::collect-interfaces 'om-text-editor))
        (ok t))
    (loop for ed in editors 
          while ok do
          (when (save-operation-enabled ed)
            (capi::find-interface (type-of ed) :name (capi::capi-object-name ed))
            (setf ok (check-close-buffer ed)))
          ;(when ok (capi::destroy ed))
          )
    ok))


;;;====================
;;; SAVE
;;;====================

;;;  Utility functions
(defvar *om-text-editor-creator-code*)
(setq *om-text-editor-creator-code* "")
(defvar *om-text-editor-type-code*)
(setq *om-text-editor-type-code* "")

;;; what is is this for ? :-s
(defun set-hfs-codes (path type creator)
 #+cocoa
 (objc:with-autorelease-pool ()
   (labels ((transform-for-endianess (x)
            (if (find :little-endian *features*)
                (- 3 x)
              x))
          (code-for-string (str)
            (if (>= (length str) 4)
                (loop for i from 0 to 3
                        sum
                        (ash (char-code (aref str i)) (ash (transform-for-endianess i) 3)))
              0))
          (set-value-for-key (dict str key)
            (objc:invoke dict "setValue:forKey:"
                    (objc:invoke "NSNumber" "numberWithUnsignedInt:" (code-for-string str)) key)))
     (let ((dict (objc:invoke "NSMutableDictionary" "dictionaryWithCapacity:" 2)))
       (set-value-for-key dict type "NSFileHFSTypeCode")
       (set-value-for-key dict creator "NSFileHFSCreatorCode")
       (objc:invoke (objc:invoke "NSFileManager" "defaultManager") "changeFileAttributes:atPath:" dict (namestring path))
       (objc:invoke-into 'string dict "description"))))
)

(defun save-to-file (text path)
    (with-open-file (ss path :direction :output :if-exists :supersede)
      (write-string text ss))
    (set-hfs-codes path *om-text-editor-type-code* *om-text-editor-creator-code*))

(defmethod om-set-text-editor-file ((self om-text-editor) path)
  (setf (file self) path)
  (set-editor-window-title self nil))


;;; SAVE the current buffer to the attached pathname
;;; or save as + change if no pathname
(defmethod save-text-file ((self om-text-editor))
  (with-slots (ep) self
    (let ((current (capi::editor-pane-buffer ep)))
      (if (file self)
          ;;; SAVE
          (let ((path (editor:buffer-pathname current)))
            (with-safe-file-operation self path "save" "Saved"
              (call-editor ep (list 'editor:save-file-command current))
              (set-hfs-codes path *om-text-editor-type-code* *om-text-editor-creator-code*))
            (om-set-text-editor-file self (file self)))
        ;;; SAVE AS
        (save-as-text-file self)
        ))))

;;; SAVE AS : the current buffer to a new file
(defmethod save-as-text-file ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (capi::editor-pane-buffer ep)))
      (when-let (path (prompt-for-file "Save file as:"
                                   :pathname (file self)
                                   :if-does-not-exist :ok
                                   :filter "*.*"
                                   :if-exists :prompt
                                   :operation :save))
        (setf *last-open-directory* (make-pathname :directory (pathname-directory path)))
        (save-to-file (om-buffer-text buffer) path)
        (om-kill-buffer buffer)
        (setf (capi::editor-pane-buffer ep) (editor:find-file-buffer path))
        (setf (editor:buffer-major-mode (capi::editor-pane-buffer ep)) "Lisp")
        (om-set-text-editor-file self path)
        ))))
       

;;; REVERT : put the file contents back in the editor
(defmethod revert-text-file ((self om-text-editor))
  (if (file self)
    (with-slots (ep) self
      (let* ((current (capi::editor-pane-buffer ep))
             (path (editor:buffer-pathname current)))
        (with-safe-file-operation self path "revert" "Reverted"
          (call-editor ep (list 'editor:revert-buffer-command nil current nil)))
        ))
    (error "No file is attached to this editor"))
  (set-editor-window-title self))

;;;=====================
;;; TEXT EDIT TOOLS 
;;;=====================

(defmethod text-edit-copy ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (capi::editor-pane-buffer ep)))
      (call-editor ep (list 'editor::copy-to-cut-buffer-command buffer)))))

(defmethod text-edit-cut ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (capi::editor-pane-buffer ep)))
      (call-editor ep (list 'editor::copy-to-cut-buffer-command buffer))
      (call-editor ep (list 'editor::kill-region-command buffer)))))

(defmethod text-edit-paste ((self om-text-editor))
  (print "paste")
  (with-slots (ep) self
    (let ((buffer (capi::editor-pane-buffer ep)))
      (call-editor ep (list 'editor::insert-cut-buffer-command buffer)))))

(defmethod text-edit-undo ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (capi::editor-pane-buffer ep)))
      (call-editor ep (list 'editor::undo-command buffer)))))


(defvar *def-text-edit-font* nil)

(defmethod change-text-edit-font ((self om-text-editor))
  (with-slots (ep) self
    (setf (capi::simple-pane-font ep) 
          (setf *def-text-edit-font*
                (capi::prompt-for-font "" :font (capi::simple-pane-font ep))))))


;;; SELECT ALL BUFFER
;;; to do ge back to initial position...
(defmethod text-select-all ((self om-text-editor))
    (with-slots (ep) self
      (let ((buffer (capi::editor-pane-buffer ep)))
        (editor::use-buffer buffer
          (editor::with-point ((p (editor::buffer-point buffer)))
            (call-editor ep (list 'editor::beginning-of-buffer-cancelling-selection-command buffer))
            #+cocoa(call-editor ep (list 'editor::end-of-buffer-extending-selection-command buffer))
            #-cocoa(call-editor ep (list 'editor::end-of-buffer-modifying-selection-command buffer))
            ;(editor::goto-line buffer 4)
            ;(call-editor ep (list 'editor::goto-point-command buffer p))
            ;(editor::move-point (editor::current-point) p)
            ;(editor::set-current-mark (editor::current-point))
            ;(call-editor ep (list 'editor::goto-point-command p 3))
            )))))

;;; Utile pour la suite...
;;;
;;; EDITOR::APROPOS-COMMAND --> symbol browser
;;; EDITOR::BACKUP-FILE-COMMAND --> cree un backup sans changer le buffer
;;; EDITOR::COMMENT-REGION-COMMAND
;;; EDITOR::EVALUATE-REGION-COMMAND

;;;=====================
;;; LISP TOOLS
;;;=====================

(setf editor::*SHOW-EDITOR-EVAL-WARNING* nil)

;; :buffer or :process
(defparameter *lisp-eval-mode* :buffer)
(defvar *lisp-eval-current-editor-file*)

;;; EVAL the buffer...
(defmethod eval-lisp-buffer ((self om-text-editor))
  (with-slots (ep) self
    (setf *lisp-eval-current-editor-file* (file self))
    (if (equal *lisp-eval-mode* :buffer)
        (let ((buffer (capi::editor-pane-buffer ep)))
          (call-editor ep (list 'editor::evaluate-buffer-command buffer)))
      (eval-string-on-process (capi:editor-pane-text ep)))
    ))

(defun eval-string-on-process (forms-as-string)
 (let ((commands (concatenate 'string forms-as-string (format nil "~Ct" #\Newline))))
   (loop while (> (length commands) 0)
         do
         (handler-case
             (multiple-value-bind (form new-pos) (read-from-string commands)
               (om-lisp::om-eval-on-process form)
               (loop while (and (< new-pos (length commands)) (lw:whitespace-char-p (aref commands new-pos)))
                     do
                     (incf new-pos))
               (setf commands (subseq commands new-pos)))
           (t (condition) (progn (capi::display-message "error ~A" condition) (abort)))))))


(defmethod eval-lisp-region ((self om-text-editor))
 (with-slots (ep) self
   (let* ((buffer (capi::editor-pane-buffer ep))
          (command
           (if (editor:variable-value "Highlight Active Region" :buffer buffer)
               'editor::evaluate-region-command
             'editor::evaluate-defun-command)))
     (capi:call-editor ep (list command buffer)))))

(defmethod text-edit-abort ((self om-text-editor))
  (capi::execute-with-interface self 'abort))

(defmethod find-definition ((self om-text-editor))
 (with-slots (ep) self
    (let ((buffer (capi::editor-pane-buffer ep))
          (symbol nil))
      (editor::use-buffer buffer
        (setf symbol (editor::intern-symbol-from-string (editor::read-symbol-from-point :previous t :read-package-name t)))
        ;(print (list symbol (type-of symbol)))
        (when symbol (om-lisp::om-edit-definition symbol))
      ))))

;;; LOAD THE LISP FILE ATTACHED...
(defmethod load-lisp-file ((self om-text-editor))
  (with-slots (ep) self
    (let ((path (editor:buffer-pathname (editor-pane-buffer ep))))
      (if path
          (if (probe-file path)
              (with-safe-file-operation self path "load" "Loaded"
                (load path :print nil :verbose nil))
            (error "File ~A not found." path))
        (echo-string self (concatenate 'string "This buffer is not attached to a file."))))))


(defvar *fasl-extension* (pathname-type (cl-user::compile-file-pathname "")))

;;; LOAD ANOTHER FILE
(defmethod load-a-lisp-file ((self om-text-editor))
  (let ((filename (capi::prompt-for-file "Choose a File to Load..." 
                                         :filters (list "All files" "*.*" "Lisp File" "*.lisp" "Compiled Lisp File" 
                                                        (concatenate 'string "*." *fasl-extension*))
                                         :pathname *last-open-directory*)))
    (when filename
      (if (probe-file filename)
          (progn 
            (load filename)
            (setf *last-open-directory* (make-pathname :directory (pathname-directory filename)))
            (echo-string self (concatenate 'string "File " (namestring filename) " loaded."))
            )
        (progn 
          (beep-pane nil)
          (echo-string self (concatenate 'string "File " (namestring filename) " not found."))
          ))
      )))


;;;==========================
;;; MENUS
;;;==========================

(defun disabled (window) (declare (ignore window)) nil)

;;; text edit menubar
(defmethod internal-window-class-menubar ((self om-text-editor)) 
  (remove 
   nil
   (list 
    (make-instance 
     'capi::menu :title "File"
     :items 
     (list (make-instance 
            'capi::menu-component 
            :items (list 
                    (make-instance 'capi::menu-item :title "New..."
                                   :callback-type :none
                                   :callback 'om-open-text-editor
                                   :accelerator #\n
                                   :enabled-function 'file-operations-enabled)
                    (make-instance 'capi::menu-item :title "Open..."
                                   :callback-type :none
                                   :callback 'open-text-file
                                   :accelerator #\o
                                   :enabled-function 'file-operations-enabled)))
           (make-instance 
            'capi::menu-component 
            :items (list 
                    (make-instance 'capi::menu-item :title "Import From..."
                                   :callback-type :interface
                                   :callback 'import-text-from-file)
                    (make-instance 'capi::menu-item :title "Revert to Saved"
                                   :callback-type :interface
                                   :callback 'revert-text-file
                                   :accelerator nil
                                   :enabled-function #'(lambda (item) (file item)))))
           (make-instance 
            'capi::menu-component 
            :items (list 
                    (make-instance 'capi::menu-item :title "Save"
                                   :callback-type :interface
                                   :callback 'save-text-file
                                   :accelerator #\s
                                   :enabled-function 'save-operation-enabled)
                    (make-instance 'capi::menu-item :title "Save As..."
                                   :callback-type :interface
                                   :callback 'save-as-text-file)))
           (make-instance 'capi::menu-item :title "Close"
                          :callback-type :interface
                          :callback 'close-text-editor-window
                          :accelerator #\w)
                                       
           ))
     (make-instance 'capi::menu :title "Edit"
                    :items (list 
                            (make-instance 'capi::menu-item :title "Undo"
                                           :callback-type :interface
                                           :callback 'text-edit-undo
                                           :accelerator #\z)
                            (make-instance 
                             'capi::menu-component 
                             :items (list 
                                     (make-instance 'capi::menu-item :title "Cut"
                                                    :callback-type :interface
                                                    :callback 'text-edit-cut
                                                    :accelerator #\x)
                                     (make-instance 'capi::menu-item :title "Copy"
                                                    :callback-type :interface
                                                    :callback 'text-edit-copy
                                                    :accelerator #\c)
                                     (make-instance 'capi::menu-item :title "Paste"
                                                    :callback-type :interface
                                                    :callback 'text-edit-paste
                                                    :accelerator #\v)))
                            (make-instance 'capi::menu-item :title "Select All" 
                                           :callback 'text-select-all 
                                           :accelerator #\a
                                           :callback-type :interface)
                            (make-instance 
                             'capi::menu-component 
                             :items (list (make-instance 'capi::menu-item :title "Text Font"
                                                         :callback-type :interface
                                                         :callback 'change-text-edit-font
                                                         :accelerator nil)
                                          ))
                            (make-instance 
                             'capi::menu-component 
                             :items (list 
                                     (make-instance 'capi::menu-item :title "Find..."
                                                    :callback-type :interface
                                                    :callback 'find-in-file
                                                    :accelerator #\f)
                                     (make-instance 'capi::menu-item :title "Replace..."
                                                    :callback-type :interface
                                                    :callback 'replace-in-file
                                                    :accelerator #\r)
                                     ))
                            (make-instance 'capi::menu-item :title "Search..." 
                                           :callback 'search-files 
                                           :accelerator nil
                                           :enabled-function 'disabled
                                           :callback-type :interface)))
     (if (lisp-operations-enabled self)
         (make-instance 'capi::menu :title "Lisp"
                        :items (list 
                                (make-instance 
                                 'capi::menu-component 
                                 :items (list 
                                         (make-instance 'capi::menu-item :title "Eval All"
                                                        :callback-type :interface
                                                        :callback 'eval-lisp-buffer
                                                        :enabled-function 'lisp-operations-enabled
                                                        :accelerator #\y)
                                         (make-instance 'capi::menu-item :title "Eval Region"
                                                        :callback-type :interface
                                                        :callback 'eval-lisp-region
                                                        :enabled-function 'lisp-operations-enabled
                                                        :accelerator #\e)))
                                (make-instance 
                                 'capi::menu-component 
                                 :items (list 
                                         (make-instance 'capi::menu-item :title "Find Definition"
                                                        :callback-type :interface
                                                        :callback 'find-definition
                                                        :enabled-function 'lisp-operations-enabled
                                                        :accelerator #\.)))
                                              
                                (make-instance 
                                 'capi::menu-component 
                                 :items (list 
                                         (make-instance 'capi::menu-item :title "Load File"
                                                        :callback-type :interface
                                                        :callback 'load-lisp-file
                                                        :enabled-function 'save-operation-enabled
                                                        :accelerator nil)
                                         ))
                                #|
                          (make-instance 
                           'capi::menu-component 
                           :items (list 
                                   (make-instance 'capi::menu-item :title "Abort"
                                                  :callback-type :interface
                                                  :callback 'text-edit-abort
                                                  :enabled-function 'lisp-operations-enabled
                                                  :accelerator #\.)
                                   ))
|#
                                ))
       )
     )))



;;; additional specific app text edit menu bar items
;;; possibly redefined/extended by om-text-editor subclasses
(defmethod om-window-class-menubar ((self t)) nil)

(defmethod om-window-class-menubar ((self om-text-editor))  
  (list 
   (make-instance 
    'capi::menu 
    :title "Windows" 
    :callback-type :none
    :items (list
            (make-instance 
             'capi::menu-item 
             :title "Listener" 
             :accelerator "accelerator-L"
             :setup-callback-argument :item
             :callback-type :none
             :callback #'(lambda () 
                           (let ((win (capi::locate-interface 'om-listener)))
                             (if win (capi::raise-interface win)
                               (om-make-listener :title "OM Listener" 
                                                 :initial-lambda #'(lambda () (in-package :om))
                                                 :height 200)))))
             (make-instance 
             'capi:menu-component
             :items (mapcar 
                     #'(lambda (w) 
                         (make-instance 
                          'capi::menu-item 
                          :title (interface-title w)
                          :setup-callback-argument :item
                          :callback-type :none
                          :callback #'(lambda () (capi::find-interface 
                                                  (type-of w) 
                                                  :name (capi::capi-object-name w)))))
                     (capi::collect-interfaces 'om-text-editor))
             :callback-type :item
             :interaction :no-selection)
            )
    )))


;;;=============================================
;;; ADDITIONAL EDITOR FEATURES
;;;=============================================

;;;=============================================
;;; ARGLIST (from LW examples)
;;;=============================================
(in-package "CL-USER")

(defvar *arglist-delay* 1)
(defvar *arglist-timer* nil)

(defvar *setf-names-p* nil "when true, show the argument list for setf names too.")

(editor:defcommand "Insert Space and Show Arglist" (p)
     "Display the argument list in the echo area a while after inserting Space to the right of the function."
     "Display the argument list."
  (editor:self-insert-command p #\Space)
  (let* ((x (if *setf-names-p*
                (editor:with-point ((temp1 (editor:current-point))
                                    (temp2 (editor:current-point)))
                  (when (editor:form-offset temp1 -1)
                    (ignore-errors
                      (let ((*package* (editor::buffer-package-to-use temp1)))
                        (read-from-string 
                         (editor:points-to-string temp1 temp2))))))
              (editor:buffer-symbol-at-point (editor:current-buffer))))
         (window (editor:current-window))
         (function (find-function-for-arglist x)))
    (when (fboundp function)
      (show-arglist function 
                    (capi:top-level-interface 
                     (editor:window-text-pane window)) 
                    window))))

(defun show-arglist (function interface editor-window)
  (let ((lambdalist (function-lambda-list  function)))
    (setq *arglist-timer* 
          (mp:make-timer 'capi:execute-with-interface interface 
                         'editor:process-character 
                         (list 'editor:message (format nil "ARGS: ~A" lambdalist))
                         editor-window))
    (mp:schedule-timer-relative *arglist-timer* *arglist-delay*)
    ))

(defun find-function-for-arglist (x)
  (typecase x
    (symbol x)
    (list (unless (dotted-list-p x)
            (if (eq (length x) 1)
                (find-function-for-arglist (car x))
              (case (car x)
                ((quote function) (find-function-for-arglist (cdr x)))
                (setf (and (= (length x) 2)
                           (symbolp (second x))
                           x)))))))) 

(editor:bind-key "Insert Space and Show Arglist" #\Space :mode "Lisp") 
(editor:bind-key "Insert Space and Show Arglist" #\Space :mode "Execute") 



