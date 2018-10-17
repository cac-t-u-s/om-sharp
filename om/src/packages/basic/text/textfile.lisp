;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================


(in-package :om)

(defclass* textbuffer (named-object)
  ((contents :initarg :contents :initform nil :accessor contents :documentation "data or text input/output")
   (input-mode :initform :lines-cols :accessor input-mode :documentation "determines how input <contents> is read and converted to text")
   (reader :initform nil :accessor reader))
  (:documentation
"The class TEXTBUFFER represents a text buffer in a visual program. It can be used to enter or collect data and is editable in a text window.

- <self> represents/returns the TEXTBUFFER object. 

- <contents> represents the data in the TEXTBUFFER 
As input it can be a single item (string or value) or a list, and then each item is considered as a new line in the text buffer.
As output it returns the contents of the text buffer as a list formatted according to the box :read-mode

- <input-mode> determines how input <contents> is read and formatted. The options are 
    - 'lines-cols' [default] : each item of the input list becomes a line of text, each element in the line is a 'row'
    - 'lines' : each item of the input list becomes a raw line of text
    - 'value' : the input is formatted as such in the textbuffer contents
"
))
   

;The input can be connected to a pathname to attach and fill the TextFile buffer with a file on the disk. 
;(Note: use the contextual menu in order to change the TextFile attachement settings.)

;- <read-mode> determines how <contents> access is formatted. The options are 
;    - NIL (default) : each line is read as a string (returns a list of strings)
;    - ':lines' : each line is collected in a list of values (returns a list of lists)
;    - ':lines-cols' : each line is collected a value (returns a list of lists)
;    - ':list' : ignores line breaks and returns a flat list (returns a list)
;    - ':value' : returns the contents as read by the Lisp reader
;    - ':text' : returns a single text


(defmethod additional-class-attributes ((self textbuffer)) '(input-mode))

(defmethod class-attributes-menus ((self textbuffer))
  '((input-mode (("2D matrix" :lines-cols) 
                 ("list of lines" :lines) 
                 ("plain" :value)))))
  
(defmethod additional-box-attributes ((self textbuffer)) 
  '((:read-mode "determines how <contents> text is formatted for output" 
     (("2D matrix" :lines-cols) 
      ("list of lines" :lines) 
      ("single value (lisp reader)" :value) 
      ("flat list (ignore lines)" :list)
      ("list of text lines" :text-list) 
      ("plain text" :text)))))

(defmethod object-default-edition-params ((self textbuffer))
  '((:read-mode :text)))


;;;===================================
;;; FORMATTING
(defun format-from-text-lines (lines mode)
  (case mode 
      (:lines-cols (remove nil (loop for line in lines collect (om-read-list-from-string line)))) ;; (or ... (list line))
      (:lines (remove nil (mapcar #'(lambda (l) (read-from-string l nil)) lines)))
      (:value (read-from-string (apply 'string+ (mapcar #'(lambda (line) (string+ line " ")) lines)) nil))
      (:list (flat (mapcar 'om-read-list-from-string lines) 1))
      (:text-list lines)
      ;(:text (if lines (reduce #'(lambda (s1 s2) (concatenate 'string s1 (string #\Newline) s2)) lines) ""))
      (:text (if lines (reduce #'(lambda (s1 s2) (concatenate 'string s1 (string #\Newline) s2)) lines) ""))
      ))

(defun format-to-text-lines (data mode)
  (case mode 
      (:lines-cols (loop for line in (list! data) collect (format nil "~{~a~^ ~}" (list! line))))
      (:lines (loop for line in (list! data) collect (format nil "~A" line)))
      (:text-list data)
      (otherwise (list (format nil "~A" data)))
      ))


;;;===================================
(defmethod om-init-instance ((self textbuffer) &optional initargs)
  (let ((supplied-contents (find-value-in-kv-list initargs :contents))
        (in-mode (find-value-in-kv-list initargs :input-mode)))
    ;; in-mode exists only if the input is explicitely out... (not unsed anyway...)
    (when supplied-contents 
      ;; we're evaluating the box
      (setf (contents self) (format-to-text-lines supplied-contents (input-mode self))))
    self))


(defmethod prepare-obj-for-request ((object textbuffer) (box omboxeditcall)) 
  (setf (reader object) (get-edit-param box :read-mode))
  object)

(defmethod get-slot-val ((obj textbuffer) slot-name)
  (if (string-equal (string slot-name) "contents")
      (format-from-text-lines (contents obj) (reader obj))
    (call-next-method)))

;;;===================================



(defmethod* save-as-text ((self textbuffer) &optional (path "data") (type "txt"))
   (save-as-text (format-from-text-lines (contents self) :text) path type))

(defmethod objfromobjs ((model pathname) (target textbuffer))
  (when (probe-file model)
    (om-init-instance target `((:contents ,(lines-from-file model))
                               (:input-mode :text-list)))))

(defmethod objfromobjs ((model string) (target textbuffer))
  (objfromobjs (pathname model) target))

(defmethod objfromobjs ((model null) (target textbuffer)) target)

;;;========================
;;; BOX
;;;========================

(defclass TextBufferBox (omboxeditcall) ())
(defmethod special-box-type ((self (eql 'textbuffer))) 'TextBufferBox)

(defmethod display-modes-for-object ((self textbuffer)) '(:hidden :text :mini-view))

(defmethod get-cache-display-for-text ((self textbuffer))
  (list (list :text-buffer 
              (if (contents self) 
                  (format nil "[~D lines]" (length (contents self)))
                "<EMPTY>"))
        ))
        
(defmethod draw-mini-view ((self textbuffer) (box TextBufferBox) x y w h &optional time)
  (let ((display-cache (get-display-draw box))
        (font (om-def-font :font1 :size 10)))
    (om-with-font 
     font 
     (loop for line in (list! (contents self)) 
           for y = 18 then (+ y 12) 
           do (if (< y (- h 8)) 
                  (let ((line (format nil "~A" line)))
                    (if (> (om-string-size line font) (- w 10))
                        (om-draw-string 
                         5 y 
                         (concatenate 
                          'string 
                          (subseq line 0 (min (length line) (- (round w (om-string-size "a" font)) 3)))
                          " ..."))
                      (om-draw-string 5 y line)))
                (progn 
                  (om-draw-string (- (round w 2) 10) (- h 10) "...") 
                  (return)))))))



(defmethod gen-code-for-call ((self TextBufferBox) &optional args)
  (declare (ignore args))
  `(let ((tb ,(call-next-method)))
     (setf (reader tb) ,(get-edit-param self :read-mode))
     tb))
 


;;;========================
;;; UTILS / OM FUNCTIONS
;;;========================

(defmethod* textbuffer-eval ((self textbuffer))
    :indoc '("a textfile object")
    :doc "Evaluates the contents of a TEXTBUFFER (<self>).

Evaluation allows to define functions or data in Lisp and run commands or programs from the TEXTBUFFER."
    (eval (read-from-string (apply 'string+ (append '("(progn ") (contents self) '(")"))))))

(defmethod* textbuffer-read ((self textbuffer) mode)
    :indoc '("a textfile object" "a reading mode")
    :initvals '(nil :lines-cols)
    :menuins '((1 (("value" :value) ("list" :list) ("lines" :lines) ("lines-cols" :lines-cols) ("text" :text))))
    :doc "Reads the contents of a TEXTBUFFER (<self>) as Lisp values depending on <mode>:
- :value = reads the contents as a single value
- :list = reads the successive values and returns a list 
- :lines = reads each line as a single value (returns a list)
- :lines-cols = reads each line as a list of values (returns a list of lists) 
- :text = collects the contents as a single text (string with line returns)
"
    (format-from-text-lines (contents self) mode))

;;;========================
;;; EDITOR
;;;========================

(defmethod object-has-editor ((self textbuffer)) t)
(defmethod get-editor-class ((self textbuffer)) 'textbuffer-editor)

(defclass textbuffer-editor (omeditor) ())
(defmethod editor-window-class ((self textbuffer-editor)) 'textbuffer-editor-window)

(defclass textbuffer-editor-window (om-lisp::om-text-editor-window) 
  ((editor :initarg :editor :initform nil :accessor editor)))

(defmethod om-lisp::save-operation-enabled ((self textbuffer-editor-window)) nil) 

;;; REDEFINED SOME EDITOR-WINDOW METHODS

(defmethod open-editor-window ((self textbuffer-editor))
  (if (and (window self) (om-window-open-p (window self)))
      (om-select-window (window self))
    (let* ((textbuffer (object-value self))
           (edwin (om-lisp::om-open-text-editor 
                   :contents (contents textbuffer)
                   :class 'textbuffer-editor-window
                   :lisp nil
                   :title (editor-window-title self)
                   :x (and (window-pos (object self)) (om-point-x (window-pos (object self))))
                   :y (and (window-pos (object self)) (om-point-y (window-pos (object self))))
                   :w (and (window-size (object self)) (om-point-x (window-size (object self))))
                   :h (and (window-size (object self)) (om-point-y (window-size (object self))))
                   )))
      (setf (editor edwin) self)
      (setf (window self) edwin)
      edwin)))

(defmethod om-lisp::om-text-editor-activate-callback ((self textbuffer-editor-window) activatep)
  (when (editor self)
    (editor-activate (editor self) activatep)))

(defmethod om-lisp::om-text-editor-destroy-callback ((self textbuffer-editor-window))
  (when (editor self)
    (editor-close (editor self))
    (setf (window (editor self)) nil)
    (setf (g-components (editor self)) nil)))

;(defmethod om-view-key-handler ((self textbuffer-editor-window) key)
;  (editor-key-action (editor self) key)
;  (report-modifications (editor self)))


;;; NOT CALLED !
;(defmethod om-window-resized ((self textbuffer-editor-window) size)
;  (when (editor self) ;;; sometimes the editor is not yet set (e.g. textbuffer editor)
;    (setf (window-size (object (editor self))) size))) 
;(defmethod om-window-moved ((self textbuffer-editor-window) pos)
;  (when (editor self) ;;; sometimes the editor is not yet set (e.g. textbuffer editor)
;    (setf (window-pos (object (editor self))) pos)))

;;; SPECIFIC CALLBACKS
(defmethod om-lisp::om-text-editor-modified ((self textbuffer-editor-window))
  (let ((textbuffer (object-value (editor self))))
    (when textbuffer 
      (setf (contents textbuffer) (om-lisp::om-get-text-editor-text self))))
  (when (equal self (om-front-window)) (report-modifications (editor self))))

;;; pb : this will generate the callback above and lock the box if the window is open...
(defmethod update-to-editor ((self textbuffer-editor) (from OMBoxEditCall))
  (when (window self)
    (om-lisp::om-set-text-editor-text (window self) (contents (object-value self)))))

(defmethod om-lisp::om-text-editor-resized ((win textbuffer-editor-window) w h) 
  (when (editor win)
    (setf (window-size (object (editor win))) (omp w h))))

(defmethod om-lisp::om-text-editor-moved ((win textbuffer-editor-window) x y)
  (when (editor win)
    (setf (window-pos (object (editor win))) (omp x y))))



