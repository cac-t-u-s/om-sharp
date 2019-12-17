;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
  

;;;==================================
;;; VISUAL PROGRAMMIN WITH FILE I/O
;;; EQUIVALENT TO OM6's FILE-BOX (but much simpler!)
;;; ==================================

(defclass fstream (om-cleanup-mixin) 
  ((fpath :accessor fpath :initarg :fpath :initform nil)
   (fs :accessor fs :initarg :fs :initform nil)
   (open? :accessor open? :initarg :open? :initform nil)))

(defmethod om-cleanup ((self fstream))
  (when (open? self)
    (close (fs self))
    (setf (open? self) nil)))

(defmethod* open-file-stream (path &key (direction :io) (if-exists :supersede))
  :initvals '(nil :io :supersede)
  :indoc '("a valid pathname" "stream direction (read/write)" "behaviour if the file exists")
  :menuins '((1 (("read" :input) ("write" :output) ("read/write" :io))) 
             (2 (("rename" :rename) ("supersede" :supersede) ("overwrite" :overwrite) ("append" :append))))
  :icon :file
  :doc "Opens a file stream where other functions can read and write" 
  (make-instance 'fstream 
                 :fs (open path :direction direction :if-exists if-exists)
                 :fpath path
                 :open? t)
  )

;;; COMPAT WITH OM6 FILE-BOX: STREAM FILE ACCESS (R/W)
(defmethod streamfile (path) 
  (om-beep-msg "WARNING: STREAMFILE IS NOW DEPRECATED. SEE 'OPEN-FILE-STREAM' / 'CLOSE-FILE-STREAM'.")
  (fs (open-file-stream path)))

(defmethod (setf filetype) (type box) nil)
(defmethod (setf direction) (type box) nil)
(defmethod (setf if-ex) (type box) nil)



(defmethod* close-file-stream ((self fstream)) 
  :indoc '("a valid pathname" "stream direction (read/write)" "behaviour if the file exists")
  :icon :file
  :doc "Closes a file stream created by open-file-stream. 
Returns the file path.

Open FILE-STREAMs are automatically closed when they are not used anymore by the Common Lisp garbage collection system, however, it is recommended to close them explicitely as soon as they are not needed in order to limit the number of streams open." 
  (om-cleanup self)
  (fpath self))



;;; R/W BOXES

(defmethod* file-write-line ((line string) (stream fstream))
  :indoc '("a line to write" "a file pointer")
  :icon :write
  :doc "Writes <line> in <stream> as a new line (i.e. with a line return).

<stream> is a file pointer created by open-file-stream."
  (write-line line (fs stream)))

(defmethod* file-write-line ((line symbol) (stream fstream))
  (write-line (string line) (fs stream)))

(defmethod* file-write-line ((line number) (stream fstream))
  (write-line (format nil "~D" line) (fs stream)))

(defmethod* file-write-line ((line character) (stream fstream))
  (write-line (string line) (fs stream)))

(defmethod* file-write-line ((line t) (stream fstream))
  (om-print "Only write numbers and strings" "file-write-line"))

;;============

(defmethod* file-write ((line t) (stream fstream))
  :indoc '("something to write" "a file pointer")
  :icon :write
  :doc "Writes <line> in <stream> (with no additional line return).

<stream> is a file pointer represented created by open-file-stream."
  (om-print "Only write numbers symbols and strings" "file-write"))

(defmethod* file-write ((line string) (stream fstream))
  (write-string line (fs stream)))

(defmethod* file-write ((line symbol) (stream fstream))
  (write-string (string line) (fs stream)))

(defmethod* file-write ((line number) (stream fstream))
  (write-string (format nil "~D" line) (fs stream)))

(defmethod* file-write ((line character) (stream fstream))
  (write-char line (fs stream)))

;;============

(defmethod* file-read-line ((stream fstream))
  :indoc '("a file pointer")
  :icon :write
  :doc "Reads a line in <stream>.

<stream> is a file pointer created by open-file-stream."
  (read-line (fs stream) nil nil))

(defmethod* file-eof-p ((stream fstream))
  :indoc '("a file pointer")
  :icon :write
  :doc "Check if <stream> is at the end of the file.

<stream> is a file pointer created by open-file-stream."
  (om-stream-eof-p (fs stream)))


