;=========================================================================
; o7: visual programming language for music composition
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
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
; Author: J. Bresson
;=========================================================================
;
; UTILS TO EXPORT DATA AS TEXT
;
;=========================================================================


(in-package :om)


(defmethod write-data ((self list) out)
  (loop for elt in self do
        (loop for p in (list! elt) 
              do (if (floatp p) (format out "~f " p) (format out "~d " p)))
        (format out "~%")))

(defmethod write-data ((self t) out)
  (format out "~A~%" self))

;;; COMPAT 
;;; DEPRECATED
(defmethod save-params ((self t) file)
  (save-as-text self file))

(defmethod* save-as-text ((self t) &optional (path "data") (type "txt"))
  :icon 908
  :initvals '(nil "data")
  :indoc '("data (list, BPF, or TextFile)" "a file location")
  :doc "Saves the data from <self> as a text file in <path>." 
  (let ((file (cond 
               ((null path) (om-choose-new-file-dialog :directory (def-save-directory) :types '("Text files" "*.txt" "All files" "*.*")))
               ((pathnamep path) path)
               ((stringp path) (if (pathname-directory path) (pathname (string+ path type)) (outfile path :type type))))))
    (if file
        (progn 
          (with-open-file (out file :direction :output :if-does-not-exist :create :if-exists :supersede)
            ;(print self)
            (write-data self out))
          file)
      (om-abort))))



