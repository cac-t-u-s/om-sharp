;=========================================================================
; LW Lisp Tools
; Lisp programming tools for LispWorks delivered applications
;=========================================================================
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed; in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program. If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Author: J. Bresson
;=========================================================================

;;; LISP FORMATTING


(in-package :editor)

;;; Redefine this function from :editor, so indent works
;;; even if the region does not start with a Lisp form
;;; and doesn't stop after double comments (;;)

(defun lisp-indent-remaining-forms (cur-start form-end end syntax)
  (when t ; (eq (next-character cur-start) #\( )
    (loop
     (if (point>= form-end end)
         (return))
     ; (point-skip-x-space cur-start syntax nil)
     (point-skip-x-space cur-start syntax t t)
     (move-point form-end cur-start)
     (if (and (form-offset form-end 1)
              (point< form-end end))
         (progn
           (indent-form cur-start 0 form-end (blank-before-p cur-start))
           ; was (indent-form cur-start 0 form-end t) *** - see above
           (move-point cur-start form-end))
       (progn
         (when (blank-before-p cur-start)
           (indent-form cur-start 0 end t t))
         (return))))))


(in-package :om-lisp)


(defun om-lisp-format-buffer (buffer &key (indent t) (whitespaces t))

  (setf (editor:buffer-major-mode buffer) "LISP")

  (editor:with-point
     ((start (editor:buffers-start buffer))
      (end (editor:buffers-end buffer)))

    (when whitespaces

      (let ((line-start 0))

        (loop for l = 1 then (+ l 1)
              while (editor::character-at start line-start) do

              (let* ((line-end (loop for p = line-start then (+ p 1)
                                     for last-char = (editor::character-at start p)
                                     while (and last-char (not (equal last-char #\Newline)))
                                     finally return p))
                     (line (om-buffer-substring buffer line-start line-end))
                     (reformat nil))

                ;; (print (list "LINE:" l line-start line-end line))

                (when (find #\Tab line)
                  (print (format nil "--- Repacing some Tabs with Spaces (l. ~A)" l))
                  (setq reformat t)
                  (setq line (substitute #\Space #\Tab line)))

                (let ((last-char (or (position-if
                                      #'(lambda (c) (not (equal c '#\Space))) line :from-end t)
                                     -1)))
                  (when (< last-char (1- (length line)))
                    (print (format nil "--- Removing trailing whitespaces (l. ~A)" l))
                    (setq reformat t)
                    (setq line (subseq line 0 (1+ last-char)))))

                #|
                     ;; Too dangerous: should escape strings at least...
                     (let ((first-char (or (position-if #'(lambda (c) (not (equal c #\Space))) line) 0)))
                    (let ((double-space (search "  " (print (subseq line first-char)))))
                      (when double-space
                        (print "-- Removing multiple whitespaces (l. ~A)" l)
                        (setq reformat t)
                        (setq line (concatenate 'string (subseq line 0 (+ first-char double-space))
                                                (subseq line (+ first-char double-space 1))))
                        (loop while double-space
                              do (setf line (concatenate 'string (subseq line 0 (+ first-char double-space))
                                                         (subseq line (+ first-char double-space 1)))
                                       double-space (search "  " (subseq line first-char))))
                        )))
                     |#

                (when reformat
                  ;; Rewrite this line
                  ;; (print (list "===>" line))
                  (om-buffer-delete buffer line-start line-end)
                  (om-buffer-insert buffer line line-start)
                  )

                ;; Go to next line
                (setq line-start (+ line-start (length line) 1))
                ))

        ;;(print "Check for line end...")
        (unless (equal (editor::character-at start (- line-start 1)) #\Newline)
          (print (format nil "--- Add Newline at file ending"))
          (editor::newline (editor:buffers-end buffer)))

        ))

    (when indent
      ;; Indent the whole buffer
      (editor::lisp-indent-region-for-commands
       (editor:buffers-start buffer)
       (editor:buffers-end buffer)))

    )
  )


(defun om-lisp-format-file (path &key (indent t) (whitespaces t))

  (when (probe-file path)

    (print (format nil "Checking Lisp format: ~s" path))

    (let ((file-buffer (editor::find-file-buffer path)))
      (unwind-protect

          (progn
            (om-lisp-format-buffer file-buffer :indent indent :whitespaces whitespaces)

            (when (editor:buffer-modified file-buffer)
              (print (format nil "Writing reformatted Lisp file: ~s" path))
              (editor:save-file-command nil file-buffer)
              path))

        (unless (equal path *load-pathname*)
          (editor::kill-buffer-no-confirm file-buffer))
        ))
    ))
