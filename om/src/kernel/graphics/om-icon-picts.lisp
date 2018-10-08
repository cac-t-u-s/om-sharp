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

;;;===================
;;; ICONS
;;;===================

(in-package :om)

(defvar *om-loaded-picts* nil)

;;; Registers all images in a folder OM resources/icon
(defun register-images (folder)
  (mapc #'(lambda (file)
              (let* ((ic (read-from-string (pathname-name file)))
                     (id (if (symbolp ic) ic (intern (format nil "~A" ic)))))
                (om-register-picture id file)
                (push id *om-loaded-picts*)))
          (om-directory folder 
                        :type *om-pict-types*
                        :directories nil
                        :recursive t))
  *om-loaded-picts*)

;;; called at startup
(defun register-om-icons ()
  (let ((resources-folder (om-resources-folder)))
    (register-images (om-relative-path '("icons") nil resources-folder))
    (register-images (om-relative-path '("di") nil resources-folder))))

