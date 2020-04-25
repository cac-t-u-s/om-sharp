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

;;;===================
;;; ICONS
;;;===================

(in-package :om)

;;; Registers all images in the folder resources/icon
;;; ALL IDENTIFIERS ARE KEYWORD SYMBOLS
(defun register-images (folder)
  (mapc #'(lambda (file)
              (let* ((ic (read-from-string (pathname-name file)))
                     (id (if (numberp ic) ic (intern-k (format nil "~A" ic)))))
                (om-register-picture id file)))
        (om-directory folder 
                      :type (om-get-know-pict-types)
                      :directories nil
                      :recursive t))
  T)

;;; called at startup
(defun register-om-icons ()
  (let ((resources-folder (om-resources-folder)))
    (register-images (om-relative-path '("icons") nil resources-folder))
    (register-images (om-relative-path '("di") nil resources-folder))))

