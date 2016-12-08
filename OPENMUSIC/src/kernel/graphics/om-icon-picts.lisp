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

(defun register-om-icons ()
  (let ((resources-folder (om-resources-folder)))
    (register-images (om-relative-path '("icons") nil resources-folder))
    (register-images (om-relative-path '("di") nil resources-folder))))

