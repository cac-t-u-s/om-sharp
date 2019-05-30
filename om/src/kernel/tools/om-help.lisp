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

; Help patches

(in-package :om)


(defun get-base-help-patches () 
  (let* ((folder-name "help-patches/")
         (path (if (oa::om-standalone-p) 
                   (merge-pathnames folder-name (om-resources-folder))
                 (merge-pathnames folder-name (om-root-folder)))))
    (om-directory path :type "opat")))

(defun get-lib-help-patches (lib) 
  (let ((thelib (if (stringp lib) (find-library lib) lib)))
    (when thelib
      (let ((help-folder 
             (find-if 
              #'(lambda (path) (member (car (last (pathname-directory path))) '("patches" "tutorials" "examples") :test #'string-equal))
              (om-directory (mypathname thelib) :directories t :files nil))))
        (when help-folder
          (om-directory help-folder :type '("opat" "omp") :recursive t)
          )))))
    
;;; can be redefined for specific symbols  
(defmethod symbol-reference-patch-name ((self symbol)) (string self))


(defun get-symbol-help-patch (symb)
  (let* ((from-lib (or (and (omclass-p symb) (library (find-class symb)))
                       (and (omgenericfunction-p symb) (library (fdefinition symb)))))
         (file-list (if from-lib 
                       (get-lib-help-patches from-lib)
                     (get-base-help-patches))))
    (find (symbol-reference-patch-name symb)
          file-list :key 'pathname-name :test 'string-equal)))
                     

(defun open-help-patch (path)
  
  (om-print-format "Opening help-patch : ~A" (list (pathname-name path)) "HELP")
  
  (case (extension-to-doctype (pathname-type path))
    
    (:old (import-doc-from-previous-om path))
    
    (otherwise ;;; need to perform specific actions to abvoid opening it as a normal patch...
     (let ((doc (open-om-document path)))
       (setf (mypathname doc) nil)
       (update-window-name (editor doc))
       (when *om-main-window* (update-elements-tab *om-main-window*))
       doc))
    ))
    