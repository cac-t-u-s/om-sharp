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
  (let ((lib (find-library lib)))
    (when lib
      (let ((help-folder (find-if 
                          #'(lambda (path) (member (car (last (pathname-directory path))) '("patches" "tutorials" "examples") :test #'string-equal))
                          (om-directory (mypathname lib) :directories t :files nil))))
        (when help-folder
          (om-directory help-folder :type '("opat" "omp") :recursive t)
          )))))
      

(defun open-help-patch (patchpath) nil)

