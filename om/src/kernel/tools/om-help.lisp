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

; Help patches

(in-package :om)


(defun menus-from-folder (folder)
  (append 
   (loop for sub in (sort (om-directory folder :directories t :files nil) #'string< 
                          :key #'(lambda (path) (car (last (pathname-directory path)))))
         when (menus-from-folder sub)
         collect (om-make-menu (car (last (pathname-directory sub)))
                               (menus-from-folder sub)))
   (loop for item in (sort (om-directory folder :type '("opat" "omaq")) #'string< :key #'pathname-name)
         collect  (let ((path item))           
                    (om-make-menu-item 
                     (pathname-name path)
                     #'(lambda () (open-help-patch path)))))
   ))
  
(defun get-base-help-patches-folder () 
  (let* ((folder-name "help-patches/")
         (help-folder 
          #+macosx(if (oa::om-standalone-p) 
                     (merge-pathnames folder-name (om-resources-folder))
                   (merge-pathnames folder-name (om-root-folder)))
          #-macosx(merge-pathnames folder-name (om-root-folder))
          ))
    help-folder))

(defun make-base-help-menu-items ()
  (menus-from-folder (get-base-help-patches-folder)))


(defun get-lib-help-patches-foler (lib) 
  (let ((thelib (if (stringp lib) (find-library lib) lib)))
    (when thelib
      (let ((help-folder 
             (find-if 
              #'(lambda (path) (and 
                                (member (car (last (pathname-directory path))) '("patches" "tutorials" "examples") :test #'string-equal)
                                (menus-from-folder path)))
              (om-directory (mypathname thelib) :directories t :files nil))))
        help-folder
        ))))


(defun make-lib-help-menu (lib)
  (om-make-menu lib (menus-from-folder (get-lib-help-patches-foler lib))))

;;; can be redefined for specific symbols  
(defmethod symbol-reference-patch-name ((self symbol)) (string self))


(defun get-symbol-help-patch (symb)
  (let* ((from-lib (or (and (omclass-p symb) (library (find-class symb)))
                       (and (omgenericfunction-p symb) (library (fdefinition symb)))))
         (file-list (om-directory 
                     (if from-lib 
                         (get-lib-help-patches-foler from-lib)
                       (get-base-help-patches-folder))
                     :recursive t
                     :type '("opat")
                     )))
    (find (symbol-reference-patch-name symb)
          file-list :key 'pathname-name :test 'string-equal)))
                     

(defun open-help-patch (path)
  (declare (special *om-main-window*))
  (om-print-format "Opening help-patch : ~A" (list (pathname-name path)) "HELP")
  
  (case (extension-to-doctype (pathname-type path))
    
    (:old (import-doc-from-previous-om path))
    
    (otherwise ;;; need to perform specific actions to abvoid opening it as a normal patch...
     (let ((doc (open-om-document path nil)))
       (setf (mypathname doc) nil)
       (setf (saved? doc) t)
       (update-document-path doc)  ;; for the document manager
       (update-window-name (editor doc))
       (when *om-main-window* (update-elements-tab *om-main-window*))
       doc))
    ))
    