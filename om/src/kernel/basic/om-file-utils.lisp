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

;;;=========
; USER FILES 
;;;=========

(defun default-folder (name &optional reference-path)
  (let ((ref-path (or reference-path
                      (and *current-workspace* (mypathname *current-workspace*))
                      (om-make-pathname :directory (append (pathname-directory (om-user-home)) '("OM"))
                                        :host (pathname-host (om-user-home)))
                      )))
    (check-folder (om-make-pathname :device (pathname-device ref-path) :host (pathname-host ref-path)
                                    :directory (append (pathname-directory ref-path) (list name))
                                    :host (pathname-host ref-path)))))


(defun def-input-folder () (default-folder "in-files"))
(defun def-output-folder () (default-folder "out-files"))
(defun def-temp-folder () (default-folder "temp-files"))

(add-preference-module :files "Files and folders")

(add-preference-section :files "Default Folders" "Used by the functions infile/outfile/tmpfile and as default path by the file chooser dialog")
(add-preference :files :in-file "Input files" :folder 'def-input-folder)
(add-preference :files :out-file "Output files" :folder 'def-output-folder)
(add-preference :files :tmp-file "Temporary files" :folder 'def-temp-folder)

(add-preference-section :files "Misc.")
(add-preference :files :delete-tmp-files "Auto-cleanup temporary files" :bool nil)
(add-preference :files :file-exists-ation "If Output File Exists..." '(replace auto-rename) 'replace)

(add-preference-section :files "Search path" 
                        '("= where to find embedded patch abstractions when loading patches." "Empty/NIL means pathname search is relative to the top-level patch."))
(add-preference :files :search-path "Main search folder" :folder nil)
(add-preference :files :search-path-rec "Recursive" :bool t "(= also search in sub-folders)")


;;;===================================
;;; GENERATE PATHNAMES
;;;===================================
(defmethod* infile ((name string) &key (subdirs nil) (type nil))
  :icon :folder
  :indoc '("file name" "directories" "type extension")
  :initvals '("" nil nil)
  :doc "Returns a file pathname corresponding to <name> in the default OM IN FILES directory.

The IN FILES directory can be set in the OM Preferences. It is used as a default location to read files in OM.

<subdirs> is a list of strings corresponding to IN FILES subdirectories.
<type> is a type extension to append to the filename. If not specified, the type of <name> is used.

Ex. (infile \"myfile.midi\") ==> #P\"/Users/bresson/om-infiles/myfile.midi\"
Ex. (infile \"myfile.midi\" :subdirs '(\"folder1\" \"folder2\") ==> #P\"/Users/bresson/om-infiles/folder1/folder2/myfile.midi\"
"
  (om-make-pathname :directory (append (pathname-directory (get-pref-value :files :in-file))
                                       (list! subdirs))
                    :host (and (get-pref-value :files :in-file) (pathname-host (get-pref-value :files :in-file)))
                    :name (pathname-name name) :type (or type (pathname-type name))))
  
(defmethod* infile ((name null) &key (subdirs nil) (type nil))
  (om-make-pathname :directory (append (pathname-directory (get-pref-value :files :in-file)) (list! subdirs))
                    :host (pathname-host (get-pref-value :files :in-file))))

;;;===================================
(defmethod* outfile ((name string) &key (subdirs nil) (type nil))
  :icon :folder
  :indoc '("file name" "directories" "type extension")
  :initvals '("" nil nil)
  :doc "Returns a file pathname corresponding to <name> in the default OM OUT FILES directory.

The OUT FILES directory can be set in the OM Preferences. It is used as a default location to write files in OM.

<subdirs> is a list of strings corresponding to INFILES subdirectories.
<type> is a type extension to append to the filename. If not specified, the type of <name> is used.

Ex. (outfile \"myfile.midi\") ==> #P\"/Users/bresson/om-outfiles/myfile.midi\"
Ex. (outfile \"myfile.midi\" :subdirs '(\"folder1\" \"folder2\") ==> #P\"/Users/bresson/om-outfiles/folder1/folder2/myfile.midi\"
"
  (om-make-pathname :directory (append (pathname-directory (get-pref-value :files :out-file)) (list! subdirs))
                 :host (pathname-host (get-pref-value :files :out-file))
                 :name (pathname-name name) :type (or type (pathname-type name))))

(defmethod* outfile ((name null) &key (subdirs nil) (type nil))
  (om-make-pathname :directory (append (pathname-directory (get-pref-value :files :out-file)) (list! subdirs))
                    :host (pathname-host (get-pref-value :files :out-file))))


;;;===================================
(defmethod* tmpfile ((name string) &key (subdirs nil) (type nil))
  :icon :folder
  :indoc '("file name" "directories" "type extension")
  :initvals '("" nil nil)
  :doc "Returns a file pathname corresponding to <name> in the default OM TMP FILES directory.

The TMP FILES directory can be set in the OM Preferences. It is used as a default location to write temporary files in OM.

<subdirs> is a list of strings corresponding to TMP FILES subdirectories.
<type> is a type extension to append to the filename. If not specified, the type of <name> is used.

Ex. (tmpfile \"myfile.midi\") ==> #P\"/Users/bresson/om-tmpfiles/myfile.midi\"
Ex. (tmpfile \"myfile.midi\" :subdirs '(\"folder1\" \"folder2\") ==> #P\"/Users/bresson/om-tmpfiles/folder1/folder2/myfile.midi\"
"
  (om-make-pathname :directory (append (pathname-directory (get-pref-value :files :tmp-file)) (list! subdirs)) 
                    :host (pathname-host (get-pref-value :files :tmp-file))
                    :name (pathname-name name) :type (or type (pathname-type name))))

(defmethod* tmpfile ((path null) &key (subdirs nil) (type nil))
  (om-make-pathname :directory (append (pathname-directory (get-pref-value :files :tmp-file)) (list! subdirs))
                    :host (pathname-host (get-pref-value :files :tmp-file))))


;;;===================================
;;; HANDLE TEMP FILE CLEANUP
;;;===================================

(defvar *tmpparfiles* nil)
(defun add-tmp-file (file)
  (push file *tmpparfiles*)
  file)

(defun clean-tmp-files ()
  (when *tmpparfiles*
    (om-print "Removing files:")
    (loop for file in *tmpparfiles* do (when (and file (probe-file file))
                                       (om-print (format nil "   ~s" file))
                                       (om-delete-file file)))
    (setf *tmpparfiles* nil)))

(defun maybe-clean-tmp-files ()
  (when (get-pref-value :files :delete-tmp-files)
    (clean-tmp-files)))


;;;===================================
;;; SEARCH PATH
;;;===================================
(defun check-path-using-search-path (path)
      
  (or (probe-file path)
  
      (when (get-pref-value :files :search-path)
        
        (let ((found-matches (find-file-in-folder (pathname-name path) (get-pref-value :files :search-path) 
                                                  :type (pathname-type path) :recursive (get-pref-value :files :search-path-rec) :return-all t)))
          
          (when (> (length found-matches) 1)
            (om-beep-msg "Warning: several candidates were found in the search path folder for file ~A.~A !!" (pathname-name path) (pathname-type path)))
          
          (car found-matches)
          ))
      ))

;;;===================================
;;; HANDLE FILE EXIST
;;;===================================
;;; FINDS A GOOD (UNIQUE) PATH FOR NAME IN DIR
(defun unique-pathname (dir name &optional (ext ""))
  (let ((pathname (om-make-pathname :directory dir :name name :type ext)))
    (loop while (probe-file pathname)
          for i = 1 then (+ i 1) do
          (setf pathname (make-pathname :device (pathname-device dir) :directory (pathname-directory dir) :name (string+ name (format nil "~D" i)) :type ext)))
    pathname))

(defun auto-rename (path)
  (unique-pathname path (pathname-name path) (pathname-type path)))


;;; IF AUTOMATIC-RENAME OPTION IS ON AND FILE EXISTS, FINDS A NEW NAME
(defun handle-new-file-exists (newpath)
  (when (and newpath (probe-file newpath))
    (if (equal 'auto-rename (get-pref-value :files :file-exists-ation))
        (setf newpath (unique-pathname (make-pathname :directory (pathname-directory newpath)
                                                      :host (pathname-host newpath) :device (pathname-device newpath))
                                       (pathname-name newpath) (pathname-type newpath)))
      (delete-file newpath)
      ))
  newpath)

;;;===================================
;;; FILE SAVER/LOADER MEMORY
;;;===================================

(defvar *last-saved-dir* nil)

(defun def-save-directory ()
  (or (and *last-saved-dir* (probe-file *last-saved-dir*))
      (and (get-pref-value :files :out-file) (probe-file (get-pref-value :files :out-file)))
      (om-user-home)))

(defvar *last-loaded-dir* nil)

(defun def-load-directory ()
  (or (and *last-loaded-dir* (probe-file *last-loaded-dir*))
      (and (get-pref-value :files :in-file) (probe-file (get-pref-value :files :in-file)))
      (om-user-home)))


;;; FILE CHOOSE TOOLS
(defmethod* file-chooser (&key (type 'file) (mode 'existing) (initial-folder nil) (message nil))
  :icon :folder
  :initvals '(file existing desktop nil)
  :indoc '("file or directory" "new or existing" "pathname" "prompt for the dialog")
  :menuins '((0 (("file" file) ("directory" directory))) 
             (1 (("new" new) ("existing" existing))) 
             (2 (("home" home) ("desktop" desktop) ("other" nil))))
  :doc "Pops up a file or directory chooser dialog.

<type> allows to choose between a file or directory.
<mode> determines whether this should be an existing file or directory or a new one to be created.
<initial-folder> allows to determine a strating directory for browsing the file system.
<message> allows to set a specific message on the dialog.

Returns the selected pathname or NIL if cancelled."

  (let ((initfolder 
         (cond ((equal initial-folder 'home) (om-user-home))
               ((equal initial-folder 'desktop) (om-make-pathname :directory (append (pathname-directory (om-user-home)) '("Desktop"))))
               (t (if (equal mode 'new) *last-saved-dir* *last-loaded-dir*))))
        (rep nil))
    (setf rep
          (cond ((and (equal type 'file) (equal mode 'existing))
                 (om-choose-file-dialog :prompt message :directory initfolder))
                ((and (equal type 'directory) (equal mode 'existing))
                 (om-choose-directory-dialog :prompt message :directory initfolder))
                ((and (equal type 'file) (equal mode 'new))
                 (om-choose-new-file-dialog :prompt message :directory initfolder))
                ((and (equal type 'directory) (equal mode 'new))
                 (om-choose-new-directory-dialog :prompt message :directory initfolder)))
          )
    (if rep 
        (setf *last-loaded-dir* (om-make-pathname :directory rep))
      (om-abort))
    rep))










