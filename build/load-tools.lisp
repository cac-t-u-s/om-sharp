(in-package :cl-user)

(export '(compile&load decode-local-path) :cl-user)

; (clean-sources)


(defvar *compile-type* "xfasl")
;;; should be : "xfasl" on MacIntel, "nfasl" on MacPPC, "ofasl" on Win32, "64xfasl" or "xfasl" on Linux
(setf *compile-type* (pathname-type (cl-user::compile-file-pathname "")))

#+win32(editor::bind-key "Find Source" "Control-." :global :pc)


;;; equivalent to LW'w CURRENT-PATHNAME, allowing other reference path like in om-relative-path
(defun decode-local-path (path &optional relative-path)
  (labels ((string-until-char (string char)
             (let ((index (search char string)))
               (if index (values (subseq string 0 index) (subseq string (+ index 1)))
                 (values string nil))))
           (str2list-path (str)
             (let (list)
               (loop while str do
                 (let ((rep (multiple-value-list (string-until-char str "/"))))
                   (setf str (second rep))
                   (when (first rep) (push (first rep) list))))
               (reverse list))))
    (let ((decoded-path (str2list-path path))
          (ref (or relative-path *load-pathname*)))
      (make-pathname
       :host (pathname-host ref) :device (pathname-device ref) 
       :directory (append (pathname-directory ref) (butlast decoded-path))
       :name (car (last decoded-path))))))



;;; This is how to use a new compiled-file extension (not used)
;(when (and compile-ext (not (find compile-ext sys:*binary-file-types* :test 'string-equal)))
;  (push compile-ext sys:*binary-file-types*))


; WARNINGS
; - <file> can be a .lisp or a pathname with no extension; 
; - Not sure why, but compile/load don't find the file type is :unspecific 
; they do if the type is NIL (load searches for "*fasl" then "lisp", compile-file seraches "lisp") 
; - The Lisp function "merge-pathname" tends to generate pathnames with type = :unspecific 

(defun compile&load (file &optional (verbose t) (force-compile nil) (compile-to nil))
  
  ;;; Replace :unspecific pathname-type by NIL
  (when (equal :unspecific (pathname-type file))
    (setf file (make-pathname :directory (pathname-directory file)
                              :device (pathname-device file)
                              :name (pathname-name file) 
                              :type NIL)))
  

  (let* (;;; Resolve the name of the actual Lisp file
         (lisp-file (truename (make-pathname :directory (pathname-directory file)
                                             :device (pathname-device file)
                                             :name (pathname-name file)
                                             :type "lisp")))
         ;;; Find out the target for compiled file (default = same as Lisp file)
         (fasl-target (if compile-to 
                          (make-pathname :directory (pathname-directory compile-to)
                                         :device (pathname-device compile-to)
                                         :name (pathname-name file))
                        (make-pathname :directory (pathname-directory file)
                                       :device (pathname-device file)
                                       :name (pathname-name file))))
         ;;; Resolve the name of the compiled-file
         (fasl-file (make-pathname :directory (pathname-directory fasl-target)
                                   :device (pathname-device fasl-target)
                                   :name (pathname-name fasl-target) 
                                   :type *compile-type*))
         ;;; Is there a compiled-file already ?
         (fasl-present (probe-file fasl-file))
         ;;; ... and is it up-to-date ? (= more recent than the last modification of the Lisp file) 
         (fasl-outofdate (and fasl-present
                              (or (not (file-write-date lisp-file))
                                  (not (file-write-date fasl-file))
                                  (> (file-write-date lisp-file) (file-write-date fasl-file))))))
    
    
    ; (print (format nil "File: ~s~%Lisp: ~s~%Fasl: ~s (~A-~A)" file lisp-file fasl-file fasl-present fasl-outofdate))
    
    ;;; COMPILE-FILE is not available in delivered applications
    ;;; If it is and if required/necessary: compile the file
    (when (and (fboundp 'compile-file) ;; == ;; (not (member :om-deliver *features*))
               (or force-compile 
                   (not fasl-present) 
                   fasl-outofdate))
      
      (when fasl-target (ensure-directories-exist fasl-target))

      (compile-file 
       lisp-file 
       :verbose 0
       :output-file fasl-target)
      
      (setf fasl-outofdate nil))

    (if fasl-outofdate
        ;;; If the fasl was here and is still out-of-date (couldn't be compiled) 
        ;;; then load the Lisp file
        (progn (print (format nil "WARNING: File ~A is older than the LISP source file.~%=> Loading ~A." fasl-file lisp-file))
          (load lisp-file :verbose verbose))
      
      ;;; Otherwise, load the compiled file
      ;;; The handler-bind gives us a chance to recompile, just in case the compilation contents was wrong despite the date
      (catch 'faslerror
        (handler-bind ((conditions::fasl-error 
                        #'(lambda (c) 
                            (declare (ignore c))
                            (when (and (fboundp 'compile-file) fasl-file)
                              (print (format nil "File ~s will be recompiled..." fasl-file))
                              (compile-file file :verbose verbose :output-file fasl-target)
                              (load fasl-file :verbose verbose)
                              (throw 'faslerror t)
                              ))))
          
          ;;; At this stage, load what we have !
          (if (probe-file fasl-file)
              (load fasl-file :verbose verbose)
            (load lisp-file :verbose verbose))
          
          )))))


;;; TEMP -- BUG LISPWORKS
; (trace (error :backtrace :bug-form :trace-output *terminal-io*))
;(editor:defcommand "Buffer List To File" (p)
;  ""
;  (with-open-file (out "~/LispWorks-Buffer-List.txt"
;                       :direction :output
;                       :if-exists :supersede)
;    (print editor::*buffer-list* out)))
;(editor:bind-key "Buffer List To File" #("control-c" "z"))
;;; END

(defun clean-svn (&optional dir)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)))))
    (mapc #'(lambda (file) 
             
              (if (system::directory-pathname-p file)
                  (if (string-equal ".svn" (car (last (pathname-directory file))))
                      (system::call-system (concatenate 'string "rm -Rf \"" (namestring file) "\""))
                    (clean-svn file))
                (when (and (pathname-type file)
                           (or (string-equal (pathname-type file) "lisp~")
                               (string-equal (pathname-type file) "DS_STORE")))
                  (delete-file file))
                ))
          (directory (namestring src-root) :directories t))))
              
; (clean-svn (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) '("libraries"))))

(defun clean-sources (&optional dir (verbose t))
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*))))))
    (mapc #'(lambda (file) 
              (if (and (system::directory-pathname-p file) (not (string-equal (car (last (pathname-directory file))) ".git")))
                  (clean-sources file verbose)
                (when (and (pathname-type file)
                           (or (find (pathname-type file) '("64xfasl" "xfasl" "fasl" "DS_STORE" "nfasl" "ofasl" "ufasl" "omfasl" "lisp~") :test 'string-equal)
			       (string= (pathname-type file) *compile-type*))) ; remove compiled files
                  (when verbose (print (concatenate 'string "Deleting " (namestring file) " ...")))
                  (delete-file file)
                  )))
          (directory (namestring src-root) :directories t))
    ))

; (clean-sources)
; (clean-sources (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 4) '("om-6-7-libs" "OMChroma"))))


(defun count-lines (file)
  (flet ((delete-spaces (string)
           (let ((pos (position-if #'(lambda (x) (not (member x (list #\Linefeed #\Space #\Tab) :test 'equal))) string)))
             (if pos (subseq string pos) ""))))
    (let ((n 0))
      (with-open-file (f file :direction :input)
        (let ((line (read-line f nil 'eof)))
          (loop while (and line (not (equal line 'eof))) do
                (unless (or (string-equal (delete-spaces line) "")
                            (equal (elt (delete-spaces line) 0) #\;))
                  (setf n (+ n 1)))
                (setf line (read-line f nil 'eof))
                )))
      n)))

(defun count-sources (&optional dir)
  (let ((nfiles 0)
        (nlines 0)
        (src-root (or dir (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 1) '("code"))))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (let ((count (count-sources file)))
                    (setf nfiles (+ nfiles (car count)))
                    (setf nlines (+ nlines (caDr count))))
                (when (and (pathname-type file)
                           (string-equal (pathname-type file) "lisp"))
                  (setf nfiles (+ nfiles 1))
                  (setf nlines (+ nlines (count-lines file)))
                  )
                  ))
          (directory (namestring src-root) :directories t))
    (list nfiles nlines)
    ))

; (count-sources)
; ==> 444 files,  132219 lines of code,  183377 lines

