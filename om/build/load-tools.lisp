(in-package :cl-user)

(export '(compile&load decode-local-path) :cl-user)

; (clean-sources)


(defvar *compile-type* "xfasl")
;;; should be : "xfasl" on MacIntel, "nfasl" on MacPPC, "ofasl" on Win32.
(setf *compile-type* (pathname-type (cl-user::compile-file-pathname "")))

#+win32(editor::bind-key "Find Source" "Control-." :global :pc)


;;; equivalent to LW'w CURRENT-PATHNAME, allowing other reference path like in om-relative-path
(defun decode-local-path (path &optional (relative-path :current))
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
          (ref *load-pathname*))
      (make-pathname
       :host (pathname-host ref) :device (pathname-device ref) 
       :directory (append (pathname-directory ref) (butlast decoded-path))
       :name (car (last decoded-path))))))

(defun compile&load (file &optional (verbose t) (force-compile nil))
  (let* ((lisp-file (truename (if (pathname-type file) file (concatenate 'string (namestring file) ".lisp"))))
          (fasl-file (probe-file (make-pathname :directory (pathname-directory lisp-file)
                                                :device (pathname-device lisp-file)
                                                :name (pathname-name lisp-file) :type *compile-type*)))
          (fasl-outofdate (and fasl-file
                               (or (not (file-write-date lisp-file))
                                   (not (file-write-date fasl-file))
                                   (> (file-write-date lisp-file) (file-write-date fasl-file))))))
     (when (and (not (member :om-deliver *features*))
                (or force-compile (not fasl-file) fasl-outofdate))
       (compile-file file :verbose verbose)
       (setf fasl-outofdate nil))
     (if fasl-outofdate
         (progn (print (format nil "WARNING: File ~A is older than the LISP source file. File ~A will be loaded instead."
                               fasl-file lisp-file))
           (load lisp-file :verbose verbose))
       (catch 'faslerror
         (handler-bind ((conditions::fasl-error #'(lambda (c) 
                                                    (when (and (fboundp 'compile-file) fasl-file)
                                                      (print (format nil "File ~s will be recompiled..." fasl-file))
                                                      (compile-file file :verbose verbose)
                                                      (load file :verbose verbose)
                                                      (throw 'faslerror t)
                                                      ))))
           ;; (print file)
           (load file :verbose verbose)
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

(defun clean-sources (&optional dir)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*))))))
    (mapc #'(lambda (file) 
              (if (and (system::directory-pathname-p file) (not (string-equal (car (last (pathname-directory file))) ".git")))
                  (clean-sources file)
                (when (and (pathname-type file)
                           (or (find (pathname-type file) '("64xfasl" "xfasl" "fasl" "DS_STORE" "nfasl" "ofasl" "ufasl" "lisp~") :test 'string-equal)
			       (string= (pathname-type file) *compile-type*))) ; remove compiled files
                  (print (concatenate 'string "Deleting " (namestring file) " ..."))
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

