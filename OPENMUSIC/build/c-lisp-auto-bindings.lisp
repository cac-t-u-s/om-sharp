
(in-package :cl-user)

(defparameter *c-lisp-types* '((void :void) (int :int) (bool :boolean) (float :float) (double :double) 
                               (char* :string) (iaeom_t* :pointer)))
(defun colle-star (in)
  (let* ((str in) (pos (search " *" str)))
    (loop while pos do
          (setf str (concatenate 'string (subseq str 0 pos) "* " (subseq str (+ pos 2))))
          (setf pos (search " *" str)))
    str))

(defun string-until-char (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

(defun convert-type (type)
  (or (cadr (find type *c-lisp-types* :key 'car))
      (and (find #\* (string type)) :pointer)
      (and type (intern (concatenate 'string "UNKNOWN_TYPE_" (string type)) :keyword))))

(defun get-fun-list (file)
  (let ((line nil)
        (return-types )
        (functions nil)
        (last-comment nil))
    (with-open-file (f file :direction :input)
      (loop while (not (string-equal "eof" (setf line (colle-star (remove #\: (read-line f nil "eof"))))))
            do (multiple-value-bind (item-read pos) (read-from-string line nil nil)
              (if (find item-read *c-lisp-types* :key 'car)
                  (let ((fun-and-args (subseq line pos)))
                    (loop while (or (equal #\Space (elt fun-and-args 0))
                                    (equal #\Tab (elt fun-and-args 0)))
                          do (setf fun-and-args (subseq fun-and-args 1)))
                    (multiple-value-bind (fname args) (string-until-char fun-and-args " ")
                    (push 
                     ;; (function-name n-type n-type comments)
                     (list fname item-read args (reverse last-comment))
                     functions)
                    (setf last-comment nil)))
                (if item-read (push line last-comment))))))
    (remove-duplicates (reverse functions) :key 'car :from-end t)))

(defun write-args (string stream) 
  (let ((argstring (remove #\( (remove #\) (substitute #\Space #\Tab string))))
        (list nil))
    (loop while argstring do
          (let ((rep (multiple-value-list (string-until-char argstring ","))))
            (setf argstring (second rep))
            (when (first rep) (push (first rep) list))))
    (loop for item in (reverse list)
          collect 
          (progn
            (loop while (and (> (length item) 0) (equal #\Space (elt item 0)))
                  do (setf item (subseq item 1)))
            (when (and (>= (length item) 5) (string-equal (subseq item 0 5) "const")) (setf item (subseq item 6)))
            (multiple-value-bind (type pos) 
                (read-from-string item nil nil)
              (when type
                (format stream " (~a :~a)" 
                        (read-from-string (subseq item pos) nil nil)
                        (convert-type type)))
              ))
          )))

(defun make-ffi (in out &optional (package :cl-user))
  (with-open-file (f out :direction :output :if-exists :supersede)
    (format f "(in-package :~a)" package)
    (terpri f) (terpri f)
    (loop for fun in (get-fun-list in) do
          (loop for cline in (nth 3 fun) do
                (write-line (concatenate 'string ";;; " cline) f))
          (format f "(cffi::defcfun (~s ~a) :~a" (nth 0 fun) (nth 0 fun) (convert-type (nth 1 fun)))
          (write-args (nth 2 fun) f)
          (format f ")~%")
          (terpri f)))
  out)