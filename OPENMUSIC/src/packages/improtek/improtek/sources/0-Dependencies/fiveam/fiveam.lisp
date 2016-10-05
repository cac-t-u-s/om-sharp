(require-library "alexandria")

(defvar *source-dir* nil)
(setf *source-dir* (append (pathname-directory *load-pathname*) (list "src")))

(defvar *lib-files* nil)
(setf *lib-files* 
      '(
        "package"
        "utils"
        "check"
        "fixture"
        "classes"
        "random"
        "test"
        "explain"
        "suite"
        "run"
        ))


(mapc #'(lambda (file) (compile&load (make-pathname :directory *source-dir* :name file))) *lib-files*)



