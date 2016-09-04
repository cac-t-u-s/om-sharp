(defvar *source-dir* nil)
(setf *source-dir* (append (pathname-directory *load-pathname*) (list "src")))

(defvar *lib-files* nil)
(setf *lib-files* 
      '(
        "package"
        "definitions"
        "binding"
        "strings"
        "conditions"
        "hash-tables"
        "io"
        "macros"
        "control-flow"
        "symbols"
        "functions"
        "lists"
        "types"
        "arrays"
        "sequences"
        "numbers"
        "features"
        ))


(mapc #'(lambda (file) (compile&load (make-pathname :directory *source-dir* :name file))) *lib-files*)


#|
:components
  ((:static-file "LICENCE")
   (:static-file "tests.lisp") ?????????????
   (:file "package")
   (:file "definitions" :depends-on ("package"))
   (:file "binding" :depends-on ("package"))
   (:file "strings" :depends-on ("package"))
   (:file "conditions" :depends-on ("package"))
   (:file "hash-tables" :depends-on ("package"))
   (:file "io" :depends-on ("package" "macros" "lists" "types"))
   (:file "macros" :depends-on ("package" "strings" "symbols"))
   (:file "control-flow" :depends-on ("package" "definitions" "macros"))
   (:file "symbols" :depends-on ("package"))
   (:file "functions" :depends-on ("package" "symbols" "macros"))
   (:file "lists" :depends-on ("package" "functions"))
   (:file "types" :depends-on ("package" "symbols" "lists"))
   (:file "arrays" :depends-on ("package" "types"))
   (:file "sequences" :depends-on ("package" "lists" "types"))
   (:file "numbers" :depends-on ("package" "sequences"))
   (:file "features" :depends-on ("package" "control-flow"))))
|#
