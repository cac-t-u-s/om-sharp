(eval-when (:compile-toplevel :load-toplevel :execute)
  #+allegro (require :smputil)
  #+corman  (require :threads))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or armedbear
        (and allegro multiprocessing)
        (and clisp mt)
        (and openmcl openmcl-native-threads)
        (and cmu mp)
        corman
        (and ecl threads)
        mkcl
        lispworks
        (and digitool ccl-5.1)
        (and sbcl sb-thread)
        scl)
  (pushnew :thread-support *features*))

(require-library "fiveam")

(defvar *source-dir* nil)
(setf *source-dir* (append (pathname-directory *load-pathname*) (list "src")))

(defvar *lib-files* nil)
(setf *lib-files* 
      '(
        "pkgdcl"
        "bordeaux-threads"
        "impl-lispworks"
        "default-implementations"
        ))


(mapc #'(lambda (file) (compile&load (make-pathname :directory *source-dir* :name file))) *lib-files*)


#|
En regardant la variable gloabale "features" on voit dans la liste "lispworks", donc ne prendre que le fichier correspondant ?
(:file #+(and thread-support armedbear) "impl-abcl"
                        #+(and thread-support allegro)   "impl-allegro"
                        #+(and thread-support clisp)     "impl-clisp"
                        #+(and thread-support openmcl)   "impl-clozure"
                        #+(and thread-support cmu)       "impl-cmucl"
                        #+(and thread-support corman)    "impl-corman"
                        #+(and thread-support ecl)       "impl-ecl"
                        #+(and thread-support mkcl)      "impl-mkcl"
                        #+(and thread-support lispworks) "impl-lispworks"
                        #+(and thread-support digitool)  "impl-mcl"
                        #+(and thread-support sbcl)      "impl-sbcl"
                        #+(and thread-support scl)       "impl-scl"
                        #-thread-support                 "impl-null")



;D'AUTRE PART
;
; #+(and thread-support lispworks (not lispworks6))
;                 (:file "impl-lispworks-condition-variables")
;                 #+(and thread-support digitool)
;                 (:file "condition-variables")
;                 (:file "default-implementations"))))

ET DANS LA LISTE IL Y A LISPWORKS6 ET APPAREMENT PAS DIGITOOLS

|#


