(in-package :om)

; LoadImprotek.lisp
;------------------------------------------------------------------------------------------------------------
; Load the library (without starting the server to listen to the Max/MSP interface)
; If it is not defined, the path of the work directory (containing the interface, the midi buffers, and the 
; saved oracles and antescofo scores) will be asked.
;
; Jérôme Nika - March 11th 2013
;------------------------------------------------------------------------------------------------------------

;(if (find-library "Improtek") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "Improtek")))))
;(if (find-library "alexandria") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "alexandria")))))
;(if (find-library "bordeaux-threads") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "bordeaux-threads")))))
;(if (find-library "fiveam") (cl-user::clean-sources (om-make-pathname :directory (lib-pathname   (find-library "fiveam")))))


;GLOBAL VARIABLES FOR NAVIGATION INFO DISPLAY
;---------------------------------------------
;Jerome 26/10/13
(defparameter *print-navig-basics* t) ; Print labels and min. info during navigation
(defparameter *print_info_MP* 0) ;Print every step in "Morris&Pratt"
(defparameter *print_info_find* 0) ;Print every step in "find-prefix-label-match"
(defparameter *print_info_navig* 1)  ;Print every step in "Improvize"

(setq *print_info_navig* 0)
(setq *print-navig-basics* nil)

;================================================================================================================================
; (Compile and) load "Improtek.lisp" in the parent directory
; to (compile and) load all the files listed in it.

(let ((file-lib-improtek (make-pathname :directory (reverse (cdr (reverse (pathname-directory *load-pathname*)))) :name "Improtek")))
  (format *om-stream* "loading ~a~%" file-lib-improtek)
  (if (om-standalone-p) (load file-lib-improtek) (compile&load file-lib-improtek)))
;=================================================================================================================================

(defun set-current-tune (s s1) 
  (print s)
  (setf req_tune (gethash s *available-grids*))
  (setf *current-tune* (clone req_tune))  
  (setf *current-memory* (clone req_tune))                    
  (generate-grid *current-tune* (beatduration *current-tune*))
  ;Jerome 26/10/13
  (generate-grid-param *current-tune* (beatduration *current-tune*))

  ;!!!!!!!!!!!!!!!!!!
  ;;;;;;;MARC 26/9/14  creation of a subfolder for live oracles
  (let* ((foldername s1)
         (absolute-path-filename (format nil "~a~a/~a/" path_work_directory "_Oracles" foldername))
         ;;;;;(absolute-path-filename-empty (format nil "~a~a/~a/~a" path_work_directory "_Oracles" foldername "- empty -"))
         )
                                            ;;; global variable 'path_dir_live_oracles', cf. WorkDirectory_Paths.lisp
    (ensure-directories-exist absolute-path-filename)
    ;;;; plus utile: "-non-" et "...LiveOracle..." ajoutes dans le popup (save-improvizer (NewImprovizer) absolute-path-filename-empty)
    (setf (subfolder-for-live-oracles *current-tune*) foldername))
  )


;Jerome 29/04/2013 : ex "meloharm_oracle_initialization"
(defun load-saved-oracle-in-meloharm-oracle (s) 
  (if (probe-file s)
      (progn (setf *current-MeloHarmOracle* (load-improvizer s)))
        (print "MeloHarm Oracle not found")))

;Jerome 29/04/2013 : ex "voicings_oracle_initialization"
(defun load-saved-oracle-in-voicings-oracle (s) 
  (if (probe-file s)
      (progn (setf *current-VoicingOracle* (load-improvizer s)))
    (print "MeloHarm Oracle not found")))


(defvar *flag-loaded-lib* t)