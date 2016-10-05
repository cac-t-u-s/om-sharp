 (in-package :om)

; StartServerMax.lisp
;------------------------------------------------------------------------------------------------------------
; Load the library (only if not loaded) and start the server to listen to the Max/MSP interface
; If it is not defined, the path of the work directory (containing the interface, the midi buffers, and the 
; saved oracles and antescofo scores) will be asked.
;
; Jérôme Nika - March 11th 2013
;------------------------------------------------------------------------------------------------------------

(defvar *flag-loaded-lib* nil)

(if (not *flag-loaded-lib*)
    (let ((file-load-improtek (make-pathname :directory (pathname-directory *load-pathname*) :name "LoadImprotek")))
      (format *om-stream* "loading ~a~%" file-load-improtek)
      (if (om-standalone-p) (load file-load-improtek) (compile&load file-load-improtek))))


; "Global variables"
(defparameter prtRcv 7413)
(defparameter prtSnd 7414)
(defparameter host_server "127.0.0.1") 
(defvar *server* nil)
#|
Warning: (defparameter prtrcv) being redefined in /Users/jnika/Zeugm/Zeugm_OM/OM-LIBRARIES/improtek/sources/StartServerMax.lisp (previously in /Users/jnika/Zeugm/Zeugm_OM/OM-LIBRARIES/improtek/sources/WorkDirectory_Paths.lisp).

Warning: (defparameter prtsnd) being redefined in /Users/jnika/Zeugm/Zeugm_OM/OM-LIBRARIES/improtek/sources/StartServerMax.lisp (previously in /Users/jnika/Zeugm/Zeugm_OM/OM-LIBRARIES/improtek/sources/WorkDirectory_Paths.lisp).

Warning: (defparameter host_server) being redefined in /Users/jnika/Zeugm/Zeugm_OM/OM-LIBRARIES/improtek/sources/StartServerMax.lisp (previously in /Users/jnika/Zeugm/Zeugm_OM/OM-LIBRARIES/improtek/sources/WorkDirectory_Paths.lisp).;;
|#

; Send the list of known grids
(defun send-available_grids (server_host portRcv portSnd)
  (setf grid_list '()  message '("/info-available_grid"))
  (setf grid_list (loop for e being the hash-key of *available-grids* using (hash-value v)
        collect (info_tune v) append grid_list))
  (setf m (append message grid_list))
  (format *om-stream* "=-=-=-=-=-=-= INFOGRID WILL SEND ~a~%" m)
  (osc-send m server_host portSnd))


; Stop the server
;Jerome 29/04/2013 : ex "kill-server"
(defun stop-osc-server ()
  (om-stop-osc-server *server*)
  (setf *server* nil))

; Start the server
(defun start-communication-max (server_host portRcv portSnd)
  ;Announce
  (osc-send '("/hello" "Communication with OM : OK") server_host portSnd)
  ;Open server listening to MAX
  (when *server* (stop-osc-server)) 
  (setf *server* 

        (if (<= *om-version* 6.04)
        (om-start-osc-server portRcv server_host
                             ;#'(lambda (mess)
                             #'(lambda (mess host) ; POUR OM6.6 : plus le host en param
                                 (handle-messages-from-max (om-decode-msg-or-bundle mess)) nil))

          (om-start-osc-server portRcv server_host
                             #'(lambda (mess)
                             ;#'(lambda (mess host) ; POUR OM6.6 : plus le host en param
                                 (handle-messages-from-max (om-decode-msg-or-bundle mess)) nil))))
  ;Send infos
  (send-available_grids server_host portRcv portSnd))

;===================================================
(start-communication-max host_server prtRcv prtSnd)
; (stop-osc-server)
;===================================================












