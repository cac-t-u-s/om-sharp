(in-package :om)
;;===========================================================================
;Copyright (C) 2016 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Author: Samuel Bell-Bell
;;===========================================================================

;-----------------------Begin-End methods------------------------------------------
;initialisation de variable global nécessaire au bon fonctionnement de la prise d'information
;+ ajout de fonction play stop permettant d'activé des mesure depuis Intel(R) tools
;;redéfinition de player play object pour lancer prise on appuie sur play de maquette begin is on
;; redéfinition de player stop object qui arrête la prise de log freq cpu.
;;definition de fonctions tests.

(defvar *begin* 0)
(defvar *end* 0)
(defvar *begin-date* 0)
(defvar *bpf-period* 1)
(defvar *logical-core-nb*  (parse-integer (multiple-value-bind (out)   (sys:run-shell-command "sysctl -n hw.logicalcpu"
			   :wait nil
			   :output :stream
			   )
  (with-open-stream (out out)
      (values (read-line out))))))


;(defvar *taskbegin* #'(lambda () (setq *begin* (om-get-internal-time))))
(defvar *taskend* #'(lambda () (setq *end* (om-get-internal-time))))

(defvar *list-action-supposed* nil)

(defvar *list-buffer* nil)

(defvar *cpt-elmt-bpf* 0)

;(sys:run-shell-command "/usr/local/bin/gnuplot"
;			   :wait nil
;			   :output :stream)


(defun date-with-ms()
(multiple-value-bind (out err pid)
    (sys:run-shell-command "/usr/local/bin/gdate  +\"%H:%M:%S:%3N\" "
			   :wait nil
			   :output :stream)
  (with-open-stream (out out)
   
      (values (read-line out) ))))


 

;ref-time dans om-objet-schedulable recupérer par rapport a zero, heure du point - ref time de l'objet
;play de maquette 
(defmethod player-play-object ((self scheduler) (object ommaquette) caller &key parent interval)
  (setq *begin* (round (MACH::mach_absolute_time) 1000)) 
  (setq *begin-date* (date-with-ms)) ;*begin-date*
  (setq *cpt-elmt-bpf* 0)
  (setf (maquette (monitor-time  *engine*)) object)
  ;;;Ajouter ici la task begin :
  (let ((var 0)(lock-t 1))
  
  
    (poke-thread-pool *engine*) 
    (loop for box in (get-all-boxes object)  do
          (if (eq (type-of (car (value box))) 'bpf) 
       
              
              (progn (print "salut , cas bpf")    
                (setf  (action-list-supposed-timed (monitor-time *engine*))
                       (om+ (mapcar #'(lambda (a) (* a 1000)) (x-points (car (value box)))) (box-x box)))
        
                )
       
            )
          (if (not(eq (type-of (car (value box))) 'bpf))
              (progn (eq var 1)
                (return))))
    (if (= var 1)
        (progn (print "var=1 in player-play cas non bpf")
          (setf  (action-list-supposed-timed (monitor-time *engine*) )  (loop for box in (get-all-boxes object) 
                                                                              collect  (* 1000  (box-x box))) ))))
  (call-next-method)
                 
  )

(defmethod player-play-object-ms ((self scheduler) (object ommaquette) caller &key parent interval)
  
  (progn
    (let ((var 0))
      (print "debut")
  
      (poke-thread-pool *engine*)
      (call-next-method)  
      (loop for box in (get-all-boxes object)  do
            (if (eq (type-of (car (value box))) 'bpf) 
              (progn (print "salut , cas bpf")    
                  (setf  (action-list-supposed-timed (monitor-time *engine*))
                         (loop  for bpf-x in (x-points (car (value box)))
                                collect (+ (+ (ref-time object) (box-x box)) bpf-x)))
                  (setq *begin* (om-get-internal-time))
               (setq *begin-date* (date-with-ms)))
            
              )
            (if (not(eq (type-of (car (value box))) 'bpf))
                (progn (eq var 1)
                  (return))))
      (if (= var 1)
          (progn (print "var=1 in player-play cas non bpf")
            (setf  (action-list-supposed-timed (monitor-time *engine*) )  (loop for box in (get-all-boxes object) 
                                                                                collect  (+ (ref-time object) (box-x box)))))))                 
    )) 




 

(defmethod player-stop-object ((self scheduler) (object ommaquette))
  ;;;Stop all boxes under the maquette cursor (that is being rendered).
  ;;;Note : useful only for objects triggered by the maquette (hierarchical).
  (progn
    (kill-powerlog)
    (call-next-method)))



;-------------------Stuff to compute
(defun list-without-last (list)
           (loop for l on list
                 while (rest l)
                 collect (first l)))

(defun is-premier (n)
(compute
(let ((b 1))
  (loop for x from (- n 1) downto 2 do
        (if (= (mod n x) 0)
           (setq b 0)))
      (if (= b 0 )
          (print "no, it s not number primary")
        (print "yes it s a primary number")))))

(defun is-premier-no-compute (n)
(let ((b 1))
  (loop for x from (- n 1) downto 2 do
        (if (= (mod n x) 0)
           (setq b 0)))
      (if (= b 0 )
          (print "no, it s not number primary")
        (print "yes it s a primary number"))))





(defun sound-silence-sam  (dur  &optional (channels 1) (sample-rate *default-audio-sr*))
(compute
 (sound-silence dur channels sample-rate)))


(defun sound-silence-mixer-sam  (dur  &optional (channels 1) (sample-rate *default-audio-sr*))
  (compute
   (if (eq(length *list-buffer*) 100 )
       (setq *list-buffer* (list-without-last *list-buffer*))
     (push   (sound-silence dur channels sample-rate)  *list-buffer*))
   ))
;(sound-silence 100)
;(sound-silence-sam 100)


;( sound-silence-mixer-sam  100)


(defun list-n(n)
  (let ((list-res nil))
  (loop for x from 0 to n do
     ( setq list-res  (push  x list-res)))
 (reverse list-res))
)


(defun map-plus-un (listN)
  (compute
   (setq lst-give (mapcar #'1+ listN))))

(defun map-loop-plus-un (times)
  (compute
   (dotimes (x times)
     (setq lst-give (mapcar #'1+ lst-give)))))


(setq *global-cpt* 0)

(defun add-1000time-one()
  (compute (dotimes (x 1000)
             (incf *global-cpt*))))

;(add-1000time-one)

(defun list-complex (nbElem)
  (compute
  (let ((lst 
         (loop for x from 1 to nbElem
               for y = (* (/ x 100) 2)
               for z = (* y 2)
               collect (list y z))
         ))
(print "ok"))))


(defun list-lambda(lambda size)
(setq lambdaLst (loop for i from 1 to size collect
                      lambda)))




(defun make-IAE-grains-sam (n &key (nsources 1) (maxpos 10000) (durtot 40000) (mindur 100) (maxdur 100))
(let ((cpt 0) (subcpt 0) (pos (om-random 0 maxpos)) (duree 2000)) 

  (sort 
   (loop for i from 1 to n ; do
         collect
         (make-instance 'IAE-grain :date (*  i   (/ duree 25) );(om-random (* subcpt 1000) (*(+ subcpt 1) 1000) )  ; (om-random 0 durtot);(* i duree);
                        :source (om-random 0 (1- nsources))
                        :pos (om-random 0 maxpos);i;(+ (* subcpt 50) 100) ;pos
                        :duration (* duree 1))) ;(om-random mindur maxdur))) 
   '< :key 'date)))

(defun gentest2 ()
  (let* ((channel (om-random 1 2))
         (pitchlist (om+ 2 '(50 52 53 55 55 56 58 60))))
    (if (= channel 1)
        (loop for i from 0 to 42 collect
              (make-midinote :onset (abs (+ (* (mod i 14) 333) (om-random -40 40)))
                             :pitch (om+ (nth-random pitchlist) (nth-random '(0 12))) 
                             :vel 100
                              :dur (if (= channel 1) 300 100)                         
                             :channel 1))
      (loop for i from 0 to 4 collect
            (make-midinote :onset (abs (+ (* i 1000) (om-random -40 40)))
                           :pitch (nth-random '(52 53))
                           :vel (om-random 45 90)
                           :dur (om-random 800 1000) 
                           :channel 2)))))







