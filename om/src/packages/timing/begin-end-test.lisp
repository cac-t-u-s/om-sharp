(in-package :om)

(setq *end* (om-get-internal-time))
*begin*
(- *end* *begin*)
(length *list-buffer*)

(length(action-list-supposed-timed (monitor-time *engine*)))
(last(action-list-supposed-timed (monitor-time *engine*)))

;--------------------TODO measurement---------------------

(setq *monitor* (make-instance 'om-time-mesure))
(compteur *monitor*)   

 (mesure-all-time (monitor-time *threadpool*) (intern(format nil "~S~A" 'clef (cpt(monitor-time *threadpool*)))) (mp:find-process-from-name "Thread-1")  (princ "hey"))

(intern(format nil "~S~A" 'clef (cpt(monitor-time *threadpool*))))
(gethash 'threadkey3 (hash-exec *monitor*))
;-----------------------reinitialise le scheduler---------
(progn (om::abort-om-player) (om::init-om-player)) 
;-----------------------
(compteur)
;;value
 (gethash 'schedkey (hash-exec (scheduler-monitor-time  *scheduler*)))
(reverse  (gethash 'schedkey (hash-exec (scheduler-monitor-time  *scheduler*))))

(engine-tasks-left)    



(gethash "'threadkey1" (hash-exec (monitor-time *threadpool*)))


;;;;;;ICI
;;;;;pour recuperer temps total d'exec -> last-end - begin first 
(- (cadr (gethash 499 (hash-exec (monitor-time *engine*)))) (car (gethash 1 (hash-exec (monitor-time *engine*)))))



(gethash 500 (hash-exec (monitor-time *engine*)))
(gethash 499 (hash-exec (monitor-time *engine*)))
(gethash 498 (hash-exec (monitor-time *engine*)))
(gethash 0 (hash-exec (monitor-time *engine*)))
(gethash 1 (hash-exec (monitor-time *engine*)))
(gethash 2 (hash-exec (monitor-time *engine*)))
(gethash 3 (hash-exec (monitor-time *engine*)))
(gethash 4 (hash-exec (monitor-time *engine*)))




(gethash "threadkey4" (hash-exec (monitor-time *engine*)))

  (mesure-time-in-body *monitor*(intern (format nil "'~A~A" (time-key *engine*)  (subseq(reverse(mp:process-name mp:*current-process*)) 0 1))) (time-type *monitor*)  (princ "coucou"))

  (mesure-time-in-body *monitor* 'hello (time-type *monitor*)  (princ "coucou"))

(princ(format nil "'~A~A" (time-key *engine*) (subseq(reverse(mp:process-name(mp:find-process-from-name "Thread-1")))0 1)  (subseq(reverse(mp:process-name mp:*current-process*)) 0 1)))

 (parse-integer(subseq(reverse (mp:process-name(mp:find-process-from-name "Thread-1"))) 0 1))

(format nil "~A" (subseq(reverse(mp:process-name mp:*current-process*)) 0 1))
(format t "~A" (reverse(mp:process-name mp:*current-process*)))
(format t "~$" pi)
(subseq(reverse(mp:process-name(mp:find-process-from-name "Thread-1"))) 0 1)

(intern "lkjlkj")  

(eq (monitor-time *engine*) (monitor-time *threadpool*))

(let ((h (make-hash-table)) :test string-machin) ;(make-hash-table :test 'equal)
  (setf (gethash "test" h) 1)




(princ(hash-keys (hash-exec (monitor-time *engine*))))
(gethash "'threadkey3" (hash-exec *monitor*))
(hash-keys  (hash-exec (scheduler-monitor-time  *scheduler*)))
(hash-keys (hash-exec *monitor*))
(car(hash-keys  (hash-exec (monitor-time  *engine*))))
(princ(hash-keys  (hash-exec (monitor-time  *threadpool*))))

(finder  (hash-exec *monitor*))
*begin*

(length(action-list-supposed-timed (monitor-time *engine*)))
(princ(order-finder2 (hash-exec (monitor-time  *engine*))))  
(princ(finder (hash-exec (monitor-time  *engine*))))  
(princ( finder  (hash-exec (scheduler-monitor-time  *scheduler*))))


;------------------------TASKNUMBERLEFT--------------------
(task-number-left *engine*)
;;;---------------------------------------------------------

(nth 2 (action-list-supposed-timed (monitor-time *engine*)))

;;;;;;;;TEST REQUEST
 (om-get-internal-time)
(all (monitor-time *engine*))
(car(car(req-between (monitor-time *engine*) 10 500000000)))

(sort (flatten (req-recup-execTime  (monitor-time *engine*))) '<)

;;;;;; TODO => fonction qui ordonne dans l'ordre , fonction pour par thread avoir exec time (donc les deux => donne dans le bonne ordre!)
;ok

;get begin per thread in order
(length(mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) 8) #'(lambda(x y) (< (car(car x)) (car(car y)))))))



;get exec per thread (end-begin)
(mapcar #'(lambda(x) (- (cadr(car x))(car(car x)))) (sort ( req-per-thread (monitor-time *engine*) 1) #'(lambda(x y) (< (car(car x)) (car(car y))))))


;get retard (begin-supposetime) per thread...
 (mapcar #'truncate(mapcar #'(lambda(x) (- (car(car x))(cadddr(car x))))  (sort ( req-per-thread (monitor-time *engine*) 8) #'(lambda(x y) (< (car(car x)) (car(car y)))))))


;get retard and begin time meanwhile
;(begin retard)
 (mapcar #'(lambda(x y) (list x y))
(mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) 1) #'(lambda(x y) (< (car(car x)) (car(car y))))))
(mapcar #'truncate
        (mapcar #'(lambda(x) (- (car(car x))(cadddr(car x))))  (sort ( req-per-thread (monitor-time *engine*) 1) #'(lambda(x y) (< (car(car x)) (car(car y))))))
))









;si nil add
 (loop for i from 1 to 8 do
(print (gethash i (hash-exec (monitor-time *engine*))))
)
(print (gethash 5 (hash-exec (monitor-time *engine*))))
;get exec sched
;to pop sometime...?
 (mapcar #'(lambda(x) (-  (cadr x) (car x))) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))
 
(car(gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))
(pop (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))

(mapcar #'(lambda(x)  (car x)) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))






(car(cadr(list(gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))))
 (req-recup-execTime  (monitor-time *engine*))

(ordered-keys (hash-exec (monitor-time *engine*)))



;test write into file
( write-result-onto-file  (monitor-time *engine*))
(format stream (flatten(princ (req-recup-execTime  (monitor-time *engine*)))))

(numlist-to-string (flatten(princ (req-recup-execTime  (monitor-time *engine*)))))

;resumer trier les clef dans l'ordre et regenerer dans l'ordre la table de hashage



(write-all-in-file "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Emaquette-granu-call-2ms-dur100ms-execAAA-test.txt" "sound-silence-sam-50voixEnmemeTemps" 50000 0 0 20)

;( write-testColonne-in-file  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/sched-maquette-mix-table.dat")
;( write-testColonne2-in-file  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/C-sound-silence-sam-50K-90s-horizon20K-2.txt")

(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Fmaquette-granu-call2ms-dur100ms-exec.dat")
(write-retard-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Fmaquette-granu-call2ms-dur100ms-retard.dat")
(write-sched-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Fmaquette-granu-call2ms-dur100ms-sched.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Fmaquette-granu-actif-thread-call2ms-dur100ms-exec.dat")
(write-activeThread-retard  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Fmaquette-granu-actif-thread-call2ms-dur100ms-retard.dat")
; (write-exec-time-2  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/alt-ispremier-400K-per100ms-exec.dat")

;(write-retard-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/alt-maquette-mix-table-2000ms-per20ms-retard.dat")


(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Amaquette-ispremier-call100ms-arg400000-exec.dat")
(write-retard-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Amaquette-ispremier-call100ms-arg400000-retard.dat")
(write-sched-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelleAmaquette-ispremier-call100ms-arg400000-sched.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Amaquette-ispremier-thread-actif-call100ms-arg400000-exec.dat")
(write-activeThread-retard  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Amaquette-ispremier-thread-actif-call100ms-arg400000-retard.dat")


;;;;;
(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/task-dependancy-peign12-exec.dat")
(write-retard-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/task-dependancy-peign12-retard.dat")
(write-sched-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/task-dependancy-peign12-sched.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/task-dependancy-thread-actif-peign12-exec.dat")
(write-activeThread-retard  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/task-dependancy-thread-actif-peign12-retard.dat")



(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/TESSSSSSST.dat")
(write-retard-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/TESSSSSSST2.dat")
(write-sched-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle//TESSSSSSST3.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/TESSSSSSST4.dat")
(write-activeThread-retard  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/TESSSSSSST5.dat")

(write-all-in-file "/Users/samuel/Documents/openMusic/MesuresComputationnelle/map-plus-un-200Kelt-10Kcomputation-1thread-exec.dat" "map-plus-un-10K-ecart100" 0 0 0 0)
(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/map-plus-un-200Kelt-10Kcomputation-1thread-exec.dat")
(write-retard-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/map-plus-un-200Kelt-10Kcomputation-1thread-retard.dat")
(write-sched-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/map-plus-un-200Kelt-10Kcomputation-1thread-schedtime.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/map-plus-un-actif-thread-200Kelt-10Kcomputation-1thread-exec.dat")
(write-activeThread-retard  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/map-plus-un-actif-thread-200Kelt-10Kcomputation-1thread-retard.dat")



(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/plus-un-x1000-1000task-1thread-saturer-exec.dat")

(length(mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) 2) #'(lambda(x y) (< (car(car x)) (car(car y)))))))
;-----------------------reinitialise le scheduler---------
(progn (om::abort-om-player) (om::init-om-player))
;-----------------------
(length (req-recup-all   (monitor-time *engine*)))

(round(/(reduce #'+ (flat(req-recup-execTime  (monitor-time *engine*)))))10000))


 ;pas mal   ;;;;ou faire un fichier qui tronc ou voir si gnuplot peut diviser par 100 et prendre a l'ordre de la micro second...
(req-recup-supposedTime (monitor-time *engine*))
*begin*
(req-recup-retard (monitor-time *engine*))
(req-recup-execTime  (monitor-time *engine*))
(setq lst-give (list-n 120000))
( map-plus-un lst-give)
;(setq lst-give ( map-plus-un lst-give))
lst-give
(print "ok")
;(round(/ *begin* 1000))
 (  req-active-thread (monitor-time *engine*) 2)
( req-test  (monitor-time *engine*)3)
(hash-exec (monitor-time *engine*))

(- 7548622365 7548522067)


;calcul temps nominal un thread, systeme non charger
;;;;117 temps nominal de is
(setq lst-give (list-n 200000))
(add-task  *engine* (lambda ()( map-plus-un lst-give)))
(add-task  *engine* (lambda ()((print "ok"))))

)

(progn 
(let ((cpt 0))
(round(/  (reduce #'+
(loop for a from 0 to 9999  collect
      (nth a (flat(req-recup-execTime  (monitor-time *engine*))))
))10000)

)))

;------------OK-----------------------------------------------stat------------------
;

(float (epsilon-pourcent (epsilon-list *execlist*  65) 65))
(* 1.0 (/(reduce #'+ (flat (req-recup-execTime  (monitor-time *engine*))))(length (req-recup-execTime  (monitor-time *engine*)))));moyenne 
(sqrt(/(reduce #'+ (mapcar #'(lambda(x)(* x x)) (epsilon-list *execlist*  65)))1000.0)) ;variance

(sqrt(/(reduce #'+ (mapcar #'(lambda(x)(* x x)) (epsilon-list  (flat (req-recup-execTime  (monitor-time *engine*)))  10357)))994.0))


;;;;il y a des pertubation tout de meme du au garbage collector
(let ((prcentlist (mapcar #'(lambda (a) (abs (/ a 65.0))) (epsilon-list *execlist* 65))))
  (* 100 (/ (reduce '+ prcentlist) (length prcentlist))))


(espilon-variation-moyenne (epsilon-list *execlist* 65.0)65.0)

;-------------->------------->------------>------------>-------------->----------------<-------------------<-----------
;tâche 10X plus grande (boucle entre 5 10 fois la meme tâche , entre 1ms 2ms...
;meme tache toutes les 117us est ce que je prend du retard
;faire une tâche qui dure plus lgt
;degradation des coeurs a regardé coef multiplicatif ?
;histogram

;saturation ----> d'un coeur , des fini je la relance taskaddseveral
;satu 2coeur, tout les demi temps de chaque tâche 2thread ---> 4 ---> 8

;65 micro-seconde temps nominal de map-un-plus sur un liste de 200K element =>mille 1er echantillon
;51 sur les 100premier echantillon...
;117micro-sec sur les 10000premiers => appelle tout les 500milli sec -> 1H20 de computation;;;


;-----------------------reinitialise le scheduler---------
(progn (om::abort-om-player) (om::init-om-player))
;-----------------------
(setq lst-give (list-n 100000))
(setq lambdaLst (loop for i from 0 to 999 collect
                      (lambda ()  (dotimes (x 1)
     (mapcar #'1+ lst-give)))))
(length lambdaLst)

(setq *global-cpt* 0)
(setq lambdaLst (loop for i from 0 to 999 collect
(lambda ()(dotimes(x 1000)(setq *global* (+ 1 *global-cpt*))))))




(task-number-left *engine*)
 (add-multiple-tasks  *engine*  lambdaLst)
(map-loop-plus-un lst-give 5)
 (setq list-res (req-recup-execTime  (monitor-time *engine*)))
(pop list-res)

(length(req-recup-execTime  (monitor-time *engine*)))
(setf (nth 3(gethash 1  (hash-exec  (monitor-time *engine*)))) (+ 100000000 (nth 3(gethash 1  (hash-exec  (monitor-time *engine*))))))



;;;;todo !
(update-supposed-time (monitor-time *engine*))
(add-to-supposed-time  (monitor-time *engine*) 1000000)
;;;;;testing 
(add-task *engine* #'(lambda () (map-loop-plus-un 5)))
(add-task *engine* #'(lambda ()(dotimes(x 1001)(setq *global* (+ 1 *global*)))))


(defun test (a) (compute (sleep 1)))

(poke-thread-pool *engine*)
(req-recup-execTime  (monitor-time *engine*))

(is-premier 5000)

;;;;;test Jeudi 9 juin

;;;TODO ajouter 1000000 a toutes les mesures temps supposed. !!!

 ( req-per-thread (monitor-time *engine*) 1)
(mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort ( req-per-thread (monitor-time *engine*) 4) #'(lambda(x y) (< (car(car x)) (car(car y))))))

(setq lambdaLst (loop for i from 0 to 999 collect
                      (lambda ()(is-premier-no-compute 5000))))


(loop for task in lambdaLst
      do
      (mp:mailbox-send (taskqueue *engine*) task)
      (poke-thread-pool *engine*)
      (sleep 0.007))


 (add-multiple-tasks-delay  *engine*  0.007)

 (add-multiple-tasks-delay  *engine*  0.007)



(req-recup-retard (monitor-time *engine*)) ;;;to update..

;;;;a retravaillé (? mesurer le temps avec la macro du sleep) et essayer de call du c usleep? nanosleep....?
 (add-multiple-tasks-delay *engine*  lambdaLst 0.002)

(length(flat(req-recup-execTime  (monitor-time *engine*))))




(write-idle-time-per-thread "/Users/samuel/Documents/openMusic/MesuresComputationnelle/test_idle.dat")






;------LISP-----

(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-5K-1000task-8thread-saturer-exec-lispwork.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-actif-thread-5K-1000task-8thread-saturer-exec-lispwork.dat")


(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-5K-1000task-8thread-non-saturer-exec-lispwork.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-actif-thread-5K-1000task-8thread-non-saturer-exec-lispwork.dat")



;--------OM

(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-5K-1000task-2thread-saturer-exec-OM.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-actif-thread-5K-1000task-2thread-saturer-exec-OM.dat")


(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-5K-1000task-2thread-non-saturer-exec-OM-pertuber-IMOVIE.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-actif-thread-5K-1000task-2thread-non-saturer-exec-OM-pertuber-IMOVIE.dat")


;----------
(write-all-in-file "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-50K-1000task-2thread-nonSat-exec.dat" "ispremier50K" 0 0 0 0)
(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-50K-1000task-nonSat-2thread-exec.dat")
(write-retard-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-50K-1000task-2thread-nonSat-retard.dat")
(write-sched-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-50K-1000task-2thread-nonSat-1thread-exec-schedtime.dat")
(write-activeThread-time  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-actif-thread-50K-1000task-2thread-nonSat-exec.dat")
(write-activeThread-retard  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-actif-thread-50K-1000task-2thread-nonSat-retard.dat")

(sleep 0.001)
