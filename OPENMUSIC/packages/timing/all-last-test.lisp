(in-package :om)

;-----------------------reinitialise le scheduler---------
(progn (om::abort-om-player) (om::init-om-player))
;-------------------------------------------------------

(setq *global-cpt* 0)
(setq lambdaLst (loop for i from 0 to 999 collect
(lambda ()(list-complex 1000))))

(compute(lambda()(list-complex 1000)))


(task-number-left *engine*)
 (add-multiple-tasks  *engine*  lambdaLst)


;(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/maquette-mix-2ms-100ms-non-saturer-freq-OM.csv")
(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/is-premier-25K-8thread-non-saturer-OM-pertuber-IMOVIE.csv")
(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/is-premier-25K-1thread-non-saturer-OM.csv")

(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/IAE-SAM-2s-all80ms-500grains-4thread-non-saturer-OM.csv")
(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/IAE-SAM-2s-all40ms-500grains-4thread-non-saturer-OM-pertuber-IMOVIE.csv")

(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/list-complex-75K-all100ms-1thread-non-saturer-OM.csv")
(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE.csv")

(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/list-complex-5K-all60ms-1thread-non-saturer-OM.csv")
(setq *file-freq* "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE.csv")
;OM pertuber IMOVIE: un thread => pas tout le son / 8thread cash des thread...

(setq *bpf-period* 100) ; en ms
(setq *bpf-period* 80) ; en ms
(setq *bpf-period* 1000) 
*cpt-elmt-bpf*
*begin*

(call-powerlog *file-freq* 10000)
(task-number-left *engine*)

(req-per-thread (monitor-time *engine*) 1)
(length(req-recup-all  (monitor-time *engine*)))
(req-recup-execTime  (monitor-time *engine*))
(last(req-recup-all  (monitor-time *engine*)))

(nth 1 (req-recup-all  (monitor-time *engine*)))
(apply 'min (flat(req-recup-execTime  (monitor-time *engine*))))
(apply 'max (flat(req-recup-execTime  (monitor-time *engine*))))
(apply 'min (flat(req-recup-retard  (monitor-time *engine*))))
*begin-date*
(date-to-ms *begin-date*)
*begin*


(>(date-to-ms(nth 7(nth 201(req-recup-all  (monitor-time *engine*)))))
(date-to-ms(nth 8(nth 201(req-recup-all  (monitor-time *engine*))))))
;(setq list-good (clean-list-cpu-freq ( find-nearest-begin-date(read_csv_freq "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/PwrData_2016-6-15_14-5-1.csv")) (read_csv_freq "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/PwrData_2016-6-16_21-32-32.csv")))






;;;HELP
;;;AQUI
(recup-freq-pond-per-task  (date-to-ms(nth 7(nth 0(req-recup-all  (monitor-time *engine*)))))   (date-to-ms(nth 8(nth 0(req-recup-all  (monitor-time *engine*)))))  (read_csv_freq *file-freq*)   )

(print(time(recup-freq-pond-per-task  (date-to-ms(nth 7(nth 990(req-recup-all  (monitor-time *engine*)))))   (date-to-ms(nth 8(nth 990(req-recup-all  (monitor-time *engine*)))))  (read_csv_freq *file-freq*)   )))

(print(time(recup-freq-pond-per-task-log  (date-to-ms(nth 7(nth 0(req-recup-all  (monitor-time *engine*)))))   (date-to-ms(nth 8(nth 0(req-recup-all  (monitor-time *engine*)))))  (read_csv_freq *file-freq*) (mapcar #'cadr  (read_csv_freq *file-freq*)))))



(print(time(recup-freq-pond-per-task-log  (date-to-ms(nth 7(last(req-recup-all  (monitor-time *engine*)))))   (date-to-ms(nth 8(last 0(req-recup-all  (monitor-time *engine*)))))  (read_csv_freq *file-freq*))))

(last(list(cadr(car (read_csv_freq *file-freq*)))))
(date-to-ms(nth 8(car (last (req-recup-all  (monitor-time *engine*))))))


;checking
(first(read_csv_freq *file-freq*))
(last (read_csv_freq *file-freq*))
(date-to-ms(nth 7(nth 0(req-recup-all  (monitor-time *engine*)))))
(date-to-ms(nth 8(nth 0(req-recup-all  (monitor-time *engine*)))))

recup-freq-pond-per-task-log
(recup-freq-pond-all-task)
(recup-freq-pond-all-task-log)
(print "ok")
 (read_csv_freq *file-freq*)) 
;file X nbCompu => list hz
; (recup-cpu-freq-from-file  "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/PwrData_2016-6-15_14-5-1.csv" 1000)

;(add-to-supposed-time  (monitor-time *engine*) 1000000)
; (update-freq-cpu (monitor-time *engine*)  (recup-cpu-freq-from-file  "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/PwrData_2016-6-15_14-5-1.csv" 1000))


;(write-exec-time "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-5K-1000task-2thread-non-saturer-exec-OM.dat")

 

;todo with bpf time for update outline 
;also freq...
;;;;;---------------------------------------------------------
(progn
;(update-supposed-time (monitor-time *engine*) 20000)
(add-to-supposed-time  (monitor-time *engine*) (reduce 'min (flat(req-recup-retard  (monitor-time *engine*))))))
; (update-freq-first-cpu (monitor-time *engine*) (recup-freq-first-all-task-log)))

(caddr(car (recup-freq-first-all-task-log)))
(reduce 'min '(1 2 3 4))

(update-freq-first-cpu (monitor-time *engine*) (recup-freq-first-all-task-log))
(update-freq-ponderate-cpu (monitor-time *engine*) (recup-freq-pond-all-task-log))
; (recup-cpu-freq-from-file  *file-freq* 1000)) old
; (update-freq-ponderate-cpu (monitor-time *engine*)  (recup-cpu-freq-from-file  *file-freq* 1000))

(read_csv_freq *file-freq*))

(read_csv_freq file)

;is-premier
(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task" *file-freq* "8thread-non-saturer-OM-pertuber-IMOVIE" "ok")
(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task" *file-freq* "1thread-non-saturer-OM" "ok")

;IAE
(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains" *file-freq* "4thread-non-saturer-OM" "ok")
(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains" *file-freq* "4thread-non-saturer-OM-pertuber-IMOVIE" "ok")

;list-complex
(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms" *file-freq* "1thread-non-saturer-OM" "ok")
(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms" *file-freq* "4thread-non-saturer-OM-pertuber-IMOVIE" "ok")


(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms" *file-freq* "1thread-non-saturer-OM.csv" "ok")
 ;(write-result-from (format nil "~D-~D-~D" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task" "TEST-4thread-non-saturer-OM-pertuber-IMOVIE" "freqFirst.dat")(get-freq-computation  (monitor-time *engine*)) "2D")

 ;(write-result-from (format nil "~D-~D-~D" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task" "4thread-non-saturer-OM-pertuber-IMOVIE" "freqPond.dat")(get-freq-computation  (monitor-time *engine*)) "2D")



;mix
(write-all-files "/Users/samuel/Documents/openMusic/MesuresComputationnelle/maquette-mix-2ms-100ms"  *file-freq* "8thread-non-saturer-OM" "pasok")

(if (file-exist-p  "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-1active-thread-exec.dat")
    (print "oui")
  (print "non"))
    (file


;;;isPremier non pertu
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-ponderate-exec.dat" 0.1)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-freqFirst.dat" 0.001)
;(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-ponderateMin-exec.dat" 0.1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/TEST-Cis-premier-35K-1000task-4thread-non-saturer-OM-perturber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Cis-premier-35K-1000task-4thread-non-saturer-OM-freqFirst.dat" 0.001)
Cis-premier-35K-1000task-4thread-non-saturer-OM-perturber-IMOVIE-freqFirst.dat

;;;pertu is premier
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/maquette-mix-2ms-100ms-4thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/maquette-mix-2ms-100ms-4thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/maquette-mix-2ms-100ms-4thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnellemaquette-mix-2ms-100ms-4thread-non-saturer-OM-ponderate-exec.dat" 0.1)
;(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-ponderateMin-exec.dat" 0.1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/maquette-mix-2ms-100ms-4thread-non-saturer-OM-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/maquette-mix-2ms-100ms-4thread-non-saturer-OM-ponderate-freq-exec.dat" 0.001)
)


;okok
(sys:run-shell-command  "/Applications/Intel\\ Power\\ Gadget/Intel\\(R\\)\\ Power\\ Gadget.app/Contents/MacOS/Intel\\(R\\)\\ Power\\ Gadget")
(sys:run-shell-command "/Applications/Intel\\ Power\\ Gadget/Powerlog")





;chaque tache , changement de freq ponderé



;;;;;;;;;;;computation faites
;---------------
;is-premier OM non saturer

;;;;is-premier de 15K OM  2thread
;;;is-premier de 20K OM 2thread
;;;is-premier de 17_5K OM 2thread ; "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-17_5K-1000task-2thread-non-saturer-OM-active-thread-exec.dat"
;--
;is-premier 35K OM 4thread => 3thread actif
















;;;;;;;entrevu JLG vendredi :> TODO

;;;;ARGUMENT
;1)vrai effort computationnel (en div par freq)


;;;2)comparaison des variance
;variance => pourcentage du range(max 4.5 a 9.5) => variance / range (sur pond)
;l'autre variance par range (sur non pond)

;;;3)SYSCALL sans attente de fin de computation

;;;4)open shell command (direct application intel;;;;ARGUMENT
;/Applications/Intel\ Power\ Gadget/Intel\(R\)\ Power\ Gadget.app/Contents/MacOS/Intel\(R\)\ Power\ Gadget 
;voir le man

;/Applications/Intel\ Power\ Gadget/Powerlog -duration 2000 -file  "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/bonjour.csv"


