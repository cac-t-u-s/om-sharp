(in-package :om)
;(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 5000)
;(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-freq.dat" 50)



 (save-eps-gnuplot2D "/Users/samuel/Desktop/IAE-1T-freq-hist.svg" "/Users/samuel/Desktop/IAE-1T-freq.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" "freq en Mhz" "nbElem" "time (micro sec)" "freq en Mhz" 50 67790000)

 (save-eps-gnuplot2D "/Users/samuel/Desktop/is-premier-1T-25K-freq-hist.svg" "/Users/samuel/Desktop/is-premier-1T-25K-freq.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freq.dat" "freq (Mhz)" "nbElem" "time (micro sec)" "freq (Mhz)" 1 55970000)

;template
(save-eps-gnuplot3D "/Users/samuel/Desktop/is-premier-1T-25K-hist.svg" "/Users/samuel/Desktop/is-premier-1T-25K.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-freq.dat" "freq" "nbElem" "x" "y" "z" 1000)

;pour exec
(save-eps-gnuplot2-3D "/Users/samuel/Desktop/is-premier-1T-25K-hist-exec.svg" "/Users/samuel/Desktop/is-premier-1T-25K-exec.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-ponderate-freq-exec.dat" "duree d execution pond(microsec/Mhz)" "nbElem" "temps (micro sec)" " duree d execution pond (micro sec/Mhz)" " duree d execution (micro sec) "  5000)

(save-eps-gnuplot2-3D "/Users/samuel/Desktop/is-premier-1T-25K-hist-exec.svg" "/Users/samuel/Desktop/is-premier-1T-25K-exec.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-exec.dat" "duree d execution (micro sec)" "nbElem" "temps (micro sec)" " duree d execution (micro sec)" " duree d execution (micro sec) "  5000)


;(save-eps-gnuplot2-3D "/Users/samuel/Desktop/is-premier-1T-25K-hist-exec.svg" "/Users/samuel/Desktop/is-premier-1T-25K-exec.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-exec.dat" "duree d execution (microsec)" "nbElem" "temps (micro sec)" "durée d'exec (microsec)" " duree  d execution (microsec)"  5000)


(gnuplot-2D-file-3D "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-exec.dat" 50)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-exec.dat" 50)

(gnuplot-2D-file-3D file binwidth)

(save-eps-gnuplot3D "/Users/samuel/Desktop/ok-hist.svg" "/Users/samuel/Desktop/ok-exec.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-idle.dat" "duree d execution  (micro sec)" "nbElem" "temps (micro sec)" "nbthreadactif" "\\n duree \\n d execution (micro sec) "  500)


(save-eps-gnuplot3D "ok-hist.svg" "/Users/samuel/Desktop/ok-exec.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" "duree d execution (micro sec)" "nbElem" "temps (micro sec)" "nbthreadactif" "\\n duree \\n d execution \\n (micro sec) "  500)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-exec.dat" 500)
                                                               
;(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
"/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freq.dat"


(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-ponderate-freq-exec.dat")
(* 1.0(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-exec.dat"))

(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-exec.dat")
(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat")

(* 1.0(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-exec.dat"))
(* 1.0(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-exec.dat"))

8thread nonpertu/pertu : 99 - 64
1thread nonpertu/pertu : 94.4 - 85.8