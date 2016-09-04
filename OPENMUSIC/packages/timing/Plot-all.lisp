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
;;============================================================================

;;;fichier qui permet de tracer c'est fonction, il faut avoir gnuplot au préalable...
;;;try-rule-10-pourcent rend en pourcentage l'ensemble des tâches présent entre -10 et +10 pourcent de la moyenne. (multiplier par 1.0)

(in-package :om)


(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-exec.dat")
(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-exec.dat")


;;;isPremier non pertu OM -> 25K 1000task 1thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-1active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-exec.dat" 1000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-retard.dat" 1000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-ponderate-exec.dat" 10000000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-sched.dat" 10000000)


(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-inverse-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)

; (save-eps-gnuplot2D "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freq.dat" "freq" "nbElem" "time" "freq" 1)

;;;isPremier pertu IMOVIE -> 25K 1000task 1 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 1000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
( gnuplot-2D-file-inverse-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)


(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)

;;;isPremier non pertu OM -> 25K 1000task 2thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-2active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier  pertu IMOVIE -> 25K 1000task 2 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)

;;;isPremier non pertu OM -> 25K 1000task 3thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-3active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-3active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier pertu IMOVIE -> 25K 1000task 3 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-3thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)

;;;isPremier non pertu OM -> 25K 1000task 4thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-4active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-exec.dat" (+ (* 2 966) 9666))
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier  pertu IMOVIE -> 25K 1000task 4 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 5000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)

;;;isPremier non pertu OM -> 25K 1000task 5thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-5active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-5active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier pertu IMOVIE -> 25K 1000task 5 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-5active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-5thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)

;;;isPremier non pertu OM -> 25K 1000task 6thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-6active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier  pertu IMOVIE -> 25K 1000task 6 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-7active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-6thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)

;;;isPremier non pertu OM -> 25K 1000task 7thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-7active-thread-exec.dat" 5000)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-7active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier pertu IMOVIE -> 25K 1000task 7 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-7active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-7active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)


(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-7thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)




;;;isPremier non pertu OM -> 25K 1000task 8thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-7active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-8active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-8active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-sched.dat" 0.1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)



;;;isPremier  pertu IMOVIE -> 25K 1000task 8 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-7active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)




;----IAE

;;;make-iae-grains-sam non pertu OM -> 25K 1000task 8thread
(progn 
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelleIAE-SAM-1s-all40ms-8thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-7active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-8active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-8active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)




;;;make-iae-grains-sam  pertu IMOVIE -> 25K 1000task 8 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-7active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)



;;;make-iae-grains-sam non pertu OM -> old
(progn 
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-1active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)

;;;make-iae-grains-sam  pertu IMOVIE -> old
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 10000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-1s-all40ms-1thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)





;;;make-iae-grains-sam  pertu IMOVIE -> 8 thread 25 voix en même temps
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 10000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-1thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)



;---IAE OK
;;;make-iae-grains-sam non pertu OM ->  8 thread 25 voix en même temps
(progn 
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-1active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-exec.dat" 2000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-retard.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-sched.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-8thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)




;;;make-iae-grains-sam  pertu IMOVIE -> 1 thread 25 voix en même temps
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
;le temps d'execution trop long trop de retard => pas de son (qd perturber)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 10000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-1thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)



;---IAE OK
;;;make-iae-grains-sam non pertu OM ->  8 thread 25 voix en même temps
(progn 
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-1active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-exec.dat" 2000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-retard.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-sched.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)







;;;list complex

;;;list-complex non pertu OM -> 75K 500task 8thread all 100ms
(progn 
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-sched.dat" 0.1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)




;;list-complex pertu OM -> 75K 500task 8thread all 100ms
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-4thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)


;;;make-iae-grains-sam  pertu IMOVIE -> 25 thread 25 voix en même temps
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 10000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-1thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)



;---IAE OK
;;;make-iae-grains-sam non pertu OM ->  25 thread 25 voix en même temps
(progn 
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-1active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-1active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-exec.dat" 2000)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-retard.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-freq.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-sched.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-25thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)




;;list-complex non pertu OM -> 75K 500task 1thread all 100ms


(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-1active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-exec.dat" 100)

(Gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-sched.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-75K-all100ms-1thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)

;list-complex non pertu OM -> 5K 5minute tout les 60ms 1T
(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-ponderate-freq-exec.dat")
(try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-exec.dat")


(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-1active-thread-exec.dat" 100)

(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-exec.dat" 100)

(Gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-sched.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-idle-core-percent.dat" 1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/list-complex-5K-all60ms-1thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)





















;;;;;obsolete



;;;isPremier non pertu OM -> 35K 1000task 4thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-ponderate-exec.dat" 100000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-freqFirst.dat" 0.001)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-idle-core-percent.dat" 1)
;(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-ponderateMin-exec.dat" 0.1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier  pertu IMOVIE -> 35K 1000task 4 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 10000000)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)
;(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/Bis-premier-40K-1000task-4thread-non-saturer-OM-ponderateMin-exec.dat" 0.1)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier non pertu OM -> 35K 1000task 8thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-7active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-8active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-8active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-ponderate-exec.dat" 0.1)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


;;;isPremier non pertu IMOVIE -> 35K 1000task 8 thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-5active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-6active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-7active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-8active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-exec.dat" 0.1)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-freqFirst.dat" 0.001)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-8thread-non-saturer-OM-pertuber-IMOVIE-ponderate-freq-exec.dat" 0.005)
)




;;;isPremier non pertu OM -> 35K 1000task 4thread
(progn
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-1active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-2active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-3active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-4active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-4active-thread-ponderate-exec.dat" 0.5)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-2active-thread-ponderate-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-active-thread-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-exec.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-idle.dat" 100)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-ponderate-exec.dat" 0.1)

(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-freqPond.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-freqFirst.dat" 0.001)
(gnuplot-2D-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-freq.dat" 1)
(gnuplot-file-all "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-35K-1000task-4thread-non-saturer-OM-ponderate-freq-exec.dat" 0.005)
)


