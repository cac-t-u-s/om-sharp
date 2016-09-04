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
(in-package :om)


;en réinitialisant le scheduler, on efface les données de l'execution précédente.
;-----------------------reinitialise le scheduler-------
(progn (om::abort-om-player) (om::init-om-player))
;-------------------------------------------------------

(task-number-left *engine*) ; compte le nombre tâche présente dans la file de tâches
(setq *file-freq* "/Users/CHEMIN/FONCTION.csv") ;permet d'enregister un fichier CSV à partir de Intel(R) Power Gadget (il faut l'avoir)


(setq *bpf-period* 80) ; donne une période entre chaque appel périodique (l'argument est en millisecond).

(call-powerlog *file-freq* 10000) ;laisse 10000sec pour exécuter une maquette tout en débutant de prise de données au niveau des fréquences dans le fichier csv

;;après on commence la maquette 

;fin de la prise dans la maquette

 (read_csv_freq *file-freq*) ; permet de voir ce qu'il y a dans le fichier csv

(req-recup-all  (monitor-time *engine*)) ;voir ce que contient la table de hash.

 ;; ! très important pour avoir les bons retard, mettre la date supposé à jour avec: 

(add-to-supposed-time  (monitor-time *engine*) (reduce 'min (flat(req-recup-retard  (monitor-time *engine*)))))

;Enregistrement de l'ensemble des fichiers texte avec données
(write-all-files "/Users/CHEMIN/NOM_DE_FONCTION" *file-freq* "NBTHREAD-PERTURBER-OU-NON" "ok") ; l'argument ok permet de récupérer toutes les fichiers possible, en notant "pasok" on recupere tout sauf les frequences...

;;affichage sur gnuplot à titre d'exemple changer les chemins !
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

;;on peut aussi sauvegarder les fichiers gnuplot en svg : 

;TEMPLATE 2D.
 (save-eps-gnuplot2D "/Users/samuel/Desktop/is-premier-1T-25K-freq-hist.svg" "/Users/samuel/Desktop/is-premier-1T-25K-freq.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-freq.dat" "freq (Mhz)" "nbElem" "time (micro sec)" "freq (Mhz)" 1 55970000)

;template 3D.
(save-eps-gnuplot3D "/Users/samuel/Desktop/is-premier-1T-25K-hist.svg" "/Users/samuel/Desktop/is-premier-1T-25K.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/IAE-SAM-2s-all80ms-500grains-1thread-non-saturer-OM-freq.dat" "freq" "nbElem" "x" "y" "z" 1000)

;template (cas monothread pour graphe 2D D'EXECUTION PAR EXEMPLE)
(save-eps-gnuplot2-3D "/Users/samuel/Desktop/is-premier-1T-25K-hist-exec.svg" "/Users/samuel/Desktop/is-premier-1T-25K-exec.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-ponderate-freq-exec.dat" "duree d execution pond(microsec/Mhz)" "nbElem" "temps (micro sec)" " duree d execution pond (micro sec/Mhz)" " duree d execution (micro sec) "  5000)



