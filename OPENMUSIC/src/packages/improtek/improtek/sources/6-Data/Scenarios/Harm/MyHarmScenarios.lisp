; J. Nika, Nov. 2011
;
; Déclaration des grilles connues par le logiciel

(in-package :om)

;--------------------------------------------------------------------------------------------------------------------
(defparameter *available-grids* (make-hash-table :test #'string= ))

(defun bpmtobeatdur (bpm) (round (/ 60000 bpm)))

(defmethod improtest-current-tune () 
            (beats->chseq (mix-poly-impro *current-tune* (oracle *current-tune*)) (beatduration *current-tune*) 0)) ;for OM patch 'improtest-current-tune'

; (d 7 4) => (d 7)(d 7)(d 7)(d 7)
(defun expand_grid (grid)
  (setf expanded_grid 
        (loop for i from 0 to (1- (list-length grid)) 
              append 
              (if (> (list-length (nth i grid)) 2)
                  (loop for j from 0 to (1- (nth 2 (nth i grid))) 
                        collect
                        (list (nth 0 (nth i grid)) (nth 1 (nth i grid)))
                        ) (list (nth i grid))))))

(defun make-oracle-from-beatlist (beatlist) (NewImprovizer beatlist))
;--------------------------------------------------------------------------------------------------------------------


#|
;AmantsDunJour, BPM=126
;--------------------------------------------------------------------------------------------------------------------
(setf AmantsDunJour_grid 
      '( (d m7 12) (a 7 12)(d m7 12) (a 7 12) (d maj7 12) (a 7 12) (d maj7 12)(a 7 12) (d maj7 3) (g maj7 3)(d maj7 3) (g maj7 3)(d maj7 3) (g maj7 3) (e 7 6) (a 7 3)(d maj7 9) (a 7 12) (d maj7 12)(a 7 12) (d maj7 1) (d m7 11) (a 7 12) (d m7 12) (a 7 12) (d maj7 12) (a 7 12) (d maj7 12) (a 7 12) (d maj7 3) (g maj7 3)(d maj7 3) (g maj7 3)(d maj7 3) (g maj7 3) (e 7 6) (a 7 3)(d maj7 9) (a 7 12) (d maj7 12)(a 7 12) (d maj7 1) (d m7 11) (a 7 12)(d m7 12) (a 7 12) (d m7 12) (a 7 12) (d m7 3) )
                                                        
      AmantsDunJour_beatdur (bpmtobeatdur 126))
(setf AmantsDunJour_tune (make-instance 'tune :grid AmantsDunJour_grid :beatduration AmantsDunJour_beatdur :tunename "AmantsDunJour" :NbBeatsPerMeasure 3))
(setf (gethash '"AmantsDunJour" *available-grids*) AmantsDunJour_tune)



;StLouisBlues, BPM=90
;--------------------------------------------------------------------------------------------------------------------

(setf StLouisBlues_grid 
     '( (c 7 4) (f 7 4) (c 7 8) (f 7 8) (c 7 8) (g 7 4) (f 7 4) (c 7 8) (c 7 4) (f 7 4) (c 7 8) (f 7 8) (c 7 8) (g 7 4) (f 7 4) (c 7 8) (c m7 4) (f m7 4) (g 7 16) (c m7 12) (f m7 4) (g 7 16) (c m7 2) (d 7 2) (g 7 4) (c 7 16) (f 7 8) (c 7 8) (g 7 8) (c 7 8) (c 7 16) (f 7 8) (c 7 8) (g 7 8) (c 7 8) (c 7 4) (f 7 4) (c 7 8) (f 7 8) (c 7 8) (g 7 4) (f 7 4) (c 7 8))
                                                        
      StLouisBlues_beatdur (bpmtobeatdur 90))
(setf StLouisBlues_tune (make-instance 'tune :grid StLouisBlues_grid :beatduration StLouisBlues_beatdur :tunename "StLouisBlues" :NbBeatsPerMeasure 4))
(setf (gethash '"StLouisBlues" *available-grids*) StLouisBlues_tune)

|#

#|
;MyRentPartyLong
;--------------------------------------------------------------------------------------------------------------------
(setf 
	MyRentPartyLong_grid (quote (
                                     ;(2 0)
                                     (b m7)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 1)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(0 2)

                                     ;(2 2)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 1)
                                     ) )
	MyRentPartyLong_beatdur (bpmtobeatdur 87))
	(setf clusters_centroids (list :Chroma (list  0.478 0.24162 0.67584 ) :TonalCentroid (list  -0.0034029 -0.57528 -0.70782 ) ))
(setf MyRentPartyLong_tune (make-instance (quote realtimetune) :grid MyRentPartyLong_grid :chapters (quote (1)) :beatduration MyRentPartyLong_beatdur :tunename "MyRentPartyLong" :NbBeatsPerMeasure 4 :DataFromAnalysis  (list  0.478 0.24162 0.67584 ) :chapters '(1 33)))
(setf (gethash (quote "MyRentPartyLong") *available-grids*) MyRentPartyLong_tune)
|#


;MyRentPartyLong
;--------------------------------------------------------------------------------------------------------------------
(setf 
	MyRentPartyLong_grid (quote (
                                     ;(2 0)
                                     (f rien3 3) (bb deb 1)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 1)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(0 2)

                                     ;(2 2)(1 1)(2 0)
                                     (c m7final 2) (bb 7final 2) ;(ab maj7 2) (bb 7 1)
                                     ) )
	MyRentPartyLong_beatdur (bpmtobeatdur 87))
	(setf clusters_centroids (list :Chroma (list  0.478 0.24162 0.67584 ) :TonalCentroid (list  -0.0034029 -0.57528 -0.70782 ) ))
(setf MyRentPartyLong_tune (make-instance (quote realtimetune) :grid MyRentPartyLong_grid :beatduration MyRentPartyLong_beatdur :tunename "MyRentPartyLong" :NbBeatsPerMeasure 4 :DataFromAnalysis  (list  0.478 0.24162 0.67584 ) :chapters '(1 33)))
(setf (gethash (quote "MyRentPartyLong") *available-grids*) MyRentPartyLong_tune)

;MyRentPartyLong-MEM
;--------------------------------------------------------------------------------------------------------------------
(setf 
	MyRentPartyLong-MEM_grid (quote (
                                     ;(2 0)
                                     (bb deb 1)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 1)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(0 2)

                                     ;(2 2)(1 1)(2 0)
                                     (c m7final 2) (bb 7final 2) (f rien) (f rien2) (f rien3)
                                     ) )
	MyRentPartyLong-MEM_beatdur (bpmtobeatdur 87))
	(setf clusters_centroids (list :Chroma (list  0.478 0.24162 0.67584 ) :TonalCentroid (list  -0.0034029 -0.57528 -0.70782 ) ))
(setf MyRentPartyLong-MEM_tune (make-instance (quote realtimetune) :grid MyRentPartyLong-MEM_grid :beatduration MyRentPartyLong-MEM_beatdur :tunename "MyRentPartyLong-MEM" :NbBeatsPerMeasure 4 :DataFromAnalysis  (list  0.478 0.24162 0.67584 ) :chapters '(1 33)))
(setf (gethash (quote "MyRentPartyLong-MEM") *available-grids*) MyRentPartyLong-MEM_tune)



#|
;Oleo
;--------------------------------------------------------------------------------------------------------------------
(setf Oleo_grid '(
                        ;A1
                        (bb maj7 2) (g m7 2) (c m7 2) (f 7 2) 
                        (bb maj7 2) (g 7 2) (c m7 2) (f 7 2)
                        (bb maj7 2) (bb 7 2) (eb maj7 4)
                        (d m7 2) (g m7 2) (c m7 2) (f 7 2)
                        ;A2
                        (bb maj7 2) (g m7 2) (c m7 2) (f 7 2) 
                        (bb maj7 2) (g 7 2) (c m7 2) (f 7 2)
                        (bb maj7 2) (bb 7 2) (eb maj7 4)
                        (d m7 2) (g m7 2) (c m7 1) (f 7 1) (bb maj7 2)
                        ;B
                        (d 7 8) (g 7 8) (c 7 8) (f 7 8)
                        ;C = A2
                        (bb maj7 2) (g m7 2) (c m7 2) (f 7 2) 
                        (bb maj7 2) (g 7 2) (c m7 2) (f 7 2)
                        (bb maj7 2) (bb 7 2) (eb maj7 4)
                        (d m7 2) (g m7 2) (c m7 1) (f 7 1) (bb maj7 2)
                        
)
                                                        
      Oleo_beatdur (bpmtobeatdur 224))   ;BPM=190, beatdur= 315 ms
(setf Oleo_tune (make-instance 'tune :grid Oleo_grid :beatduration Oleo_beatdur :tunename "Oleo"))
(setf (gethash '"Oleo" *available-grids*) Oleo_tune)
|#


;MyRentPartyLong-loop
;--------------------------------------------------------------------------------------------------------------------
(setf 
	MyRentPartyLong-loop_grid (quote (
                                     ;(2 0)
                                     ;(f rien3 3) (bb deb 1)
                                     (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(2 1)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(2 0)

                                     ;(0 2)(0 2)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(1 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(1 1)

                                     ;(0 2)(0 2)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(1 0)(2 0)(1 1)(1 1)

                                     ;(0 2)(2 0)(1 1)(2 1)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 1)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 0)(1 1)(0 2)

                                     ;(2 0)(2 0)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) (ab maj7 2) (bb 7 2)
                                     ;(2 0)(2 2)(1 1)(0 2)

                                     ;(2 2)(1 1)(2 0)
                                     (c m7 2) (bb 7 2) ;(ab maj7 2) (bb 7 1)
                                     ) )
	MyRentPartyLong-loop_beatdur (bpmtobeatdur 87))
	(setf clusters_centroids (list :Chroma (list  0.478 0.24162 0.67584 ) :TonalCentroid (list  -0.0034029 -0.57528 -0.70782 ) ))
(setf MyRentPartyLong-loop_tune (make-instance (quote realtimetune) :grid MyRentPartyLong-loop_grid :beatduration MyRentPartyLong-loop_beatdur :tunename "MyRentPartyLong-loop" :NbBeatsPerMeasure 4 :DataFromAnalysis  (list  0.478 0.24162 0.67584 ) :chapters '(1 33)))
(setf (gethash (quote "MyRentPartyLong-loop") *available-grids*) MyRentPartyLong-loop_tune)



;MyOleo
;--------------------------------------------------------------------------------------------------------------------
(setf MyOleo_grid '(
                        ;B'
                        (d 7 4) (g 7 4) (c 7 4) (f 7 4)
                        ;A1
                        (bb maj7 2) (g m7 2) (c m7 2) (f 7 2) 
                        (bb maj7 2) (g 7 2) (c m7 2) (f 7 2)
                        (bb maj7 2) (bb 7 2) (eb maj7 4)
                        (d m7 2) (g m7 2) (c m7 2) (f 7 2)
                        ;A2
                        (bb maj7 2) (g m7 2) (c m7 2) (f 7 2) 
                        (bb maj7 2) (g 7 2) (c m7 2) (f 7 2)
                        (bb maj7 2) (bb 7 2) (eb maj7 4)
                        (d m7 2) (g m7 2) (c m7 1) (f 7 1) (bb maj7 2)
                        ;B
                        (d 7 8) (g 7 8) (c 7 8) (f 7 8)
                        ;C = A2
                        (bb maj7 2) (g m7 2) (c m7 2) (f 7 2) 
                        (bb maj7 2) (g 7 2) (c m7 2) (f 7 2)
                        (bb maj7 2) (bb 7 2) (eb maj7 4)
                        (d m7 2) (g m7 2) (c m7 1) (f 7 1) (bb maj7 2)
                        
)
                                                        
      MyOleo_beatdur (bpmtobeatdur 140))
(setf MyOleo_tune (make-instance 'tune :grid MyOleo_grid :beatduration MyOleo_beatdur :tunename "MyOleo"))
(setf (gethash '"MyOleo" *available-grids*) MyOleo_tune)