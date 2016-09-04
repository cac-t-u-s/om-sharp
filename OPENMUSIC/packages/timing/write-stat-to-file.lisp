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

;resumer trier les clef dans l'ordre et regenerer dans l'ordre la table de hashage

(defun write-all-in-file(file &optional function arg runtime totaltime taskinterval)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "function ~s arg ~A tempsRun ~A DernierComputation ~A taskinterval ~A ~%" function arg runtime totaltime taskinterval)

    (format stream "execTime ")
    (format stream (numlist-to-string (flatten(princ (req-recup-execTime  (monitor-time *engine*))))))
    (format stream "~%")

    (format stream "begin-time ~%")
    (setq cpt 0)
    (loop for i from 1 to (length(workers *engine*)) do
          (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
              (print "nil")
            (setq cpt (+ cpt 1))))
    (princ cpt)

    (loop for i from 1 to cpt do
          (format stream "thread-~A " i)
          (format stream (numlist-to-string(mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))))
          (format stream  "~%"))

    (format stream "exec ~%")
    (loop for i from 1 to cpt do
      
          (format stream "thread-~A " i)
          (format stream 
                  ( numlist-to-string(mapcar #'(lambda(x) (- (cadr(car x))(car(car x)))) (sort ( req-per-thread (monitor-time *engine*) (parse-integer(format nil "~A" i))) #'(lambda(x y) (< (car(car x)) (car(car y))))))))
          (format stream  "~%")
          )

    (format stream "supposed  ~%" )
    (loop for i from 1 to cpt do
      
          (format stream "thread-~A " i)
          (format stream ( numlist-to-string(mapcar #'truncate(mapcar #'(lambda(x) (cadddr(car x)))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))))
          (format stream  "~%")
          )



    (format stream "retard ~%" )
    (loop for i from 1 to cpt do
      
          (format stream "thread-~A " i)
          (format stream ( numlist-to-string(mapcar #'truncate(mapcar #'(lambda(x) (- (car(car x))(cadddr(car x))))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))))
          (format stream  "~%")
          )
 
; (format stream "exec-sched ")
;   (format stream ( numlist-to-string (mapcar #'(lambda(x) (-  (cadr x) (car x))) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))))
;(format stream  "~%")

;(format stream "begin-time-sched ")
;(format stream (numlist-to-string(mapcar #'(lambda(x)  (car x)) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))))


    (format stream " ~%fin ~%")
  
    )
  )

;; ? to erase?
(defun write-testColonne-sched-exec-in-file(file)
(with-open-file (stream file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format stream "execsched ~%")
(loop for elt in(mapcar #'(lambda(x) (-  (cadr x) (car x))) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*)))) do   (format stream "~A" elt)  (format stream  "~%")
)))
;;;; ? to erase ?
(defun write-testColonne2-sched-begin-in-file(file)
(with-open-file (stream file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format stream "execsched ~%")
(loop for elt in(mapcar #'(lambda(x)  (car x)) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*)))) do   (format stream "~A" elt)  (format stream  "~%")
)))

;;;;did changing time here <==== TO TEST AND TO DO TO OTHER FUNCTION
(defun write-exec-time(file)
  (let((cpt 0) (lock 0))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)

      (loop for i from 1 to (length(workers *engine*)) do
            (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort (req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
                (print "nil")
              (setq cpt (+ cpt 1))))
      (format stream "#begin threadNum exec ~%" )
      (loop for i from 1 to (+ 1 cpt) do
            (loop for elt in (mapcar #'(lambda(x y) (list x y))
                                     (mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                     (mapcar #'truncate (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                
                  (if (eq lock 0)
                      (progn
                        (setq beginTime (car elt))
                        (setq lock 1 )))

                  (format stream "~A ~A ~A " (-(car elt) beginTime) i (cadr elt) )       
                  (format stream  "~%")

                  )
        ;  (format stream  "~%")
            ))))

;;;inutile !
(defun write-exec-time-special-zero-pad(file)
     (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
  (setq cpt 0)  (setq lock 0)
    (loop for i from 1 to (length(workers *engine*)) do
          (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
              (print "nil")
            (setq cpt (+ cpt 1))))
       (format stream "#begin threadNum retard ~%" )
    (loop for i from 1 to cpt do
        
          (loop for elt in (mapcar #'(lambda(x y) (list x y))
(mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
(mapcar #'truncate (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                    
                (progn
                           (format stream "~A ~A ~A " (car elt) i (cadr elt) )       
                           (format stream  "~%"))
               (loop for j from 1 to (length(workers *engine*)) do
                     (if(not (eq i j))      
                       (progn
                           (format stream "~A ~A ~A " (car elt) j 0 )       
                           (format stream  "~%"))))

          )
        
)))

(defun write-retard-time(file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (setq cpt 0)  (setq lock 0)
    (loop for i from 1 to (length(workers *engine*)) do
          (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
              (print "nil")
            (setq cpt (+ cpt 1))))
    (format stream "#begin threadNum retard ~%" )
    (loop for i from 1 to cpt do
        
          (loop for elt in (mapcar #'(lambda(x y) (list x y))
                                   (mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                   (mapcar #'truncate (mapcar #'(lambda(x) (- (car(car x))(cadddr(car x))))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                
                (if (eq lock 0)
                    (progn
                      (setq beginTime (car elt))
                      (setq lock 1 )))

                (format stream "~A ~A ~A " (-(car elt) beginTime) i (cadr elt) )       
                (format stream  "~%")
       
                )
         ; (format stream  "~%")
          )))



(defun write-sched-time(file)
  
  (while(if(not (listp (car (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))))
            (pop (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))))
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (setq lock 0)
  ;  (if (length (pop (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*)))) popper toris fois...
    (format stream "#begintime  numUnique execsched  ~%")
    (loop for elt in (mapcar #'(lambda(x y)(list x y))(mapcar #'(lambda(x)  (car x)) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))  (mapcar #'(lambda(x) (-  (cadr x) (car x))) (gethash 'schedkey(hash-exec (scheduler-monitor-time *scheduler*))))) do 
          (if (eq lock 0)
              (progn
                (setq beginTime (car elt))
                (setq lock 1 )))

          (format stream "~A ~A ~A " (-(car elt) beginTime) (cadr elt) 0 )  
 ; (format stream "~A ~A ~A" (car elt) (cadr elt) 0)
          (format stream  "~%")
          )))


(defun write-activeThread-time(file)
  (let ((cpt 0) (lock 0) (list-nb-actf nil))
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
   
    (loop for i from 1 to (length(workers *engine*)) do
          (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort (  req-active-thread(monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
              (print "nil")
            ;(setq cpt (+ cpt 1))
            (push i list-nb-actf)
            ))
    (format stream "#begin nbActifThread exec ~%" )
    (loop for i in list-nb-actf do
        
          (loop for elt in (mapcar #'(lambda(x y) (list x y))
                                      (mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                   (mapcar #'truncate (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort (  req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                
                (if (eq lock 0)
                    (progn
                      (setq beginTime (car elt))
                       (princ "in exectime")
                      (princ beginTime)
                      (setq lock 1 )))

                (format stream "~A ~A ~A " (-(car elt) beginTime) i (cadr elt) )  
               ; (format stream "~A ~A ~A " (car elt) i (cadr elt) )       
                (format stream  "~%")

                )
         ; (format stream  "~%")
          ))))



(defun write-activeThread-retard(file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)

     
    (setq cpt 0)  (setq lock 0)
    (loop for i from 1 to (length(workers *engine*)) do
          (print(length(mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))))
          (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
              (print "nil")
            (setq cpt (+ cpt 1))))
    (format stream "#begin nbActifThread retard  ~%")
    (loop for i from 1 to (+ cpt 1) do
        
          (loop for elt in (mapcar #'(lambda(x y) (list x y))
                                   (mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                   (mapcar #'truncate (mapcar #'(lambda(x) (- (car(car x))(cadddr(car x))))  (sort ( req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                
                (if (eq lock 0)
                    (progn
                      (setq beginTime (car elt))
                      (princ "in retard")
                      (princ beginTime)
                      (setq lock 1 )))

                (format stream "~A ~A ~A " (-(car elt) beginTime) i (cadr elt) )       
                (format stream  "~%")
       
                )
        
          ))   
  )

(defun write-n-activeThread-time(file n)
  (let ((cpt 0) (lock 0) (list-nb-actf nil))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
   
    
      (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort (  req-active-thread(monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
          (print "nil")
            ;(setq cpt (+ cpt 1))
        (push n list-nb-actf)
        )
      (format stream "#begin nbActifThread exec ~%" )
      (loop for i in list-nb-actf do
        
            (loop for elt in (mapcar #'(lambda(x y) (list x y))
                                     (mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                     (mapcar #'truncate (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort (  req-active-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                
                  (if (eq lock 0)
                      (progn
                        (setq beginTime (car elt))
                        (princ "in exectime")
                        (princ beginTime)
                        (setq lock 1 )))

                  (format stream "~A ~A ~A " (-(car elt) beginTime) i (cadr elt) )  
               ; (format stream "~A ~A ~A " (car elt) i (cadr elt) )       
                  (format stream  "~%")

                  )
         ; (format stream  "~%")
            ))))



(defun write-n-activeThread-retard(file n)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)

     
    (setq cpt 0)  (setq lock 0)

    (print(length(mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y))))))))
    (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
        (print "nil")
      (setq cpt (+ cpt 1)))
    (format stream "#begin %DemeActifThread retard  ~%" n)
     
        
          (loop for elt in (mapcar #'(lambda(x y) (list x y))
                                   (mapcar #'(lambda(x) (car(car x))) (sort ( req-active-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                   (mapcar #'truncate (mapcar #'(lambda(x) (- (car(car x))(cadddr(car x))))  (sort ( req-active-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                
                (if (eq lock 0)
                    (progn
                      (setq beginTime (car elt))
                      (princ "in retard")
                      (princ beginTime)
                      (setq lock 1 )))

                (format stream "~A ~A ~A " (-(car elt) beginTime) n (cadr elt) )       
                (format stream  "~%")
       
                )
        
          )   
  )

;idle time =>begin de la tache moins temps de fin du précédent 
;perthread...not really
(defun write-idle-time(file)
  (let ((elt_before nil) (cpt 0) (lock 0))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)

      (loop for i from 1 to (length(workers *engine*)) do
            (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort (req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
                (print "nil")
              (setq cpt (+ cpt 1))))
      (format stream "#beginidletime threadNum idletime ~%" )
      (loop for i from 1 to (+ 1 cpt) do
            (loop for elt in (mapcar #'(lambda(x y) (list x y))
                                     (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))
                                     (mapcar #'truncate (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))) do                
                  (if (eq lock 0)
                      (progn
                        (setq beginTime (car elt))
                        (setq lock 1 )))
                  (if (not(eq elt_before nil))
                      (progn
                      
                        (format stream "~A ~A ~A " (cadr(car(car elt_before))) i (-(car(car (car elt)))  (cadr(car(car elt_before))) ) )       
                        (format stream  "~%")
                
                        ))
                  (setq elt_before elt))
            (setq elt_before nil)
           ; (format stream  "~%")
            ))))

;;;;approximation ne change pas pendant l'exec => pas de moyen celle du début plutôt
;;;; la plus grande freq pendant la tâche
;;;; la plus petite pendant
(defun write-ponderate-time-freq(file)
  (let((cpt 0) (lock 0)(value-freq 0))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)

      (loop for i from 1 to (length(workers *engine*)) do
            (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort (req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
                (print "nil")
              (setq cpt (+ cpt 1))))
      (format stream "#begin threadNum execFreqPond ~%" )
      (loop for i from 1 to (+ 1 cpt) do
            (loop for elt in (mapcar #'(lambda(x y z) (list x y z))
                                     (mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                     (mapcar #'truncate (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))
                                      (mapcar #'(lambda(x) (nth 4 (car x)) )(sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                     ) do                
                  (if (eq lock 0)
                      (progn
                        (setq beginTime (car elt))
                        (setq lock 1 )))
                  (if (not(equal (caddr elt) 0)  )                  
                      ;(format stream "~A ~A ~A "(-(car elt) beginTime) i 0) 
                    (format stream "~A ~A ~A ~%"(-(car elt) beginTime) i (* 1.0 (/(cadr elt)(caddr elt)) ))) 
                     
                  ;(format stream  "~%")

                  )
        ;  (format stream  "~%")
            ))))


;non
(defun write-ponderate-time-freq2(file)
 (let((cpt 0) (lock 0)(min-freq  (apply 'min (mapcar #'truncate (mapcar #'(lambda(x) (nth 4 (car x)) )(sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))))))
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)

    (loop for i from 1 to (length(workers *engine*)) do
          (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort (req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
              (print "nil")
            (setq cpt (+ cpt 1))))
    (format stream "#begin threadNum execFreqPond ~%" )
    (loop for i from 1 to (+ 1 cpt) do
          (loop for elt in (mapcar #'(lambda(x y z) (list x y z))
                                   (mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                                   (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y))))))
(mapcar #'truncate (mapcar #'(lambda(x) (nth 4 (car x)) )(sort ( req-per-thread (monitor-time *engine*) i) #'(lambda(x y) (< (car(car x)) (car(car y)))))))
) do             

                (if (eq lock 0)
                    (progn
                      (setq beginTime (car elt))
                      (setq lock 1 )))

                (format stream "~A ~A ~A "(-(car elt) beginTime) i (* 1.0 (/(cadr elt)min-freq) ))       
                (format stream  "~%")

                )
        ;  (format stream  "~%")
          ))))


(defun write-n-active-thread-ponderate-time(file n)
  (let((cpt 0) (lock 0))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)


      (if (eq (length(mapcar #'(lambda(x) (car(car x))) (sort (req-per-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y))))))) 0)
          (print "nil")
        (setq cpt (+ cpt 1)))
      (format stream "#begin threadNum execFreqPond ~%" )

      (loop for elt in (mapcar #'(lambda(x y z) (list x y z))
                               (mapcar #'(lambda(x) (car(car x))) (sort ( req-per-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                               (mapcar #'truncate (mapcar #'(lambda(x) (- (cadr(car x))(car(car x))))  (sort ( req-per-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y)))))))
                               (mapcar #'(lambda(x) (nth 4 (car x)) )(sort ( req-per-thread (monitor-time *engine*) n) #'(lambda(x y) (< (car(car x)) (car(car y))))))
                               ) do                
            (if (eq lock 0)
                (progn
                  (setq beginTime (car elt))
                  (setq lock 1 )))
            (if (not (equal (caddr elt) 0))
             ;  (format stream "~A ~A ~A "(-(car elt) beginTime) n 0) 
              (format stream "~A ~A ~A "(-(car elt) beginTime) n (* 1.0 (/(cadr elt)(caddr elt)))))    
            (format stream  "~%")

            )
        ;  (format stream  "~%")
      )))

;res format begin threadN res
(defun write-result-from(file res type)
  (let ((elt_before nil) (cpt 0) (lock 0))
    (if (equal type "2D")
        (with-open-file (stream file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)    
          (format stream "#begin freq" )
          (format stream  "~%")
          (loop for elt in res do
                (if (not (equal (car elt) nil))
               ;   (format stream "~D ~D~%"  (cadr elt) 0)   
                (format stream "~D ~D~%"  (cadr elt) (car elt))))
          ))
        (if (equal type "2D-percent")
        (with-open-file (stream file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)    
          (format stream "#begin percent" )
          (format stream  "~%")
          (loop for elt in res do
                (if (not (equal (car elt) nil))
               ;   (format stream "~D ~D~%"  (cadr elt) 0)   
                (format stream "~D ~D~%"  (car elt) (cadr elt))))
          ))
  (if (equal type "3D")
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "#begin thread pond-freq-exec")
        (format stream  "~%")
        (loop for elt in res do
              (if (not(equal (caddr elt) nil))
            ;      (format stream "~D ~D ~D"  (car elt) (cadr elt)0)
                (format stream "~D ~D ~D"  (car elt) (cadr elt)(caddr elt)))
              (format stream "~%")          
              ))))
)




;file X type -> nil 
;ecrit des fichiers exec sched
;type :example OM-non-sature-Pertuber-IMOVIE

(defun write-all-files(file filefreq type other)

  (write-result-from (format nil "~D-~D-~D" file type "freq.dat") (read_csv_freq filefreq) "2D")
  (write-result-from  (format nil "~D-~D-~D" file type "idle-core-percent.dat") (recup-idle-core-percent) "2D-percent")
  (write-result-from (format nil "~D-~D-~D" file type "ponderate-freq-exec.dat") ( recup-freq-pond-all-task-log) "3D")
  (write-all-in-file  (format nil "~D-~D-~D" file type "all.dat"))
  (write-exec-time (format nil "~D-~D-~D" file type "exec.dat"))
  (write-retard-time (format nil "~D-~D-~D" file  type "retard.dat"))
  (write-sched-time (format nil "~D-~D-~D" file type "sched.dat"))
  (write-activeThread-time (format nil "~D-~D-~D" file type "active-thread-exec.dat"))
  (write-activeThread-retard (format nil "~D-~D-~D" file type "active-thread-retard.dat"))
  (write-idle-time (format nil "~D-~D-~D" file type "idle.dat"))
   (update-freq-first-cpu (monitor-time *engine*) (recup-freq-first-all-task-log))
  (if (equal other "ok")
      (progn
        (write-ponderate-time-freq (format nil "~D-~D-~D" file type "ponderate-exec.dat"))
  ;  (write-ponderate-time-freq2 (format nil "~D-~D-~D" file type "ponderateMin-exec.dat"))
        (loop for i from 1 to (length (workers *engine*)) do
              (write-n-activeThread-time (format nil "~D-~D-~D~D" file type i "active-thread-exec.dat") i)
              (write-n-activeThread-retard  (format nil "~D-~D-~D~D" file type i "active-thread-retard.dat") i)
              (write-n-active-thread-ponderate-time (format nil "~D-~D-~D~D" file type i "active-thread-ponderate-exec.dat") i)
              )))

 (write-result-from (format nil "~D-~D-~D" file type "freqFirst.dat")(get-freq-computation  (monitor-time *engine*)) "2D")


(update-freq-ponderate-cpu (monitor-time *engine*) (recup-freq-pond-all-task-log))
 (write-result-from (format nil "~D-~D-~D" file type "freqPond.dat")(get-freq-computation  (monitor-time *engine*)) "2D")

)

;--------------------------------------------------------powerlog cmd--------------------------
;"~D-~D-~D"
(defun call-powerlog(file timeSleep)
(sys:run-shell-command  (format nil  "/Applications/Intel\\ Power\\ Gadget/PowerLog -resolution 20 -file ~D -cmd sleep ~D" file timeSleep ))
)

(defun kill-powerlog()
(sys:run-shell-command "pkill -9 sleep")
)

;(call-powerlog "/Users/samuel/Documents/openmusic/MesuresComputationnelle/AlogFile/HEY3.csv" 1000)
;  (kill-powerlog)


;-----------------------reading csv-----------------------------------------------

(defun remove-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list))) 

(defun remove-all (lst elt)
(let ((cpt 0))
  (loop for stuff in lst do
        (if (equal stuff elt)
          (setq lst  (remove-nth cpt lst))
          (setq cpt (+ 1 cpt)))))
lst)



(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

;list freq heure


(defun find_CPU_frequency(str)
  (list (parse-integer(nth 3
                           (remove-all (split-str str ", ")   ""))) (date-to-ms (nth 0
                                                                                               (remove-all (split-str str ", ")   "")))))



;list freq(MHZ) heure (Timestamp /1000 => microsec)
(defun read_csv_freq(file)
  (let ((in (open file :if-does-not-exist nil)) (list-freq-timestamp nil))
    (when in
      (read-line in nil)
      (setq list-freq-timestamp  (loop for line = (read-line in nil)
                                       while (not(eq  (length(split-str line ", ")) 1))  do (format t "~a~%" line)
                                       collect (find_CPU_frequency line)
                                       ))
      (close in))
    list-freq-timestamp))

(defun date-to-ms(str-date)
  (let ((val-date (split-str str-date ":"))(value-in-ms 0))
        (setq value-in-ms (+ (+ (+ (* 60 (* 60 (* 1000 (parse-integer(nth 0 val-date)))))
                                   (* 60 (* 1000 (parse-integer(nth 1 val-date)))))
                                (* 1000 (parse-integer (nth 2 val-date))))
                             (parse-integer (nth 3 val-date))))
    value-in-ms    
    ))

(defun find-nearest-begin-date(list-freq-heure)
  (let ((value nil))
    (loop for x in list-freq-heure do
          (if (> (cadr x) (date-to-ms *begin-date*))
              (return)
            (setq value (cadr x))))
    value))

(defun clean-list-cpu-freq(nearest-begin list-freq-heure)
  (let ((new-list list-freq-heure))
    (print new-list)
   (loop for x in new-list do
          (if (< (cadr x) nearest-begin)
              (pop new-list)
              (return)
            ))
    new-list)
)



;;;dépend si ça tombe au milieu ou pas...to see...
(defun interpol-cpu-freq(cleanList nbComputation)
  (loop for i from 0 to (- nbComputation 1) collect
        (/(+(car (nth i cleanList)) (car (nth (+ i 1) cleanList))) 2)))
        


(defun recup-cpu-freq-from-file(file nbcomputation) 
(interpol-cpu-freq(clean-list-cpu-freq ( find-nearest-begin-date(read_csv_freq file)) (read_csv_freq file)) nbcomputation))



;;recherche dichotomique time stamp
(defun binary-search (value array)
  (let ((low 0)
        (high (1- (length array))))
 
    (do () ((< high low) nil)
      (let ((middle (floor (+ low high) 2)))
 
        (cond ((> (aref array middle) value)
               (setf high (1- middle)))
 
              ((< (aref array middle) value)
               (setf low (1+ middle)))
 
              (t (return middle)))))))

(defun list-to-1d-array (list)
       (make-array (length list)
              :initial-contents list))

(defun list-to-2d-array (list)
       (make-array  (length list)
                    (length(first list))
              :initial-contents list))




;IxIxlistnI => list2I
(defun recup-nth-dicho(begin end vector-time-stamp)
  (let ((begin-test begin)(end-test end) (search-begin nil)(search-end nil))

    (while(equal search-begin nil)
      (if  (equal search-begin nil)
          (progn
            (setq search-begin  (binary-search begin-test vector-time-stamp )  )
            (setq begin-test (- begin-test 1)))))

    (while  (equal search-end nil)
      (if  (equal search-end nil)
          (progn
            (setq search-end  (binary-search end-test vector-time-stamp )  )
            (setq end-test (- end-test 1)))))

    (list search-begin search-end)))
     
      
      


(defun recup-freq-pond-per-task-log(begin end list-time-stamp list-time-freq)
  (let ((begin-end-nth  (recup-nth-dicho begin end (list-to-1d-array (mapcar #'cadr list-time-stamp))))
        (result 0))
    (print begin-end-nth)
        (loop for i from (car begin-end-nth)to  (cadr begin-end-nth) do

          (if (equal i  (car begin-end-nth))
              (setq result (+ result (/ (- (cadr(nth (+ i 1) list-time-stamp)) begin) (car (nth i list-time-stamp)))))
            )

          (if (equal i (cadr begin-end-nth))
                  (setq result   (+ result (/ (- end (cadr(nth  i  list-time-stamp)))  (car (nth i list-time-stamp)))))
            )

          (if (not (or (equal i  (car begin-end-nth)) (equal i  (cadr begin-end-nth))))
               (setq result   (+ result (/ (- (cadr(nth (+ i 1) list-time-stamp))   (cadr(nth  i list-time-stamp))) (car (nth i list-time-stamp)))))
      
            )
          )(print "ok1")(* 1.0 result))
)

(defun recup-freq-first-per-task-log(begin end list-time-stamp list-time-freq)
  (let ((begin-end-nth  (recup-nth-dicho begin end (list-to-1d-array (mapcar #'cadr list-time-stamp))))
        (result 0))
    (print begin-end-nth)
        (loop for i from (car begin-end-nth)to  (cadr begin-end-nth) do

          (if (equal i  (car begin-end-nth))
              (setq result (+ result (/ (- (cadr(nth (+ i 1) list-time-stamp)) begin) (car (nth i list-time-stamp)))))
            ) 
          )(* 1.0 result))
)


(defun recup-idle-core-percent()
  (mapcar #'(lambda(x) (list (car x) (nth 9 x)))(req-recup-all (monitor-time *engine*)))
)



;;;not working
(defun recup-freq-pond-per-task(begin end list-time-stamp)
  (let* (( list-time  (mapcar #'cadr list-time-stamp))
         (list-less-then-begin  (mapcar #'(lambda (x )(- x begin)) list-time))
         (list-less-then-end (mapcar #'(lambda (x )(- x end )) list-time))
         (pos-less-begin (- (length  (remove-if-not #'minusp  list-less-then-begin)) 1))
         (pos-less-end (-(length(remove-if-not #'minusp list-less-then-end))1))
         (result 0))
       
    (setq list-less-begin (list pos-less-begin pos-less-end ))

    (loop for i from pos-less-begin to pos-less-end do

          (if (equal i pos-less-begin)
              (setq result (+ result (/ (- (cadr(nth (+ i 1) list-time-stamp)) begin) (car (nth i list-time-stamp)))))
            )

          (if (equal i pos-less-end)
              (setq result   (+ result (/ (- end (cadr(nth  i  list-time-stamp)))  (car (nth i list-time-stamp)))))
            )

          (if (not (or (equal i pos-less-begin) (equal i pos-less-end)))
              (setq result   (+ result (/ (- (cadr(nth (+ i 1) list-time-stamp))   (cadr(nth  i list-time-stamp))) (car (nth i list-time-stamp)))))
      
            )
          )(print "ok2")(* 1.0 result)))

(defun recup-freq-pond-all-task()
  (loop for i from  0 to (-(length (req-recup-all  (monitor-time *engine*)))3) collect
        (recup-freq-pond-per-task (date-to-ms(nth 7(nth i(req-recup-all  (monitor-time *engine*)))))   (date-to-ms(nth 8(nth (+ i 1)(req-recup-all  (monitor-time *engine*)))))(read_csv_freq *file-freq*)))) 




;-2 OK
(defun recup-freq-pond-all-task-log()
  (let* ((list-time-stamp (read_csv_freq *file-freq*))
         (freq-time  (mapcar #'cadr list-time-stamp))
         (list-all  (req-recup-all  (monitor-time *engine*)))
         (result-pond-time-exec 0))
    (setq result-pond-time-exec (loop for i from  0 to (-(length list-all)2) collect
                                      (recup-freq-pond-per-task-log (date-to-ms(nth 7(nth i(req-recup-all  (monitor-time *engine*)))))   (date-to-ms(nth 8(nth (+ i 1)list-all))) list-time-stamp freq-time) ))
(loop for k from 0 to (-(length list-all)2) collect
      (list (car(nth k list-all)) (caddr(nth k list-all)) (nth k result-pond-time-exec))
)))

(defun recup-freq-first-all-task-log()
  (let* ((list-time-stamp (read_csv_freq *file-freq*))
         (freq-time  (mapcar #'cadr list-time-stamp))
         (list-all  (req-recup-all  (monitor-time *engine*)))
         (result-first-time-exec 0))
    (setq result-first-time-exec (loop for i from  0 to (-(length list-all)2) collect
                                      (recup-freq-first-per-task-log (date-to-ms(nth 7(nth i(req-recup-all  (monitor-time *engine*)))))   (date-to-ms(nth 8(nth (+ i 1)list-all))) list-time-stamp freq-time) ))
(loop for k from 0 to (-(length list-all)2) collect
      (list (car(nth k list-all)) (caddr(nth k list-all)) (nth k result-first-time-exec))
)))

          
;(length( req-recup-all(monitor-time *engine*)))
            







(defun find-nearest(begin list-time-stamp)
(let ((list-value (mapcar #'(lambda (x) (- x begin) ) list-time-stamp)))
 setq list-value (<list-value
)))

;---------read-file--------- ;todo recuperer moyenne et variance via
;(* 1.0 (/(reduce #'+ (flat (req-recup-execTime  (monitor-time *engine*))))(length (req-recup-execTime  (monitor-time *engine*)))));moyenne 
;(sqrt(/(reduce #'+ (mapcar #'(lambda(x)(* x x)) (epsilon-list *execlist*  65)))1000.0)) ;variance


;;;file->float X float =>list moyenne variance
(defun recup-moyenne-variance-from-file(file)
  (let ((in (open file :if-does-not-exist nil)) (list-time nil)(split-val nil)(moyenne 0)(variance 0)(range 0))
    (when in
      (read-line in nil)
       

      (setq list-time  (loop for line = (read-line in nil)
                             do 
                             (setq split-val (print (split-str line " ")))
                             until (equal line nil)
                             collect (nth 2 split-val) 
                             ))
      (close in))
    (print list-time)
    (setq list-time (remove-all list-time 'nil))
    (print (find  #\.  (car list-time) :test #'equal))
    (if (not(find  #\.  (car list-time) :test #'equal))
        (setq list-time  (mapcar #'parse-integer list-time))
      (setq list-time (mapcar #'parse-float list-time) ))
    (if (OR (equal (length list-time) 0) (equal (length list-time) 1))
        (progn
          (PRINT "COUCOU")
          (setq variance 1);(sqrt (/ (reduce #'+ (mapcar #'(lambda(x)(* x x)) (epsilon-list list-time moyenne))) 1)))
          (setq moyenne 1);(* 1.0 (/ (reduce #'+ list-time) 1)))
            (setq range variance))
        (progn
      (setq moyenne(* 1.0 (/ (reduce #'+ list-time) (length list-time))))
    (setq variance (sqrt (/ (reduce #'+ (mapcar #'(lambda(x)(* x x)) (epsilon-list list-time moyenne))) (length list-time))))
    (setq range (-(reduce 'max list-time)(reduce 'min list-time)))
      ))

    
    (list moyenne variance range)))

;;;file->float X float =>list moyenne variance
(defun recup-moyenne-variance-from-file-2D(file)
  (let ((in (open file :if-does-not-exist nil)) (list-time nil)(split-val nil)(moyenne 0)(variance 0)(range 0))
    (when in
      (read-line in nil)
       

      (setq list-time  (loop for line = (read-line in nil)
                             do 
                             (setq split-val (print (split-str line " ")))
                             until (equal line nil)
                             collect (nth 1 split-val) 
                             ))
      (close in))
    (print list-time)
    (setq list-time (remove-all list-time 'nil))
    (print (find  #\.  (car list-time) :test #'equal))
    (if (not(find  #\.  (car list-time) :test #'equal))
        (setq list-time  (mapcar #'parse-integer list-time))
      (setq list-time (mapcar #'parse-float list-time) ))
    (if (OR (equal (length list-time) 0) (equal (length list-time) 1))
        (progn
          (PRINT "COUCOU")
          (setq variance 1);(sqrt (/ (reduce #'+ (mapcar #'(lambda(x)(* x x)) (epsilon-list list-time moyenne))) 1)))
          (setq moyenne 1);(* 1.0 (/ (reduce #'+ list-time) 1)))
          (setq range variance))
      (progn
        (setq moyenne(* 1.0 (/ (reduce #'+ list-time) (length list-time))))
        (setq variance (sqrt (/ (reduce #'+ (mapcar #'(lambda(x)(* x x)) (epsilon-list list-time moyenne))) (length list-time))))
        (setq range (-(reduce 'max list-time)(reduce 'min list-time)))
        ))   
    (list moyenne variance range)))



(defun try-rule-10-percent(file)
  (let ((in (open file :if-does-not-exist nil)) (list-time nil)(split-val nil)(moyenne 0)(total-length 0)(list-10-percent 0))
    (when in
      (read-line in nil)
       

      (setq list-time  (loop for line = (read-line in nil)
                             do 
                             (setq split-val (print (split-str line " ")))
                             until (equal line nil)
                             collect (nth 2 split-val) 
                             ))
      (close in))
    (print list-time)
    (setq list-time (remove-all list-time 'nil))
    (print (find  #\.  (car list-time) :test #'equal))
    (if (not(find  #\.  (car list-time) :test #'equal))
        (setq list-time  (mapcar #'parse-integer list-time))
      (setq list-time (mapcar #'parse-float list-time) ))
    (setq total-length (length list-time))
      (setq moyenne(* 1.0 (/ (reduce #'+ list-time) total-length)))
   ;    (setq list-10-percent (remove-if-not  #'(lambda (x)  (< x (- moyenne (/ moyenne 10.0)))) (remove-if-not  #'(lambda (x) (> x (+ moyenne (/ moyenne 10.0)))) list-time)))
       (setq list-10-percent (remove-if  #'(lambda (x)  (> x (+ moyenne (/ moyenne 10.0)))) list-time))
        (setq list-10-percent (remove-if  #'(lambda (x)  (< x (- moyenne (/ moyenne 10.0)))) list-10-percent))
(/(*(length list-10-percent)100)total-length)
))



;(/(* 100(length (try-rule-10-percent "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-exec.dat"))) 1000)
;(remove-if  #'(lambda (x)  (> x 3))(remove-if  #'(lambda (x)  (< x 3)) '(1 2 3 4)))
;(length (setq listamoi (recup-moyenne-variance-from-file "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-5K-1000task-2thread-non-saturer-exec-OM.dat")))
;(mapcar #'parse-integer listamoi)qq

;(find #\.   "3.14" :test #'equal)
;(find  "The Hyperspec contains . approximately 110,000 hyperlinks." :test #'equal)
;((char "test" 1)
(defun factorial (x)
   (cond ((or (not (typep x 'integer)) (minusp x))
      (error "~S is a negative number." x))
      ((zerop x) 1)
      (t (* x (factorial (- x 1))))
   )
)


;----------------call gnuplot--------------
;pause mouse keypress to enable rotation with -persist option
;check sys-run-check-command => lui dire ne pas attendre la fin option
(defun gnuplot-file-all(file binwidth)
  (let ((moyenne-variance-range (recup-moyenne-variance-from-file file)))
    (print "là")
    (print moyenne-variance-range)
    (if (not (equal (caddr moyenne-variance-range)0))
        (progn
 ;   (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \" set title \\\"moy: ~D var: ~D\\\";plot x u 1:2:3;pause mouse keypress \"" (car moyenne-variance) (cadr moyenne-variance) file) 
          (PRINT "OKOKOK")
      (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \" bin(x,width)=width*floor(x/width) ;binwidth=~D; plot '~D' u (bin(\\$3,binwidth)):(1.0) smooth freq with boxes \" &" binwidth file) 
                             )
      (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \" set title \\\"moy: ~D var: ~D range: ~D coefvarsurrange: ~D\\\";splot '~D' u 1:2:3;pause mouse keypress \" &" (car moyenne-variance-range) (cadr moyenne-variance-range) (caddr moyenne-variance-range) (/ 1.0 (/ (cadr moyenne-variance-range)(caddr moyenne-variance-range))) file) 
                             )))

    ))



(defun gnuplot-2D-file-all(file binwidth)
  (let ((moyenne-variance-range (recup-moyenne-variance-from-file-2D file)))
  (progn
 (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \" bin(x,width)=width*floor(x/width) ;binwidth=~D; plot '~D' u (bin(\\$2,binwidth)):(1.0) smooth freq with boxes \"&" binwidth file))
   
 (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \"  set title \\\"moy: ~D var: ~D range: ~D coefvarsurrange: ~D\\\"; plot '~D'\"&"  (car moyenne-variance-range) (cadr moyenne-variance-range) (caddr moyenne-variance-range) (/ 1.0 (/ (cadr moyenne-variance-range)(caddr moyenne-variance-range))) file))
)))

(defun gnuplot-2D-file-3D(file binwidth)
  (let ((moyenne-variance-range (recup-moyenne-variance-from-file file)))
  (progn
 (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \" bin(x,width)=width*floor(x/width) ;binwidth=~D; plot '~D' u (bin(\\$1,binwidth)):(1.0) smooth freq with boxes \"&" binwidth file))
   
 (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \"  set title \\\"moy: ~D var: ~D range: ~D coefvarsurrange: ~D\\\"; plot '~D' u 1:3\"&"  (car moyenne-variance-range) (cadr moyenne-variance-range) (caddr moyenne-variance-range) (/ 1.0 (/ (cadr moyenne-variance-range)(caddr moyenne-variance-range))) file))
)))

(defun gnuplot-2D-file-inverse-all(file binwidth)
  (let ((moyenne-variance-range (recup-moyenne-variance-from-file-2D file)))
  (progn
 (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \" bin(x,width)=width*floor(x/width) ;binwidth=~D; plot '~D' u (bin(\\$1,binwidth)):(1.0) smooth freq with boxes \"&" binwidth file))
   
 (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e \"  set title \\\"moy: ~D var: ~D range: ~D coefvarsurrange: ~D\\\";plot '~D' u 2:1\"&"  (car moyenne-variance-range) (cadr moyenne-variance-range) (caddr moyenne-variance-range) (/ 1.0 (/ (cadr moyenne-variance-range)(caddr moyenne-variance-range))) file))
)))

;(sys:run-shell-command "/usr/local/bin/gnuplot -p -e \" plot '/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-45K-1000task-4thread-non-saturer-OM-freq.dat'\"")


;------------------save image from gnuplot-----
(defun save-eps-gnuplot2D(output output2 file xlabel1 ylabel1 xlabel ylabel binwidth &optional (soustract 0))
  (let ((moyenne-variance-range (recup-moyenne-variance-from-file-2D file)))
    (progn

(sys:run-shell-command (print(format nil "/usr/local/bin/gnuplot -p -e \" set terminal svg size 640,480 fname 'Verdana' fsize 10 ; set output '~D'  ; set xlabel '~D' ; set ylabel '~D' ; set key off ;  bin(x,width)=width*floor(x/width) ; binwidth=~D ; plot '~D' u (bin(\\$2,binwidth)):(1.0) smooth freq with boxes \"&"  output  xlabel1 ylabel1 binwidth file)  ))

      (sys:run-shell-command (print(format nil "/usr/local/bin/gnuplot -p -e \" set terminal svg size 640,480 fname 'Verdana' fsize 10 ; set output '~D';  set xlabel '~D'  ; set ylabel '~D' ;  set title \\\"moy: ~D var: ~D range: ~D coefvarsurrange: ~D\\\" font \\\"sans, 15\\\"  ; set key off ; plot '~D'  u (\\$1-~D):2 \"&"  output2  xlabel ylabel (car moyenne-variance-range) (cadr moyenne-variance-range) (caddr moyenne-variance-range) (/ 1.0 (/ (cadr moyenne-variance-range)(caddr moyenne-variance-range))) file soustract)))
      )))


(defun save-eps-gnuplot2-3D(output output2 file xlabel1 ylabel1 xlabel ylabel binwidth &optional (soustract 0))
  (let ((moyenne-variance-range (recup-moyenne-variance-from-file file)))
    (progn

(sys:run-shell-command (print(format nil "/usr/local/bin/gnuplot -p -e \" set terminal svg size 640,480 fname 'Verdana' fsize 10 ; set output '~D'  ; set xlabel '~D' ; set ylabel '~D' ; set key off ;  bin(x,width)=width*floor(x/width) ; binwidth=~D ; plot '~D' u (bin(\\$2,binwidth)):(1.0) smooth freq with boxes \"&"  output  xlabel1 ylabel1 binwidth file)  ))

      (sys:run-shell-command (print(format nil "/usr/local/bin/gnuplot -p -e \" set terminal svg size 640,480 fname 'Verdana' fsize 10 ; set output '~D';  set xlabel '~D'  ; set ylabel '~D' ;  set title \\\"moy: ~D var: ~D range: ~D coefvarsurrange: ~D\\\" font \\\"sans, 15\\\"  ; set key off ; plot '~D'  u (\\$1-~D):3 \"&"  output2  xlabel ylabel (car moyenne-variance-range) (cadr moyenne-variance-range) (caddr moyenne-variance-range) (/ 1.0 (/ (cadr moyenne-variance-range)(caddr moyenne-variance-range))) file soustract)))
      )))


(defun save-eps-gnuplot3D(output output2 file xlabel1 ylabel1 xlabel ylabel zlabel binwidth &optional (soustract 0))
  (let ((moyenne-variance-range (recup-moyenne-variance-from-file file)))
    (progn


      (sys:run-shell-command (print(format nil "/usr/local/bin/gnuplot -p -e \"set terminal svg size 640,480 fname 'Verdana' fsize 10 ; set output '~D'  ; set xlabel '~D' ; set ylabel '~D' ; set key off ; set xtics rotate by -45 ;  bin(x,width)=width*floor(x/width) ; binwidth=~D ; plot '~D' u (bin(\\$3,binwidth)):(1.0) smooth freq with boxes \"&"  output  xlabel1 ylabel1 binwidth file binwidth))  )



      (sys:run-shell-command (princ (format nil "/usr/local/bin/gnuplot -p -e \"set terminal svg size 640,480 fname 'Verdana' fsize 10 ; set output '~D'; set ztics rotate by -45 ; set xlabel '~D'  ; set ylabel '~D' ; set zlabel \\\"~D\\\"  offset graph 0,0,1; set title \\\"moy: ~D var: ~D range: ~D coefvarsurrange: ~D\\\" font \\\"sans, 15\\\"  ; set key off ; splot '~D' u (\\$1-~D):2:3  \"&"  output2  xlabel ylabel zlabel (car moyenne-variance-range) (cadr moyenne-variance-range) (caddr moyenne-variance-range) (/ 1.0 (/ (cadr moyenne-variance-range)(caddr moyenne-variance-range))) file soustract)))
      )))

; (save-eps-gnuplot2D "/Users/samuel/Desktop/test.svg" "/Users/samuel/Desktop/test2.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-2thread-non-saturer-OM-freq.dat" "freq" "nbElem" "time" "freq" 1)

;(save-eps-gnuplot3D "/Users/samuel/Desktop/stest.svg" "/Users/samuel/Desktop/stest2.svg" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-1thread-non-saturer-OM-exec.dat" "freq" "nbElem" "x" "y" "z" 1)

; (sys:run-shell-command (print(format nil "/usr/local/bin/gnuplot -p -e  \"set terminal svg size 400,260 fname 'Verdana' fsize 10 ; set output '/Users/samuel/Desktop/test8.svg' ; set xlabel 'test' ; set key off ;  plot '~D' \"" "/Users/samuel/Documents/openMusic/MesuresComputationnelle/is-premier-25K-1000task-4thread-non-saturer-OM-freq.dat")))
 

; (sys:run-shell-command (format nil "/usr/local/bin/gnuplot -p -e  \" plot '/Users/samuel/Documents/openMusic/ \""))
