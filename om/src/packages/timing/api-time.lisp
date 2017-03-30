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

;;an api for mesuring time execution of the function , avgr and variance ,etc...
(in-package :om) ; (list t1 t2  threadId tsup freq-processeur   (thread-actif-cpt ,obj) (cpt ,obj)  time-begin time-end idle-core-percent)))     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this class contain an hashtable which contain the list of : execution-time , end-exec-time, le numero de thread appelant, le temps supposé, la freq du processeur à 0 a updater a posteriori , le nombre de thread actif, le numero de la tâche appellé (combien est-il) ,  le temps en timestamp du debut, même chose pour la fin et enfin l'oisiveté de la machine (l'ensemble de coeur logique) en pourcentage sur 100. 
;if you use om-time-mesure
;;; ajout d'une deuxieme table de hachage qui mets a jour les statistique 
;regarder à la fin choix si en ms ns ou us (micro sec)




(defclass om-time-mesure ()
  ((hash-exec :initform (make-hash-table) :accessor hash-exec)
   (hash-stat :initform (make-hash-table) :accessor hash-stat) 
  (time-type :initform 'ms :accessor time-type)
  (action-list-supposed-timed :initform nil :accessor action-list-supposed-timed)
(cpt :initform 0 :accessor cpt)
(thread-actif-cpt :initform 0 :accessor thread-actif-cpt)
(maquette :initform nil :accessor maquette)
)) 



;;CELLE UTILISER DANS LA FONCTION DU THREAD
(defmacro mesure-all-time-us(obj clef thread &body body)
  `(let (res) 
     (let ((t1 (round (MACH::mach_absolute_time) 1000))
           (threadId (parse-integer(subseq(reverse(mp:process-name mp:*current-process*)) 0 1)))
           (tsup (+ *begin* (* *cpt-elmt-bpf* (* 1000 *bpf-period* ))));recuperer a posteriori les supposedTIME
           (t2) (idle-core-percent (- 100(/ (parse-float(multiple-value-bind (out)
                                                            (sys:run-shell-command "ps -A -o %cpu | awk '{s+=$1} END {print s}'"
                                                                                   :wait nil
                                                                                   :output :stream
                                                                                   )
                                                          (with-open-stream (out out)
                                                            (values (read-line out))))) *logical-core-nb*)))
           (freq-processeur 0)
           (time-begin (date-with-ms)) (time-end 0))
       (setf *cpt-elmt-bpf* (+ *cpt-elmt-bpf* 1))       
       (progn
         (setq res (progn ,@body))  
         (setq t2 (round (MACH::mach_absolute_time) 1000))
         (setq time-end (date-with-ms))
         ;on remplace si tsup est nil par t1...
         (if (eq tsup nil)
             (add-list-to-list-of-key  ,obj ,clef (list t1 t2  threadId t1 freq-processeur  (thread-actif-cpt ,obj) (cpt ,obj)  time-begin time-end idle-core-percent))
           (add-list-to-list-of-key  ,obj ,clef (list t1 t2  threadId tsup freq-processeur   (thread-actif-cpt ,obj) (cpt ,obj)  time-begin time-end idle-core-percent)))         
         (setf  (cpt ,obj) (+ (cpt ,obj) 1))
         )
    
       res)))


;----
(defmacro mesure-all-time(obj clef thread &body body)
   `(let (res)
       (let ((t1 (om-get-internal-time))(threadId (parse-integer(subseq(reverse(mp:process-name mp:*current-process*)) 0 1))) (tsup (nth (+ 2(cpt   (monitor-time *engine*))) (action-list-supposed-timed (monitor-time *engine*)))) (t2) (time-begin (date-with-ms)) (time-end 0)) 
   
       (progn
         (setq res (progn ,@body))  
         (setq t2 (om-get-internal-time))
         (setq time-end (date-with-ms))
         
      
         ;on remplace si tsup est nil par t1...
         (if (eq tsup nil)
               (add-list-to-list-of-key  ,obj ,clef (list t1 t2  threadId t1  (thread-actif-cpt ,obj) (cpt ,obj) time-begin time-end))
             (add-list-to-list-of-key  ,obj ,clef (list t1 t2  threadId tsup   (thread-actif-cpt ,obj) (cpt ,obj) time-begin time-end)))
          

       (setf  (cpt ,obj) (+ (cpt ,obj) 1)))

     res)))
 (print (om-get-internal-time))
   


;;macro qui ajoute le tuple (begin end threadNumber debutTheorique nbthreadOcc idUnique )
;debut théorique????
(defmacro mesure-all-time-work(obj clef thread  &body body)
   `(let (res)
       (let ((t1 (om-get-internal-time))(threadId (parse-integer(subseq(reverse(mp:process-name mp:*current-process*)) 0 1)))(t2))
         (progn
           (setq res (progn ,@body))  
           (setq t2 (om-get-internal-time))
           (print "mesure-all-time-call1")
           (add-list-to-list-of-key  ,obj ,clef (list t1 t2 threadId (cpt ,obj)))
           (print "mesure-all-time-call2")
           (setf  (cpt ,obj) (+ (cpt ,obj) 1)))
        ; (add-to-list-of-key ,obj ,clef ,(list t1 t2 threadId (cpt obj)))) 
     ;    (update-analyse-from-key ,obj ,clef)))
     res)))

(defmacro mesure-all-time-sched(obj clef &body body)
   `(let (res)
       (let ((t1 (om-get-internal-time)) (t2))
       (progn
         (setq res (progn ,@body))  
         (setq t2 (om-get-internal-time))

       (add-list-to-list-of-key  ,obj ,clef (list t1 t2 (cpt ,obj)))
              
       (setf  (cpt ,obj) (+ (cpt ,obj) 1)))

     res)))




;add in the specify key an new element in the list of the row of the hashtable
(defmethod add-to-list-of-key ((self om-time-mesure) key elt)
  (push elt (gethash key (hash-exec self)))
  (gethash key (hash-exec self)))

(defmethod add-list-to-list-of-key ((self om-time-mesure) key elt)

  (if (gethash key (hash-exec self)) ;rend en premier le cas sans liste null on
;ajoute dans la liste alors l element.
      (progn
        (insert-after (gethash key (hash-exec self)) (-(length(gethash key
                                                                       (hash-exec self)))1)elt))
    (progn
      (setf (gethash key  (hash-exec self)) elt))
    )
;retourne la liste en question
  (gethash key (hash-exec self))
  )


(defmethod update-supposed-time((self om-time-mesure) micro-sec-pas)
  (let ((list-key (loop for i in (hash-keys (hash-exec self)) collect i)))
    (loop for v in list-key do
          (setf (nth 3(gethash v  (hash-exec self))) (+(* micro-sec-pas v) (nth 3(gethash v  (hash-exec self))))))
    ))

(defmethod add-to-supposed-time((self om-time-mesure) value-add)
  (let ((list-key (loop for i in (hash-keys (hash-exec self)) collect i)))
    (loop for v in list-key do
          (setf (nth 3(gethash v  (hash-exec self))) (+ value-add (nth 3(gethash v  (hash-exec self))))))
    ))

(defmethod update-freq-first-cpu((self om-time-mesure) freq-list)
  (let ((list-key (loop for i in (hash-keys (hash-exec self)) collect i)))
    (loop for v in list-key do
          (if(equal  (caddr(nth v freq-list)) nil)
              (setf (nth 4 (gethash v (hash-exec self))) 0)
            (setf (nth 4 (gethash v (hash-exec self))) (caddr(nth v freq-list)))))))

(defmethod update-freq-ponderate-cpu((self om-time-mesure) freq-list)
  (let ((list-key (loop for i in (hash-keys (hash-exec self)) collect i)))
     (loop for v in list-key do
           (setf (nth 4 (gethash v (hash-exec self))) (caddr(nth v freq-list))))))


(defmethod get-freq-computation((self om-time-mesure))
  (let ((list-key (loop for i in (hash-keys (hash-exec self)) collect i)))
     (loop for v in list-key collect
           (list (nth 4 (gethash v (hash-exec self)))(nth 1 (gethash v(hash-exec self))))
           )))
                       
;;;;getter setter
(defmethod (setf hash-exec) ( (self om-time-mesure) exec-time )
  (setf (slot-value self 'hash-exec) exec-time))

(defmethod (setf hash-stat) ( (self om-time-mesure) stat-stuff )
  (setf (slot-value self 'hash-stat) stat-stuff) )

(defmethod (setf cpt) ( (self om-time-mesure) cpt-stuff )
  (setf (slot-value self 'cpt) cpt-stuff) )


(defmethod (setf action-list-supposed-timed) ( (self om-time-mesure)  action-list-supposed-timed )
  (setf (slot-value self 'action-list-supposed-timed)  action-list-supposed-timed))


(defmethod (setf time-type) ( (self om-time-mesure) time-type-update )
  (setf (slot-value self 'time-type) time-type-update))


;;;;;;;visualisation de hashTable
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun finder(hash-table)
(loop for key being the hash-keys of  hash-table
         for value being the hash-values of  hash-table
         collect (list key value)))

(defun order-finder2(hash-table)
(sort(loop for key being the hash-keys of  hash-table
         for value being the hash-values of  hash-table
         collect (list key value)  ) #'(lambda(x y)(< (car x)(car y) ))))

;;------------------------------------------------------requete pour hash table

(defun ordered-keys (table)
  (sort
   (loop for key being each hash-key of table
         collect key )
   #'<))

(defmethod req-between( (self om-time-mesure)begin end)
  (let ((hash-table (hash-exec self)))
       (loop 
         for value being the hash-values of  hash-table when(and (>= (car value) begin) (<= (cadr value) end))
          collect (list   value))
          ))


(defmethod req-per-thread( (self om-time-mesure) threadnum)
  (let ((hash-table (hash-exec self)))
       (loop 
         for value being the hash-values of  hash-table when(= (caddr value) threadnum)
         collect (list value)))
)

(defmethod req-recup-all ((self om-time-mesure))
   (let ((hash-table (hash-exec self)))
       (loop 
         for value being the hash-values of  hash-table 
         collect  value)))

(defmethod req-recup-execTime ((self om-time-mesure))
   (let ((hash-table (hash-exec self)))
       (loop 
         for value being the hash-values of  hash-table 
         collect (list (- (cadr value)(car value))))))

(defmethod req-recup-retard ((self om-time-mesure))
   (let ((hash-table (hash-exec self)))
       (loop 
         for value being the hash-values of  hash-table 
         collect (list (-(car value)(cadddr value))))))

(defmethod req-recup-supposedTime ((self om-time-mesure))
   (let ((hash-table (hash-exec self)))
       (loop 
         for value being the hash-values of  hash-table 
         collect (cadddr value))))

(defmethod req-active-thread ((self om-time-mesure) nbthreadactif)
    (let ((hash-table (hash-exec self)))
      (loop
       for value being the hash-values of hash-table when(eq (nth 5 value) nbthreadactif)
       collect (list value)))
)



(defmethod req-test ((self om-time-mesure) nbthreadactif)
    (let ((hash-table (hash-exec self)))
      (loop
       for value being the hash-values of hash-table do
       (print value)
       (print (nth 4 value))
 (print (nth 5 value))))
  
)

(defmethod epsilon-list (list-exec nominal-task-time)
  (mapcar #'(lambda(x)(- x nominal-task-time)) list-exec)
)

(defmethod epsilon-sum-nominal (espilon-list nominal-task-time)
  (/ (reduce  #'+ espilon-list) nominal-task-time))

(defmethod espilon-variation-moyenne(espilon-list nominal-task-time)
  (let ((prcentlist (mapcar #'(lambda (a) (abs (/ a nominal-task-time))) espilon-list))(variation 0))
  (setq variation (* 100 (/ (reduce '+ prcentlist) (length prcentlist))))
  variation)
)

(mapcar #'(lambda(x)(- x 3)) '(2 4 0))



;hashtable stats où stat-stuff est une liste d'élément et de list (dans l'ordre
;1:moyenne 2:mediane  3:variance  4:ecarttype  5:range 6:representation donnée
;d'histogram
;;;;;;;;;;partie statistiques;;;;;;;;
(defun square(elt)
  (* elt elt))

  ;;;trier la liste puis prendre la moitier (central si impair)
(defun median (lst)  
  (let* ((res)
         (size (length lst))
         (copylst (copy-list (copy-list lst)))
         (lst-order (sort copylst #'<)))
    (print size)
    (if (> (mod size 2) 0)
        ;si impair
        (setq res (nth (ceiling(/ size 2)) lst-order))    
      ;sinon pair
      (setq res (/(+ (nth (-(/ size 2) 1) lst-order)  (nth (/ size 2) lst-order))2)))
    res))
 
;;;calculate variance
(defun variance (lst)
  (if  (> (length lst) 1)
      (let ((average-f (average-f lst))
            (n (length lst)))
        (/ (reduce #'+ (map 'list #'(lambda (x) (square (- average-f x))) lst))
           n))))

(defun ecart-type(lst)
  (if  (> (length lst) 1)
      (sqrt (variance lst)))
  )

;moyenne
(defun average-f (lst)(float (/ (reduce #'+ lst) (length lst))))

(defun range-stat(lst)
  (- (reduce #'max lst) (reduce #'min lst)))

(defmethod average-time ((self om-time-mesure)  key)
  (average-f (gethash key (hash-exec self))))

(defun position-list(lst)
 (setq list-cop lst)
  (loop
   for i from 0 to (-(length list-cop)1)
   collect i))



;nb de class pour l'histogram
;notabene fct (log base nb)
; Règle de Sturges utiliser pour nombre de class suppose distribution normal => c'est le cas...proba eq pr chq calcul...?///
(defmethod nbclass((self om-time-mesure) key)
   (let* ((lst (gethash key (hash-exec self)))
     (nbrClass  (fceiling (*(+ 1 (/ 10 3)) (log  (length lst) 10)))))
     (truncate nbrClass)
))

;;doit on arrondir un peu, prendre en compte la resolution du moyen de mesure...????????
(defmethod amplitude-per-class((self om-time-mesure) key)
 (let* ((lst (gethash key (hash-exec self)))
 (k (/ (range-stat lst) (nbclass self key))))
k)
)


;min inclus max exclus
(defmethod count-is-between(lst down up)
 (let ((cpt))
   (setf cpt 0)
  (loop for x in lst do
        (if (>= x down)
          (if (< x up) 
        (setf cpt (+ cpt 1))
        ))
        (if (= x (car(last lst)))
            (if (= x up)
               (setf cpt (+ cpt 1))
        )))
cpt
))

(defmethod true-histogram((self om-time-mesure) key)
  (let*  ((copy-lst  (copy-list  (gethash key (hash-exec self))))
          (lst (sort copy-lst #'<))
          (amp (amplitude-per-class self key))
          (incr  (reduce #'min lst))
          (res))
    (dotimes (i (nbclass self key))
      (let ((nb (count-is-between lst incr (+ incr amp))))
        (if (null res)
            (setf res (list nb));then
          (setf res (append res (list nb)))) ;else
        (setf incr (+ incr amp))
        (setf lst (subseq lst nb))))
    res)) 
          


;decalage de min nbclass et rangeperclass
(defmethod x-histo (min nbclass range-class)
  (setq lst (list min))
  (dotimes (i nbclass)
    (insert-after lst i (+ min (* (+ i 1) range-class))))
lst)




;create couple list of couple x y for tracing an histogram
(defmethod histogram((self om-time-mesure) key)
  (let*((y-lst (true-histogram self key))
        (amp (amplitude-per-class self key))
        (min  (reduce #'min  (gethash key (hash-exec self))))
        (nbrclass (nbclass self key))
        (x-lst (x-histo min nbrclass amp))
        (res-x (copy-list x-lst))
        (res-y (copy-list y-lst)))
    (dotimes (i (-(length x-lst) 2))
      (progn
        (insert-after res-x (* 3 i)  (nth (+ i 1) x-lst))
        (insert-after res-x (+ (* 3 i) 1)  (nth (+ i 1) x-lst))))
             
    (dotimes (j (- (length y-lst) 1))
 
      (progn
        (insert-after res-y (* 3 j)  (nth  j y-lst))
        (insert-after res-y (+ (* 3 j) 1)  0))) 
    (insert-after res-y (+ (* 3  (- (length y-lst) 2)) 2)   (nth (+  (- (length y-lst) 2) 1) y-lst))
    
    ;;;ajouter au debut et à la fin des points dans res-x et res-y
    (setq res-x (cons  (nth  0 res-x) res-x))
    (setq res-y  (cons  0 res-y))
    (setq res-x (append res-x (last res-x)))
    (setq res-y (append res-y '(0)))


    (let ((result (make-instance 'bpf :x-points res-x :y-points res-y)))
      result)))

(defmethod distribution-temporelle((self om-time-mesure) key)
  (let*((lst  (gethash key (hash-exec self)))
        (pos-lst (position-list lst))
        (res (loop 
              for elt in lst and elt-pos in pos-lst
              collect (list elt elt-pos))))
 (let ((result (make-instance 'bpf :x-points (cadrL res) :y-points (carL res ))))
    
    result)))

(defmethod distribution-decroissant((self om-time-mesure) key)
  (let*((copy-lst  (copy-list  (gethash key (hash-exec self))))
        (lst (sort copy-lst #'>))
        (pos-lst (position-list lst))
        (res (loop 
              for elt in lst and elt-pos in pos-lst
              collect (list elt elt-pos))))

 (let ((result (make-instance 'bpf :x-points (cadrL res) :y-points   (carL res ))))
    
    result)))

;apply car on list the list (= car of each sublist)
(defun carL(lst)
  (mapcar 'car lst))

;apply cadr on list the list (= cadr of each sublist)
(defun cadrL(lst)
  (mapcar 'cadr lst))


;;getter of stat in hash-stat
(defmethod hstat-moyenne ((self om-time-mesure) key)
(nth 0 (gethash key (hash-stat self))))

(defmethod hstat-median ((self om-time-mesure) key)
(nth 1 (gethash key (hash-stat self))))

(defmethod hstat-variance ((self om-time-mesure) key)
(nth 2 (gethash key (hash-stat self))))

(defmethod hstat-ecart-type ((self om-time-mesure) key)
(nth 3 (gethash key (hash-stat self))))

(defmethod hstat-range ((self om-time-mesure) key)
(nth 4 (gethash key (hash-stat self))))

(defmethod hstat-histogram ((self om-time-mesure) key)
(nth 5 (gethash key (hash-stat self))))

(defmethod hstat-distribution-temporelle ((self om-time-mesure) key)
(nth 6 (gethash key (hash-stat self))))

(defmethod hstat-distribution-decroissant ((self om-time-mesure) key)
(nth 7 (gethash key (hash-stat self))))

;;;displayer
(defmethod display-histo()
 (hstat-histogram  *monitor* 'test4)
)

(defmethod display-distribution-temporelle()
 (hstat-distribution-temporelle  *monitor* 'test4)
)

(defmethod display-distribution-decroissante()
 (hstat-distribution-decroissant  *monitor* 'test4)
)

;;; print stat
(defmethod analyse-value-from-key((self om-time-mesure) key)
  ;;;probleme de typage entre list et elmt...faire des list d'elem !
  (let ((lst (gethash 'clef (hash-exec self))) )
    (print "moyenne:")
    (print (hstat-moyenne self key))   
    (print "mediane:")
    (print (hstat-median self key))
    (print "variance:")
    (print (variance lst))
    (print (hstat-variance self key))
    (print "ecart-type:")
    (print (ecart-type lst))
    (print (hstat-ecart-type self key))  
    (print "range:")
    (print (hstat-range self key))
    (print "histogram:")
    (print (hstat-histogram self key))
    (print " distribution temporelle:")
    (print (hstat-distribution-temporelle self key))
    (print " distribution decroissant:")
    (print (hstat-distribution-decroissant self key))))

(defmethod update-analyse-from-key((self om-time-mesure) key)
  (let* ((lst (gethash key (hash-exec self)))
        (stat-lst (list  (list (average-f lst)) (list (median lst)) (list (variance
lst)) (list (ecart-type lst)) (list (range-stat  lst)) (histogram self
key) (distribution-temporelle self key) (distribution-decroissant self key))))
 
      (setf (gethash key (hash-stat self)) stat-lst)
      stat-lst)
    )

(defun test-time (object)
  (let ((t1 (om-get-internal-time)))
    (dotimes (i 10000)
      (get-action-list-for-play object (list 0 *positive-infinity*)))
    (print (- (om-get-internal-time) t1))
    ))



(defun flatten (list)
  (labels ((%flatten (list tail)
             (cond
               ((null list) tail)
               ((atom list) (list* list tail))
               (t (%flatten (first list)
                            (%flatten (rest list)
                                      tail))))))
    (%flatten list '())))


(defun numlist-to-string (lst)
  (when lst
    (concatenate 'string 
                 (format nil "~a " (write-to-string (car lst))) (numlist-to-string (cdr lst)))))

;ms est le pas
(defun generate-for-bpf(ms min max)
  (let ((val min) (lst '()))
    (while (<= val max)
      (setq lst (cons val lst))
      (setq val (+ val ms)))
(reverse lst))) 

;---------COEUR IDLE Pourcent
(defun core-idle-percent()
(- 100(/ (parse-float(multiple-value-bind (out)
    (sys:run-shell-command "ps -A -o %cpu | awk '{s+=$1} END {print s}'"
			   :wait nil
			   :output :stream
			   )
  (with-open-stream (out out)
      (values (read-line out))))) *logical-core-nb*))
)




