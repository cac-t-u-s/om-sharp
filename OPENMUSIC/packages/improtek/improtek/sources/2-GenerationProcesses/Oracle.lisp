(in-package :om)   

; Adapted from G. Assayag (OMax 2.0)
; --------------------------------------------------------------------------------
;;; classes oracle



(defclass* oracle ()
  (
   (vectext :initform nil :accessor vectext :initarg :vectext)
   (hashtransition :initform (make-hash-table :test 'equal) :accessor hashtransition :initarg :hashtransition)
   (hashsuppl :initform (make-hash-table :test 'equal) :accessor hashsuppl :initarg :hashsuppl)
   (hashsuppl-> :initform (make-hash-table :test 'equal) :accessor hashsuppl-> :initarg :hashsuppl->)
   (veclrs :initform nil :accessor veclrs :initarg :veclrs)
   (maxetat :initform 0 :accessor maxetat :initarg :maxetat)
   (hashrepeats :accessor hashrepeats :initform nil :initarg :hashrepeats)
   (lrsmode :initform nil :accessor lrsmode :initarg :lrsmode)
   (nbresauts :accessor nbresauts :initform 0)
   (ptitsaut :accessor ptitsaut :initform 0)
   ))

(defmethod initialize-instance ((self oracle) &key (otext nil))
  (call-next-method)
  (setf (suppleance self 0) -1)
  (cond
   (otext
    (let ((length (1+ (length otext))))
      (setf (vectext self) (make-array length :adjustable t))
      (when (lrsMode self)
        (setf (veclrs self) (make-array length :adjustable t)))
      (loop for objet in otext
            do (ajouter-objet self objet))))
   (t (unless (vectext self) (setf (vectext self) (make-array 1000 :adjustable t)))
      (unless (vecLrs self)
        (when (lrsMode self) (setf (veclrs self) (make-array 1000 :adjustable t))))))
  self)

(defmethod creer-etat ((oracle oracle) (etat integer))
  (setf (maxetat oracle) etat)
  )

(defmethod transition ((oracle oracle) (etat integer) (objet t))
  (gethash (list etat objet) (hashtransition oracle)))

(defmethod (setf transition) ((etat2 integer) (oracle oracle) (etat integer) (objet t))
  (setf (gethash (list etat objet) (hashtransition oracle))
        etat2)
  )

(defmethod suppleance ((oracle oracle) (etat integer))
  (gethash etat (hashsuppl oracle)))

(defmethod suppleance-> ((oracle oracle) (etat integer))
  (gethash etat (hashsuppl-> oracle)))


(defmethod (setf suppleance) ((etat2 integer) (oracle oracle) (etat integer) )
  (setf (gethash etat (hashsuppl oracle))
        etat2)
  (setf (gethash etat2 (hashsuppl-> oracle))
        etat)
  )

;(defmethod (setf suppleance) ((etat2 integer) (oracle oracle) (etat integer) )
;  (setf (gethash etat (hashsuppl oracle))
;        (append (gethash etat (hashsuppl oracle)) '(etat2)))
;  (setf (gethash etat2 (hashsuppl-> oracle))
;        (append (gethash etat2 (hashsuppl-> oracle)) '(etat)))
;  )


(defmethod lrs ((oracle oracle) (etat integer))
  (aref  (veclrs oracle) etat))


(defmethod (setf lrs) ((etat2 integer) (oracle oracle) (etat integer) )
  (setf (aref  (veclrs oracle) etat)
        etat2)
  )

(defmethod otext ((oracle oracle) (etat integer))
  (aref  (vectext oracle) etat))

(defmethod otext ((oracle oracle) (etat t))
  (vectext oracle) )


(defmethod (setf otext) ((objet t) (oracle oracle) (etat integer) )
  (setf (aref  (vectext oracle) etat)
        objet)
  )


; --------------------------------------------------------------------------------
; classes pythie
; Definition de la classe Pythie a partir de la classe Oracle. On rajoute un comparateur qui sera la fonction de comparaison


(defclass* Pythie (oracle)
  (
   ;(hashFactorLink :initform (make-hash-table) :accessor hashFactorLink)
   ;(comparateur :initform #'comp1 :initarg :comparateur :accessor comparateur)
   (comparateur :initform nil :initarg :comparateur :accessor comparateur)
   ))


(defmethod initialize-instance ((self Pythie)  &key (otext nil) )
  (call-next-method))


;; when a transition state1 --a--> state2, add state2 to a list of targets from state1
;; in the hashtable = ( (state1  (list of arrows)) ...)
(defmethod transition ((self pythie) (etat integer) (objet t))
  (let ((arrows (gethash  etat (hashtransition self))))
    (loop for arrow in arrows
          if (funcall (comparateur self) objet (otext self arrow))
          do (return arrow)
          finally (return nil))))

  
(defmethod (setf transition) ((etat2 integer) (self Pythie) (etat integer) (objet t))
    (push etat2 (gethash etat (hashtransition self))))


; fonction qui retourne la liste des suivants d'un etat
(defmethod flink  ((self pythie) (etat integer))
  (gethash etat (hashtransition self)))

; donne la suppleance
(defmethod fsuppl  ((self pythie) (etat integer))
  (gethash etat (hashsuppl self)))

; egalite des valeurs absolues
(defun myequal (x1 x2)
  (and (not (or (null x1) (null x2)))
       (= (abs x1) (abs x2))))




; --------------------------------------------------------------------------------

;;; ORACLE algo de Crochemore, allauzen , raffinot, lefebvre, lecroq  



(defun oracle-online (otext)
  (let ((oracle (make-instance 'oracle :otext otext  :lrsMode t)))
    oracle))

(defmethod ajouter-objet ((self oracle) (objet t))
  (let ((m (maxetat self)) (Pi1))
    (when (>= (1+ m) (length (vectext self)))
      (setf (vectext self) (adjust-array (vectext self) (+ 500 (length (vectext self)))))
      (when (lrsmode self) (setf (veclrs self) (adjust-array (veclrs self) (+ 500 (length (veclrs self)))))))
    
    (creer-etat self (1+ m))
    (setf (otext self (1+ m)) objet)
    (setf (transition self m objet) (1+ m))
    (loop for etat = (suppleance self m) then (suppleance self etat)
          with Pi1 = m
          while (and (> etat -1)
                     (null (transition self etat objet)))
          do (setf (transition self etat objet) (1+ m)
                   Pi1 etat)
          finally
          (let ((sp (if (= etat -1)  0 (transition self etat objet))))
            (when sp 
              (setf (suppleance self (1+ m)) sp)
              (when (lrsMode self)
                (setf (lrs self (1+ m)) (LenghrepeatedSuffix self Pi1 sp))))))
    self))

    
(defmethod LenghRepeatedSuffix ((self oracle) (Pi1 integer) (S integer))
  (if (zerop S) 0
      (1+ (lengthCommonSuffix self Pi1 (1- S)))))

(defmethod LengthCommonSuffix ((self oracle) (Pi1 integer) (Pi2 integer))
  (if (= Pi2 (suppleance self Pi1)) 
    (lrs self Pi1)
    (loop while (/= (suppleance self Pi2) (suppleance self Pi1))
          do (setf Pi2 (suppleance self Pi2))
          finally (return (min (lrs self Pi1) (lrs self Pi2))))))
            

;
(defmethod factor-p ((self oracle) (factor list))
  (loop for etat = 0 then (transition self etat (pop factor))
        while (and factor (not (null etat)))
        finally
        (if (null etat) (return nil)  (return t))))


; --------------------------------------------------------------------------------
;;; recherche de repetitions (pas utilise dans OMAX)


(defmethod look4repeats ((self oracle) (level integer))
  (init-repeats self)
  (prog1 (loop for i from (maxetat self) downto 1
               with partition = Nil
               if (>= (lrs self i) level)
               do (setf partition (collect-repeat self i  partition))
               finally (return (format-repeats self  partition level)))
    (terminate-repeats self)))

(defmethod stats ((self oracle))
  (let ((maxrepeats (loop for i from 1 to (maxetat self)
                         maximize (lrs self i))))
    (loop for i from maxrepeats downto 1
          for repeats = (look4repeats self i)
          for stat =  (loop for j in repeats sum (length (first j)))
          do (format  T "Longueur : ~D ~D patterns ~D occurences au total~%" i (length repeats) stat))))


                   

(defmethod format-repeats ((self oracle) (rset list) (level integer))
  (sort (loop for set in rset
              for etat = (first set)
              collect (list (loop for state in (sort set '<) collect (1+ (- state level)))
                            (subseq  (otext self t) (1+ (- etat level)) (1+ etat))))
        '>
        :key #'(lambda (x) (length (first x)))))


(defmethod init-repeats ((self oracle))
  (setf (hashrepeats self) (make-hash-table)))

(defmethod terminate-repeats ((self oracle))
  (setf (hashrepeats self) nil))

(defmethod collect-repeat ((self oracle) (etat integer) (partition list))
  (let* ((source (suppleance self etat))
         (rsource (gethash source (hashrepeats self)))
         (retat (gethash etat (hashrepeats self))))
    (cond 
     ((and (null retat) (null rsource))
      (let ((shared (list etat source)))
        (setf (gethash source (hashrepeats self)) shared
              (gethash etat (hashrepeats self)) shared )
        (push shared partition)))
     ((null retat)
      (setf (gethash etat (hashrepeats self)) (rplacd rsource (cons etat (rest rsource)))))
     ((null rsource)
      (setf (gethash source (hashrepeats self)) (rplacd retat (cons source (rest retat)))))
     (t (setf partition (delete rsource partition))
        (let ((merge (nconc retat rsource)))
          (loop for state in merge
                do (setf (gethash state (hashrepeats self)) merge))))))
  
  partition)



                        

#|

tests

(oracle-online '(a b c a a b a c b b a c))
(setf o (oracle-online '(a b b b a a b)))
(setf o (oracle-online '(l e v i e r g e l e v i v a c e e t l e b e l a u j o u r d h u i)))
(setf o (oracle-online '(l e v i e r g e l e v i v a c e e l e b e l a u j o u r d h u i)))
(setf o (oracle-online '( a b c a b c a b c)))
(setf o (oracle-online '( a b d a b d a b a b)))
(transition o 2 'b)
(suppleance o 0)
(veclrs o)
(loop for i from 0 to 10 collect (suppleance o i))
(maphash  #'(lambda (x y) (print (list x y))) (hashrepeats o))

(factor-p o '( e t l e b e l))
(otext o t)
(setf x (loop for i from 1 to 10000 collect (random 10)))
(setf y (oracle-online x))
(stats y)

(look4repeats y 1)

|#




#|

EXEMPLES

(setf o (oracle-online '(l e v i è r j e l e v i v a s é  l e b è l o j  u r d w i v a t i l n u d é ch i r é d yn k u d è l e i v r e s e l a k u b l i é k e an t e s u l e j i v r e l e t r an s p a r an g l a s i é d é v o l k i n on p a f w i)))

(inspect o)

(stats o)

(look4repeats o 4)
(look4repeats o 3)
(look4repeats o 2)
(look4repeats o 1)


(factor-p o '(l o j u r))
(factor-p o '(g e l i))

(setf x (loop for i from 1 to 10000 collect (if (< (random 10) 3) 5 (random 5))))
(setf y (oracle-online x))

(stats y)

(look4repeats y 10)
(look4repeats y 9)
(look4repeats y 6)


(setf o (oracle-online '(1 2 3 4 5 1 2 3 4 5)))

(stats o)

(look4repeats o 5)
|#