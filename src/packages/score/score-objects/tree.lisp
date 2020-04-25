
;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================
;;; mostly imported from OM6 code (by C. Agon & G. Assayag)
;;; TOOLS TO WORK ON THE RHYTHM-TREE (list/text) REPRESENTATION


(in-package :om)


;;;===================================
;;; A ratio keeping track of denum
;;;===================================
;;; not sure we need this anymore ? or maybe just to replace (numdenom)

#|
(defstruct r-ratio (num) (denom))
(defmethod r-ratio-value ((r r-ratio))
  (/ (r-ratio-num r) (r-ratio-denom r)))
(defmethod r-ratio-* ((r r-ratio) (n number))
  (make-r-ratio :num (* (r-ratio-num r) n) :denom (r-ratio-denom r)))
(defmethod r-ratio-* ((r r-ratio) (n ratio))
  (make-r-ratio :num (* (r-ratio-num r) (numerator n)) :denom (* (r-ratio-denom r) (denominator n))))
|#


;;; get the absolute duration (in proportion)
(defun decode-extent (dur)
  (cond ((listp dur) ;;; e.g. '(4 4)
         (/ (first dur) (second dur)))
        ((floatp dur) 
         (round (abs dur)))
        (t ;;; hopefully this is a number 
         (abs dur))
        ))


; (reduce #'(lambda (x y) (+ (abs x) (fullratio y))) '((2 8) (3 8) (4 8) (3 8) (2 8)) :initial-value 0)
; (compute-total-extent '(1 (1 (2 5)) 1 1)) (((8 4) (1 (1 (2 5)) 1 1)) ((4 4) (1 1 5 1))))


;;; convert to 'better' values
(defun convert-extent (extent)
  (case extent
    (5 (list 4 1))
    (9 (list 8 1))
    (10 (list 8 2))
    (11 (list 8 3))
    (13 (list 12 1))
    (17 (list 16 1))
    (18 (list 16 2))
    (19 (list 16 3))
    (20 (list 16 4))
    (21 (list 15 6))
    (22 (list 16 6))
    (23 (list 16 7))
    (25 (list 24 1))
    (t  extent)))




;;; simplify complex subdivisions into tied/simpler ones
(defun normalize-tree (tree)
   
  (labels 
    ((normalize-recursive (tree)
       
       (cond 
        
        ;;; normalize recursively (the subdiv-part)
        ((listp tree)
         (list (list (car tree) (mapcan #'normalize-recursive (cadr tree)))))
        
        ((numberp tree) ;;; a leaf
         
         (let ((converted-extent (convert-extent (abs (round tree))))) ;;; (1) convert to positive integer just to call convert-extent
           
           (if (listp converted-extent) ;;; the leaf was converted
               (progn 
                 
                 (if (plusp tree) 
                     ;;; abs did not apply in (1)
                     (progn 
                       ;;; tie the second pulse to the first pulse
                       (setf (second converted-extent) (float (second converted-extent)))
                       
                       (when (floatp tree) 
                         ;;; the first pulse was already a tie
                         (setf (first converted-extent) (float (first converted-extent)))))
                   
                   ;;; else: (1) abs did invert the sign: restore it
                   (setf (first converted-extent) (- (first converted-extent)) 
                         (second converted-extent) (- (second converted-extent))))
                 
                 converted-extent)
             
             ;;; nothing changes
             (list tree))))      
        )))
    
    (list 
     (car tree)
     (mapcan #'normalize-recursive (cadr tree)))
    ))

;;;=============================================
;;; other pre-processing of rhythm-trees
;;; from OM6
;;; TOP-LEVEL RULES MARKED WITH ->
;;;=============================================

;Check the syntax of the tree and computes the value of '? if there is in the tree
(defun subtree-extent (subtree) ;verifier qui l'appele
  (cond ((listp subtree) (fullratio (first subtree)))
        ((floatp subtree) (round (abs subtree)))
        ((or (lw::ratiop subtree)(integerp subtree))  (abs subtree)) ))

(defun symbol->ratio (symbol)
  "expects symbols like |4//4| and returns a list (4 4)"
  (let ((string (copy-seq (symbol-name symbol))))
    (loop for i from 0 to (1- (length string))
       when (char= (elt string i) #\/) do (setf (elt string i) '#\Space))
    (read-from-string (format nil "(~A)" string))))

(defun resolve-? (list)
  (cond 
   ((numberp list) list)
   ((or (numberp (first list)) (listp (first list)))
    (if (listp (second list)) 
        (list (first list) (mapcar #'resolve-? (second list)))
      (error (format nil "Invalid Rhythm Tree : ~A" list))))
   ((and (symbolp (first list)) (equal (symbol-name (first list)) "?"))
    (let ((solved (mapcar #'resolve-? (second list))))
      (list (reduce #'(lambda (x y) (+  (abs x) (subtree-extent y))) 
                    solved :initial-value 0)
            solved)))
   ((symbolp (first list))				    ; ie: |4//4|
    (if (listp (second list)) 
        (list (symbol->ratio (first list)) (mapcar #'resolve-? (second list)))
	(error (format nil "Invalid Rhythm Tree : ~A" list))))
   ))


;;;===============================================
; If only one element
;'((4 4) ((1 (1 1 1)))) = T
(defun measure-single? (mes)
  (= (length (cadr mes)) 1))

; If only one leaf
;'((4 4) (5)) = T
(defun measure-super-single? (mes)
  (and (measure-single? mes) (numberp (caadr mes))))

(defun replace-num-in-tree (old new)
  (cond
   ((minusp old) (* -1 new))
   ((floatp old) (* 1.0 new))
   (t new)))

(defun rw-singleton (list &optional reduction)
  (cond
   ((= (length list) 1)
    (let ((elem (first list)))
      (if (numberp elem)
          (if reduction 
              (list (replace-num-in-tree elem reduction))
            list)
        
        (if reduction 
            
            (if (= (length (second elem)) 1)
                (rw-singleton (second elem) reduction)
              (list (list reduction (rw-singleton (second elem)))))
          
          (if (= (length (second elem)) 1)
              (rw-singleton (second elem) (car elem))
            (list (list (first elem) (rw-singleton (second elem)))))
          ))))
   (t
    (loop for item in list append
          (if (numberp item) 
            (rw-singleton (list item))
            (if (= (length (second item)) 1)
              (rw-singleton (second item) (first item)) ;;; reduction is HERE
              (list (list (first item) (rw-singleton (second item))))))))))

; ->
(defun resolve-singletons (tree)
  (let* ((measures tree))
     ; (list (car tree)
    (mapcar #'(lambda (mes)  ;pour chaque measure
                (let ((sign (first mes))
                      (slist (second mes)))
                  (cond
                   ((measure-super-single? mes) ; un mesure de la forme ((4//4 (n))) avec n n number
                    (list sign (list (replace-num-in-tree (first slist) (first sign)))))
                   ((measure-single? mes) ;un mesure de la forme ((4//4 (g))) ou g est un groupe, donc une liste
                    (let ((group (first slist)))
                      (if (= (length (second group)) 1)
                          (list sign (rw-singleton (second group) (caar mes)))
                        (list sign (list (list (first sign)
                                               (rw-singleton (second group))))))))
                   (t ;un mesure de la forme ((4//4 (r1 r2 ...rn))) ou r est un groupe ou un number
                    (list sign (rw-singleton (second mes)))))))
            measures)
           ;)
    ))


;;;==============================================
;;; returns top-level subdivision of the measure
;'((4 4) (3 (1 (1 1 1)) -5)) = '(3 1 5)
(defun measure-repartition (mes)
   (loop for item in (cadr mes)
         collect (floor (if (numberp item) (abs item)
                          (abs (car item))))))

(defun modulo3-p (n)
  (or (zerop ( mod n 3)) (= n 1)))

; ->
(defun list-first-layer (tree)
  
  (loop for measure-tree in tree collect 
        
        (if (measure-single? measure-tree) 
      
            measure-tree 
    
          (let* ((signature (car measure-tree))
                 (subdivs (apply '+ (measure-repartition measure-tree)))
                 (ratio1 (/ subdivs (car signature))))
            (cond
             ((and (integerp ratio1) (power-of-two-p ratio1)) measure-tree)
             ((and (power-of-two-p subdivs) 
                   (or (power-of-two-p (car signature))
                       (and (integerp ratio1) (modulo3-p (car signature))))) measure-tree)
             ((not (integerp (/ (car signature) subdivs))) 
              (list signature (list (list (car signature) (cadr measure-tree)))))
             ((and (= (numerator ratio1) 1) (not (power-of-two-p (denominator ratio1))) measure-tree)
              (list signature (list (list (car signature) (cadr measure-tree)))))
             (t measure-tree)))
          )
        )
  )


;;;===================================================================
;;; convert long/uneven durations to even + ties

(defun only-one-point (n)
  (cond
   ((floatp n) (mapcar 'float (only-one-point (round n))))  
   (t                              
    (if (member n '(0 1 2 3 4 6 8 12 16 32)) ;only for optimization
        (list n)
      (let ((bef (bin-value-below n)))
        (cond
         ((or (= bef n) (= (* bef 1.5) n) (= (* bef 1.75) n))  (list n))
         ((> n (* bef 1.5))
          (append (list (+ bef (/ bef 2))) (only-one-point (/ (- n (* bef 1.5)) 1.0))))
         (t (cons bef (only-one-point (/ (- n bef) 1.0))))))))))

(defun add-measure-ties (tree)
  (cond ((numberp tree)
         (let ((convert (only-one-point (abs tree))))
           (if (minusp tree)
               (setf convert (om* convert -1)))
           convert))
        ((listp tree)
         (list (list (first tree) (mapcan #'add-measure-ties (second tree)))))))

; ->
(defun add-ties-to-tree (tree)
   (let* ((measures tree))
     ;(list (car tree)
     (mapcar #'(lambda (mes)
                 (list (first mes) (mapcan #'add-measure-ties (second mes))))
             measures)
     ;)
   ))


;;;===================================================================
;;; called at voice intialization:
(defun format-tree (tree) 
  (list 
   (car tree)
   (add-ties-to-tree 
    (resolve-singletons 
     (list-first-layer (cadr tree))))))


;; fullratios are either ratios or lists (num denom)
;; use fullratio function to cast a fullratio to a number
;; use fdenominator and fdenominator to access to a fullratio num and denum

(defmethod fullratio ((self list)) (/ (first self)  (second self)))
(defmethod fullratio ((self number)) self)
(defmethod fullratio ((self float)) (round self))

(defmethod fdenominator ((self t)) (denominator (fullratio self)))
(defmethod fdenominator ((self list)) (second self))
(defmethod fnumerator ((self t)) (numerator (fullratio self)))
(defmethod fnumerator ((self list)) (first self))

(defun simplify-subtrees (subtrees)
  
  (let ((lcm (abs (reduce #'lcm subtrees :key #'(lambda (x) (fdenominator (if (listp x) (first x) x))))))
        (gcd (abs (reduce #'gcd subtrees :key #'(lambda (x) (fnumerator (if (listp x) (first x) x)))))))
    
    (mapcar #'(lambda (x) 
                (if (listp x) 
                    (list (* (fullratio (first x)) (/ lcm gcd)) (second x))
                  (let ((div (*  (fullratio x) (/ lcm gcd))))
                    (if (floatp x) (float div) div))
                  ))
            subtrees)
    ))

