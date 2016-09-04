(in-package :om)

;;;;deja dans api des lists...
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)


(defun remove-nth (n list)
  (if (zerop n)
    (cdr list)
    (let ((cons (nthcdr (1- n) list)))
      (if cons
        (setf (cdr cons) (cddr cons))
        cons))))

;; Create a hash table
(defparameter *m* (make-hash-table))

;; set a value
(setf (gethash 'a *m*) 1)

;; Retrieve a value
(gethash 'a *m*) ; => 1, t

;test some stat function and hashtable exec...
(let ((obj (make-instance 'om-time-mesure )))
  (setf (gethash 'clef  (hash-exec obj)) (list 1))
  (setf (gethash 'clef  (hash-exec obj)) (list  ))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 1) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (add-to-list-of-key obj 'a 4)  ;;; penser a mettre ' devant le nom de la clef dans l'argument pour faire marcher la macro...
  ( average-time obj  'clef)
  (average '( 1 2 3))
  (variance '(1 2 3 5))
  (ecart-type '(1 2 3 5))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj)))

;;;;;;partie test
(let ((self (make-instance 'om-time-mesure )))
;(mesure-time-in-body "hd" obj
  (print "saulgoodman")
  (mesure-time-in-body  'self hd (dotimes (x 1)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 2)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 3)(sleep 1)))
  (gethash 'hd (hash-exec self))
  (gethash 'hd (hash-stat self)))

;;;good test : creating value, test setter and test getter
(let ((obj (make-instance 'om-time-mesure )))
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 3) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (analyse-all-from-key obj 'clef)
  (add-to-list-of-key obj 'clef 1)
  (add-to-list-of-key obj 'clef 5)
  (gethash 'clef (hash-exec obj))
  ( update-analyse-from-key obj 'clef)
  (analyse-value-from-key obj 'clef)
  (hstat-moyenne obj 'clef)
  (hstat-median obj 'clef)
  (hstat-variance obj 'clef)
  (hstat-ecart-type obj 'clef)
  (hstat-range obj 'clef)
  (hstat-histogram obj 'clef)
  (histogram-decroissant  obj 'clef)
  (gethash 'clef (hash-stat obj)))


(let ((obj (make-instance 'bpf :action 'print)))
  (insert-point obj (omp 15 65))
  (insert-point obj (omp 18 68))
  (insert-point obj (omp 17 66))
  (insert-point obj (omp 2 65))
  (insert-point obj (omp 3 65))
  (insert-point obj (omp 6 65))
  (test-time obj))


(defun foo (fn)
  (mapcar fn '(1 2 3)))

(foo #'(lambda (x) (* x 2)))
(foo #'1+)
(foo #'sqrt)
(foo #'(lambda (x) (1+ (* x 3))))

(median '(1 5 2 3 ))





;;;;deja dans api des lists...
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)


(defun remove-nth (n list)
  (if (zerop n)
    (cdr list)
    (let ((cons (nthcdr (1- n) list)))
      (if cons
        (setf (cdr cons) (cddr cons))
        cons))))

;; Create a hash table
(defparameter *m* (make-hash-table))

;; set a value
(setf (gethash 'a *m*) 1)

;; Retrieve a value
(gethash 'a *m*) ; => 1, t

;test some stat function and hashtable exec...
(let ((obj (make-instance 'om-time-mesure )))
  (setf (gethash 'clef  (hash-exec obj)) (list 1))
  (setf (gethash 'clef  (hash-exec obj)) (list  ))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 1) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (add-to-list-of-key obj 'a 4)  ;;; penser a mettre ' devant le nom de la clef dans l'argument pour faire marcher la macro...
  ( average-time obj  'clef)
  (average '( 1 2 3))
  (variance '(1 2 3 5))
  (ecart-type '(1 2 3 5))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj)))

;;;;;;partie test
(let ((self (make-instance 'om-time-mesure )))
;(mesure-time-in-body "hd" obj
  (print "saulgoodman")
  (mesure-time-in-body  'self hd (dotimes (x 1)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 2)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 3)(sleep 1)))
  (gethash 'hd (hash-exec self))
  (gethash 'hd (hash-stat self)))

;;;good test : creating value, test setter and test getter
(let ((obj (make-instance 'om-time-mesure )))
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 3) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (analyse-all-from-key obj 'clef)
  (add-to-list-of-key obj 'clef 1)
  (add-to-list-of-key obj 'clef 5)
  (gethash 'clef (hash-exec obj))
  ( update-analyse-from-key obj 'clef)
  (analyse-value-from-key obj 'clef)
  (hstat-moyenne obj 'clef)
  (hstat-median obj 'clef)
  (hstat-variance obj 'clef)
  (hstat-ecart-type obj 'clef)
  (hstat-range obj 'clef)
  (hstat-histogram obj 'clef)
  (histogram-decroissant  obj 'clef)
  (gethash 'clef (hash-stat obj)))


(let ((obj (make-instance 'bpf :action 'print)))
  (insert-point obj (omp 15 65))
  (insert-point obj (omp 18 68))
  (insert-point obj (omp 17 66))
  (insert-point obj (omp 2 65))
  (insert-point obj (omp 3 65))
  (insert-point obj (omp 6 65))
  (test-time obj))


(defun foo (fn)
  (mapcar fn '(1 2 3)))

(foo #'(lambda (x) (* x 2)))
(foo #'1+)
(foo #'sqrt)
(foo #'(lambda (x) (1+ (* x 3))))

(median '(1 5 2 3 ))


(in-package :om)

;;;;deja dans api des lists...
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)


(defun remove-nth (n list)
  (if (zerop n)
    (cdr list)
    (let ((cons (nthcdr (1- n) list)))
      (if cons
        (setf (cdr cons) (cddr cons))
        cons))))

;; Create a hash table
(defparameter *m* (make-hash-table))

;; set a value
(setf (gethash 'a *m*) 1)

;; Retrieve a value
(gethash 'a *m*) ; => 1, t

;test some stat function and hashtable exec...
(let ((obj (make-instance 'om-time-mesure )))
  (setf (gethash 'clef  (hash-exec obj)) (list 1))
  (setf (gethash 'clef  (hash-exec obj)) (list  ))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 1) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (add-to-list-of-key obj 'a 4)  ;;; penser a mettre ' devant le nom de la clef dans l'argument pour faire marcher la macro...
  ( average-time obj  'clef)
  (average '( 1 2 3))
  (variance '(1 2 3 5))
  (ecart-type '(1 2 3 5))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj)))

;;;;;;partie test
(let ((self (make-instance 'om-time-mesure )))
;(mesure-time-in-body "hd" obj
  (print "saulgoodman")
  (mesure-time-in-body  'self hd (dotimes (x 1)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 2)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 3)(sleep 1)))
  (gethash 'hd (hash-exec self))
  (gethash 'hd (hash-stat self)))

;;;good test : creating value, test setter and test getter
(let ((obj (make-instance 'om-time-mesure )))
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 3) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (analyse-all-from-key obj 'clef)
  (add-to-list-of-key obj 'clef 1)
  (add-to-list-of-key obj 'clef 5)
  (gethash 'clef (hash-exec obj))
  ( update-analyse-from-key obj 'clef)
  (analyse-value-from-key obj 'clef)
  (hstat-moyenne obj 'clef)
  (hstat-median obj 'clef)
  (hstat-variance obj 'clef)
  (hstat-ecart-type obj 'clef)
  (hstat-range obj 'clef)
  (hstat-histogram obj 'clef)
  (histogram-decroissant  obj 'clef)
  (gethash 'clef (hash-stat obj)))


(let ((obj (make-instance 'bpf :action 'print)))
  (insert-point obj (omp 15 65))
  (insert-point obj (omp 18 68))
  (insert-point obj (omp 17 66))
  (insert-point obj (omp 2 65))
  (insert-point obj (omp 3 65))
  (insert-point obj (omp 6 65))
  (test-time obj))

(let ((l '(166 164 164 164 158 157 157 156 156 155 155 154 154 154 153 152 152 152 151 151 150 150 150 150 150 149 149 149 149 148 148 148 148 148 148 147 147 147 147 147 146 146 146 146 146 146 145 145 145 145 145 145 144 144 144 144 143 143 143 143 143 143 143 143 142 142 142 142 142 142 142 142 142 141 141 141 141 140 140 140 140 140 140 140 140 139 139 139 139 139 138 138 138 138 138 138 137 137 137 137 137 137 136 136 136 136 135 135 133)))
  (/ 33.0 (ceiling (+ 1 (* (/ 10 3) (log (length l) 10))))))

(defun foo (fn)
  (mapcar fn '(1 2 3)))

(foo #'(lambda (x) (* x 2)))
(foo #'1+)
(foo #'sqrt)
(foo #'(lambda (x) (1+ (* x 3))))

(median '(1 5 2 3 ))





;;;;deja dans api des lists...
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)


(defun remove-nth (n list)
  (if (zerop n)
    (cdr list)
    (let ((cons (nthcdr (1- n) list)))
      (if cons
        (setf (cdr cons) (cddr cons))
        cons))))

;; Create a hash table
(defparameter *m* (make-hash-table))

;; set a value
(setf (gethash 'a *m*) 1)

;; Retrieve a value
(gethash 'a *m*) ; => 1, t

;test some stat function and hashtable exec...
(let ((obj (make-instance 'om-time-mesure )))
  (setf (gethash 'clef  (hash-exec obj)) (list 1))
  (setf (gethash 'clef  (hash-exec obj)) (list  ))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 1) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (add-to-list-of-key obj 'a 4)  ;;; penser a mettre ' devant le nom de la clef dans l'argument pour faire marcher la macro...
  ( average-time obj  'clef)
  (average '( 1 2 3))
  (variance '(1 2 3 5))
  (ecart-type '(1 2 3 5))
  (hash-exec obj)
  (gethash 'clef (hash-exec obj)))

;;;;;;partie test
(let ((self (make-instance 'om-time-mesure )))
;(mesure-time-in-body "hd" obj
  (print "saulgoodman")
  (mesure-time-in-body  'self hd (dotimes (x 1)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 2)(sleep 1)))
  (mesure-time-in-body  'self hd (dotimes (x 3)(sleep 1)))
  (gethash 'hd (hash-exec self))
  (gethash 'hd (hash-stat self)))

;;;good test : creating value, test setter and test getter
(let ((obj (make-instance 'om-time-mesure )))
  (gethash 'clef (hash-exec obj))
  (add-to-list-of-key obj 'clef 3) 
  (add-to-list-of-key obj 'clef 2)
  (add-to-list-of-key obj 'clef 4)
  (analyse-all-from-key obj 'clef)
  (add-to-list-of-key obj 'clef 1)
  (add-to-list-of-key obj 'clef 5)
  (gethash 'clef (hash-exec obj))
  ( update-analyse-from-key obj 'clef)
  (analyse-value-from-key obj 'clef)
  (hstat-moyenne obj 'clef)
  (hstat-median obj 'clef)
  (hstat-variance obj 'clef)
  (hstat-ecart-type obj 'clef)
  (hstat-range obj 'clef)
  (hstat-histogram obj 'clef)
  (histogram-decroissant  obj 'clef)
  (gethash 'clef (hash-stat obj)))


(let ((obj (make-instance 'bpf :action 'print)))
  (insert-point obj (omp 15 65))
  (insert-point obj (omp 18 68))
  (insert-point obj (omp 17 66))
  (insert-point obj (omp 2 65))
  (insert-point obj (omp 3 65))
  (insert-point obj (omp 6 65))
  (test-time obj))


(defun foo (fn)
  (mapcar fn '(1 2 3)))

(foo #'(lambda (x) (* x 2)))
(foo #'1+)
(foo #'sqrt)
(foo #'(lambda (x) (1+ (* x 3))))

(median '(1 5 2 3 ))




(defun test-time (object)
  (let ((t1 (om-get-internal-time)))
    (dotimes (i 10000)
      (get-action-list-for-play object (list 0 *positive-infinity*)))
    (print (- (om-get-internal-time) t1))
    ))
;;;this file may be delete...
;;;;deja dans api des lists...
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)


(defun remove-nth (n list)
  (if (zerop n)
    (cdr list)
    (let ((cons (nthcdr (1- n) list)))
      (if cons
        (setf (cdr cons) (cddr cons))
        cons))))

;;;an example of macro
;(defmacro with-schedulable-object (object &rest body)
 ; `(let (res)
 ;    (setq res (progn ,@body))
 ;    (when (and (eq (state ,object) :play) (> (time-window ,object) *Lmin*))
 ;      (setf (time-window ,object) *Lmin*)
 ;      (reschedule ,object *scheduler* (get-obj-time ,object)))
   ;  res))



;(macro "monscheduler" du code)
(setq lalist (list 2 3 1))
(nbclass lalist)
(princ lalist)

;; Create a hash table
(defparameter *m* (make-hash-table))

;; set a value
(setf (gethash 'a *m*) 1)

;; Retrieve a value
(gethash 'a *m*) ; => 1, t

(let ((obj (make-instance 'om-time-mesure )))
   ; (setf (hash-exec obj) 3)
  ; (setf (slot-value obj 'hash-exec) 3)
  (setf (gethash 'clef  (hash-exec obj)) (list 1))
(setf (gethash 'clef  (hash-exec obj)) (list ))
   (hash-exec obj)
(gethash 'clef (hash-exec obj))
 (add-to-list-of-key obj 'clef 1) 
 (add-to-list-of-key obj 'clef 2)
(add-to-list-of-key obj 'clef 4)
(add-to-list-of-key obj 'a 4)  ;;; penser a mettre ' devant le nom de la clef dans l'argument...
( average-time obj  'clef)
(average '( 1 2 3))
(variance '(1 2 3 5))
(ecart-type '(1 2 3 5))
(hash-exec obj)
(gethash 'clef (hash-exec obj))
)

;;;;;;partie test
(let ((self (make-instance 'om-time-mesure )))
;(mesure-time-in-body "hd" obj
 (print "saulgoodman")
(mesure-time-in-body-micro-sec  self 'hd (dotimes (x 3)(sleep 1)))
(mesure-time-in-body-micro-sec  self 'hd (dotimes (x 3)(sleep 2)))
;(test '(self))
;(mesure-time-in-body 'self key    1)
(gethash 'hd (hash-exec self))
)

;;;good test
(let ((obj (make-instance 'om-time-mesure )))
   ; (setf (hash-exec obj) 3)
  ; (setf (slot-value obj 'hash-exec) 3)
  (setf (gethash 'clef  (hash-exec obj)) (list 1))
(setf (gethash 'clef  (hash-exec obj)) (list ))
   (hash-exec obj)
(gethash 'clef (hash-exec obj))
 (add-to-list-of-key obj 'clef 1) 
 (add-to-list-of-key obj 'clef 2)
(add-to-list-of-key obj 'clef 4)
(add-to-list-of-key obj 'clef 3)
(add-to-list-of-key obj 'a 4)  ;;; penser a mettre ' devant le nom de la clef dans l'argument...
;( average-time obj  'clef)
 (analyse-all-from-key obj 'clef)
(princ  (analyse-all-from-key obj 'clef)))

(princ "coucou")




(let ((obj (make-instance 'bpf :action 'print)))
   (insert-point obj (omp 15 65))
  (insert-point obj (omp 18 68))
  (insert-point obj (omp 17 66))
 (insert-point obj (omp 2 65))
 (insert-point obj (omp 3 65))
 (insert-point obj (omp 6 65))

(test-time obj))

(mesure-time-in-body 2
(loop for i below 300000
 (princ 1) 
(princ 2)
(princ 3)))


(time (loop for i below 3000
                         sum (sqrt i)))
;récupérer l heure courante dans OM :  om-get-internal-real-time

(defun foo (fn)
  (mapcar fn '(1 2 3)))

(foo #'(lambda (x) (* x 2)))
(foo #'1+)
(foo #'sqrt)
(foo #'(lambda (x) (1+ (* x 3))))

(median '(1 5 2 3 ))



