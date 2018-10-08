;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
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

;;;==========================
;;; Misc. Lisp Tools for OM
;;;==========================

(in-package :om)

(defconstant *positive-infinity* most-positive-fixnum)
(defconstant *negative-infinity* most-negative-fixnum)

;;;===========================
;;; LISTS 
;;;===========================

(defun list! (thing) (if (listp thing) thing (list thing)))
(defun list+ (&rest lists) (apply 'concatenate (append (list 'list) lists)))

(defun first?  (thing) (if (listp thing) (first thing) thing))

(defun max? (a b) (if a (if b (max a b) a) b))
(defun min? (a b) (if a (if b (min a b) a) b))

(defun firstn (list  n )
   (cond
    ((< (length list)  n) list )
    (t  (butlast  list (- (length list) n)))))

(defun replace-in-list (list elem pos)
   (let* ((first-list (subseq list 0 pos))
          (second-list (subseq list (+ pos 1))))
     (append first-list (list elem) second-list)))

(defun insert-in-list (list elem pos)
  (if (> pos (- (length list) 1))
      (append list (list elem))
    (append (subseq list 0 pos) 
            (list elem) 
            (subseq list pos))))

(defun erase-n (list pos)
   (loop for item in list
         for i = 0 then (+ i 1)
         when (not (= i pos)) collect item))

;; pos = a list of indices
(defun erase-nn (list pos)
   (loop for item in list
         for i = 0 then (+ i 1)
         when (not (member i pos)) collect item))
  
(defun list-typep (list typeList)
   "Checks if every elt in 'list' belongs to one of the types in 'typelist'"
  (every #'(lambda (elt) (some #'(lambda (type) (equal (type-of elt) type)) (list! typelist))) list))

(defun list-subtypep (list typeList)
   "Checks if every elt in 'list' belongs to one of the subtypes in 'typelist'"
   (every #'(lambda (elt) (some #'(lambda (type) (subtypep (type-of elt) type)) (list! typelist))) list))

(defun next-in-list (list item) 
  (let* ((pos (position item list))
         (newpos (if pos (mod (1+ pos) (length list)) 0)))
    (nth newpos list)))


(defmacro %car (x)
  `(car (the cons ,x)))

(defmacro %cdr (x)
  `(cdr (the cons ,x)))

(defmacro %cadr (x)
  `(cadr (the cons ,x)))

(defun quoted-form-p (form)
  (and (consp form)
        (eq (%car form) 'quote)
        (consp (%cdr form))
        (null (%cdr (%cdr form)))))

(defun lambda-expression-p (form)
   (and (consp form)
       (eq (%car form) 'lambda)
       (consp (%cdr form))
       (listp (%cadr form))))

;push item in place but in the last position
(defmacro pushr (item place)
  `(if ,place
     (setf (cdr (last ,place)) (cons ,item nil))
     (setf ,place (list ,item))))

(defmacro insert-in-order (object list &key (key #'identity) (test '<))
  `(let ((p (position (funcall ,key ,object) ,list :test ',test :key ,key)))
     (if (not p)
         (nconc ,list (list ,object))
       (if (= p 0) 
           (push ,object ,list)
         (push ,object (nthcdr p ,list))))
     ,list))

(defmacro filter-list (list from to &key (key #'identity))
  `(remove-if #'(lambda (element) 
                 (or (< (funcall ,key element) ,from)
                     (>= (funcall ,key element) ,to)))
             ,list))

(defun in-interval (n interval &key exclude-low-bound exclude-high-bound)
  (and (funcall (if exclude-low-bound '> '>=) n (car interval)) 
       (funcall (if exclude-high-bound '< '<=) n (cadr interval))))

;=================
;safe random
(defun om-random-value (num)
  (if (= num 0) 0
    (if (< num 0)
        (- (random (- num)))
      (random num))))

;;;==============================
;;; HASH-TABLES
;;;==============================

(defmethod hash-keys ((h hash-table))
  (loop for key being the hash-keys of h collect key))

(defmethod hash-items ((h hash-table))
  (loop for key in (hash-keys h) collect (gethash key h)))

;;;==============================
;;; PROPERTIES / KEY-VALUE LISTS
;;;==============================


;(let ((ll (list-from-file "/Users/bresson/Desktop/testomx/testomx.omws"))) (find-value-in-kv-list ll :info))

;; '(:key1 :val1 :key2 :val2 ...)
(defun find-value-in-arg-list (list key)
    (let ((pos (position key list :test 'equal)))
      (and pos (nth (1+ pos) list))))
;;; => GETF

(defun set-value-in-arg-list (list key value)
  (let ((pos (position key list :test 'equal)))
    (if pos 
        (setf (nth (1+ pos) list) value)
      (setf list (append list (list key value))))
    ))

;; '((:key1 :val1) (:key2 :val2) ...)
(defun find-value-in-kv-list (list key)
  (cadr (find key list :test 'equal :key 'car)))

(defun set-value-in-kv-list (list key value)
  (if list 
      (let ((kv (find key list :test 'equal :key 'car)))
        (if kv (setf (cadr kv) value)
          (push (list key value) list))
        list)
    (list (list key value))))

;; '((:key1 prop1 prop2 prop3...) (:key2 prop1 prop2 prop3...) ...)
(defun find-values-in-prop-list (list key)
  (remove nil ;;; ?
          (cdr (find key list :test 'equal :key 'car))
          ))

;=======================
; types
;=======================

(defun ensure-type (object type)
  (and (subtypep (type-of object) type) object))

;=======================
; FUNCTIONS / LAMBDA LIST PARSING
;=======================

(defun function-arg-list (fun)
  (remove-if #'(lambda (item) (member item lambda-list-keywords :test 'equal)) (function-arglist fun)))

(defun function-main-args (fun) 
  (loop for item in (function-arglist fun) 
        while (not (member item lambda-list-keywords :test 'equal)) 
        collect item))

(defun function-n-args (fun)
  (length (function-main-args fun)))

(defun function-optional-args (fun) 
  (let ((al (function-arglist fun)))
    (when (find '&optional al)
      (subseq al (1+ (position '&optional al))
              (position (remove '&optional lambda-list-keywords) al :test #'(lambda (a b) (find b a)))))))
  
(defun function-keyword-args (fun) 
  (let ((al (function-arglist fun)))
    (when (find '&key al)
      (subseq al (1+ (position '&key al))
              (position (remove '&key lambda-list-keywords) al :test #'(lambda (a b) (find b a)))))))

(defun get-keywords-fun (fun)
  (let ((args (function-arglist fun))
        rep)
    (loop while (and args (not rep)) do
          (when (equal (pop args) '&key)
            (setf rep t)))
    (setf rep nil)
    (loop for item in args 
          while (not (member item lambda-list-keywords :test 'equal)) do
          (push (if (listp item) (car item) item) rep))
    (reverse rep)))

(defun get-optional-fun (fun)
 (let ((args (function-arglist fun))
       rep)
   (loop while (and args (not rep)) do
         (when (equal (pop args) '&optional)
           (setf rep t)))
   (setf rep nil)
   (loop for item in args
         while (not (member item lambda-list-keywords :test 'equal)) do
         (push (if (listp item) (car item) item) rep))
   (reverse rep)))


(defun valued-val (val)
   (if (or (symbolp val) 
           (and (consp nil) (or (not (symbolp (car list))) (not (fboundp (car list))))))
       val 
     (eval val)))

;=======================
; POSITION
;=======================

(defun interval-intersec (int1 int2)
   (when (and int2 int1)
     (let ((x1 (max (car int1) (car int2)))
           (x2 (min (cadr int1) (cadr int2))))
       (if (<= x1 x2)
         (list x1 x2)))))

;; each rect = (left top right bottom)
(defun rect-intersec (rect1 rect2)
  (let* ((tx (max (first rect1) (first rect2)))
	 (ty (max (second rect1) (second rect2)))
	 (t2x (min (third rect1) (third rect2)))
         (t2y (min (fourth rect1)  (fourth rect2))))
    (if (or (< t2x tx) (< t2y ty))
	nil
      (list tx ty t2x t2y))))

(defun point-in-interval-p (point interval)
   (and (<= point (second interval))
        (>= point (first interval))))

(defun point-in-rectangle-p (point left top right bottom)
   (let ((x (om-point-x point))
         (y (om-point-y point)))
     (and (>= x left) (<= x right)
          (>= y top) (<= y bottom))))

;=======================
; MATH
;=======================

(defun space-ranges (range &optional (factor 0.05) (min 1))
  (list (- (first range) (max min (abs (* (- (second range) (first range)) factor))))
        (+ (second range) (max min (abs (* (- (second range) (first range)) factor))))
        (- (third range) (max min (abs (* (- (fourth range) (third range)) factor))))
        (+ (fourth range) (max min (abs (* (- (fourth range) (third range)) factor))))))


(defun average (xs weights?)
  (let ((num 0) (den 0) ampl)
    (loop while xs do 
          (setq ampl (or (if (consp weights?) (pop weights?) 1) 1))
          (incf num (* ampl (pop xs)))
          (incf den ampl))
    (/ num den)))


