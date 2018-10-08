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
;=========================================================================
; Authors: G. Assayag, C. Agon, J. Bresson (code from OM6)
;=========================================================================

(in-package :om)

;===========================
; Comparison functions
; simple wrappers around lisp comparisons
;===========================

(defmethod* om< ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is smaller than <num2>.
Returns T if the test is verified and NIL if not.
"
  (< num1 num2))

(defmethod* om> ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is greater than <num2>
Returns T if the test is verified and NIL if not." 
  (> num1 num2))

(defmethod* om<= ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is smaller or equal to <num2>
Returns T if the test is verified and NIL if not."
  (<= num1 num2))

(defmethod* om>= ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is greater or equal to <num2>
Returns T if the test is verified and NIL if not."
  (>= num1 num2))

(defmethod* om= ((num1 number) (num2 number))  
  :initvals '(0 0) :indoc '("a number" "a number")
  :doc "Tests if <num1> is equal to <num2>
Returns T if the test is verified and NIL if not."
  (= num1 num2))

(defmethod* om/= ((num1 number) (num2 number))  
  :initvals '(0 0) :indoc '("a number" "a number")
  :doc "Tests if <num1> is NOT equal to <num2>
Returns T if the test is verified and NIL if not." 
  (/= num1 num2))



;===========================
; Tools
;===========================

(defun round_0 (arg &optional div)
  (if (plusp arg)
      (truncate (+ (/ arg div) 0.5))
    (truncate (- (/ arg div) 0.5))))

(defun approx-decimals (x y nbdec)
  (let ((ndec 
         (if (> nbdec 0 ) (float (expt 10 nbdec)) (expt 10 nbdec))))
    (/ (round_0 (/ x y) (/ ndec)) ndec)))


(defun mulalea (n percent)
  (* n (+ 1  (om-random  (- percent) (float percent)) )))


(defmethod  tree-min ((self list) &optional (min MOST-POSITIVE-LONG-FLOAT))
  (if (null self)
    min
    (tree-min (rest self) (tree-min (first self) min))))

(defmethod  tree-min ((self number) &optional (min MOST-POSITIVE-LONG-FLOAT))
  (min self min))

(defmethod  tree-max ((self list) &optional (max MOST-NEGATIVE-LONG-FLOAT))
  (if (null self)
    max
    (tree-max (rest self) (tree-max (first self) max))))

(defmethod  tree-max ((self number) &optional (max MOST-NEGATIVE-LONG-FLOAT))
  (max self max))


;===========================
; ADDITIONS
;===========================

(defmethod* om+ ((arg1 number) (arg2 number))  
  :initvals '(0 0) :indoc '("number or list" "number or list")
  :doc "Sum of two numbers or lists.

Ex. (om+ 2 3)  => 5
Ex. (om+ 2 '(3 4))  => (5 6)
Ex. (om+ '(1 2) '(3 4))  => (4 6)
" 
  :icon 'om-plus 
  (+ arg1 arg2))


(defmethod* om+ ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
              (om+ arg1 input)) arg2))

(defmethod* om+ ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om+  input arg2)) arg1))

(defmethod* om+ ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input1 input2)
              (om+ input1 input2)) arg1 arg2))


;===========================
; MULTIPLICATIONS
;===========================

(defmethod* om* ((arg1 number) (arg2 number))  
  :initvals '(0 0) :indoc '("number or list" "number or list") 
  :icon 'om-times
  :doc "Product of two numbers or lists.

Ex. (om* 2 3)  => 6
Ex. (om* 2 '(3 4))  => (6 8)
Ex. (om* '(1 2) '(3 4))  => (3 8)
"
  (* arg1 arg2))

(defmethod* om* ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
              (om* arg1 input)) arg2))

(defmethod* om* ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om*  input arg2)) arg1))

(defmethod* om* ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input1 input2)
              (om* input1 input2)) arg1 arg2))

;===========================
; SQUARE
;===========================

(defmethod* om-square ((arg1 number))  
  :initvals '(0) :indoc '("number or list") 
  :icon 'om-times
  :doc "Square of a number or list.

Ex. (om-square 2)  => 4
Ex. (om-square '(3 4))  => (9 16)
"
  (* arg1 arg1))

(defmethod* om-square ((arg1 list))  
  (mapcar #'(lambda (n)
              (* n n)) arg1))


;===========================
; SUBTRACTIONS
;===========================

(defmethod* om- ((arg1 number) (arg2 number))  
   :initvals '(0 0) :indoc '("number or list" "number or list") 
   :icon 'om-minus
   :doc "Difference of two numbers or lists.

Ex. (om- 5 2)  => 3
Ex. (om- 5 '(2 4))  => (3 1)
Ex. (om- '(5 4) '2)  => (3 2)
Ex. (om- '(5 4) '(2 4))  => (3 0)
"
  (- arg1 arg2))

(defmethod* om- ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
              (om- arg1 input)) arg2))

(defmethod* om- ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om- input arg2)) arg1))

(defmethod* om- ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input input2)
              (om- input input2)) arg1 arg2))


;===========================
; DIVISIONS
;===========================

(defmethod* om/ ((arg1 number) (arg2 number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") 
  :icon 'om-div
  :doc "Division of two  numbers or lists.

Ex. (om/ 8 2)  => 4
Ex. (om/ 8 '(2 4))  => (4 2)
Ex. (om/ '(8 5) '(2 4))  => (4 5/4)"
  (/ arg1 arg2))

(defmethod* om/ ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
                 (om/ arg1 input)) arg2))

(defmethod* om/ ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om/  input arg2)) arg1))

(defmethod* om/ ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input input2)
              (om/  input input2)) arg1 arg2))


;===========================
; POWERS
;===========================

(defmethod* om^ ((a number) (b number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") :icon 197
  :doc "Exponentiation of base a and exponent b.

Corresponds to the multiplication of <a> repeated <b> times.
Can be used on numbers or lists.

Ex. (om^ 2 3)  => 8
Ex. (om^ 2 '(3 4 5))  => (8 16 32)
Ex. (om^ '(2 3 4) 3)  => (8 27 64)
Ex. (om^ '(2 3 4) '(2 3 4))  => (4 27 256)
"
  (expt a b))

(defmethod* om^ ((a number) (b list))  
  (mapcar #'(lambda (input)
                 (om^ a input)) b))

(defmethod* om^ ((a list) (b number))   
  (mapcar #'(lambda (input)
              (om^ input b)) a))

(defmethod* om^ ((a list) (b list))
  (mapcar #'(lambda (input1 input2)
              (om^  input1 input2)) a b))

;===========================
; EXPONENTIAL
;===========================

(defmethod* om-e ((n number)) 
  :initvals '(1) :indoc '("number or list") :icon 198
  :doc "Exponential function.

This function can be applied on numbers or lists.

Ex. (om-e 4)  => 54.59815
Ex. (om-e '(3 4))  => (20.085537 54.59815)
"
  (exp n))


(defmethod* om-e ((n list))   
  (mapcar #'(lambda (input)
              (om-e input)) n))

;===========================
; LOGARITHM
;===========================

(defmethod* om-log ((n number) &optional (base (exp 1)))
  :initvals (list 1 (exp 1)) :indoc '("a number or list" "a number") :icon 199
  :doc "Logarithm function. 
(the logarithm of a number to the base is the power to which the base must be raised in order to produce the number)

The <base> argument is optional. By default, <base> is equal to the number 'e', so om-log computes the 'natural' logarithm of <n>.

This function can be applied to numbers or lists.

Ex. (om-log 3)  => 1.0986123
Ex. (om-log 3 10)  => 0.47712126
Ex. (om-log '(3 4))  => (1.0986123 1.3862944)
"
  (log n base))


(defmethod* om-log ((n list)  &optional (base (exp 1)))
  (mapcar #'(lambda (input)
              (om-log input base)) n))

;===========================
; ROUNDING
;===========================

(defmethod* om-round ((n number) &optional (decimals 0) (divisor 1))
  :initvals '(1 0 1) :indoc '("number or list" "number of decimals" "divisor") :icon 209
  :doc "Rounds a number or a list of numbers with a given number of decimals (default = 0, i.e. returns integer values) and a divisor.

This function can be applied to numbers or lists.

Ex. (om-round 4.3)  => 4
Ex. (om-round '(4.3 5.0 6.8))  => (4 5 7)
Ex. (om-round '(4.308 5.167 6.809) 2)  => (4.31 5.17 6.81)
Ex. (om-round '(4.308 5.167 6.809) 0 2)  => (2 3 3)
Ex. (om-round '(4.308 5.167 6.809) 1 2)  => (2.2 2.6 3.4)
"
  (approx-decimals n divisor decimals))


(defmethod* om-round ((n list) &optional (decimals 0) (divisor 1))
  (mapcar #'(lambda (input)
              (om-round input decimals divisor)) n))

;===========================
; EUCLIDIAN DIVISION (WITH REST)
;===========================
(defmethod mat-trans-with-nil ((matrix list))
 (let ((maxl (1- (loop for elt in matrix maximize (length elt))))
        result)
    (loop for i from 0 to maxl do
         (push (mapcar #'(lambda (list) (nth i list)) matrix) result))
    (nreverse result)))


(defmethod* om//  ((n number) (divisor number))
  :initvals '(1 1) :indoc '("number or list"  "number or list") :icon 209
  :numouts 2
  :doc "Euclidean division of <n> and <divisor>. 
Yields an integer result and the rest of the division. 
When the divisor is 1, the operation is known as 'floor'.

<n> and <divisor> can be numbers or lists.

Ex. (om// 5.5 2)  =>  2 , 1.5
Ex. (om// 5.5 1)  =>  5 , 0.5
Ex. (om// '(5.5 6) 2)  =>  (2 3) , (1.5 0)
Ex. (om// 5.5 '(2 3))  =>  (2 1) , (1.5 2.5)
Ex. (om// '(5.5 6) '(2 3))  =>  (2 2) , (1.5 0)
"
  (floor n divisor))


(defmethod* om// ((n number) (divisor list))  
  (values-list 
   (mat-trans-with-nil (mapcar #'(lambda (input)
                          (multiple-value-list (om// n input)))
                      divisor))))

(defmethod* om// ((n list) (divisor number))
  (if (null n)
    (values nil nil)
    (values-list 
     (mat-trans-with-nil (mapcar #'(lambda (input)
                                    (multiple-value-list (om// input divisor)))
                                n)))))

(defmethod* om// ((n list) (divisor list))  
  (values-list
   (mat-trans-with-nil 
    (mapcar #'(lambda (input1 input2)
                (multiple-value-list (om// input1 input2)))
            n divisor))))



;===========================
; ABSOLUTE VALUE
;===========================

(defmethod* om-abs ((self number))
  :initvals (list 1 ) :indoc '("number or tree" ) :icon 209
  :doc "Absolute value.

This function can be applied to numbers or lists.

Ex. (om-abs 3)  => 3
Ex. (om-abs -3)  => 3
Ex. (om-abs '(3 -4 -1.5 6))  => (3 4 1.5 6)
"
  (abs  self ))


(defmethod* om-abs ((self list))
  (mapcar #'(lambda (input)
              (om-abs input)) self))


;===========================
; MIN/MAX
;===========================

(defmethod* om-min ((a number) (b number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") :icon 209
  :doc "Minimum of two numbers.

This function can be applied to numbers or lists.

Ex. (om-min 3 4)  => 3
Ex. (om-min 3 '(1 2 3 4))  => (1 2 3 3)
Ex. (om-min '(4 3 2 1) '(1 2 3 4))  => (1 2 2 1)
"
  (min a b))

(defmethod* om-min ((a number) (b list))  
  (mapcar #'(lambda (input)
                 (om-min a input)) b))

(defmethod* om-min ((a list) (b number))   
  (mapcar #'(lambda (input)
              (om-min input b)) a))

(defmethod* om-min ((a list) (b list))
  (mapcar #'(lambda (input1 input2)
              (om-min input1 input2)) a b))



(defmethod* list-min ((self list))
  :initvals '((0 1 2)) :indoc '("a list") :icon 209
  :doc "Returns the minimum element in a list.

Ex. (list-min '(2 3 1 4))  => 1"
  (and (remove nil self) (list-min2 (remove nil self) MOST-POSITIVE-LONG-FLOAT)))


(defmethod* list-min ((self t)) self)


(defun list-min2 (l minimum)
  (if (null l)
    minimum
    (if (atom (first l))
      (list-min2 (rest l) (min minimum (first l)))
      (list-min2 (first l) (min minimum (list-min2 (rest l) minimum))))))



(defmethod* om-max ((a number) (b number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") :icon 209
  :doc "Maximum of two numbers.

This function can be applied to numbers or lists.

Ex. (om-max 3 4)  => 4
Ex. (om-max 3 '(1 2 3 4))  => (3 3 3 4)
Ex. (om-max '(4 3 2 1) '(1 2 3 4))  => (4 3 3 4)"
  (max a b))

(defmethod* om-max ((a number) (b list))  
  (mapcar #'(lambda (input)
                 (om-max a input)) b))

(defmethod* om-max ((a list) (b number))   
  (mapcar #'(lambda (input)
              (om-max input b)) a))

(defmethod* om-max ((a list) (b list))
  (mapcar #'(lambda (input1 input2)
              (om-max input1 input2)) a b))


(defun list-max2 (l minimum)
  (if (null l)
    minimum
    (if (atom (first l))
      (list-max2 (rest l) (max minimum (first l)))
      (list-max2 (first l) (max minimum (list-max2 (rest l) minimum))))))


(defmethod* list-max ((self list) )
  :initvals '((0 1 2)) :indoc '("a list") :icon 209
  :doc "Returns the maximum element in a list.

Ex. (list-max '(2 4 1 3))  => 4"
  (and (remove nil self) (list-max2 (remove nil self) MOST-NEGATIVE-LONG-FLOAT)))

(defmethod* list-max ((self t)) self)



(defmethod* all-equal ((list list) &optional (test 'equal))
  :initvals '(nil equal) :indoc '("a list" "a binary test function") 
  :icon 209
  :doc "Tests if all elements of <list> are equal (using <test>).
Returns the value if this is true.

Ex. (all-equal '(8 8 8) '=)  => 8
Ex. (all-equal '(8 8 7 8) '=) => NIL"

  (if (loop for ll on list 
            while (or (null (cdr ll)) ;; last elem
                      (funcall test (car ll) (cadr ll)))
            finally return ll) ;; if this is nil, then all elements were equal
      NIL
    (values T (car list))))



;===========================
; MEAN
;===========================

(defmethod* om-mean ((self list) &optional (weights 1))
  :initvals (list '(1) 1) :indoc '("list of numbers" "list of numbers") :icon 209
  :doc "Arithmetic mean of numbers in a list. 

The optional input <weights> is a list of weights used to ponderate the successive elements in the list.

Ex. (om-mean '(1 2 3 4))  => 2.5
Ex. (om-mean '(1 2 3 4) '(3 2 1 1))  => 2.0
"
  (average self weights))


;===========================
; RANDOM GENERATORS
;===========================

;;; COMPATIBILITY
(defmethod* aleanum ((high number) (low number)) 
  :initvals '(0 0) :indoc '("min" "max") :icon 200 
  (om-random high low))


(defmethod* om-random ((low number) (high number))
  :initvals '(0 1) :indoc '("min value" "max value") :icon 'alea 
  :doc "Returns a random number between <low> and <high>."
  (if (zerop (- low high))
    (+ low high (- high))
    (let ((low-min (min low high)))
      (if (or (floatp low) (floatp high))
        (+ (om-random-value (- (max low high) low-min)) low-min)
        (+ (om-random-value (- (1+ (max low high)) low-min)) low-min)))))


(defmethod* perturbation ((self number) (percent number))
  :initvals '(1 0) :indoc '("number or list"  "number or list") :icon 200
  :doc "Applies to <self> a random deviation bounded by the <percent> parameter, a value in [0 1]. 
<self> and <percent> can be numbers or lists."
  (mulalea self percent))


(defmethod* perturbation ((self number) (num list)) 
  (mapcar #'(lambda (input)
                 (perturbation self input)) num))

(defmethod* perturbation ((self list) (num number))   
  (mapcar #'(lambda (input)
              (perturbation  input num)) self))

(defmethod* perturbation ((self list) (num list))
  (mapcar #'(lambda (input1 input2)
              (perturbation input1 input2)) self num))


;===========================
; SCALES
;===========================

(defmethod* om-scale ((self number) (minout number) (maxout number) &optional (minin 0) (maxin 0))
  :initvals '(1 0 1 0 1) :indoc '("number or list"  "a number" "a number" ) :icon 209
  :doc 
"Scales <self> (a number or list of numbers) considered to be in the interval [<minin> <maxin>] towards the interval [<minout> <maxout>].

If [<minin> <maxin>] not specified or equal to [0 0], it is bound to the min and the max of the list.

Ex. (om-scale 5 0 100 0 10)  => 50
Ex. (om-scale '(0 2 5) 0 100 0 10)  => (0 20 50)
Ex. (om-scale '(0 2 5) 0 100)  => (0 40 100)
 "
  (if (= maxin minin)
    minin
    (+ minout (/ (* (- self minin) (- maxout minout)) (- maxin minin)))))


(defmethod* om-scale ((self list) (minout number) (maxout number) &optional (minin 0) (maxin 0))
    (let ((min minin) (max maxin))
      (when (= minin maxin) 
        (setf min (list-min self) max (list-max self))) 
      (when (= min max)
        (setf min 0 max (abs max)))
      (mapcar #'(lambda (item) (om-scale item minout maxout min max)) self)))

(defmethod* om-scale ((self null) (minout number) (maxout number) &optional (minin 0) (maxin 0)) nil)

;------------------------------------------------------------------------
;;
;;this comes directly from PW. makes some functions generic
;;
(defmethod less-tree-mapcar ((fun function) (arg1 number) (arg2 number) &optional deep)
  (funcall fun (list arg1)
           (if deep arg2 (list arg2))))
 
(defmethod less-tree-mapcar ((fun function) (arg1 cons) (arg2 number) &optional deep)
  (if (consp (first arg1))
    (cons (less-tree-mapcar fun (car arg1) arg2 deep)
          (less-tree-mapcar fun  (cdr arg1) arg2 deep))
    (funcall fun arg1 (if deep arg2 (list arg2)))))

(defmethod less-tree-mapcar ((fun function) (arg1 null) arg2 &optional deep)
  (declare (ignore arg1 arg2 deep)) nil)

(defmethod less-tree-mapcar ((fun function) (arg1 number) (arg2 cons) &optional deep)
  (if (consp (first arg2))
    (cons (less-tree-mapcar fun arg1 (car arg2) deep)
          (less-tree-mapcar fun  arg1 (cdr arg2) deep))
    (funcall fun (list arg1) (car arg2))))

(defmethod less-tree-mapcar ((fun function) arg1 (arg2 null) &optional deep)
  (declare (ignore arg1 arg2 deep)) nil)

(defmethod less-tree-mapcar ((fun function) (arg1 cons) (arg2 cons) &optional deep)
  (if (or deep (consp (first arg1)) (consp (first arg2)))
    (cons (less-tree-mapcar fun (car arg1) (car arg2) deep)
          (less-tree-mapcar fun  (cdr arg1) (cdr arg2) deep))
    (funcall fun arg1 arg2)))

(defun g-scaling/sum (list sum)
  "scales <list> (may be tree) so that its sum becomes <sum>. Trees must be
well-formed. The children of a node must be either all leaves or all
nonleaves. "
  (less-tree-mapcar #'(lambda (x y) (om* x (/ y (apply #'+ x)))) list sum t))

(defmethod* om-scale/sum ((self list) (sum number))
  :initvals (list '(1 2 3) 10) :indoc '("number or list"  "number" ) :icon 209
  :doc "Scales <self> so that the sum of its elements become <sum>

Ex. (om-scale/sum '(2 3 5) 30)  => (6 9 15) "
  (g-scaling/sum self sum))

(defmethod* om-scale/sum ((self number) (sum number) )
  (g-scaling/sum self sum))


;===========================
; FACTORIZATION
;===========================


(defmethod* factorize ((number number)) 
  :initvals '(100)
  :indoc '("an integer")
  :doc "Returns the prime decomposition of <number> in the form ((prime1 exponent1) (prime2 exponent2) ...)
Primes known to the system are the 1230 primes ranging from 1 to 9973.

Ex. (factorize 100) => ((2 2) (5 2))     [100 = 2^2 * 5^2]"
  :icon 209
  (prime-facts number))



;; curve exp => 0 = lineaire
(defun number-interpolation (n1 n2 n curve)
  (+ n1 (* (- n2 n1) (expt n (exp curve)))))

(defun number-interpole-values (begin end samples curve)
  (if (<= samples 1)
      (list (number-interpolation begin end 0.5 curve))
    (let ((step (/ 1 (1- samples))))
      (loop for j from 0 to (1- samples) collect
            (number-interpolation begin end (* j step) (- curve))))))

(defmethod* interpolation ((begin number) (end number) (samples integer) (curve number))
  :initvals '(0 1 5 0.0)
  :icon 209
  :indoc '("number or list" "number or list" "integer" "number")
  :doc "Interpolates 2 numbers or lists (from <begin> to <end>) through <samples> steps.

<curve> is an exponential factor interpolation (0 = linear).
" 
  (number-interpole-values begin end samples curve))

(defmethod* interpolation ((begin number) (end list) (samples integer) (curve number))
  (mat-trans (mapcar #'(lambda (item)  (interpolation  begin item samples curve))
          end)))
          
(defmethod* interpolation ((begin list) (end number) (samples integer) (curve number))
  (mat-trans (mapcar #'(lambda (item) (interpolation item end samples  curve))
          begin)))
          
(defmethod* interpolation ((begin list) (end list) (samples integer) (curve number))
  (mat-trans (mapcar #'(lambda (item1 item2) (interpolation item1 item2 samples curve))
          begin end)))
          


;==============================
; GREATEST COMMON DIVISOR
;==============================

(defmethod pgcd ((a rational) (b rational))
   "Find the greats common divisor bethween 2 rational."
   (let ((x (* (denominator a) (numerator b)))
         (y (* (denominator b) (numerator a)))
         (d (* (denominator a) (denominator b))))
     (/ (gcd x y) d)))

(defmethod pgcd ((a integer) (b integer))
   "Find the greats common divisor bethween 2 rational."
   (gcd a b))

(defmethod pgcd ((a number) (b number))
   "Find the greats common divisor bethween 2 rational."
   (pgcd (rational a) (rational b)))

