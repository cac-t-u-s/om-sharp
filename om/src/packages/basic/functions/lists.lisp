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

(defmethod* last-elem ((list list))
  :initvals '(nil) 
  :indoc '("a list")
  :doc 
"Returns the last element of <list>

Ex. (last-elem '(1 2 3 4 5))  => 5
"
  :icon 'list
  (first (last (list! list))))

(defmethod* last-n ((list list) (n integer))
  :initvals '(nil 0) 
  :indoc '("a list" "number of elements")
  :icon 'list
  :doc  
  "Returns the <n> last elements of <list>

Ex. (last-n '(1 2 3 4 5) 3)  => (3 4 5)
"
  (last list n))


(defmethod* first-n ((list list) (n integer))
  :initvals '(nil 0) 
  :indoc '("a list" "number of elements")
  :icon 'list
  :doc  
"Returns the <n> first elements of <list>

Ex. (first-n '(1 2 3 4 5) 3)  => (1 2 3)
"
  (cond
   ((< (length list) n) list)
   (t  (butlast list (- (length list) n)))))


(defmethod* x-append ((l1? list) (l2? list) &rest lst?)
  :initvals '(nil nil nil) 
  :indoc '("first element" "second element" "additional elements")
  :icon 'list
  :doc "Appends lists or atoms together to form a new list.

Ex. (x-append '(1 2 3) '(4 5)) => (1 2 3 4 5)
Ex. (x-append 1 '(4 5)) => (1 4 5)
Ex. (x-append '(1 2 3) 4) => (1 2 3 4)

This function also works with additional elements.

Ex. (x-append '(1 2 3) 4 '(5 6 7)) => (1 2 3 4 5 6 7)

"
  (apply 'append l1? l2? (mapcar #'list! lst?)))

(defmethod* x-append ((l1? t) (l2? list) &rest lst?)
  (apply 'append (list l1?) l2? (mapcar #'list! lst?)))

(defmethod* x-append ((l1? list) (l2? t) &rest lst?)
  (apply 'append l1? (list l2?) (mapcar #'list! lst?)))

(defmethod* x-append ((l1? t) (l2? t) &rest lst?)
  (apply 'append (list l1?) (list l2?) (mapcar #'list! lst?)))

;-----------FLAT


(defun rev-flat (lst)  
  (let ((l ()))
    (loop while lst do
          (if (not (consp (car lst)))
              (push (pop lst) l)
            (setq l (nconc (rev-flat (pop lst)) l))))
    l))


(defun lo-flat (list) 
  (cond ((atom list) list)
        ((atom (car list)) (cons (car list) (lo-flat (cdr list))))
        ((atom (caar list)) (apply 'append list))
        (t (cons (lo-flat (car list)) (lo-flat (cdr list))))))

(defmethod flat-low ((list list)) 
  (lo-flat list))

(defmethod flat-once ((list list))
  (if (consp (car list))
    (apply 'append list)  list))

(defmethod flat-one ((list list))
  (loop for item in list
        append (list! item)))

(defmethod n-flat-one ((list list) (level integer))
  (let ((rep list))
    (loop for i from 1 to level do
          (setf rep (flat-one rep)))
    rep))


(defmethod* flat ((lst list) &optional (level nil)) 
  :initvals '(nil nil) 
  :indoc '("a list" "level of parenthesis")
  :icon 'list
  :doc "Transforms a tree-list (i.e. a list of lists) into a flat list.
If <level> is 1 (resp n) remove 1 (resp. n) level(s) of list imbrication.
If <level> is NIL (default) remove all levels of imbrication, down to a purely flat list.

Ex. (flat '((a b) c ((d e) f)))  =>  (a b c d e f)
Ex. (flat '((a b) c ((d e) f)) 1)  =>  (a b c (d e) f)    [1 level of parenthesis]
"
  (cond
   ((null level) (nreverse (rev-flat lst)))
   ((= level 0) lst)
   ((and (integerp level) (plusp level))
    (n-flat-one lst level))
   (t lst)))

;-----------CREATE-LIST

(defmethod* create-list ((count integer) (elem t))
  :initvals '(10 nil) 
  :indoc '("number of elements" "initial element")
  :icon 'list
  :doc "Returns a list of length <count> filled with repetitions of element <elem>

Ex. (create-list 4 'a)  =>  (a a a a)"
  (make-list count :initial-element elem))

;-----------MATRIX TRANSPOSITION

(defmethod* mat-trans ((matrix list))
  :initvals '(nil)
  :indoc '("a list of lists")
  :doc "Matrix transposition. 
The matrix is represented by a list of rows. Each row is a list of items.
Rows and columns are interchanged.

Ex. (mat-tran '((1 2 3) (a b c) (4 5 6))  =>  ((1 a 4) (2 b 5) (3 c 6))"
  :icon 'mat-trans
  (let ((maxl (1- (loop for elt in matrix maximize (length elt))))
        result)
    (loop for i from 0 to maxl do
         (push (mapcar #'(lambda (list) (nth i list)) matrix) result))
    (nreverse result)))

;----------------EXPAND LIST

(defvar *valid-expand-chars* '(#\* #\_))

(defun is-in (list chars)
  (let (res)
    (dolist (ch chars res)
      (if (setq res (member ch list :test #'char=)) (return res)))))


(defmethod* expand-lst ((list list))
  :icon 'list 
  :initvals '('(3*(2 4) 0_8))
  :indoc '("a list to expand")
  :doc  "Expands a list following repetition patterns.

1. <number>* (x1 ...x2)
repeats the pattern x1...x2 <number> times.

2. <n>_<m>s<k>
appends an arithmetic series counting from <n> to <m> by step <k>.
s<k> can be omitted (k=1). 

Ex. (expand-lst (3* (2 4) 0_8))  =>  (2 4 2 4 2 4 0 1 2 3 4 5 6 7 8)
Ex. (2* (a z 2* (4 12) (1_5 )) 0_16s2)  =>  (a z 4 12 4 12 (1 2 3 4 5) a z 4 12 4 12 (1 2 3 4 5) 0 2 4 6 8 10 12 14 16)"

  (and list
       (let ((lists (list! list))  result)
         (loop while lists do 
                (let ((next-elem (pop lists)))
                  (cond 
                   ((symbolp next-elem)
                    (let* ((form (coerce (format () "~A" next-elem) 'list))
                           (from-char (is-in form *valid-expand-chars*))
                           (char-symb (car from-char))
                           (third (cdr from-char))
                           (int (butlast form (length from-char)))
                           up-to)
                      (cond 
                       ((and (not third) char-symb (char= #\* char-symb) int
                             (numberp (setq int (read-from-string (coerce int 'string)))))
                        (push (apply #'append
                                     (make-list int
                                                :initial-element 
                                                (expand-lst (pop lists))))
                              result))
                       ((and char-symb (char= #\_ char-symb) third
                             (numberp (setq int (read-from-string (coerce int 'string)))))
                        (if (setq from-char (member #\s  ;;;[CR, 14/01/99] #\S
                                                    third :test #'char=))
                          (progn (setq up-to (butlast third (length from-char))
                                       char-symb (car from-char) third (cdr from-char))
                                 (if (and char-symb 
                                          (char= #\s ;;;[CR, 14/01/99] #\S
                                                 char-symb)
                                          (or (null third)
                                              (numberp 
                                               (setq third (read-from-string (coerce third 'string)))))
                                          (numberp 
                                           (setq up-to (read-from-string (coerce up-to 'string)))))
                                   (push (arithm-ser int up-to (or third 1)) result)
                                   (push (list next-elem) result)))
                          (progn
                            (setq up-to (read-from-string (coerce third 'string)))
                            (push (arithm-ser int up-to 1) result))
                          ))
                       (t (push (list next-elem) result)))))
                   ((consp next-elem)
                    (push (list (expand-lst next-elem)) result))
                   (t (push (list next-elem) result)))))
         (apply #'append (nreverse result)))))



;;;-----------------GROUP-LIST


(defmethod* group-list ((list list) (segmentation list) mode)
   :icon 'list 
   :initvals '((1 2 3 4) (1 3) linear)
   :indoc '("list to group (anything)" "list of group lengths (numbers)" "linear or circular")
   :doc  "Segments a <list> in successives sublists which lengths are successive values of the list <segmentation>.
 <mode> indicates if <list> is to be read in a circular way.

Ex. (group-list '(1 2 3 4) '(1 3) 'linear)  => ((1) (2 3 4))
Ex. (group-list '(1 2 3 4) '(1 2 3) 'linear)  => ((1) (2 3) (4))
Ex. (group-list '(1 2 3 4) '(1 2 3) 'circular)  => ((1) (2 3) (4 1 2))
"
   :menuins '( (2 ( ("linear" linear) ("circular" circular))))
   (let ((list2 list) (res nil))
     (catch 'gl
      (loop for segment in segmentation
            while (or list2 (eq mode 'circular))
            do (push (loop for i from 1 to segment
                           when (null list2)
                           do (case mode
                                (linear (push sublist res) (throw 'gl 0))
                                (circular (setf list2 list)))
                           end
                           collect (pop list2) into sublist
                           finally (return sublist))
                              res))
     )
     (nreverse res)
     ))

(defmethod* group-list ((list list) (segmentation number) mode)
  (group-list list (make-list (ceiling (length list) segmentation) :initial-element segmentation) 'linear))



;;;;-----------------Remove-dup
(defmethod* remove-dup ((list list) (test symbol) (depth integer))
  :icon 'list 
  :initvals (list '(1 2 3 4) 'eq 1)
  :indoc '("a list" "equality test (function or function name)" "an integer")
  :doc  "Removes duplicates elements from <list>.
If <depth> is more than 1 duplicates are removed from sublists of level <depth>.

Ex. (remove-dup '(1 2 3 2 2 4) '= 1) => (1 3 2 4)
Ex. (remove-dup '((1 2) (3 2 2) 4) '= 2) => ((1 2) (3 2) 4)
"
  (remove-dup list (symbol-function test) depth))


(defmethod* remove-dup ((list list) (test function) (depth integer))
  (if (<= depth 1)
    (remove-duplicates list :test test)
    (mapcar #'(lambda (x) (remove-dup x test (1- depth))) list)))


(defmethod* remove-dup ((list t) (test t) (depth integer)) list)

;;;-----------------LIST-MODULO

(defmethod* list-modulo ((list list) (ncol integer))
  :initvals '(nil 2) 
  :indoc '("a list" "modulo")
  :icon 'list
  :doc 
  "Groups the elements of a list distant of a regular interval <ncol> and returns these groups as a list of lists. 

Ex. (list-modulo '(1 2 3 4 5 6 7 8 9) 2)  => ((1 3 5 7 9) (2 4 6 8))
Ex. (list-modulo '(1 2 3 4 5 6 7 8 9) 3)  => ((1 4 7) (2 5 8) (3 6 9))
"
  (when (and (> ncol 0) (< ncol (length list))) (list-part list ncol)))

(defun list-part (list ncol)  
  (let ((vector (make-array  ncol )) res)
    (loop while list do 
      (for (i 0 1 (1- ncol))
        (and list (vset vector i (push (pop list) (vref vector i))))))
    (for (i 0 1 (1- ncol))
      (push (remove nil (nreverse (vref vector i))) res))
    (nreverse res)))

;;;-----------------INTERLOCK
(defmethod* interlock ((lis1 list) (lis2 list) (plc1 list))
   :initvals '((0 1 2 3) (a b) (1 3))
   :indoc '("a list" "a list" "a list of indexes")
   :icon 'list
   :doc "Inserts the successive elements of <lis2> in <lis1> before the elements of <lis1> of respective positions from <plc1>.

Ex. (interlock '(0 1 2 3 ) '(a b) '(1 3))  =>  (0 a 1 2 b 3)"
   (let ((aux) (pointeur 0))
     (dotimes (n (length lis1) (reverse aux))
       (when  (member n plc1)
         (progn ()
                (push (nth pointeur lis2) aux)
                (incf pointeur)))
       (push (nth n lis1) aux))))

;;;-----------------SUBS-POSN

(defmethod* subs-posn ((lis1 list) posn val)
   :initvals '((0 1 2 3)  (1 3) (a b))
   :indoc '("a list" "a list of indices" "a list or value")
   :icon 'list
   :doc "Substitutes the elements of <lis1> at position(s) <posn> (if they exist) with the corresponding elements in <val>.

Ex. (subs-posn '(0 1 2 3) 2 'a)  => (0 1 a 3)
Ex. (subs-posn '(0 1 2 3) '(1 3) '(a b))  => (0 a 2 b)
"
   (let ((copy (copy-list lis1)))
    (loop for item in (list! posn)
          for i = 0 then (+ i 1) do
          (setf (nth item copy) (if (listp val) (nth i val) val)))
    copy))


;;; old
;;(setf posn (sort posn '<))
;;   (loop for elt in lis1
;;         for counter from 0
;;         if (and posn (= counter (first posn))) collect (pop val) and do 
;;         (pop posn) else collect elt)




;------------------------------------------------------------------------
(defmethod* reduce-tree ((self t) (function symbol) &optional (accum nil))
  :initvals (list '(10 10) '+ nil)
  :icon 'list
  :indoc '("a tree (list)" "a function or a patch" "a neutral value for <function>")
  :doc "Applies the commutative binary <function> recursively throughout the list <self>.
(Applies to the first elements, then the result to the next one, and so forth until the list is exhausted.)

Function '+, for instance, makes reduce-tree computing the sum of all elements in the list.

Optional <accum> should be the neutral element for the <function> considered (i.e. initial result value).
If <accum> is nil, figures out what the neutral can be (works for +,*,min,max)."
  (reduce-tree self (symbol-function function) accum))


(defmethod* reduce-tree ((self list) (function function) &optional (accum nil))
  (unless accum (setf accum (neutral-element function)))
  (if (null self)
    accum
    (reduce-tree (rest self) function (reduce-tree (first self) function accum))))

(defmethod*  reduce-tree ((self number) (function function) &optional (accum nil))
  (funcall function self accum))


(defun neutral-element (function)
  (case (function-name function)
    (+ 0)
    (* 1)
    (min MOST-POSITIVE-LONG-FLOAT)
    (max MOST-NEGATIVE-LONG-FLOAT)
    (t 0)))



;------------------------------------------------------------------------
;by M. Malt

(defmethod* rang-p ((liste list) (elem number) &optional (test 'eq) (key nil)) 
  :initvals '((6000) 2)
  :indoc '("a list"  "element to look for" "test function" "key function")
  :icon 'list 
  :doc "Returns the position(s) of <elem> in <liste>.

<test> is a function or function name used to test if the elements of the list are equal to <elem>.
<key> is a function or function name that will be applied to elements before the test.

Ex. (rang-p '(0 1 2 3 4 3 2) 3)  =>  (3 5)
Ex. (rang-p '(0 1 2 3 4 3 2) 3 '<)  =>  (0 1 2 6)    [elements at positions 0, 1,2 and 6 are lower than 3]
"
  (let ((aux nil) (index 0))
    (mapcar #'(lambda (z) (progn (when (funcall test (if key (funcall key z) z) elem) (push index aux))
                                 (incf index))) 
            liste)
    (reverse aux)))


(defmethod* rang-p ((liste list) (elem list) &optional (test 'eq) (key nil)) 
  (let ((aux nil) (index 0))
    (mapcar #'(lambda (z) (progn (when (funcall test (if key (funcall key z) z) elem) (push index aux))
                                 (incf index))) 
            liste)
    (reverse aux)))


;------------------------------------------------------------------------
; more list operators

; list-explode list-filter table-filter band-filter range-filter posn-match

(defmethod* list-explode ((list list) (nlists integer))
  :initvals '((1 2 3 4 5 6) 2)
  :indoc '("List" "segment size")
  :icon 'list
  :doc
  "Segments <list> into <nlist> sublists of (if possible) equal length.

Ex. (list-explode '(1 2 3 4 5 6 7 8 9) 2)  => ((1 2 3 4 5) (6 7 8 9))
Ex. (list-explode '(1 2 3 4 5 6 7 8 9) 5)  => ((1 2) (3 4) (5 6) (7 8) (9)).

If the number of divisions exceeds the number of elements in the list, the divisions will have one element each, and remaining divisions are repeat the last division value.

Ex. (list-explode '(1 2 3 4 5 6 7 8 9) 12)  => ((1) (2) (3) (4) (5) (6) (7) (8) (9) (9) (9) (9))
"
  (if (> nlists (length list))
    (setq list (append list (make-list (- nlists (length list))
                                       :initial-element (first (last list))))))
  (if (<= nlists 1) list
      (let* ((length (length list))
             (low (floor length nlists))
             (high (ceiling length nlists))
             (step (if (< (abs (- length (* (1- nlists) high))) (abs (-
                                                                      length (* nlists low))))
                     high  low))
             (rest (mod length nlists))
             (end (- length 1 rest))
             (ser (arithm-ser 0  (1- step) 1))
             res)
        (for (i 0 step end)
          (push (remove () (posn-match  list (om+  i ser))) res))
        (setq low (length (flat-once res)))
        (if (< low length) (setq res (cons (append (first res) (nthcdr low
                                                                       list)) (rest res))))
        (cond ((> (length res) nlists)
               (nreverse (cons (nconc (second res) (first res)) (nthcdr 2
                                                                        res))))
              ((< (length res) nlists)
               (when (= (length (first res)) 1)
                 (setq res (cons (nconc (second res) (first res)) (nthcdr 2
                                                                          res))))
               (nreverse (nconc (nreverse (list-explode (first res) (1+ (-
                                                                         nlists (length res)))))
                                (rest res))))
              (t (nreverse res))))))

(defmethod* list-filter ((test symbol) (list list) (mode symbol))
  :initvals '(numberp (1 2 3) pass)
  :indoc '("function or function name" "a list" "pass or reject")
  :menuins '((2 (("Reject" reject) ("Pass" pass))))
  :icon 'list 
  :doc  "Filters out  <list> using the predicate <test>.

<test> may be a function name (a symbol) or it may be a visual function or patch in 'lambda' mode. 

If <list> is a list of lists, the filter is applied recursively in the sub-lists.

<mode> 'reject' means reject elements that verify <test>. 
<mode>'pass' means retain only elements that verify <test>.

Ex. (list-filter 'numberp '(5 6 aaa bbb 8) 'pass)  => (5 6 8)
Ex. (list-filter 'numberp '(5 6 aaa bbb 8) 'reject)  => (aaa bbb)
 "
  
  (list-filter  (symbol-function test) list mode))

(defmethod* list-filter ((test function) (list list) (mode symbol))
  (if (eq mode 'reject)
    (do-filter list #'(lambda (x) (not (funcall test x))))
    (do-filter list test)))



(defmethod* table-filter ((test symbol) (list list) (numcol integer) (mode symbol))
  :initvals '(numberp ((1 2) (1 2)) 1 pass)
  :indoc '("function or function name" "list of lists" "rank" "pass or reject")
  :menuins '((3 (("Reject" 'reject) ("Pass" 'pass))))
  :icon 'list 
  :doc  "Filters out <list> (a list of lists) using the predicate <test>.

<test> may be a function name (a symbol) or it may be a visual function or patch in 'lambda' mode. 

The predicate <test> is applied to the element of rank <numcol> in every sublist in <list> and filters the whole sublists.

<numcol> counts from 0.

<mode> 'reject' means reject elements whose <numcol>th element verifies <test>. 
<mode>'pass' means retain only elements whose <numcol>th element verifies <test>.

Ex. (table-filter 'oddp '((1 2 3) (4 5 6) (7 8 9)) 1 'pass)  --> ((4 5 6))       [keeps lists which first element is odd]
Ex. (table-filter 'oddp '((1 2 3) (4 5 6) (7 8 9)) 1 'reject)  --> ((1 2 3) (7 8 9))     [rejects lists which first element is odd]
"
  
  (table-filter  (symbol-function test) list numcol mode))

(defmethod* table-filter ((test function) (list list) (numcol integer) (mode symbol))
  (if (eq mode 'reject)
    (do-table-filter list #'(lambda (x) (not (funcall  test x))) numcol)
    (do-table-filter list  test numcol)))


(defmethod* band-filter ((list list) (bounds list) (mode symbol))
  :initvals '((1 2 3 4 5) ((0 2) (5 10)) pass)
  :indoc '("a list" "a list of (low high) pairs" "pass or reject" )
  :menuins '((2 (("Reject" 'reject) ("Pass" 'pass))))
  :icon 'list 
  :doc  "Filters out <list> using <bounds>.
<bounds> is a pair or list of pairs (min-value max-value). 

If <list> is a list of lists, the filter is applied recursively in the sub-lists.

If <bounds> is a list of pairs, each pair is applied to each successive element in <list>

<mode> 'reject' means reject elements between the bounds 
<mode> 'pass' means retain only elements between the bounds
"
  (list-filter 
   #'(lambda (item)
       (some #'(lambda (bound) (and (>= item (first bound)) (<= item (second bound)))) bounds))
   list 
   mode))


(defmethod* range-filter ((list list) (posn list) (mode symbol))
  :initvals '((1 2 3 4 5) ((0 1) (3 4)) reject)
  :indoc '("a list" "position bounds" "pass or reject")
  :menuins '((2 (("Reject" reject) ("Pass" pass))))
  :icon 'list 
  :doc  "Select elements in <list> whose positions (couting from 0) in the list are defined by <posn>
<posn> is a list of pairs (min-pos max-pos) in increasing order with no everlap.

<mode> 'reject' means reject the selected elements. 
<mode> 'pass' means retain only the selected elements.

Ex. (range-filter '(10 11 12 13 14 15 16) '((0 1) (3 4)) 'pass)  => (10 11 13 14)
Ex. (range-filter '(10 11 12 13 14 15 16) '((0 1) (3 4)) 'reject) => (12 13)
"
  (loop for item in list
        for i from 0 
        with bound = (pop posn)
        while bound
        when (and (eq mode 'pass) (>= i (first bound)) (<= i (second bound))) collect item
        when (and (eq mode 'reject) (or (< i (first bound)) (> i (second bound)))) collect item
        when (> i (second bound)) do (setf bound (pop posn))))



(defmethod* posn-match ((list list) (positions list))
  :initvals '((10 20 30 40 50 60 70 80 90) ((0 1) 4 (6)) )
  :indoc '("a list" "a list positions")
  :icon 'list 
  :doc  "Constructs a new list by peeking elements in <list> at positions defined by <positions> (a list or tree of positions). 

<positions> supports the syntax of 'expand-lst'.

Ex. (posn-match '(10 20 30 40 50 60 70 80 90) '((0 1) 4 (6))) => ((10 20) 40 (60))
Ex. (posn-match '(10 20 30 40 50 60 70 80 90) '(3*(0) 3_6)) => (10 10 10 40 50 60 70)
"
  (do-posn-match list (expand-lst positions)))


(defmethod* posn-match ((list list) (positions integer) )
  (nth positions list ))

(defmethod do-posn-match ((self list) (positions list))
  (cond 
   ((numberp positions) (nth positions self))
   ((listp positions)
    (loop for pos in positions
          collect (do-posn-match self pos)))))

(defmethod do-posn-match ((self list) (positions number))
   (nth positions self))
   


(defmethod do-filter ((self t) (test function))
  (if (funcall test self) self 'fail))

(defmethod do-filter ((self cons) (test function)) 
  (loop for item in self 
        for elt =  (do-filter item test)
        when (not (eq elt 'fail)) collect elt))

(defmethod do-table-filter ((list list) (test function) (numcol integer))
  (loop for sublist in list
        when (funcall test (nth numcol sublist))
        collect sublist))


;;;;;;;;;;;;;;;;;;;;;;
;;; UTILS;;;;;;;;;;;;;

;find all positions of an element in a list
(defun om-all-positions (a L)
  (loop
   for element in L 
   and position from 0
   when (eql element a)
   collect position))

(defun remove-nth (n lst)
  (if (= n 0)
      (cdr lst)
    (append (subseq lst 0 n) (subseq lst (1+ n) (length lst)))))

(defun delete-nth (n lst)
  (if (zerop n)
      (cdr lst)
    (let ((cons (nthcdr (1- n) lst)))
      (if cons
          (setf (cdr cons) (cddr cons))
        cons))))

(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst)))
  lst)

