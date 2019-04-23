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
; Authors: G. Assayag, C. Agon, J. Bresson (code form OM6)
;=========================================================================


(in-package :om)

;-------UNION---------

(defun noRep-union (lists oper test key)
  (let ((the-union (list! (car lists))))
  (dolist (one-in (cdr lists))
      (setq the-union (funcall oper the-union (list!  one-in)  :test test :key key)))
  the-union))

(defmethod* x-union ((l1? list) (l2? list) 
                     &optional (test 'equal) (key 'identity)
                     &rest lists)
  :initvals '(nil nil equal identity)
  :indoc '("a list" "a list" "test function" "test key" "more lists")
  :doc "Merges lists (<l1?> and <l2?> and possibly more) into a single list with no repetitions. 

<test> is a function or function name for a binary comparison.
<key> is a name or function name to apply to the elements before comparison.

Ex. (x-union '(1 2 3 4 5) '(4 5 6 7 8)) => (8 7 6 1 2 3 4 5)
"
  :icon 191
  (noRep-union  (list* l1? l2? (and lists (list! lists))) 'union test key))

;-------INTERSECTION---------

(defmethod* x-intersect ((l1? list) (l2? list)
                         &optional (test 'equal) (key 'identity)
                         &rest list)
  :initvals '(nil nil equal identity)
  :indoc '("a list" "a list" "test function" "test key" "more lists")
  :doc "Returns the intersection (i.e. common elements) from lists (<l1?> and <l2?> and possibly more) into a single list.

<test> is a function or function name for a binary comparison.
<key> is a name or function name to apply to the elements before comparison.

Ex. (x-intersect '(1 2 3 4 5) '(4 5 6 7 8)) => (4 5)"
  :icon 191
  (nreverse (noRep-union (list* l1? l2? (and list (list! list))) 'intersection test key)))

;-------XOR---------

(defmethod* x-Xor ((l1? list) (l2? list)
                   &optional (test 'equal) (key 'identity)
                   &rest list) 
  :initvals '(nil nil equal identity)
  :indoc '("a list" "a list" "test function" "test key" "more lists")
  :doc "XOR's lists (<l1?> and <l2?> and possibly more) into a single list.
XOR keeps only the elements present in one list and not in the other one(s).

<test> is a function or function name for a binary comparison.
<key> is a name or function name to apply to the elements before comparison.

Ex. (x-xor '(1 2 3 4 5) '(4 5 6 7 8)) => (1 2 3 6 7 8)"
  :icon 191
  (nreverse (noRep-union (list* l1? l2? (and list (list! list))) 'set-exclusive-or test key)))

(x-xor '(1 2 3 4 5) '(4 5 6 7 8))

;-------XDIFF---------



(defmethod* x-diff ((l1? list) (l2? list)
                    &optional (test 'equal) (key 'identity)
                    &rest list)
  :initvals '(nil nil equal identity)
  :indoc '("a list" "a list" "test function" "test key" "more lists")
  :doc "Returns the list of elements present in <l1?> but not in <l2?>.

<test> is a function or function name for a binary comparison.
<key> is a name or function name to apply to the elements before comparison.

Ex. (x-diff '(1 2 3 4 5) '(4 5 6 7 8)) => (1 2 3)"
  :icon 191
  (nreverse (noRep-union (list* l1? l2? (and list (list! list))) 'set-difference test key)))

;------INCLUDED?------

(defmethod* included? ((lst1 list) (lst2 list) &optional (test 'equal))
  :initvals '(nil nil equal)
  :indoc '("a list" "a list" "test")
  :doc "Tests if <lst1> is included in <lst2>.

<test> is a function or function name for a binary comparison.

Ex. (included? '(1 2 3 4 5) '(4 5 6 7 8)) => NIL
Ex. (included? '(5 6) '(4 5 6 7 8)) => T
"
  :icon 191
  (subsetp lst1 lst2 :test test))

 
