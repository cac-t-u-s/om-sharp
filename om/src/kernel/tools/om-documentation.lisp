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

;;;=====================================================================
;;; Documentation for Lisp functions
;;;=====================================================================

(in-package :om)

(setf (documentation 'first 'function) 
      "Returns the 1st element in <list>.

(Equivalent to Lisp CAR)

Ex. (first '(1 2 3 4 5 6)) ==> 1")

(setf (documentation 'second 'function) 
      "Returns the 2nd element in <list>.

Ex. (second '(1 2 3 4 5 6)) ==> 2")

(setf (documentation 'third 'function) 
      "Returns the 3rd element in <list>.

Ex. (third '(1 2 3 4 5 6)) ==> 3")

(setf (documentation 'nth 'function) 
      "Returns the <n>th element in <list>.
The count starts from 0, i.e. (nth 0 list) is the first element of the list.

Ex. (nth 0 '(1 2 3 4 5 6)) ==> 1
Ex. (nth 2 '(1 2 3 4 5 6)) ==> 3")

(setf (documentation 'rest 'function) 
      "Returns the tail of <list>, i.e. the same list without irts first element.

(Equivalent to Lisp CDR)

Ex. (rest '(1 2 3 4 5 6)) ==> (2 3 4 5 6)")

(setf (documentation 'nthcdr 'function) 
      "Returns the tail of <list> that would be obtained by calling REST <n> times in succession, i.e. without its <n> first elements.

Ex. (nthcdr 2 '(1 2 3 4 5 6)) ==> (3 4 5 6)")

(setf (documentation 'butlast 'function) 
      "Returns a copy of <list> without its last element or without its last <n> elements if <n> is supplied.

Ex. (butlast '(1 2 3 4 5 6)) ==> (1 2 3 4 5)
Ex. (butlast '(1 2 3 4 5 6)) ==> (1 2 3)")

(setf (documentation 'reverse 'function) 
      "Returns a new sequence or list of the same kind as <sequence>, containing the same elements but in reverse order.

Ex. (reverse '(1 2 3 4 5 6)) ==> (6 5 4 3 2 1)")

(setf (documentation 'length 'function) 
      "Returns the number of elements in <sequence> (a list, a string, ...)

Ex. (length '(1 2 3 4 5 6)) ==> 6
Ex. (length \"hello\") ==> 5")

(setf (documentation 'list 'function) 
      "Returns a list containing the supplied objects (<args>).

Ex. (list 1 2 'a 7) ==> (1 2 a 7)


LIST also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.
")

(setf (documentation 'remove 'function) 
      "Returns a new sequence that has the same elements as <sequence> except those that satisfy the test <test> with <item>.
By default the test is 'eql so the items that are equal to <item> are removed.

<test> can be a function or a function name.
<start> and <end> determine bounding indices in the original sequence for removing elements.
<count> allows to specify a maximal number of items to remove.
<from-end> if T, starts removing items from end of the sequence
<key> is a function applyed to each item before to be tested
<test-not> is used to remove elemet that do not satistfy the test (deprecated use)

Ex. (remove 5 '(2 5 6 7 5 3)) ==> (2 6 7 3)
Ex. (remove 5 '(2 5 6 7 5 3) :test '>) ==> (2 3)
Ex. (remove 5 '((b 2) (c 5) (d 6) (e 7) (f 5) (g 3))) :key 'second) ==> ((b 2) (d 6) (e 7) (g 3))
")


(setf (documentation 'cons 'function)
      "Creates a new CONS with <car> and <cdr>. 
A CONS is a basic compound data object having two components called the CAR and the CDR
A LIST is recursively defined as a CONS which CDR is a list.

Ex. (cons 'a 'b) ==> (a . b)
Ex. (cons 'a nil) ==> (a)
Ex. (cons 'a '(b c)) ==> (a b c)
Ex. (cons 'a (cons 'b nil)) ==> (a b)")


(setf (documentation 'append 'function) 
      "Returns a new list that is the concatenation of the <lists>.

Ex. (append '(1 2 3) '(4 5)) ==> (1 2 3 4 5)")

(setf (documentation 'apply 'function) 
     "Applies <function> to the arguments in <arg>.

<function> is a function or function name.
<arg> is a list of arguments.

Ex. (apply '+ '(3 4)) ==> 7
")

(setf (documentation 'funcall 'function) 
      "Applies <function> to <args>.

<function> is a function or function name.
<args> are the arguments.

Ex. (funcall '+ 3 5) ==> 8")

(setf (documentation 'mapcar 'function) 
      "Operates on successive elements of <list> (and of the other lists of <more-lists>). 

<function> can be a function or function name. It is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the list runs out, or when the shorter list runs out if various lists are supplied. Excess elements in other lists are ignored. 

The value returned is a list of the results of successive calls to <function>.")

(setf (documentation 'mapcan 'function) 
      "Operates on successive elements of <list> (and of the other lists of <more-lists>). 

<function> can be a function or function name. It is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the list runs out, or when the shorter list runs out if various lists are supplied. Excess elements in other lists are ignored. 

The value returned is a the concatenation of the results of successive calls to <function>. <function> should therefore return lists.")


(setf (system::class-documentation (find-class 't))
      "Special constant in Lisp, meaning 'TRUE'.

T can be used as a type specifier in class slots or method arguments in order to describe any object type (all objects inherit from the Lisp class 'T').")

(setf (system::class-documentation (find-class 'integer))
      "Integer number.

This class is mainly used in OM to set types in other class slots or method arguments.")

(setf (system::class-documentation (find-class 'number))
      "Any type of number.

This class is mainly used in OM to set types in other class slots or method arguments.")


(setf (documentation 'float 'function)
      "Float number (decimal).

FLOAT exists in Lisp as a functions, which converts its input (a real number) to a float number.
Ex. (float 4)  =>  4.0
Ex. (flat 1/2) =>  0.5

FLOAT also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.")

(setf (documentation 'rational 'function)
      "Rational number (P/Q).

RATIONAL exists in Lisp as a functions, which converts its input (a real number) to a rational number.

It also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.")

(setf (documentation 'string 'function)
      "Vector of characters.

STRING exists in Lisp as a functions, which converts its input (another string, a symbol or a character) into a string.
Ex. (string 'hello) => \"hello\"

It also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.

Strings are represented as characters between double-quotes (\"\").

")

(setf (documentation 'null 'function)
      "In Common Lisp, NIL, also notated '(), represents the empty list.

The function NULL tests if something is equal to NIL or not.
This function returns T if the argument is NIL, and NIL if not.

Ex. (null 4) => NIL
Ex. (null NIL) => T
Ex. (null '()) => T
Ex. (null '(5 6 7)) => NIL

NULL is also the name of a class (the 'class of NIL'), and can be used in OM to specialize method arguments.")
