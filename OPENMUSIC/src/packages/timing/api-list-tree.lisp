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
;;list-tree dépendance, mais inutile car api-DAG plus généraliste.

(defstruct task-with-cpt
  (lambda-f nil)
  (cpt 0 :type integer)
  ) 


(defun make-tree (data)
  "Creates a new node that contains 'data' as its data."
  (cons (cons data nil) nil))

(defun first-child (tree)
  "Returns a reference to the first child of the node passed in,
  or nil if this node does not have children."
  (cdr (car tree)))

(defun next-sibling (tree)
  "Returns a reference to the next sibling of the node passed in,
  or nil if this node does not have any siblings."
  (cdr tree))

(defun data (tree)
  "Returns the information contained in this node."
  (car (car tree)))

(defun get-lambda (tree)
(task-with-cpt-lambda-f(data tree)))

(defun get-cpt(tree)
(task-with-cpt-cpt(data tree)))

(defun first-child (tree)  (when (listp tree)
    (cdr (car tree))))


(defun parent(subtree tree)
 (car (member subtree tree :key #'cdr :test #'equal)))

(defun first-bro(subtree tree)
  (loop for i from 0 to (length (first-child tree)) do
       (if (equal (nthcdr i (first-child tree)) subtree)
           (return (first-child tree))))
)

(defun remove-childs(tree)
  (loop while (pop(cdr (car tree))))
  tree
)

(defun add-child (tree child)
  (setf (car tree) (append (car tree) child))
  tree)

(defun traverse (tree &optional (padding 0))
  (when tree
    (format t "~&~v@TData: ~A" padding (data tree))
    (when (first-child tree)
      (format t "  Children: ~A"
              (maplist #'(lambda (x) (data x))
                       (first-child tree))))
    (traverse (first-child tree) (+ padding 3))
    (traverse (next-sibling tree) padding)))

(defun get-list-parent-fils (tree-give)
(let* ((tree-return)(parent)(fils)(list-parent-fils))
  (when tree-give
    (setf parent (data tree-give))
    (print parent)
     (when (first-child tree-give)
       (setf fils  (maplist #'(lambda (x) (data x))
                       (first-child tree-give))))
(if (first-child tree-give)(setq list-parent-fils ( get-list-parent-fils(first-child tree-give))))
 (if (next-sibling tree-give)  (setq list-parent-fils ( get-list-parent-fils (next-sibling tree-give))))
  (push (list parent fils) list-parent-fils))
list-parent-fils))

(defun get-parent(list-parent-fils elt)
   (setq res nil)
  (if(equal (car (car list-parent-fils)) elt)
      (setq res nil))
         ;sinon c'est un des fils
        (loop for x in (cadr (car list-parent-fils)) do
              (if (equal x elt)
                  (progn
                  (setq res (car(car list-parent-fils)))
                  (return)
                  )))
         ;sinon un des descendant des fils   
        (print "second phase")
        (if (not res)
              (loop for i from 1 to (length list-parent-fils) do
                      (loop for x in (cadr(car(nthcdr i list-parent-fils ))) do
                          (print x)
                             (if (equal x elt)
                                 (progn
                                   (setq res (car (car (nthcdr i list-parent-fils))))
                                   (return))
                  ))))                      
res)

(defun list-tree-lambda-to-task-empty(tree)
  (setf (car (car  tree)) (make-task-with-cpt :lambda-f (data tree) :cpt 0))
 (if  (first-child tree)  (list-tree-lambda-to-task-empty (first-child tree)))
 (if (next-sibling tree) (list-tree-lambda-to-task-empty (next-sibling tree)))
)

(defun update-all (list-p-f)
  (loop for couple in list-p-f
        do
        (let ((parent (car couple)))
          (if (cadr couple)
          (progn    
            (setf (task-with-cpt-cpt parent) (length (cadr couple))))
            (loop for elmt in (cadr couple)
                do
                (let ((fun (task-with-cpt-lambda-f elmt)))
                  (setf (task-with-cpt-lambda-f elmt) #'(lambda ()
                                                        (funcall fun) (print fun)                                                       
                                                        (mydecf parent)
))))))))

(defun list-tree-lambda-to-task-to-engine(tree)
(list-tree-lambda-to-task-empty tree)
(update-all (get-list-parent-fils tree))
 ( distrib-flatten-to-polls(flat tree)))

(defun distrib-flatten-to-polls(list-flat)
  (loop for elmt in list-flat do
        (if (> (task-with-cpt-cpt elmt) 0)
             (push (pending-taskqueue *engine*) (task-with-cpt-lambda-f elmt) )
             (mp:mailbox-send (taskqueue *engine*) (task-with-cpt-lambda-f elmt))))
)

(defun mydecf (task)
  (decf (task-with-cpt-cpt task))
  (if (= (task-with-cpt-cpt task) 0)
      (mp:mailbox-send (taskqueue *engine*) (task-with-cpt-lambda-f task)))) ;(mailbox-read ?????))
            

(defun list-parent-fils-filtre-without-nil (list-parent-fils)
  (let ((cpt 0)(nb-retrieve 0))
  (loop for x in list-parent-fils do
        (PRINC  (cdr x) )
        (if (equal (cdr x) '(nil))
               (PROGN (PRINT "COUCOU")
               (setq list-parent-fils (remove-nth (- cpt nb-retrieve) list-parent-fils))
          (setq nb-retrieve (+ nb-retrieve 1))))
        (setq cpt (+ 1 cpt)))
list-parent-fils))


(defun minimum-child-level (tree &optional min-so-far)
  (cond ((endp tree)         ; end of child list
         (if min-so-far
             (1+ min-so-far) ; there were children
             0))             ; no children
        ((atom (first tree)) ; ignore symbols, go to next child
         (minimum-child-level (rest tree) min-so-far))
        (t                   ; sublist
         ;; recurse into child
         (let ((child-minlevel (minimum-child-level (first tree))))
           ;; go to next child
           (minimum-child-level (rest tree)
                                (if min-so-far
                                    (min min-so-far child-minlevel)
                                    child-minlevel))))))

(defun sum-bros(tree)
  (let ((sum (get-cpt tree))(tree-b tree))
    (loop while (next-sibling tree-b) do
           (setq sum (+ sum (get-cpt(next-sibling tree))))
           (setq tree-b (next-sibling tree-b)))
    sum))


(defun remove-from-tree(el tree)
  (mapc (lambda(subtree)
            (cond ((null subtree) (list nil))
                  ((consp subtree) (list (remove-from-tree el subtree)))
                  ((eql subtree el) nil)
                  (t (list subtree))))
          subtree))


(defun parcour(tree &optional min-so-far) 
  (if (equal (first-child tree) nil)
      (print "pas de fils")
      (if (eq (sum-bros (first-child tree)) 0)
          (if (check-into-engine task);recuperer la task courante de l'arbre;
             (progn  (print "still in the pool continu parcour")  (parcour (first-child tree)))
            (progn (print "remove childs")
              (remove-childs tree )))
        ))
  (if (equal (next-sibling tree) nil)
      (print "pas de frere")
    (parcour (next-sibling tree)))
  (if (eq (get-cpt tree) 0)
      (print "you can do the task"))) ;;; a concervé?

(defun check-into-engine(task)
  (let ((lambda-f (task-with-cpt-lambda-f task )) (bool))
    (loop for lambda-pool in  (pending-taskqueue *engine*) do
          (if (equal lambda-f  lambda-pool)
              (progn (setq bool t) (return))
            (setq bool nil)))
    ))

(defun distrib-task-dep-to-polls-when-arrive(tree)
;parcour l'arbre de task et les mets dans la pool de task pendante.
  (if (>  (get-cpt tree) 0)
      (mp:mailbox-send (pending-taskqueue *engine*) (get-lambda tree))
    (mp:mailbox-send (taskqueue *engine*) (get-lambda tree)))
  (if (equal (first-child tree) nil)
      (print "pas de fils")
    (distrib-task-dep-to-polls-when-arrive (first-child tree)))
  (if (equal (next-sibling tree) nil)
      (print "pas de frere")
    (distrib-task-dep-to-polls-when-arrive  (next-sibling tree)))          
  )

(mp:mailbox-count (pending-taskqueue *engine*)) ; to pop/erase at each distrib when zero...




