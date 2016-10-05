

(in-package :om)


;;;qqch dans le genre
(setf work1 (make-task-with-cpt :lambda-f (lambda () (print(* 1 2)) ) :cpt 0));(decf ((task-with-cpt-cpt (car(parent(first-bro (next-sibling(next-sibling(first-child tree)))tree) tree))))  ))) :cpt 0))
(setf work2 (make-task-with-cpt :lambda-f (lambda () (print(* 2 2))) :cpt 0)) ;(decf ((task-with-cpt-cpt (car(parent(first-bro (next-sibling(next-sibling(first-child tree)))tree) tree))))))) :cpt 0))
(setf work3 (make-task-with-cpt :lambda-f (lambda () (print(* 1 2))) :cpt 2))
(type-of work1)

(setf tree (let ((one (make-tree work3)))
  (add-child one (make-tree work1))
  (add-child one (make-tree work2))
))

(setf work1a (make-task-with-cpt :lambda-f (lambda () ((* 1 2) (decf ((task-with-cpt-cpt (car(parent(first-bro (next-sibling(next-sibling(first-child tree)))tree) tree))))  ))) :cpt 0))
(setf work2a (make-task-with-cpt :lambda-f (lambda () ((* 2 2) (decf ((task-with-cpt-cpt (car(parent(first-bro (next-sibling(next-sibling(first-child tree)))tree) tree))))))) :cpt 0))
(setf work3a (make-task-with-cpt :lambda-f (lambda () (* 1 2)) :cpt 2))

(setf work4a)
(setf work5a)
(setf work6a)


(setf tree (let ((one (make-tree work3a)))
  (add-child one (make-tree work1a))
   (add-child one (make-tree work2a))
   (let ((two (next-sibling(first-child one)))
     (add-child two (make-tree work2))
))

(setf tree4 (let ((one (make-tree "A")))
      (add-child one (make-tree "B"))
      (add-child one (make-tree "C"))
      (add-child one (make-tree "D"))
      (let ((two (next-sibling (first-child one))))
        (add-child two (make-tree "E"))
        (add-child two (make-tree "F")))
one))

tree4

(print tree)
(traverse tree)
(type-of tree)
(type-of (data tree))
(task-with-cpt-cpt (data tree))
(first-child tree)
(parent (first-child tree) tree)
(task-with-cpt-lambda-f work1)

((#S(|OpenMusic|::TASK-WITH-CPT :LAMBDA-F #<anonymous interpreted function 4160018F9C> :CPT 2) (#S(|OpenMusic|::TASK-WITH-CPT :LAMBDA-F #<anonymous interpreted function 41600167EC> :CPT 0)) (#S(|OpenMusic|::TASK-WITH-CPT :LAMBDA-F #<anonymous interpreted function 4160018EA4> :CPT 0))))

((1 (2) (3) (4) (5)))
(member '(2 3) '(1 (2 3)) :key #'cdr :test #'equal)
(minimum-child-level tree)
(next-sibling (first-child tree))
(car(car tree))
(cdr tree)

(parent (first-child tree)tree)
(task-with-cpt-cpt (car(parent(first-bro (next-sibling(next-sibling(first-child tree)))tree) tree)))

(parcour tree)
(parcour tree2)

(traverse tree)

;(rplaca tree (append (car tree) child))
(rplaca tree (append (car tree) (first-child tree)))
(first-bro (cdr(first-child tree)) tree)

(task-with-cpt-lambda-f(data tree))
task-with-cpt-cpt

(setq tree2 (let ((one (make-tree 1)))
  (add-child one (make-tree 2))
  (add-child one (make-tree 3))
  (add-child one (make-tree 4))
  (add-child one (make-tree 5))
  (let ((two (first-child one)))
    (add-child two (make-tree 6))
    (add-child two (make-tree 7))
    (add-child two (make-tree 8)))
  (let ((four (next-sibling (next-sibling (first-child one)))))
    (add-child four (add-child (make-tree 9) (make-tree 12)))
    (let ((five (next-sibling four)))
      (add-child five (make-tree 10))
      (add-child five (make-tree 11))))
  one))

(setq tree3 (let ((one (make-tree (lambda () (print "FIRST")))))
              (add-child one (make-tree (lambda ()(print "Second"))))
              (add-child one (make-tree (lambda ()(print "Three"))))
              (add-child one (make-tree (lambda ()(print "four"))))
              (add-child one (make-tree (lambda ()(print "Five"))))
              (let ((two (first-child one)))
                (add-child two (make-tree (lambda ()(print "Six"))))
                (add-child two (make-tree (lambda ()(print "Seven"))))
                (add-child two (make-tree (lambda ()(print "Eight")))))
              (let ((four (next-sibling (next-sibling (first-child one)))))
                (add-child four (add-child (make-tree (lambda ()(print "Nine"))) (make-tree (lambda ()(print "TWelve")))))
                (let ((five (next-sibling four)))
                  (add-child five (make-tree (lambda ()(print "Ten"))))
                  (add-child five (make-tree (lambda ()(print "Eleven"))))))
              one))

(traverse tree3)
(traverse tree2)

(setq tree (let ((one (make-tree 1)))
  (add-child one (make-tree 2))
  (add-child one (make-tree 3))
  (add-child one (make-tree 4))
  (add-child one (make-tree 5))
))

(traverse tree)

;(rplaca tree (append (car tree) child))
(rplaca tree (append (car tree) (first-child tree)))
(first-bro (cdr(first-child tree)) tree)

(minimun-child-level (tree &optional min-so-far)

(let ((task (make-task-with-cpt :lambda-f (lambda (x) (* x x)) :cpt 0)))
(task-with-cpt-lambda-f task ))

(remove-from-tree(first-child tree) tree)


( remove-childs tree)

(sum-bros (first-child tree))

(delete-duplicates tree :key #'first-child :test #'equal)


;------------------------------------test 2---------
 (distrib-task-dep-to-polls-when-arrive tree)




;TTTTEEEEESSSSTTTTTT
 tree3
   (get-list-parent-fils tree3 '())
   (list-tree-lambda-to-task tree3)
   (update-all (get-list-parent-fils tree3 '()))
( distrib-flatten-to-polls(flat tree3))




(let ((l (sort (flat tree3) '< :key 'task-with-cpt-cpt)))
  (loop for task in l
        do
        (print task)
        (if (= (task-with-cpt-cpt task) 0)
            (funcall (task-with-cpt-lambda-f task))
          (print "ERROR"))
        (sleep 0.5)))

(loop for tsk in (flat tree3) do (funcall tsk))

(progn
  (setq tree3 (let ((one (make-tree (lambda () (print "FIRST")))))
              (add-child one (make-tree (lambda ()(print "Second"))))
              (add-child one (make-tree (lambda ()(print "Three"))))
              (add-child one (make-tree (lambda ()(print "four"))))
              (add-child one (make-tree (lambda ()(print "Five"))))
              (let ((two (first-child one)))
                (add-child two (make-tree (lambda ()(print "Six"))))
                (add-child two (make-tree (lambda ()(print "Seven"))))
                (add-child two (make-tree (lambda ()(print "Eight")))))
              (let ((four (next-sibling (next-sibling (first-child one)))))
                (add-child four (add-child (make-tree (lambda ()(print "Nine"))) (make-tree (lambda ()(print "TWelve")))))
                (let ((five (next-sibling four)))
                  (add-child five (make-tree (lambda ()(print "Ten"))))
                  (add-child five (make-tree (lambda ()(print "Eleven"))))))
              one))
  (list-tree-lambda-to-task tree3)
(update-all (get-list-parent-fils tree3 )))

(list-tree-lambda-to-task-to-engine tree3)


  ;(update-all (get-list-parent-fils tree3 '())))