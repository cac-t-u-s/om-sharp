(in-package :om)

(setq a (make-dag-task-dependancy))

 

(;pere fils petitfils...)
(setq lambdaLst (list  (lambda () (print "FIRST"))  (list(lambda () (print "TWO"))  (lambda () (print "THREE"))  (lambda () (print "FOUR"))) (list  (lambda () (print "FIVE"))))
)
(car lambdaLst)
(loop for elmt in lambdaLst do
      (if (atom elmt)
          (setf (gethash (pop lambdaLst)  (dag-task-dependancy-hash-dep a)) (car lambdaLst))
           (progn (loop for elt2 in elmt do
                (setf (gethash (pop elmt)  (dag-task-dependancy-hash-dep a)) (cadr lambdaLst))
                )
             (pop lambdaLst)))
      
)

(setf (gethash key  (hash-dep a)) elt))

(to-dag-task-dependancy (dag-task-dependancy-hash-dep a)) 

 (loop for value being the hash-key of (dag-task-dependancy-hash-dep a) do
       (print "ok")
 (print value)
(print "sons")
(print  (gethash value  (dag-task-dependancy-hash-dep a))))




(setq *my-hash-dag* (dag (dag-task-dependancy-hash-dep a)))

 (loop for value being the hash-key of *hashtab* do
       (print "ok")
 (print value)
;(print "sons")
;(print  (gethash value  *hashtab*))
)

;--------------------NEW SCHOOL, C'est ici-----------------
;;;struct hashtabledel'example 1-3-1 (flux donné 1->3->1)...
; le flux a plus marche pas bien, on a fait la hashmap fils pere, voir des algorithme en fonction, le compteur....
(progn
  (let ((lambda1 (lambda ()(print "un"))) (lambda2 (lambda ()(print "deux"))) (lambda3(lambda ()(print "trois"))) (lambda4(lambda ()(print "quatre"))) (lambda5(lambda ()(print "cinq"))) (lambda6(lambda ()(print "six"))))
  (setq *hashtab* (make-hash-table))
  (setf (gethash lambda1 *hashtab*) (list lambda2 lambda3 lambda4 lambda5))
   (setf (gethash lambda2 *hashtab*) (list lambda6))
   (setf (gethash lambda3 *hashtab*) (list lambda6))
   (setf (gethash lambda5 *hashtab*) (list lambda6))
   (setf (gethash lambda4 *hashtab*) (list lambda6))
   (setf (gethash lambda6 *hashtab*) nil)
)*hashtab*)



    




;-----------------------reinitialise le scheduler et exec---------
(progn (om::abort-om-player) (om::init-om-player))
 
(peigne-nb-task-dependancy  13)
(setq *list-task-pere-fils* (update-hashtable-lambda *hashtab*))
(length *list-task-pere-fils*)

(pending-taskqueue *engine*)
(distrib-flatten-to-polls (flat (loop for task-pere in *list-task-pere-fils* collect (car task-pere))))

;----------------subtest ------

(progn
(let ((lambdas (loop for i from 0 to 5  collect
                     (let ((x i))
                     (lambda() (print x)))))
      (fils-zero nil))
(loop for x in lambdas do (funcall x))))


 (loop for i from 0 to 10 collect
                    (funcall (lambda() (print i))))

(nth 5(funcall(car(cadr (loop for key in (hash-keys *hashtab*)
       collect
       (print key)))))

(loop for eltm  in(loop for key in (hash-keys *hashtab*)
       collect
       (print key))
do (funcall eltm))


;;;;;BON en gros, faut avoir qu'une seul fois chaque fonction, il y a des doublons et des nil qui traine...w
(distrib-flatten-to-polls (flat (loop for task-pere in *list-task-pere-fils* collect (car task-pere))))




(
(#2=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4390008B1C> :cpt 1 :peres (#1=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4390008B7C> :cpt 4 :peres (#5=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4390008BDC> :cpt 1 :peres (#1#)) #4=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4390008BAC> :cpt 1 :peres (#1#)) #3=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4390008B4C> :cpt 1 :peres (#1#)) #2#)))) (#1#))
 (#3# (#1#))
 (#1# (#4# #5# #2# #3#))
 (#4# (#1#))
 (#S(|OpenMusic|::task-with-cpt :lambda-f (#<anonymous interpreted function 4390008C0C>) :cpt 0 :peres nil) nil) (#5# (#1#)))

(
(#6=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4060010EEC> :cpt 0 :peres (#5=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4060010EBC> :cpt 1 :peres (#1=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4060010DFC> :cpt 4 :peres nil))) #4=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4060010E8C> :cpt 1 :peres (#1#)) #3=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4060010E5C> :cpt 1 :peres (#1#)) #2=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4060010E2C> :cpt 1 :peres (#1#)))) nil) 
(#1# (#2# #3# #4# #5#)) 
(#2# (#6#)) 
(#3# (#6#)) 
(#4# (#6#))
 (#5# (#6#)))

;;la liste n'est pas ordonné


; (to-list-couple *hashtab*)
(setf couple-empty ( to-list-couple-task *hashtab*))
(setf couple  (to-list-couple-task-lambda *hashtab*))
to-list-couple-task-lambda

(update-all-dag couple)
 (list-couple-to-empty-task  (to-list-couple *hashtab*) )

list-couple-to-empty-task
(defparameter *my-hash* (make-hash-table))
(setf (gethash 'first-key *my-hash*) 'one)

(progn
  
  (let* ((lambda1 (lambda()((print 1)))) (lambda2 (lambda()((print 2))))(lambda3 (lambda()(print "3")))
        (task (make-task-with-cpt :lambda-f lambda1 )) (task2 (make-task-with-cpt :lambda-f lambda2 ))
        (listlambda (list lambda1 lambda2 lambda3)))
       (if (eq lambda1 (task-with-cpt-lambda-f task))
           (PROGN
             (PRINT "HELO")
             (print lambda1)
       (setq listlambda (substitute task lambda1  listlambda  ))
(substitute task2 lambda2 listlambda  )))));:test #'(lambda (x y)(equal (print x) (print y))) )))))
 

  (substitute 1 2 '(2 2 3) )


 (let* ((lambda1 (lambda()((print 1)))) (lambda2 (lambda()((print 2))))(lambda3 (lambda()(print "3"))))
   (eq lambda1 lambda1))


(
(#2=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 40700008F4> :cpt 1 :peres (#1=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4070000894> :cpt 3 :peres nil))) (#4=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4070000954> :cpt 0 :peres (#5=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 40700008C4> :cpt 1 :peres (#1#)) #3=#S(|OpenMusic|::task-with-cpt :lambda-f #<anonymous interpreted function 4070000924> :cpt 1 :peres (#1#)) #2#)))) 
(#3# (#4#)) 
(#4# nil) 
(#1# (#5# #2# #3#)) 
(#5# (#4#)))