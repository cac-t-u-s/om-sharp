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

;cette api permet la représentation de tâche dépendante avec une structure dag à compteur.
;une fonction permet de construire des ``peignes de dépendance''.

(defstruct task-with-cpt
  (lambda-f nil)
  (cpt 0 :type integer)
  (peres (list))
  )



(defstruct dag-task-dependancy
 (hash-dep (make-hash-table)))



(defun to-list-couple-task-lambda(hash-lambda)
  (let ((Lcouple
         (loop  for key being the hash-key of hash-lambda collect
                (list (make-task-with-cpt :lambda-f key :cpt (length(gethash key hash-lambda)))
                      (loop for elt in (gethash key hash-lambda) do
                            collect
                            elt)))))
    (setf fathers (loop for elt in Lcouple collect (car elt)))

    (loop for elt in Lcouple do
          (if (cadr elt)
              (loop for elt-pere in fathers do
                    (loop for elt-fils in (cadr elt) do
                          (if (equal (task-with-cpt-lambda-f elt-pere) elt-fils)
                              (setf (cadr elt) (substitute elt-pere elt-fils  (cadr elt) )))))))
    Lcouple))


(defun update-all-dag (list-p-f)
  (loop for couple in list-p-f
        do
        (let ((parent (car couple))(Lfils (cadr couple)))
          (if  Lfils
            (loop for elmt in Lfils
                do                
                  (push parent  (task-with-cpt-peres elmt)  )
))))
list-p-f)

(defun update-hashtable-lambda (hash-lambda)
  (let ((res-task nil)(couple nil))
    (setf couple  (to-list-couple-task-lambda *hashtab*))
    (setf res-task (update-all-dag couple))
    res-task)
  )

(defun distrib-flatten-to-polls (list-flat)
  (loop for elmt in list-flat do    
        (if (> (task-with-cpt-cpt elmt) 0)
            (push  elmt (pending-taskqueue *engine*))
              (exec-task elmt)))
)


;hum
(defun exec-task(task)
  (mp:mailbox-send (taskqueue *engine*) (lambda ()
                                          (funcall (task-with-cpt-lambda-f task))
                                          (loop for pere in (task-with-cpt-peres task) do (mydecf pere)))))

(defun mydecf (task)
  (decf (task-with-cpt-cpt task))
  (if (= (task-with-cpt-cpt task) 0)
      (progn                  
        (exec-task task)
        (setf (pending-taskqueue *engine*) (remove task (pending-taskqueue *engine*))))))

(defun peigne-nb-task-dependancy(nb)
(let ((lambdas (loop for i from 0 to nb  collect
                     (eval `(function (lambda () (print ,i))))))        
      (fils-zero nil))
   (setq *hashtab* (make-hash-table))
   (setq filszero (loop for k from 1 to (- nb 1) collect (nth k lambdas)))
   (setf (gethash (nth 0 lambdas) *hashtab*) filszero)
   (loop for j from 1 to (- nb 1) do
         (setf (gethash (nth j lambdas) *hashtab*) (last lambdas)))

   (setf (gethash (car(last lambdas)) *hashtab*)  nil)
  lambdas))

