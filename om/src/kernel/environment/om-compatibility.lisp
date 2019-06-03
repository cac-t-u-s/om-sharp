;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2019 J. Bresson et al., IRCAM.
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


(in-package :om)

(defvar *om-current-persistent*)

(defun load-om6-patch (path)
  
  (om-print-format "Opening document : ~A" (list path) "Compatibility")

  (clos::set-clos-initarg-checking nil) ;; in case some class initargs do not exist anymore...
  
  (unwind-protect 
  
  (with-relative-ref-path path
    
    (with-open-file (in path :direction :input :if-does-not-exist nil)
      
      (unless (equal (read-line in nil :eof) :eof) ;; not empty
        
        ;; line #2 contains meta-data about the patch
        (let ((metadata (read-from-string (subseq (om-read-line in) 1))))
      
          (if metadata 
              (om-print-format "Converting from OM ~D (type '~A')" (list (car metadata) (cadr metadata)) "Compatibility")
            (om-print-format "No header found in document..." nil "Compatibility"))
          
            ;(handler-case  
               (load path)
            ;(error (err) 
            ;  (om-print err "Compatibility")
            ;  (abort)
            ;;;(error err)
            ;  )
            ;)
          
          (when *om-current-persistent* ;;; filled in the patch-loading process

            (let ((object (make-instance 'OMPatchFile :name (pathname-name path))))
              
              (copy-contents *om-current-persistent* object)
              
              (setf (omversion object) *om-version*
                    (create-info object) (list (om-get-date) (om-get-date))
                    (window-size object) (eval (nth 4 metadata))
                    (saved? object) nil)
              
              (register-document object)
            
              object)
            ))
        
          )))
    
    (clos::set-clos-initarg-checking t)
    ))


;============================================================================
; CONVERT ALL OM6 FUNCTIONS TO OM7 CALLS...
;============================================================================

;======================================
; REQUIRED LIBS
;======================================
;;; called to lod libs prior to the patch
(defun load-lib-for (list)
  (loop for libname in list do (require-library libname)))


;======================================
; PATCH (MAIN SECTION)
;======================================

;;; idea: consider an auto-alignment option for patches, set "on" when an old patch is loaded...

;;; main patch file
(defun om-load-patch1 (name boxes connections &rest ignore)
  
  (declare (ignore ignore))
  
  ;(let ((patch (make-instance 'OMPatchFile :name name)))
  ; (loop for box-code in boxes do
  ;   (let ((box (eval box-code)))
  ;     (when box (omng-add-element patch box))))
  ;  patch)
   
  ;;; only top-level patch acually load their contents:
  (omng-load
   `(:patch
     (:name ,name)
     (:boxes .,(loop for box-code in boxes collect (eval box-code)))
     (:connections .,(loop for c in connections collect (format-imported-connection c)))
     )
   )
  )
    

;;; internal sub-patch
(defun om-load-patch-abs1 (name boxes connections &rest ignore)
  (declare (ignore ignore))
  
  ;(let ((patch (make-instance 'OMPatchInternal :name name)))
  ;  (loop for box-code in boxes do
  ;        (let ((box (eval box-code)))
  ;          (when box 
  ;            (omng-add-element patch box))))      
  ;  patch)
  `(:patch
    (:name ,name) 
    (:boxes .,(loop for box-code in boxes collect (eval box-code)))
    (:connections .,(loop for c in connections collect (format-imported-connection c)))
    )
  )


;======================================
; PATCH BOX
;======================================
;;; textfun will also call this one. 
;;; they load ok anyway.
(defmethod om-load-boxcall ((self (eql 'abstraction)) name reference inputs position size value lock &rest rest)

  (declare (ignore name value rest))
  
  `(:box 
    (:type :patch)
    (:reference ,reference)   ;; contains the actual patch
    (:name ,name)
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:h ,(if size (om-point-y size) 48))
    (:lock ,(if lock (cond ((string-equal lock "x") :locked) 
                           ((string-equal lock "&") :eval-once))))
    (:lambda ,(if lock (cond ((string-equal lock "l") :lambda) 
                             ((string-equal lock "o") :reference))))
    (:inputs .,inputs)
    (:display :mini-view)
    )
  )


(defun om-load-lisp-abspatch (name version expression)
  `(:textfun
    (:om-version ,version)
    (:name ,name)
    (:text
     (:list .,(string-to-list expression "$")))
    ))


;======================================
; BOXES (GENERAL)
;======================================
;;; a box input
(defun om-load-inputfun (class doc name value) 
  (declare (ignore class doc))
  `(:input (:type :standard) (:name ,name) (:value ,(omng-save value))))

;;; a box input "keyword" 
(defun om-load-inputkeyword (class doc name value defval menu) 
  (declare (ignore class doc value menu))
  `(:input (:type :key) (:name ,name) (:value ,(omng-save defval))))

;;; a box input with menu for different values
(defun om-load-inputfunmenu1 (class doc name value items) 
  (declare (ignore class doc items))
  `(:input (:type :standard) (:name ,name) (:value ,(omng-save value))))


;;; handle dispatch to 'lispfun or handle unknown box 
(defmethod om-load-boxcall ((class t) name reference inputs position size value lock &rest rest)
  (cond ((fboundp reference)
         (om-load-boxcall 'lispfun name reference inputs position size value lock))
        (t 
         ;; (om-print-format "Unknown function for box of type ~A: '~A'" (list class reference) "Compatibility")
         ;; => will be signaled in om-load-from-id
         `(:box 
           (:type :function)
           (:reference ,reference)
           (:x ,(om-point-x position))
           (:y ,(om-point-y position))
           (:w ,(and size (om-point-x size)))
           (:inputs .,(mapcar #'eval inputs))
           ))
        ))
  

;;; redefine with eql-specializers for specific functions of class name
(defmethod changed-arg-names (function) nil)

(defun check-arg-for-new-name (reference name)
  (or (cadr (find name (changed-arg-names reference) :key #'car :test #'string-equal))
      (and (string-equal name "add-input") (fboundp reference)
           (getf (function-arglist reference) '&rest)) ;;; the last element in lambda-list is the name of the &rest arg
      name))
  

;======================================
; FUNCTION BOXES
;======================================

(defmethod om-load-boxcall ((self (eql 'lispfun)) name reference inputs position size value lock &rest rest) 

  (let ((new-inputs 
         (loop for formatted-in in (mapcar #'eval inputs) collect
               ;;; correct the type and eventually the name of box inputs
               (let ((name (check-arg-for-new-name 
                              reference 
                              (find-value-in-kv-list (cdr formatted-in) :name))))
                 (cond ((not (fboundp reference)) formatted-in)
                       ((find name (mapcar #'symbol-name (function-main-args reference)) :test #'string-equal)
                        `(:input (:type :standard) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                       ((find name (mapcar #'symbol-name (function-optional-args reference)) :test #'string-equal)
                        `(:input (:type :optional) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                       ((find name (mapcar #'symbol-name (function-keyword-args reference)) :test #'string-equal)
                        `(:input (:type :key) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                       ((and (find '&rest (function-arglist reference))
                             (equal name (getf (function-arglist reference) '&rest)))
                        `(:input (:type :optional) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                       (t (om-print-format "Unknown input for function '~A': ~A" (list reference name) "Compatibility")
                          formatted-in))
                 ))))

    `(:box 
      (:type :function)
      (:reference ,reference)
      (:x ,(om-point-x position))
      (:y ,(om-point-y position))
      ;;; importing the size of function boxes might not be necessary...
      (:w ,(and size (om-point-x size)))
      ;(:h ,(and size (om-point-y size)))
      (:lock ,(if lock (cond ((string-equal lock "x") :locked) 
                             ((string-equal lock "&") :eval-once))))
      (:lambda ,(if lock (cond ((string-equal lock "l") :lambda) 
                               ((string-equal lock "o") :reference))))
      (:inputs .,new-inputs)
      )   
    ))


;;; super filou
(defmethod (setf numouts) (n (box-data list)) 
  (setf (cdr (last box-data))
        `((:outputs
           ,.(loop for i from 1 to n collect 
                   '(:output (:name "out"))))
          ))
  box-data)


(defmethod update-reference ((self t)) nil)

;======================================
; OBJECT BOXES
;======================================

(defun om-load-editor-box1 (name reference inputs position size value lock 
                                 &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore fname editparams meditor pictlist))

  (let ((inputs (loop for formatted-in in (mapcar #'eval inputs) collect
                      ;;; correct the type and eventually the name of box inputs
                      (let ((name (check-arg-for-new-name 
                                   reference 
                                   (find-value-in-kv-list (cdr formatted-in) :name))))

                        (cond ((and (find-class reference nil)
                                    (find name (mapcar #'symbol-name (additional-class-attributes (make-instance reference)))
                                          :test #'string-equal))  ;;; if the input has become a keywork input
                               `(:input (:type :key) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                              (t formatted-in))
                        ))))
    
    ;;; when a class has changed into something else...
    (when (update-reference reference) 
      (setf reference (update-reference reference))
      (setf value (update-value value)))
    
    `(:box
       (:type :object)
       (:reference ,reference)
       (:name ,name)
       (:value ,value)
       (:x ,(om-point-x position))
       (:y ,(om-point-y position))
       (:w ,(om-point-x size))
       (:h ,(om-point-y size))
       (:lock ,(if lock (cond ((string-equal lock "x") :locked)
                              ((string-equal lock "&") :eval-once))))
       (:showname ,show-name)
       (:display ,(if spict :mini-view :hidden))
       (:inputs .,inputs)
       )
    ))
    
;======================================
; 'SLOTS' BOXES
;======================================

(defmethod om-load-boxcall ((self (eql 'slot)) name reference inputs position size value lock &rest rest)
  
  (declare (ignore value rest))
  
  `(:box 
    (:type :slots)
    (:reference ,reference) 
    (:name ,(concatenate 'string (string-upcase reference) " SLOTS"))
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:h ,(and size (om-point-y size)))
    (:lock ,(if lock (cond ((string-equal lock "x") :locked)
                           ((string-equal lock "&") :eval-once))))
    (:inputs .,inputs))
  )



;======================================
; SIMPLE VALUE BOXES
;======================================

(defmethod om-load-boxcall ((self (eql 'bastype)) name reference inputs position size value lock &rest rest)
  `(:box 
     (:type :value)
     (:reference ,reference) 
     (:value ,(omng-save value))
     (:name ,name)
     (:x ,(om-point-x position))
     (:y ,(om-point-y position))
     (:w ,(om-point-x size))
     (:h ,(om-point-y size)))
  )


;======================================
; INSTANCE BOXES
;======================================

(defclass ominstance () 
  ((instance :accessor instance :initform nil)
   (edition-params :accessor edition-params :initform nil)
   (create-info :accessor create-info :initform nil)
   (doc :accessor doc :initform nil)))

(defclass omlistinstance (ominstance) ())

(defun om-load-boxinstance (name instance inputs position &optional fname size) 
  
  (declare (ignore inputs fname))
  
  (let* ((value (instance instance))
         (type (type-of value)))
    
    (cond ((listp value)
           ;;; lists => simple value boxes
           `(:box 
             (:type :value)
             (:reference list) 
             (:value ,(omng-save value))
             (:name ,name)
             (:lock :locked)
             (:x ,(om-point-x position))
             (:y ,(om-point-y position))
             (:w ,(om-point-x size))
             (:h ,(om-point-y size)))
           )
           
          (t ;;; other sort of objects => object box
             (when (update-reference type) 
               (setf type (update-reference type))
               (setf value (update-value value)))
             
             `(:box
               (:type :object)
               (:reference ,type)
               (:name ,name)
               (:value ,value)
               (:x ,(om-point-x position))
               (:y ,(om-point-y position))
               (:lock :locked)
               (:showname t)
               (:display :mini-view :hidden)
               )
             ))
    )
  )

;============================================================================
; CONNECTIONS
;============================================================================

(defun n-to-color (n)
  (case n 
    (1 (om-def-color :blue))
    (2 (om-def-color :red))
    (3 (om-def-color :green))
    (4 (om-def-color :orange))
    (5 (om-def-color :pink))
    (6 (om-def-color :dark-red))
    (7 (om-def-color :dark-blue))
    (8 (om-def-color :yellow))
    (9 (om-def-color :black))
    (10 (om-def-color :gray))
    (11 (om-def-color :purple))
    (12 (om-def-color :light-blue))
    (13 (om-def-color :brown))
    (14 (om-def-color :green2))
    (15 (om-def-color :salmon))
    (16 (om-def-color :steelblue))))

(defun format-imported-connection (c)
  (append 
   `(:connection 
     (:from (:box ,(nth 0 c) :out ,(nth 1 c)))
     (:to (:box ,(nth 2 c) :in ,(nth 3 c))))
   (when (nth 5 c)
     `((:attributes 
        (:color ,(n-to-color (nth 5 c)))))
     )
   ))
     

;============================================================================
; COMMENTS
;============================================================================

(defun str-with-nl (str)
  (map 'string  #'(lambda (x) (if (equal x #\$) #\Newline x)) str))
  
(defun str-without-nl (str)
  (map 'string #'(lambda (x) (if (equal x #\Newline) #\$ x)) str))

(defun om-load-boxcomment (name size reference value position fname color style) 
  (declare (ignore name value fname))
  
  `(:comment
     (:text ,(str-with-nl reference))
     (:x ,(om-point-x position))
     (:y ,(om-point-y position))
     (:w ,(om-point-x size))
     (:h ,(om-point-y size))
     (:text-font ,style)
     (:fgcolor ,color)
     (:border 0)
     )
  )

;============================================================================
; OTHER BOXES
;============================================================================

;;; IN BOX
(defun om-load-boxin (name indice position docu &optional fname val fsize) 
  
  (declare (ignore fname fsize))

  `(:box
    (:type :io)
    (:reference (:in (:type omin) (:index ,indice) (:name ,name) (:doc ,docu)))
    (:name ,name)
    (:value ,(omng-save val))
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:outputs (:output (:name "out")))
    ))

;;; OUT BOX
(defun om-load-boxout (name indice position inputs &optional fname fsize) 

  (declare (ignore name fsize))

  `(:box
    (:type :io)
    (:reference (:out (:type omout) (:name ,fname) (:index ,indice)))
    (:name ,fname)
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:inputs .,(mapcar #'eval inputs))
    ))


;;; REPEAT-N BOX
(defmethod om-load-boxcall ((class t) name (reference (eql 'repeat-n)) inputs position size value lock &rest rest)
  
  (declare (ignore class name size value rest))

  (when (string-equal lock "l")
    (om-print 
     "REPEAT-N boxes can not be use in 'lambda' mode. In order to restore the patch, encapsulate it in a lambda-patch !"  
     "Compatibility"))
  
  `(:box 
    (:type :special)
    (:reference repeat-n)
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:inputs .,(mapcar #'eval inputs))
    (:lock ,(when lock (cond ((string-equal lock "x") :locked)
                             ((string-equal lock "&") :eval-once))))
    ))


;;; SEQUENCE BOX
(defmethod om-load-seqbox (name reference inputs position sizeload value lock numouts) 

  (declare (ignore name reference value numouts))

  `(:box 
    (:type :special)
    (:reference sequence)
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and sizeload (om-point-x sizeload)))
    (:inputs .,(cons (eval (car inputs))
                     (mapcar #'(lambda (i) 
                                 (declare (ignore i))
                                 '(:input (:type :optional) (:name "op+")))
                             (cdr inputs))))
    (:lock ,(when lock (cond ((string-equal lock "x") :locked)
                             ((string-equal lock "&") :eval-once))))
    )
  )

;;; these are useless and should just disappear
(defmethod om-load-boxcall ((self (eql 'undefined)) name reference inputs position size value lock &rest trest) nil)



;======================================
; OMLOOP
;======================================

;;; This was used to save OMLoops
;;; In om7 OMloop is just an normal patch

(defmethod om-load-boxwithed1  ((class t) name reference inputs position size value lock boxes connec numouts &optional fname pictlist) 
  `(:box 
    (:type :patch)
    (:name ,name)

    (:reference 
     (:loop-patch ;;; special reference to make adaptations -- see just below
      (:name ,name) 
      (:boxes .,boxes)
      (:connections .,(loop for c in connec collect (format-imported-connection c)))))
    
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:h ,(if size (om-point-y size) 48))
    (:lock ,(if lock (cond ((string-equal lock "x") :locked) 
                           ((string-equal lock "&") :eval-once))))
    (:lambda ,(if lock (cond ((string-equal lock "l") :lambda) 
                             ((string-equal lock "o") :reference))))
    (:inputs .,inputs)
    (:display :mini-view)
    ))


(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'listloop)) inputs position size value lock &rest rest)
  `(:box
    (:type :special)
    (:reference loop-list)
    (:name "list-loop")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,(remove nil 
                       (list
                        `(:input 
                          (:type :standard) 
                          (:name "list")
                          (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
                        (when (second inputs)
                          `(:input 
                            (:type :optional) 
                            (:name "by")
                            (:value ,(find-value-in-kv-list (cdr (eval (second inputs))) :value)))
                          )))
     )))


(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'onlistloop)) inputs position size value lock &rest rest)
  `(:box
    (:type :special)
    (:reference loop-tail)
    (:name "tail-loop")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,(remove nil 
                       (list
                        `(:input 
                          (:type :standard) 
                          (:name "list")
                          (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
                        (when (second inputs)
                          `(:input 
                            (:type :optional) 
                            (:name "by")
                            (:value ,(find-value-in-kv-list (cdr (eval (second inputs))) :value)))
                          ))))
    ))

(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'forloop)) inputs position size value lock &rest rest)
  `(:box
    (:type :special)
    (:reference loop-for)
    (:name "for")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,(remove nil 
                       (list
                        `(:input 
                          (:type :standard) 
                          (:name "from")
                          (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
                        `(:input 
                          (:type :standard) 
                          (:name "to")
                          (:value ,(find-value-in-kv-list (cdr (eval (second inputs))) :value)))
                        (when (third inputs)
                          `(:input 
                            (:type :optional) 
                            (:name "by")
                            (:value ,(find-value-in-kv-list (cdr (eval (second inputs))) :value)))
                          ))))))

(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'whileloop)) inputs position size value lock &rest rest)
  `(:box
    (:type :special)
    (:reference loop-while)
    (:name "while")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,inputs)))


;;; COLLECT
(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'listing)) inputs position size value lock &rest rest)
  `(:box
    (:type :special)
    (:reference collect)
    (:name "collect")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,inputs)))


;;; ACCUM
(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'accumulator)) inputs position size value lock &rest rest)

  (declare (ignore name reference value numouts))
  
  `(:box
    (:type :special)
    (:reference accum)
    (:name "accum")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,inputs)
    (:inputs 
     (:input (:type :standard) (:name "data-in")
      (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
     
     ; !!! we invert the values of 2nd and 3rd input
     (:input (:type :standard) (:name "accum-function")
      (:value ,(find-value-in-kv-list (cdr (eval (third inputs))) :value)))
     (:input (:type :standard) (:name "init")
      (:value ,(find-value-in-kv-list (cdr (eval (second inputs))) :value)))
     )
    ))


(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'minim)) inputs position size value lock &rest rest)
  (om-print "Warning/OMLoop: MIN converted to ACCUM" "Compatibility")
  `(:box
    (:type :special)
    (:reference accum)
    (:name "accum (converted from min)")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs 
     (:input (:type :standard) (:name "data-in")
      (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
     (:input (:type :standard) (:name "accum-function")
      (:value om-min))
     (:input (:type :standard) (:name "init")
      (:value nil))
     )
    ))
  
(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'maxi)) inputs position size value lock &rest rest)
  (om-print "Warning/OMLoop: MAX converted to ACCUM" "Compatibility")
  `(:box
    (:type :special)
    (:reference accum)
    (:name "accum")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs 
     (:input (:type :standard) (:name "data-in")
      (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
     (:input (:type :standard) (:name "accum-function")
      (:value om-max))
     (:input (:type :standard) (:name "init")
      (:value nil))
     )
    ))

(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'sum)) inputs position size value lock &rest rest)
  (om-print "Warning/OMLoop: SUM converted to ACCUM" "Compatibility")
  `(:box
    (:type :special)
    (:reference accum)
    (:name "accum")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs 
     (:input (:type :standard) (:name "data-in")
      (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
     (:input (:type :standard) (:name "accum-function")
      (:value om+))
     (:input (:type :standard) (:name "init")
      (:value 0))
     )
    ))

(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'counter)) inputs position size value lock &rest rest)
  (om-print "Warning/OMLoop: COUNT converted to ACCUM" "Compatibility")
  `(:box
    (:type :special)
    (:reference accum)
    (:name "accum")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs 
     (:input (:type :standard) (:name "data-in")
      (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value)))
     (:input (:type :standard) (:name "accum-function")
      (:value om-1+))
     (:input (:type :standard) (:name "init")
      (:value 0))
     )
    ))



;;; used for the eachtime/iterate, finally/tempfinall, initdo/init-do boxes 
(defun convert-loop-box-inputs (inputs)
  (cons 
   ;;; one standard input...
   (let* ((in (eval (car inputs)))
          (val (find-value-in-kv-list (cdr in) :value)))
     `(:input 
       (:type :standard) 
       (:name "action")
       (:value ,val)))
   ;;; .. and the rest of optional inputs
   (mapcar #'(lambda (i) 
               (let* ((in (eval i))
                      (val (find-value-in-kv-list (cdr in) :value)))
                 `(:input 
                   (:type :optional) (:name "action")
                   (:value ,val)))) (cdr inputs))))


;;; EACH-TIME
(defmethod om-load-seqbox (name (reference (eql 'loopdo)) inputs position size value lock numouts)

  (declare (ignore name reference value numouts))

  `(:box
    (:type :special)
    (:reference iterate)
    (:name "iterate")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,(convert-loop-box-inputs inputs)) 
    )
  )

;;; INITDO
(defmethod om-load-seqbox (name (reference (eql 'initdo)) inputs position size value lock numouts)

  (declare (ignore name reference value numouts))

  `(:box
    (:type :special)
    (:reference init-do)
    (:name "init-do")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,(convert-loop-box-inputs inputs)))
  )


;;; Finally box needs to be replaced by one or several output boxe(es)
;;; but we need to "simulate" a single box during the time of loading the patch 
;;; in order to restore connections correctly
(defclass TempFinally (OMPatchInit) ())
(defclass TempFinallyBox (OMPatchInitBox) ())
(defmethod special-box-p ((name (eql 'temp-finally))) t)
(defmethod get-box-class ((self TempFinally)) 'TempFinallyBox)
(defmethod box-symbol ((self TempFinally)) 'temp-finally)
(defmethod omNG-make-special-box ((reference (eql 'temp-finally)) pos &optional init-args)
  (omNG-make-new-boxcall (make-instance 'TempFinally :name "finally") pos init-args))


;;; FINALLY
(defmethod om-load-seqbox (name (reference (eql 'finaldo)) inputs position size value lock numouts)

  (declare (ignore name reference value numouts))

  `(:box
    (:type :special)
    (:reference temp-finally)
    (:name "temp-finally")
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(and size (om-point-x size)))
    (:inputs .,(convert-loop-box-inputs inputs)))
  )



(defmethod convert-temp-finally-to-outputs ((self TempFinallyBox) (patch OMPatch))
 
  (let ((init-x (box-x self))
        (init-y (box-y self)))

    (loop for input in (inputs self)
          for i from 0
          do (let ((newout (omng-make-new-boxcall 
                            (make-instance 'omout :name (format nil "output ~D" (1+ i)))
                          (om-make-point (+ init-x (* i 60)) init-y))))
               
               (omng-add-element patch newout)
               (setf (value (car (inputs newout))) (value input))
               
               (when (car (connections input)) ;;; the input was connected...
                 (let ((new-connection (omng-make-new-connection 
                                        (from (car (connections input))) ;;; same "from" output
                                        (car (inputs newout)))))
                   (omng-add-element patch new-connection)))
                   
               ))
    
    (omng-remove-element patch self)))


(defmethod connect-to-iterator-box ((self OMPatchLoopBox) (patch OMPatch))
  
  (unless (connections (car (outputs self)))
    (let ((iterate-box (car (get-boxes-of-type patch 'OMPatchIteratorBox))))
      ;;; in principle there will be one (and only one) iterate in the patch
      (when iterate-box
        (optional-input++ iterate-box)
        (omng-add-element 
         patch 
         (omng-make-new-connection 
          (car (outputs self))
          (car (last (inputs iterate-box))))))
      )))


(defmethod flip-box-incoming-connections ((self OMBox) i1 i2)
  
  (let* ((in1 (nth i1 (inputs self)))
         (in2 (nth i2 (inputs self)))
         (c1 (car (connections in1)))
         (c2 (car (connections in2))))
    
    (when c1 (setf (to c1) in2))
    (when c2 (setf (to c2) in1))

    ))
 

;;; SOME MODIFICATIONS MADE:
;;; - input order in accum
;;; - auto connect forloop/while loops etc. to iterate
;;; - convert the finally to several outputs

(defmethod om-load-from-id ((id (eql :loop-patch)) data)
  (let ((patch (om-load-from-id :patch data)))
    
    ;;; do corrections
    (loop for box in (boxes patch) do

          (setf (box-y box) (- (box-y box) 40))

          (typecase box
            
            (OMValueBox 
             (initialize-size box))
            
            (TempFinallyBox 
             (convert-temp-finally-to-outputs box patch))
            
            (OMPatchLoopBox
             (connect-to-iterator-box box patch))

            (OMAccumBox
             (flip-box-incoming-connections box 1 2))

            (otherwise nil)))
        
    patch))



;======================================
; old forms not supported: 
;======================================
; old-old: not exported by OM6
;(defun om-load-ominstance1 (class name icon instance edparams &optional pictlist doc &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'editor)) name reference inputs position size value lock &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'slot)) name reference inputs position size value lock &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'comment)) name reference inputs position size value lock &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'mk-ins)) name reference inputs position size value lock &rest rest) )

; No Visual OOP in OM7
;(defun om-load-boxtypein (name type indice position docu keys defval &optional fname fsize) )
;(defun om-load-initin  (name type indice posi self? class &optional fsize) )


;============================================================================
; MAQUETTE
;============================================================================
#|
(defmethod om-load-boxcall ((self (eql 'maqabs)) name reference inputs position size value lock &rest rest) )
(defun om-load-maq1 (name boxes connections range markers &rest ignore) )
(defun om-load-maq2 (name boxes connections range markers &rest ignore) )
(defun om-load-maq-abs1 (name boxes connections range markers) )
(defun om-load-maq-boxin (name indice position docu &optional fname val fsize) )
(defun om-load-maq-boxout (name indice position inputs &optional fname fsize) )
(defun om-load-boxmaqselfin (name position  &optional fsize) )
(defun om-load-temp-patch (name boxes connections &optional version) )
(defun om-load-temp-patch1 (name boxes connections &optional version pictlist) )
(defun om-load-tempobj1 (name inputs refer numouts posx sizex clorf value ignorepict) )
(defun om-load-boxselfin (name position  &optional fsize) )
(defun om-load-tempboxout (name position inputs &optional fname fsize) )
|#

;============================================================================
; LISP-PATCH
;============================================================================
#|
(defun om-load-lisp-patch (name version expression) )
(defun om-load-lisp-abspatch (name version expression) )
|#

;============================================================================
; DATA: specific conversions for object types
;============================================================================

(defun load-obj-list-from-save (list)
  (loop for item in list collect (eval item)))

;;; SCORE OBJECTS
;;; => TODO
(defmethod set-patch-pairs ((self t) list) )
(defmethod load-port-info ((self t) port) )
(defmethod init-mus-color ((self t) color) )
(defmethod set-extra-pairs ((self t) extras) )
(defmethod set-tonalite ((self t) tonalite) )
(defmethod set-object-analysis ((self t) analyse) )

;;; SCORE EDITOR PARAMS
;;; => hack / to(re)do when score editors are operational
(defclass edition-values () 
  ((paper-size :accessor paper-size)
   (top-margin :accessor top-margin)
   (left-margin :accessor left-margin)
   (right-margin :accessor right-margin)
   (bottom-margin :accessor bottom-margin)
   (orientation :accessor orientation)
   (scale :accessor scale)
   (system-space :accessor system-space)
   (system-color :accessor system-color)
   (line-space :accessor line-space)
   (title :accessor title)
   (show-title? :accessor show-title?)
   (show-page? :accessor show-page?)
   (sheet-id :accessor sheet-id)
   (page-mode :accessor page-mode)))
   
;;; BPF
(defun simple-bpf-from-list (x-points y-points &optional (class 'bpf) (decimals 0))
  (make-instance class :x-points x-points :y-points y-points :decimals decimals))

(defun 3Dc-from-list (xlist ylist zlist &optional (class '3DC) (decimals 0))
  (make-instance class :x-points xlist :y-points ylist :z-points zlist :decimals decimals))


;;;============================================================================
;;; FROM EXTERNAL RESOURCES
;;;============================================================================

;;; If the resource is not found we give a try in the in-file folder
(defun search-for-resource (pathname)
  (let ((in-file-folder (get-pref-value :files :in-file)))
    (or (probe-file (om-make-pathname 
                     :name (pathname-name pathname)
                     :type (pathname-type pathname)
                     :directory in-file-folder))
        pathname)))

;;; SDIFFILE
(defmethod load-sdif-file ((path pathname))
  (let ((sdiff (make-instance 'SDIFFILE))
        (filepath (or (file-exist-p path)
                      (search-for-resource path))))
    (setf (file-pathname sdiff) filepath)
    (om-init-instance sdiff)))

;;; SOUND
(defun load-sound (path &optional track vol pan)
  (declare (ignore track vol pan))
  (let ((filepath (or (file-exist-p path)
                      (search-for-resource path))))
    (get-sound filepath)))


;============================================================================
; CHANGED ARG NAMES
;============================================================================
(defmethod changed-arg-names ((reference (eql 'om-sample)))
  '(("sample-rate" "nbs-sr")))

(defmethod changed-arg-names ((reference (eql 'chord-seq)))
  '(("legato" "llegato")))

;============================================================================
; CHANGED NAMES
;============================================================================
(defmethod changed-name ((reference (eql 'list-elements))) 'split)
(defmethod changed-name ((reference (eql 'get-mrk-onsets))) 'sdif->markers)
;============================================================================
; CHANGED CLASSES
;============================================================================

;;; xxx-LIBs
(defmethod update-reference ((ref (eql 'bpf-lib))) 'collection)
(defmethod update-reference ((ref (eql 'bpc-lib))) 'collection)
(defmethod update-reference ((ref (eql '3dc-lib))) 'collection)

(defclass bpf-lib () ((bpf-list :accessor bpf-list :initarg :bpf-list :initform nil)))
(defclass bpc-lib (bpf-lib) ())
(defclass 3DC-lib (bpc-lib) ())

(defmethod update-value ((self bpf-lib))
  (make-instance 'collection :obj-list (bpf-list self)))



#|

;;; Main boxes
x VALUE BOX
x FUNCTION BOX
x EDITOR BOX
x COMMENTS

;;; Inputs/Connections
x INPUT VALUES
x KEYWORD INPUTS incl. stored value (ex. sort-list)
x INPUTS WITH MENUINS eg. band-filter
x OPTIONAL INPUTS 
x REST INPUTS ex. list
x CHANGE OPTIONAL => KEY   ex. FILE-CHOOSER
x OBJECT NEW KEYWORDS ex. BPF decimals
x CONNECTIONS incl color

;;; State
x LOCK/EV-ONCE
x LAMBDA
x REF MODE

;;; Special boxes
x SLOTS
x REPEAT-N
x LIST-ELEMENTS
x SEQUENCE

x INSTANCE BOXES

;;; Abstractions
x IN/OUTS
x INTERNAL PATCH
x LISP-FUNCTIONS
x OMLOOP

GLOBAL PATCHES
GLOBAL VARS
MAQUETTE
DIALOG-ITEMS

x PATCH WITH LIB FUNCTIONS

x SDIFFILE
x SOUND  !!! markers seconds vs. milliseconds

TEXTFILE
CLASS-ARRAY

|#