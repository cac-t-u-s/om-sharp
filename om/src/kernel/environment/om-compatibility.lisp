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
          
          (handler-case  
          
              (load path)
            
            (error (err) 
              (om-print err "Compatibility")
              (abort)
              ; (error err)
              )
            )
          
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

;============================================================================
; REQUIRED LIBS
;============================================================================
;;; called to lod libs prior to the patch
(defun load-lib-for (list)
  (loop for libname in list do (require-library libname)))


;============================================================================
; PATCH (MAIN SECTION)
;============================================================================
;;; main patch file
(defun om-load-patch1 (name boxes connections &rest ignore)
  
  (declare (ignore ignore))
  
  ;(let ((patch (make-instance 'OMPatchFile :name name)))
 ;   (loop for box-code in boxes do
 ;         (let ((box (eval box-code)))
 ;           (when box (omng-add-element patch box))))
 ;   patch)
   
  (omng-load
   `(:patch
     (:boxes .,(loop for box-code in boxes collect (eval box-code)))
     (:connections .,(loop for c in connections collect (format-imported-connection c)))
     )
   )
  )
    


;;; internal sub-patch
(defun om-load-patch-abs1 (name boxes connections &rest ignore)
  (declare (ignore ignore))
  
  (let ((patch (make-instance 'OMPatchInternal :name name)))
  
    (loop for box-code in boxes do
          (let ((box (eval box-code)))
            (when box 
              (omng-add-element patch box))))
          
    patch))


;============================================================================
; BOXES
;============================================================================
;;; a box input
(defun om-load-inputfun (class doc name value) 
  `(:input (:type :standard) (:name ,name) (:value ,(omng-save value))))

;;; a box input "keyword" 
(defun om-load-inputkeyword (class doc name value defval menu) 
  `(:input (:type :key) (:name ,name) (:value ,(omng-save defval))))

;;; a box input with menu for different values
(defun om-load-inputfunmenu1 (class doc name value items) 
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
  

;;; Function boxes
(defmethod om-load-boxcall ((self (eql 'lispfun)) name reference inputs position size value lock &rest rest) 
  
  (let ((inputs (loop for formatted-in in (mapcar #'eval inputs) collect
                      ;;; correct the type and eventually the name of box inputs
                      (let ((name (check-arg-for-new-name 
                                   reference 
                                   (find-value-in-kv-list (cdr formatted-in) :name))))
                        (cond ((find name (mapcar #'symbol-name (function-main-args reference)) :test #'string-equal)
                               `(:input (:type :standard) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                              ((find name (mapcar #'symbol-name (function-optional-args reference)) :test #'string-equal)
                               `(:input (:type :optional) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                              ((find name (mapcar #'symbol-name (function-keyword-args reference)) :test #'string-equal)
                               `(:input (:type :key) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                              ((equal name (getf (function-arglist reference) '&rest))
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
      (:inputs .,inputs)
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

;;; Object boxes
(defun om-load-editor-box1 (name reference inputs position size value lock &optional fname editparams spict meditor pictlist show-name)
  
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
    

;;; Value boxes
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


;;; This was used to save OMLoops
;;; In om7 OMloop is just an normal patch
(defmethod om-load-boxwithed1 ((class t) name reference inputs position size value lock boxes conec numouts) 
  (declare (ignore numouts))
  (let ((patch (om-load-patch-abs1 name boxes conec)))
    (om-load-boxcall 'abstraction name reference inputs position size patch lock)))


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
; OTHER
;============================================================================

;;; IN BOX
(defun om-load-boxin (name indice position docu &optional fname val fsize) 
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
  `(:box 
    (:type :special)
    (:reference ,reference)
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:inputs .,(mapcar #'eval inputs))
    ))


;============================================================================
; TODO !!!
;============================================================================

(defmethod om-load-seqbox (name reference inputs position sizeload value lock numouts) )

(defun om-load-boxinstance (name instance inputs position &optional fname size) )
(defun om-load-ominstance1 (class name icon instance edparams &optional pictlist doc &rest rest) )

(defun om-load-boxtypein (name type indice position docu keys defval &optional fname fsize) )
(defun om-load-initin  (name type indice posi self? class &optional fsize) )

(defmethod om-load-boxcall ((self (eql 'abstraction)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'maqabs)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'editor)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'slot)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'comment)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'mk-ins)) name reference inputs position size value lock &rest rest) )
(defmethod om-load-boxcall ((self (eql 'undefined)) name reference inputs position size value lock &rest trest) nil)

;============================================================================
; MAQUETTE
;============================================================================

(defun om-load-maq1 (name boxes connections range markers &rest ignore) )
(defun om-load-maq2 (name boxes connections range markers &rest ignore) )

(defun om-load-maq-abs1 (name boxes connections range markers) )

(defun om-load-maq-boxin (name indice position docu &optional fname val fsize) )
(defun om-load-maq-boxout (name indice position inputs &optional fname fsize) )
(defun om-load-boxmaqselfin (name  position  &optional fsize) )

(defun om-load-temp-patch (name boxes connections &optional (version nil)) )
(defun om-load-temp-patch1 (name boxes connections &optional (version nil) pictlist) )

(defun om-load-tempobj1 (name inputs refer numouts posx sizex clorf value ignorepict) )

(defun om-load-boxselfin (name  position  &optional fsize) )
(defun om-load-tempboxout (name position inputs &optional fname fsize) )

;============================================================================
; LISP-PATCH
;============================================================================

(defun om-load-lisp-patch (name version expression) )
(defun om-load-lisp-abspatch (name version expression) )


;============================================================================
; DATA
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
 

;============================================================================
; CHANGED INPUT NAMES
;============================================================================
(defmethod changed-arg-names ((reference (eql 'om-sample)))
  '(("sample-rate" "nbs-sr")))

(defmethod changed-arg-names ((reference (eql 'chord-seq)))
  '(("legato" "llegato")))

;============================================================================
; CHANGED NAMES
;============================================================================
(defmethod changed-name ((reference (eql 'list-elements))) 'split)


#|
;;;=========================================
;;; TESTS
;;;=========================================

;;; Main boxes
x VALUE BOX
X FUNCTION BOX
X EDITOR BOX
X COMMENTS

;;; Inputs/Connections
X INPUT VALUES
X KEYWORD INPUTS incl. stored value (ex. sort-list)
X INPUTS WITH MENUINS eg. band-filter
X OPTIONAL INPUTS 
X REST INPUTS ex. list
X CHANGE OPTIONAL => KEY   ex. FILE-CHOOSER
X OBJECT NEW KEYWORDS ex. BPF decimals
X CONNECTIONS incl color

;;; State
X LOCK/EV-ONCE
X LAMBDA
X REF MODE

;;; Special boxes
X REPEAT-N
X LIST-ELEMENTS

;;; Abstractions
X IN/OUTS
LISP-FUNCTIONS
ABSTRACTION

OMLOOP

TEXTFILE

CLASS-ARRAY

PATCH WITH LIB FUNCTIONS

MAQUETTE

|#