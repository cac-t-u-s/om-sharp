;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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

(defvar *om-current-persistent* nil)
(defvar *current-loading-document* nil)

(pushr '(:old ("omp" "omm" "oml") "OM6 Files [compatibility mode]") *doctypes*)

(defun load-om6-file (path)
  
  (declare (special *om-current-persistent* *current-loading-document*))
  
  (om-print-format "Opening document : ~A" (list path) "Import/Compatibility")

  (clos::set-clos-initarg-checking nil) ;; in case some class initargs do not exist anymore...
  
  (let* ((previous-loading-document *current-loading-document*)
         (obj-type (cond ((string-equal (pathname-type path) "omp") 'OMPatchFile)
                         ((string-equal (pathname-type path) "oml") 'OMLispFunctionFile)
                         ((string-equal (pathname-type path) "omm") 'OMMaquetteFile)
                         (t nil)))
         (object (when obj-type (make-instance obj-type :name (pathname-name path)))))
    
    (when object 
      (setf *current-loading-document* path)
      (register-document object)
            
      (unwind-protect 
  
          (with-relative-ref-path path
    
            (with-open-file (in path :direction :input :if-does-not-exist nil)
      
              (unless (equal (read-line in nil :eof) :eof) ;; not empty
        
                ;; line #2 contains meta-data about the patch
                (let ((metadata (read-from-string (subseq (om-read-line in) 1))))
      
                  (if metadata 
                      (om-print-format "Converting from OM ~D (type '~A')" (list (car metadata) (cadr metadata)) "Import/Compatibility")
                    (om-print-format "No header found in document..." nil "Import/Compatibility"))
                
                  (load path)
                      
                  (if *om-current-persistent* ;;; filled in the patch-loading process
                          
                      (unwind-protect 
                          
                          (progn 
                            (copy-contents *om-current-persistent* object)
                            
                            (setf (create-info object) (list (om-get-date) (om-get-date) *app-name* *version*)
                                  (window-size object) (eval (nth 4 metadata))
                                  (saved? object) nil)
                            object)
                        
                        ;;; need to close *om-current-persistent* to remove references etc. ? 
                    
                        )
                        
                    (unregister-document object))
                  
                  ))))
        
        (unless previous-loading-document (clos::set-clos-initarg-checking t))
        (setf *current-loading-document* previous-loading-document)
        ))))



;============================================================================
; CONVERT ALL OM6-SAVE FUNCTIONS...
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

;;; idea: consider an auto-alignment option for patches, 
;;; => set a global variable "on" when an old patch is being loaded...

;;; Patch file
;;; macro allows us not to load any argument in &rest
(defmacro om-load-patch1 (name boxes connections &rest ignore)
  
  ;;; among the ignored arguments are also the patch-pictures
  (declare (ignore ignore))
  
  ;(let ((patch (make-instance 'OMPatchFile :name name)))
  ; (loop for box-code in boxes do
  ;   (let ((box (eval box-code)))
  ;     (when box (omng-add-element patch box))))
  ;  patch)
  
  `(let ((name-loaded ,name)
         (boxes-loaded ,boxes)
         (connections-loaded ,connections))
     
     ;;; only top-level patch acually load their contents:
     (omng-load
      `(:patch
        (:name ,name-loaded)
        (:boxes .,(loop for box-code in boxes-loaded collect (eval box-code)))
        (:connections .,(loop for c in connections-loaded collect (format-imported-connection c)))
        )
      )
     ))


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
; LISP FUNCTIONS
;======================================

;; Lisp-patch will return an "interned" version of the the form:
;; `(:textfun (:name ,name) (:text (:list "(lambda (x) (+ x 1))")))


;;; Lisp-fun file
(defun om-load-lisp-patch (name version expression)
  (omng-load 
   (om-load-lisp-abspatch name version expression)))

;;; internal Lisp fun
(defun om-load-lisp-abspatch (name version expression)
  (declare (ignore version))
  `(:textfun
    (:info 
     (:by "OM")
     (:version ,version))
    (:name ,name)
    (:text
     (:list .,(string-to-list expression "$")))
    ))


;======================================
; ABSTRACTION BOX (PATCH/LISP FUN)
;======================================
;;; textfun will also call this one. 
;;; they load ok anyway.
(defmethod om-load-boxcall ((self (eql 'abstraction)) name reference inputs position size value lock &rest rest)

  (declare (ignore value rest))
  
  ;; reference contains the actual patch-code
  ;; (let ((type (if (equal (car reference) :textfun) :textfun :patch)))
    
    `(:box 
      (:type :abstraction)
      (:reference ,reference)   ;; contains the actual patch-code
      (:name ,name)
      (:x ,(om-point-x position))
      (:y ,(om-point-y position))
      (:w ,(and size (om-point-x size)))
      (:h ,(if size (om-point-y size) 48))
      (:lock ,(if lock (cond ((string-equal lock "x") :locked) 
                             ((string-equal lock "&") :eval-once))))
      (:lambda ,(if lock (cond ((string-equal lock "l") :lambda) 
                               ((string-equal lock "o") :reference))))
      (:inputs .,(mapcar #'eval inputs))
      (:display :mini-view)
      )
    )


;======================================
; EXTERNAL PATCH (BLUE)
;======================================

(defun load-om6-patch-from-relative-path (path-list)
  
  (declare (special *om-current-persistent* *current-loading-document*))
  
  (let* ((name (car (last path-list)))
         
         ;;; try to locate the container-patch's OM6 workspace 
         (workspace-root 
          (when (find "elements" (pathname-directory *current-loading-document*) :test 'string-equal)
            (om-make-pathname :directory 
                              (subseq (pathname-directory *current-loading-document*) 0 
                                      (position "elements" (pathname-directory *current-loading-document*) :test 'string-equal)))))
         
         ;;; try to determine the original abstraction folder 
         (doc-path-folder (when workspace-root
                            (om-make-pathname :directory 
                                              (append (pathname-directory workspace-root) (butlast path-list)))))
         
         ;;; determine the actual abstraction file
         (doc-path (or (and doc-path-folder 
                            (find name (om-directory doc-path-folder :type (doctype-to-ext-list :old)) 
                                  :key 'pathname-name :test 'string-equal))
                       
                       ;;; also search in-current-path:
                       (find name (om-directory (om-make-pathname :directory *current-loading-document*) 
                                                :type (doctype-to-ext-list :old)) 
                             :key 'pathname-name :test 'string-equal)
                       
                       )))
    
    ;;; => found the patch
    (if (and doc-path (probe-file doc-path)) 
         
        ;;; if the main form is a om-load-patch1, the returned form will be a patch
        (let* ((expected-type (cond ((string-equal (pathname-type doc-path) "omp") "Patch")
                                    ((string-equal (pathname-type doc-path) "oml") "LispFun")
                                    ((string-equal (pathname-type doc-path) "omm") "Maquette")))
               
               ;;; check if there is a registered open document with same name that is _not yet saved_
               ;;; this happens in recursive patches or patches with several references to the same abstraction
               (registered-entry (find-if #'(lambda (entry) 
                                              (and (string-equal (name (doc-entry-doc entry)) name)
                                                   (string-equal (get-object-type-name (doc-entry-doc entry)) expected-type)
                                                   (null (doc-entry-file entry))))
                                          *open-documents*))
            
               (new-patch (if registered-entry ;; the doc was already loaded (or considered so!)
                              (doc-entry-doc registered-entry)
                            (load-om6-file doc-path))))
              
          ;;; return this: 
          (omng-save-relative new-patch *current-loading-document*))

      
      ;;; => patch not found
      ;;; search more broadly in non-saved registered documents (if it was previously open manually)
      (let ((registered-entry (find-if #'(lambda (entry) 
                                           (and (string-equal (name (doc-entry-doc entry)) name)
                                                (null (doc-entry-file entry))))
                                       *open-documents*)))
        
        (if registered-entry ;; the doc was already loaded (or considered so!)
            
            (let ((new-patch (doc-entry-doc registered-entry)))
              ;;; return this:
              (omng-save-relative new-patch *current-loading-document*))
          
          ;;; no way to find anything: ask user
          (let ((user-located (and (om-y-or-n-dialog (format nil "Can you locate the original OM6 abstraction ~s ?~%(Not found in ~s)" 
                                                             name 
                                                             (reduce #'(lambda (s1 s2) (concatenate 'string s1 "/" s2))
                                                                     (butlast (cdr path-list)))))
                                   (om-choose-file-dialog 
                                    :prompt (format nil "Please: locate OM6 abstraction ~s." name) 
                                    :directory *current-loading-document* :types (doctype-info :old)))))
            
            (if user-located
                
                ;;; got a patch
                (let ((new-patch (load-om6-file user-located)))
                  ;;; return this:
                  (omng-save-relative new-patch *current-loading-document*))
              
              ;;; not found at all
              (progn 
                (om-print-format "Abstraction box: ~s was not imported." (list name) "Import/Compatibility")
                ;;; will appear as "not found patch box"
                `(:patch-from-file
                  (:pathname
                   (:directory ,(cons :relative (butlast path-list)))
                   (:name ,name)))
                )
              )
            ))
        ))
    ))
  

;;; reference is a relative-path list pointing to a Patch or Lisp-fun file
(defmethod om-load-boxcall ((self (eql 'patch-box)) name reference inputs position size value lock &rest rest)
  
  (declare (ignore value rest))
  
  (let ((loaded-reference (load-om6-patch-from-relative-path reference)))
    
    (om-load-boxcall 'abstraction name loaded-reference inputs position size value lock)
    
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
  (declare (ignore class doc name menu))
  `(:input (:type :key) (:name ,value) (:value ,(omng-save defval))))

;;; a box input with menu for different values
(defun om-load-inputfunmenu1 (class doc name value items) 
  (declare (ignore class doc items))
  `(:input (:type :standard) (:name ,name) (:value ,(omng-save value))))


;;; handle dispatch to 'lispfun or handle unknown box 
(defmethod om-load-boxcall ((class t) name reference inputs position size value lock &rest rest)
  (cond ((fboundp reference)
         (om-load-boxcall 'lispfun name reference inputs position size value lock))
        ((and (function-changed-name reference) (fboundp (function-changed-name reference)))
         (let ((new-reference (function-changed-name reference)))
           (om-print-format "Function '~A' now replaced by '~A'" (list reference new-reference) "Import/Compatibility")
           (om-load-boxcall 'lispfun (string new-reference) new-reference inputs position size value lock)))
        (t 
         ;; (om-print-format "Unknown function for box of type ~A: '~A'" (list class reference) "Import/Compatibility")
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
  

;; this is the automatic name given in OM6 to added "rest" inputs
(defun is-om6-rest-arg (name)
  (find name '("add-input") :test 'string-equal))

(defun check-arg-for-new-name (reference name)
  (or 
   (cadr (find name (update-arg-names reference) :key #'car :test #'string-equal))
   
   (when (and (is-om6-rest-arg name) 
              (fboundp reference))
     (let ((rest-arglist (member '&rest (function-arglist reference))))
       ;;; the last element in lambda-list is the name of the &rest arg
       (when rest-arglist 
         (string (cadr rest-arglist))))) 
   
   name))


(defmethod (setf frame-position) (pos box) pos)
        

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
                             (string-equal name (string (cadr (member '&rest (function-arglist reference))))))
                        `(:input (:type :optional) (:name ,name) (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                       (t (om-print-format "Unknown input for function '~A': ~A" (list reference name) "Import/Compatibility")
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




;======================================
; OBJECT BOXES
;======================================

(defmethod om-load-editor-box1 (name reference inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore fname editparams meditor pictlist))
  
  (let* ((ref-val (or value (and (find-class reference nil) (make-instance reference))))
         (inputs (loop for formatted-in in (mapcar #'eval inputs) collect
                       ;;; correct the type and eventually the name of box inputs
                       (let ((name (check-arg-for-new-name 
                                    reference 
                                    (find-value-in-kv-list (cdr formatted-in) :name))))
                         
                         (cond 
                          
                          ((and (find-class reference nil)
                                (string-equal name "self")
                                (box-def-self-in reference))  ;;; this box has a special defval for "self"
                           `(:input 
                             (:type :standard) 
                             (:name ,name) 
                             (:value ,(box-def-self-in reference))))
                          
                          ((and (find-class reference nil)
                                (find name (mapcar #'symbol-name (additional-class-attributes ref-val))
                                      :test #'string-equal))  ;;; if the input has become a keywork input
                           `(:input 
                             (:type :key) 
                             (:name ,name) 
                             (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                          
                          (t `(:input 
                               (:type ,(find-value-in-kv-list (cdr formatted-in) :type))
                               (:name ,name) 
                               (:value ,(find-value-in-kv-list (cdr formatted-in) :value))))
                          )))))
    
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
    (:inputs .,(mapcar #'eval inputs)))
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

;;; only ominstances / omclasses have this
(defun icon-from-lib (icon name) (declare (ignore name)) icon)


(defun get-inst-from-globals (name) 
  (make-instance 'OMGlobalVar :name name))


(defun om-load-boxinstance (name instance inputs position &optional fname size) 
  
  (declare (ignore inputs fname size))
  
    
  (if (typep instance 'OMGlobalVar)
      
      ;;; global var
      `(:box 
        (:type :special)
        (:reference global)
        (:value nil)
        (:name ,name)
        (:x ,(om-point-x position))
        (:y ,(om-point-y position))
        )

     (let* ((value (instance instance))
            (type (type-of value)))
       
       ;;; instance
       (if (listp value)
           ;;; lists => collection
           `(:box 
             (:type :object)
             (:reference collection) 
             (:value (:object (:class collection) 
                      (:slots ((:obj-list ,(omng-save value))))))
             (:name ,name)
             (:lock :locked)
             (:x ,(om-point-x position))
             (:y ,(om-point-y position))
             (:display :text))
         
         (let () ;;; other sort of objects => object box
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
             (:display :mini-view)
             )
        )))
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

  (declare (ignore fname fsize))

  `(:box
    (:type :io)
    (:reference (:out (:type omout) (:name ,name) (:index ,indice)))
    (:name ,name)
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
     "Import/Compatibility"))
  
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
;;; OMloop is now just a normal patch

(defmethod om-load-boxwithed1  ((class t) name reference inputs position size value lock boxes connec numouts &optional fname pictlist) 
  `(:box 
    (:type :abstraction)
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
  (om-print "Warning/OMLoop: MIN converted to ACCUM" "Import/Compatibility")
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
      (:value om-min))
     (:input (:type :standard) (:name "init")
      (:value nil))
     )
    ))
  
(defmethod om-load-boxcall ((class (eql 'genfun)) name (reference (eql 'maxi)) inputs position size value lock &rest rest)
  (om-print "Warning/OMLoop: MAX converted to ACCUM" "Import/Compatibility")
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
  (om-print "Warning/OMLoop: SUM converted to ACCUM" "Import/Compatibility")
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
  (om-print "Warning/OMLoop: COUNT converted to ACCUM" "Import/Compatibility")
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
; DI-BOXES 
;======================================
;;; hacks: expressions contained in the OM6 patch format:
(defmethod om-make-dialog-item ((type t) pos size text &rest args) nil)

;;; TEXT BOX
;;; hacks: expressions contained in the OM6 patch format:
(defmethod om-make-dialog-item ((type (eql 'text-box)) pos size text &rest args) text)

(defmethod om-load-editor-box1 (name (reference (eql 'text-box))
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore reference fname editparams meditor pictlist))
  
  `(:box 
     (:type :value)
     (:reference string) 
     (:value ,value)
     (:name ,name)
     (:x ,(om-point-x position))
     (:y ,(om-point-y position))
     (:w ,(om-point-x size))
     (:lock ,(if lock :locked nil))
     (:inputs 
      (:input (:type :optional) (:name "in")
       (:value ,(find-value-in-kv-list (cdr (eval (first inputs))) :value))))
     ))


;;; TEXT-VIEW
;;; => Convert to textbuffer

;;; hacks: expressions contained in the OM6 patch format:
(defmethod om-make-dialog-item ((type (eql 'text-view)) pos size text &rest args) (list text))
(defmethod om-set-dialog-item-text ((dummy list) text) (setf (car dummy) text))

(defmethod om-load-editor-box1 (name (reference (eql 'text-view))
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)

  `(:box
    (:type :object)
    (:reference textbuffer)
    (:name "text-view")
    (:value (:object
             (:class textbuffer)
             (:slots ((:contents ,(omng-save (string-to-list (car value) (string #\Newline))))))))
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(om-point-x size))
    (:h ,(om-point-y size))
    (:lock ,(if lock :locked nil))
    (:showname nil)
    (:display :mini-view)
    (:edition-params (:output-mode :text))
    (:inputs 
     (:input (:type :standard) (:name "self") (:value nil))
     (:input (:type :standard) (:name "contents") (:value nil))
     (:input (:type :key) (:name "output-mode") (:value :text))
     )
    ))


;;; SINGLE-ITEM-LIST / MULTI-ITEM-LIST

;;; hacks: expressions contained in the OM6 patch format:
(defmethod om-make-dialog-item ((type (eql 'single-item-list)) pos size text &rest args) 
  (mapcar #'list (getf args :range)))
(defmethod om-make-dialog-item ((type (eql 'multi-item-list)) pos size text &rest args) 
  (mapcar #'list (getf args :range)))

(defmethod om-set-selected-item-index ((self list) index)
  (let ((ilist (list! index)))
    (loop for pos in ilist do
          (setf (nth pos self) (list (car (nth pos self)) t)))))

(defmethod (setf di-data) (data list) nil)


(defmethod om-load-editor-box1 (name (reference (eql 'single-item-list))
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore name lock reference fname editparams meditor pictlist))
  
  (let* ((items-list value)
         (items (mapcar #'car items-list))
         (selection-index (list (position t items-list :key #'cadr)))
         (selection (car (find t items-list :key #'cadr))))
    
    `(:box 
      (:type :interface)
      (:reference list-selection)
      (:name "list-selection")
      (:multiple-selection nil)
      (:value ,selection)
      (:items ,(omng-save items))
      (:selection ,(omng-save selection-index))
      (:x ,(om-point-x position))
      (:y ,(om-point-y position))
      (:w ,(om-point-x size))
      (:h ,(om-point-y size))
      (:inputs 
       (:input (:type :key) (:name "items")
        (:value ,(if lock (omng-save items)
                   (find-value-in-kv-list (cdr (eval (first inputs))) :value)))))
      )
    ))

(defmethod om-load-editor-box1 (name (reference (eql 'multi-item-list))
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore name lock reference fname editparams meditor pictlist))
  
  (let* ((items-list value)
         (items (mapcar #'car items-list))
         (selection-indices (loop for i from 0 
                                  for item in items-list 
                                  when (cadr item) 
                                  collect i))
         (selection (loop for item in items-list 
                          when (cadr item) 
                          collect (car item))))
 
    `(:box 
      (:type :interface)
      (:reference list-selection)
      (:multiple-selection t)
      (:items ,(omng-save items))
      (:selection ,(omng-save selection-indices))
      (:value ,(omng-save selection))
      (:name "list-selection")
      (:x ,(om-point-x position))
      (:y ,(om-point-y position))
      (:w ,(om-point-x size))
      (:h ,(om-point-y size))
      (:inputs 
       (:input (:type :key) (:name "items")
        (:value (if lock (omng-save items)
                  (find-value-in-kv-list (cdr (eval (first inputs))) :value)))))
      )
    ))


;;; MENU

;;; hacks: expressions contained in the OM6 patch format:
(defmethod om-make-dialog-item ((type (eql 'pop-up-menu)) pos size text &rest args) 
  (mapcar #'list (getf args :range)))

(defmethod om-load-editor-box1 (name (reference (eql 'pop-up-menu))
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore name lock reference fname editparams meditor pictlist))
  
  (let* ((items-list value)
         (items (mapcar #'(lambda (item) 
                            (if (stringp (car item))
                                (let ((read-item (ignore-errors (read-from-string (car item)))))
                                  (if (symbolp read-item) (car item) ;;; keep it as it was
                                    read-item)) ;;; we read numbers, lists, etc...
                              (car item)))
                        items-list))
         (selection (position t items-list :key #'cadr)))
    
    `(:box 
      (:type :interface)
      (:reference list-menu)
      (:name "list-menu")
      (:value ,(nth selection items))
      (:items ,(omng-save items))
      (:selection ,selection)
      (:x ,(om-point-x position))
      (:y ,(om-point-y position))
      (:w ,(om-point-x size))
      (:inputs 
       (:input (:type :key) (:name "items")
        (:value ,(if lock (omng-save items)
                   (find-value-in-kv-list (cdr (eval (first inputs))) :value)))))
      )
    ))

;;; CHECK-BOX

;;; hacks: expressions contained in the OM6 patch format:
(defmethod om-make-dialog-item ((type (eql 'check-box)) pos size text &rest args) (list nil))
(defmethod om-set-check-box ((self list) val) (setf (car self) val))

(defmethod om-load-editor-box1 (name (reference (eql 'check-box))
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore name size lock reference fname editparams meditor pictlist))
  
  `(:box 
    (:type :interface)
    (:reference check-box)
    (:name "check-box")
    (:value ,(car value))
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    )
    )

;;; BUTTON

(defmethod om-make-dialog-item ((type (eql 'button)) pos size text &rest args) text)

(defmethod om-load-editor-box1 (name (reference (eql 'button))
                                     inputs position size value lock 
                                     &optional fname editparams spict meditor pictlist show-name)
  (declare (ignore name lock reference fname editparams meditor pictlist))
  
  `(:box 
    (:type :interface)
    (:reference button)
    (:name "button")
    (:text ,value)
    (:x ,(om-point-x position))
    (:y ,(om-point-y position))
    (:w ,(om-point-x size))
    ))


;============================================================================
; DATA: specific conversions for object types
;============================================================================

;;;============================================================================
;;; FROM EXTERNAL RESOURCES
;;;============================================================================
;;; REdefinition of some OM6 load-utilities

;;; If the resource is not found we give a try in the in-file folder
(defun search-for-resource (pathname)
  (let ((in-file-folder (get-pref-value :files :in-file)))
    (or (probe-file (om-make-pathname 
                     :name (pathname-name pathname)
                     :type (pathname-type pathname)
                     :directory in-file-folder))
        pathname)))

(defun om-load-if (pathname fun)
  (funcall fun pathname))


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


;;;============================================================================
;;; PICTURES
;;;============================================================================
;;; TODO: not supported yet

(defclass picture () ())
(defclass patch-picture () 
  ((pict-pos :accessor pict-pos :initarg :pict-pos)
   (pict-size :accessor pict-size :initarg :pict-size)))
(defun restore-pict-path (path) path)
(defun om-get-picture (name location) nil)

(defun corrige (lis) lis)

#|

;(let ((newpict (make-instance (quote patch-picture) :name "arrow_down_1" :source (quote user) :pict-pathname (restore-pict-path (restore-path nil)) :thepict (om-get-picture "arrow_down_1" (quote user)) :storemode :external :draw-params (quote (p 0 0 100 100)) :extraobjs nil))) (setf (pict-pos newpict) (om-make-point 619 173)) (setf (pict-size newpict) (om-make-point 50 112)) newpict)

|#


;============================================================================
;;; THE "COMPATIBILITY API":
;============================================================================

;;; Also useful, from OM-LOAD-FROM-ID:
;;; The function FUNCTION-CHANGED-NAME allows to convert a box to a new one.
;;; (e.g. (defmethod function-changed-name ((reference (eql 'old-name))) 'new-name))

;;; When a reference has changed: 
;;; e.g.: (defmethod update-reference ((ref (eql 'old-class))) 'new-class) 
(defmethod update-reference ((ref t)) nil)

;;; When the class exists (might have been redefined just for enabling import) 
;;; and needs to be converted to something else
;;; e.g.: (defmethod update-value ((self 'old-class)) (make-instance 'new-class)) 
(defmethod update-value ((self t)) self)

;;; This compatibility system favors name-semantics over order: inputs will be correctly imported (in particular, if they have a default value) and connected if they have the same name in OM6. This permist same arguments to have a different position on the box. If they don't the following function allows to cover specific cases:

;;; When some box inputs have changed name 
;;; redefine with eql-specializers for specific functions of class name
;;; e.g. (defmethod update-arg-names ((reference (eql 'function-or-class))) '(("old-arg-name" "new-arg-name")))
(defmethod update-arg-names ((ref t)) nil)


;======================================
; compat earlier versions (OM4.x !)
;======================================

(defun om-load-patch (name boxes connections &rest ignore)
  (om-load-patch1 name boxes connections))

(defmethod om-point-x ((point number))
  (- point (ash (ash point -16) 16)))

(defmethod om-point-y ((point number))
  (ash point -16))

;======================================
; old forms not supported: 
;======================================
; old-old: not exported by OM6
;(defun om-load-ominstance1 (class name icon instance edparams &optional pictlist doc &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'editor)) name reference inputs position size value lock &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'slot)) name reference inputs position size value lock &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'comment)) name reference inputs position size value lock &rest rest) )
;(defmethod om-load-boxcall ((self (eql 'mk-ins)) name reference inputs position size value lock &rest rest) )

; Visual-OOP specifics: not supported
;(defun om-load-boxtypein (name type indice position docu keys defval &optional fname fsize) )
;(defun om-load-initin  (name type indice posi self? class &optional fsize) )


