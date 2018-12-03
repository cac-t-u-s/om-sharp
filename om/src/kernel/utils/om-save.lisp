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

;;;============================
;;; BASIC PERSISTANT FEATURES
;;;============================

(in-package :om)

;;;===================================
;;; GENERAL SAVE FORMAT:
;;;
;(:IDENTIFIER
;  (:ATTRIBUTE1 VALUE1
;   :ATTRIBUTE2 VALUE2
;   ...))
;;;===================================

;;; OMNG-SAVE is the general methods for saving something on disk
;;; OMNG-LOAD reloads and build Lisp/OM data from the saved list

(defmethod omng-save ((self t)) 'NIL)
(defmethod omng-load ((self t)) self)

(defmethod omng-save ((self list)) 
  (cons :list (mapcar 'omng-save self)))

(defmethod omng-save ((self null)) 'NIL)
(defmethod omng-save ((self number)) self)
(defmethod omng-save ((self character)) self)


; (defmethod omng-save ((self symbol)) self)
(defmethod omng-save ((self symbol)) 
  (if (find (symbol-package self) (append 
                                   (list (find-package :om)
                                         (find-package :keyword)
                                         (find-package :common-lisp))
                                   ;(package-use-list :om)
                                   ))
      self
    `(:symbol ,(symbol-name self) ,(package-name (symbol-package self)))))
  
(defmethod om-load-from-id ((id (eql :symbol)) data)
  (let ((p (find-package (cadr data))))
    (when p
      (intern (car data) p))))


;;; CAR = IDENTIFIER, CDR = DATA
;;; OM-LOAD IS A SUB-PROCESS OF OMNG-LOAD
;;; THE FIRST ELEMENT IS PROCESSED CLOS BINDING TO OM-LOAD
(defmethod omng-load ((self cons))
  ;(let ((*package* (find-package :om)))
    (om-load-from-id (car self) (cdr self)))

;;; If ID is not recognized, the full list is loaded
(defmethod om-load-from-id (id data) data)

(defmethod om-load-from-id ((id (eql :list)) data)
  (mapcar 'omng-load data))

;;;===================================
;;; SAVE/LOAD METHODS FOR STANDARD AND OM OBJECTS
;;;===================================

#|
(defmethod omng-save ((self standard-object))  
  `(:object 
    (:class ,(type-of self))
    (:slots ,(loop for slot in (class-slots (class-of self))
                    when (slot-definition-initargs slot)
                    collect (list (car (slot-definition-initargs slot))
                                  (omng-save 
                                   (slot-value self (slot-definition-name slot))
                                   ))))
    ))

(defmethod om-load-from-id ((id (eql :object)) data) 
  (let ((class (find-value-in-kv-list data :class))
        (slots (mapcar #'(lambda (slot)
                           (list (car slot) (omng-load (cadr slot))))
                       (find-value-in-kv-list data :slots))))
    (apply 'make-instance (cons class (reduce 'append slots)))))
|#


(defmethod additional-slots-to-save ((self t)) 
  (additional-class-attributes self))

;;; objects copy/save only the slot with initargs and additional class attributes
(defmethod omng-save ((self standard-object))  ; OMObject
  (append 
   `(:object
     (:class ,(omng-save (type-of self)))
     (:slots ,(loop for slot in (class-slots (class-of self))
                    when (slot-definition-initargs slot)
                    collect (list (car (slot-definition-initargs slot))
                                  (omng-save 
                                   (if (fboundp (slot-definition-name slot))
                                       (funcall (slot-definition-name slot) self)
                                     (slot-value self (slot-definition-name slot)))
                                   )))))
   (when (additional-slots-to-save self)
     `((:add-slots ,(loop for add-slot in (additional-slots-to-save self)
                          collect (list (intern-k add-slot) 
                                        (omng-save 
                                         ;(slot-value self add-slot)
                                         ;;; in _some cases_ the accessor is defined and not the slot...
                                         (funcall add-slot self)
                                         )))))
     )))


(defmethod om-load-from-id ((id (eql :object)) data)
  (let* ((class-name (omng-load (find-value-in-kv-list data :class)))
         (class (find-class class-name nil)))
    (if class
        (let ((slots (remove nil 
                             (mapcar #'(lambda (slot)
                                         (if (find (car slot) (class-slots class) :key #'(lambda (slotdef) (car (slot-definition-initargs slotdef))))
                                             (list (car slot) (omng-load (cadr slot)))
                                           (om-beep-msg "LOAD: Slot '~A' not found in class ~A !!" (car slot) (string-upcase class-name))))
                                     (find-value-in-kv-list data :slots))))
              (more-slots (mapcar #'(lambda (slot)
                                      (list (car slot) (omng-load (cadr slot))))
                                  (find-value-in-kv-list data :add-slots))))
    
          (let ((object (apply 'make-instance (cons class-name (reduce 'append slots)))))
            
            (loop for slot in more-slots
                  ;; when (slot-exists-p object (car slot))
                  ;; do (setf (slot-value object (car slot)) (cadr slot))
                  do (set-slot-val object (symbol-name (car slot)) (cadr slot))
                  )
            
            (om-init-instance object nil)
            ))
      (om-beep-msg "LOAD: Class ~A not found !!" class-name)
      )))

; (omng-save (make-instance 'bpf))
; (om-load-from-id :om-object '((:class bpf) (:slots ((:x-points (0 100 200)) (:y-points (0 700 200)) (:decimals 0))) (:add-slots ((color nil) (name "BPF")))))
    
;;;===================================
;;; SAVE/LOAD METHODS FOR
;;; BASIC TYPES
;;;===================================

(defmethod omng-save ((self pathname))  
  `(:pathname 
    (:directory ,(pathname-directory self))
    (:device ,(pathname-device self))
    (:host ,(pathname-host self))
    (:name ,(pathname-name self))
    (:type ,(pathname-type self))))

(defmethod om-load-from-id ((id (eql :pathname)) data)
  
  (let* ((dir (find-value-in-kv-list data :directory))
         (path (om-make-pathname :directory dir
                                 :device (find-value-in-kv-list data :device)
                                 :host (find-value-in-kv-list data :host)
                                 :name (find-value-in-kv-list data :name)
                                 :type (find-value-in-kv-list data :type))))
    
    (if (equal (car dir) :relative)
        ; (restore-path path *relative-path-reference*)
        (merge-pathnames path *relative-path-reference*)
      
      path)))
   

(defmethod omng-save ((self gp::font-description))  
  `(:font 
    (:face ,(om-font-face self))
    (:size ,(om-font-size self))
    (:style ,.(om-font-style self))
    ))

(defmethod om-load-from-id ((id (eql :font)) data)
  (om-make-font (find-value-in-kv-list data :face)
                (find-value-in-kv-list data :size)
                :style (find-values-in-prop-list data :style)
                ))

(defmethod omng-save ((self font-or-nil))  
  `(:font-or-nil 
    (:font ,(omng-save (font-or-nil-font self)))
    (:t-or-nil ,(font-or-nil-t-or-nil self))))

(defmethod om-load-from-id ((id (eql :font-or-nil)) data)
  (make-font-or-nil 
   :font (omng-load (find-value-in-kv-list data :font))
   :t-or-nil (find-value-in-kv-list data :t-or-nil)))


(defmethod omng-save ((self oa::ompoint))  
  `(:point ,(om-point-x self) ,(om-point-y self))) 

(defmethod om-load-from-id ((id (eql :point)) data)
  (apply 'om-make-point data))

(defmethod omng-save ((self oa::omcolor))  
  `(:color ,(om-color-r self) ,(om-color-g self) ,(om-color-b self) ,(om-color-a self)))

(defmethod om-load-from-id ((id (eql :color)) data)
    (apply 'om-make-color data))

(defmethod omng-save ((self color-or-nil))  
  `(:color-or-nil 
    (:color ,(omng-save (color-or-nil-color self)))
    (:t-or-nil ,(color-or-nil-t-or-nil self))))

(defmethod om-load-from-id ((id (eql :color-or-nil)) data)
  (make-color-or-nil 
   :color (omng-load (find-value-in-kv-list data :color))
   :t-or-nil (find-value-in-kv-list data :t-or-nil)))


(defmethod sethash ((self hash-table) (entry t) (value t))
  (setf (gethash entry self) value))

(defmethod omNG-save ((self hash-table))
  (let (keylist vallist)
                      (maphash #'(lambda (key val)
                                   (push key keylist)
                                   (push val vallist)) self)
                      (setf keylist (reverse keylist)
                            vallist (reverse vallist))
  `(:hash-table
    (:keys ,(omng-save keylist))
    (:vals ,(omng-save vallist))
    (:test ,(hash-table-test self)))))

(defmethod om-load-from-id ((id (eql :hash-table)) data)
  (let ((res (make-hash-table :test (find-value-in-kv-list data :test))))
    (loop for key in (omng-load (find-value-in-kv-list data :keys))
          for val in (omng-load (find-value-in-kv-list data :vals))
          do
          (setf (gethash key res) val))
    res))

(defmethod omng-save ((self array)) ;;faire les types etc aussi
  (let* ((dimensions (array-dimensions self))
         (vals (loop for i from 0 to (1- (reduce '+ dimensions))
                     collect
                     (row-major-aref self i))))
    `(:array 
      (:dimensions ,(omng-save dimensions))
      (:content-as-list ,(omng-save vals)))))
  
(defmethod om-load-from-id ((id (eql :array)) data)
  (let ((res (make-array (omng-load (find-value-in-kv-list data :dimensions)))))
    (loop for val in (omng-load (find-value-in-kv-list data :content-as-list))
          for i from 0
          do
          (setf (row-major-aref res i) val))
    res))

(defmethod omng-save ((self string)) self)
(defmethod omng-save ((self function)) nil)


(defmethod omng-save ((self number-or-nil))  
  `(:number-or-nil 
    (:number ,(number-or-nil-number self))
    (:t-or-nil ,(number-or-nil-t-or-nil self))))

(defmethod om-load-from-id ((id (eql :number-or-nil)) data)
  (make-number-or-nil 
   :number (find-value-in-kv-list data :number)
   :t-or-nil (find-value-in-kv-list data :t-or-nil)))


;;;===================================
;;; SAVE/LOAD METHODS FOR
;;; SPECIAL OM OBJECTS
;;;===================================

(defmethod save-patch-contents ((self OMProgrammingObject) &optional (box-values nil))
  `(,(object-doctype self)
    (:om-version ,(omversion self))
    (:name ,(name self))
    (:doc ,(doc self))
    (:info 
     (:created ,(car (create-info self)))
     (:modified ,(cadr (create-info self))))
    (:window 
     (:size ,(when (window-size self)
               (list (om-point-x (window-size self)) (om-point-y (window-size self)))))  
     (:position ,(when (window-pos self)
                   (list (om-point-x (window-pos self)) (om-point-y (window-pos self))))))
    ))

;;;=============
; PATCH / MAQUETTE
;;;=============

(defmethod save-patch-contents ((self OMPatch) &optional (box-values nil))
 (append
   (call-next-method self t)
   `((:grid ,(grid self))
     (:lock ,(lock self))
     (:boxes ,.(loop for box in (boxes self) 
                     for i = 0 then (+ i 1) 
                     collect (append (if box-values (omng-save-with-value box) (omng-save box))
                                     (list (list :id i)))))
     (:connections ,.(save-connections-from-boxes (boxes self))))))


(defmethod omng-save ((self OMPatch)) 
  (save-patch-contents self))

;;; when we save an abstraction box..
;;; I think this is never called anymore since OMBoxAbstraction handles its own omng-save
;(defmethod omng-save ((self OMPatchFile))  
;  `(:patch-from-file ,(namestring (mypathname self))))

(defmethod omng-save-relative ((self OMPatchFile) ref-path)  
  `(:patch-from-file ,(omng-save (relative-pathname (mypathname self) ref-path))))


(defmethod load-patch-contents ((patch OMPatch) data)
  (let ((*required-libs-in-current-patch* nil))
    
    (let ((name (find-value-in-kv-list data :name))
          (info (find-values-in-prop-list data :info))
          (win (find-values-in-prop-list data :window)))
        
      ; in principle the name is determined by the pathname
      (unless (name patch) (setf (name patch) name))

      (setf (create-info patch) (list (find-value-in-kv-list info :created)
                                      (find-value-in-kv-list info :modified))
            (doc patch) (find-value-in-kv-list data :doc)
            (omversion patch) (find-value-in-kv-list data :om-version))
      
      (when win
        (let ((pos (find-value-in-kv-list win :position))
              (size (find-value-in-kv-list win :size)))
          (when pos (setf (window-pos patch) (omp (car pos) (cadr pos))))
          (when size (setf (window-size patch) (omp (car size) (cadr size))))))
           
      ;;; set loaded? now in case of self-nested patched
      (setf (loaded? patch) t)

      (let* ((saved-boxes (find-values-in-prop-list data :boxes))
             (saved-connections (find-values-in-prop-list data :connections)))

        ;;; we load the i/o boxes first (so they are taken into account, e.g. on self-embedded patch boxes
        ;;; but we must not change the order of boxes in the list because of the connections
        ;;; => double loop
        (let ((boxes (loop for saved-box in saved-boxes
                           collect
                           (if (equal :io (find-value-in-kv-list (cdr saved-box) :type))
                               (let ((box (omng-load saved-box)))
                                 (omng-add-element patch box)
                                 box)
                               nil) ;; do it in the next round
                           )))
          
          ;;; load other boxes
          (loop for box in boxes
                for i = 0 then (+ i 1) 
                when (null box)
                do (let ((box (omng-load (nth i saved-boxes))))
                     (setf (nth i boxes) box)
                     (omng-add-element patch box)
                     ))
                
          (mapc 
           #'(lambda (c) (omng-add-element patch c))
           (restore-connections-to-boxes saved-connections boxes))
          
          ))
      
      (setf (lock patch) (find-value-in-kv-list data :lock))
      (setf (grid patch) (find-value-in-kv-list data :grid))

      patch)))


;;; Handles libary-dependent boxes. See om-library.lisp.
;(defmethod load-patch-contents :before ((patch OMPatch) data)
;  (setf *required-libs-in-current-patch* nil))

;(defmethod load-patch-contents :after ((patch OMPatch) data)
;  (setf *required-libs-in-current-patch* nil))


(defmethod om-load-from-id ((id (eql :patch)) data)
  (let ((patch (make-instance 'OMPatchInternal)))
    (when data (load-patch-contents patch data))
    patch))


(defmethod om-load-from-id ((id (eql :patch-from-file)) data)
  
  (let* ((path (omng-load (car data)))
         (checked-path (check-path-using-search-path path)))
    
    (if checked-path
        
        (load-doc-from-file checked-path :patch)
      
      (let ((patch (make-instance'OMPatchFile :name (pathname-name path))))
        (om-beep-msg "PATCH NOT FOUND: ~S !" path)
        (setf (mypathname patch) path)
        patch)  
      
      )))

;;;=================================
;;; MAQUETTE
;;;=================================
(defmethod save-patch-contents ((self OMMaquette) &optional (box-values nil)) 
  (append
   (call-next-method self t)
   `((:range ,(range self))
     (:control-patch ,(omng-save (ctrlpatch self))))))


(defmethod load-patch-contents ((patch OMMaquette) data)
  (let ((maquette (call-next-method))
        (patch (find-value-in-kv-list data :control-patch))
        (range (find-value-in-kv-list data :range)))
    (when patch (set-control-patch maquette (omng-load patch)))
    (when range (setf (range maquette) range))
    maquette))
      
(defmethod om-load-from-id ((id (eql :maquette)) data)
  (let ((maq (make-instance 'OMMaquetteInternal :name (find-value-in-kv-list data :name))))
    (load-patch-contents maq data)
    maq))



(defmethod omng-save-relative ((self OMMaquetteFile) ref-path)  
  `(:maquette-from-file ,(omng-save (relative-pathname (mypathname self) ref-path))))

(defmethod om-load-from-id ((id (eql :maquette-from-file)) data)

  (let* ((path (omng-load (car data)))
         (checked-path (check-path-using-search-path path)))
    
    (if checked-path
        
        (load-doc-from-file checked-path :maquette)
      
      (om-beep-msg "FILE NOT FOUND: ~S !" path))
    ))

;(let ((data (cdr (car (list-from-file "/Users/bresson/Desktop/test.omp")))))
  ;(find-values-in-prop-list data :info)
 ; (mapcar #'omng-load (find-values-in-prop-list data :boxes))
;  (find-values-in-prop-list data :boxes)
;  )
;(omng-load '(:box (:reference pprint) (:position (:point 260 242)) (:size (:point 63 28)) (:icon :left) (:color nil) (:border t) (:font nil) (:align :left) (:lock nil) (:lambda nil) (:reactive nil) (:inputs (:standard "OBJECT" nil))))

;;;=================================
;;; LISP FUNCTION
;;;=================================

(defmethod om-load-from-id ((id (eql :textfun)) data)
  (let ((fun (make-instance 'OMLispFunctionInternal :name (find-value-in-kv-list data :name)))
        (info (find-values-in-prop-list data :info))
        (win (find-values-in-prop-list data :window)))
    
    (setf 
     (text fun) (omng-load (find-value-in-kv-list data :text))
     (create-info fun) (list (find-value-in-kv-list info :created)
                             (find-value-in-kv-list info :modified))
     (doc fun) (find-value-in-kv-list data :doc)
     (omversion fun) (find-value-in-kv-list data :om-version))
    
    (when win
      (let ((pos (find-value-in-kv-list win :position))
            (size (find-value-in-kv-list win :size)))
        (when pos (setf (window-pos fun) (omp (car pos) (cadr pos))))
        (when size (setf (window-size fun) (omp (car size) (cadr size))))))
    
    (setf (loaded? fun) t)
    fun))

(defmethod save-patch-contents ((self OMLispFunction) &optional (box-values nil))
  (append
   (call-next-method self t)
   `((:text ,(omng-save (text self))))))

(defmethod omng-save ((self OMLispFunction)) 
  (save-patch-contents self))

;;; when we save an abstraction box..
;;; I think this is never called anymore since OMBoxAbstraction handles its own omng-save
;(defmethod omng-save ((self OMLispFunctionFile))
;  `(:textfun-from-file ,(namestring (mypathname self))))

(defmethod omng-save-relative ((self OMLispFunctionFile) ref-path)  
  `(:textfun-from-file ,(omng-save (relative-pathname (mypathname self) ref-path))))


(defmethod load-patch-contents ((patch OMLispFunction) data) nil)


(defmethod om-load-from-id ((id (eql :textfun-from-file)) data)
  
  (let* ((path (omng-load (car data)))
         (checked-path (check-path-using-search-path path)))
    
    (if checked-path
        
        (load-doc-from-file checked-path :textfun)

      (om-beep-msg "FILE NOT FOUND: ~S !" path))
    ))


;;;=================================
;;; INs and OUTs ...
;;;=================================

(defmethod omng-save ((self OMIn))
  `(:in
    (:type ,(type-of self)) 
    (:index ,(index self))
    (:name ,(name self))
    (:doc ,(doc self))))

(defmethod omng-save ((self OMSelfIn))
  `(:in
    (:type ,(type-of self)) 
    (:index ,(index self))
    (:name ,(name self))
    (:doc ,(doc self))))

(defmethod omng-save ((self OMMaqIn))
  `(:in
    (:type ,(type-of self)) 
    (:index ,(index self))
    (:name ,(name self))
    (:doc ,(doc self))))

(defmethod omng-save ((self OMOut))
  `(:out
    (:type ,(type-of self)) 
    (:name ,(name self)) 
    (:index ,(index self))
    (:doc ,(doc self))))

(defmethod om-load-from-id ((id (eql :in)) data)
  (let* ((type (find-value-in-kv-list data :type))
         (index (find-value-in-kv-list data :index))
         (name (find-value-in-kv-list data :name))
         (defval (find-value-in-kv-list data :defval))
         (doc (find-value-in-kv-list data :doc))
         (in (make-instance type :name name :defval (omng-load defval) :doc doc)))
    (setf (index in) index)
    in))
 
(defmethod om-load-from-id ((id (eql :out)) data)
  (let* ((type (find-value-in-kv-list data :type))
         (index (find-value-in-kv-list data :index))
         (name (find-value-in-kv-list data :name))
         (doc (find-value-in-kv-list data :doc))
         (out (make-instance type :name name :doc doc)))
    (setf (index out) index)
    out))

;;;=============
; BOXES
;;;=============

(defmethod box-type ((self OMBox)) :unknown)
(defmethod box-type ((self OMValueBox)) :value)
(defmethod box-type ((self OMFunBoxCall)) :function)
(defmethod box-type ((self OMBoxEditCall)) :object)
(defmethod box-type ((self OMSlotsBox)) :slots)
(defmethod box-type ((self OMInOutBox)) :io)
(defmethod box-type ((self OMBoxPatch)) :patch)
(defmethod box-type ((self OMBoxLisp)) :textfun)
(defmethod box-type ((self OMInterfaceBox)) :interface)
(defmethod box-type ((self OMPatchComponentBox)) :special)


(defmethod save-box-reference ((self OMBox)) 
  (omng-save (reference self)))

(defmethod save-box-reference ((self OMBoxAbstraction)) 
   (if (and (is-persistant (reference self))
            (find-persistant-container self))
       (omng-save-relative (reference self) (mypathname (find-persistant-container self)))
     (call-next-method)))


;;; THE CASE WHERE THE REFERENCE OBJECT IS FROM A LIBRARY
(defmethod save-box-library ((self t)) nil)

(defmethod save-box-library ((self OMGFBoxcall))
  (get-ref-library (fdefinition (reference self))))

(defmethod save-box-library ((self OMBoxEditCall))
  (get-ref-library (find-class (reference self))))

(defmethod get-ref-library ((self t)) nil)
(defmethod get-ref-library ((self OMGenericFunction)) (library self))
(defmethod get-ref-library ((self OMClass)) (library self))


;;; save the outputs only if some are reactive,
;;; otherwise it is not useful in the general case
;;; update : now it is useful if the reference is lost
(defmethod save-outputs? ((self OMBox)) 
  ;(find t (outputs self) :key 'reactive)
  t)

(defmethod save-state ((i box-input))
  (cond ((subtypep (type-of i) 'box-optional-input) 
         `(:input (:type :optional) (:name ,(name i)) (:value ,(omng-save (value i))) (:reactive ,(reactive i))))
        ((subtypep (type-of i) 'box-keyword-input) 
         `(:input (:type :key) (:name ,(name i)) (:value ,(omng-save (value i))) (:reactive ,(reactive i))))
        (t 
         `(:input (:type :standard) (:name ,(name i)) (:value ,(omng-save (value i))) (:reactive ,(reactive i))))
        ))

(defmethod save-state ((o box-output)) 
  `(:output (:name ,(name o)) (:reactive ,(reactive o))))

(defmethod omng-save ((self OMBox))  
  (cons :box
        (append 
         
         (and (save-box-library self) `((:library ,(save-box-library self))))
         
         `((:reference ,(save-box-reference self))
           (:type ,(box-type self))
           (:group-id ,(group-id self))
           (:name ,(name self))
           (:x ,(box-x self)) (:y ,(box-y self))
           (:w ,(box-w self)) (:h ,(box-h self)))
  
         (remove nil (mapcar #'(lambda (prop)
                                 (when (slot-exists-p self (nth 3 prop))
                                   (list (car prop) (omng-save (slot-value self (nth 3 prop))))))
                             (get-flat-properties-list self)))
         
         `((:inputs ,.(mapcar #'(lambda (i) (save-state i)) (inputs self))))
         
         (when (save-outputs? self)
           `((:outputs ,.(mapcar #'(lambda (o) (save-state o)) (outputs self)))))
         )))

(defmethod restore-inputs ((self OMBox) inputs)
  (ignore-errors 
    (loop for input-desc in inputs do
          (let ((type (find-value-in-kv-list (cdr input-desc) :type))
                (name (find-value-in-kv-list (cdr input-desc) :name))
                (val (find-value-in-kv-list (cdr input-desc) :value))
                (reac (find-value-in-kv-list (cdr input-desc) :reactive)))
            (case type
              (:standard (let ((in (find name (inputs self) :test 'string-equal :key 'name)))
                           (if in 
                               (setf (value in) (omng-load val) (reactive in) reac)
                             (om-print-dbg "input ~s not found on box ~A" (list name (name self)) "restore-inputs"))
                           ))
              (:optional (more-optional-input self :name name :value (omng-load val) :reactive reac))
              (:key (more-keyword-input self :key name :value (omng-load val) :reactive reac))
              )))))

(defmethod restore-outputs ((self OMBox) outputs)
  (loop for output-desc in outputs do
        (let* ((name (find-value-in-kv-list (cdr output-desc) :name))
               (reac (find-value-in-kv-list (cdr output-desc) :reactive))
               (out (find name (outputs self) :test 'string-equal :key 'name)))
          (if out
              (setf (reactive out) reac)
            (om-print-dbg "output ~s not found on box ~A" (list name (name self)) "restore-outputs"))
          )))

(defmethod save-value ((self OMBox))
  `(:value ,(omng-save (car (value self)))))

;;; called for instance in the maquette
(defmethod omng-save-with-value ((self OMBox)) 
  (append (omng-save self)
          (unless (lambda-state self)
            (list (save-value self)))))

;;; OMBoxEditCall always save the value
;;; + it is a subclass of objectWithEditor (save the editor size and pos)
(defmethod omng-save ((self OMBoxEditCall))  
  (append (call-next-method)
          (append 
           `((:window 
              (:size ,(when (window-size self)
                        (list (om-point-x (window-size self)) (om-point-y (window-size self)))))  
              (:position ,(when (window-pos self)
                            (list (om-point-x (window-pos self)) (om-point-y (window-pos self)))))
              )
             (:edition-params ,.(mapcar 
                                 #'(lambda (p) `(,(car p) ,(omng-save (cadr p))))
                                 (edition-params self))))
           (unless (lambda-state self) (list (save-value self))))))
  

;;; OMValueBox always save the value
(defmethod omng-save ((self OMValueBox))  
  (append (call-next-method)
          (list (save-value self))))

;(defmethod omng-save ((self OMInOutBox))
;  (append (call-next-method)
;          `((:index ,(index self))
;            (:doc ,(doc self)))))
;(defmethod omng-save ((self OMInBox)) (call-next-method))  
;  (append (call-next-method)
;          `((:defval ,(omng-save (defval self))))))


(defmethod om-load-from-id ((id (eql :box)) data)
  ;; (print (list "load BOX" (find-value-in-kv-list data :type)))
  (let* ((type (find-value-in-kv-list data :type))
         (reference (omng-load (find-value-in-kv-list data :reference)))
         (x (find-value-in-kv-list data :x))
         (y (find-value-in-kv-list data :y))
         (w (find-value-in-kv-list data :w))
         (h (find-value-in-kv-list data :h))
         (name (find-value-in-kv-list data :name))
         (pos (omp x y))
         (size (and (or w h) (omp w h)))
         (group-id (find-value-in-kv-list data :group-id))
         (val (omng-load (find-value-in-kv-list data :value)))
         (inputs (find-values-in-prop-list data :inputs))
         (outputs (find-values-in-prop-list data :outputs))
         (box (case type
                
                (:value (omng-make-new-boxcall 'value pos val))
                
                (:function 
                 (if (fboundp reference) 
                     (omng-make-new-boxcall (fdefinition reference) pos)
                   (progn (om-beep-msg "unknown function: ~A" reference)
                     (omng-make-lost-fun-box reference pos)
                     )))
                
                (:patch 
                 (let ((box (omng-make-new-boxcall (omng-load reference) pos)))
                   ;; sometimes (e.g. in maquettes) the patches save their value
                   (when box (setf (value box) (list val)))
                   box))
                
                (:textfun (omng-make-new-boxcall (omng-load reference) pos))
                
                (:io (omng-make-new-boxcall (omng-load reference) pos))
                
                (:special 
                 (let ((box (omng-make-new-boxcall (omng-load reference) pos)))
                   (set-value box (list val))
                   box))
                
                (:interface 
                 (let ((box (omNG-make-special-box reference pos)))
                   (set-value box (list val))
                   box))
                
                (:object 
                 (if (find-class reference nil) 
                             (omng-make-new-boxcall (find-class reference nil) pos val)
                           (progn (om-beep-msg "unknown class: ~A" reference)
                             (omng-make-lost-class-box reference pos))))
                
                (:slots 
                 (if (find-class reference nil) 
                     (omng-make-new-boxcall 'slots pos (find-class reference nil))
                   (progn (om-beep-msg "unknown class: ~A" reference)
                     (omng-make-lost-slots-box reference pos))))
                
                (otherwise (om-beep-msg "unknown box type: ~A" type))))) ;;; DO SOMETHING FOR UNKNOWN BOX ID (kind of 'dead boxes')
    
    (when box
    
      (load-box-attributes box data)
      
      
      (when inputs (restore-inputs box inputs))
      (when outputs (restore-outputs box outputs))
      (when name (set-name box name))
      (when group-id (setf (group-id box) group-id))
      
      (loop for property in (get-flat-properties-list box)
            do (let ((prop (find-value-in-kv-list data (car property))))
                 (when prop (set-property box (car property) (omng-load prop)))))
      
      ;;; some properties (e.g. icon-pos) can modify the size of the box: better do it at the end
      (when size (setf (box-w box) (om-point-x size)
                       (box-h box) (om-point-y size)))
      )
    box))


(defmethod load-box-attributes ((box t) data) nil)

(defmethod load-box-attributes ((box OMBoxEditCall) data)
  (let ((edwin-info (find-values-in-prop-list data :window))
        (ed-params (find-values-in-prop-list data :edition-params)))
         
    (when edwin-info
      (let ((wsize (find-value-in-kv-list edwin-info :size))
            (wpos (find-value-in-kv-list edwin-info :position)))
        (setf (window-pos box) (omp (car wpos) (cadr wpos))
              (window-size box) (omp (car wsize) (cadr wsize)))))
    (when ed-params 
      (setf (edition-params box) 
            (mapcar #'(lambda (p)
                        (list (car p) (omng-load (cadr p))))
                    ed-params)))
    box))


;;;=============
; COMMENTS
;;;=============

(defmethod omng-save ((self OMComment))  
  (cons :comment
    (append
     `((:x ,(box-x self)) (:y ,(box-y self))
       (:w ,(box-w self)) (:h ,(box-h self)))
     (mapcar #'(lambda (prop)
                 (list (car prop) (omng-save (slot-value self (nth 3 prop)))))
             (get-flat-properties-list self))
     `((:text ,(value self))))))

(defmethod om-load-from-id ((id (eql :comment)) data)
  (let* ((x (find-value-in-kv-list data :x))
         (y (find-value-in-kv-list data :y))
         (w (find-value-in-kv-list data :w))
         (h (find-value-in-kv-list data :h))
         (pos (omp x y))
         (size (and (or w h) (omp w h)))
         (text (omng-load (find-value-in-kv-list data :text)))
         (comment (omng-make-new-comment text pos)))
    (when comment
      (when size (setf (box-w comment) (om-point-x size) (box-h comment) (om-point-y size)))
      (loop for property in (get-flat-properties-list comment)
            do (let ((prop (find-value-in-kv-list data (car property))))
                 (when prop (set-property comment (car property) (omng-load prop)))))
      )
    comment))


;;;=============
; PACKAGE
;;;=============

(defmethod om-load-from-id ((id (eql :package)) data)
  (let* ((name (find-value-in-kv-list data :name))
         (pack (make-instance 'OMPackage :name (or name "Untitled Package"))))
    (mapc #'(lambda (class) (addclass2pack class pack)) (find-values-in-prop-list data :classes))
    (mapc #'(lambda (fun) (addFun2Pack fun pack)) (find-values-in-prop-list data :functions))
    (mapc #'(lambda (spk) 
              (let ((sub-pack (omng-load spk)))
                (addpackage2pack sub-pack pack)))
          (find-values-in-prop-list data :packages))

    pack))


