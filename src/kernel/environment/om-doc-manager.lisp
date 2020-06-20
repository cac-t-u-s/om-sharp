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

(defparameter *last-open-dir*  nil)
(defvar *quit-at-last-doc* nil "If T, prevents quitting when the last open document is closed")

(defstruct doc-entry (doc) (file))
(defparameter *open-documents*  nil)


(defmethod find-doc-entry ((doc pathname))
  (find (namestring doc) *open-documents* :key 'doc-entry-file :test 'string-equal))
(defmethod find-doc-entry ((doc string))
  (find doc *open-documents* :key 'doc-entry-file :test 'string-equal))
(defmethod find-doc-entry ((doc OMPersistantObject))
  (find doc *open-documents* :key 'doc-entry-doc))

;;; the object does not need to be registered
(defmethod register-document ((self OMObject) &optional path) nil)
(defmethod unregister-document ((self OMObject)) nil)

(defmethod register-document ((self OMPersistantObject) &optional path)
  (unless (find-doc-entry self)
    (om-print-dbg "Registering document:  ~A (~A / ~A)" (list (name self) self (mypathname self)))
    (push (make-doc-entry :doc self :file (and path (namestring path))) *open-documents*)))

(defmethod unregister-document ((self OMPersistantObject))
  (when (find-doc-entry self)
    (om-print-dbg "Unregistering document: ~A (~A / ~A)" (list (name self) self (mypathname self)))
    (setf *open-documents* (remove self *open-documents* :key 'doc-entry-doc)))
  (when (and (null *open-documents*) *quit-at-last-doc*
             (member :om-deliver *features*))
    (om-quit)))

(defmethod update-document-path ((self OMPersistantObject))
  (let ((doc-entry (find self *open-documents* :key 'doc-entry-doc)))
    (if doc-entry 
        (setf (doc-entry-file doc-entry) (and (mypathname self) (namestring (mypathname self))))
      (om-print-dbg "Patch ~A was not registered / path can not be updated" (list self)))))

(defmethod update-create-info ((self OMPersistantObject))
  (setf (cdr (create-info self)) 
        (list (om-get-date) *app-name* *version*)))


(defun print-documents ()
  
  (let ((str (format nil "DOCUMENTS~%")))
    (loop for doc in *open-documents*
          do (setf str (string+ str (format nil "- ~A (~A) ~A reference(s)" 
                                            (name (doc-entry-doc doc)) 
                                            (doc-entry-file doc) 
                                            (length (references-to (doc-entry-doc doc))))))
          (when (editor-window (doc-entry-doc doc))
            (setf str (string+ str " (incl. EDITOR)")))
          
          (setf str (string+ str (format nil "~%")))
          
          (loop for ref in (references-to (doc-entry-doc doc))
                do 
                (setf str (string+ str (format nil "--- ~A (~A)~%" 
                                               ref
                                               (if (subtypep (type-of ref) 'OMBox) (list (name ref) "in" (container ref))
                                                 "EDITOR")
                                               ))))
          )
    (print str)))
           
; (print-documents)
                                    
;;;==================================================================
;;;==================================================================
;;;==================================================================
     
               
(defvar *doctypes*
  '((:patch "opat" "Patch")
    (:lisp ("lisp" "lsp") "Lisp File")
    (:text "txt" "Text File")
    (:om ("opat") "OM# Documents")))

(defvar *om-doctypes* '(:patch))

;;; used to add doc types for Lisp functions, maquette...
(defun add-om-doctype (type ext desc)
  (setf *om-doctypes* (append *om-doctypes* (list type)))
  
  (setf *doctypes* (cons (car *doctypes*)
                         (cons (list type ext desc)
                               (cdr *doctypes*))))
                         
  (let ((om-entry-pos (position :om *doctypes* :key #'car)))
    (setf (nth 1 (nth om-entry-pos *doctypes*))
          (append (nth 1 (nth om-entry-pos *doctypes*)) (list ext))))
  )


(defun doctype-name (symb) 
  (caddr (find symb *doctypes* :key #'car)))

(defun extension-to-doctype (str)
  (car 
   (find-if #'(lambda (elt) 
                (cond ((listp (cadr elt))
                       (find str (cadr elt) :test #'string-equal))
                      ((stringp (cadr elt))
                       (string-equal str (cadr elt)))
                      (t nil)))
            *doctypes*)))

(defun doctype-to-extension (symb)
  (let ((ext (cadr (find symb *doctypes* :key #'car))))
    (if (listp ext) (car ext) ext)))

(defun doctype-to-ext-list (symb)
  (list! (cadr (find symb *doctypes* :key #'car))))

(defun doctype-info (symb) 
  (list (doctype-name symb)
        (format nil "~{~a~^;~}" 
                (loop for ext in (doctype-to-ext-list symb) 
                      collect (string+ "*." ext)))
        ))


(defmethod make-new-om-doc (type name)
  (om-beep-msg "Document type ~S unknown." type))

;;; called by the interface menus and commands ("New")
(defun open-new-document (&optional (type :patch)) 
  (let ((newobj (make-new-om-doc type (om-str :untitled))))
    (setf (create-info newobj) (list (om-get-date) (om-get-date) *app-name* *version*))
    (setf (saved? newobj) t)
    (register-document newobj)
    (open-editor newobj)
    newobj))

;;; secure version... (still needed ?)
;(defun open-om-document (&optional path)
;  (if path (open-document-from-file path)
;    (capi::with-dialog-results (file ok-p)
;        (capi::prompt-for-file (string+ (om-str :open) "...")
;                               :pathname (or *last-open-dir* (om-user-home))
;                               :filters (list "Patch" "*.omp") :filter "*.omp")
;      (when file
;        (open-document-from-file file)))))
        
(defvar *om-recent-files* nil)

(defun record-recent-file (file)

  (if (find (namestring file) *om-recent-files* :key 'namestring :test 'string-equal)
      (setf *om-recent-files*
            (cons file (remove (namestring file) *om-recent-files* :key 'namestring :test 'string-equal)))
    (setf *om-recent-files*
          (cons file (first-n *om-recent-files* 9)))
    )
  ;;; to store the list of documents...
  (save-om-preferences))
      

;;; called from the menu ("Open")
(defun open-om-document (&optional path (record t))
  (let ((file (or path 
                  (om-choose-file-dialog :prompt (string+ (om-str :open) "...")
                                         :directory (or *last-open-dir* (om-user-home))
                                         :types (append
                                                 (doctype-info :om)
                                                 (loop for type in *om-doctypes* append (doctype-info type))
                                                 (doctype-info :lisp) (doctype-info :text)
                                                 (doctype-info :old)
                                                 '("All documents" "*.*"))))))
    (when file
      (when record 
        (setf *last-open-dir* (om-make-pathname :directory file))
        (record-recent-file file))
      (let ((type (extension-to-doctype (pathname-type file))))
        (cond ((find type *om-doctypes*)
               (open-doc-from-file type file))
              ((find type '(:text :lisp))
               (om-lisp::om-open-text-editor :contents file :lisp t))
              ((equal type :old) 
               (import-doc-from-previous-om file))
              (t (om-message-dialog (format nil "Unknown document type: ~s" (pathname-type file)))
                 nil))
        ))))


(defmethod type-check ((type (eql :patch)) obj)
  (let ((patch (ensure-type obj 'OMPatch)))
    (when patch
      (change-class patch 'OMPatchFile)
      (setf (icon patch) :patch-file)
      patch)))

(defmethod type-check ((type t) obj) nil)


(defun load-doc-from-file (path type)
  
  (om-print-format "Opening document: ~A" (list path))
  
  (let ((doc-entry (find-doc-entry path)))
    (if doc-entry 
      
        (progn
          (om-print-dbg "Document found in register: ~A" (list (doc-entry-doc doc-entry)))
          (doc-entry-doc doc-entry))
      
      (let ((*package* (find-package :om)))
      
        (with-relative-ref-path path
         
          (let* ((file-contents (car (list-from-file path)))
                 (doc (omng-load (list (car file-contents)))) ;; load just the object type (no contents)
                 (object (type-check type doc))) ;; will change the class of object to persistant
           
            (if object
             
                (progn 
                  (setf (mypathname object) path
                        (name object) (pathname-name path)
                        (loaded? object) nil
                        (saved? object) t)
                  
                  (register-document object path)
                  (load-patch-contents object (cdr file-contents))
                  (update-document-path object) ;;; just to update create/modif info display
                  )
             
              (om-beep-msg "Document ~s of type ~S could not be loaded." path type))
           
            object)
          )
        )
      )))
      

(defun import-doc-from-previous-om (path)
  (let ((obj (load-om6-file path)))
    (if obj (open-editor obj)
      (om-print (string+ "file: \""  (namestring path) "\" could not be open.")))
    obj))


(defun open-doc-from-file (type file)
  (when file
    (if (not (probe-file file))
        (om-message-dialog (format nil (om-str :file-not-exists) (namestring file)))
      (om-with-error-handle    
        (let ((obj (load-doc-from-file file type)))
          (if obj (open-editor obj)
            (om-print (string+ "file: \""  (namestring file) "\" could not be open.")))
          obj)
        ))))



(defmethod prepare-save-as ((self OMPersistantObject))
  (let ((path (om-choose-new-file-dialog :prompt (om-str :save-as) 
                                         :name (name self)
                                         :directory (or *last-open-dir* (om-user-home))
                                         :types (doctype-info (object-doctype self)))))
    (when path 

      ;; add default pathname-type if not specified
      (unless (pathname-type path)
      	(let ((type (doctype-to-extension (object-doctype self))))
      	  (setf path (merge-pathnames (make-pathname :type type) path))))

      (setf *last-open-dir* (om-make-pathname :directory path))
      (if (find-doc-entry path)
          (progn (om-message-dialog 
           (format nil "An open document named ~S already exist in this folder.~%Please choose another name or location." 
                   (pathname-name path)))
            (prepare-save-as self))
        (progn 
          (setf (mypathname self) path)
          (set-name self (pathname-name path))
          (update-document-path self)
          (record-recent-file path)
          )))
    ))
  

(defmethod save-document ((self OMPersistantObject))
  
  (unless (mypathname self)
    (prepare-save-as self))
  
  (when (mypathname self)
    (let ((tempfile (om-make-pathname :directory (mypathname self) 
                                      :name (pathname-name (mypathname self))
                                      :type (string+ (pathname-type (mypathname self)) ".tmp"))))
      
      (update-create-info self) ;;; modif date is 'now'
      (with-open-file (out tempfile :direction :output  
                           :if-does-not-exist :create :if-exists :supersede)
        (handler-bind 
            ((error #'(lambda (err)
                        (om-message-dialog (format nil "An error of type ~a occurred: ~a~%~%File ~s could not be saved." 
                                                   (type-of err) (format nil "~A" err) (mypathname self)))
                        (close out))))

          (let ((*package* (find-package :om)))
            (with-relative-ref-path (mypathname self)
              (pprint (save-patch-contents self) out)))
          ))
      
      (when (probe-file tempfile)
        (rename-file tempfile (mypathname self))
        (om-print-format "Document saved on disk: ~A - ~A" (list self (mypathname self)))
        (setf (saved? self) t)))))



;;; T = OK (close)
(defmethod ask-save-before-close ((self t)) t)

(defvar *no-check* nil)
(defmacro with-no-check (&body body)
  `(let ((prev-check *no-check*))
     (setf *no-check* t) 
     (let ((rep ,@body))
       (setf *no-check* prev-check)
       rep)))
     
(defvar *save-apply-all* nil)

;;; Called autiomatically when the document is closed
;;; Must return T or NIL depending on wether we can actually close it or not
(defmethod ask-save-before-close ((self OMPersistantObject))
  (or 
   
   *no-check*  
   (equal *save-apply-all* :no)
   (saved? self)
                 
   (if (equal *save-apply-all* :yes)
                     
       (progn 
         (save-document self)
         ;;; return this: will be T if save went ok
         (and (mypathname self) (saved? self)))
                       
     (let ((rep (om-save-y-n-cancel-dialog (name self))))
                     
       (when (and rep (cadr rep)) ;;; dialog exited with yes or no, and the apply-all box was checked
         (setf *save-apply-all* (if (car rep) :yes :no)))
                     
       (cond 
        ((equal rep nil) nil)  ;;; cancel : do not close
        ((and (consp rep) (equal (car rep) nil)) t)  ;; No : do not save and close
        ((and (consp rep) (equal (car rep) t))   ;; Yes : save and close if save is OK
         (save-document self)
         (and (mypathname self) (saved? self)))
        (t nil))
       ))
   ))


;;; Called when application quits
(defun check-om-docs-before-close ()
  (setf *save-apply-all* nil)
  (let ((ok t))
    (loop for doc-entry in *open-documents* 
          while ok do
          (unless (and *save-apply-all* (equal *save-apply-all* :no)) 
            (let ((doc (doc-entry-doc doc-entry)))
            (when (null (saved? doc))
              (let ((win (editor-window doc)))
                (when win (om-select-window win))
                (when (or (and *save-apply-all* (equal *save-apply-all* :yes))
                          (let ((rep (om-save-y-n-cancel-dialog (name doc))))
                            (if rep 
                                (when (cadr rep)
                                  (setf *save-apply-all* (if (car rep) :yes :no)))
                              (setf ok nil))
                            (car rep)))
                  (or (prog1 (save-document doc)
                        (and win (om-set-window-title win (window-name-from-object doc))))
                      (setf ok nil)))))
            )
            ))
    ok))


; (check-om-docs-before-close)
; (om-save-y-n-cancel-dialog "Test")

(defun om-save-y-n-cancel-dialog (name)
  (let* ((y-grid 24)
         (win (om-make-window 'om-dialog :position :centered 
                              :resizable t :maximize nil :minimize nil :owner nil
                              :title ""))
         (box (om-make-di 'om-check-box
                          :size (om-make-point 200 y-grid)
                          :text (om-str :apply-all)
                          :font (om-def-font :gui))))
    
    (om-add-subviews 
     win
     (om-make-layout 
      'om-column-layout 
      :subviews
      (list 
       (om-make-di 'om-simple-text 
                   :size (om-make-point 330 y-grid)
                   :text (format nil (om-str :save-changes-in) name))
            
       (om-make-layout 
        'om-row-layout 
        :subviews
        (list NIL
              (om-make-di 'om-button
                          :size (om-make-point 80 y-grid)
                          :text (om-str :no)
                          :di-action #'(lambda (item) (declare (ignore item))
                                         (om-return-from-modal-dialog win (list nil (om-checked-p box)))))
              (om-make-di 'om-button
                          :size (om-make-point 80 y-grid)
                          :text (om-str :yes)
                          :default t
                          :di-action #'(lambda (item) (declare (ignore item))
                                         (om-return-from-modal-dialog win (list t (om-checked-p box)))))))
            
       (om-make-layout 
        'om-row-layout 
        :subviews
        (list 
         NIL
         box
         (om-make-di 'om-button
                     :size (om-make-point 80 y-grid)
                     :text (om-str :cancel)
                     :di-action #'(lambda (item) (declare (ignore item))
                                    (om-return-from-modal-dialog win nil)))
         )))
      ))
    
    (om-modal-dialog win)
    ))
