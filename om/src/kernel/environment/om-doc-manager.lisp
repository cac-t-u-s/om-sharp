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
  (om-print-dbg "Registering document: ~A - ~A" (list self (mypathname self)))
  (push (make-doc-entry :doc self :file (and path (namestring path))) *open-documents*))

(defmethod unregister-document ((self OMPersistantObject))
  (om-print-dbg "Unregistering document: ~A - ~A" (list self (mypathname self)))
  (let ((doc-entry (find self *open-documents* :key 'doc-entry-doc)))
    (setf *open-documents* (remove self *open-documents* :key 'doc-entry-doc))
    (when (and (null *open-documents*) *quit-at-last-doc*
               (member :om-deliver *features*))
      (om-quit))))

(defmethod update-document-path ((self OMPersistantObject))
  (let ((doc-entry (find self *open-documents* :key 'doc-entry-doc)))
    (if doc-entry 
        (setf (doc-entry-file doc-entry) (namestring (mypathname self)))
      (om-beep-msg "Problem: patch ~A was not registered!" self))))

(defmethod update-create-info ((self OMPersistantObject))
  (setf (cadr (create-info self)) (om-get-date)))

#|
;------------------------------------------------------------------------------
; HANDLING PATCH DEPENDENCIES
;------------------------------------------------------------------------------
(defmethod register-editor ((self OMPersistantObject))
  (let ((doc-entry (find self *open-documents* :key 'doc-entry-doc)))
    (if doc-entry
        (setf (doc-entry-editor doc-entry) t)
      (om-beep-msg "Problem: patch ~A was not registered!" self))))

(defmethod unregister-editor ((self OMPersistantObject))
  (let ((doc-entry (find self *open-documents* :key 'doc-entry-doc)))
    (if doc-entry 
        (progn 
          (setf (doc-entry-editor doc-entry) nil)
          (when (<= (length (references-to self)) 0)
            (unregister-document self)))
      (om-beep-msg "Problem: patch ~A was not registered!" self)
      )))

;;; A new dependency link is created
(defmethod register-dependency ((self OMPatch) (sub-patch OMPatch))
  (unless (find sub-patch (dependencies self))
    (push sub-patch (dependencies self))))
|#



;;;==================================================================
;;;==================================================================
;;;==================================================================
                    
(defmethod make-new-om-doc (type name)
  (om-beep-msg "Document type ~S unknown." type))

(defmethod doctype-info ((type (eql :patch))) '("OM Patch" "*.opat"))
(defmethod doctype-info ((type (eql :maquette))) '("OM Maquette" "*.omaq"))
(defmethod doctype-info ((type (eql :textfun))) '("OM Text (Lisp) Function" "*.olsp"))

(defmethod doctype-info ((type (eql :om)))
  (list "OM Documents" (string+ (cadr (doctype-info :patch)) ";" 
                                (cadr (doctype-info :maquette)) ";"
                                (cadr (doctype-info :textfun))
                                )))

(defun extension-to-doctype (str)
  (cond ((string-equal str "opat") :patch)
        ((string-equal str "omaq") :maquette)
        ((string-equal str "olsp") :textfun)
        ((or (string-equal str "lisp")
             (string-equal str "lsp")) :lisp)
        ((string-equal str "txt") :text)
        (otherwise nil)))
        
;;; called by the interface menus and commands ("New")
(defun open-new-document (&optional (type :patch)) 
  (let ((newobj (make-new-om-doc type (om-str :untitled))))
    (setf (omversion newobj) *om-version*)
    (setf (create-info newobj) (list (om-get-date) (om-get-date)))
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
;                               :filters (list "OM Patch" "*.omp") :filter "*.omp")
;      (when file
;        (open-document-from-file file)))))
        
(defvar *om-recent-files* nil)

(defun record-recent-file (file)

  (if (find (namestring file) *om-recent-files* :key 'namestring :test 'string-equal)
      (setf *om-recent-files*
            (cons file (remove (namestring file) *om-recent-files* :key 'namestring :test 'string-equal)))
    (setf *om-recent-files*
          (cons file (first-n *om-recent-files* 5)))
    )
  ;;; to store the list of documents...
  (save-om-preferences))
      

;;; called from the menu ("Open")
(defun open-om-document (&optional path)
  (let ((file (or path 
                  (om-choose-file-dialog :prompt (string+ (om-str :open) "...")
                                         :directory (or *last-open-dir* (om-user-home))
                                         :types (append (append 
                                                         (doctype-info :om)
                                                     (doctype-info :patch) (doctype-info :maquette) (doctype-info :textfun))
                                                        '("Text File" "*.txt" "Lisp File" "*.lisp;*.lsp" "All documents" "*.*"))))))
    (when file
      (record-recent-file file)
      (let ((type (extension-to-doctype (pathname-type file))))
        (case type
          (:patch (open-doc-from-file type file))
          (:maquette (open-doc-from-file type file))
          (:textfun (open-doc-from-file type file))
          ((or :text :lisp) (om-lisp::om-open-text-editor :contents file :lisp t))
          (otherwise (progn (om-message-dialog (format nil "Unknown document type: ~s" (pathname-type file)))
                        nil)))
        ))))


(defmethod type-check ((type (eql :patch)) obj)
  (let ((patch (ensure-type obj 'OMPatch)))
    (when patch
      (change-class patch 'OMPatchFile)
      (setf (icon patch) :patch-file)
      patch)))

(defmethod type-check ((type (eql :maquette)) obj)
  (let ((maq (ensure-type obj 'OMMaquette)))
    (when maq
      (change-class maq 'OMMaquetteFile)
      (setf (icon maq) :maq-file))
    maq))

(defmethod type-check ((type (eql :textfun)) obj)
  (let ((fun (ensure-type obj 'OMLispFunction)))
    (when fun
      (change-class fun 'OMLispFunctionFile)
      (setf (icon fun) :lisp-f-file))
    fun))

    
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
                 )
             
             (om-beep-msg "Document ~s of type ~S could not be loaded." path type))
           
           object)
         )
       )
     )))
      

(defun open-doc-from-file (type &optional path)
  (let ((file (or path (om-choose-file-dialog 
                        :prompt (string+ (om-str :open) "...") :types (doctype-info type)
                        :directory (or *last-open-dir* (om-user-home))))))
    (when file
      (if (not (probe-file file))
          (om-message-dialog (format nil (om-str :file-not-exists) (namestring file)))
        (om-with-error-handle    
          (setf *last-open-dir* (pathname-dir file))
          (let ((obj (load-doc-from-file file type)))
            (if obj (open-editor obj)
              (om-print (string+ "file: \""  (namestring file) "\" could not be open.")))
            obj)
          )))))

(defmethod prepare-save-as ((self OMPersistantObject))
  (let ((path (om-choose-new-file-dialog :prompt (om-str :save-as)
                                         :directory (or *last-open-dir* (om-user-home))
                                         :types (doctype-info (object-doctype self)))))
    (when path 
      (setf *last-open-dir* (pathname-dir path))
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
  


;;; T = OK (close)
(defmethod ask-save-before-close ((self t)) t)

(defvar *no-check* nil)
(defmacro with-no-check (&body body)
  `(let ((prev-check *no-check*))
     (setf *no-check* t) 
     (let ((rep ,@body))
       (setf *no-check* nil)
       rep)))
     

(defmethod ask-save-before-close ((self OMPersistantObject))
  (let ((close? (or *no-check* 
                    (saved? self)
                    (let ((rep (om-save-y-n-cancel-dialog (name self))))
                      (cond 
                       ((equal rep nil) nil)  ;;; cancel : do not close
                       ((and (consp rep) (equal (car rep) nil)) t)  ;; No : do not save and close
                       ((and (consp rep) (equal (car rep) t))   ;; Yes : save and close if save is OK
                        (save-document self)
                        (and (mypathname self) (saved? self)))
                       (t nil))))))
    close?))


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

          (let ((*package* (find-package :om))
                (patch-contents (save-patch-contents self)))
            (pprint patch-contents out))))
      
      (when (probe-file tempfile)
        (rename-file tempfile (mypathname self))
        (om-print-format "Document saved on disk: ~A - ~A" (list self (mypathname self)))
        (setf (saved? self) t)))))

(defvar *save-apply-all* nil)

(defun check-om-docs-before-close ()
  (setf *save-apply-all* nil)
  (let ((ok t))
    (loop for doc-entry in *open-documents* 
          while ok do
          (unless (and *save-apply-all* (= *save-apply-all* 0)) 
            (let ((doc (doc-entry-doc doc-entry)))
            (when (null (saved? doc))
              (let ((win (window (editor doc))))
                (om-select-window win)
                (when (or (and *save-apply-all* (= *save-apply-all* 1))
                          (let ((rep (om-save-y-n-cancel-dialog (name doc))))
                            (if rep 
                                (when (cadr rep)
                                  (setf *save-apply-all* (if (car rep) 1 0)))
                              (setf ok nil))
                            (car rep)))
                  (or (prog1 (save-document doc)
                        (om-set-window-title win (window-name-from-object doc)))
                      (setf ok nil)))))
            )
            ))
    ok))


; (check-om-docs-before-close)
; (om-save-y-n-cancel-dialog "Test")

(defun om-save-y-n-cancel-dialog (name)
  (let* ((v (om-make-view 'om-view :bg-color (om-def-color :transparent)))
         (win (om-make-window 'om-dialog :position :centered :size (om-make-point 330 120)
                              :resizable nil :maximize nil :minimize nil :owner nil
                              :win-layout (om-make-layout 'om-simple-layout :ratios '(1) :delta 20 :align :center)
                              :title ""
                              :subviews (list v)))
         (text (om-make-di 'om-simple-text :position (om-make-point 25 15) :size (om-make-point 280 50)
                           :text (format nil (om-str :save-changes-in) name)
                           ;:font (om-def-font :font1)
                           ))
         (box (om-make-di 'om-check-box :position (om-make-point 150 80) :size (om-make-point 200 25)
                                   :text (om-str :apply-all)
                                   :font (om-def-font :gui)
                                   )))
      
    (om-add-subviews v 
                     text 
                     box
                     (om-make-di 'om-button :position (om-make-point 20 55) :size (om-make-point 80 26) :text (om-str :cancel)
                                 :di-action (om-dialog-item-act item
                                              (om-return-from-modal-dialog win nil)))
                     
                     (om-make-di 'om-button :position (om-make-point 145 55) :size (om-make-point 80 26) :text (om-str :no)
                                 :di-action (om-dialog-item-act item
                                              (om-return-from-modal-dialog win (list nil (om-checked-p box)))))
                     
                     (om-make-di 'om-button :position (om-make-point 230 55) :size (om-make-point 80 26) :text (om-str :yes)
                                 :default-button t
                                 :di-action (om-dialog-item-act item
                                              (om-return-from-modal-dialog win (list t (om-checked-p box)))))
                     )
    (om-modal-dialog win)
    ;(om-open-window win)
    ))



