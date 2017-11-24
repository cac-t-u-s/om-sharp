;============================================================================
; o7: visual programming language for computer-aided music composition
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

;;;======================================
;;; GENERATOR OF FUNCTION-REFERENCE PAGES
;;;======================================

(in-package :om)

;;;======================================
;;; FILE MANAGEMENT / UTILS
;;;======================================

(defun get-om-reference-pages-folder ()
  (merge-pathnames 
   (make-pathname :directory '(:relative "reference-pages"))
   (om-resources-folder)))

(defun get-om-reference-pages-index ()
  (merge-pathnames 
   (make-pathname :name "index" :type "html")
   (get-om-reference-pages-folder)))

;;; characters to avoid in file names
(defun special-path-check (name)
  (loop while (setf pos (position #\/ name)) do
        (replace name "I" :start1 pos))
  (loop while (setf pos (position #\\ name)) do
        (replace name "l" :start1 pos))
  (loop while (setf pos (position #\: name)) do
        (replace name "%" :start1 pos))
  (loop while (setf pos (position #\* name)) do
        (replace name "X" :start1 pos))
  (loop while (setf pos (position #\? name)) do
        (replace name "$" :start1 pos))
  (loop while (setf pos (position #\" name)) do
        (replace name "'" :start1 pos))
  (loop while (setf pos (position #\> name)) do
        (replace name ")" :start1 pos))
  (loop while (setf pos (position #\< name)) do
        (replace name "(" :start1 pos))
  (loop while (setf pos (position #\| name)) do
        (replace name "I" :start1 pos))
  name)

;;; characters to avoid in HTML files
(defun special-html-check (name)
  (loop while (setf pos (position #\> name)) do
        (setf name (concatenate 'string (subseq name 0 pos)
                                "&gt;" (subseq name (+ pos 1)))))
  (loop while (setf pos (position #\< name)) do
        (setf name (concatenate 'string (subseq name 0 pos)
                                "&lt;" (subseq name (+ pos 1)))))
  name)


;;;======================================
;;; AUTO GEN ENTRIES FROM PACKAGES
;;;======================================

(defmethod gen-ref-entries ((pack OMAbstractPackage))
  (list (list (name pack) (doc pack))
              (cons
               (list "" (remove nil (remove-duplicates 
                                     (append (mapcar 'class-name (classes pack)) 
                                             (mapcar 'function-name (functions pack))
                                             (special-items pack))
                                     :test 'string-equal)))
               (remove nil (loop for pack in (subpackages pack) append 
                                 (gen-subpack-entries pack)))
               )))

(defmethod gen-subpack-entries ((pack OMAbstractPackage) &key exclude-packages)
  (unless (find (name pack) exclude-packages :test 'string-equal)
    (let ((packentries (remove nil (remove-duplicates 
                                    (append (mapcar 'class-name (classes pack)) 
                                            (mapcar 'function-name (functions pack))
                                            (special-items pack)) 
                                    :test 'string-equal))))
      (remove nil (cons
                   (if packentries (list (list (name pack) (doc pack)) packentries))
                   (loop for pk in (subpackages pack) append (gen-subpack-entries pk :exclude-packages exclude-packages))
                   )))))


;;;======================================
;;; EXTRACT INFO FROM METAOBJECTS
;;;======================================

(defun fun-to-doclist (symbol)
  (let ((fun (fdefinition symbol)))
    (list symbol
          (if (typep fun 'generic-function)
              (setf doc-list "GENERIC-FUNCTION")
            (setf doc-list "FUNCTION"))
          (function-arglist symbol)
          (function-documentation  symbol))))

(defun class-to-doclist (symbol) 
  (let ((class (find-class symbol nil))
        (instance (make-instance symbol)))
    (list symbol "CLASS"
          ;;; slots desc
          (append 
           (loop for slot in (remove-if-not 'slot-initargs (class-direct-instance-slots class))
                 collect 
                 (list (slot-name slot)
                       (eval (slot-initform slot))
                       (slot-doc slot)))
           (when(additional-class-attributes instance)
             (cons "Optional:"
                   (loop for add-slot in (additional-class-attributes instance) 
                         collect
                         (let ((slot (find add-slot (class-slots class) :key 'slot-name)))
                           (list add-slot
                                 (eval (slot-initform slot))
                                 (slot-doc slot))))))
           )
          (class-documentation class))))
  
(defun box-to-doclist (symbol) 
  (let* ((class (find-class (special-item-reference-class symbol)))
         (instance (make-instance (special-item-reference-class symbol))))
    (list symbol "INTERFACE BOX"
          ;;; slots desc
          (loop for k in (flat (get-all-keywords instance)) 
                collect 
                (let ((slot (find (symbol-name k) (class-slots class) 
                                  :key #'(lambda (slot) (symbol-name (slot-name slot))) 
                                  :test 'string-equal)))
                   (list (slot-name slot)
                         (eval (slot-initform slot))
                         (slot-doc slot))))
          (class-documentation class))))

;;;======================================
;;; REFERENCE-PAGES GENERATION AND ACCESS
;;;======================================

(defun get-documentation-info (symbol) 
  (let ((*print-case* :downcase))
    (cond 
     ((fboundp symbol) (fun-to-doclist symbol))
     ((find-class symbol nil) (class-to-doclist symbol))
     ((special-box-p symbol) (box-to-doclist symbol)))
    ))

(defparameter *om-ref-text*
  "This is the reference documentation for the main functions and classes in o7.")

(defun gen-om-reference ()
  (gen-reference (mapcar 'gen-ref-entries (subpackages *om-package-tree*))
                 (get-om-reference-pages-folder)
                 :maintext *om-ref-text*))

(defun ensure-reference-pages ()
  (unless (get-om-reference-pages-index)
    (gen-om-reference)))

;(show-reference-page 'om+)
(defmethod show-reference-page ((symbol symbol))
  (ensure-reference-pages)
  (let ((file (om-make-pathname :directory (get-om-reference-pages-folder)
                                :name (special-path-check (string-downcase (string symbol)))
                                :type "html")))
    (if (probe-file file)
        (sys:open-url (namestring file))
      (om-beep-msg "No reference page found for '~A'" symbol))))

; (gen-om-reference)

;;;======================================
;;; HTML LAYOUT, CONTENTS AND STYLE
;;;======================================

(defparameter *om-ref-css* 
      "<STYLE TYPE=\"text/css\"><!--  

BODY {background-color: #666666;
	  font-family: \"Century Gothic\";
font-size: 14;
color : #0000000;
}

A:link {text-decoration: underline; color : 333366;}
A:visited {text-decoration: underline; color : 333366;}
A:active {text-decoration: underline; color:333366;}
A:hover {text-decoration: underline; color: 333366;}

A:link.dark {text-decoration: underline; color: FFFFFF;}
A:visited.dark {text-decoration: underline; color: FFFFFF;}
A:active.dark {text-decoration: underline; color: FFFFFF;}
A:hover.dark {text-decoration: underline; color: FFFFFF;}

H1 {
 font-size: 22;
 font-weight: bold
 text-align: left;
}
	
H2 {
 color : #333333;
 font-size: 20;
 font-weight: bold;
}

H3 {
 color : #444444;
 font-size: 18;
 font-weight: bold;
}

P,UL,LI,TD {	
 font-size: 15;
 text-indent: 0cm;
 text-align: justify;
}

table {
    display: table;
    border-collapse: separate;
    border-spacing: 0px;
}

.dark { 
  background-color: #222222; 
  color: #FFFFFF;
}
.right {text-align: right ;}
.center {text-align: center ;}
.top {text-align: top ;}

   --></style>")

(defun credits-line ()
  (concatenate 'string 
               "<center><font size=-1>" 
               "Auto doc generation by OM7 "
               (cl-user::version-to-string *om-version*) 
               " - " 
               (om-get-date)  
               "</font></center>"))


; (gen-om-reference)

(defun gen-reference (entries dir &key title maintext)

  (when (probe-file dir) (om-delete-directory dir))
  (om-create-directory dir)
  
  (let ((title (or title (concatenate 'string "o7 " (cl-user::version-to-string *om-version*))))
        (indexpath (om-make-pathname :directory dir
                                  :name "index" :type "html"))
        (alphaindexpath (om-make-pathname :directory dir
                                  :name "ind-alpha" :type "html"))
        (allsymbols (remove nil (loop for pack in entries append
                                      (loop for group in (cadr pack) append (cadr group)))))
        )
    (with-open-file (index indexpath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      
      (write-line "<body bgcolor=BBBBBB>" index)
      (write-line "<table align=center bgcolor=FFFFFF width=85% cellpadding=20>" index)
      
      ;;; TITLE BAR
      (write-line  "<tr>" index)
      (write-line "<td class=\"dark\" colspan=2>" index)
      (write-line (concatenate 'string "<H1>" title " reference pages</H1>") index)
      (write-line "<p style=\"text-align: right;\"><b>Main Index | <a class=\"dark\" href=ind-alpha.html>Alphabetical Index</a></b></p>" index)
      (write-line "</td>" index)
      (write-line  "</tr>" index)
      
      ;;; Intro text and logo
      (write-line "<tr>" index)
      (write-line "<td>" index)
      (when maintext
        (loop for par in (list! maintext) do
              (write-line "<p>" index)
              (write-line par index)
              (write-line "</p>" index)))
      (write-line "</td>" index)
      (write-line (concatenate 'string "<td><img src=./omlogo.png width=60 align=centre></td>") index)
      (write-line "</tr>" index)
      
      ;;; PACKAGE LIST
      (write-line "<tr><td colspan=2>" index)
      (write-line "<p>Packages covered: " index)
      (loop for pack in entries do
            (let ((name (if (consp (car pack)) (first (car pack)) (car pack))))
              (when (and name (not (string-equal name "")))
                (write-line (concatenate 'string "<a href=#" (string name) ">" (string-upcase (string name)) "</a> ") index))))
      (write-line "</td></tr>" index)
      
      ;;; MAIN CONTENTS
      (write-line "<tr><td colspan=2>" index)
      
      (loop for pack in entries do
            (let ((name (if (consp (car pack)) (first (car pack)) (car pack)))
                  (doc (if (consp (car pack)) (second (car pack)) nil)))
              
              (when name
                (write-line (concatenate 'string "<a name=" (string name) ">" "<h2>" (string-upcase (string name)) "</h2></a>") index))
              (when doc
                (write-line (concatenate 'string "<p>" doc "</p>") index))
              (write-line "<blockquote>" index)
              (loop for group in (cadr pack) do
                    (let ((n (if (consp (car group)) (first (car group)) (car group)))
                          (d (if (consp (car group)) (second (car group)) nil)))
                      (when n
                        (write-line (concatenate 'string "<h3>" (string n) "</h3>") index))
                      (when d
                        (write-line (concatenate 'string "<p>" (string d) "</p>") index))
                       (loop for item in (cadr group) do
                            (write-line (concatenate 'string "<a href=" 
                                                     (special-path-check
                                                      (string-downcase (string item))) ".html>" 
                                                     (special-html-check (string item)) "</a><br>") index)
                            )
                       ))
              (write-line "</blockquote>" index)
              ))
      (write-line "<br><br></td></tr><td colspan=2 class=center>" index)
      (write-line (credits-line) index)
      (write-line "</td></tr></table>" index)
      (write-line "</body></html>" index))

    (with-open-file (index alphaindexpath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      
      (write-line "<body bgcolor=BBBBBB>" index)
      (write-line "<table align=center bgcolor=FFFFFF width=85% cellpadding=20>" index)
      
      ;;; TITLE BAR
      (write-line  "<tr>" index)
      (write-line "<td class=\"dark\" colspan=2>" index)
      (write-line (concatenate 'string "<H1>" title " reference pages</H1>") index)
      (write-line "<p style=\"text-align: right;\"><b><a class=\"dark\" href=index.html>Main Index</a> | Alphabetical Index</b></p>" index)
      (write-line "</td>" index)
      (write-line  "</tr>" index)
      
      (write-line "<tr><td colspan=2>" index)
      (mapcar #'(lambda (item) (write-line (concatenate 'string "<a href=" (special-path-check
                                                                               (string-downcase (string item))) ".html>" 
                                                        (string item) "</a><br>") index))
              (sort allsymbols 'string<))
      (write-line "</td></tr>" index)
      
      (write-line (concatenate 'string "<tr><td class=center>" (credits-line) "</td></tr></table>") index)
      
      (write-line "</body></html>" index))
    
    (mapcar #'(lambda (symb) (make-ref-page symb dir title)) allsymbols)
    
    (let ((logopict (om-make-pathname :directory (om-resources-folder) :name "om" :type "png")))
      (when (probe-file logopict)
        (om-copy-file logopict
                      (make-pathname :directory (pathname-directory dir) :name "omlogo" :type "png"))))
    
    indexpath))

; (gen-om-reference)

(defun make-ref-page (symbol dir &optional title)
  (let ((title (or title ""))
        (pagepath (om-make-pathname :directory dir
                                    :name (special-path-check
                                           (string-downcase (string symbol)))
                                    :type "html"))
        (doc (get-documentation-info symbol)))
    (with-open-file (index pagepath :direction :output :if-exists :supersede)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference: " (string symbol) "</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      
      (write-line "<body bgcolor=BBBBBB>" index)
      (write-line "<table align=center bgcolor=FFFFFF width=85% cellpadding=20>" index)
      
      ;;; TITLE BAR
      (write-line  "<tr>" index)
      (write-line "<td class=\"dark\" colspan=2>" index)
      (write-line (concatenate 'string "<H1>" title " reference pages</H1>") index)
      (write-line "<p style=\"text-align: right;\"><b><a class=\"dark\" href=index.html>Main Index</a> | <a class=\"dark\" href=ind-alpha.html>Alphabetical Index</a></b></p>" index)
      (write-line "</td>" index)
      (write-line  "</tr>" index)
      
      (write-line "<tr><td>" index)
      (write-line (concatenate 'string "<h2>" (special-html-check (string symbol)) "</h2>") index)
      
      (unless (null doc)
        (write-line (concatenate 'string "<font size=-1>[" (string (nth 1 doc)) "]" "</font>") index))
      (write-line "</td>" index)
      
      ;;; ICON
      (write-line "<td width=130 align=right>" index)
      (let ((icn nil))
        (cond ((and (find-class symbol nil) (omclass-p (find-class symbol nil)))
               (setf icn (icon (find-class symbol nil))))
              ((and (fboundp symbol) (omgenericfunction-p (fdefinition symbol)))
               (setf icn (icon (fdefinition symbol))))
              (t nil))
        (let ((iconfile (and icn (om-relative-path '("icons" "boxes") (format nil "~A.png" icn) (om-resources-folder)))))
          (if (file-exist-p iconfile)
              (setf iconfile (namestring iconfile))
            (setf iconfile "./omlogo.png"))
          (write-line (concatenate 'string "<p class=right><img src=" iconfile " width=60></p>") index)
          ))
      
      (write-line "</td></tr>" index)

      ;;; MAIN BODY
      (write-line "<tr><td colspan=2>" index)
      
      (if (null doc)
          (write-line "<p>No documentation</p></td>" index)
       
        (progn
          (cond ;;; FUNTION ARGUMENTS
                ((or (string-equal (nth 1 doc) "FUNCTION")
                     (string-equal (nth 1 doc) "GENERIC-FUNCTION"))
                 (write-line "<font color=882222><b>ARGUMENTS:</b></font>" index)
                 (if (null (nth 2 doc))
                     (write-line "None<br>" index)
                   (progn
                     (write-line "<table width=100%>" index)
                     (loop for arg in (nth 2 doc) 
                         for i = 0 then (+ i 1) do
                         (write-line "<tr>" index)
                         (if (and (not (consp arg)) (equal #\& (elt (string arg) 0)))
                             (progn 
                               (write-line "<td>" index)
                               (write-line (concatenate 'string "<i><b>" (string-downcase (string arg))"</b></i>") index)
                               (write-line "</td>" index)
                               (write-line "<td colspan=3>&nbsp;</td></tr>" index)
                               (setf i (- i 1)))
                           (let ((argname (if (consp arg) (car arg) arg)))
                             (write-line "<td width=40>&nbsp;</td>" index) 
                             (write-line "<td>" index)
                             (write-line (concatenate 'string "- <font color=333366><b>" (format nil "~s" (intern (string-upcase argname))) "</b></font>") index)
                             (write-line "</td>" index)
                             ;; doc and defaults
                             (if (subtypep (class-of (fdefinition symbol)) 'OMGenericFunction)
                                 ; OM FUN
                                 (let ((indoc (nth i (inputs-doc (fdefinition symbol))))
                                       (defval (nth i (inputs-default (fdefinition symbol)))))
                                   (if indoc
                                       (write-line (concatenate 'string "<td>" (special-html-check indoc) "</td>") index)
                                     (write-line "<td>&nbsp;</td>" index))
                                   (write-line (concatenate 'string "<td>[default = " (format nil "~s" defval) "]</td>") index)
                                   )
                               (progn
                                 (write-line "<td width=20%>&nbsp;</td>" index)
                                 (if (consp arg)
                                     (write-line (concatenate 'string "<td>[default = " (format nil "~s" (cadr arg)) "]</td>") index)
                                   (write-line "<td>&nbsp;</td>" index))
                                 )
                               )
                             (write-line "</tr>" index)
                             ))
                         (write-line "<tr>" index))
                     (write-line "</table>" index)
                     )))
                
                ;;; CLASS SLOTS
                ((or (string-equal (nth 1 doc) "CLASS")
                     (string-equal (nth 1 doc) "INTERFACE BOX"))
                 (write-line "<font color=882222><b>SLOTS:</b></font><br><br>" index)
                 (if (null (nth 2 doc))
                     (write-line "None<br><br>" index)
                   (progn
                     (write-line "<table width=100% border=0>" index)
                     (loop for slot in (nth 2 doc) do
                           (write-line "<tr>" index)
                           (cond ((not (consp slot))
                                  (write-line "<tr><td colspan=3>&nbsp;</td></tr><td colspan=3>" index)
                                  (write-line (concatenate 'string "<i><b>" slot "</b></i>") index)
                                  (write-line "</td>" index))
                                 ((null (cadr slot))
                                  (write-line (concatenate 'string "<td colspan=3> - <font color=333366><b>" (string-upcase (string (car slot))) "</b></font></td>") index))
                                 (t (write-line (concatenate 'string "<td width=160> - <font color=333366><b>" (string-upcase (string (car slot))) "</b></font>:</td>") index)
                                    (write-line (concatenate 'string "<td>" (if (nth 2 slot) (special-html-check (nth 2 slot)) "") "</td>") index)
                                    (write-line (concatenate 'string "<td>" (format nil "[default = ~A]" (nth 1 slot)) "</td>") index)
                                    ))
                           (write-line "</tr>" index)
                           )
                     (write-line "</table>" index))
                 ))
          (write-line "</td>" index))))
      (write-line "</tr>" index)

      ;;; DESCRIPTION
      (write-line "<tr><td colspan=2>" index)
      (when doc
        (write-line "<font color=882222><b>Description:</b></font><p>" index)
        (loop for str in (om-text-to-lines (nth 3 doc)) do
              (write-line (concatenate 'string "" (special-html-check str) "<br>") index))
        )
      
      (write-line "</p><br></td></tr>" index)
      (write-line "<td class=center colspan=2>" index)
      (write-line (credits-line) index)
      (write-line "</td></tr></table>" index)
      
      (write-line "</body></html>" index))
    pagepath))

; (gen-om-reference)


