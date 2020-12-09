;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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

(defun get-om-reference-pages-index (&optional folder)
  (merge-pathnames
   (make-pathname :name "index" :type "html")
   (or folder (get-om-reference-pages-folder))))

;;; characters to avoid in file names
(defun special-path-check (name)
  (let ((pos nil))
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
    name))

;;; characters to avoid in HTML files
(defun special-html-check (name)
  (let ((pos nil))
    (loop while (setf pos (position #\> name)) do
          (setf name (concatenate 'string (subseq name 0 pos)
                                  "&gt;" (subseq name (+ pos 1)))))
    (loop while (setf pos (position #\< name)) do
          (setf name (concatenate 'string (subseq name 0 pos)
                                  "&lt;" (subseq name (+ pos 1)))))
    name))


;;;======================================
;;; AUTO GEN ENTRIES FROM PACKAGES
;;;======================================

; (gen-package-entries *om-package-tree*)
; (gen-om-reference)

;;; the toplevel package
(defmethod gen-package-entries ((pack OMAbstractPackage) &key exclude-packages)
  `(:package
    (:name ,(name pack))
    (:doc ,(doc pack))
    (:entries
     ,(remove nil (remove-duplicates
                   (append (mapcar 'class-name (classes pack))
                           (mapcar 'function-name (functions pack))
                           (special-items pack))
                   :test 'string-equal)))
    (:sections
     ,(loop for sub-pack in (subpackages pack)
            when (not (find (name sub-pack) exclude-packages :test 'string-equal))
            collect
            `(:section
              (:name ,(name sub-pack))
              (:doc ,(doc sub-pack))
              (:entries
               ,(remove nil (remove-duplicates
                             (append (mapcar 'class-name (classes sub-pack))
                                     (mapcar 'function-name (functions sub-pack))
                                     (special-items sub-pack))
                             :test 'string-equal)))
              (:groups ,(loop for sub-sub-pack in (subpackages sub-pack)
                              when (not (find (name sub-sub-pack) exclude-packages :test 'string-equal))
                              append (gen-subpack-entries sub-sub-pack :exclude-packages exclude-packages))))
            ))
    ))

;;; will keep all groups at the same hierarchical level (uses append)
(defmethod gen-subpack-entries ((pack OMAbstractPackage) &key exclude-packages)
  (cons `(:group
          (:name ,(name pack))
          (:doc ,(doc pack))
          (:entries ,(remove nil (remove-duplicates
                                  (append (mapcar 'class-name (classes pack))
                                          (mapcar 'function-name (functions pack))
                                          (special-items pack))
                                  :test 'string-equal))))
        (loop for sub-pack in (subpackages pack)
              when (not (find (name sub-pack) exclude-packages :test 'string-equal))
              append
              (gen-subpack-entries sub-pack :exclude-packages exclude-packages))
        ))


;;;======================================
;;; EXTRACT INFO FROM METAOBJECTS
;;;======================================

(defun fun-to-doclist (symbol)
  (let ((fun (fdefinition symbol)))
    (list symbol
          (if (typep fun 'generic-function) "GENERIC-FUNCTION" "FUNCTION")
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
           (when (additional-class-attributes instance)
             (cons "Optional:"
                   (loop for add-slot in (additional-class-attributes instance)
                         collect
                         (let ((slot (find add-slot (class-slots class) :key 'slot-name)))
                           (list add-slot
                                 (eval (slot-initform slot))
                                 (slot-doc slot))))))
           )
          (class-documentation class))))

;(special-item-reference-class 'collect)

(defun box-to-doclist (symbol)
  (let ((class (find-class (special-item-reference-class symbol) nil)))
    (when class
      (let ((instance (make-instance (special-item-reference-class symbol))))
        (list symbol "SPECIAL BOX"
              ;;; slots desc
              (loop for k in (flat (get-all-keywords instance))
                    collect
                    (let ((slot (find (symbol-name k) (class-slots class)
                                      :key #'(lambda (slot) (symbol-name (slot-name slot)))
                                      :test 'string-equal)))
                      (list (slot-name slot)
                            (eval (slot-initform slot))
                            (slot-doc slot))))
              (class-documentation class))))))

; (gen-om-reference)

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
  "This is the reference documentation for the main functions and classes.")

(defun gen-om-reference ()
  (gen-reference (gen-package-entries *om-package-tree*)
                 (get-om-reference-pages-folder)
                 :maintext *om-ref-text*))


(defun ensure-om-reference-pages ()
  (unless (probe-file (get-om-reference-pages-index))
    (gen-om-reference)))

;(show-reference-page 'om+)
(defmethod show-reference-page ((symbol symbol))

  (let* ((lib (get-object-library symbol))
         (reference-folder
          (if lib (let ((folder (get-lib-reference-pages-folder lib)))
                    (unless (probe-file (get-om-reference-pages-index folder))
                      (gen-lib-reference lib))
                    folder)
            (progn
              (ensure-om-reference-pages)
              (get-om-reference-pages-folder)))))

    (let ((file (om-make-pathname :directory reference-folder
                                  :name (special-path-check (string-downcase (string symbol)))
                                  :type "html")))
      (if (probe-file file)
          (om-open-in-browser (namestring file))
        (om-beep-msg "No reference page found for '~A'" symbol))
      )))


; (gen-om-reference)

;;;======================================
;;; HTML LAYOUT, CONTENTS AND STYLE
;;;======================================

(defparameter *om-ref-css*
  "<STYLE TYPE=\"text/css\"><!--

body, p, td, li {
  background-color: #fff;
  font-family: \"Calibri\";
  font-size: 14;
  color : #222222;
}

A:link {text-decoration: underline; color : 333366;}

h2 {
  margin-bottom: 0px;
}

p {
  margin-top: 0px;
}

hr {
  border: .4px dashed;
}

table, td {
  border: 0px;
  background-color: #f0f0f0;
}

.frame {
  width: 80%;
  max-width: 600px;
  margin-left: 10%;
}

.main {
  border: 1px solid;
  padding: 20px;
  background-color: #e5e5e5;
}


 --></style>")

(defun credits-line ()
  (concatenate 'string
               "<br><center><font size=-2>"
               "Auto doc generation by "
               *app-name* " "
               *version-string*
               " - "
               (om-get-date nil)
               "</font></center>"))

; (gen-om-reference)

(defun gen-reference (package-entries dir &key title maintext logofile)

  (when (probe-file dir) (om-delete-directory dir))
  (om-create-directory dir)

  (let* ((data (cdr package-entries))
         (title (or title (format nil "~A ~A" *app-name* *version-string*)))
         (indexpath (om-make-pathname :directory dir :name "index" :type "html"))
         (alphaindexpath (om-make-pathname :directory dir :name "ind-alpha" :type "html"))
         (allsymbols (remove nil
                             (append
                              (find-value-in-kv-list data :entries)
                              (loop for section in (find-value-in-kv-list data :sections) append
                                    (append (find-value-in-kv-list (cdr section) :entries)
                                            (loop for group in (find-value-in-kv-list (cdr section) :groups) append
                                                  (find-value-in-kv-list (cdr group) :entries)))))))
         )

    (let ((logopict (or logofile (om-make-pathname :directory (om-resources-folder) :name "om-sharp" :type "png"))))
      (when (probe-file logopict)
        (om-copy-file logopict
                      (make-pathname :directory (pathname-directory dir) :name "logo" :type "png"))))


    (with-open-file (index indexpath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head>") index)
      (write-line (concatenate 'string "<title>" title " Reference</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)

      (write-line "<body>" index)

      (write-line "<div class=\"frame\">" index)

      ;;; TITLE BAR
      (write-line "<div class=\"header\">" index)
      (write-line (concatenate 'string "<H1>" title " reference pages</H1>") index)
      (write-line "<b>Main Index | <a href=ind-alpha.html>Alphabetical Index</a></b>" index)
      (write-line "</div>" index)
      (write-line "<br><hr>" index)

      ;;; CENTER
      (when maintext
        (loop for par in (list! maintext) do
              (write-line "<p>" index)
              (write-line par index)
              (write-line "</p>" index)))

      ;;; PACKAGE LIST
      (when (find-value-in-kv-list data :sections)
        (write-line "<p>Packages covered: " index)
        (write-line "<ul>" index)
        (loop for pack in (find-value-in-kv-list data :sections) do
              (let ((name (find-value-in-kv-list (cdr pack) :name)))
                (when (and name (not (string-equal name "")))
                  (write-line (concatenate 'string "<li><b><a href=#" (string name) ">" (string-upcase (string name)) "</a></b></li>") index))))
        (write-line "</ul></p>" index))

      ;;; MAIN CONTENTS

      (when (find-value-in-kv-list data :entries)
        (write-line "<hr>" index)
        ;;; top level items (not in a section)
        (loop for item in (find-value-in-kv-list data :entries) do
              (write-line (concatenate 'string "<a href=" (special-path-check (string-downcase (string item))) ".html>"
                                       (special-html-check (string item)) "</a> ") index))
        )

      (loop for section in (find-value-in-kv-list data :sections) do

            (let ((name (find-value-in-kv-list (cdr section) :name))
                  (doc (find-value-in-kv-list (cdr section) :doc)))

              (write-line "<hr>" index)
              (when name
                (write-line (concatenate 'string "<a name=" (string name) ">" "<h2>" (string name) "</h2></a>") index))
              (when doc
                (write-line (concatenate 'string "<p>" doc "</p>") index))

              ;;; items in section (not in a sub-group)
              (write-line "<ul>" index)
              (loop for item in (find-value-in-kv-list (cdr section) :entries) do
                    (write-line (concatenate 'string "<li><a href=" (special-path-check (string-downcase (string item))) ".html>"
                                             (special-html-check (string item)) "</a></li>") index))
              (write-line "</ul>" index)

              (loop for group in (find-value-in-kv-list (cdr section) :groups) do

                    (let ((n (find-value-in-kv-list (cdr group) :name))
                          (d (find-value-in-kv-list (cdr group) :doc)))
                      (when n
                        (write-line (concatenate 'string "<h3>" (string n) "</h3>") index))
                      (when d
                        (write-line (concatenate 'string "<p>" (string d) "</p>") index))

                      (write-line "<ul>" index)
                      (loop for item in (find-value-in-kv-list (cdr group) :entries) do
                            (write-line (concatenate 'string "<li><a href=" (special-path-check (string-downcase (string item))) ".html>"
                                                     (special-html-check (string item)) "</a></li>") index))
                      (write-line "</ul>" index)
                      ))
              ))

      (write-line "<br><br>" index)
      (write-line (concatenate 'string "<center>" (credits-line) "</center>") index)

      (write-line "</div>" index)

      (write-line "</body></html>" index))

    (with-open-file (index alphaindexpath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)

      (write-line "<body>" index)

      (write-line "<div class=\"frame\">" index)


      ;;; TITLE BAR
      (write-line "<div class=\"header\">" index)
      (write-line (concatenate 'string "<H1>" title " reference pages</H1>") index)
      (write-line "<b><a href=index.html>Main Index</a> | Alphabetical Index</b>" index)
      (write-line "</div>" index)
      (write-line "<br><hr>" index)

      (mapcar #'(lambda (item)
                  (write-line
                   (concatenate 'string "<a href=" (special-path-check
                                                    (string-downcase (string item))) ".html>"
                                (string item) "</a><br>")
                   index))
              (sort allsymbols 'string<))

      (write-line (concatenate 'string "<center>" (credits-line) "</center>") index)

      (write-line "</div>" index)

      (write-line "</body></html>" index))

    ;;; GENERATE ALL THE PAGES
    (mapcar #'(lambda (symb) (make-ref-page symb dir title)) allsymbols)

    indexpath))


; (gen-om-reference)


(defun make-ref-page (symbol dir &optional title)
  (let* ((title (or title ""))
         (pagepath (om-make-pathname :directory dir
                                     :name (special-path-check
                                            (string-downcase (string symbol)))
                                     :type "html"))
         (doc (get-documentation-info symbol))
         (object (cond ((or (string-equal (nth 1 doc) "FUNCTION")
                            (string-equal (nth 1 doc) "GENERIC-FUNCTION"))
                        (fdefinition symbol))
                       ((string-equal (nth 1 doc) "CLASS")
                        (find-class symbol))
                       ((string-equal (nth 1 doc) "INTERFACE BOX")
                        (find-class symbol nil))))
         )

    (with-open-file (index pagepath :direction :output :if-exists :supersede)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference: " (string symbol) "</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)

      (write-line "<body>" index)

      (write-line "<div class=\"frame\">" index)

      ;;; TITLE BAR
      (write-line "<div class=\"header\">" index)

      (write-line (concatenate 'string "<H1>" title " reference pages</H1>") index)
      (write-line "<b><a href=index.html>Main Index</a> | <a href=ind-alpha.html>Alphabetical Index</a></b>" index)

      (write-line "<br><br>" index)
      (write-line "</div>" index)


      ;;; EMBEDDED TABLE AT CENTER
      (write-line "<div class=\"main\">" index)

      ;;; ICON
      (let ((iconfile
             (when (or (and (string-equal (nth 1 doc) "CLASS")
                            (omclass-p object))
                       (and (string-equal (nth 1 doc) "GENERIC-FUNCTION")
                            (omgenericfunction-p object)))
               (if (icon object)
                   (if (library object)
                       (om-relative-path '("icons") (format nil "~A.png" (icon object))
                                         (lib-resources-folder (find-library (library object))))
                     (om-relative-path '("icons" "boxes") (format nil "~A.png" (icon object)) (om-resources-folder)))))))
        (when (file-exists-p iconfile)
          (write-line (concatenate 'string "<img src=" (namestring iconfile) " align=\"right\" width=60>") index)
          ))

      ;;; NAME
      (write-line (concatenate 'string "<h2>" (special-html-check (string symbol)) "</h2>") index)

      (unless (null doc)
        (write-line (concatenate 'string "<font size=-1>[" (string (nth 1 doc)) "]" "</font>") index))


      ;;; MAIN BODY

      (if (null doc)
          (write-line "No documentation" index)

        (progn
          (cond ;;; FUNTION ARGUMENTS
                ((or (string-equal (nth 1 doc) "FUNCTION")
                     (string-equal (nth 1 doc) "GENERIC-FUNCTION"))
                 (write-line "<h3>Function arguments:</h3>" index)
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
                               (write-line (concatenate 'string "- <font color=333366><b>"
                                                        (string-upcase (format nil "~S" (intern (string-upcase argname))))
                                                        "</b></font>") index)
                               (write-line "</td>" index)
                               ;; doc and defaults
                               (if (subtypep (class-of (fdefinition symbol)) 'OMGenericFunction)

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
                 (write-line "<h3>Class slots/attributes:</h3>" index)
                 (if (null (nth 2 doc))
                     (write-line "<p>None</p>" index)
                   (progn
                     (write-line "<table width=100% border=0>" index)
                     (loop for slot in (nth 2 doc) do
                           (write-line "<tr>" index)
                           (cond ((not (consp slot))
                                  (write-line "<tr><td colspan=3>&nbsp;</td></tr><td colspan=3>" index)
                                  (write-line (concatenate 'string "<i><b>" slot "</b></i>") index)
                                  (write-line "</td>" index))
                                 (t (write-line (concatenate 'string "<td > - <font color=333366><b>" (string-upcase (string (car slot))) "</b></font></td>") index)
                                    (write-line (concatenate 'string "<td>" (if (nth 2 slot) (special-html-check (nth 2 slot)) "") "</td>") index)
                                    (write-line (concatenate 'string "<td>" (format nil "[default = ~A]" (nth 1 slot)) "</td>") index)
                                    ))
                           (write-line "</tr>" index)
                           )
                     (write-line "</table>" index))
                   ))
                )))

      ;;; DESCRIPTION
      (when (nth 3 doc)
        (write-line "<h3>Description:</h3>" index)
        (loop for str in (om-text-to-lines (nth 3 doc)) do
              (write-line (concatenate 'string "" (special-html-check str) "<br>") index))
        )

      (write-line "</div>" index)

      (write-line (credits-line) index)

      (write-line "</div>" index)

      (write-line "</body></html>" index))
    pagepath))


; (gen-om-reference)


