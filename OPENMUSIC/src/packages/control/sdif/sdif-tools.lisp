(in-package :om)


;;;===============
;;; TEXT CONVERSION
;;;===============

(defmethod* SDIF->text ((self string) &optional out-filename)
   :icon 639
   :indoc '("SDIF file" "text file pathname")
   :doc "Converts <self> to text-SDIF in <out-filename>."
   (let ((outfile (or (and out-filename (handle-new-file-exists out-filename))
                      (om-choose-new-file-dialog :types (list (format nil (om-str :file-format) "SDIF") "*.sdif" )))))
     (when outfile 
       (let ((SDIFF (sdif-open-file self)))
         (when SDIFF 
           (sdif::SdifToText SDIFF (namestring outfile))
           (sdif::SDIFFClose SDIFF)
           outfile)))))
     
(defmethod* SDIF->text ((self pathname) &optional out-filename)
   (SDIF->text (namestring self) out-filename))
  
