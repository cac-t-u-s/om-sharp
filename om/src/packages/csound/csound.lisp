;============================================================
; o7 -- an implementation of the OpenMusic visual programming
; language and computer-aided composition environment.
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; 
; For information on usage and redistribution, 
; and for a DISCLAIMER OF ALL WARRANTIES, 
; see the file "LICENSE.txt" in this distribution.
;============================================================

;============================================================
; CSOUND INTERFACE
;============================================================

(in-package :om)

(add-preference-section :externals "Csound")
(add-preference :externals :csound-path "Csound exec path" :file #P"/usr/local/bin/csound")
(add-preference :externals :csound-flags "Default flags" :string "-f -m7 -A -N -g -b8192 -B8192")
(add-preference :externals :csound-gen-args "Max GEN arguments" (make-number-in-range :min 2 :max 10000) 1024)
(add-preference :externals :csound-def-table "Default table" :string "f 1 0 4097 7  0 2048 1 2048 0")
(add-preference :externals :csound-table-size "Default table size" (make-number-in-range :min 2 :max 10000) 4097)
;(add-preference :externals :csound-normalize "Normalize Csound outputs" :bool t)

;;; Main Csound synthesis function

(defmethod* csound-synth ((sco pathname) (orc pathname) &key out resolution) ; normalize
  :icon 'csound
  (if (get-pref-value :externals :csound-path)
      (let* ((RT-OUT (equal out :rt))
             (outpath (if RT-OUT nil
                        (handle-new-file-exists
                         (cond ((pathnamep out) out)
                               ((stringp out) (outfile out))
                               (t (outfile (pathname-name sco)))))))
             (tmppath (unless RT-OUT
                        (handle-new-file-exists 
                         (om-make-pathname :directory outpath :name (pathname-name outpath) :type "tmp"))))
             (csout (if (and (not rt-out) 
                             (or normalize (get-pref-value :externals :csound-normalize))
                             ) tmppath outpath)))
   
        (print "======================================")
        (print "CSOUND SYNTHESIS...")
        (print "======================================")
        (print (format nil "~%Orchestra: ~s~%Score: ~s~%Output: ~s" orc sco csout))
        
        (when (and (not RT-OUT) (probe-file outpath))
          (print (string+ "Deleting existing file: " (namestring outpath)))
          (om-delete-file outpath))
           
        (om-cmd-line
         (print (format nil "~s ~A ~s ~s ~A" 
                        (namestring (get-pref-value :externals :csound-path))
                        (get-pref-value :externals :csound-flags)
                        (namestring orc)
                        (namestring sco)
                        (if RT-OUT "-odac" (format nil "-o ~s" (namestring outpath)))
                        )))
        
        (if (and (not RT-OUT) (null (probe-file outpath)))
            (om-message-dialog "!!! Error in CSound synthesis !!!")
          (om-print "END CSOUND SYNTHESIS"))
        
        (om::maybe-clean-tmp-files)
        (and outpath (probe-file outpath)))
    (om-beep-msg "ErRROR: CSound exec not found! (check in OM/External preferences)")))


(defmethod! csound-synth ((sco t) (orc t) &optional out-name normalize resolution)
  (csound-synth (convert-input-to-csound sco "sco") (convert-input-to-csound orc "orc") 
                out-name 
                normalize resolution))


(defmethod convert-input-to-csound ((self string) &optional type) (pathname self))
(defmethod convert-input-to-csound ((self pathname) &optional type) self)

(defmethod convert-input-to-csound ((self list) &optional type)
  (let ((path (tmpfile "temp_csound_file" :type type)))
    (with-open-file (out path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop for item in self do (write-line item out)))
    (add-tmp-file path)
    path))
    
(defmethod convert-input-to-csound ((self textbuffer) &optional type)
   (let ((path (tmpfile "temp_csound_file" :type type)))
     (save-as-text self path)
     (add-tmp-file path)
     path))


