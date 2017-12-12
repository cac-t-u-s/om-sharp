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


;============================================================
; CSOUND INTERFACE
;============================================================

(in-package :om)

;;;=============================
;;; Preferences
;;;=============================

(defun which-executable (program-file &optional default)
  (with-open-stream (s (sys::open-pipe (format nil "which ~A" program-file)))
    (let ((path (read-line s nil nil)))
      (if path
	  (handler-case (and (file-executable-p path) path)
	    (simple-error () default))
	  default))))

(add-preference-section :externals "Csound")
(add-preference :externals :csound-path "Csound exec path" 
                :file 
                #+linux (pathname (which-executable "csound" "no csound found in $PATH"))
                #-linux #P"/usr/local/bin/csound"
                )
(add-preference :externals :csound-flags "Default flags" :string "-f -m7 -A -N -g -b8192 -B8192")
(add-preference :externals :csound-gen-args "Max GEN arguments" (make-number-in-range :min 2 :max 10000) 1024)
(add-preference :externals :csound-def-table "Default table" :string "f 1 0 4097 7  0 2048 1 2048 0")
(add-preference :externals :csound-table-size "Default table size" (make-number-in-range :min 2 :max 10000) 4097)

;;;=============================
;;; Main Csound synthesis function
;;;=============================

(defmethod* csound-synth ((orc pathname) (sco pathname) &key out format)
  :icon 'csound
  (if (get-pref-value :externals :csound-path)
      (let* ((RT-OUT (equal out :rt))
             (outpath (if RT-OUT nil
                        (handle-new-file-exists
                         (cond ((pathnamep out) out)
                               ((stringp out) (outfile out))
                               (t (outfile (pathname-name sco)))))))
             (csout (unless RT-OUT
                        (handle-new-file-exists 
                         (om-make-pathname :directory outpath :name (pathname-name outpath) :type (or format "wav"))))))
   
        (om-print "======================================")
        (om-print "BEGIN CSOUND SYNTHESIS...")
        (om-print "======================================")
        (om-print-format "~%Orchestra: ~s~%Score: ~s~%Output: ~A~%~%" 
                         (list (namestring orc) (namestring sco) (if RT-OUT "DAC" (namestring csout))))
        
        (when (and (not RT-OUT) (probe-file outpath))
          (om-print (string+ "Deleting existing file: " (namestring outpath)))
          (om-delete-file outpath))
           
        (om-cmd-line
         (format nil "~s ~A ~A ~s ~s ~A" 
                 (namestring (get-pref-value :externals :csound-path))
                 (get-pref-value :externals :csound-flags)
                 (if format (string+ "--format=" format) "")
                 (namestring orc)
                 (namestring sco)
                 (if RT-OUT "-odac" (format nil "-o ~s" (namestring outpath)))
                 ))
        
          (om-print "======================================")
          (om-print "END CSOUND SYNTHESIS")
          (om-print "======================================")
          
          (when (and (not RT-OUT) (null (probe-file outpath)))
            (om-message-dialog "!!! Error in CSound synthesis !!!"))
    
          (om::maybe-clean-tmp-files)
        (and outpath (probe-file outpath)))
    (om-beep-msg "ERROR: CSound exec not found! (check in OM/External preferences)")))


(defmethod! csound-synth ((orc t) (sco t) &key out format)
  (csound-synth (convert-input-to-csound orc "orc") 
                (convert-input-to-csound sco "sco") 
                :out out :format format))

(defmethod convert-input-to-csound ((self string) &optional type) (pathname self))
(defmethod convert-input-to-csound ((self pathname) &optional type) self)

(defmethod convert-input-to-csound ((self cons) &optional type)
  (let ((path (tmpfile "temp_csound_file" :type type)))
    (with-open-file (out path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop for item in self do (write-line item out)))
    (add-tmp-file path)
    path))
    
(defmethod convert-input-to-csound ((self textbuffer) &optional type)
   (let ((path (tmpfile "temp_csound_file" :type type)))
     (save-as-text self path)
     (when (probe-file path)
       (add-tmp-file path)
       path)))

(defmethod convert-input-to-csound ((self null) &optional type)
  (error "Empty input for CSOUND-SYNTH"))






