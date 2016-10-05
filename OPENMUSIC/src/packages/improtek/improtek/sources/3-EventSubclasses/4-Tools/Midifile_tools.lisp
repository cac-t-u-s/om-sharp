;Issu d'un découpage de Beatlist.lisp
;by Marc Chemillier (2004, revised version 2012)
; LA PLUPART A METTRE DANS UN FICHIER MIDITOOLS ????


(in-package :om)


(format *om-stream* "loading MIDIFILE_TOOLS~%")

;-------------------------------------------
;MIDIFILE UTILITIES
;-------------------------------------------

(defmethod save-as-midi-with-tempo ((object t) (beatdur integer) &optional filename &key (approx 2) (format nil))   
  (when *midiplayer*
      (let ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog  :directory (def-save-directory) 
                                                                                     :prompt (om-str "Save as...") 
                                                                                     :types (list (om-str "MIDI Files") "*.mid;*.midi")))))
      (when name 
        (unless (stringp (pathname-type name))
          (setf name (make-pathname :device (pathname-device name)
                                    :directory (pathname-directory name)
                                    :name (pathname-name name)
                                    :type "midi")))
        (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
        (my-MidiSaveAny object beatdur approx)         ;write events at OM fixed tempo BPM=60
        (my-save-seq *playing-midi-seq* name beatdur)  ;add a tempo change (but it does not change barlines)
        (namestring name)
        ))))

;for compatibility with older version 'my-save-as-midi' = 'save-as-midi-with-tempo'
;'save-as-midi' already exists in OpenMusic, but it saves the file with a fixed tempo BPM=60

(defun my-save-as-midi (chseq beatdur) (save-as-midi-with-tempo chseq beatdur))   


(defmethod my-MidiSaveAny ((object t) beatdur approx)               ;write notes on different tracks, thanks to Jean Bresson, Oct. 12th 2010
  (when *midiplayer*
    (setf *MidiShare-start-time* 0)
    (setf *playing-midi-seq* (om-midi-new-seq))
    (let ((5uplelist ;(chord-seq->mf-info object))                                 ; -> save with OM fixed tempo BPM=60 for adjusting barlines
                     (timestretch (chord-seq->mf-info object) (/ 1000 beatdur)))   ; + -> THEN update to 'beatdur'   ;MARC 7/2/2012
                     ;(timestretch (chord-seq->mf-info object) 2))   ;MARC 22/4/2012   ??????????????? for barlines to be adjusted to midi data:
                                                                     ;---> it seems that seq. must be saved by OM twice slower (half tempo)

          (hashchan (make-hash-table))) 
      (loop for x in 5uplelist 
            do (push x (gethash (MEChannel x) hashchan)))  ; groups events with the same channel into different sequences devoted to one channel
      (loop for i from 1 to 16 with voice = 1              ; for each channel, write the corresponding sequence into a separate track of the MIDI file
            when (gethash i hashchan) 
            do (progn (PrepareToPlay t      ;this function add events to *playing-midi-seq* ---> (om-midi-seq-add-evt *playing-midi-seq* newevent)
                                     (mf-info->chord-seq (reverse (gethash i hashchan))) 0 :approx approx :voice voice) 
                 (incf voice))))))                                                                                       

(defun my-save-seq (seq name beatdur)                         ;add the correct tempo at the beginning of the MIDI file
  (let ((tempo-evnt (om-midi-new-evt (om-midi-get-num-from-type "Tempo")
                                     :date 0 :vals (* beatdur 1000))))    ;"Tempo" -> = beatdur expressed in microseconds
    (om-midi-seq-concat-evt seq tempo-evnt nil)      ;nil ---> tempo-evnt placed at the beginning
    (om-midi-save-seq-in-file seq (om-path2cmdpath name) :fileformat 1)    ; fileformat default value = 1
    ))



(defmethod update-midifile-tempo-from-max ()
  (let* ((midifromfile (evts-from-midifile)))
    (if (null midifromfile) (format *om-stream* "Empty MIDI file~%")  
                                                                     
                                        ;22/4/2012   FROM MAX: no tempo meta event, but an implicit tempo BPM=120
                                        ;when slowed down by OM to BPM=60, durations must be divided by 2
      (let* ((midifromfile-halfdurations (timestretch midifromfile 0.5))
             (defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile-halfdurations))))))))
        (print defaultbeatdur)
        (format *om-stream* "~%Beat duration ~a ms, BPM = ~a~%" defaultbeatdur (round (/ 60000 defaultbeatdur))) 
        (let ((name (om-choose-new-file-dialog  :directory (def-save-directory) :prompt (om-str "Save as...") 
                                                :types (list (om-str "MIDI Files") "*.mid;*.midi"))))
          (when name 
            (format *om-stream* "Delete unused tracks and update tempo ~%Save MIDI file as \"~a.mid\"~%~%" (pathname-name name))
            (unless (stringp (pathname-type name))
              (setf name (make-pathname :device (pathname-device name) :directory (pathname-directory name)
                                        :name (pathname-name name) :type "midi")))
            (save-as-midi-with-tempo (mf-info->chord-seq midifromfile-halfdurations) defaultbeatdur name)))))))

(defmethod update-midifile-tempo-from-intuem ()
  (let* ((midifromfile (evts-from-midifile)))
    (if (null midifromfile) (format *om-stream* "Empty MIDI file~%")  ;Marc 16/2/2012 nil when "midibuff.mid" is empty 
                                                                     
      (let ((defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile))))))))
        (format *om-stream* "~%Beat duration ~a ms, BPM = ~a~%" defaultbeatdur (round (/ 60000 defaultbeatdur))) 
        (let ((name (om-choose-new-file-dialog  :directory (def-save-directory) :prompt (om-str "Save as...") 
                                                :types (list (om-str "MIDI Files") "*.mid;*.midi"))))
          (when name 
            (format *om-stream* "Delete unused tracks and update tempo ~%Save MIDI file as \"~a.mid\"~%~%" (pathname-name name))
            (unless (stringp (pathname-type name))
              (setf name (make-pathname :device (pathname-device name) :directory (pathname-directory name)
                                        :name (pathname-name name) :type "midi")))
            (save-as-midi-with-tempo (mf-info->chord-seq midifromfile) defaultbeatdur name)))))))





(defun dump-midifile (file)
  (setq midifileaccepted " Midifile not accepted")
  (with-open-file (stream file :direction :input)
    (let* ((res1 (read-header-chunk stream)) (bool (car res1)))
      (when bool (let ((res2 (read-track-chunk stream)))
                   (write (cons (cadr res1) (cadr res2)) :stream *om-stream* :base 16) (format *om-stream* midifileaccepted))))))

(defun dump-straight (file)
  (with-open-file (stream file :direction :input)
    (let* ((res1 (read-header-chunk stream)) (bool (car res1)))
      (when bool (let ((res2 (read-inside stream)))
                   (write (cons (cadr res1) (cadr res2)) :stream *om-stream* :base 16) (format *om-stream* "end of file"))))))

(setq midifileaccepted " Midifile accepted")

(defun read-inside (stream)
  (let* ((byte (my-read-byte stream ())) (res (list byte)))
    (when (not byte) (setq midifileaccepted " Midifile accepted"))   ;;; FINAL STATE
    (loop until (not byte) do (progn (setq byte (my-read-byte stream ())) (push byte res)))
    (list byte (reverse res))))

(defun read-header-chunk (stream) (read-n-bytes 14 stream))

(defun read-track-chunk (stream)
  (let* ((res1 (read-init-track-chunk stream)) (bool (car res1)) (res-notes ()))
    (loop until (not bool) do (let ((res2* (read-event-track-chunk stream))) (setq bool (car res2*)) (when bool (push (cadr res2*) res-notes))))
    (list bool (cons (cadr res1) (reverse res-notes)))))

(defun read-init-track-chunk (stream) (read-n-bytes 8 stream))

(defun read-event-track-chunk (stream)
  (let* ((res1 (read-deltatime stream)) (bool (car res1)))
    (when bool (let ((res2 (read-event stream))) (setq bool (car res2)) (list bool (append (cadr res1) (cadr res2)))))))
  
(defun read-deltatime (stream)
  (let* ((byte (my-read-byte stream ())) (res (list byte)))
    (when (not byte) (setq midifileaccepted " Midifile accepted"))   ;;; FINAL STATE
    (loop until (or (not byte) (< byte #x80)) do (progn (setq byte (my-read-byte stream ())) (push byte res)))
    (list byte (reverse res))))

(defun read-event (stream)
  (let ((statut (my-read-byte stream ()))) (cond ((= statut #xFF) (read-meta-event stream)) 
                                                 ((or (= statut #x90) (= statut #x80)) (read-note statut stream)))))

(defun read-note (statut stream) (let ((res (read-n-bytes 2 stream))) (list (car res) (cons statut (cadr res)))))

(defun read-meta-event (stream)
  (let ((type (my-read-byte stream ())))
    (case type (#x58 (let ((res (read-n-bytes 5 stream))) (list (car res) (append (list #xFF #x58) (cadr res)))))  ;;; signature
               (#x51 (let ((res (read-n-bytes 4 stream))) (list (car res) (append (list #xFF #x51) (cadr res)))))  ;;; tempo
               (#x2F (let ((res (read-n-bytes 1 stream))) (list (car res) (append (list #xFF #x2F) (cadr res)))))))) ;;; end-of-track

;;; Read n first bytes in the stream:

(defun read-n-bytes (n stream)
  (let* ((byte (my-read-byte stream ())) (res (list byte)))
    (do ((i n (- i 1))) ((or (= i 1) (not byte)) (list byte (reverse res))) (setq byte (my-read-byte stream ())) (push byte res))))

;;; Ascii: (char-code #\a) = 97, (code-char 97) = #\a

(defun my-read-byte (stream bool) (let ((x (read-char stream bool))) (if (not x) x (char-code x))))















