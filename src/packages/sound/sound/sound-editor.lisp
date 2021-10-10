;============================================================================
; om#: visual programming language for computer-assisted music composition
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

;====================
; SOUND EDITOR
;====================

(in-package :om)


(defclass sound-editor (data-stream-editor)
  ((cache-display-list :accessor cache-display-list :initform nil)))

(defclass sound-panel (stream-panel) ())
(defmethod get-editor-class ((self sound)) 'sound-editor)
(defmethod editor-view-class ((self sound-editor)) 'sound-panel)

(defmethod window-title-for-object ((self sound))
  (string+ "SOUND - "
           (if (file-pathname self)
               (namestring (file-pathname self))
             "Temp Buffer")))

(defmethod frame-display-modes-for-object ((self sound-editor) (object sound)) '(:lines))

(defmethod make-editor-controls ((editor sound-editor))
  (let ((sound (object-value editor)))
    (om-make-layout
     'om-row-layout
     :align :center
     :subviews
     (list
      (om-make-di 'om-simple-text :text "Gain"
                  :size (omp 40 20)
                  :font (om-def-font :font1))
      (om-make-view
       'om-view :size (om-make-point 28 20)
       :subviews
       (list (om-make-graphic-object 'numbox
                                     :value (gain sound)
                                     :bg-color (om-def-color :white)
                                     :border t
                                     :decimals 2
                                     :size (om-make-point 36 18)
                                     :font (om-def-font :font1)
                                     :min-val 0.0 :max-val 10.0
                                     :change-fun #'(lambda (item)
                                                     (set-gain sound (get-value item)))
                                     :after-fun #'(lambda (item)
                                                    (declare (ignore item))
                                                    (report-modifications editor)))
             ))))))


(defmethod editor-view-after-init-space ((self sound)) 0)

(defmethod default-editor-min-x-range ((self sound-editor)) 0)

(defmethod om-draw-contents ((self sound-panel))
  (call-next-method)
  (let* ((editor (editor self))
         (sound (if (multi-display-p editor)
                    (nth (stream-id self) (multi-obj-list editor))
                  (object-value editor))))
    (if (or (buffer sound) (and (access-from-file sound) (valid-pathname-p (file-pathname sound))))
        ;;; draw the sound..
        (draw-sound-waveform sound editor self (x1 self) (x2 self) (stream-id self))
      ;;; no sound
      (om-with-fg-color (om-def-color :light-gray)
        (om-with-font (om-make-font "Arial" (round (h self) 4) :style '(:bold))
                      (om-draw-string 10 (+ (round (h self) 2) (round (h self) 8)) "No sound loaded..")
                      ))
      )
    ))


(defun draw-sound-waveform (sound editor view from to &optional (sound-id 0))
  (unless (nth sound-id (cache-display-list editor))
    (setf (cache-display-list editor)
          (if (multi-display-p editor)
              (mapcar #'(lambda (o) (get-cache-display-for-draw o nil)) (multi-obj-list editor))
            (list (get-cache-display-for-draw (object-value editor) nil)))))
  (let ((dur (get-obj-dur sound))
        (pict (nth sound-id (cache-display-list editor))))
    (when (and pict (not (equal :error pict)))
      (om-draw-picture pict
                       :w (w view) :h (h view)
                       :src-h (* (om-pict-height pict))
                       :src-x (* (om-pict-width pict) (/ from dur))
                       :src-w (* (om-pict-width pict) (/ (- to from) dur))))
    ))


(defmethod update-to-editor ((editor sound-editor) (from t))
  (unless (or (equal from editor)
              (and (multi-display-p editor)
                   (equal from (container-editor editor))))
    (setf (cache-display-list editor) nil))
  (call-next-method))


(defmethod editor-key-action ((editor sound-editor) key)
  (let* ((panel (active-panel editor))
         (stream (object-value editor)))
    (case key
      (#\l
       (when (selection editor)
         (let ((newlabel (om-get-user-string "enter a new label for selected markers"
                                             :initial-string (or (label (nth (car (selection editor)) (frames stream)))
                                                                 ""))))
           (when newlabel
             (loop for pos in (selection editor) do
                   (setf (label (nth pos (frames stream))) newlabel))
             (om-invalidate-view panel)
             (report-modifications editor)
             )
           )))
      (:om-key-esc NIL) ;;; we don't want to reinit-x-ranges as in the next-method
      (otherwise (call-next-method)))
    ))


#|
(defmethod om-draw-waveform ((self soundPanel))
  (multiple-value-bind (data smplevel)
      (om-get-display-slice self)
    (when (and (> (car (array-dimensions data)) 0) (> (cadr (array-dimensions data)) 0))
      (let* ((thesound (object (om-view-container self)))
           (window-v-size (* 0.99 (om-point-v (om-view-size self))))
           (system-etat (get-system-etat self))
           (xmin (car (rangex self)))
           (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
           (xmax (cadr (rangex self)))
           (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
           (xtime (- xmax xmin))
           (nbpts (cadr (array-dimensions data)))
           (timestep (/ xtime (coerce nbpts 'single-float)))
           (nch (om-sound-n-channels thesound))
           (channels-h (round window-v-size nch))
           (offset-y (round channels-h 2))
           pixpoint
           pixtime
           pixprev pixtprev)
      (when data
        (om-with-fg-color nil *om-steel-blue-color*
          ;(when smplevel    ;;;Use this "when" only when HQ display is used below
          (om-with-fg-color nil (om-make-color-alpha 0.41 0.54 0.67 0.5) ;;;=*om-steel-blue-color* with 50% transparency
            (dotimes (i nch)
              (om-draw-line pixmin (- (+ (* i channels-h) offset-y) 10) pixmax (- (+ (* i channels-h) offset-y) 10))));)
          (dotimes (c nch)
            (setq pixprev (round (* offset-y (* 0.99 (aref data c 0)))))
            (setq pixtprev (om-point-h (point2pixel self (om-make-point (round xmin) 0) system-etat)))
            (loop for i from 1 to (1- (cadr (array-dimensions data))) do
                  (let ((val (aref data c i)))
                    (setf pixtime (om-point-h (point2pixel self (om-make-point (+ xmin (* i timestep)) 0) system-etat)))
                    (setf pixpoint (* offset-y (* 0.99 val))) ; scaled 0-1 --> 0 -->256/2
                    (if (not smplevel) ;(= nbpts nbpix)
                        (progn
                          ;;;Use this for HQ display : requires a bit more computation to display middle lines with dynamic intensity
                          ;(when (not smplevel)
                          ;  (let ((colorval (- 1 (log (1+ (* (1- (exp 1)) (expt (abs val) 0.1)))))));(- 1 (expt (abs val) 0.2))));(expt (abs val) 0.2)))
                          ;    (om-with-fg-color nil (om-make-color-alpha 0.41 0.54 0.67 colorval)
                          ;      (om-draw-line pixtprev (- (+ (* c channels-h) offset-y) 10) pixtime (- (+ (* c channels-h) offset-y) 10)))))
                          (om-fill-polygon `(,(om-make-point pixtprev (- (+ offset-y (* c channels-h) pixprev) 9))
                                             ,(om-make-point pixtime (- (+ offset-y (* c channels-h) pixpoint) 9))
                                             ,(om-make-point pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 9))
                                             ,(om-make-point pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 9)))))
                      (om-draw-line pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 10) pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 10)))
                    (setq pixprev pixpoint pixtprev pixtime))))))))))


(defmethod om-get-display-slice ((self soundpanel))
  (let* ((snd (object (om-view-container self)))
         (system-etat (get-system-etat self))
         (xmin (car (rangex self)))
         (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
         (xmax (cadr (rangex self)))
         (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
         (nbpix (- pixmax pixmin)))
    (sound-get-display-array-slice snd nbpix xmin xmax)))


;;;===================
;;; DISPLAY-ARRAY
;;;===================

;;; Function used to FILL the display array of a sound (and choosed max window)
;;; (use LIBSNDFILE and FLI)
(defmethod om-fill-sound-display-array ((format t) path ptr channels size &optional (window 128))
  #+libsndfile
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    ;;;Initialisation du descripteur
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0)
    (let* ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames) :type :int :index #+powerpc 1 #-powerpc 0))
           (channels (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) :type :int :index #+powerpc 1 #-powerpc 0))
           (buffer-size (* window channels))
           (buffer (fli::allocate-foreign-object :type :float :nelems buffer-size))   ;Fenêtrage du son
           ;(MaxArray (make-array (list channels (ceiling size window)) :element-type 'single-float :initial-element 0.0))   ;Tableau pour stocker les max
           (indxmax (1- (ceiling size window)))
           (frames-read 0)
           maxi)
      (loop for indx from 0 do ;(print (list indx "/" (ceiling size window)))
            (setq frames-read (sf::sf-readf-float sndfile-handle buffer window))
            (dotimes (n channels)
              (dotimes (i window)
                (setq maxi (max (abs (fli:dereference buffer :type :float :index (+ n (* channels i)))) (or maxi 0.0))))
              ;(setf (aref MaxArray n (min indx indxmax)) maxi)
              (setf (fli:dereference ptr :index (+ (min indx indxmax) (* n (ceiling size window)))) maxi)
              (setq maxi 0.0))
            while (= frames-read window))
      (fli:free-foreign-object buffer)
      (sf::sf_close sndfile-handle))))


;(ratio (round (om-sound-n-samples self) 2000)))) for variable ratio. 2000 +/- screen size
; ok for small files, not good for big ones


;;; not used for the moment
(defmethod build-display-array-dynamic ((self sound))
  (let* ((ratio 128)
         (size (om-sound-n-samples self))
         (channels (om-sound-n-channels self))
         ;(array-width (ceiling size ratio))
         )
    (setf (display-ratio self) ratio
          (display-builder self) (om-run-process
                                  "DisplayArrayBuilder"
                                  #'(lambda (snd)
                                      (setf (display-array snd)
                                            (make-array (list channels (ceiling size ratio))
                                                        :element-type 'single-float :initial-element 0.0 :allocation :static))
                                      (fli:with-dynamic-lisp-array-pointer
                                          (ptr (display-array snd) :type :float)
                                        (om-fill-sound-display-array (namestring (filename snd)) ptr ratio))
                                      (sound-get-best-pict snd)
                                      (setf (display-builder self) nil)
                                      (print (format nil "~A Loaded..." (filename self)))) self))))


(defmethod build-display-array ((self sound))
  (let ((winsize 128)
        (format (om-sound-format self))
        (channels (om-sound-n-channels self)))
    (when (and format channels)
      (let ((array-width (ceiling (om-sound-n-samples self) winsize)))
        (setf (display-ratio self) winsize)
      ;"DisplayArrayBuilder"
        (funcall
         #'(lambda (snd)
             (setf (display-array snd)
                   (make-array (list channels array-width)
                               :element-type 'single-float :initial-element 0.0 :allocation :static))
             (fli:with-dynamic-lisp-array-pointer
                 (ptr (display-array snd) :type :float)
               (om-fill-sound-display-array format (namestring (filename snd)) ptr channels array-width winsize))
             (sound-get-best-pict snd)
        ;(setf (display-builder self) nil)
             (print (format nil "~A Loaded..." (filename self))))
         self)))))


(defun om-get-sound-display-array-slice (path nsmp-out start-time end-time)
  #+libsndfile
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    ;;;Initialisation du descripteur
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0)
    (let* (;;;Remplissage du descripteur et affectation aux variables temporaires
           (sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (sr (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ratio (* sr 0.001))
           (start-smp (floor (* start-time sr-ratio)))
           (end-smp (ceiling (* end-time sr-ratio)))
           (size (- end-smp start-smp))
           (channels (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) :type :int :index #+powerpc 1 #-powerpc 0))
           (window (/ size nsmp-out 1.0))
           (window-adaptive (round window))
           ;;;Variables calcul de waveform
           (buffer-size (* (ceiling window) channels))
           (buffer (fli::allocate-foreign-object :type :float :nelems buffer-size))
           (MaxArray (make-array (list channels (min nsmp-out size)) :element-type 'single-float :initial-element 0.0))   ;Tableau pour stocker les max
           (indxmax (1- (min nsmp-out size)))
           (frames-read 0)
           (frames-count 0)
           (winsum 0)
           maxi throw-buffer)
      (when (> start-smp 0)
        (setq throw-buffer (fli::allocate-foreign-object :type :float :nelems (* start-smp channels)))
        (sf::sf-readf-float sndfile-handle throw-buffer start-smp)
        (fli:free-foreign-object throw-buffer))
      (if (> size nsmp-out)
          (loop for indx from 0 do
                (setq winsum (+ winsum window-adaptive))
                (if (> indx 0) (setq window-adaptive (- (round (* (+ 2 indx) window)) (round winsum))))
                (setq frames-read (sf::sf-readf-float sndfile-handle buffer window-adaptive)
                      frames-count (+ frames-count frames-read))
                (dotimes (n channels)
                  (dotimes (i window-adaptive)
                    (setq maxi (max (abs (fli:dereference buffer :type :float :index (+ n (* channels i)))) (or maxi 0.0))))
                  (setf (aref MaxArray n (min indx indxmax)) maxi)
                  (setq maxi 0.0))
                while (and (< frames-count size) (= frames-read window-adaptive)))
        (loop for indx from 0 do
              (setq window-adaptive (max window-adaptive 1)
                    frames-read (sf::sf-readf-float sndfile-handle buffer window-adaptive)
                    frames-count (+ frames-count frames-read))
              (dotimes (n channels)
                (setf (aref MaxArray n (min indx indxmax)) (fli:dereference buffer :type :float :index n)))
              while (and (< frames-count size) (= frames-read window-adaptive))))
      (fli:free-foreign-object buffer)
      (sf::sf_close sndfile-handle)
      MaxArray)))


(defmethod sound-get-display-array-slice ((self sound) nbpix start-time end-time)
  (when (display-array self)
    (let* ((sr (* (om-sound-sample-rate self) 0.001))
           (maxtime (round (om-sound-n-samples self) sr))
           (targettime (- end-time start-time))
           (timeratio (float (/ targettime maxtime 1.0)))
           (win (display-ratio self))
           (maxnbpix (round (* timeratio (cadr (array-dimensions (display-array self))))))
           ;(start-smp (floor (* start-time sr)))
           ;(end-smp (ceiling (* end-time sr)))
           (start (floor (* start-time sr) win))
           (end (ceiling (* end-time sr) win))
           (stoppoint (1- (cadr (array-dimensions (display-array self)))));(+ start (1- maxnbpix)))
           (maxi 0.0)
           result)
      (cond ((= nbpix maxnbpix)
             (setq result (display-array self)))
            ((< nbpix maxnbpix)
             (let* ((step (/ (- end start) nbpix 1.0)))
               (setq result (make-array (list (om-sound-n-channels self) nbpix) :element-type 'single-float :initial-element 0.0))
               (dotimes (c (om-sound-n-channels self))
                 (dotimes (i nbpix)
                   (dotimes (j (round step))
                     (setq maxi (max maxi (aref (display-array self) c (min stoppoint (round (+ start j (* i step))))))))
                   (setf (aref result c i) maxi)
                   (setq maxi 0.0)))
               result))
            ((> nbpix maxnbpix)
             (setq result (om-get-sound-display-array-slice (namestring (filename self)) nbpix start-time end-time))))
      (values result (< (cadr (array-dimensions result)) nbpix)))))

|#
