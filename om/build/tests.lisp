;;; OpenMusic tests file

(in-package :om)

(defparameter *soundfile1* "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/Bassclarinet1.aif")
(defparameter *soundfile2* "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/add-synth-4ch-sas.aiff")

; (test-play-sound)

(defun add-sound-box (view soundfile pos) 
  (let ((sound (om-init-instance (make-instance 'sound) 
                                 `((:file ,soundfile)
                                   (:access-from-file nil))))
        (box (omng-make-new-boxcall (find-class 'sound) pos)))
    (setf (value box) (list sound))
    (setf (display box) :mini-view)
    (add-box-in-patch-editor box view)
    box))

(defun play-sound-box (box)
  (let ((dur (ms->sec (get-obj-dur (car (value box))))))
    (play-boxes (list box))
    (sleep (+ dur .5))))

(defun test-play-sound ()
  (print "test-play-sound")
  (let* ((patch (open-new-document :patch))
         (patchview (main-view (editor patch)))
         (box1 (add-sound-box patchview *soundfile1* (omp 200 200)))
         (box2 (add-sound-box patchview *soundfile2* (omp 300 200))))           
    (sleep 0.5)
    (play-sound-box box1)
    (play-sound-box box2)
    ))
   
(defun test-om-spat ()
  (print "test-om-spat")
  (open-doc-from-file :patch "/Users/bresson/SRC/OM7/IRCAM-FORGE/om7-patches/om-spat/om-spat-spat-scene.opat"))

(defun test-symbolist ()
  (print "test-symbolist")
  (open-doc-from-file :patch "/Users/bresson/SRC/symbolist/OM/symbolist/patches/symbolist-test.opat"))