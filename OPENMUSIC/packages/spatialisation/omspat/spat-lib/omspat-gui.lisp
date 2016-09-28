;/************************************************************************************/
;/*  FILE DESCRIPTION							             */
;/*----------------------------------------------------------------------------------*/
;/*!
; *   @file       omspat-gui.lisp
; *   @brief      lisp-interface to OMSpat.framework GUIs
; *   @author     Thibaut Carpentier & J. Bresson
; *   @version    $(PRODUCT_VERSION)
; *   @date       15/09/2014
; *
; */
;/************************************************************************************/

(in-package :spat)

;;;================================
;;; DATA / ALLOC 
;;;================================

#|
(cffi:defcstruct OmSpatViewerData 
  (sources :pointer) 
  (speakers :pointer))

(defun allocate-spat-viewer-data (sources-pos speakers-pos)
  (let ((data (cffi::foreign-alloc 'OmSpatViewerData)))
    (setf (cffi:foreign-slot-value data 'OmSpatViewerData 'sources) 
          (allocate-spat-points :format FORMAT_XYZ :points sources-pos)
          (cffi:foreign-slot-value data 'OmSpatViewerData 'speakers) 
          (allocate-spat-points :format FORMAT_XYZ :points speakers-pos))
    data))

(defun free-spat-viewer-data (viewer-data)
  (cffi::foreign-free (cffi:foreign-slot-value viewer-data 'OmSpatViewerData 'sources))
  (cffi::foreign-free (cffi:foreign-slot-value viewer-data 'OmSpatViewerData 'speakers))
  (cffi::foreign-free viewer-data))

(defun get-spat-positions (viewer-data)
  (get-spat-points (cffi:foreign-slot-value viewer-data 'OmSpatViewerData 'sources)))

(defun set-spat-positions (viewer-data new-points)
  (let ((pts (cffi:foreign-slot-value viewer-data 'OmSpatViewerData 'sources)))
    (setf (cffi:foreign-slot-value pts 'omspat-points 'data) 
          (points-list-to-buffer new-points (cffi:foreign-slot-value pts 'omspat-points 'format)))
    viewer-data))

(defun get-spat-speakers (viewer-data)
  (get-spat-points (cffi:foreign-slot-value viewer-data 'OmSpatViewerData 'speakers)))
|#

(defun spat-get-view-pointer (view)
  #+macosx(objc::objc-object-pointer (capi-internals::representation view))
  #+windows(win32::hwnd (capi-internals::representation view)))

 
;;;===============================================
;view functions
(cffi:defcfun ("OmSpatCreateSpatViewerWithNSView" OmSpatCreateSpatViewerWithNSView) :pointer (view :pointer) (id :int))
(cffi:defcfun ("OmSpatFreeSpatViewer" OmSpatFreeSpatViewer) :boolean (viewer :pointer))

;data functions
(cffi:defcfun ("OmSpatViewerGetNumSources" OmSpatViewerGetNumSources) :int (viewer :pointer))
(cffi:defcfun ("OmSpatViewerGetNumSpeakers" OmSpatViewerGetNumSpeakers) :int (viewer :pointer))
;(cffi:defcfun ("OmSpatViewerGetData" OmSpatViewerGetData) :boolean (viewer :pointer) (data (:pointer (:struct OmSpatViewerData))))
(cffi:defcfun ("OmSpatViewerIsSourceSelected" OmSpatViewerIsSourceSelected) :boolean 
  (viewer :pointer) (n :unsigned-int) (isSelected :pointer))
(cffi:defcfun ("OmSpatViewerGetSelectedSources" OmSpatViewerGetSelectedSources) :boolean 
  (viewer :pointer) (areSelected :pointer) (numSources :unsigned-int))
(cffi:defcfun ("OmSpatViewerGetSourcePosition" OmSpatViewerGetSourcePosition) :boolean 
  (viewer :pointer) (sourceIndex :unsigned-int) (dest (:pointer (:struct OMSpat-Points))))
(cffi:defcfun ("OmSpatViewerGetSpeakerPosition" OmSpatViewerGetSpeakerPosition) :boolean 
  (viewer :pointer) (sourceIndex :unsigned-int) (dest (:pointer (:struct OMSpat-Points))))

;;; main command
(cffi:defcfun ("OmSpatViewerProcessOSCCommands" OmSpatViewerProcessOSCCommands) :boolean 
  (viewer :pointer)
  (bundle :pointer))

;;;===============================================

(defun spat-source-selected-p (view nb_source)
  (let ((b (cffi::foreign-alloc :boolean)))
    (unwind-protect
        (progn 
          (OmSpatViewerIsSourceSelected view nb_source b)
          (cffi::mem-aref b :boolean))
      (cffi::foreign-free b))))

(defun spat-get-selected-sources (view nb_sources)
   (let ((array (cffi::foreign-alloc :int :count nb_sources)))
     (unwind-protect
         (progn 
           (OmSpatViewerGetSelectedSources view array nb_sources)
           (loop for i from 0 to (1- nb_sources) 
                 collect
                 (not (zerop (cffi:mem-aref array :int i)))))
       (cffi::foreign-free array))))

(defun spat-get-source-position (view src)
  (let* ((spatpoints (allocate-spat-points :size 1)))
    (unwind-protect 
        (progn 
          (OmSpatViewerGetSourcePosition view src spatpoints)
          (get-spat-points spatpoints))
      (OMSpatFreePoints spatpoints))))

(defun spat-get-speaker-position (view spk)
  (let* ((spatpoints (allocate-spat-points :size 1)))
    (unwind-protect 
        (progn 
          (when (and spk (>= spk 0))
            (OmSpatViewerGetSpeakerPosition view spk spatpoints))
          (get-spat-points spatpoints))
      (OMSpatFreePoints spatpoints))))

;;;===============================================
(defconstant kSourcePositionChanged 0)
(defconstant kSourceApertureChanged 1)
(defconstant kSourceYawChanged 2)
(defconstant kSpeakerPositionChanged 3)
(defconstant kSourceSelectionChanged 4)
(defconstant kSourceDoubleClicked 5)

;;; Callback is a function with 3 argument
;;; componentId : ID of the spat.viewer from which the callback emanates
;;; paramType : the type of parameter that changed
;;; index : index of the element (source or speaker) that changed. (or -1 if this is not relevant)

;;;(cffi::defcallback notify-change :void ((id :int) (param-type :int) (n :int)) 
;;;  (handler-bind ((error #'(lambda (e) (print e))))
;;;    (spat-view-changed-callback id param-type n)
;;;    ))

(cffi::defcallback notify-change :void ((id :int) (bundle :pointer)) 
  (handler-bind ((error #'(lambda (e) (print e))))
    (print (om::decode-bundle-s-pointer-data bundle))
    ;(spat-view-changed-callback id param-type n)
    ))

;;; to be redefined
(defun spat-view-changed-callback (id param-type n) t)

(cffi:defcfun ("OmSpatViewerRegisterOscCallback" OmSpatViewerRegisterOscCallback) :boolean (ptr :pointer) (callback :pointer))

(defun spat-view-register-callback (viewer)
  (spat::OmSpatViewerRegisterOscCallback viewer (cffi::get-callback 'notify-change)))

;;;===============================================


;;;; TEST FUNS
#|

(om-spat-initialize)

(defparameter *vhdl* nil)

(defun viewer-osc-command (viewerhdl messages)
  (let ((ob (om::make-o.bundle (make-instance 'om::osc-bundle :messages (print messages)))))
    (spat::OmSpatViewerProcessOSCCommands viewerhdl (om::o.bundle-ptr ob) (om::o.bundle-size ob))))

(defun test-win ()
  (let ((win (oa::om-make-window 'oa::om-window))
        (view (oa::om-make-view 'oa::om-view :bg-color (oa::om-def-color :gray) ;; oa::*om-gray-color*
                                :size (oa::om-make-point 300 300))))
    (oa::om-add-subviews win view)
    (oa::om-show-window win)
    (sleep 1)
    (setf *vhdl* (OmSpatCreateSpatViewerWithNSView (spat-get-view-pointer view) 1))
    win
    ))

; (test-win)
; (viewer-osc-command *vhdl* '(("/om/window/size" 300 300)))
; (viewer-osc-command *vhdl* '(("/source/number" 4)))
; (OmSpatViewerGetNumSources *vhdl*) 
; (viewer-osc-command *vhdl* '(("/speaker/number" 2)))
; (viewer-osc-command *vhdl* '(("/layout" "simple")))

(defun test-osc-command (messages)
  (let ((ob (om::make-o.bundle (make-instance 'om::osc-bundle :messages (print messages)))))
    (spat::OmSpatDebugOSCPacket (om::o.bundle-ptr ob) (om::o.bundle-size ob))))


(test-osc-command '(("/numsources" 4)))

|#


                     
        
