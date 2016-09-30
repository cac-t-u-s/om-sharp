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

(defun spat-get-view-pointer (view)
  #+macosx(objc::objc-object-pointer (capi-internals::representation view))
  #+windows(win32::hwnd (capi-internals::representation view)))

;;;===============================================
(cffi:defcfun ("OmSpatCreateComponentWithType" OmSpatCreateComponentWithType) :pointer (type :string))
(cffi:defcfun ("OmSpatFreeComponent" OmSpatFreeComponent) :boolean (component :pointer))
(cffi:defcfun ("OmSpatGetComponentType" OmSpatGetComponentType) :string (component :pointer))
(cffi:defcfun ("OmSpatIsDspComponent" OmSpatIsDspComponent) :boolean (component :pointer))
(cffi:defcfun ("OmSpatIsGuiComponent" OmSpatIsGuiComponent) :boolean (component :pointer))
(cffi:defcfun ("OmSpatInstallComponentInNSView" OmSpatInstallComponentInNSView) :boolean (component :pointer) (view :pointer))
(cffi:defcfun ("OmSpatGetCurrentStateAsOscBundle" OmSpatGetCurrentStateAsOscBundle) :pointer (component :pointer))
(cffi:defcfun ("OmSpatProcessOscCommands" OmSpatProcessOscCommands) :boolean (component :pointer)(bundle :pointer))
(cffi:defcfun ("OmSpatRegisterOscCallback" OmSpatRegisterOscCallback) :boolean (component :pointer) (callback :pointer))

(defun spat-component-register-callback (component)
  (spat::OmSpatRegisterOscCallback component (cffi::get-callback 'spat-component-callback)))

(cffi::defcallback spat-component-callback :void ((component :pointer) (bundle :pointer)) 
  (handler-bind ((error #'(lambda (e) (print (format nil "ERROR IN SPAT CALLBACK: ~% ~A" e)))))
    (spat-component-handle-callback component bundle)))

;;; to be redefined
(defun spat-component-handle-callback (component-ptr bundle-ptr) 
  (print "Callback undefied"))
  
;;; OLD FUNCTIONS
;(cffi:defcfun ("OmSpatViewerGetNumSources" OmSpatViewerGetNumSources) :int (viewer :pointer))
;(cffi:defcfun ("OmSpatViewerGetNumSpeakers" OmSpatViewerGetNumSpeakers) :int (viewer :pointer))
;(cffi:defcfun ("OmSpatViewerIsSourceSelected" OmSpatViewerIsSourceSelected) :boolean 
;  (viewer :pointer) (n :unsigned-int) (isSelected :pointer))
;(cffi:defcfun ("OmSpatViewerGetSelectedSources" OmSpatViewerGetSelectedSources) :boolean 
;  (viewer :pointer) (areSelected :pointer) (numSources :unsigned-int))
;(cffi:defcfun ("OmSpatViewerGetSourcePosition" OmSpatViewerGetSourcePosition) :boolean 
;  (viewer :pointer) (sourceIndex :unsigned-int) (dest (:pointer (:struct OMSpat-Points))))
;(cffi:defcfun ("OmSpatViewerGetSpeakerPosition" OmSpatViewerGetSpeakerPosition) :boolean 
;  (viewer :pointer) (sourceIndex :unsigned-int) (dest (:pointer (:struct OMSpat-Points))))

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


