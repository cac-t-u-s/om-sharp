;/************************************************************************************/
;/*  FILE DESCRIPTION							             */
;/*----------------------------------------------------------------------------------*/
;/*!
; *   @file       omspat-dsp.lisp
; *   @brief      lisp-interface to OMSpat.framework DSP
; *   @author     Thibaut Carpentier
; *   @version    $(PRODUCT_VERSION)
; *   @date       05/11/2013
; *
; */
;/************************************************************************************/

; (cffi::define-foreign-library :LIBOMSPAT (:macosx "/Users/bresson/SRC/OM7/OPENMUSIC/resources/lib/mac/OMSpat.framework/OMSpat"))
; (cffi::load-foreign-library :LIBOMSPAT)

(in-package :spat)

;;;========================================
;;; AUDIOBUFFER
;;;========================================

(cffi:defcstruct omspat-audiobuffer 
  (numChannels :unsigned-int) 
  (numSamples :unsigned-long)
  (data :pointer))

(cffi::defctype spat-audiobuffer omspat-audiobuffer)
(cffi::defctype OmSpatPanning :pointer)

;;; we suppose that **DATA is already allocated
(defun allocate-spat-audiobuffer (&key (size nil size-supplied-p) 
                                       (channels nil channels-supplied-p) 
                                       (data nil data-supplied-p))
  (let ((audiob (cffi::foreign-alloc 'spat-audiobuffer)))
    (when channels (setf (cffi:foreign-slot-value audiob 'omspat-audiobuffer 'numChannels) channels))
    (when size (setf (cffi:foreign-slot-value audiob 'omspat-audiobuffer 'numSamples) size))
    (when data (setf (cffi:foreign-slot-value audiob 'omspat-audiobuffer 'data) data))
    audiob))

;;; we do not free DATA
(defun free-spat-audiobuffer (audiob)
  ;;;(cffi::foreign-free (cffi:foreign-slot-value audiob '(:struct spat::omspat-audiobuffer) 'data))
  (cffi::foreign-free audiob))

(defun spat-audiobuffer-set-data (audiob ptr)
  (setf (cffi:foreign-slot-value audiob '(:struct spat::omspat-audiobuffer) 'data) ptr))

(defun spat-audiobuffer-get-data (audiob)
  (cffi:foreign-slot-value audiob '(:struct spat::omspat-audiobuffer) 'data))

(defun spat-audiobuffer-get-n-samples (audiob)
  (cffi:foreign-slot-value audiob '(:struct spat::omspat-audiobuffer) 'numsamples))

; Prints the content of an OmAudioBuffer
;;; IN LISP / LISTENER
(defun om-print-audiobuffer (buffer)
  (format nil "OmSpatAudioBuffer - numChannels = ~D - numSamples = ~D"
          (cffi:foreign-slot-value buffer 'omspat-audiobuffer 'numChannels)
          (cffi:foreign-slot-value buffer 'omspat-audiobuffer 'numSamples)))

;;;======================================================
(cffi:defcfun ("OmSpatResizeAudioBuffer" OmSpatResizeAudioBuffer) :boolean 
  (buffer omspat-audiobuffer) (numChannels :unsigned-int) (numSamples :unsigned-long))
(cffi:defcfun ("OmSpatFreeAudioBuffer" OmSpatFreeAudioBuffer) :boolean 
  (buffer omspat-audiobuffer))

(cffi:defcfun ("OmSpatLoadAudioBufferFromFile" OmSpatLoadAudioBufferFromFile) :boolean 
  (buffer omspat-audiobuffer) (filename :string))
(cffi:defcfun ("OmSpatSaveAudioBufferToFile" OmSpatSaveAudioBufferToFile) :boolean 
  (buffer omspat-audiobuffer) (filename :string) (samplerate :float) (bitpersample :int) (overwrite :boolean))
(cffi:defcfun ("OmSpatPrintAudioBuffer" OmSpatPrintAudioBuffer) :boolean 
  (buffer omspat-audiobuffer) (printData :boolean))

;;=======================================================
;; in-place operations on audio buffers
;; set channelIndex = -1 to apply to all channels
;;=======================================================
(cffi:defcfun ("OmSpatReverseAudioBuffer" OmSpatReverseAudioBuffer) :boolean 
  (srcDest omspat-audiobuffer) (channelIndex :int))
(cffi:defcfun ("OmSpatApplyGainToAudioBuffer" OmSpatApplyGainToAudioBuffer) :boolean 
  (srcDest omspat-audiobuffer) (gain :float) (channelIndex :int))
(cffi:defcfun ("OmSpatFillAudioBuffer" OmSpatFillAudioBuffer) :boolean 
  (srcDest omspat-audiobuffer) (value :float) (channelIndex :int))
(cffi:defcfun ("OmSpatNormalizeAudioBuffer" OmSpatNormalizeAudioBuffer) :boolean 
  (srcDest omspat-audiobuffer) (channelIndex :int) (newMaximumIndB :float))
(cffi:defcfun ("OmSpatApplyFadeToAudioBuffer" OmSpatApplyFadeToAudioBuffer) :boolean 
  (srcDest omspat-audiobuffer) (numSamplesForFadeIn :long) (numSamplesForFadeOut :long) (channelIndex :int))


;;;=======================================================
;;; SPATIALIZE
;;;=======================================================

(cffi:defcfun ("OmSpatCreateSpatPan" OmSpatCreateSpatPanSTR) OmSpatPanning 
  (numInputs :unsigned-int) 
  (numOutputs :unsigned-int)
  (panningType :string))

(defun OmSpatCreateSpatPan (numInputs numOutputs panningType)
 (cffi:with-foreign-string (s panningType)
   (OmSpatCreateSpatPanSTR numInputs numOutputs s)))

(cffi:defcfun ("OmSpatFreeSpatPan" OmSpatFreeSpatPan) :boolean (obj OmSpatPanning))

(cffi:defcfun ("OmSpatPanProcessOSCCommands" OmSpatPanProcessOSCCommands) :boolean 
  (obj OmSpatPanning)
  (content :pointer)
  (size :unsigned-long))

(cffi:defcfun ("OmSpatPanProcess" OmSpatPanProcess) :boolean 
  (obj OmSpatPanning)
  (out omspat-audiobuffer) 
  (in omspat-audiobuffer)
  (numSamplesToProcess :unsigned-long))


;;; in-buffer and out-buffer come from / is returned to the environment
;;; all the rest is freed 
(defmethod spat-spatialize-buffer (in-buffer n-samples-in n-sources-in n-channels-out osc-bundle bundle-size panningtype)
  (let* ((out-buffer (fli:allocate-foreign-object :type :pointer :nelems n-channels-out))
         (spat (OmSpatCreateSpatPan n-sources-in n-channels-out panningtype)))
    ;;; allocate the out buffer channels
    (dotimes (ch n-channels-out)
            (setf (cffi::mem-aref out-buffer :pointer ch) 
                  (fli:allocate-foreign-object :type :float :nelems n-samples-in)))
    
    (let ((spat-in (allocate-spat-audiobuffer :channels n-sources-in :size n-samples-in :data in-buffer))
          (spat-out (allocate-spat-audiobuffer :channels n-channels-out :size n-samples-in :data out-buffer)))
      
      (unwind-protect 
          (handler-bind ((error #'(lambda (e) 
                                    (print (format nil "~A" e))
                                    (print (OmSpatGetLastError))
                                    (OmSpatClearLastError)
                                    (abort e))))
            (unless (or (null osc-bundle)
                        (OmSpatPanProcessOSCCommands spat osc-bundle bundle-size))
              (error "ERROR IN SPAT CONTROL PROCESSING"))
            (unless (OmSpatPanProcess spat spat-out spat-in n-samples-in)
              (error "ERROR IN SPAT AMPLITUDE PANNING:"))
            out-buffer)
        ;;;; cleanup forms
        (OmSpatFreeSpatPan spat)
        (free-spat-audiobuffer spat-in)
        (free-spat-audiobuffer spat-out)
        )
      )))



