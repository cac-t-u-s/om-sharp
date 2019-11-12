;============================================================================
; o.OM : odot OSC interface in OM
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


(in-package :cl-user)

(defpackage :odot
  (:use :common-lisp :cl-user))

(defvar odot::libo nil)

(defun load-o-lib ()
  (setf odot::libo
        (om-fi::om-load-foreign-library 
         "O."
         `((:macosx ,(merge-pathnames 
                      "lib/mac/libo.dylib" 
                      (om::mypathname (om::find-library "odot"))))
           (t (:default "libo"))))))

;; load now
(load-o-lib)

;; load at OM startup
;; #+macosx(om-fi::add-foreign-loader 'load-o-lib)

(push :odot *features*)
