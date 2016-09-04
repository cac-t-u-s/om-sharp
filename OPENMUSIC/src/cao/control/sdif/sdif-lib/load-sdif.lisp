;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;Macintosh version (Digitool Macintosh Common Lisp - MCL)
;
;Copyright (C) 2004 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Jean Bresson and Augusto Agon
;;===========================================================================

;;===========================================================================
;DocFile
;loads the SDIF API
;;===========================================================================

(in-package :cl-user)
      
(defpackage "SDIF-PACKAGE"
  (:nicknames "SDIF")
   (:use :common-lisp :cffi))
    
(defvar sdif::*sdif-library* nil)
                              
(compile&load (merge-pathnames "sdif" *load-pathname*))
(compile&load (merge-pathnames "sdif-api" *load-pathname*))

(pushnew :sdif *features*)

;;;==============================
;;; CHARGEMENT



;;; !!! *sdif-pathname* is modified in OM

(defun load-sdif-lib ()
  (setf sdif::*sdif-library*
        (om-fi::om-load-foreign-library  
         "SDIF"
         `((:macosx (:or 
                     ,(om-fi::om-foreign-library-pathname "SDIF.framework/SDIF")
                     (:framework "SDIF")
                     ))
           (:windows (:or ,(om-fi::om-foreign-library-pathname "sdif.dll")
                      (:default "sdif")))
           (:unix (:or "/usr/local/lib/libsdif.so" "libsdif.so" ,(om-fi::om-foreign-library-pathname "libsdif")))
           (t (:default "libsdif"))))))

(om-fi::add-foreign-loader 'load-sdif-lib)





