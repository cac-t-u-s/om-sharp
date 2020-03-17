;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2020)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
; File author: A. Vinjar
;============================================================================

;;; Load swank w. contribs.  The actual call to start the swank server is in a
;;; function added to om#'s *init-func-list* (controllable by user-prefs) inside
;;; environment/om-swank.lisp.

(load (merge-pathnames "slime/swank-loader.lisp" *load-pathname*))

(setq swank-loader::*fasl-directory* (merge-pathnames "fasl/" *load-pathname*))

(swank-loader:init :setup nil :load-contribs t)
