;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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
; File author: D. Bouche
;============================================================================

(in-package :om)

;================================================================
; Architecture switch : Multi/Single Thread
;================================================================
;; This code has to be updated according to modifications in the following functions :
;;   - compute (macro)
;;   - cast-computation-list (function)
;;   - schedule (method)
;; Also, it has access to the slot process of the dispatcher.

(defun architecture-switch (multi-thread-p) nil)
