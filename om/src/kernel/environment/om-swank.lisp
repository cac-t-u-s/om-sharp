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

;;; 
;;; slime/swank I/O om# <-> emacs
;;;
;;; This file is to enable preference items to control the swank server running
;;; inside om#.  Slime/Swank setup is inside src/api/om-slime/om-slime.lisp
;;; 
;;; Start swank server to communicate with om# from within Emacs (or other
;;; pipes).  Default port - 4005 - is used here:
;;; 

(add-preference :general :om-swank-server "Start Swank server" :bool t
		"Enable Swank server (in Emacs: 'M-x slime-connect RET RET')"
		'start-swank-server)

(defun start-swank-server ()    ;hook called after om# init
  (when (get-pref-value :general :om-swank-server)
    (swank::init)
    (swank:create-server)))

(add-om-init-fun 'start-swank-server)
