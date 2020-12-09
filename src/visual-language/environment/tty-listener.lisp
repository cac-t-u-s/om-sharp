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

(in-package :om)

;; Communication with OM through *terminal-io*
;;
;; listener I/O in tty
;;
(add-preference :general :listener-in-tty "Start TTY listener" :bool nil
                "Start (Stop) a listener in the startup shell"
                'start-stop-tty-listener)

(defvar *tty-listener-running* nil)

(defun start-stop-tty-listener (&optional force)
  (if (or force (get-pref-value :general :listener-in-tty))
      (progn
        (and (not *tty-listener-running*)
             (format *terminal-io* "~&Starting TTY Listener"))
        (lispworks:start-tty-listener)
        (setf *tty-listener-running* t))
    (progn
      (and *tty-listener-running*
           (format *terminal-io* "~&Quitting TTY Listener"))
      (mp:process-terminate (mp:get-process "TTY Listener"))
      (setf *tty-listener-running* nil))))

(add-om-init-fun 'start-stop-tty-listener)
