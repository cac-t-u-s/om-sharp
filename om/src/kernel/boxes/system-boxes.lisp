;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
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
; File author: J. Bresson
;============================================================================

;===============================================================================
;SYSTEM BOXES
;===============================================================================

(in-package :om)

(add-preference :general :print-system-output "Print System Outputs" :bool t "Redirect command-line system outputs to the Listener")

(defmethod* om-shell ((command-line string) &key (open-shell-window nil))
  :icon 'shell
  :indoc '("a system command line")
  :initvals '("")
  :doc "Sends <command-line> (a string) to the system. 

If <open-shell-window> the command will be executed in a special shell window. Otherwise the output is printed in the Listener window."
  (if open-shell-window
      (progn 
        (om-lisp::om-open-shell)
        (om-lisp::om-send-command-to-shell command-line))
    (om-cmd-line command-line)))

(defun om-cmd-line (str)
  (oa::om-command-line str (om::get-pref-value :general :print-system-output)))
