;============================================================================
; om#: visual programming language for computer-assisted music composition
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


(in-package :om)

(add-preference-module :osc "OSC")

(add-preference-section :osc "OSC In/Out")

(add-preference :osc :out-port "Default Output port" (make-number-in-range :min 0 :max 10000 :decimals 0)
                3000
                "OSC messages are sent by default to this UDP port of the receiver host")
(add-preference :osc :out-host "Default Output receiver host" :string
                "127.0.0.1"
                "IP address of the receiver host")
(add-preference :osc :in-port "Default Input port" (make-number-in-range :min 0 :max 10000 :decimals 0)
                3000
                "Default port for OM# toos and editors receiving OSC.")
