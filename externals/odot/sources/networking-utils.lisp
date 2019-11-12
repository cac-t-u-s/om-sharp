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

;;; ADDITIONAL FEATURES FOR COMM+

(in-package :comm+)

(defmethod send-message-from-pointer ((socket inet-datagram) msg-pointer msg-size &key  host service)
  "Send message to a socket, using sendto()/send(). message is already a foreign-pointer"
  (declare (type sequence buffer))
  (flet ((send-it (client-addr client-addr-length)
          (let ((socket-fd (socket-datagram-socket socket)))
            (if client-addr
                (%sendto socket-fd msg-pointer msg-size 0
                         (fli:copy-pointer client-addr :type '(:struct sockaddr))
                         client-addr-length)
             (%send socket-fd msg-pointer msg-size 0)))))
    (if (and host service)
      (fli:with-dynamic-foreign-objects ()
        (multiple-value-bind (error address-family client-addr client-addr-length)
            (initialize-dynamic-sockaddr host service "udp")
          (declare (ignore address-family))
          (if error
              (error "cannot resolve hostname ~S, service ~S: ~A"
                     host service error)
            (send-it client-addr client-addr-length))))
      (send-it nil nil))))


(in-package :odot)

(defun osc_send_bundle_s (port host bundle_s)
  (let ((outs (comm+:connect-to-udp-server host port)))
    (comm+::send-message-from-pointer outs (osc_bundle_s_getPtr bundle_s) (osc_bundle_s_getLen bundle_s))
    (comm+:close-datagram outs)
    t))

