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
; File author: J. Bresson
;============================================================================

(in-package :om)

;;;================
;;; OSC+UDP WRAPPERS
;;;================

(defun osc-decode (msg)
  (osc::decode-message-or-bundle msg))

(defun osc-encode-message (message)
  (apply #'osc::encode-message message))

(defun osc-encode-bundle (bundle)
  (osc::encode-bundle bundle))

(defun osc-send-message (port host message)
  (om-send-udp port host (apply #'osc::encode-message message)))

(defun osc-send-bundle (port host bundle &optional time-tag)
  (om-send-udp port host (osc::encode-bundle bundle (or time-tag :now))))

(defun process-osc-bundle (bundle msg-processing-fun)
  (let ((tt nil))
    (if (or (and (arrayp (car bundle)) (not (stringp (car bundle))))
            (consp (car bundle)))
        ;;; LOOKS LIKE OSC...
        (loop for item in bundle
              unless (and (arrayp item) (not (stringp item)) (setf tt item)) ;;; THIS IS THE TIME TAG
              collect
              (if (and (consp item) (consp (car item))) ;;; THE FIRST ELEMENT OF THE MESSAGE IS ANOTHER MESSAGE
                  (process-osc-bundle (if tt (cons tt (car item)) (car item)) msg-processing-fun)
                (process-message item msg-processing-fun))
              )
      ;;; NOT OSC
      (list (process-message bundle msg-processing-fun)))))



;;==========
;; OSC SEND / RECEIVE
;;==========

(defmethod* osc-send (bundle host port)
  :icon 'osc
  :initvals '(("/test" 0) nil nil)
  :indoc '("OSC message" "IP address" "port number")
  :doc "Sends the an OSC message pf bundle (<bundle>) to the port <port> of <host>.

An OSC message consists of a string (URL-style symbolic naming) followed by numerical parameters. Its is formatted as a list.
See http://opensoundcontrol.org/introduction-osc

<bundle> can also contain a list of messages (list of lists) to be sent simultaneously, or an object of type OSC-BUNDLE.

Notes:
- If NIL, <host> and <port> values are taken from the OSC preferences.
- 127.0.0.1 is the 'localhost', i.e. the message is sent to the local computer.
"
  (osc-send-bundle
   (or port (get-pref-value :osc :out-port))
   (or host (get-pref-value :osc :out-host))
   bundle))


(defmethod* osc-receive (port msg-processing &optional host)
  :icon 'osc
  :indoc '("port number" "incoming message processing patch" "an IP address")
  :initvals '(nil nil nil)
  :doc "A local server receiving OSC.

Use 'R' to set the box reactive and activate/deactivate the server.

When the server is on, OSC-RECEIVE waits for OSC messages on port <port> and calls <msg-processing> with the decoded message as parameter.

<msg-processing> must be a patch in mode 'lambda' with 1 input corresponding to an OSC message.
This patch should handle and process the incoming messages.

- If NIL <port> value is taken from the OSC preferences.

By default the server listen to ANY addresses (localhost and IP address). Set <host> to listen only to the messages sent from the network to the speficic address.
"
  t)


(defmethod boxclass-from-function-name ((self (eql 'osc-receive))) 'OMReceiveBox)

(defmethod start-receive-process ((self (eql 'osc-receive))) 'osc-start-receive)
(defmethod stop-receive-process ((self (eql 'osc-receive))) 'udp-stop-receive)

;;;========================================
;;; oSC features
;;;========================================
;;; OSC-RECEIVE PROCESSES ALL THE MESSAGES IN THE BUNDLE ONE BY ONE

(defun osc-start-receive (box args)
  (let ((port (or (car args) (get-pref-value :osc :in-port)))
        (fun (cadr args))
        (host (caddr args)))
    (if (and port (numberp port))
        (let ((process (om-start-udp-server
                        port host
                        #'(lambda (msg)
                            (let* ((message (osc-decode msg))
                                   (delivered (process-osc-bundle message fun)))
                              (when delivered
                                (set-delivered-value box (reverse delivered))))
                            nil)
                        nil box)))
          (when process
            (om-print (format nil "Start OSC receive server on ~A ~D" host port))
            process))

      (om-beep-msg (format nil "Error - bad port number for OSC-RECEIVE: ~A" port)))))

