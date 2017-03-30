;===============================================================================
;SYSTEM BOXES
;===============================================================================

(in-package :om)

(add-preference :general :print-system-output "Print System Outputs" :bool t "(Redirects command-line system outputs to the OM Listener)")

(defmethod* om-shell ((command-line string) &key (open-shell-window nil))
  :icon 'om-shell
  :indoc '("a system command line")
  :initvals '("")
  :doc "Sends <command-line> (a string) to the system. 

If <open-shell-window> the command will be executed in a special OM shell window. Otherwise the output is printed in the Listener window."
  (if open-shell-window
      (progn 
        (om-open-shell)
        (om-send-command-to-shell command-line))
    (om-cmd-line command-line)))

(defun om-cmd-line (str)
  (oa::om-command-line str (om::get-pref-value :general :print-system-output)))
