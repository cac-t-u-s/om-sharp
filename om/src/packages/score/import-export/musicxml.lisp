(in-package :om)


(defun om-list-from-xml (stream)
  #+xml(s-xml::parse-xml-dom stream :lxml)
  #-xml(om-beep-msg "No support for XML"))
