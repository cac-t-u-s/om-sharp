
(in-package :om)

(load (om-relative-path '("lib-t") "lib-t"))

(defun libt-call (o.bundle)
  (when (bundle_s o.bundle)
    (let ((newptr (lib-t::t_req_s (oa::om-pointer-ptr (bundle_s o.bundle))))
          (out (make-instance 'o.bundle)))
      (assign-foreign-pointer out newptr)
      out)))
