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


(in-package :odot)

;;;================================================
;;; TIME-TAGS
;;;================================================

(cffi:defcstruct _osc_timetag_ntptime (sec :int) (frac_sec :int))
(cffi::defctype t_osc_timetag _osc_timetag_ntptime)

(defun make_osc_timetag (time_sec)
  (cffi::foreign-alloc 
   :int :count 2
   :initial-contents (multiple-value-bind (s fs) (floor time_sec) 
                       (list s (round (* fs 1000))))))

(defun decode_osc_timetag (timetag)
  (let ((s (cffi::mem-aref timetag :int 0))
        (fs (cffi::mem-aref timetag :int 1)))
    (+ s (/ fs 1000.0))))

(cffi::defctype t_osc_err :unsigned-int)


;;;================================================
;;; BINDINGS
;;;================================================


(defconstant MAX_ERR_STRING_LEN 512)
(defconstant OSC_ERR_NONE 0)
(defconstant OSC_ERR_BUNDLETOOSMALL #x1)
(defconstant OSC_ERR_NOBUNDLEID #x2)
(defconstant OSC_ERR_MSGTOOSMALL #x4)
(defconstant OSC_ERR_MALFORMEDADDRESS #x8)
(defconstant OSC_ERR_NOBUNDLE #x10)
(defconstant OSC_ERR_OUTOFMEM #x11)
(defconstant OSC_ERR_NULLPTR #x12)
(defconstant OSC_ERR_BADTYPETAG #x14)
(defconstant OSC_ERR_MALFORMEDMSG #x18)
(defconstant OSC_ERR_INVAL #x20)
(defconstant OSC_ERR_EXPPARSE #x21)
(defconstant OSC_ERR_MSGTOOLARGE #x22)
(defconstant OSC_ERR_INVALIDCHARINADDRESS #x24)
(defconstant OSC_ERR_PARSER_FUNCTIONNOTFOUND #x100)
(defconstant OSC_ERR_EXPR_FUNCTIONNOTFOUND #x101)
(defconstant OSC_ERR_EXPR_ADDRESSUNBOUND #x102)
(defconstant OSC_ERR_EXPR_ARGCHK #x104)
(defconstant OSC_ERR_EXPR_EVAL #x108)
(defconstant OSC_ERR_PARSER #x1000)


(cffi::defcfun ("osc_bundle_u_alloc" osc_bundle_u_alloc) :pointer)
(cffi::defcfun ("osc_bundle_u_free" osc_bundle_u_free) :void (bndl :pointer))
(cffi::defcfun ("osc_bundle_u_addMsg" osc_bundle_u_addMsg) t_osc_err (bndl :pointer) (msg :pointer))
(cffi::defcfun ("osc_bundle_u_serialize" osc_bundle_u_serialize) :pointer (bndl_u :pointer))

(cffi::defcfun ("osc_bundle_s_alloc" osc_bundle_s_alloc) :pointer (len :long) (ptr :pointer))
(cffi::defcfun ("osc_bundle_s_allocEmptyBundle" osc_bundle_s_allocEmptyBundle) :pointer)
(cffi::defcfun ("osc_bundle_s_free" osc_bundle_s_free) :void (bndl :pointer))
(cffi::defcfun ("osc_bundle_s_deepFree" osc_bundle_s_deepFree) :void (bndl :pointer))
(cffi::defcfun ("osc_bundle_s_getLen" osc_bundle_s_getLen) :long (bndl_s :pointer))
(cffi::defcfun ("osc_bundle_s_getPtr" osc_bundle_s_getPtr) :pointer (bndl_s :pointer))
(cffi::defcfun ("osc_bundle_s_setLen" osc_bundle_s_setLen) :void (bndl_s :pointer) (len :long))
(cffi::defcfun ("osc_bundle_s_setPtr" osc_bundle_s_setPtr) :void (bndl_s :pointer) (ptr :pointer))
(cffi::defcfun ("osc_bundle_s_nformat" osc_bundle_s_nformat) :long (buf :pointer) (n :long) (bndllen :long) (bndl :pointer) (nindent :int))
(cffi::defcfun ("osc_bundle_s_getTimetag" osc_bundle_s_getTimetag) t_osc_timetag (len :long) (buf :pointer))
(cffi::defcfun ("osc_bundle_s_setTimetag" osc_bundle_s_setTimetag) :void (len :long) (buf :pointer) (tt t_osc_timetag))
(cffi::defcfun ("osc_bundle_s_deepCopy" osc_bundle_s_deepCopy) t_osc_err (dest :pointer) (src :pointer))
(cffi::defcfun ("osc_bundle_s_deserialize" osc_bundle_s_deserialize) :pointer (len :long) (ptr :pointer))

(cffi::defcfun ("osc_message_u_alloc" osc_message_u_alloc) :pointer)
(cffi::defcfun ("osc_message_u_allocWithAddress" osc_message_u_allocWithAddress) :pointer (address :string))
(cffi::defcfun ("osc_message_u_allocWithString" osc_message_u_allocWithString) :pointer (address :string) (s :string))
(cffi::defcfun ("osc_message_u_allocWithInt" osc_message_u_allocWithInt) :pointer (address :string) (i :int))
(cffi::defcfun ("osc_message_u_allocWithFloat" osc_message_u_allocWithFloat) :pointer (address :string) (f :float))
(cffi::defcfun ("osc_message_u_allocWithDouble" osc_message_u_allocWithDouble) :pointer (address :string) (d :double))
(cffi::defcfun ("osc_message_u_allocWithTimetag" osc_message_u_allocWithTimetag) :pointer (address :string) (tt t_osc_timetag))
(cffi::defcfun ("osc_message_u_setAddress" osc_message_u_setAddress) t_osc_err (m :pointer) (address :string))
(cffi::defcfun ("osc_message_u_appendAtom" osc_message_u_appendAtom) t_osc_err (m :pointer) (a :pointer))
(cffi::defcfun ("osc_message_u_appendInt32" osc_message_u_appendInt32) :pointer (m :pointer) (v :int))
(cffi::defcfun ("osc_message_u_appendFloat" osc_message_u_appendFloat) :pointer (m :pointer) (v :float))
(cffi::defcfun ("osc_message_u_appendDouble" osc_message_u_appendDouble) :pointer (m :pointer) (v :double))
(cffi::defcfun ("osc_message_u_getArgCount" osc_message_u_getArgCount) :int (m :pointer))
(cffi::defcfun ("osc_message_u_getArg" osc_message_u_getArg) :void (m :pointer) (n :int) (atom (:pointer :pointer)))
(cffi::defcfun ("osc_message_u_getSize" osc_message_u_getSize) :int (m :pointer))
(cffi::defcfun ("osc_message_u_getAddress" osc_message_u_getAddress) :string (m :pointer))

(cffi::defcfun ("osc_message_s_alloc" osc_message_s_alloc) :pointer)
(cffi::defcfun ("osc_message_s_free" osc_message_s_free) :void (m :pointer))
(cffi::defcfun ("osc_message_s_deepFree" osc_message_s_deepFree) :void (m :pointer))
(cffi::defcfun ("osc_message_s_initMsg" osc_message_s_initMsg) :void (m :pointer))
(cffi::defcfun ("osc_message_s_copy" osc_message_s_copy) :void (dest (:pointer :pointer)) (src :pointer)) ;; !! dest is a pointer on pointer
(cffi::defcfun ("osc_message_s_getArgCount" osc_message_s_getArgCount) :int (m :pointer))
(cffi::defcfun ("osc_message_s_getArg" osc_message_s_getArg) :void (m :pointer) (n :int) (atom (:pointer :pointer)))
(cffi::defcfun ("osc_message_s_getTypetag" osc_message_s_getTypetag) :char (m :pointer) (n :int))
(cffi::defcfun ("osc_message_s_getSize" osc_message_s_getSize) :int (m :pointer))
(cffi::defcfun ("osc_message_s_getAddress" osc_message_s_getAddress) :string (m :pointer))
(cffi::defcfun ("osc_message_s_deserialize" osc_message_s_deserialize) t_osc_err (msg :pointer) (msg_u (:pointer :pointer)))

(cffi::defcfun ("osc_atom_u_alloc" osc_atom_u_alloc) :pointer)
(cffi::defcfun ("osc_atom_u_allocWithString" osc_atom_u_allocWithString) :pointer (s :string))
(cffi::defcfun ("osc_atom_u_allocWithInt32" osc_atom_u_allocWithInt32) :pointer (i :int))
(cffi::defcfun ("osc_atom_u_allocWithFloat" osc_atom_u_allocWithFloat) :pointer (f :float))
(cffi::defcfun ("osc_atom_u_allocWithDouble" osc_atom_u_allocWithDouble) :pointer (d :double))
(cffi::defcfun ("osc_atom_u_allocWithTimetag" osc_atom_u_allocWithTimetag) :pointer (tt t_osc_timetag))
(cffi::defcfun ("osc_atom_u_allocWithBndl" osc_atom_u_allocWithBndl) :pointer (b :pointer))

(cffi::defcfun ("osc_atom_u_getFloat" osc_atom_u_getFloat) :float (atom :pointer))
(cffi::defcfun ("osc_atom_u_getInt" osc_atom_u_getInt) :int (atom :pointer))
(cffi::defcfun ("osc_atom_u_getTypetag" osc_atom_u_getTypetag) :char (atom :pointer))

(cffi::defcfun ("osc_atom_s_alloc" osc_atom_s_alloc) :pointer)
(cffi::defcfun ("osc_atom_s_free" osc_atom_s_free) :void (atom :pointer))
(cffi::defcfun ("osc_atom_s_getFloat" osc_atom_s_getFloat) :float (atom :pointer))
(cffi::defcfun ("osc_atom_s_getDouble" osc_atom_s_getDouble) :double (atom :pointer))
(cffi::defcfun ("osc_atom_s_getInt" osc_atom_s_getInt) :int (atom :pointer))
(cffi::defcfun ("osc_atom_s_getStringLen" osc_atom_s_getStringLen) :int (atom :pointer))
(cffi::defcfun ("osc_atom_s_getString" osc_atom_s_getString) :int (atom :pointer) (n :int) (out (:pointer :pointer)))
(cffi::defcfun ("osc_atom_s_getTypetag" osc_atom_s_getTypetag) :char (atom :pointer))

(defun osc_atom_s_get_str (atom)
  (let* ((len (odot::osc_atom_s_getStringLen atom))
         (foreign-str (cffi:foreign-alloc :char :count len))
         (foreign-str_PTR (cffi:foreign-alloc :pointer :initial-element foreign-str)))
    (unwind-protect 
       (let ((i (odot::osc_atom_s_getString atom len foreign-str_PTR)))
         (cffi:foreign-string-to-lisp (cffi:mem-ref foreign-str_PTR :pointer) :count len))
      (cffi-sys:foreign-free foreign-str)
      (cffi-sys:foreign-free foreign-str_PTR))))
         
(cffi::defcfun ("osc_bundle_iterator_s_getIterator" osc_bundle_iterator_s_getIterator) :pointer (len :long) (ptr :pointer))
(cffi::defcfun ("osc_bundle_iterator_s_destroyIterator" osc_bundle_iterator_s_destroyIterator) :void (it :pointer))
(cffi::defcfun ("osc_bundle_iterator_s_resetIterator" osc_bundle_iterator_s_resetIterator) :void (it :pointer))
(cffi::defcfun ("osc_bundle_iterator_s_hasNext" osc_bundle_iterator_s_hasNext) :int (it :pointer))
(cffi::defcfun ("osc_bundle_iterator_s_next" osc_bundle_iterator_s_next) :pointer (it :pointer))

(cffi::defcfun ("osc_bundle_iterator_u_getIterator" osc_bundle_iterator_u_getIterator) :pointer (ptr :pointer))
(cffi::defcfun ("osc_bundle_iterator_u_destroyIterator" osc_bundle_iterator_u_destroyIterator) :void (it :pointer))
(cffi::defcfun ("osc_bundle_iterator_u_resetIterator" osc_bundle_iterator_u_resetIterator) :void (it :pointer))
(cffi::defcfun ("osc_bundle_iterator_u_hasNext" osc_bundle_iterator_u_hasNext) :int (it :pointer))
(cffi::defcfun ("osc_bundle_iterator_u_next" osc_bundle_iterator_u_next) :pointer (it :pointer))

(cffi::defcfun ("osc_message_iterator_s_getIterator" osc_message_iterator_s_getIterator) :pointer (m :pointer))
(cffi::defcfun ("osc_message_iterator_s_destroyIterator" osc_message_iterator_s_destroyIterator) :void (it :pointer))
(cffi::defcfun ("osc_message_iterator_s_resetIterator" osc_message_iterator_s_resetIterator) :void (it :pointer))
(cffi::defcfun ("osc_message_iterator_s_hasNext" osc_message_iterator_s_hasNext) :int (it :pointer))
(cffi::defcfun ("osc_message_iterator_s_next" osc_message_iterator_s_next) :pointer (it :pointer))

(cffi::defcfun ("osc_message_iterator_u_getIterator" osc_message_iterator_u_getIterator) :pointer (m :pointer))
(cffi::defcfun ("osc_message_iterator_u_destroyIterator" osc_message_iterator_u_destroyIterator) :void (it :pointer))
(cffi::defcfun ("osc_message_iterator_u_resetIterator" osc_message_iterator_u_resetIterator) :void (it :pointer))
(cffi::defcfun ("osc_message_iterator_u_hasNext" osc_message_iterator_u_hasNext) :int (it :pointer))
(cffi::defcfun ("osc_message_iterator_u_next" osc_message_iterator_u_next) :pointer (it :pointer))


;; useful to have this ?
(cffi::defcstruct _osc_expr (_osc_expr_rec :pointer) (_osc_expr_arg :pointer) (argc :int) (_osc_expr :pointer))
(cffi::defctype t_osc_expr (:struct _osc_expr))

(cffi::defcfun ("osc_expr_alloc" osc_expr_alloc) :pointer)
(cffi::defcfun ("osc_expr_free" osc_expr_free) :void (e :pointer))
(cffi::defcfun ("osc_expr_parser_parseExpr" osc_expr_parser_parseExprSTR) :int (ptr :pointer) (f (:pointer _osc_expr)))

(cffi::defcfun ("osc_expr_parser_parseExpr_jean" osc_expr_parser_parseExprSTR_jean) :pointer (ptr :string))
;(:reference-pass (:ef-mb-string :external-format #+cocoa :macos-roman #-cocoa :latin-1))

(cffi::defcfun ("osc_atom_array_u_free" osc_atom_array_u_free) :void (ar :pointer))


;;;================================================
;;; ENCODE / DECODE 
;;;================================================

(defgeneric osc_make_atom_from_data (data))

(defmethod osc_make_atom_from_data ((data integer)) 
  (odot::osc_atom_u_allocWithInt32 data))
(defmethod osc_make_atom_from_data ((data float)) 
  (odot::osc_atom_u_allocWithFloat data))
(defmethod osc_make_atom_from_data ((data double-float)) 
  (odot::osc_atom_u_allocWithDouble data))
(defmethod osc_make_atom_from_data ((data string)) 
  (odot::osc_atom_u_allocWithString data))
(defmethod osc_make_atom_from_data ((data list)) 
  (odot::osc_atom_u_allocWithBndl (odot::osc_make_foreign_bundle_u data)))
(defmethod osc_make_atom_from_data ((data null)) 
  (odot::osc_atom_u_allocWithInt32 0))
(defmethod osc_make_atom_from_data ((data symbol))
  (if (equal data t) (odot::osc_atom_u_allocWithInt32 1)
    (call-next-method)))

;;; we suppose Lisp BIGNUM = OSC Timetag...
(defmethod osc_make_atom_from_data ((data bignum)) 
  (odot::osc_atom_u_allocWithTimetag (odot::make_osc_timetag data)))

(defmethod osc_make_atom_from_data ((n t)) 
  (print (format nil "Unknown type for OSC atom: ~A (~A)" (type-of n) n))
  nil)


;;; who calls this must free the result with odot::osc_bundle_u_free
(defun osc_make_foreign_bundle_u (data)
  (let ((bndl_u (odot::osc_bundle_u_alloc)))
    (loop for msg in data do
          (let ((msg_u (odot::osc_message_u_allocWithAddress (car msg))))
            (loop for data in (cdr msg) do
                  (let ((atom_u (odot::osc_make_atom_from_data data)))
                    (when atom_u 
                      (odot::osc_message_u_appendAtom msg_u atom_u))))
            (odot::osc_bundle_u_addMsg bndl_u msg_u)))
    bndl_u))

;;; who calls this must free the result with odot::osc_bundle_s_free
(defun osc_make_foreign_bundle_s (data &optional timetag)
  (let ((bndl_u (osc_make_foreign_bundle_u data)))
    (unwind-protect 
        (let ((bndl_s (odot::osc_bundle_u_serialize bndl_u)))
          (when timetag
            (odot::osc_bundle_s_setTimetag 
             (odot::osc_bundle_s_getLen bndl_s) 
             (odot::osc_bundle_s_getPtr bndl_s) 
             (odot::make_osc_timetag timetag)))
          bndl_s)
      (odot::osc_bundle_u_free bndl_u)
      )))


(defun osc_decode_atom_from_s_data (a)
  (case (code-char (odot::osc_atom_s_getTypetag a))
    (#\f (odot::osc_atom_s_getFloat a))
    (#\d (odot::osc_atom_s_getDouble a))
    (#\i (odot::osc_atom_s_getInt a))
    (#\s (odot::osc_atom_s_get_str a))
    (#\T t) (#\F nil) 
    ;;; (#\. (osc_decode_bundle_s_data (odot::osc_atom_s_getBndl_s a)))
    (otherwise :unknown-osc-type)))

(defun osc_decode_bundle_s_data (ptr)
  (let ((b_it_s (odot::osc_bundle_iterator_s_getIterator 
                 (odot::osc_bundle_s_getLen ptr) 
                 (odot::osc_bundle_s_getPtr ptr))))
    (unwind-protect 
        (loop while (= 1 (odot::osc_bundle_iterator_s_hasNext b_it_s)) collect
              (let ((m (odot::osc_bundle_iterator_s_next b_it_s)))
                ;(print (list "MESSAGE n args = " (odot::osc_message_s_getArgCount m)))
                (cons (odot::osc_message_s_getAddress m)
                      (let ((m_it_s (odot::osc_message_iterator_s_getIterator m)))
                        (unwind-protect 
                            (loop while (= 1 (odot::osc_message_iterator_s_hasNext m_it_s)) collect
                                  (odot::osc_decode_atom_from_s_data (odot::osc_message_iterator_s_next m_it_s)))
                          (odot::osc_message_iterator_u_destroyIterator m_it_s))))
                ))
      (odot::osc_bundle_iterator_s_destroyIterator b_it_s))))

;;;================================================
;;; EXPRESSIONS
;;;================================================

(defun osc_expr_parser_parseExpr (exprSTR exprSTRUCT)
  (cffi:with-foreign-string (str exprSTR)
    (osc_expr_parser_parseExprSTR str exprSTRUCT)))

(defun osc_expr_parser_parseExpr_jean (exprSTR)
  (cffi:with-foreign-string (str exprSTR)
    (osc_expr_parser_parseExprSTR_jean str)))
   
(cffi::defcfun ("osc_expr_next" osc_expr_next) :pointer (e (:pointer t_osc_expr)))
(cffi::defcfun ("osc_expr_eval" osc_expr_eval) :int 
  (function :pointer) 
  (len :pointer)
  (oscbundle :pointer)
  (out :pointer))
  

;;; if we want to allocate only one expr 
(defparameter +global_osc_expr+ nil)
(defun init_global_osc_expr ()
  (unless +global_osc_expr+
    (setf +global_osc_expr+ (cffi:foreign-alloc '(:pointer (:struct _osc_expr)))))
  +global_osc_expr+)

(defun make_osc_expr_1 (str)
  (let ((o.expr_PTR (cffi:foreign-alloc :pointer :initial-element (osc_expr_alloc)))) 
    (handler-bind ((error #'(lambda (e)
                              (print e)
                              (free_osc_expr (cffi:mem-ref o.expr_PTR :pointer)))))
      (unwind-protect 
          (let ((err (osc_expr_parser_parseExpr str o.expr_PTR)))
            (if (= err OSC_ERR_NONE)
                (cffi:mem-ref o.expr_PTR :pointer)
              (error (format nil "ERROR parsing o.expression: ~s" str))))
        (cffi-sys:foreign-free o.expr_PTR)
      ))))

(defun make_osc_expr_2 (str)
  (handler-bind ((error #'(lambda (e) (print "ERROR parsing o.expression !")
                            (abort))))
    (osc_expr_parser_parseExpr_jean str)))


(defun free_osc_expr (expr)
  (print (format nil "Free OSC expr: ~A" expr))
  (osc_expr_free expr))


;;; bunle-in will not be freed
;;; bndl-out needs to be freed as well when not needed anymore
(defun execute_osc_expr (str bndl_in)
  (let* ((bndl_out_* (if bndl_in (cffi:foreign-alloc :pointer :initial-element (cffi::null-pointer)))))
    (unwind-protect 
        ;;; protected form
        (let ((bndl_out 
               (if bndl_in
                   (let ((err (osc_bundle_s_deepCopy bndl_out_* bndl_in)))
                     (if (= err OSC_ERR_NONE) (cffi:mem-ref bndl_out_* :pointer)
                       (error "Error copying OSC bundle !!")))
                 (osc_bundle_s_allocEmptyBundle)))
              (osc_expr (make_osc_expr_2 str)))
          (unwind-protect 
              ;;; protected form
              (let ((bndl_out_ptr_* (cffi:foreign-alloc :pointer :initial-element (osc_bundle_s_getPtr bndl_out)))
                    (bndl_out_len_* (cffi:foreign-alloc :long :initial-element (osc_bundle_s_getLen bndl_out)))
                    (out_* (cffi:foreign-alloc :pointer :initial-element (cffi::null-pointer))))
                
                (unwind-protect
                    ;;; protected form
                    (let ((iterator osc_expr)) ;(cffi-sys:make-pointer (cffi-sys:pointer-address o.expr)))
                      (loop while (not (cffi-sys:null-pointer-p iterator))
                            for i from 1 to 100  ;; just in case !
                            do 
                            (print (list "EXPRESSION" i iterator))
                            (let ((err (osc_expr_eval iterator bndl_out_len_* bndl_out_ptr_* out_*)))
                              (unless (= err OSC_ERR_NONE)
                                (error "Error evaluating EXPRESSION #~D!" i)))
                            (osc_atom_array_u_free (cffi:mem-ref out_* :pointer))
                            (setf (cffi:mem-ref out_* :pointer) (cffi::null-pointer))
                            (setf iterator (osc_expr_next iterator)))
                      (print (list "NEW LENGTH" (cffi:mem-ref bndl_out_len_* :long)))
                      
                      (osc_bundle_s_setLen bndl_out (cffi:mem-ref bndl_out_len_* :long))
                      (osc_bundle_s_setPtr bndl_out (cffi:mem-ref bndl_out_ptr_* :pointer))
                      (odot::osc_bundle_s_setTimetag 
                       (odot::osc_bundle_s_getLen bndl_out) 
                       (odot::osc_bundle_s_getPtr bndl_out) 
                       (odot::make_osc_timetag 0.0))
                  )
                  ;;; cleanup forms
                  (osc_atom_array_u_free (cffi:mem-ref out_* :pointer))
                  (cffi-sys:foreign-free out_*)
                  (cffi-sys:foreign-free bndl_out_ptr_*)
                  (cffi-sys:foreign-free bndl_out_len_*)
                  )
                ;;; RETURN VALUE:
                bndl_out)

            ;;; cleanup forms
            (free_osc_expr osc_expr)))
      ;;; cleanup forms
      (when bndl_out_* (cffi-sys:foreign-free bndl_out_*)))
    ))



;;;================================================
;;; MISC./TESTS
;;;================================================


;;; doesn't work
(defun print_bundle_s (bndl_s)
  (let* ((len (osc_bundle_s_getLen bndl_s))
         (tlen (osc_bundle_s_nformat nil 0 len bndl_s 0))
         (text (cffi::foreign-alloc :char :count (1+ tlen))))
    (osc_bundle_s_nformat text (1+ tlen) len bndl_s 0)
    (print (list "print serialized text:" tlen))
    (print (cffi:foreign-string-to-lisp text))
    (cffi-sys:foreign-free text)
    t))


#|
; local style
(let ((o.expr (make_osc_expr "/test = 6")))
  (unwind-protect 
      (print o.expr)
   (free_osc_expr o.expr)))

(let ((o.expr (make_osc_expr "/test = 6")))
  (print o.expr))
|#
    
  
(defun odot_test ()
  (let ((bndl_u (osc_bundle_u_alloc)))
    (unwind-protect 
        (let ()
          (let ((m1 (osc_message_u_alloc))
                (m2 (osc_message_u_allocWithString "/bar" "whatevs"))
                (m3 (osc_message_u_allocWithAddress "/bloo"))
                (a (osc_atom_u_allocWithInt32 12)))
            (osc_message_u_setAddress m1 "/foo/a")
            (osc_message_u_appendFloat m1 3.14)
            (osc_bundle_u_addMsg bndl_u m1)
            (osc_bundle_u_addMsg bndl_u m2)
            (osc_message_u_appendAtom m3 a)
            (osc_bundle_u_addMsg bndl_u m3)
            (print "U_BUNDLE DONE"))

          (let* ((bndl_s (osc_bundle_u_serialize bndl_u))
                 (len (osc_bundle_s_getLen bndl_s))
                 (ptr (osc_bundle_s_getPtr bndl_s)))
            (print "serialize:")
            (print len)
            (print "serialize DONE")
          
            ;(let ((len (cffi::mem-aref len_ptr :long))
            ;      (bndl_uu (cffi::pointer-address (cffi::foreign-alloc :pointer))))
            ;  (cffi::foreign-free len_ptr)
            ;  ;(osc_bundle_s_deserialize len buf bndl_uu)
            ;  (print "DESERIALIZE 2 NOT OK")
            ;  (cffi::foreign-free buf))
            
          ;; iterate over messages in a serialized bundle
          (let ((b_it_s (osc_bundle_iterator_s_getIterator len ptr))
                (aa (osc_atom_s_alloc)))
            (print "S_ITERATOR")
            (loop while (= 1 (osc_bundle_iterator_s_hasNext b_it_s)) do
		(let ((m (osc_bundle_iterator_s_next b_it_s)))
                  (print "------------")
                  (print (osc_message_s_getAddress m))
                  (print (list "n args = " (osc_message_s_getArgCount m)))
                  (let ((m_it_s (osc_message_iterator_s_getIterator m)))
                      (loop while (= 1 (osc_message_iterator_s_hasNext m_it_s)) do
                            (let* ((a (osc_message_iterator_s_next m_it_s))
                                   (type (code-char (osc_atom_s_getTypetag a)))
                                   (val (case type
                                          (#\f (osc_atom_s_getFloat a))
                                          (#\i (osc_atom_s_getInt a))
                                          (#\s (let* ((n (osc_atom_s_getStringLen a))
                                                      (out (cffi:foreign-string-alloc "aaaaaaaa")) ; :char :count n))
                                                      (str nil))
                                                 (print n)
                                                 ;(osc_atom_s_getString a n (cffi::make-pointer (cffi::pointer-address out)))
                                                 ;(dotimes (c n)
                                                 ;  (print (cffi::mem-aref out :char 0)))
                                                 (cffi::foreign-string-free out)
                                                 str))
                                          (otherwise :unknown-osc-type))))
                              (print (list type val))))
                      (osc_message_iterator_u_destroyIterator m_it_s)
                      )))
                  ;(osc_message_s_getArg m 0 aa)
                  ;(print (osc_atom_s_getTypetag aa))
                  ;(print (osc_atom_s_getStringLen aa))
                  ;(print (osc_atom_s_getFloat aa))
                  ;(print (osc_atom_s_getInt aa))
                  
            (osc_bundle_iterator_s_destroyIterator b_it_s)
            (print "S_ITERATOR DONE"))

          ; turn a serialized bundle into printable text
	  (let* ((tlen (osc_bundle_s_nformat nil 0 len bndl_s 0))
                 (text (cffi::foreign-alloc :char :count (1+ tlen))))
            (osc_bundle_s_nformat text (1+ tlen) len bndl_s 0)
            (print (list "print serialized text:" tlen))
            (print (cffi:foreign-string-to-lisp text)))

          ;; iterate over messages in an unserialized bundle
          (let ((b_it_u (osc_bundle_iterator_u_getIterator bndl_u)))
            (print "U_ITERATOR")
            (loop while (= 1 (osc_bundle_iterator_u_hasNext b_it_u)) do
                  (let ((m (osc_bundle_iterator_u_next b_it_u)))
                    (print (format nil "~S has typetags " (osc_message_u_getAddress m)))
		; iterate over atoms in list
                    (let ((m_it_u (osc_message_iterator_u_getIterator m)))
                      (loop while (= 1 (osc_message_iterator_u_hasNext m_it_u)) do
                            (let ((a (osc_message_iterator_u_next m_it_u)))
                              (print (code-char (osc_atom_u_getTypetag a)))))
                      (osc_message_iterator_u_destroyIterator m_it_u)
                      )))
            (osc_bundle_iterator_s_destroyIterator b_it_u)
            (print "U_ITERATOR DONE"))
          
          ;(o.send-bundle_s 3000 "localhost" bndl_s)

          (osc_bundle_s_free bndl_s)
          (print "free bundle_s")
          ))
      
      (osc_bundle_u_free bndl_u)
      (print "free bundle_u")
    )))

; (o.test)

;(om::osc-send '("/test" 3) "localhost" 3000)




#|

#include "osc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "osc_match.h"
#include "osc_bundle_u.h"
#include "osc_message_u.h"
#include "osc_bundle_s.h"
#include "osc_message_s.h"
#include "osc_bundle_iterator_u.h"
#include "osc_bundle_iterator_s.h"
#include "osc_message_iterator_u.h"
#include "osc_parser.h"

int main(int argc, char **argv)
{
	// create a bundle and add messages to it
	t_osc_bndl_u *bndl_u = osc_bundle_u_alloc();
	t_osc_msg_u *m1 = osc_message_u_alloc();
	osc_message_u_setAddress(m1, "/foo");
	osc_message_u_appendFloat(m1, 3.14);
	osc_bundle_u_addMsg(bndl_u, m1);

	t_osc_msg_u *m2 = osc_message_u_allocWithString("/bar", "whatevs");
	osc_bundle_u_addMsg(bndl_u, m2);

	t_osc_msg_u *m3 = osc_message_u_allocWithAddress("/bloo");
	t_osc_atom_u *a = osc_atom_u_allocWithInt32(12);
	osc_message_u_appendAtom(m3, a);
	osc_bundle_u_addMsg(bndl_u, m3);

	// serialize the bundle
	long len = osc_bundle_u_nserialize(NULL, 0, bndl_u);
	char bndl_s[len];
	osc_bundle_u_nserialize(bndl_s, len, bndl_u);

	// free the original unserialized bundle
	osc_bundle_u_free(bndl_u);

	bndl_u = NULL;

	// deserialize the serialized bundle
	osc_bundle_s_deserialize(len, bndl_s, &bndl_u);
	
	// iterate over messages in a serialized bundle
	t_osc_bndl_it_s *b_it_s = osc_bndl_it_s_get(len, bndl_s);
	while(osc_bndl_it_s_hasNext(b_it_s)){
		t_osc_msg_s *m = osc_bndl_it_s_next(b_it_s);
		printf("%s\n", osc_message_s_getAddress(m));
	}
	osc_bndl_it_s_destroy(b_it_s);

	// turn a serialized bundle into printable text
	long tlen = osc_bundle_s_nformat(NULL, 0, len, bndl_s, 0);
	char text[tlen + 1];
	osc_bundle_s_nformat(text, tlen+1, len, bndl_s, 0);
	printf("\nBUNDLE:\n");
	printf("%s\n", text);
	printf("\n");

	// turn text into an unserialized bundle
	t_osc_bndl_u *bndl_u_2 = NULL;
	char *text2 = "/jean : [1, 2, 3], /john : 6.66, /jeremy : \"is cool\"";
	osc_parser_parseString(strlen(text2), text2, &bndl_u_2);

	// iterate over messages in an unserialized bundle
	t_osc_bndl_it_u *b_it_u = osc_bndl_it_u_get(bndl_u_2);
	while(osc_bndl_it_u_hasNext(b_it_u)){
		t_osc_msg_u *m = osc_bndl_it_u_next(b_it_u);
		printf("%s has typetags ", osc_message_u_getAddress(m));
		// iterate over atoms in list
		t_osc_msg_it_u *m_it_u = osc_msg_it_u_get(m);
		while(osc_msg_it_u_hasNext(m_it_u)){
			t_osc_atom_u *a = osc_msg_it_u_next(m_it_u);
			printf("%c", osc_atom_u_getTypetag(a));
		}
		osc_msg_it_u_destroy(m_it_u);
		printf("\n");
	}
	osc_bndl_it_u_destroy(b_it_u);
}

didl::odot::Var Expr::execute( const Var& var )
{
    t_osc_err error;
    bool no_errors = true;
    
    char* scope = var.getBundle();
    long scope_length = var.getBundleLength();
    
    t_osc_bundle_s* source = osc_bundle_s_alloc( scope_length, scope );
    t_osc_bundle_s* copy = nullptr;
    osc_bundle_s_deepCopy( &copy, source );
    char* scope_copy = osc_bundle_s_getPtr( copy );
    osc_bundle_s_free( source );
    osc_bundle_s_free( copy );
    
    t_osc_expr* iterator = expression;
    t_osc_atom_ar_u* out = nullptr;
    while ( iterator ) {
        error = osc_expr_eval( iterator, &scope_length, &scope_copy, &out );

        if ( error != OSC_ERR_NONE ) {
            no_errors = false;
            std::cerr << osc_error_string( error ) << std::endl;
        }
        
        iterator = osc_expr_next( iterator );
        osc_atom_array_u_free( out );
        out = nullptr;
    }
    
    if ( no_errors ) {
        return Var( scope_copy, scope_length );
    }
    
    return Var();
}

|#