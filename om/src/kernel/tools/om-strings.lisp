;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
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

;;;=======================
;;; STRING UTILITIES FOR OM
;;;=======================

(in-package :om)


(defun string+ (&rest strings) (eval `(concatenate 'string ,.strings)))

(defun om-read-list-from-string (string &optional (pos 0))
  (block nil
    (handler-bind ((error #'(lambda (e) (om-beep-msg "read error: ~A" (type-of e)) 
                              (return nil))))
      (multiple-value-bind (val pos) 
          (read-from-string string nil :eof :start pos)
        (if (eql val :eof)
            nil
          (cons val (om-read-list-from-string string pos)))))))
  
(defun om-text-to-lines (text)
  (let ((p2 (position #\Newline text)) 
        (rep nil))
        (loop while p2 do
              (push (subseq text 0 p2) rep)
              (setf text (subseq text (+ p2 1)))
              (setf p2 (position #\Newline text)))
        (push text rep)
        (reverse rep)))

(defun string-until-char (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

(defun string-to-list (string &optional (separator " "))
  (when string 
    (multiple-value-bind (token rest)
        (string-until-char string separator)
      (cons token (string-to-list rest separator)))))

(defun string-lines-to-list (string &optional (separator " "))
  (string-to-list string (string #\Newline)))

(defun number-to-string (num &optional decimals)
  (if decimals
      (format nil (format nil "~~,~DF" decimals) num)
    (format nil "~D" num)))

(defun read-number (str)
   (if (equal str "") nil
     (let ((rep (read-from-string str)))
       (if (numberp rep) rep nil))))


(defun string-to-number (string)
  (and (stringp string) (read-from-string string)))


(defun string-until-space (string)
  (let ((index (search " " string)))
    (if index (subseq string 0 index) string)))


(defun delete-spaces (string)
   (let ((pos (position-if #'(lambda (x) (and 
                                          (not (equal x #\Linefeed))
                                          (not (equal x #\Space))
                                          (not (equal x #\Tab)))) string)))
     (if pos
       (subseq string pos)
       "")))

;=======================
; FILE READER
;=======================

(defun list-from-file (pathname)
  (let ((rep-list nil))
    (with-open-file (in pathname :direction :input :if-does-not-exist nil)
      (let ((line (read in nil :eof)))
        (loop while (and line (not (equal line :eof))) do 
              (setf rep-list (append rep-list (list line))
                    line (read in nil :eof)))))
    rep-list))

(defun lines-from-file (pathname)
  (with-open-file (in pathname :direction :input :if-does-not-exist nil)
    (let ((line (read-line in nil :eof)))
      (loop while (and line (not (equal line :eof))) 
            collect line
            do (setf line (read-line in nil :eof))))))

;=======================
; STRING TO SYMBOLS
;=======================

(defun intern-om (string) (intern (string-upcase string) :om))
(defun intern-pack (string pck) (intern (string-upcase string) pck))
(defun intern-k (string) (intern (string-upcase string) :keyword))

;=======================
; cross-platform / encoding tools
;=======================

(defun str-check (string)
  (let ((pos nil))
    (loop for char-switch in (get-switch-chars) do
          (loop while (setf pos (position (code-char (car char-switch)) string)) do
                (replace string (string (code-char (cadr char-switch))) :start1 pos)))
    ;; unknown chars... a chercher
    (loop for ch in *unknown-chars* do
          (loop while (setf pos (position ch string)) do   
                (replace string "?" :start1 pos)))
    string))

;; multiple tabs
;(setf string (remove-duplicates string :test #'(lambda (x y) (equal x #\Tab))))

(defun get-switch-chars ()
  #+win32
  '((381 233) ;; é
    (136 224) ;; à
    (144 234) ;; ê
    (143 232) ;; è
    (157 249) ;; ù
    (153 244) ;; ô
    (148 238) ;; î
    (0 231)   ;; ç
    (8217 39) ;; '
    )
  #-win32
  '((142 233) ;; é
    (136 224) ;; à
    (144 243) ;; ê
    (143 232) (768 232) ;; è     ;; #\U+0300
    (157 249) ;; ù
    (153 244) ;; ô
    (148 238) ;; î
    (139 227) ;; ã     ;; #\U+008B
    (141 231) ;; ç
    (135 225) ;; á     ;; #\U+0087
    (146 237) ;; í     ;; #\U+0092
    (8217 39) ;; '
    ))


(defvar *unknown-chars* '(#\U+0080 #\U+0081 #\U+0082 #\U+0083 #\U+0084 #\U+0085 #\U+0086 #\U+0088 #\U+0089 #\U+008A 
                                   #\U+008C #\U+008D #\U+008E #\U+008F #\U+0090 #\U+0091 #\U+0093 #\U+0094 #\U+0095 
                                   #\U+0096 #\U+0097 #\U+0098 #\U+0099 #\U+009A #\U+009B #\U+009C #\U+009D #\U+009E #\U+009F))

; (string #\U+0097)
; (code-char 135)
; (print (remove-duplicates (mapcar 'code-char (om::arithm-ser 0 10000 1)) :test 'equal))
; (char-code (elt "bamos" 1))
; (char-code #\U+008B)
; (char-code #\U+0087)

; (char-code #\) (char-code #\')
; (str-check "Laghj « dqkjdqmlj »")

