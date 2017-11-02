;=========================================================================
; LW Lisp Tools 
; Lisp programming tools for LispWorks delivered applications
;=========================================================================
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed; in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Author: J. Bresson
;=========================================================================


;;; TEXT BUFFER UTILS

(in-package :om-lisp)

;;;================= 
;;; TEXT BUFFER 
;;;=================

(defun om-make-buffer ()
  (editor::make-buffer (string (gensym)) :base-name "om-text-buffer-"
                       :temporary t))
   
; PLUS UTILISE...
; En declarant les objets :
;    (hcl::flag-special-free-action b)
; ...
; on peut leur appliquer une action au moment du GC
;
;(defun kill-buffers (object)
;  (when (equal (type-of object) 'ombuffer)
;    (print (list "buffer garbage" (buffer object)))
;    (editor::delete-buffer (buffer object))
;    ))
;
; (hcl::add-special-free-action 'kill-buffers)

(defun om-kill-buffer (buffer)
  (editor::kill-buffer-no-confirm buffer))

(defun om-copy-buffer (buffer)
  (let ((newbuffer (om-make-buffer)))
    (ignore-errors 
      (editor::set-buffer-contents 
       newbuffer
       (editor::points-to-string (editor::buffers-start buffer)
                                 (editor::buffers-end buffer))))
    newbuffer
    ))

(defun om-buffer-insert (buffer string &optional (position nil))
  (editor::use-buffer buffer
    (if position
        (editor::with-point ((p (editor:buffers-start buffer)))
          (editor::character-offset p position)
          (editor::insert-string p string))
      (editor::insert-string (editor:buffer-point buffer) string))))

(defun om-buffer-insert-newline (buffer &optional (position nil))
  (editor::use-buffer buffer
    (if position
        (editor::with-point ((p (editor:buffers-start buffer)))
          (editor::character-offset p position)
          (editor::newline p))
      (editor::newline (editor:buffer-point buffer)))))

(defun om-buffer-text (buffer)
  (editor::use-buffer buffer
    (editor:points-to-string (editor:buffers-start buffer) 
                             (editor:buffers-end buffer))))

      
(defun om-buffer-substring (buffer from &optional to)  
  (editor::use-buffer buffer
    (editor::with-point ((p1 (editor:buffers-start buffer))
                         (p2 (if to (editor:buffers-start buffer)
                               (editor:buffers-end buffer))))
      (editor::character-offset p1 from)
      (when to (editor::character-offset p2 to))
      (editor:points-to-string p1 p2))))

(defun om-buffer-char (buffer &optional pos)
  (editor::use-buffer buffer
    (editor::with-point ((p (if pos (editor:buffers-start buffer)
                              (editor:buffer-point buffer))))
      (editor::character-at p pos))))

(defun om-buffer-size (buffer)
  (editor::use-buffer buffer
    (editor::count-characters (editor::buffers-start buffer)
                              (editor::buffers-end buffer))))

(defun om-lines-in-buffer (buffer)
  (editor::use-buffer buffer
    (+ 1 (editor::count-lines (editor::buffers-start buffer)
                              (editor::buffers-end buffer)))))

(defun om-buffer-lines (buffer)
  (let (numlines listline)
    (editor::use-buffer buffer
      (setq numlines (editor::count-lines (editor::buffers-start buffer)
                                          (editor::buffers-end buffer)))
      (setq listlines (editor::list-lines (editor::buffers-start buffer) numlines))
      (butlast listlines (- (length listlines) (1+ numlines))) 
      )))


(defun om-buffer-delete (buffer &optional start end)
  (handler-bind ((error #'(lambda (c) 
                               (print (format nil "Error while cleaning text buffer : ~%~A" c))
                               (om-kill-buffer buffer)
                               (om-make-buffer)
                               )))
    (if start
        (editor::use-buffer buffer
          (editor::with-point ((p1 (editor::buffers-start buffer)) 
                               (p2 (if end (editor::buffers-start buffer)
                                     (editor::buffers-endbuffer))))
            (editor::character-offset p1 start)
            (when end (editor::character-offset p2 end))
            (editor::delete-between-points p1 p2)           
            ))
      (editor::clear-buffer buffer))))
    
(defun om-buffer-set (buffer text) 
  (om-buffer-delete buffer)
  (om-buffer-insert buffer text))

;;; position de la fin de la ligne courante (ou au point start)
(defun om-buffer-line-end (buffer &optional pos)
  (editor::use-buffer buffer
      (editor::with-point ((p (if pos (editor::buffers-start buffer)
                                (editor:buffer-point buffer))))
        ;(when pos (editor::character-offset p pos))
        (when pos (editor::move-point-to-offset p pos))
        (let ((rep (editor::next-newline p)))
          (print (list "NEXT NEWLINE AT" (editor::find-point-offset buffer p) "-->" rep))
          rep
        ))))

;;; position du debut de la ligne courante (ou au point start)
(defun om-buffer-line-start (buffer &optional pos)
  (editor::use-buffer buffer
    (editor::with-point ((p (if pos (editor::buffers-start buffer)
                              (editor:buffer-point buffer))))
      (when pos (editor::character-offset p pos))
      (if (editor::same-line-p p (editor::buffers-start buffer))
          0
        (if (equal (editor::character-at p 0) #\Newline) pos
          (+ 1 (editor::find-point-offset buffer (editor::find-previous-character p #\Newline))))
        ))))


(defun om-buffer-skip-forward (buffer &optional start end)
  (editor::use-buffer buffer
      (editor::with-point ((p (if start (editor::buffers-start buffer)
                                (editor:buffer-point buffer)))
                           (limit (if end (editor::buffers-start buffer)
                                    (editor::buffers-end buffer))))
        (when start (editor::character-offset p start))
        (when end (editor::character-offset limit end))
        (loop while (and (editor::blank-line-p p) (editor::point< p limit)) do
                 (editor::move-point-to-offset p (editor::next-newline p)))
        (editor::find-point-offset buffer p))))


(defun om-get-lisp-expression (buffer)
  (editor::use-buffer buffer
    ;; (editor::SKIP-LISP-READER-WHITESPACE (editor:buffers-start textbuffer) textbuffer)
    (editor::current-form-as-read (editor:buffers-start buffer))
    ))

;; pour sauter aussi les comments : SKIP-LISP-READER-WHITESPACE

;; tests
;(setf buf (om-make-buffer))
;(om-buffer-insert buf "123456789")
;(om-buffer-insert buf "  ")
;(om-buffer-insert-newline buf)
;(om-buffer-text buf)
;(om-buffer-char buf 4)
;(om-buffer-delete buf)
;(om-buffer-size buf)
;(om-lines-in-buffer buf)
;(setf b2 (om-copy-buffer buf))
;(om-buffer-substring buf 6 6)
;(editor::kill-buffer-no-confirm (buffer buf))
;;(editor::delete-buffer (buffer buf))

;;; DOC fonctions du package EDITOR
; (editor::buffers-start buf)  --> renvoie un point correspondant au debut du buffer
; (editor::current-point buf) --> (verif) renvoir lepoint courant (tous buffers confondus)
; (editor::buffer-point buf) --> renvoie le point courant sur un buffer
; (editor::buffer-end point) -> deplace point a la fin du buffer
; (editor::buffer-start point) -> deplace point au debut du debut... buffer
; (editor::delete-buffer buf &optional force-reset) --> efface un buffer
; (editor::clear-buffer) --> ?
; (editor::delete-between-points start end) --> efface entre les deux points
; (editor::delete-characters point &optional (n 1) --> efface n caracteres apres point
; (editor::count-lines beg end) --> compte le nombre de lignes dans le region
; (editor::line-end point) (editor::line-start point) --> ramene point au debut ou a la fin de la ligne
; (editor::list-lines point num) --> ?
; (editor::newline point &optional cound) --> ?
; (editor::start-line-p point) (editor::end-line-p point) --> T si debut ou fin de ligne
; (editor::move-point-to-offset point offset) --> voir a la place de faire characte-offset, start-point, etc. 
 
;;; ecrit le contenu d'un fichier dans le buffer
(defun om-buffer-insert-file (buffer path &optional position)
  (let ((filebuf (editor::find-file-buffer path)))
    (if position
        (editor::use-buffer buffer 
          (editor::with-point ((p (editor:buffers-start buffer)))
            (editor::character-offset p position)
            (editor::insert-string p (editor::points-to-string (editor::buffers-start filebuf)
                                                               (editor::buffers-end filebuf)))))
      (editor::set-buffer-contents buffer 
                                   (editor::points-to-string (editor::buffers-start filebuf)
                                                             (editor::buffers-end filebuf)))
      )
    (editor::kill-buffer-no-confirm filebuf)))

;;; ecrit le contenu du buffer dans un fichier 
(defun om-buffer-write-file (buffer path &key (if-exists :supersede))
  (with-open-file (s path :direction :output :if-exists if-exists)
    (write-string (editor::points-to-string 
                   (editor::buffers-start buffer) 
                   (editor::buffers-end buffer)) 
                  s)))
