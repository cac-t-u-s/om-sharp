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

(in-package :om)

(defclass score-extra () 
  ()
  (:documentation "Superclass for score extras: elements attached to chord modifying/completing their representation in score editors."))

(defclass* head-extra (score-extra) 
  ((head-char :initarg :head-char :accessor head-char :initform nil :documentation "SMuFL char code for the note head symbol"))
  (:documentation "A score extra changing the note head"))

(defclass* vel-extra (score-extra) 
  ((dx :initarg :dx :accessor dx :initform 0 :type number :documentation "horizontal shift in score units wrt. default position")
   (dy :initarg :dy :accessor dy :initform 0 :type number :documentation "vertical shift in score units wrt. default position"))
  (:documentation "A score extra forcing the explicit display of velocity in score editors"))

(defclass* text-extra (score-extra) 
  ((dx :initarg :dx :accessor dx :initform 0 :type number :documentation "horizontal shift in score units wrt. default position")
   (dy :initarg :dy :accessor dy :initform 0 :type number :documentation "vertical shift in score units wrt. default position")
   (text :initarg :text :accessor text :initform "text" :type string :documentation "the text")
   (font :initarg :font :accessor font :initform nil :documentation "the text font"))
  (:documentation "A score extra attaching a text label"))

(defclass* symb-extra (score-extra) 
  ((dx :initarg :dx :accessor dx :initform 0 :documentation "horizontal shift in score units wrt. default position")
   (dy :initarg :dy :accessor dy :initform 0 :documentation "vertical shift in score units wrt. default position")
   (symb-char :initarg :symb-char :accessor symb-char :initform nil :documentation "SMuFL char code for the score symbol"))
  (:documentation "A score extra attaching a score symbol"))

(defclass* score-marker (score-extra) 
  ((data :initarg :data :accessor data :initform nil :type t :documentation "some data or label attached to the marker"))
  (:documentation "A score extra marking a segment or cue point in the score"))


;;;=================================
;;; EXTRA(S) IN SCORE OBJECTS
;;;=================================

;;; ensure this is always a list
(defmethod (setf extras) ((extras score-extra) (self chord))
  (setf (extras self) (list extras)))

(defmethod initialize-instance :after ((self chord) &rest initargs)
  (setf (extras self) (list! (extras self))))

(defmethod om-init-instance :after ((self chord-seq) &optional initargs) 
  (let ((extras (slot-value self 'extras)))
    (when extras 
      (if (listp extras)
          (loop for extra in extras 
                for chord in (chords self)
                do (add-extras chord extra nil nil))
        (add-extras self extras nil nil))
      (setf (slot-value self 'extras) nil)
      )))

(defmethod (setf extras) ((extras list) (self chord-seq))
  (loop for extra in extras 
        for chord in (chords self)
        do (add-extras chord extra nil nil))
  nil)

(defmethod extras ((self chord-seq)) 
  (loop for chord in (chords self) collect (extras chord)))

  
;;;=================================
;;; ATTACH EXTRA(S) TO SCORE OBJECTS
;;;=================================

(defmethod* add-extras ((self chord) extras position &optional (newobj t))
  :icon :score
  :initvals '(nil nil nil t) 
  :indoc '("a chord or musical object" "a score-extra or list of score-extras" "a position in the object" "return new object") 
  :doc "Adds 'score-extras' to a chord or a musical object.

Score-extras can be objects of the following types:
- head-extra (changing the note-heads)
- vel-extra (displaying the velocity)
- text-extra (attaching a text label)
- symb-extra (attaching a score symbol)
- score-marker (attaching a marker)

If <newobj> is T (default), a new object is returned.
If <newobj> is NIL, the modifications apply to the input object.


If <self> is a CHORD:
- <position> is ignored
- All <extras> are attached to <self>


If <self> is a sequence (CHORD-SEQ or VOICE):

   If <position> is a number:
   - All extra(s) in <extras> are attached to the chord at <position> in <self>
   
   If position is NIL:
   - All extra(s) in <extras> are attached to all the chords in <self> 

   If <position> is a list: 
   - This list is considered to contain the positions (integers) of chords to which will be attached the extra(s)
      If <extras> is also a list:
      - Each element in <extras> (can also be a sub-list) is paired to the selected chords in <position>
      Else, if <extras> is a single element:
      - A copy of the the same <extra> is attached to the chords pointed by <position>.
   
   

If <self> is a polyphony (MULTI-SEQ or POLY):
   
   If <position> is a plain list: 
   - This list is considered to contain two elements: 
          1) the index of a voice (starting from 0), and 
          2) the position of a chord in this voice.
   - All extra(s) in <extras> are attached to the chord at <position> in <self>

   If <position> is a list of list    
   - This list is considered to contain the pairs pointing to chords, as in the previous case.
      If <extras> is a list:
      - Each element in <extras> (can also be a sub-list) is paired to the selected chords in <position>
      Else, if <extras> is a single element:
      - A copy of the the same <extra> is attached to all the chords pointed by <position>
   
   If position is NIL:
   - All extra(s) in <extras> are attached to all the chords in <self> 

   Note: <position> can not be a number in this case.

"
  (declare (ignore position))

  (let ((rep (if newobj (om-copy self) self))) 
    
    (setf (extras rep) (list! extras))
    
    rep))

;== CHORD-SEQ / VOICE:

; POSITION = NUMBER
(defmethod* add-extras ((self chord-seq) extras (position number) &optional (newobj t))
  
  (let* ((rep (if newobj (om-copy self) self))
         (chord (nth position (chords rep))))
    
    (when chord
      (add-extras chord extras nil nil))
    
    rep))
  
; POSITION = NON-NULL LIST / EXTRAS = LIST
(defmethod* add-extras ((self chord-seq) (extras list) (position cons) &optional (newobj t))
   (let ((rep (if newobj (om-copy self) self)))
     (loop for p in position 
           for extra in extras do
           (add-extras rep extra p nil))
     rep))

; POSITION = NON-NULL LIST / EXTRAS = SINGLE ELEMENT
(defmethod* add-extras ((self chord-seq) (extras score-extra) (position cons) &optional (newobj t))
  (let ((rep (if newobj (om-copy self) self)))
    (loop for p in position do
          (add-extras rep (om-copy extras) p nil))
    rep))

; POSITION = NIL
(defmethod* add-extras ((self chord-seq) extras (position null) &optional (newobj t))
  (let ((rep (if newobj (om-copy self) self)))
    (loop for chord in (chords rep) do
          (add-extras chord (om-copy extras) nil nil))
    rep))

;== MULTI-SEQ / POLY

; POSITION = NIL
(defmethod* add-extras ((self multi-seq) extras (position null) &optional (newobj t))
  (let ((rep (if newobj (om-copy self) self)))
    (loop for v in (inside rep) do
          (add-extras v extras nil nil))
    rep))

; POSITION = NON-NULL LIST
(defmethod* add-extras ((self multi-seq) extras (position cons) &optional (newobj t))

  (let ((rep (if newobj (om-copy self) self)))
    
    (cond 
     
     ;;; 1 single chord targeted
     ((every #'integerp position)
      (when (> (length position) 2)
        (om-print "Warning: position list in get-extra should not be of size >2 (voice-num / chord-num)"))
      (let ((voice (nth (car position) (inside rep))))
        (when voice 
          (add-extras voice extras (cadr position) nil))))
     
     ;;; list of chords targeted
     ((every #'consp position)
      
      (if (listp extras)
          ;;; loop though position and extra lists
          (loop for pos in position 
                for extra in extras do
                (add-extras rep extra pos nil))
        
        ;;; all same extra
        (loop for pos in position do
              (add-extras rep extras pos nil))
        ))
     
     (t (om-print "Error unsupported position list in ADD-EXTRAS:")
        (om-print position)
        ))
   
    rep))



;;;=================================
;;; GET EXTRAS FROM SCORE OBJECTS
;;;=================================
(defmethod* get-extras ((self chord) extra-type)
  :icon :score
  :initvals '(nil nil) 
  :menuins '((1 (("all" nil) 
                 ("head" head-extra)
                 ("vel" vel-extra)
                 ("text" text-extra)
                 ("symbol" symb-extra)
                 ("marker" score-marker))))            
  :indoc '("a chord or musical object" "specific type(s) of score-extra") 
  :doc "Returns 'score-extras' from a chord or a musical object."
  
  (if extra-type 
      (remove-if-not 
       #'(lambda (e) (find (type-of e) (list! extra-type) :test #'subtypep))
       (extras self))
    (extras self)))

(defmethod* get-extras ((self chord-seq) extra-type)
  (loop for c in (chords self) append 
        (get-extras c extra-type)))

(defmethod* get-extras ((self multi-seq) extra-type)
  (loop for v in (inside self) append 
        (get-extras v extra-type)))


;;;=================================
;;; REMOVE EXTRAS FROM SCORE OBJECTS
;;;=================================

(defmethod* remove-extras ((self chord) extra-type &optional newobj)
  :icon :score
  :initvals '(nil nil nil) 
  :menuins '((1 (("all" nil) 
                 ("head" head-extra)
                 ("vel" vel-extra)
                 ("text" text-extra)
                 ("symbol" symb-extra)
                 ("marker" score-marker))))            
  :indoc '("a chord or musical object" "specific type(s) of score-extra" "return new object") 
  :doc "Removes 'score-extras' from a chord or a musical object."
   
  (let ((rep (if newobj (om-copy self) self)))
      
    (setf (extras rep) 
          (if extra-type 
              (remove-if 
               #'(lambda (e) (find (type-of e) (list! extra-type) :test #'subtypep))
               (extras self))
            nil))
    
    rep))


(defmethod* remove-extras ((self chord-seq) extra-type &optional newobj)

    (let ((rep (if newobj (om-copy self) self)))
      
      (loop for chord in (chords rep) do 
            (remove-extras chord extra-type))
      
      rep))

(defmethod* remove-extras ((self multi-seq) extra-type &optional newobj)

    (let ((rep (if newobj (om-copy self) self)))
      
      (loop for v in (inside rep) do 
            (remove-extras v extra-type))

      rep))


; TODO: ATTACH/REMOVE/EDIT INSIDE SCORE EDITOR





