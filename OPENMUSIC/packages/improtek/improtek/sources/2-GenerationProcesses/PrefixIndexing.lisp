(in-package :om)
;PrefixIndexing.lisp
;------------------------------------------------------------------------------------------------------------
;IMPLEMENTATION AND TUTORIAL OF THE STRING MATCHING ALGORITHMS USED IN Improvizer.lisp FOR THE NAVIGATION THROUGH THE MEMORY. 
;Jérôme Nika
;Feb 20th 2012, revised nov. 2014
;------------------------------------------------------------------------------------------------------------


;********************************************************************************
;Now in "LoadImprotek"
;(defparameter *print_info_MP* 1) ;Print every step in "Morris&Pratt"
;(defparameter *print_info_find* 1) ;Print every step in "find-prefix-label-match"
;(defparameter *print_info_navig* 1) ;Print every step in "Improvize"
;********************************************************************************


;====================================================================== ORIGINAL MORRIS & PRATT ALGORITHM (searching for the whole word) =====================================================================
#|
;====================================
; EXAMPLES ORIGINAL MORRIS&PRATT
;====================================
 
(MorrisPratt '(A Z E R T Y U A Z E R P O) '(A Z E R))

(MorrisPratt '(A Z E R T Y U A Z E R P O) '(H E L L O))

(om-inspect (MorrisPratt_failure '(A B C A B D A)))

(om-inspect (MorrisPratt_failure '(G C A G A G A G)))

(om-inspect (MorrisPratt_failure '(A B A B A B C)))

(om-inspect (MorrisPratt_failure '(A B A B A B C A B A)))
|#


;Original Morris&Pratt "failure fonction"
;----------------------------------------
(defmethod MorrisPratt_failure ((l list) &optional CompareFunction)
(let ((Compare 'equal))
  (when CompareFunction (setf Compare Comparefunction))
  ;(setf fail_table (make-hash-table :test #'CompareEvents))
  (setf fail_table (make-hash-table))
  (let* ((p 0) (i -1))
    (setf (gethash 0 fail_table) -1)
    (loop while (< p (list-length l)) do
          (loop while (and 
                       (> i -1) 
                       (not 
                        ;(Compare (nth p l) (nth i l))
                        (apply Compare (list (nth p l) (nth i l)))
                        )) do
                 (setf i (gethash i fail_table)))
           (setf (gethash (incf p) fail_table) (incf i)))
    fail_table)))

#|
;KNUTH Morris&Pratt "failure fonction"
;----------------------------------------
 (defmethod KnuthMorrisPratt_failure ((l list))
  ;(setf fail_table (make-hash-table :test #'CompareEvents))
  (setf fail_table (make-hash-table))
  (let* ((p 0) (i -1))
    (setf (gethash 0 fail_table) -1)
    (loop while (< p (list-length l)) do
          (loop while (and (> i -1) (not (apply Compare (list (nth p l) (nth i l))))) do
                 (setf i (gethash i fail_table)))
           (incf i)
           (incf p)
           (if (apply Compare (list (nth p l) (nth i l)))
               (setf (gethash p fail_table) (gethash i fail_table))
             (setf (gethash p fail_table) i)))
    fail_table))
|#


;Original Morris&Pratt algorithm
;--------------------------------
;/!\ Arguments : 1)text/corpus 2)word/motive to search
;Returns the list of idxs where the word is found
;--------------------------------
(defmethod MorrisPratt ((text list) (word list) &optional CompareFunction)
(let ((Compare 'equal))
  (when CompareFunction (setf Compare Comparefunction))
  (setf idxs '())
  
  (if (= *print_info_MP* 1) 
      (format *om-stream* "~%++++++++++++++++++++++++++++++++++++MORRIS & PRATT++++++++++++++++++++++++++++++++++++~%++++++++++++++++++++++++++++++++++++Looking for ~a in ~a++++++++++++++++++++++++++++++++++++" (FormatLabelList word) (FormatLabelList text)))
  (let* ((fail (MorrisPratt_failure word)) (i 0) (j 0) (prev_i -1))

    (loop while (< j (list-length text)) do
          (if (= *print_info_MP* 1) (format *om-stream* "~%++  ----> text: j=~D, word: i=~D, (j-i)=~D [[echec(i) = ~D]] <----~%++ Labels = text : ~a / word : ~a ===> equal = ~a~%" 
                  ;j i (- j i) (gethash i fail) (nth j text) (nth i word) (Compare (nth i word) (nth j text))))
                                            j i (- j i) (gethash i fail) (FormatLabel (nth j text)) (FormatLabel (nth i word)) (apply Compare (list (nth i word) (nth j text)))))

          ;(if (and (> i -1) (not (Compare (nth i word) (nth j text))))
          (if (and (> i -1) (not (apply Compare (list (nth i word) (nth j text)))))
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  =========Different labels (and i>-1) == ~%"))
                
                
                ;(loop while (and (> i -1) (not (Compare (nth i word) (nth j text)))) do
                (loop while (and (> i -1) (not (apply Compare (list (nth i word) (nth j text))))) do
                      (if (= *print_info_MP* 1) (format *om-stream* "++  === Still I>-1 and different labels : (text j=~D) != (word i=~D) ie ~a != ~a~%" j i (FormatLabel (nth j text)) (FormatLabel (nth i word))))
                      (setf i (gethash i fail))
                      (if (= *print_info_MP* 1) (format *om-stream* "++  => Failure function : i ->~D ~%" i)))))
          
          (incf i) (incf j) (if (= *print_info_MP* 1) (format *om-stream* "++  j++->~D ,i++->~D~%++ ~%++ ~%" j i))
         
          (if (> i (- (list-length word) 1)) 
              (progn 
                (if (= *print_info_MP* 1) (format *om-stream* "++  >>>>>>> Word found ! Begins at index = ~D<<<<<<<~%++ ~%" (- j i)))
                (push (- j i) idxs)
                (setf i (gethash i fail))
                (if (= *print_info_MP* 1) (format *om-stream* "++  Failure function : i ->~D~%" i))))
          )
    (if (= *print_info_MP* 1) (format *om-stream* "~%++++++++++++++++++++++++++++++++++++END MORRIS & PRATTS++++++++++++++++++++++++++++++++++++~%"))
    idxs)))



;========================================================================= PREFIX INDEXING ALGORITHM USED IN THE NAVIGATION (Improvize.lisp) ========================================================================



#|
;==============================================================
; EXAMPLES COMPUTATION INTERNAL PREFIXES WITH FAILURE FUNCTION
;==============================================================
(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(A B C A B D A))))

(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(G C A G A G A G))))

(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(A B A B A B C))))

(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(A B A B C A B C A B A B A))))

(gethash 5 (nth 1 (MorrisPratt_failure&internalprefixes '(A B A B A B C A B A))))
|#


;nth 0 : Morris&Pratt "failure fonction" 
;nth 1 : table lengths_ending_prefixes : table[idx] = list lengths of prefixes ending at idx
;----------------------------------------
(defmethod MorrisPratt_failure&internalprefixes ((l list) &optional CompareFunction)
  (let ((Compare 'equal)
        fail_table
        lengths_ending_prefixes)
    (when CompareFunction (setf Compare Comparefunction))
    ;(setf fail_table (make-hash-table :test #'CompareEvents))
    (setf fail_table (make-hash-table))
    ;(setf lengths_ending_prefixes (make-hash-table :test #'CompareEvents))
    (setf lengths_ending_prefixes (make-hash-table))
    (let ((p 0) (i -1))
      (setf (gethash 0 fail_table) -1)
      (setf (gethash 0 lengths_ending_prefixes) '(1))
      (loop while (< p (list-length l)) do
            ;Failure i
            ;(loop while (and (> i -1) (not (Compare (nth p l) (nth i l)))) do
            (loop while (and (> i -1) (not (apply Compare (list (nth p l) (nth i l))))) do
                  (setf i (gethash i fail_table)))
            (incf p)
            (incf i)
            (setf (gethash p fail_table) i)
            ;lengths_ending_prefixes i-1
            (if (> p 1)
                (setf (gethash (- p 1) lengths_ending_prefixes) (append (list p) (gethash (- (gethash p fail_table) 1) lengths_ending_prefixes))))

            )
      (list fail_table lengths_ending_prefixes))))



#|
;=======================================
;EXAMPLES OF PREFIX INDEXING
;=======================================

(print-PrefixIndexing (PrefixIndexing '(B A B B A B C A B C A B) '(A B C A B A)))

(print-PrefixIndexing (PrefixIndexing '(D A A B A B A B A B D A B A B A B) '(A B A B A B C)))


 
;Example with lists of characters
;---------------------------------
  (setf word '(a b c d))
  (setf text '(a b c d a b a a b c))
  (setf resultMP (PrefixIndexing text word))
  (print-PrefixIndexing resultMP)

;Example with lists of characters (particular case : some factors in the word are prefixes)
;--------------------------------------------------------------------------------------------
  (setf word '(a b c a b d))
  (setf text '(a b c a b a a b c))
  (setf resultMP (PrefixIndexing text word))
  (print-PrefixIndexing resultMP)

;Example with lists of beats
;------------------------------------------
  (setf word (make-beat-list Dicidenbassolo-juil2011_beatsfromfile))
  (setf text (make-beat-list Jaimesolo-juil2011_beatsfromfile))
  (setf resultMP (PrefixIndexing text word))
  (print-PrefixIndexing resultMP)


;Example with grids  : /!\ Use "expand_grid" !!! (ex. (f m7 3) -> ( f m7) (f m7) (f m7))
;-----------------------------------------------------------------------------------------
  (setf word (expand_grid (grid Dicidenbas_tune)))
  (setf text (expand_grid (grid CecileMaFille_tune)))
  (setf resultMP (PrefixIndexing text word))
  (print-PrefixIndexing resultMP)

 
;Example Improvizer / grid (1)
;[Overloaded method in Improvize.lisp for arguments (Improvizer) (list)]
;-------------------------------
  (setf beat-list3 '(
                     ((g#) ())
                     ((g) ())
                     ((f) ())
                     ((g) ())
                     ((ab) ())
                     ((g) ())
                     ((a) ())
                     ((d) ())
                     ((ab) ())
                     ((g) ())
                     ((a) ())
                     ((g) ())
                     ))
  (setf oracle (NewImprovizer (make-beat-list beat-list3)))
  (setf grid '((ab) (g) (a) (d) (a) (c)))
  (setf resultMP (PrefixIndexing oracle grid))
  (print-PrefixIndexing resultMP)


;Example Improvizer / grid (2)
;-------------------------------
 
  (setf tune Dicidenbas_tune)
  (setf beatduration (beatduration tune))
  (setf Improvizer (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
  (setf grid (grid tune))

  (setf resultMP (PrefixIndexing Improvizer (expand_grid grid)))
  (print-PrefixIndexing resultMP)

|#


;Looking for prefixes (inspired by Morris&Pratt)
;-----------------------------------------------
;/!\ Arguments : 1)text/corpus 2)word/motive to search
;Returns a 2 elements list :
; (nth 0 ) -> HashTable : prefix_length -> (idxs of the different occurrences)
; (nth 1 ) -> length of the longest prefix
;-----------------------------------------------
(defmethod PrefixIndexing ((text list) (word list) &optional CompareFunction)
(let ((Compare 'equal))
  (when CompareFunction (setf Compare Comparefunction))
  (setf prefix_idx (make-hash-table :test #'equal))
  
  (if (= *print_info_MP* 1) 
      (format *om-stream* "~%++++++++++++++++++++++++++++++++++++PREFIX INDEXING ALGORITHM++++++++++++++++++++++++++++++++++++~%++++++++++++++++++++++++++++++++++++Looking for prefixes of ~a in ~a++++++++++++++++++++++++++++++++++++" (FormatLabelList word) (FormatLabelList text)))
  (let* ((resultsMPfailureInternalPrefixes (MorrisPratt_failure&internalprefixes word))
         (fail (nth 0 resultsMPfailureInternalPrefixes))
         (lengthInternalPrefixes (nth 1 resultsMPfailureInternalPrefixes))
         (i 0) 
         (j 0)
         (tmp-length-longest 0))

    (loop while (< j (list-length text)) do
          (if (= *print_info_MP* 1) (format *om-stream* "~%++  ----> text: j=~D, word: i=~D, (j-i)=~D [[echec(i) = ~D]] <----~%++ Labels = text : ~a / word : ~a ===> equal = ~a~%" 
                  ;j i (- j i) (gethash i fail) (nth j text) (nth i word) (Compare (nth i word) (nth j text))))
                                            j i (- j i) (gethash i fail) (FormatLabel (nth j text)) (FormatLabel (nth i word)) (apply Compare (list (nth i word) (nth j text)))))
          
          ;(if (and (> i -1) (not (Compare (nth i word) (nth j text))))
          (if (and (> i -1) (not (apply Compare (list (nth i word) (nth j text)))))
              ;The labels are different : end of a prefix if the previous were equal
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  =========DIFFERENT LABELS (and i>-1) ==> ~%++ >>>Found prefix, begins at idx=(j-i)= ~D : length = i = ~D<<<~%" (- j i) i))
                (if (> i 0)
                    (progn
                      (loop for k from i downto 1 do (if (not (member (- j i) (gethash k prefix_idx))) (push (- j i) (gethash k prefix_idx))))
                      (if (> i tmp-length-longest) (setf tmp-length-longest i))
                      ;Look for internal prefixes in the found prefix
                      (setf prefix_idx (add_internal_prefixes prefix_idx lengthInternalPrefixes j i))
                      
                      ))
               
                ;(loop while (and (> i -1) (not (Compare (nth i word) (nth j text)))) do
                (loop while (and (> i -1) (not (apply Compare (list (nth i word) (nth j text))))) do
                      (if (= *print_info_MP* 1) (format *om-stream* "++  === Still i>-1 and different labels : (text j=~D) != (word i=~D) ie ~a != ~a~%" j i (FormatLabel (nth j text)) (FormatLabel (nth i word))))
                      (setf i (gethash i fail))
                      (if (= *print_info_MP* 1) (format *om-stream* "++  => Failure function : i ->~D ~%" i))
                      
 

                      )
                (if (> i -1) 
                    (if (= *print_info_MP* 1) 
                        (format *om-stream* "++ => New label (word) = ~a ((label text = ~a))~%" (nth i word) (nth j text))))
               
                )
            
            ;The labels are equal...
            (if (= *print_info_MP* 1) (format *om-stream* "++  =========EQUAL LABELS (or i<=-1)~%")))        

          (incf i) (incf j) (if (= *print_info_MP* 1) (format *om-stream* "++  j++->~D ,i++->~D~%++ ~%++ ~%" j i))

          ;Whole word found (original Morris & Pratt)
          (if (> i (- (list-length word) 1))   
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  >>>>>>> Word found ! Begins at index = ~D<<<<<<<~%++  ~%" (- j i)))
                (loop for k from i downto 1 do (if (not (member (- j i) (gethash k prefix_idx))) (push (- j i) (gethash k prefix_idx))))
                (if (> i tmp-length-longest) (setf tmp-length-longest i))
                ;Look for internal prefixes in the found prefix
                (setf prefix_idx (add_internal_prefixes prefix_idx lengthInternalPrefixes j i))
                (setf i (gethash i fail))
                (if (= *print_info_MP* 1) (format *om-stream* "++  Failure function : i ->~D~%" i))
                ))
          
          ;If the end of the text is reached while recognizing a prefix
          (if (and (>= j (list-length text)) (> i 0)) 
              (progn
                  (if (= *print_info_MP* 1) (format *om-stream* "++  =========End of text reached while recognizing a prefix ~%++ >>>Found prefix (end of text), begins at idx = ~D : length ~D<<<~%++ ~%" (- j i) i))
                  (if (> i tmp-length-longest) (setf tmp-length-longest i))
                  (loop for k from i downto 1 do (if (not (member (- j i) (gethash k prefix_idx))) (push (- j i) (gethash k prefix_idx))))
                  ;Look for internal prefixes in the found prefix
                  (setf prefix_idx (add_internal_prefixes prefix_idx lengthInternalPrefixes j i))

                  )))
    (if (= *print_info_MP* 1) (format *om-stream* "~%++++++++++++++++++++++++++++++++++++END PREFIX INDEXING ALGORITHM++++++++++++++++++++++++++++++++++++~%"))
    (list prefix_idx tmp-length-longest))))



;Sub-routine to index the internal prefixes within 
;the longest prefix ending at step (i,j) of the research phase
;-----------------------------------------------
;Arguments : 
;1) result table to complete
;2) prefixes of the word in itself (obtained during the analysis phase)
;3/4) Step in the research algorithm
;-----------------------------------------------
(defun add_internal_prefixes (prefixesWordInText internalPrefixesInWord j i)
  (if (= *print_info_MP* 1) (format *om-stream* "~%+++  ====Looking for internal prefixes within the last prefix found~%"))
  
  (let ((jFinPref (- j 1)) (iFinPref (- i 1)))
    (loop for i0 from 0 to iFinPref do
          (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D -> looking for prefixes ending in i-i0=~D (word)-> j-i0=~D (text) ...+++~%" i0 (- iFinPref i0) (- jFinPref i0)))
          (loop for len in (gethash (- iFinPref i0) internalPrefixesInWord) do
                (let ((idx_in_text (+ (- (- jFinPref len) i0) 1)))
                  (progn
                    (if (= *print_info_MP* 1) (format *om-stream* "+++ ... one of length l=~D ! +++~%" len))
                    (if (not (member idx_in_text (gethash len prefixesWordInText))) (push idx_in_text (gethash len prefixesWordInText))) 
                    (if (= *print_info_MP* 1)(format *om-stream* "+++  ((>>Found prefix moving backward in the failure function (AFTER END OF THE TEXT) : i0 =~D ---> begins at idx j-i0-l+1= ~D : length l= ~D<<))~%"i0 idx_in_text len))
                    )))))
  (if (= *print_info_MP* 1) (format *om-stream* "+++  ====  END Looking for internal prefixes~%"))
  prefixesWordInText)


;Display PrefixIndexing results
;------------------------------------
(defun print-PrefixIndexing (resultMP)
  (let* ((hash (nth 0 resultMP)) (length-longest (nth 1 resultMP)))
    (format *om-stream* "Longest prefix length = ~D~%Found prefixes :~%" length-longest)
    (loop for len being the hash-key of hash using (hash-value idxs)
          do (format *om-stream* "Length =~D -> ~D occurence [idxs = ~a]~%" len (list-length idxs) idxs))))




























