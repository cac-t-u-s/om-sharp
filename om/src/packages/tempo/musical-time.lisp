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
; File author: D. Bouche
;============================================================================

(declaim (optimize (speed 3) (safety 0) (debug 1)))

(in-package :om)

;;;TRANSLATES ABSOLUTE TIME IN MS, ACCORDING TO A TEMPO AND A TIME SIGNATURE, IN A LIST OF:
;;;;; - Bar index,
;;;;; - Beat index,
;;;;; - Sixteenth-Beat index,
;;;;; - Sixteenth-Beat rest (as a float).
(defun absolute->musical-time (time-ms tempo signature-numerator signature-denominator)
  (let* ((beatdur (get-beat-duration tempo signature-numerator signature-denominator))
         (bardur (* signature-numerator beatdur))
         (bar+rest (multiple-value-list (floor time-ms bardur)))
         (beat+rest (multiple-value-list (floor (cadr bar+rest) beatdur)))
         (sixteenth-beat+rest (multiple-value-list 
                               (floor (cadr beat+rest) 
                                      (/ beatdur (/ 16 signature-denominator))))))
    (list
         (1+ (car bar+rest))
         (1+ (car beat+rest))
         (1+ (car sixteenth-beat+rest)))))

;;;TRANSLATES MUSICAL TIME (AS A LIST), ACCORDING TO A TEMPO AND A TIME SIGNATURE, IN ABSOLUTE TIME (MS)
(defun musical->absolute-time (time-list tempo signature-numerator signature-denominator)
  (let* ((beatdur (get-beat-duration tempo signature-numerator signature-denominator))
         (bardur (* signature-numerator beatdur))
         (sixteenth-beat-dur (/ beatdur (/ 16 signature-denominator))))
    (+ (* (1- (nth 0 time-list))  bardur) 
       (* (1- (nth 1 time-list)) beatdur) 
       (* (+ (1- (nth 2 time-list)) (nth 3 time-list)) sixteenth-beat-dur))))

;;;OUTPUTS THE DURATION (IN MS) OF A BAR, ACCORDING TO A TEMPO AND A TIME SIGNATURE
(defun get-bar-duration (tempo signature-numerator signature-denominator)
  (* signature-numerator (get-beat-duration tempo signature-numerator signature-denominator)))

;;;OUTPUTS THE DURATION (IN MS) OF A BEAT, ACCORDING TO A TEMPO AND A TIME SIGNATURE
(defun get-beat-duration (tempo signature-numerator signature-denominator)
  (declare (ignore signature-numerator))
  (* (/ 60000.0 tempo) (/ 4.0 signature-denominator)))

;;;OUTPUTS A DURATION (IN MS), ACCORDING TO A TEMPO, TIME SIGNATURE AND A RATE
(defun get-musical-synced-duration (rate tempo signature-numerator signature-denominator)
  (* (get-beat-duration tempo signature-numerator signature-denominator) (* rate signature-denominator)))

;;;OUTPUTS A MUSICAL GRID AS A LIST OF 2-TUPLES (TIME MUSICAL-TIME) BETWEEN TWO DATES
;;;RESPECTIVELY SEPARATED BY THE DURATION RETURNED BY BEAT-DUR/BEAT-FACTOR
(defun get-musical-grid (t1-ms t2-ms beat-factor tempo signature-numerator signature-denominator)
  (let* ((beatdur (get-beat-duration tempo signature-numerator signature-denominator))
         (step-ms (/ beatdur beat-factor))
         (t1 (- t1-ms (mod t1-ms step-ms))))
    (loop for ti from t1 to t2-ms
          by step-ms
          for tick from 0
          collect
          (let* ((mustime (absolute->musical-time ti tempo signature-numerator signature-denominator)))
            (list ti (musical-time->string mustime 
                                           (if (and (= (nth 1 mustime) 1) (= (nth 2 mustime) 1))
                                               1
                                             (if (= (nth 2 mustime) 1)
                                                 2
                                               3))))))))

;;;TRANSLATES A MUSICAL-TIME IN A STRING
;;;OPTIONNALLY SET THE PRECISION
(defun musical-time->string (time-list &optional (n 3))
  (if (= n 1)
      (format nil "~A" (nth 0 time-list))
    (if (= n 2)
        (format nil "~A.~A" (nth 0 time-list) (nth 1 time-list))
      (format nil "~A.~A.~A" (max 1 (nth 0 time-list)) (max 1 (nth 1 time-list)) (max 1 (nth 2 time-list))))))

;;;GET THE BEST BEAT FACTOR FOR A TIME-SPAN (FOR DISPLAY)
(defun get-beat-factor (time-span tempo signature-numerator signature-denominator)
  (let ((div (/ time-span (get-beat-duration tempo signature-numerator signature-denominator))))
    (cond ((< div (/ 1 8))
           256)
          ((and (>= div (/ 1 8)) (< div (/ 1 4)))
           128)
          ((and (>= div (/ 1 4)) (< div (/ 1 2)))
           64)
          ((and (>= div (/ 1 2)) (< div 1))
           32)
          ((and (>= div 1) (< div 3))
           16)
          ((and (>= div 3) (< div 5))
           8)
          ((and (>= div 5) (< div 10))
           4)
          ((and (>= div 10) (< div 25))
           2)
          ((and (>= div 25) (< div 50))
           1)
          ((and (>= div 50) (< div 100))
           (/ 1 2))
          ((and (>= div 100) (< div 200))
           (/ 1 4))
          ((and (>= div 200) (< div 400))
           (/ 1 8))
          ((and (>= div 400) (< div 800))
           (/ 1 16))
          ((and (>= div 800) (< div 1600))
           (/ 1 32))
          ((and (>= div 1600) (< div 3200))
           (/ 1 64))
          ((and (>= div 3200) (< div 6400))
           (/ 1 128))
          (t (/ 1 256)))))


(defun get-fixed-beat-grid (tempo tmin tmax &optional beat-rate)
  (let ((current-time 0)
        (tvarstart 0)
        (current-date 0)
        (beatdur (get-beat-duration tempo 4 4))
        (date 0)
        (beatlist '()))
    (loop while (< current-time tmax)
          do
          (if (> date tmax)
              (return))
          (setq tvarstart current-time)
          (loop for beat from 0 to (ceiling tmax beatdur)
                by beat-rate
                do
                (setq date (+ tvarstart (setq current-date (* beatdur beat))))
                (if (>= date tmin)
                    (push (list date beat) beatlist))
                (if (> date tmax)
                    (return)))
          (incf current-time current-date))
    (reverse beatlist)))