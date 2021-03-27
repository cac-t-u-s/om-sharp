;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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
; File author: J. Bresson 2021
;============================================================================

(in-package :om)


;;; Utilities to work with score markers

(defmethod* get-marker-times ((self chord-seq))

  :doc "Extracts marker times from a score (chord-seq) using the 'score-markers'"

  (loop for chord in (chords self)
        when (get-extras chord 'score-marker)
        collect (date chord)))


(defmethod* get-segments ((self chord-seq))

  :doc "Extracts segments from a score (chord-seq) using the 'score-markers'"

  (let ((time-markers (cons (date (car (chords self)))
                            (loop for chord in (cdr (chords self))
                                  when (get-extras chord 'score-marker)
                                  collect (date chord)))))

    (loop for next-markers on time-markers
          collect (remove-extras
                   (select self (car next-markers) (cadr next-markers))
                   'score-marker nil))))


(defmethod* map-segments ((self chord-seq) function)

  :doc "Applies a function to all the segments in a chord-seq, and concatenates the results.

<function> should accept a chord-seq as argument, and return another concatenable score object."

  (reduce #'concat
          (mapcar function (get-segments self))))
