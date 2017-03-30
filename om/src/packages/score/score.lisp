;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)

(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename))) 
      '("score-objects/score-object"
        "score-objects/chord"
        "score-objects/chord-seq"
        "score-objects/score"
        "tools/conversions"
        "import-export/musicxml"))


(omNG-make-package 
 "Score"
 :container-pack *om-package-tree*
 :doc "Score tools and objects."
 :classes nil
 :functions nil
 :subpackages  (list (omNG-make-package 
                      "Utils"
                      :doc "Unit conversion utilities etc."
                      :classes nil
                      :functions '(approx-m mc->f f->mc mc->n n->mc beats->ms)
                      :subpackages nil)))
