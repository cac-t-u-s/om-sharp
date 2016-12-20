;;===========================================================================
;Copyright (C) 2015 IRCAM-Centre Georges Pompidou, Paris, France.
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
;Author: Dimitri Bouche
;;===========================================================================
(in-package :om)

(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename)))      
      '(
        "action"
        
        "scheduler"
        "engine"
        "thread-pool"
        "dispatcher"
        "graphics-callback"

        "schedulable-object"
        
        "scheduling-system"
        
        "mode-switcher"))