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

(in-package :om)


(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename))) 
      '(
        "functions/lists"
        "functions/numbers"
        "functions/sets"  
        "functions/combinatorial" 
        "functions/series" 
        "functions/interpolations" 
        
        "time-sequence/object-with-action"

        "time-sequence/time-sequence"
        "time-sequence/timeline-editor"

        "bpf-bpc/bpf"
        "bpf-bpc/bpc"
        "bpf-bpc/bpf-tools"
        "bpf-bpc/bpf-editor"
        
        "text/textfile"
        
        "automation/automation"
        "automation/automation-editor"
        "automation/tempo-automation"
        
        "containers/2d-array"
        "containers/data-stream"
        "containers/data-stream-editor"
        
;    "classes;splines"  
;    "classes;array" 
;    "classes;picture"    
;    "classes;omgraphics"
;    "functions;file"
;    "classes;time-array" 
    
        "basic-pack"
    
        ))


       
     
 
     
  
