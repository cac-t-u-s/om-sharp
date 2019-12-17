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
        
        "functions/modulations" 
        
        "text/textfile"
        
        "automation/automation"
        "automation/tempo-automation"
        "automation/automation-editor"
        
        "containers/2d-array"
        
        "containers/data-stream"
        "containers/data-stream-editor"
        
        "compatibility"
        "basic-pack"
    
        ))


       
     
 
     
  
