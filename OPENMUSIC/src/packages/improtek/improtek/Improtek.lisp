;;;;============================================================================
;;;;               Improtek.lib
;;;;
;;;; Improtek library (from OMax 2.0)
;;;; authors: J. Nika, M.Chemillier
;;;; date: 2015 (OMax 2.0, 2004, G. Assayag)
;;;;============================================================================ 

(in-package :om)

;;;;============================================================================
#|
ImproteK requires the following external libraries : 
- Repmus (in the folder "libraries" of the OpenMusic distribution : http://repmus.ircam.fr/openmusic/download)
- Bordeaux-threads (http://common-lisp.net/project/bordeaux-threads/) and its dependencies 
Alexandria (http://common-lisp.net/project/alexandria/) and Fiveam (http://common-lisp.net/project/fiveam/)


A folder containing all the ImproteK dependencies can be downloaded at:
https://drive.google.com/folderview?id=0B-b9cibNADQ0dl96U3BWVUZvVjg&usp=sharing


If ImproteK is used within the OpenMusic environment, these libraries have to be loaded with OpenMusic, 
or their source files have to be located in the folder of your external libraries whose path 
is specified in the OpenMusic preferences.
|#

;(require-library "RepMus")
;(require-library "bordeaux-threads")
;;;;============================================================================


(defvar *improtek-source-dir* nil)
(setf *improtek-source-dir* (append (pathname-directory *load-pathname*) (list "sources")))

(defvar *file_defining_path* nil)
(setf file_defining_path (merge-pathnames
                    (make-pathname :name "WorkDirectory" :type "lisp" 
                                   :directory (append (pathname-directory *load-pathname*) (list "sources" "1-Load-Init")))
                    *load-pathname*))



(defvar *improtek-lib-files* nil)
(setf *improtek-lib-files* '(
                             ;"0-Dependencies/bordeaux-threads/bordeaux-threads"
                             ;"1-Load-Init/cleansources"
                             "1-Load-Init/WorkDirectory"
                             "1-Load-Init/WorkDirectory_Paths"

                             "2-GenerationProcesses/Oracle"
                             "2-GenerationProcesses/Label-Data-Event"
                             "2-GenerationProcesses/PrefixIndexing"
                             "2-GenerationProcesses/Improvizer"
                             
                             "3-EventSubclasses/1-Labels/Harm-AudioDescr"  
                             "3-EventSubclasses/2-Data/Midi-Audio" 
                             "3-EventSubclasses/3-Events/EventsSubclasses" 
                             "3-EventSubclasses/4-Tools/Midifile_tools" 
                             "3-EventSubclasses/4-Tools/Midiharmbeatlist_tools"
                             "3-EventSubclasses/4-Tools/Audiobeat_tools"
                             ;"3-EventSubclasses/Melobeatlist"
                             
                             "6-Data/Memories/ImprotekData"
                             "8-Others/HarmoArrang/Substitution"

                             "4-Save-Send-Receive/Antescofo"
                             "4-Save-Send-Receive/Save-Format-Send"

                             "5-HandleImpro/Tune"
                             ;"5-HandleImpro/RealTimeTools/AsynchEventDrivenProdCons"
                             "5-HandleImpro/RealTimeImprovizer"
                             "5-HandleImpro/RealTimeTools/MidiToolsForRealTime"
                             "5-HandleImpro/RealTimeTune"
                            
                             ;"8-Others/Styles/Hermeto";
                             "6-Data/Scenarios/Harm/HarmScenarios"
                             "6-Data/Scenarios/Harm/MyHarmScenarios"
                             ;"6-Data/Scenarios/AudioDescr/CoupleDescrAudioScenarios"
                             "6-Data/Memories/HackOldData"

                             "4-Save-Send-Receive/HandleReceivedOSC"
                             ))


(mapc #'(lambda (file) (compile&load (make-pathname :directory *improtek-source-dir* :name file))) *improtek-lib-files*)



