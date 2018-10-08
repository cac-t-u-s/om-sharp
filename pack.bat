rmdir BUILD
mkdir BUILD

xcopy om7 BUILD\om7\ /E /y

copy "C:\Program Files (x86)\LispWorks\libPortMidi.dll" BUILD\om7\
copy "C:\Program Files (x86)\LispWorks\libsndfile-1.dll" BUILD\om7\
copy "C:\Program Files (x86)\LispWorks\libsamplerate.dll" BUILD\om7\
copy "C:\Program Files (x86)\LispWorks\OMAudioLib.dll" BUILD\om7\
copy "C:\Program Files (x86)\LispWorks\sdif.dll" BUILD\om7\


