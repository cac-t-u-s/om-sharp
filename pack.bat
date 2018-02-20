rmdir BUILD
mkdir BUILD

xcopy om BUILD\o7\ /E /y

copy "C:\Program Files (x86)\LispWorks\libPortMidi.dll" BUILD\o7\
copy "C:\Program Files (x86)\LispWorks\libsndfile-1.dll" BUILD\o7\
copy "C:\Program Files (x86)\LispWorks\libsamplerate.dll" BUILD\o7\
copy "C:\Program Files (x86)\LispWorks\OMAudioLib.dll" BUILD\o7\
copy "C:\Program Files (x86)\LispWorks\sdif.dll" BUILD\o7\


