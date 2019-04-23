rmdir BUILD
mkdir BUILD

xcopy om BUILD\o7\ /E /y

copy "om\resources\lib\win32\libportmidi.dll" BUILD\o7\
copy "om\resources\lib\win32\libsndfile-1.dll" BUILD\o7\
copy "om\resources\lib\win32\libsamplerate-0.dll" BUILD\o7\
copy "om\resources\lib\win32\OMAudioLib.dll" BUILD\o7\
copy "om\resources\lib\win32\libsdif.dll" BUILD\o7\


