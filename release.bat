cd om\build\

"C:\\Program Files (x86)\\LispWorks\\lispworks-7-1-0-x86-win32.exe" -init deliver.lisp

cd ..\..

rmdir BUILD
mkdir BUILD

xcopy om BUILD\om7-beta\ /E /y

copy "om\resources\lib\win32\libportmidi.dll" BUILD\om7-beta\
copy "om\resources\lib\win32\libsndfile-1.dll" BUILD\om7-beta\
copy "om\resources\lib\win32\libsamplerate-0.dll" BUILD\om7-beta\
copy "om\resources\lib\win32\OMAudioLib.dll" BUILD\om7-beta\
copy "om\resources\lib\win32\libsdif.dll" BUILD\om7-beta\