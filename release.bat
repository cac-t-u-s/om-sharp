cd om\build\

"C:\\Program Files (x86)\\LispWorks\\lispworks-7-1-0-x86-win32.exe" -init deliver.lisp

cd ..\..

rmdir _BUILD
mkdir _BUILD

xcopy om _BUILD\om-sharp\ /E /y

del /S _BUILD\om-sharp\*.*~
del /S _BUILD\om-sharp\resources\fonts\*.otf
del /S _BUILD\om-sharp\*.ofasl
del /F/Q/S _BUILD\om-sharp\resources\lib\*.* > NUL
rmdir /Q/S _BUILD\om-sharp\resources\lib\

copy "om\resources\lib\win32\libportmidi.dll" _BUILD\om-sharp\
copy "om\resources\lib\win32\libsndfile-1.dll" _BUILD\om-sharp\
copy "om\resources\lib\win32\libsamplerate-0.dll" _BUILD\om-sharp\
copy "om\resources\lib\win32\OMAudioLib.dll" _BUILD\om-sharp\
copy "om\resources\lib\win32\libsdif.dll" _BUILD\om-sharp\

