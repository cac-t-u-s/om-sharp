cd om\build\

"C:\\Program Files (x86)\\LispWorks\\lispworks-7-1-0-x86-win32.exe" -init deliver.lisp

cd ..\..

rmdir BUILD
mkdir BUILD

xcopy om BUILD\om-sharp\ /E /y

del /S BUILD\om-sharp\*.*~
del /S BUILD\om-sharp\resources\fonts\*.otf
del /S BUILD\om-sharp\*.ofasl
del /F/Q/S BUILD\om-sharp\resources\lib\*.* > NUL
rmdir /Q/S BUILD\om-sharp\resources\lib\

copy "om\resources\lib\win32\libportmidi.dll" BUILD\om-sharp\
copy "om\resources\lib\win32\libsndfile-1.dll" BUILD\om-sharp\
copy "om\resources\lib\win32\libsamplerate-0.dll" BUILD\om-sharp\
copy "om\resources\lib\win32\OMAudioLib.dll" BUILD\om-sharp\
copy "om\resources\lib\win32\libsdif.dll" BUILD\om-sharp\

