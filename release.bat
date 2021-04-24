cd build\

"C:\\Program Files (x86)\\LispWorks\\lispworks-7-1-0-x86-win32.exe" -init deliver.lisp

cd ..

rmdir _BUILD
mkdir _BUILD\om-sharp\

xcopy  help-patches _BUILD\om-sharp\help-patches\ /s/e/y
xcopy  init _BUILD\om-sharp\init\ /s/e/y
xcopy  resources _BUILD\om-sharp\resources\ /s/e/y
xcopy  src _BUILD\om-sharp\src\ /s/e/y
xcopy  om-sharp.exe _BUILD\om-sharp\ /y

del /S _BUILD\om-sharp\*.*~
del /S _BUILD\om-sharp\resources\fonts\*.otf
del /S _BUILD\om-sharp\src\*.ofasl
del /F/Q/S _BUILD\om-sharp\resources\lib\*.* > NUL
rmdir /Q/S _BUILD\om-sharp\resources\lib\

copy "resources\lib\win32\libportmidi.dll" _BUILD\om-sharp\
copy "resources\lib\win32\libsndfile-1.dll" _BUILD\om-sharp\
copy "resources\lib\win32\libsamplerate-0.dll" _BUILD\om-sharp\
copy "resources\lib\win32\OMAudioLib.dll" _BUILD\om-sharp\
copy "resources\lib\win32\libsdif.dll" _BUILD\om-sharp\

