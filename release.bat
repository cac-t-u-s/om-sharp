cd build\

"C:\\Program Files (x86)\\LispWorks\\lispworks-7-1-0-x86-win32.exe" -init deliver.lisp

cd ..

rmdir _BUILD
mkdir _BUILD\om-sharp\

xcopy  build _BUILD\om-sharp\ /E /y
xcopy  help-patches _BUILD\om-sharp\ /E /y
xcopy  init _BUILD\om-sharp\ /E /y
xcopy  resources _BUILD\om-sharp\ /E /y
xcopy  src _BUILD\om-sharp\ /E /y
xcopy  om-sharp.exe _BUILD\om-sharp\ /E /y

del /S _BUILD\om-sharp\*.*~
del /S _BUILD\om-sharp\resources\fonts\*.otf
del /S _BUILD\om-sharp\*.ofasl
del /F/Q/S _BUILD\om-sharp\resources\lib\*.* > NUL
rmdir /Q/S _BUILD\om-sharp\resources\lib\

copy "resources\lib\win32\libportmidi.dll" _BUILD\om-sharp\
copy "resources\lib\win32\libsndfile-1.dll" _BUILD\om-sharp\
copy "resources\lib\win32\libsamplerate-0.dll" _BUILD\om-sharp\
copy "resources\lib\win32\OMAudioLib.dll" _BUILD\om-sharp\
copy "resources\lib\win32\libsdif.dll" _BUILD\om-sharp\

