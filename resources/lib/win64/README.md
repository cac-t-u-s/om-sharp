# Building 64-bit DLL resources for OpenMusic

- Download and install WinGW-w64: https://sourceforge.net/projects/mingw-w64/
- Download and install CMake: https://cmake.org/
- Download and install Java JDK: http://www.oracle.com/technetwork/java/javase/downloads/index.html
- Download and install Python 3.x.x (64-bit): https://www.python.org/downloads/windows/

Note: "C:\omresources" will be used as a temporary location for sources and builds

## PortMidi
 1. Get latest sources: http://portmedia.sourceforge.net/portmidi/
 2. Extract to C:\omresources\src\portmidi
 3. Launch cmake-gui.exe (C:\Program Files\CMake\bin)
 4. Set source code path ('Where is the source code') to C:/omresources/src/portmidi
 5. Set build path ('Where to build the binaries') to C:/omresources/build/portmidi
 6. Hit 'Configure' ('Yes' to create build directory if prompted)
 7. Specify 'MinGW Makefiles' as generator for this project, select 'Specify native compilers' and hit 'Next'
 8. Specify path for C compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/gcc.exe) and C++ compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/g++.exe), then hit 'Finish'
 9. Wait for CMake to check compilers, then toggle ON the 'Advanced' checkbox
10. Set CMAKE_BUILD_TYPE to Release
11. Check that paths to Java libraries are correct.
    Eg. on my system:
    - JAVA_AWT_INCLUDE_PATH C:/Program Files/Java/jdk1.8.0_162/include
    - JAVA_AWT_LIBRARY      C:/Program Files/Java/jdk1.8.0_162/lib/jawt.lib
    - JAVA_INCLUDE_PATH     C:/Program Files/Java/jdk1.8.0_162/include
    - JAVA_INCLUDE_PATH2    C:/Program Files/Java/jdk1.8.0_162/include/win32
    - JAVA_JVM_LIBRARY      C:/Program Files/Java/jdk1.8.0_162/lib/jvm.lib
12. Hit 'Configure'
13. Hit 'Generate'
14. Open Command Prompt and cd to C:\omresources\build\portmidi
15. Run "C:\Program Files\mingw-w64\x86_64-$MINGW-VERSION\mingw64\bin\mingw32-make.exe"
16. libportmidi.dll is built in C:\omresources\build\portmidi

## libsndfile
 1. Get latest sources: https://github.com/erikd/libsndfile
 2. Extract to C:\omresources\src\libsndfile
 3. Launch cmake-gui.exe (C:\Program Files\CMake\bin)
 4. Set source code path ('Where is the source code') to C:/omresources/src/libsndfile
 5. Set build path ('Where to build the binaries') to C:/omresources/build/libsndfile
 6. Hit 'Configure' ('Yes' to create build directory if prompted)
 7. Specify 'MinGW Makefiles' as generator for this project, select 'Specify native compilers' and hit 'Next'
 8. Specify path for C compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/gcc.exe) and C++ compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/g++.exe), then hit 'Finish'
 9. Wait for CMake to check compilers, then toggle ON the 'Advanced' checkbox
10. Set CMAKE_BUILD_TYPE to Release
11. Set ENABLE_STATIC_RUNTIME to 'ON'
12. Check that path to Python executable is correct.
    Eg. on my system:
    - PYTHON_EXECUTABLE C:/Users/$USER/AppData/Local/Programs/Python/Python36/python.exe
13. Hit 'Configure'
14. Hit 'Generate'
15. Open Command Prompt and cd to C:\omresources\build\libsndfile
16. Run "C:\Program Files\mingw-w64\x86_64-$MINGW-VERSION\mingw64\bin\mingw32-make.exe"
17. libsndfile-1.dll is built in C:\omresources\build\libsndfile

## libsamplerate
 1. Get latest sources: https://github.com/erikd/libsamplerate
 2. Extract to C:\omresources\src\libsamplerate
 3. Launch cmake-gui.exe (C:\Program Files\CMake\bin)
 4. Set source code path ('Where is the source code') to C:/omresources/src/libsamplerate
 5. Set build path ('Where to build the binaries') to C:/omresources/build/libsamplerate
 6. Hit 'Configure' ('Yes' to create build directory if prompted)
 7. Specify 'MinGW Makefiles' as generator for this project, select 'Specify native compilers' and hit 'Next'
 8. Specify path for C compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/gcc.exe) and C++ compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/g++.exe), then hit 'Finish'
 9. Wait for CMake to check compilers, then toggle ON the 'Advanced' checkbox
10. Set CMAKE_BUILD_TYPE to Release
11. Hit 'Add Entry':
    - Name:  BUILD_SHARED_LIBS
    - Type:  STRING
    - Value: ON  
    'OK' to confirm the dialog
11. Set ENABLE_STATIC_RUNTIME to 'ON'
12. SNDFILE_INCLUDE_DIR C:/omresources/src/libsndfile/src
13. SNDFILE_LIBRARY C:/omresources/build/libsndfile/libsndfile-1.dll
13. Hit 'Configure'
14. Hit 'Generate'
15. *The example sources currently seem to block compilation, so remove them from CMakeLists.txt:*
    - Open C:\OMRESOURCES\src\libsamplerate\CMakeLists.txt in an editor
    - Find and remove:  
    set(EXAMPLE_SRCS  
	    ${PROJECT_SOURCE_DIR}/examples/sndfile-resample.c  
	    ${PROJECT_SOURCE_DIR}/examples/timewarp-file.c  
	    ${PROJECT_SOURCE_DIR}/examples/varispeed-play.c)
    - Save, and close editor
15. Open Command Prompt and cd to C:\omresources\build\libsamplerate
16. Run "C:\Program Files\mingw-w64\x86_64-$MINGW-VERSION\mingw64\bin\mingw32-make.exe"
17. libsamplerate-0.dll is built in C:\omresources\build\libsamplerate

## SDIF
 1. Get latest sources: https://sourceforge.net/projects/sdif/files/sdif/
 2. Extract to C:\omresources\src\SDIF
 3. *CMakeLists.txt causes an error in CMake:*
    - Open C:\OMRESOURCES\src\libsamplerate\CMakeLists.txt in an editor
    - Find and remove the line:  
    INCLUDE(SET_COMPILER_FLAGS)
 4. Launch cmake-gui.exe (C:\Program Files\CMake\bin)
 5. Set source code path ('Where is the source code') to C:/omresources/src/SDIF
 6. Set build path ('Where to build the binaries') to C:/omresources/build/SDIF
 7. Hit 'Configure' ('Yes' to create build directory if prompted)
 8. Specify 'MinGW Makefiles' as generator for this project, select 'Specify native compilers' and hit 'Next'
 9. Specify path for C compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/gcc.exe) and C++ compiler (by default C:/Program Files/mingw-w64/x86_64-$MINGW-VERSION/mingw64/bin/g++.exe), then hit 'Finish'
10. Wait for CMake to check compilers, then toggle ON the 'Advanced' checkbox
11. Set CMAKE_BUILD_TYPE to release
12. Hit 'Configure'
13. Hit 'Generate'
14. Open Command Prompt and cd to C:\omresources\build\SDIF
15. Run "C:\Program Files\mingw-w64\x86_64-$MINGW-VERSION\mingw64\bin\mingw32-make.exe"
16. libsdif.dll is built in C:\omresources\build\SDIF\bin