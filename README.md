
## **OM#**: Visual Programming | Computer-Aided Music Composition


**OM#** (om-sharp) is a visual programming environment for computer-assisted composition and music/audio data processing.

It is derived from [OpenMusic](http://repmus.ircam.fr/openmusic/) and based on [Common Lisp](http://www.gigamonkeys.com/book/introduction-why-lisp.html). 

OM# programs are made by assembling and connecting icons representing Lisp functions and data structures, built-in control structures (e.g. loops), and other program constructs, interpreted as Common Lisp expressions. 
The visual language can be used for general-purpose programming and reuse any existing Common Lisp code. 
At a more specialized level, built-in tools and libraries make it a powerful environment for music composition. Various classes implementing musical structures are provided, associated with graphical editors including common music notation, MIDI, OSC, 2D/3D curves, and audio buffers.

------

### Download

[Download for macOS, Windows, and Linux](https://github.com/cac-t-u-s/om-sharp/releases/latest)


------

### Contributing

OM# is a free software distributed under the GPLv3 license. 

As a Common Lisp program, OM# can be considered just as an extension of Lisp including the specific built-in features of the application.
The application is developed with the latest [LispWorks](http://www.lispworks.com/) compiler (7.1.2), which provides multi-platform support and graphical/GUI toolkits in Common Lisp. 
A limited "Personnal" edition of LispWorks 7 is now available: its limited heap size requires compiling sources in several successive runs, and it is not possible to create new OM# executables with it, however, it allows to load and run/use/edit the program from the sources.

Alternatively, the OM# executable also includes a Lisp interpreter which can load and evaluate modifications and extensiuons of the program sources.


------

### More info, documentation and resources

Project pages:    
[https://cac-t-u-s.github.io/om-sharp/](https://cac-t-u-s.github.io/om-sharp/)


------

### Credits

Design and development 2013-2019: J. Bresson, IRCAM STMS lab / Music Representations team; contributions by D. Bouche, J. Garcia, A. Vinjar.
This project uses code and features from the [OpenMusic](https://github.com/openmusic-project/openmusic/) project (by C. Agon, G. Assayag, J. Bresson and others, IRCAM STMS lab). 
