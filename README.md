
## Visual Programming | Computer-Aided Music Compositon


**OM#** (om-sharp) is a computer-assisted composition environment designed as new implementation of the [OpenMusic](http://repmus.ircam.fr/openmusic/) (OM) visual programming language.

<img src="./docs/images/scores.png" width="50%" align="right">

OM(#) is based on [Common Lisp](http://www.gigamonkeys.com/book/introduction-why-lisp.html) and allows to create graphical programs that are interpreted in this language. The programs are made by assembling and connecting icons representing Lisp functions and data structures, built-in control structures (e.g. loops), and other program constructs. The visual language can therefore be used for general-purpose programming, and reuse any existing Common Lisp code. At a more specialized level, a set of in-built tools and external libraries make it a powerful environment for music composition. Various classes implementing musical structures are provided, associated with graphical editors including common music notation, MIDI, OSC, 2D/3D curves, and audio buffers.

------

### Download

Download for macOS, Windows, and Linux:     
[https://github.com/openmusic-project/om7/releases/latest](https://github.com/openmusic-project/om7-beta/releases/latest)


------
### Sources and License

Source repository: [https://github.com/openmusic-project/om7-beta/](https://github.com/openmusic-project/om7-beta/)

This is a free software distributed under the GPLv3 license. 
As a Common Lisp program, the environment can be considered just as an extension of Lisp including the specific built-in features of the application. It is also possible to compile, load and run OpenMusic sources in a Lisp environment, using the adequate compiler.

While the sources are available under the GPL license, the application is developed with [LispWorks 7.1.2](http://www.lispworks.com/): a commercial Lisp environment providing multiplatform support and graphical/GUI toolkits. A free (limited) edition of LW6 is available on the LispWorks website, but unfortunately no free version of LW-7 exists at the moment.

In order to contribute to the code without a LispWorks license, one must therefore work both with the source package _and_ an [up-to-date reseased version on the distributed executable](https://github.com/openmusic-project/om7/releases) (which includes a Lisp interpreter).

<center><img src="./docs/images/lisp.png" width="90pix" margin="10px"></center>


------

### More info, documentation and resources

Project pages under construction:    
[https://openmusic-project.github.io/om7-beta/](https://openmusic-project.github.io/om7-beta/)


------

### Credits

Design and development: J. Bresson, IRCAM / STMS Lab 2013-2019.     
This project inherits and borrowed significant pieces of the code and features from OpenMusic 6 (by C. Agon, G. Assayag, J. Bresson and others --- see [OM sources](https://github.com/openmusic-project/OM6/)) including the direct or indirect contributions of its authors and numerous contributors.     
Other contribution and collaboration to design and development: D. Bouche, J. Garcia, A. Vinjar.