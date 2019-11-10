
## Visual Programming | Computer-Aided Music Compositon


**om7-beta** is a new implementation of [OpenMusic](http://repmus.ircam.fr/openmusic/) (OM), a visual programming language based on [Common Lisp](http://www.gigamonkeys.com/book/introduction-why-lisp.html), dedicated to computer-assisted music composition.

<img src="./docs/images/scores.png" width="50%" align="right">

Visual programs are created by assembling and connecting icons representing Lisp functions and data structures, built-in control structures (e.g. loops), and other program constructs. The visual language can therefore be used for general-purpose programming, and reuse any existing Common Lisp code. At a more specialized level, a set of in-built tools and external libraries make it a powerful environment for music composition. Various classes implementing musical structures are provided, associated with graphical editors including common music notation, MIDI, OSC, 2D/3D curves, and audio buffers.

------

### Download

Download for macOS, Windows, and Linux here:     

[https://github.com/openmusic-project/om7/releases/latest](https://github.com/openmusic-project/om7/releases/latest)


------

### What's new ?

om7-beta includes a brand-new generation of tools and features in your favorite computer-assisted composition environment:

- New **[patching interfaces and environment](pages/patch)** with easier box inspection / display control / automatic alignment / connections / etc.
- **No workspace** to set-up: open your documents and simply organize them in your usual file-system. 
- Interactive visualization of **Lisp code** corresponding to visual programs.
- A native implementation of the **[reactive mode](pages/reactive)** for visual program execution.
- **New [loops](pages/loop)**. Embed iterative processes in standard patches. Use a collection of new collectors and memory utilities. 
- A new set of **[interface components](pages/interface-boxes)**: list-selection, switch, button, slider, ... / Lock your patch for faster interaction.
- A redesigned **[_maquette_ / sequencer](pages/maquette)** interface including dual program/tracks-based visualization, meta-programming tools, and reactive execution modes.
- Human-readable, easily **editable text format** for patches and other documents. Possibility to read and edit patches as text.
- New **score editors**, BPF/BPC editors, etc. Nicer display. Easier edit. 
- **Collection** : a versatile container handling the storage, visualization and editing of collection of objects.
- A **time-based model** for "executable" objects, including dynamic function execution and data send/transfer possibility.
- Dynamic-memory allocated **audio buffers** (no need to store all your sounds in external files anymore).
- New **MIDI-Track** object / editor. 
- A framework for handling **OSC** data and bundles
- ...

------

### Compatibility

om7-beta can load most older OM-generated patches and programs. See the [how to import OM6 patches](pages/import-from-om6).

Most OM6 external libraries are easily portable (or already ported). See [how to create or adapt a library](pages/how-to-create-library). 

Do not hestitate to report any problems in porting or converting libraries or patches on the [user discussion forum](https://discussion.forum.ircam.fr/c/om7).


------

### Documentation

User [documentation](./docs/pages/index.md) is in progress. In the meantime, the [OM6 User Documentation](http://support.ircam.fr/docs/om/om6-manual/) can be useful to find out about the basics of OM visual programming workflow.    

--- See also this <a href="https://hal.archives-ouvertes.fr/hal-01567619" target="_blank">ICMC'17 paper</a> for a quick overview.


------

### Help / Bug reports / Community

A discussion group is hosted on Ircam Forumnet: [https://discussion.forum.ircam.fr/c/om7](https://discussion.forum.ircam.fr/c/om7)

=> Create an account in order to post questions and replies.    
Subscribe to group notifications using _Watching_ / _Tracking_ and other options.



------
### Sources and Licensing

Source repository: [https://github.com/openmusic-project/om7/](https://github.com/openmusic-project/om7/)

om7-beta is a free software distributed under the GPLv3 license. 
As a Common Lisp program, the environment can be considered just as an extension of Lisp including the specific built-in features of the application. It is also possible to compile, load and run OpenMusic sources in a Lisp environment, using the adequate compiler.

While the sources are available under the GPL license, the application is developed with [LispWorks 7.1.2](http://www.lispworks.com/): a commercial Lisp environment providing multiplatform support and graphical/GUI toolkits. A free (limited) edition of LW6 is available on the LispWorks website, but unfortunately no free version of LW-7 exists at the moment.

In order to contribute to the code without a LispWorks license, one must therefore work both with the source package _and_ an [up-to-date reseased version on the distributed executable](https://github.com/openmusic-project/om7/releases) (which includes a Lisp interpreter).

<center><img src="./docs/images/lisp.jpg" width="100pix" margin="10px"></center>



------

### Libraries 

_– Unzip the libraries in a folder and specify this folder in the om7 Preferences/Libraries/_    

<br>

<table width="90%">
<tr>
<td>Compatible external libraries:</td>
<td>Connection with external/DSP tools:</td>
</tr>

<tr>
<td>
<ul>
  <li> <a href="https://github.com/openmusic-project/Repmus" target="_blank">Repmus</a></li>
  <li> <a href="https://github.com/openmusic-project/Chaos" target="_blank">Chaos</a></li>
  <li> <a href="https://github.com/openmusic-project/Alea" target="_blank">Alea</a></li>
  <li> <a href="https://github.com/openmusic-project/Esquisse" target="_blank">Esquisse</a></li>
  <li> <a href="https://github.com/openmusic-project/Profile" target="_blank">Profile</a></li>
  <li> <a href="https://github.com/openmusic-project/LZ" target="_blank">LZ</a></li>
  <li> <a href="https://github.com/openmusic-project/Filters" target="_blank">Filters</a></li>
  <li> <a href="https://github.com/openmusic-project/Combine" target="_blank">Combine</a></li>
  <li> <a href="https://github.com/openmusic-project/Mathtools" target="_blank">Mathtools</a></li>
  <li> <a href="https://github.com/openmusic-project/Patterns" target="_blank">Patterns</a></li>
  <li> <a href="https://github.com/openmusic-project/Morphologie" target="_blank">Morphologie</a></li>
  <li> <a href="https://github.com/openmusic-project/OMTimePack" target="_blank">OMTimePack</a></li>
  <li> <a href="https://github.com/openmusic-project/OMCS" target="_blank">OMCS</a></li>
  <li> <a href="https://github.com/openmusic-project/OMRC" target="_blank">OMRC</a></li>
  <li> <a href="https://github.com/openmusic-project/omchroma" target="_blank">OMChroma</a></li>
  <li> <a href="https://github.com/openmusic-project/omai" target="_blank">OMAI</a></li>
</ul>
</td>

<td>
<ul>
  <li> <a href="https://github.com/openmusic-project/om-csound" target="_blank">om-csound</a></li>
  <li> <a href="https://github.com/openmusic-project/om-supervp" target="_blank">OM-SuperVP</a></li>
  <li> <a href="https://github.com/openmusic-project/om-pm2" target="_blank">OM-pm2</a></li>
  <li> <a href="https://github.com/openmusic-project/OM-Spat" target="_blank">OM-Spat</a></li>
  <li> <a href="https://github.com/openmusic-project/om-iae" target="_blank">OM-IAE</a></li>
  <li> <a href="https://github.com/openmusic-project/om-xmm" target="_blank">OM-XMM</a></li>
  <li> <a href="https://github.com/DYCI2/om-dyci2" target="_blank">OM-Dyci2</a></li>
  <li> <a href="https://github.com/openmusic-project/odot" target="_blank">libo/odot</a></li>
</ul>
</td>
</tr></table>



-------

### Publications

om7-beta has been used as a support for research and production in a number of recent projects.
See related papers below:

  * [OM-AI: A Toolkit to Support AI-Based Computer-Assisted Composition Workflows in OpenMusic](https://hal.archives-ouvertes.fr/hal-02126847). Anders Vinjar, Jean Bresson. Sound and Music Computing conference (SMC'19), Málaga, Spain, 2019.
  * [Musical Gesture Recognition Using Machine Learning and Audio Descriptors](https://hal.archives-ouvertes.fr/hal-01839050). Paul Best, Jean Bresson, Diemo Schwarz. International Conference on Content-Based Multimedia Indexing (CBMI'18), La Rochelle, France, 2018.
  * [From Motion to Musical Gesture: Experiments with Machine Learning in Computer-Aided Composition](https://hal.archives-ouvertes.fr/hal-01815988/document). Jean Bresson, Paul Best, Diemo Schwarz, Alireza Farhang. Workshop on Musical Metacreation (MUME2018), Internationa Conference on Computational Creativity (ICCC’18), Salamanca, Spain, 2018.
  * [Symbolist: An Open Authoring Environment for End-user Symbolic Notation](https://hal.archives-ouvertes.fr/hal-01804933/document). Rama Gottfried, Jean Bresson. International Conference on Technologies for Music Notation and Representation (TENOR'18), Montreal, Canada, 2018. 
  * **[Next-generation Computer-aided Composition Environment: A New Implementation of OpenMusic](https://hal.archives-ouvertes.fr/hal-01567619/document)**. Jean Bresson, Dimitri Bouche, Thibaut Carpentier, Diemo Schwarz, Jérémie Garcia. International Computer Music Conference (ICMC’17), Shanghai, China, 2017.
  * [Landschaften – Visualization, Control and Processing of Sounds in 3D Spaces](https://hal.archives-ouvertes.fr/hal-01567629/document). Savannah Agger, Jean Bresson, Thibaut Carpentier. International Computer Music Conference (ICMC’17), Shanghai, China, 2017.
  * [Timed Sequences: A Framework for Computer-Aided Composition with Temporal Structures](https://hal.archives-ouvertes.fr/hal-01484077/document). Jérémie Garcia, Dimitri Bouche, Jean Bresson. International Conference on Technologies for Music Notation and Representation (TENOR’17), A Coruña, Spain, 2017.
  * [Computer-aided Composition of Musical Processes](https://hal.archives-ouvertes.fr/hal-01370792/document). Dimitri Bouche, Jérôme Nika, Alex Chechile, Jean Bresson. Journal of New Music Research, 46(1), 2017.
  * [Interactive-Compositional Authoring of Sound Spatialization](https://hal.inria.fr/hal-01467080/document). Jérémie Garcia, Thibaut Carpentier, Jean Bresson. Journal of New Music Research, 46(1), 2017.
  * [o.OM: Structured-Functional Communication between Computer Music Systems using OSC and Odot](https://hal.archives-ouvertes.fr/hal-01353794/document). Jean Bresson, John MacCallum, Adrian Freed. ACM SIGPLAN Workshop on Functional Art, Music, Modeling & Design (FARM’16), Nara, Japan, 2016.
  * [Towards Interactive Authoring Tools for Composing Spatialization](https://hal.archives-ouvertes.fr/hal-01108709/document). Jérémie Garcia, Jean Bresson, Thibaut Carpentier. IEEE 10th Symposium on 3D User Interfaces (3DUI), Arles, France, 2015.


 
------
### Highlights / contributors / timeline of the project

This project was initiated by @j-bresson in 2013. Most of the code was rewritten, but a significant part of it is largely inspired or borrowed from the OM [original sources](https://github.com/openmusic-project/OM6/) and features, including the direct or indirect contributions of its authors and contributors (@CarlosAgon, @assayag, and others).

The initial objective of the project was to redesign the environment and experiment new visual Lisp programming features.
Important developments have been carried out during the [EFFICACe](http://repmus.ircam.fr/efficace/) research project conducted at IRCAM (2013-2017), which aimed at exploring relationships between calculation, time and interactions in computer-assisted music composition processes, focusing on specific topics such as dynamic temporal structures or the control, visualization and interactive execution of sound synthesis and spatialization processes. 

A new generation of tools and editors for the representation and manipulation of musical objects (score, sounds, temporal data streams, controllers, etc.) are included, covering most operational areas of OpenMusic/computer-assisted composition processes.

The [reactive model](https://hal.archives-ouvertes.fr/hal-00959312) recently introduced in OpenMusic has been integrated as a native feature and works seamlessly in the visual programming environment.
@jeremie-gracia created a framework for timeline-based control of musical object, and new tools for the representation adn interaction with spatial audio scenes (om-spat).
@dimitribouche developed a dynamic scheduling architecture that was implemented and integrated as the main core for musical rendering and computation, as well as new interfaces for temporal representation and organization of compositional processes (a new design of the OpenMusic _maquette_).

om7-beta is now compatible with the main OM6 features, libraries and objects, and embeds an automatic translation system for loading/converting OM6 patches.    

@andersvi is maintaining the Linux distribution.
