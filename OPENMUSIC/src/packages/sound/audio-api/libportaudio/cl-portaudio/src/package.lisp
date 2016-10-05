(defpackage portaudio
  (:use :cl :cffi :ffa)
  (:nicknames :pa)
  (:documentation
   "This package contains bindings to @a[http://portaudio.com/]{PortAudio}. PortAudio is a free, cross-platform, open-source, audio I/O library.

Binary PortAudio packages can be founded here: 
@a{http://planet.plt-scheme.org/display.ss?package=portaudio.plt&owner=clements} -> source browse -> directories lib

@begin[Installation and Usage]{section}
@begin{pre}
git clone --depth 1 https://github.com/filonenko-mikhail/cl-portaudio.git
emacs
M+x slime
  (ql:quickload :cl-portaudio)
  (ql:quickload :cl-portaudio-tests)
  (portaudio-tests:test-read-write-echo)
@end{pre}
@end{section}

@begin[Example]{section}
@begin{pre}
 (use-package :portaudio)

 (defconstant +frames-per-buffer+ 1024)
 (defconstant +sample-rate+ 44100d0)
 (defconstant +seconds+ 15)
 (defconstant +sample-format+ :float)
 (defconstant +num-channels+ 2)
 
 (defun test-read-write-converted-echo ()
  \"Record input into an array; Separate array to channels; Merge channels into array; Play last array.\" 
  (with-audio
    (format t \"~%=== Wire on. Will run ~D seconds . ===~%\" +seconds+) 
    (with-default-audio-stream (astream +num-channels+ +num-channels+ :sample-format +sample-format+ :sample-rate +sample-rate+ :frames-per-buffer +frames-per-buffer+) 
      (dotimes (i (round (/ (* +seconds+ +sample-rate+) +frames-per-buffer+)))
         (write-stream astream
                                    (merge-channels-into-array astream
                                                               (separate-array-to-channels astream
                                                                                           (read-stream astream))))))))
@end{pre}

@aboutfun{with-audio}
@aboutfun{with-default-audio-stream}
@aboutfun{read-stream}
@aboutfun{separate-array-to-channels}
@aboutfun{merge-channels-into-array}
@aboutfun{write-stream}

@b{Note}
ignore-errors is used for ignoring output-underflowed error.

@end{section}
@begin[Introduction]{section}
PortAudio provides a uniform application programming interface (API) across all supported platforms. You can think of the PortAudio library as a wrapper that converts calls to the PortAudio API into calls to platform-specific native audio APIs. Operating systems often offer more than one native audio API and some APIs (such as JACK) may be available on multiple target operating systems. PortAudio supports all the major native audio APIs on each supported platform. The diagram below illustrates the relationship between your application, PortAudio, and the supported native audio APIs:

@a[./img/portaudio-external-architecture-diagram.png]{Image}

PortAudio provides a uniform interface to native audio APIs. However, it doesn't always provide totally uniform functionality. There are cases where PortAudio is limited by the capabilities of the underlying native audio API. For example, PortAudio doesn't provide sample rate conversion if you request a sample rate that is not supported by the native audio API. Another example is that the ASIO SDK only allows one device to be open at a time, so PortAudio/ASIO doesn't currently support opening multiple ASIO devices simultaneously.
@end{section}
@begin[Key abstractions: Host APIs, Devices and Streams]{section}
The PortAudio processing model includes three main abstractions: Host APIs, audio Devices and audio Streams.

Host APIs represent platform-specific native audio APIs. Some examples of Host APIs are Core Audio on Mac OS, WMME and DirectSound on Windows and OSS and ALSA on Linux. The diagram in the previous section shows many of the supported native APIs. Sometimes it's useful to know which Host APIs you're dealing with, but it is easy to use PortAudio without ever interacting directly with the Host API abstraction.

Devices represent individual hardware audio interfaces or audio ports on the host platform. Devices have names and certain capabilities such as supported sample rates and the number of supported input and output channels. PortAudio provides functions to enumerate available Devices and to query for Device capabilities.

Streams manage active audio input and output from and to Devices. Streams may be half duplex (input or output) or full duplex (simultaneous input and output). Streams operate at a specific sample rate with particular sample formats, buffer sizes and internal buffering latencies. You specify these parameters when you open the Stream. Audio data is communicated between a Stream and your application via a user provided asynchronous callback function or by invoking synchronous read and write functions.

PortAudio supports audio input and output in a variety of sample formats: 8, 16, 24 and 32 bit integer formats and 32 bit floating point, irrespective of the formats supported by the native audio API. PortAudio also supports multichannel buffers in both interleaved and non-interleaved (separate buffer per channel) formats and automatically performs conversion when necessary. If requested, PortAudio can clamp out-of range samples and/or dither to a native format.

The PortAudio API offers the following functionality:
@begin{itemize}
@item{Initialize and terminate the library}
@item{Enumerate available Host APIs}
@item{Enumerate available Devices either globally, or within each Host API}
@item{Discover default or recommended Devices and Device settings}
@item{Discover Device capabilities such as supported audio data formats and sample rates}
@item{Create and control audio Streams to acquire audio from and output audio to Devices}
@item{Provide Stream timing information to support synchronising audio with other parts of your application}
@item{Retrieve version and error information.}
@end{itemize}
These functions are described in more detail below.

@end{section}
@begin[Initialisation, termination and utility functions]{section}
The PortAudio library must be initialized before it can be used and terminated to clean up afterwards. You initialize PortAudio by calling @fun{initialize} and clean up by calling @fun{terminate}. There is @fun{with-audio} macro that does environment.

You can query PortAudio for version information using @fun{get-version} to get a numeric version number and @fun{get-version-text} to get a string.

The size in bytes of the various sample formats represented by the sample-format enumeration can be obtained using @fun{get-sample-size}.

@fun{pa-sleep} sleeps for a specified number of milliseconds. This isn't intended for use in production systems; it's provided only as a simple portable way to implement tests and examples where the main thread sleeps while audio is acquired or played by an asynchronous callback function.
@end{section}
@begin[Host APIs]{section}
A Host API acts as a top-level grouping for all of the Devices offered by a single native platform audio API. Each Host API has a unique type identifier, a name, zero or more Devices, and nominated default input and output Devices.

Host APIs are usually referenced by index: an integer of type host-api-index that ranges between zero and @code{(- (@fun{get-host-api-count}) 1)}. You can enumerate all available Host APIs by counting across this range.

You can retrieve the index of the default Host API by calling @fun{get-default-host-api}.

Information about a Host API, such as it's name and default devices, is stored in a @class{host-api-info} structure. You can retrieve a pointer to a particular Host API's @class{host-api-info} structure by calling @fun{get-host-api-info} with the Host API's index as a parameter.

Most PortAudio functions reference Host APIs by @fun{host-api-index} indices. Each Host API also has a unique type identifier defined in the host-api-type-id enumeration. You can call @fun{host-api-type-id-to-host-api-index} to retrieve the current host-api-index for a particular host-api-type-id.
@end{section}
@begin[Devices]{section}
A Device represents an audio endpoint provided by a particular native audio API. This usually corresponds to a specific input or output port on a hardware audio interface, or to the interface as a whole. Each Host API operates independently, so a single physical audio port may be addressable via different Devices exposed by different Host APIs.

A Device has a name, is associated with a Host API, and has a maximum number of supported input and output channels. PortAudio provides recommended default latency values and a default sample rate for each Device. To obtain more detailed information about device capabilities you can call @fun{is-format-supported} to query whether it is possible to open a Stream using particular Devices, parameters and sample rate.

Although each Device conceptually belongs to a specific Host API, most PortAudio functions and data structures refer to Devices using a global, Host API-independent index of type device-index – an integer of that ranges between zero and @code{(- (@fun{get-device-count}) 1)}. The reasons for this are partly historical but it also makes it easy for applications to ignore the Host API abstraction and just work with Devices and Streams.

If you want to enumerate Devices belonging to a particular Host API you can count between 0 and @code{(- (@fun{host-api-info-device-count}) 1)}. You can convert this Host API-specific index value to a global device-index value by calling @fun{host-api-device-index-to-device-index}.

Information about a Device is stored in a PaDeviceInfo structure. You can retrieve @class{device-info} structure by calling @fun{get-device-info} with the Device's index as a parameter.

You can retrieve the indices of the global default input and output devices using @fun{get-default-input-device} and @fun{get-default-output-device}. Default Devices for each Host API are stored in the Host API's @class{host-api-info} structures.

For an example of enumerating devices and printing information about their capabilities see the @fun{print-devices} function.
@end{section}
@begin[Streams]{section}
A Stream represents an active flow of audio data between your application and one or more audio Devices. A Stream operates at a specific sample rate with specific sample formats and buffer sizes.
@end{section}
@begin[Opening and Closing Streams]{section}
You call @fun{open-stream} to open a Stream, specifying the Device(s) to use, the number of input and output channels, sample formats, suggested latency values and flags that control dithering, clipping and overflow handling. You specify many of these parameters in two @class{stream-parameters} structures, one for input and one for output.

Devices may be full duplex (supporting simultaneous input and output) or half duplex (supporting input or output) – usually this reflects the structure of the underlying native audio API. When opening a Stream you can specify one full duplex Device for both input and output, or two different Devices for input and output. Some Host APIs only support full-duplex operation with a full-duplex device (e.g. ASIO) but most are able to aggregate two half duplex devices into a full duplex Stream. PortAudio requires that all devices specified in a call to @fun{open-stream} belong to the same Host API.

A successful call to @fun{open-stream} creates a pointer to a @class{pa-stream} – an opaque handle representing the open Stream. All PortAudio API functions that operate on open Streams take a pointer to a @class{pa-stream} as their first parameter.

PortAudio also provides @fun{open-default-stream} – a simpler alternative to @fun{open-stream} which you can use when you want to open the default audio Device(s) with default latency parameters.

You call @fun{close-stream} close a Stream when you've finished using it.

There are two macros to simplify work with stream: @fun{with-audio-stream} and @fun{with-default-audio-stream}. These macros open and start stream at the beginning and stop and close stream at the end. Body is protected by unwind-protect.
@end{section}
@begin[Starting and Stopping Streams]{section}
Newly opened Streams are initially stopped. You call @fun{start-stream} to start a Stream. You can stop a running Stream using @fun{stop-stream} or @fun{abort-stream} (the Stop function plays out all internally queued audio data, while Abort tries to stop as quickly as possible). An open Stream can be started and stopped multiple times. You can call @fun{is-stream-stopped} to query whether a Stream is running or stopped.
@end{section}
@begin[The Read/Write I/O Method]{section}
PortAudio provides a synchronous read/write interface for acquiring and playing audio.

To write audio data to a Stream call @fun{write-stream} and to read data call @fun{read-stream}. These functions will block if the internal buffers are full, making them safe to call in a tight loop. If you want to avoid blocking you can query the amount of available read or write space using @fun{get-stream-read-available} or @fun{get-stream-write-available}.

For examples of the read/write I/O method see the following examples in the /t directory of the PortAudio distribution: tests.lisp (@fun{portaudio-tests:test-read-write-converted-echo}).
@end{section}
@begin[Retreiving Stream Information]{section}
You can retrieve information about an open Stream by calling @fun{get-stream-info}. This returns a @class{stream-info} structure containing the actual input and output latency and sample rate of the stream. It's possible for these values to be different from the suggested values passed to @fun{open-stream}.
@end{section}
@begin[Error Handling]{section}
Most PortAudio functions invokes signal. Possible conditions are described in pa-error enum. Some functions return values greater than or equal to zero for normal results.

PortAudio usually tries to translate error conditions into portable pa-error error codes. However if an unexpected error is encountered the unanticipated-host-error code may be returned. In this case a further mechanism is provided to query for Host API-specific error information. If PortAudio throws unanticipated-host-error you can call @fun{get-last-host-error-info} to retrieve a @class{host-error-info} structure that provides more information, including the Host API that encountered the error, a native API error code and error text.
@end{section}
@begin[Conditions]{section}
There are conditions, that are translated from PaError.
@begin{itemize}
  @item{not-anticipated}
  @item{unanticipated-host-error}
  @item{invalid-channel-count}
  @item{invalid-sample-rate}
  @item{invalid-device}
  @item{invalid-flag}
  @item{sample-format-not-supported}
  @item{bad-i-o-device-combination}
  @item{insufficient-memory}
  @item{buffer-too-big}
  @item{buffer-too-small}
  @item{null-callback}
  @item{bad-stream-ptr}
  @item{timed-out}
  @item{internal-error}
  @item{device-unavailable}
  @item{incompatible-host-api-specific-stream-info}
  @item{stream-is-stopped}
  @item{stream-is-not-stopped}
  @item{input-overflowed}
  @item{output-underflowed}
  @item{host-api-not-found}
  @item{invalid-host-api}
  @item{can-not-read-from-a-callback-stream}
  @item{can-not-write-to-a-callback-stream}
  @item{can-not-read-from-an-output-only-stream}
  @item{can-not-write-to-an-input-only-stream}
  @item{incompatible-stream-host-api}
  @item{bad-buffer-ptr}
  @end{itemize}
@end{section}
@begin[Bitfields]{section}
  @b{sample-format}
  @begin{itemize}
  @item{:float}
  @end{itemize}
  @b{stream-flags}
  @begin{itemize}
  @item{:no-flag}
  @item{:clip-off}
  @item{:dither-off}
  @end{itemize}
@end{section}
@begin[Enums]{section}
  @b{host-api-type-id}
  @begin{itemize}
  @item{:in-development}
  @item{:direct-sound}
  @item{:mme}
  @item{:asio}
  @item{:sound-manager}
  @item{:core-audio}
  @item{:oss}
  @item{:alsa}
  @item{:al}
  @item{:be-os}
  @item{:wdmks}
  @item{:jack}
  @item{:wasapi}
  @item{:audio-science-hpi}
  @end{itemize}
@end{section}
"))