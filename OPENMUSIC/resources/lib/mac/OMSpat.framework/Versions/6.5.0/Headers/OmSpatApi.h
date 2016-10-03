/************************************************************************************/
/*!
 *  @file       OmSpatApi.h
 *  @brief      C-interface to libspat, for OM-Spat
 *  @author     Thibaut Carpentier
 *  @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_API_H__
#define _OM_SPAT_API_H__

//==============================================================================
/// @n for now this is only for mac (gcc or clang)
#define OM_SPAT_VISIBILITY_DEFAULT	__attribute__ ((visibility ("default")))

//==============================================================================
// use this export macro to expose public method to the dylib
#ifdef __cplusplus
    #define OM_SPAT_C_EXPORTS   extern "C"
#else
    #define OM_SPAT_C_EXPORTS
#endif

//==============================================================================
#define OM_SPAT_API OM_SPAT_C_EXPORTS OM_SPAT_VISIBILITY_DEFAULT

#include <stdbool.h>    ///< include boolean for C interface


/************************************************************************************/
/*!
 *  @brief          Returns the version of OmSpat as a string
 *
 */
/************************************************************************************/
OM_SPAT_API
const char * OmSpatGetVersion();

/************************************************************************************/
/*!
 *  @brief          Returns true if the dylib is properly initialized
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatIsInitialized();

/************************************************************************************/
/*!
 *  @brief          Initializes the dylib
 *  @details        This resets the default sampling rate, the last error message,
 *                  and other global parameters.
 *                  This function should be call at least once, when first loading the dylib
 *  @return         true on success; false otherwise, then check "OmSpatGetLastError"
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatInitialize();

/************************************************************************************/
/*!
 *  @brief          Returns the last error or an empty string if no error occured
 *
 */
/************************************************************************************/
OM_SPAT_API
const char * OmSpatGetLastError();

/************************************************************************************/
/*!
 *  @brief          Resets the last error
 *
 */
/************************************************************************************/
OM_SPAT_API
void OmSpatClearLastError();

/************************************************************************************/
/*!
 *  @brief          Sets the sampling rate used for all further operations (in Hz)
 *
 */
/************************************************************************************/
OM_SPAT_API
void OmSpatSetGlobalSamplingRate(const float samplerate);

/************************************************************************************/
/*!
 *  @brief          Returns the sampling rate (in Hz) currently in use
 *
 */
/************************************************************************************/
OM_SPAT_API
const float OmSpatGetGlobalSamplingRate();

/// private method
extern void SetLastErrorMessage(const char * msg);



//==============================================================================
// OmSpatOscBundle
//==============================================================================

/************************************************************************************/
/*!
 *  @struct         OmSpatOscBundle
 *  @brief          Utility class representing an OSC bundle. Compatible with libo interfaces
 *
 */
/************************************************************************************/
struct OM_SPAT_VISIBILITY_DEFAULT OmSpatOscBundle
{
    long len;
    char *ptr;
};

typedef struct OmSpatOscBundle OmSpatOscBundle;     ///< C-style declaration

/************************************************************************************/
/*!
 *  @brief          Prints the OSC packet on OSX Console
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatDebugOSCPacket(const char *contents,
                                const unsigned long size);

/************************************************************************************/
/*!
 *  @brief          Prints the OSC packet on OSX Console
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatDebugOSCBundle(const OmSpatOscBundle * bundle);



//==============================================================================
// OmSpatAudioBuffer
//==============================================================================

/************************************************************************************/
/*!
 *  @struct         OmSpatAudioBuffer
 *  @brief          Utility class for handling multichannel audio buffers
 *
 */
/************************************************************************************/
struct OM_SPAT_VISIBILITY_DEFAULT OmSpatAudioBuffer
{
    unsigned int numChannels;       ///< number of channels
    unsigned long numSamples;       ///< number of samples (for each channel)
    float ** data;                  ///< data[ channelIndex ][ sampleIndex ]
};

typedef struct OmSpatAudioBuffer OmSpatAudioBuffer;     ///< C-style declaration

/************************************************************************************/
/*!
 *  @brief          Resize an OmSpatAudioBuffer
 *  @param[in]      numChannels
 *  @param[in]      numSamples
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 *  @details        The OmSpatAudioBuffer must have been previously allocated;
 *                  The buffer->data field is resized if needed
 *  @details        There is no guarantee that the content will be preserve when resizing
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatResizeAudioBuffer(OmSpatAudioBuffer * buffer,
                                   const unsigned int numChannels,
                                   const unsigned long numSamples);

/************************************************************************************/
/*!
 *  @brief          Free an OmSpatAudioBuffer
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreeAudioBuffer(OmSpatAudioBuffer * buffer);



//==============================================================================
// OmSpatComponent
//==============================================================================
typedef void OmSpatComponent;

/************************************************************************************/
/*!
 *  @brief          Creates a new (non-DSP) component
 *  @param[in]      componentType : type of component to create
 *                  e.g. : "spat.viewer", "spat.equalizer", "spat.compressor", etc.
 *  @return         a pointer to the newly created component or nullptr if error
 *
 */
/************************************************************************************/
OM_SPAT_API
OmSpatComponent * OmSpatCreateComponentWithType(const char * componentType);

/************************************************************************************/
/*!
 *  @brief          Creates a new DSP component
 *  @param[in]      componentType : type of component to create
 *                  e.g. : "spat.pan~"
 *  @param[in]      numInputs : number of input audio channels
 *  @param[in]      numOutputs : number of output audio channels
 *  @return         a pointer to the newly created component or nullptr if error
 *
 */
/************************************************************************************/
OM_SPAT_API
OmSpatComponent * OmSpatCreateDspComponentWithType(const char * componentType,
                                                   const unsigned int numInputs,
                                                   const unsigned int numOutputs);

/************************************************************************************/
/*!
 *  @brief          Process the audio component with the current parameters
 *  @param[in]      obj : a DSP component
 *  @param[in]      input : input audio buffer. The number of channels should be >= the number of sources
 *  @param[in]      numSamplesToProcess : total number of samples you want to process; if it is greater than
 *                  the number of samples in 'input' buffer, the rest is 'zero-padded'
 *  @param[out]     output : output buffer; the number of channels should match the number of speakers, and
 *                  the number of samples should be >= numSamplesToProcess. If needed, the buffer is resized internally
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatProcessAudio(OmSpatComponent * obj,
                              OmSpatAudioBuffer * output,
                              const OmSpatAudioBuffer * input,
                              const unsigned long numSamplesToProcess);

OM_SPAT_API
const bool OmSpatIsValidComponentType(const char * componentType);

OM_SPAT_API
const bool OmSpatIsGuiComponent(const OmSpatComponent * obj);

OM_SPAT_API
const bool OmSpatIsDspComponent(const OmSpatComponent * obj);

OM_SPAT_API
const char * OmSpatGetComponentType(const OmSpatComponent * obj);

/************************************************************************************/
/*!
 *  @brief          Free an OmSpatComponent
 *  @param[in]      obj : a component
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreeComponent(OmSpatComponent * obj);

/************************************************************************************/
/*!
 *  @brief          Process the incoming OSC bundle
 *  @param[in]      obj : a component
 *  @param[in]      bundle : the OSC OSC bundle
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatProcessOscCommands(OmSpatComponent * obj,
                                    const OmSpatOscBundle * bundle);

/************************************************************************************/
/*!
 *  @brief          Returns an OSC bundle representing this object
 *  @param[in]      obj : a component
 *
 */
/************************************************************************************/
OM_SPAT_API
OmSpatOscBundle * OmSpatGetCurrentStateAsOscBundle(OmSpatComponent * obj);

/************************************************************************************/
/*!
 *  @brief          C-style callback called whenever something changes in the component
 *  @param[in]      obj : the component that is changing
 *  @param[in]      bundle : the OSC bundle
 *
 */
/************************************************************************************/
typedef void (*OmSpatOscCallback)( OmSpatComponent * obj,
                                   const OmSpatOscBundle * bundle );

/************************************************************************************/
/*!
 *  @brief          Register a C-callback function
 *  @param[in]      obj : a component
 *  @param[in]      theCallbackFunction : the C-callback function
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatRegisterOscCallback(OmSpatComponent * obj,
                                     OmSpatOscCallback theCallbackFunction);

//==============================================================================
// NSView
//==============================================================================
/// a NSView
typedef void OmSpatNsViewHandler;

/************************************************************************************/
/*!
 *  @brief          Installs a GUI component into a NSView
 *  @param[in]      obj : a GUI component
 *  @param[in]      nsview : a NSView (nb : must be already installed in a NSWindow)
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatInstallComponentInNSView(OmSpatComponent * obj,
                                          OmSpatNsViewHandler * nsview);

#endif /* _OM_SPAT_API_H__ */
