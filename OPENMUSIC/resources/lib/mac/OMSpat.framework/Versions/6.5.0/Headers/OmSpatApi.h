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
 *
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
// Types definitions
//==============================================================================
/// a NSView
typedef void OmSpatNsViewHandler;

typedef void OmSpatComponent;

/************************************************************************************/
/*!
 *  @struct         OmSpatOscBundle
 *  @brief          Utility class representing an OSC bundle. Compatible with libo interfaces
 *  @ingroup        omspat
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
 *  @struct         OmSpatAudioBuffer
 *  @brief          Utility class for handling multichannel audio buffers
 *  @ingroup        omspat
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
 *  @brief          Creates a new component
 *  @param[in]      componentType : type of component to create
 *                  e.g. : "spat.viewer", "spat.equalizer"
 *  @param[in]      componentId : a unique ID for this component
 *  @return         a pointer to the newly created component or nullptr if error
 *
 */
/************************************************************************************/
OM_SPAT_API
OmSpatComponent * OmSpatCreateComponentWithType(const char * componentType);

OM_SPAT_API
const bool OmSpatIsValidComponentType(const char * componentType);

OM_SPAT_API
const bool OmSpatIsGuiComponent(const OmSpatComponent * obj);

OM_SPAT_API
const bool OmSpatIsDspComponent(const OmSpatComponent * obj);

OM_SPAT_API
const char * OmSpatGetComponentType(const OmSpatComponent * obj);

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
 *  @brief          C-style callback called whenever something changes in the component
 *  @param[in]      obj : the component that is changing
 *  @param[in]      bundle : the OSC message or bundle
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
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatRegisterOscCallback(OmSpatComponent * obj,
                                     OmSpatOscCallback theCallbackFunction);

OM_SPAT_API
OmSpatOscBundle * OmSpatGetCurrentStateAsOscBundle(OmSpatComponent * obj);

/************************************************************************************/
/*!
 *  @brief          Installs aGUI component into a NSView
 *  @param[in]      obj : a GUI component
 *  @param[in]      nsview : a NSView
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatInstallComponentInNSView(OmSpatComponent * obj,
                                          OmSpatNsViewHandler * nsview);

#endif /* _OM_SPAT_API_H__ */
