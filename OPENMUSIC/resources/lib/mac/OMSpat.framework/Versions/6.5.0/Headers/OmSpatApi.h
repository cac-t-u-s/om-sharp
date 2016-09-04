/************************************************************************************/
/*  FILE DESCRIPTION																*/
/*----------------------------------------------------------------------------------*/
/*!
 *   @file       OmSpatApi.h
 *   @brief      C-interface to libspat, for OM-Spat
 *   @author     Thibaut Carpentier
 *   @version    $(PRODUCT_VERSION)
 *   @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_API_H__
#define _OM_SPAT_API_H__

/// @n for now this is only for mac (gcc or clang)
#define OM_SPAT_VISIBILITY_DEFAULT	__attribute__ ((visibility ("default")))

// use this export macro to expose public method to the dylib
#ifdef __cplusplus
    #define OM_SPAT_C_EXPORTS   extern "C"
#else
    #define OM_SPAT_C_EXPORTS
#endif

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

/// a NSView
typedef void * OmSpatNsViewHandler;

#endif /* _OM_SPAT_API_H__ */
