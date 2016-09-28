/************************************************************************************/
/*!
 *  @file       OmSpatAlloc.h
 *  @brief      C-interface to libspat, for OM-Spat
 *  @author     Thibaut Carpentier
 *  @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_ALLOC_H__
#define _OM_SPAT_ALLOC_H__

#include "../src/OmSpatApi.h"
#include "../src/OmSpatAudioBuffers.h"
#include "../src/OmSpatCoordinates.h"

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

/************************************************************************************/
/*!
 *  @brief          Resize an OmSpatPoints
 *  @param[in]      numPoints
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 *  @details        The OmSpatPoints must have been previously allocated;
 *                  The points->data field is resized if needed
 *  @details        There is no guarantee that the content will be preserve when resizing
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatResizePoints(OmSpatPoints * points,
                              const unsigned int numPoints);

/************************************************************************************/
/*!
 *  @brief          Free an OmSpatPoints
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreePoints(OmSpatPoints * points);

#endif /* _OM_SPAT_ALLOC_H__ */
