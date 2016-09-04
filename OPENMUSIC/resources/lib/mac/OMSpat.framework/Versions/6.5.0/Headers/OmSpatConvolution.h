/************************************************************************************/
/*  FILE DESCRIPTION																*/
/*----------------------------------------------------------------------------------*/
/*!
 *   @file       OmSpatConvolution.h
 *   @brief      C-interface to libspat, for OM-Spat
 *   @author     Thibaut Carpentier
 *   @version    $(PRODUCT_VERSION)
 *   @date       16/04/2015
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_CONVOLUTION_H__
#define _OM_SPAT_CONVOLUTION_H__

#include "../src/OmSpatApi.h"
#include "../src/OmSpatAudioBuffers.h"

typedef void * OmSpatConvolver;

/************************************************************************************/
/*!
 *  @brief          Creates a spat.conv~ instance
 *  @param[in]      numChannels
 *
 *  @return         a pointer on the spat.conv~ object
 *                  or NULL if something went wrong
 *
 */
/************************************************************************************/
OM_SPAT_API
const OmSpatConvolver OmSpatCreateSpatConvolver(const unsigned int numChannels);

/************************************************************************************/
/*!
 *  @brief          Free a spat.conv~ object
 *  @param[in]      obj : a spat.conv~ object
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreeSpatConvolver(OmSpatConvolver obj);

/************************************************************************************/
/*!
 *  @brief          Performs the convolution with the current parameters
 *
 *  @param[in]      obj : a spat.conv~ object
 *  @param[in]      input : input audio buffer. The number of channels should be >= the number of sources
 *  @param[in]      numSamplesToProcess : total number of samples you want to process; if it is greater than
 *                  the number of samples in 'input' buffer, the rest is 'zero-padded'
 *  @param[out]     output : output buffer; the number of channels should match the number of speakers, and
 *                  the number of samples should be >= numSamplesToProcess. If needed, the buffer is resized internally
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 *  @details        This function is quite similar to Max/MSP spat.conv~ object
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatConvolverProcess(OmSpatConvolver obj,
                                  OmSpatAudioBuffer * output,
                                  const OmSpatAudioBuffer * input,
                                  const unsigned long numSamplesToProcess);

/************************************************************************************/
/*!
 *  @brief          Process the incoming OSC message
 *
 *  @details        List of supported messages :
 *                  (nb: you can use either float or double data types)
 *
 *                  Setting the audio configuration
 *                  /dsp/samplerate [number]
 *                  /dsp/buffersize [number]
 *                  /dsp/clear (clears the internal state of the object)
 *
 *                  Loading kernel file :
 *                  /load [string]
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatConvolverProcessOSCCommands(OmSpatConvolver obj,
                                             const char *content,
                                             const unsigned long size);

#endif /* _OM_SPAT_CONVOLUTION_H__ */

