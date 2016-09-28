/************************************************************************************/
/*!
 *  @file       OmSpatPanning.h
 *  @brief      C-interface to libspat, for OM-Spat
 *  @author     Thibaut Carpentier
 *  @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_PANNING_H__
#define _OM_SPAT_PANNING_H__

#include "../src/OmSpatApi.h"
#include "../src/OmSpatAudioBuffers.h"
#include "../src/OmSpatCoordinates.h"

typedef void * OmSpatPanning;

/************************************************************************************/
/*!
 *  @brief          Creates a spat.pan~ instance
 *  @param[in]      numInputs
 *  @param[in]      numOutputs
 *  @param[in]      panningType : type of panning you want to use; e.g. "panr", "xy", "ms", "surround",
 *                  "angular", "dbap3d", "dbap2d", "vbap3d", "vbap2d", "vbip2d", "vbip3d", "hoa3d", "hoa2d", "spcap",
 *                  "knn", "bformat"
 *
 *  @return         a pointer on the spat.pan~ object
 *                  or nullptr if something went wrong
 *
 */
/************************************************************************************/
OM_SPAT_API
const OmSpatPanning OmSpatCreateSpatPan(const unsigned int numInputs,
                                        const unsigned int numOutputs,
                                        const char * panningType);

/************************************************************************************/
/*!
 *  @brief          Free a spat.pan~ object
 *  @param[in]      obj : a spat.pan~ object
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreeSpatPan(OmSpatPanning obj);

/************************************************************************************/
/*!
 *  @brief          Performs the panning with the current parameters
 *
 *  @param[in]      obj : a spat.pan~ object
 *  @param[in]      input : input audio buffer. The number of channels should be >= the number of sources
 *  @param[in]      numSamplesToProcess : total number of samples you want to process; if it is greater than
 *                  the number of samples in 'input' buffer, the rest is 'zero-padded'
 *  @param[out]     output : output buffer; the number of channels should match the number of speakers, and
 *                  the number of samples should be >= numSamplesToProcess. If needed, the buffer is resized internally
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 *  @details        This function is quite similar to Max/MSP spat.pan~ object
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatPanProcess(OmSpatPanning obj,
                            OmSpatAudioBuffer * output,
                            const OmSpatAudioBuffer * input,
                            const unsigned long numSamplesToProcess);


/************************************************************************************/
/*!
 *  @brief          Process the incoming OSC message or OSC bundle
 *
 *  @param[in]      obj : a spat.pan~ object
 *  @param[in]      content : the OSC message or OSC bundle
 *  @param[in]      size : size of the message or bundle
 *
 *  @details        List of supported messages :
 *                  (nb: you can use either float or double or int32 or int64 data types)
 *                  (syntax is pretty similar to Spat Max/MSP messages)
 *
 *==============================================================================
 *                  Setting the audio configuration
 *                  /dsp/samplerate [number]
 *                  /dsp/buffersize [number]
 *                  /dsp/clear (clears the internal state of the object)
 *
 *==============================================================================
 *                  Setting the source position :
 *                  /source/[index]/xyz [number] [number] [number]
 *                  /source/[index]/xy [number] [number]
 *                  /source/[index]/x [number]
 *                  /source/[index]/y [number]
 *                  /source/[index]/z [number]
 *                  /source/[index]/aed [number] [number] [number]
 *                  /source/[index]/ade [number] [number] [number]
 *                  /source/[index]/ae [number] [number]
 *                  /source/[index]/ad [number] [number]
 *                  /source/[index]/az [number]
 *                  /source/[index]/azim [number]
 *                  /source/[index]/elev [number]
 *                  /source/[index]/dist [number]
 *                  /source/[index]/azim++ [number]
 *                  /source/[index]/elev++ [number]
 *                  /source/[index]/dist++ [number]
 *                  /source/[index]/dist*= [number]
 *
 *==============================================================================
 *                  Setting the source spread
 *                  /source/[index]/spread [number]
 *
 *==============================================================================
 *                  Setting the speakers positions
 *                  /speakers/xyz [number] [number] [number] [number] [number] [number] ...
 *                  /speakers/xy [number] [number] [number] [number] ...
 *                  /speakers/aed [number] [number] [number] [number] [number] [number] ...
 *                  /speakers/ae [number] [number] [number] [number] ...
 *
 *==============================================================================
 *                  Loading hrtf file (when relevant) :
 *                  /load [string]
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatPanProcessOSCCommands(OmSpatPanning obj,
                                       const char *content,
                                       const unsigned long size);

#endif /* _OM_SPAT_PANNING_H__ */

