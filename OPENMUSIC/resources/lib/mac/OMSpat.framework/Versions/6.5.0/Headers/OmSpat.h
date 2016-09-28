/************************************************************************************/
/*!
 *  @file       OmSpat.h
 *  @brief      C-interface to libspat, for OM-Spat
 *  @author     Thibaut Carpentier
 *  @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_H__
#define _OM_SPAT_H__

#include "../src/OmSpatApi.h"
#include "../src/OmSpatAudioBuffers.h"
#include "../src/OmSpatCoordinates.h"

/************************************************************************************/
/*!
 *  @brief          Performs signal matrixing with static gains
 *
 *  @param[in]      input : input audio buffer.
 *  @param[in]      numSamplesToProcess : total number of samples you want to process; if it is greater than
 *                  the number of samples in 'input' buffer, the rest is 'zero-padded'
 *  @param[in]      gains : linear gains of the matrix.
 *                  The size of the matrix is numInputs x numOutputs
 *                  where numInputs is the number of input channels
 *                  and numOutputs the number of output channels.
 *
 *                  'gains' shall have a size of (numInputs x numOutputs), row-major
 *                  For the i-th input channel and j-th output channel, the gain is
 *                  gains[ i * numOutputs + j ]
 *
 *  @param[out]     output : output buffer
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 *  @details        This is similar to Max/MSP spat.matrix~
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatMatrixingStatic(OmSpatAudioBuffer * output,
                                 const OmSpatAudioBuffer * const input,
                                 const unsigned long numSamplesToProcess,
                                 const float * const gains);

/************************************************************************************/
/*!
 *  @brief          Performs filtering with a 2-poles 2-zeros filter
 *                  with static filter parameters.
 *
 *  @param[in]      input : input audio buffer.
 *  @param[in]      numSamplesToProcess : total number of samples you want to process; if it is greater than
 *                  the number of samples in 'input' buffer, the rest is 'zero-padded'
 *  @param[in]      freq : filter's frequency in Hz
 *  @param[in]      Q : filter's Q factor
 *  @param[in]      gain : filter's gain in dB
 *  @param[in]      type : filter's topology ("lowpass", "highpass", "bandpass", "bandpasspeak", "notch",
 *                  "allpass", "peakingeq", "lowshelf", "highshelf")
 *
 *  @param[out]     output : output buffer (same size as the input buffer)
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 *  @details        The filter is applied to each channel of the buffer
 *  @details        This is similar to Max/MSP spat.eq~
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatTwoPolesTwoZerosFilteringStatic(OmSpatAudioBuffer * output,
                                                 const OmSpatAudioBuffer * const input,
                                                 const unsigned long numSamplesToProcess,
                                                 const float freq,
                                                 const float Q,
                                                 const float gain,
                                                 const char * type);

#endif /* _OM_SPAT_H__ */

