/************************************************************************************/
/*!
 *  @file       OmSpatAudioBuffers.h
 *  @brief      C-interface to libspat, for OM-Spat
 *  @author     Thibaut Carpentier
 *  @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_AUDIO_BUFFERS_H__
#define _OM_SPAT_AUDIO_BUFFERS_H__

#include "../src/OmSpatApi.h"

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
 *  @brief          Loads the content of an audio file into an OmSpatAudioBuffer
 *  @param[in]      filename : full path (wav or aiff or mp3 files)
 *  @param[out]     dest : an OM buffer
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 *  @details        The OmSpatAudioBuffer is resized if needed
 *  @details        The destination OmSpatAudioBuffer will be non-interleaved
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatLoadAudioBufferFromFile(OmSpatAudioBuffer * const dest,
                                         const char * filename);

/************************************************************************************/
/*!
 *  @brief          Saves the content of an OmSpatAudioBuffer into an audio file
 *  @param[in]      src : an OM buffer
 *  @param[in]      filename : full path (wav or aiff)
 *  @param[in]      samplerate : sampling rate in Hz
 *  @param[in]      bitPerSample : resolution
 *  @param[in]      overwrite : flag for overwriting existing file
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatSaveAudioBufferToFile(const OmSpatAudioBuffer * const src,
                                       const char * filename,
                                       const float samplerate,
                                       const int bitPerSample,
                                       const bool overwrite);

/************************************************************************************/
/*!
 *  @brief          Prints the content of an OmSpatAudioBuffer to the OSX Console
 *  @param[in]      src : source
 *  @param[in]      printData : also print the content of the src->data field
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatPrintAudioBuffer(const OmSpatAudioBuffer * const src,
                                  const bool printData);

/************************************************************************************/
/*!
 *  @brief          Reverse the content of an OmSpatAudioBuffer
 *  @param[in]      srcDest : the buffer (the reversal is applied in-place)
 *  @param[in]      channelIndex : index of the channel you want to reverse.
 *                  This uses 0-indexing i.e. 0 is the 1st channel
 *                  Pass channelIndex = -1 if you want to reverse all the channels
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatReverseAudioBuffer(OmSpatAudioBuffer * const srcDest,
                                    const int channelIndex);

/************************************************************************************/
/*!
 *  @brief          Applies gain to an OmSpatAudioBuffer
 *  @param[in]      srcDest : the buffer (the reversal is applied in-place)
 *  @param[in]      gain : linear gain
 *  @param[in]      channelIndex : index of the channel of interest.
 *                  This uses 0-indexing i.e. 0 is the 1st channel
 *                  Pass channelIndex = -1 if you want to apply gain to all the channels
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatApplyGainToAudioBuffer(OmSpatAudioBuffer * const srcDest,
                                        const float gain,
                                        const int channelIndex);

/************************************************************************************/
/*!
 *  @brief          Fills an OmSpatAudioBuffer with a given value
 *  @param[in]      srcDest : the buffer (the reversal is applied in-place)
 *  @param[in]      value : the value to fill
 *  @param[in]      channelIndex : index of the channel of interest.
 *                  This uses 0-indexing i.e. 0 is the 1st channel
 *                  Pass channelIndex = -1 if you want to fill to all the channels
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFillAudioBuffer(OmSpatAudioBuffer * const srcDest,
                                 const float value,
                                 const int channelIndex);

/************************************************************************************/
/*!
 *  @brief          Normalizes one channel so that its maximum matches 'newMaximumIndB'
 *  @param[in]      srcDest : the buffer (the reversal is applied in-place)
 *  @param[in]      newMaximumIndB : the desired maximum value (in dB) after normalization
 *  @param[in]      channelIndex : index of the channel of interest.
 *                  This uses 0-indexing i.e. 0 is the 1st channel
 *                  Pass channelIndex = -1 if you want to normalize to all the channels
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatNormalizeAudioBuffer(OmSpatAudioBuffer * const srcDest,
                                      const int channelIndex,
                                      const float newMaximumIndB);

/************************************************************************************/
/*!
 *  @brief          Applies a fade-in and/or fade-out to an OmSpatAudioBuffer
 *  @param[in]      srcDest : the buffer (the reversal is applied in-place)
 *  @param[in]      numSamplesForFadeIn : duration of the fade-in expressed in number of samples
 *                  use 0 if you dont want to apply fade-in.
 *                  numSamplesForFadeIn is expected to be < than the number of samples of the src buffer
 *  @param[in]      numSamplesForFadeOut : duration of the fade-out expressed in number of samples
 *                  use 0 if you dont want to apply fade-out.
 *                  numSamplesForFadeOut is expected to be < than the number of samples of the src buffer
 *  @param[in]      channelIndex : index of the channel of interest.
 *                  This uses 0-indexing i.e. 0 is the 1st channel
 *                  Pass channelIndex = -1 if you want to apply fade to all the channels
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatApplyFadeToAudioBuffer(OmSpatAudioBuffer * const srcDest,
                                        const unsigned long numSamplesForFadeIn,
                                        const unsigned long numSamplesForFadeOut,
                                        const int channelIndex);

#endif /* _OM_SPAT_AUDIO_BUFFERS_H__ */

