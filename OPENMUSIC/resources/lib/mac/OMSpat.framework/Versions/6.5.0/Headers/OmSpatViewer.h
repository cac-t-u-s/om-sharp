/************************************************************************************/
/*!
 *  @file       OmSpatViewer.h
 *  @brief      C-interface to libspat, for OM-Spat
 *  @author     Thibaut Carpentier
 *  @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_VIEWER_H__
#define _OM_SPAT_VIEWER_H__

#include "../src/OmSpatCoordinates.h"
#include "../src/OmSpatOsc.h"

/************************************************************************************/
/*!
 *  @struct         OmSpatViewerData
 *  @brief          Utility class representing the data of a spat.viewer
 *  @ingroup        omspat
 *
 */
/************************************************************************************/
struct OM_SPAT_VISIBILITY_DEFAULT OmSpatViewerData
{
    OmSpatPoints * sources;       ///< (pointer to) the coordinates of the sources
    OmSpatPoints * speakers;      ///< (pointer to) the coordinates of the speakers
};


typedef void * OmSpatViewer;


/************************************************************************************/
/*!
 *  @brief          Creates a spat.viewer and install it into a NSView
 *  @param[in]      nsViewHandler : a NSView
 *  @param[in]      width : width of the component
 *  @param[in]      height : height of the component
 *  @param[in]      componentId :
 *
 *  @return         a pointer on the spat.viewer GUI object
 *                  or nullptr if something went wrong
 *
 */
/************************************************************************************/
OM_SPAT_API
const OmSpatViewer OmSpatCreateSpatViewerWithNSView(OmSpatNsViewHandler view,
                                                    const int componentId);

/************************************************************************************/
/*!
 *  @brief          Returns the number of sources
 *  @param[in]      obj : a spat.viewer GUI component
 *
 *  @return         the number of sources
 *                  or -1 if something went wrong
 *
 */
/************************************************************************************/
OM_SPAT_API
const int OmSpatViewerGetNumSources(OmSpatViewer obj);

OM_SPAT_API
const int OmSpatViewerGetNumSpeakers(OmSpatViewer obj);

/************************************************************************************/
/*!
 *  @brief          Retrieves data from the spat.viewer GUI
 *  @param[in]      obj : a spat.viewer GUI component
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerGetData(OmSpatViewer obj,
                               OmSpatViewerData * data);

/************************************************************************************/
/*!
 *  @brief          Retrieves the position of one given source
 *  @param[in]      obj : a spat.viewer GUI component
 *  @param[in]      sourceIndex : index of the source
 *  @param[out]     dest : the position
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerGetSourcePosition(OmSpatViewer obj,
                                         const unsigned int sourceIndex,
                                         OmSpatPoints * dest);

/************************************************************************************/
/*!
 *  @brief          Checks whether a source is currently selected or not
 *  @param[in]      obj : a spat.viewer GUI component
 *  @param[in]      sourceIndex : index of the source
 *  @param[out]     isSelected : true or false whether the given source is selected or not
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerIsSourceSelected(OmSpatViewer obj,
                                        const unsigned int sourceIndex,
                                        int * isSelected);


/************************************************************************************/
/*!
 *  @brief          Checks whether sources are currently selected or not
 *  @param[in]      obj : a spat.viewer GUI component
 *  @param[in]      areSelected : a list which length is the number of sources,
 *                  and which values tell whether the sources are selected or not
 *  @param[out]     numSources : number of sources i.e. the length of areSelected
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerGetSelectedSources(OmSpatViewer obj,
                                          int * areSelected,
                                          const unsigned int numSources);

/************************************************************************************/
/*!
 *  @brief          Retrieves the position of one given speaker
 *  @param[in]      obj : a spat.viewer GUI component
 *  @param[in]      speakerIndex : index of the speaker
 *  @param[out]     dest : the position
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerGetSpeakerPosition(OmSpatViewer obj,
                                          const unsigned int speakerIndex,
                                          OmSpatPoints * dest);

/************************************************************************************/
/*!
 *  @brief          Free a spat.viewer object
 *  @param[in]      obj : a spat.viewer GUI component
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreeSpatViewer(OmSpatViewer obj);


/************************************************************************************/
/*!
 *  @brief          Process the incoming OSC message or OSC bundle
 *
 *  @param[in]      obj : a spat.viewer GUI component
 *  @param[in]      content : the OSC message or OSC bundle
 *  @param[in]      size : size of the message or bundle
 *
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerProcessOSCCommands(OmSpatViewer obj,
                                          const OmSpatOscBundle * bundle);

/************************************************************************************/
/*!
 *  @brief          C-style callback called whenever
 *  @ingroup        omspat
 *  @param[in]      componentId :
 *  @param[in]      bundle : the OSC message or bundle
 *
 */
/************************************************************************************/
typedef void (*OmSpatCallbackWithOsc)( const int componentId,
                                       const OmSpatOscBundle * bundle );


/************************************************************************************/
/*!
 *  @brief          Register a C-callback function
 *  @param[in]      obj : a spat.viewer GUI component
 *  @param[in]      theCallbackFunction : the C-callback function
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerRegisterOscCallback(OmSpatViewer obj,
                                           OmSpatCallbackWithOsc theCallbackFunction);

OM_SPAT_API
const bool OmSpatGetCurrentStateAsOscBundle(OmSpatViewer obj,
                                            OmSpatOscBundle * bundle );

#endif /* _OM_SPAT_VIEWER_H__ */

