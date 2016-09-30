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

#include "../src/OmSpatOscImplementation.h"

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
const OmSpatViewer OmSpatCreateSpatViewerWithNSView(OmSpatNsViewHandler * view,
                                                    const int componentId);


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

/*
OM_SPAT_API
const bool OmSpatGetCurrentStateAsOscBundle(OmSpatViewer obj,
                                            OmSpatOscBundle * bundle );
 */

#endif /* _OM_SPAT_VIEWER_H__ */

