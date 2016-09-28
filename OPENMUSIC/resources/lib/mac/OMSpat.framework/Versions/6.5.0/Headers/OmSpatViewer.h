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
                                                    const int width       ,
                                                    const int height      ,
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
 *  @details        Regarding OSC type tags:
 *                  [number] can be either float or double or int32 or int64
 *                  [integer] can be either int32 or int64
 *                  [boolean] can be either true or false or int32 or int64
 *                  [string] can be either string or char
 *
 *  @details        List of supported messages :
 *
 *
 *==============================================================================
 *                  /numsources [integer]
 *                  /numspeakers [integer]
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
 *                  /source/[index]/aperture [number]
 *                  /source/[index]/aperture/visible [boolean]
 *                  /source/[index]/yaw [number]
 *
 *                  /source/[index]/name [string]
 *                  /source/[index]/visible [boolean]
 *                  /source/[index]/color [number][number][number][number]
 *                  /source/[index]/select [boolean]
 *
 *
 *==============================================================================
 *
 *                  /sources/editable [boolean]
 *                  /sources/visible [boolean]
 *
 *
 *==============================================================================
 *                  Setting the source position (without triggering the nofitication):
 *                  /set/source/[index]/xyz [number] [number] [number]
 *                  /set/source/[index]/xy [number] [number]
 *                  /set/source/[index]/x [number]
 *                  /set/source/[index]/y [number]
 *                  /set/source/[index]/z [number]
 *                  /set/source/[index]/aed [number] [number] [number]
 *                  /set/source/[index]/ade [number] [number] [number]
 *                  /set/source/[index]/ae [number] [number]
 *                  /set/source/[index]/ad [number] [number]
 *                  /set/source/[index]/az [number]
 *                  /set/source/[index]/azim [number]
 *                  /set/source/[index]/elev [number]
 *                  /set/source/[index]/dist [number]
 *                  /set/source/[index]/azim++ [number]
 *                  /set/source/[index]/elev++ [number]
 *                  /set/source/[index]/dist++ [number]
 *                  /set/source/[index]/dist*= [number]
 *
 *                  /set/source/[index]/aperture [number]
 *                  /set/source/[index]/aperture/visible [boolean]
 *                  /set/source/[index]/yaw [number]
 *
 *                  /set/source/[index]/name [string]
 *                  /set/source/[index]/visible [boolean]
 *                  /set/source/[index]/color [number][number][number][number]
 *                  /set/source/[index]/select [boolean]
 *
 *
 *
 *==============================================================================
 *                  Setting the speaker position :
 *                  /speaker/[index]/xyz [number] [number] [number]
 *                  /speaker/[index]/xy [number] [number]
 *                  /speaker/[index]/x [number]
 *                  /speaker/[index]/y [number]
 *                  /speaker/[index]/z [number]
 *                  /speaker/[index]/aed [number] [number] [number]
 *                  /speaker/[index]/ade [number] [number] [number]
 *                  /speaker/[index]/ae [number] [number]
 *                  /speaker/[index]/ad [number] [number]
 *                  /speaker/[index]/az [number]
 *                  /speaker/[index]/azim [number]
 *                  /speaker/[index]/elev [number]
 *                  /speaker/[index]/dist [number]
 *                  /speaker/[index]/azim++ [number]
 *                  /speaker/[index]/elev++ [number]
 *                  /speaker/[index]/dist++ [number]
 *                  /speaker/[index]/dist*= [number]
 *
 *                  /speaker/[index]/name [string]
 *                  /speaker/[index]/visible [boolean]
 *                  /speaker/[index]/color [number][number][number][number]
 *
 *==============================================================================
 *
 *                  /speakers/editable [boolean]
 *                  /speakers/visible [boolean]
 *
 *                  Setting the speakers' positions :
 *                  /speakers/xyz [number] [number] [number] ...
 *                  /speakers/xy [number] [number] ...
 *                  /speakers/x [number] ...
 *                  /speakers/y [number] ...
 *                  /speakers/z [number] ...
 *                  /speakers/aed [number] [number] [number] ...
 *                  /speakers/ade [number] [number] [number] ...
 *                  /speakers/ae [number] [number] ...
 *                  /speakers/ad [number] [number] ...
 *                  /speakers/az [number] ...
 *                  /speakers/azim [number] ...
 *                  /speakers/elev [number] ...
 *                  /speakers/dist [number] ...
 *                  /speakers/azim++ [number] ...
 *                  /speakers/elev++ [number] ...
 *                  /speakers/dist++ [number] ...
 *                  /speakers/dist*= [number] ...
 *
 *
 *                  Setting the speakers' positions (without triggering the nofitication):
 *                  /set/speakers/xyz [number] [number] [number] ...
 *                  /set/speakers/xy [number] [number] ...
 *                  /set/speakers/x [number] ...
 *                  /set/speakers/y [number] ...
 *                  /set/speakers/z [number] ...
 *                  /set/speakers/aed [number] [number] [number] ...
 *                  /set/speakers/ade [number] [number] [number] ...
 *                  /set/speakers/ae [number] [number] ...
 *                  /set/speakers/ad [number] [number] ...
 *                  /set/speakers/az [number] ...
 *                  /set/speakers/azim [number] ...
 *                  /set/speakers/elev [number] ...
 *                  /set/speakers/dist [number] ...
 *                  /set/speakers/azim++ [number] ...
 *                  /set/speakers/elev++ [number] ...
 *                  /set/speakers/dist++ [number] ...
 *                  /set/speakers/dist*= [number] ...
 *
 *==============================================================================
 *                  Setting the speaker position (without triggering the nofitication):
 *                  /set/speaker/[index]/xyz [number] [number] [number]
 *                  /set/speaker/[index]/xy [number] [number]
 *                  /set/speaker/[index]/x [number]
 *                  /set/speaker/[index]/y [number]
 *                  /set/speaker/[index]/z [number]
 *                  /set/speaker/[index]/aed [number] [number] [number]
 *                  /set/speaker/[index]/ade [number] [number] [number]
 *                  /set/speaker/[index]/ae [number] [number]
 *                  /set/speaker/[index]/ad [number] [number]
 *                  /set/speaker/[index]/az [number]
 *                  /set/speaker/[index]/azim [number]
 *                  /set/speaker/[index]/elev [number]
 *                  /set/speaker/[index]/dist [number]
 *                  /set/speaker/[index]/azim++ [number]
 *                  /set/speaker/[index]/elev++ [number]
 *                  /set/speaker/[index]/dist++ [number]
 *                  /set/speaker/[index]/dist*= [number]
 *
 *                  /set/speaker/[index]/name [string]
 *                  /set/speaker/[index]/visible [boolean]
 *                  /set/speaker/[index]/color [number][number][number][number]
 *
 *==============================================================================
 *                  /grid/color [number][number][number][number]
 *                  /grid/mode [string]
 *                  /grid/spacing [number]
 *                  /grid/numlines [integer]
 *                  /grid/numangulardivisions [integer]
 *
 *==============================================================================
 *                  /window/size [integer][integer]
 *                  /window/backgroundcolor [number][number][number][number]
 *                  /window/layout [string]  ("single" or "leftright" or "topbottom" )
 *
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatViewerProcessOSCCommands(OmSpatViewer obj,
                                          const char *content,
                                          const unsigned long size);

/************************************************************************************/
/*!
 *  @struct         OmSpatViewerParameterType
 *  @brief          Different kinds of parameters for spat.viewer callbacks
 *  @ingroup        omspat
 *
 */
/************************************************************************************/
enum OM_SPAT_VISIBILITY_DEFAULT OmSpatViewerParameterType
{
    kSourcePositionChanged  =   0,      ///< receives notification when a source position changes
    kSourceApertureChanged  =   1,
    kSourceYawChanged       =   2,
    kSpeakerPositionChanged =   3,
    kSourceSelectionChanged =   4,      ///< receives notification when a source is selected/de-selected from the mouse
    kSourceDoubleClicked    =   5,      ///< receives notification when a source is double-clicked
};


/// definition of a C-function callback with one argument
/// componentId : ID of the spat.viewer from which the callback emanates
/// paramType : the type of parameter that changed
/// index : index of the element (source or speaker) that changed. (or -1 if this is not relevant)
typedef void (*OmSpatCallbackWithUnsignedInt)( const int componentId,
                                               const OmSpatViewerParameterType paramType,
                                               const unsigned int index );

/************************************************************************************/
/*!
 *  @brief          C-style callback called whenever
 *  @ingroup        omspat
 *
 */
/************************************************************************************/
typedef void (*OmSpatCallbackWithOsc)( const int componentId,
                                       const char *content,
                                       const unsigned long size );

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
const bool OmSpatViewerRegisterCallback(OmSpatViewer obj,
                                        OmSpatCallbackWithUnsignedInt theCallbackFunction);


#endif /* _OM_SPAT_VIEWER_H__ */

