/************************************************************************************/
/*!
 *  @file       OmSpatBpc.h
 *  @brief      C-interface to libspat, for OM-Spat
 *  @author     Thibaut Carpentier
 *  @date       08/05/2015
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_BPC_H__
#define _OM_SPAT_BPC_H__

#include "../src/OmSpatCoordinates.h"

typedef void * OmSpatBpc;

/************************************************************************************/
/*!
 *  @brief          Creates a spat.bpc and install it into a NSView
 *  @param[in]      nsViewHandler : a NSView
 *  @param[in]      width : width of the component
 *  @param[in]      height : height of the component
 *  @param[in]      componentId :
 *
 *  @return         a pointer on the spat.bpc GUI object
 *                  or nullptr if something went wrong
 *
 */
/************************************************************************************/
OM_SPAT_API
const OmSpatBpc OmSpatCreateSpatBpcWithNSView(OmSpatNsViewHandler view,
                                              const int width       ,
                                              const int height      ,
                                              const int componentId);

/************************************************************************************/
/*!
 *  @brief          Free a spat.bpc object
 *  @param[in]      obj : a spat.bpc GUI component
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreeSpatBpc(OmSpatBpc obj);

/************************************************************************************/
/*!
 *  @brief          Process the incoming OSC message or OSC bundle
 *
 *  @param[in]      obj : a spat.bpc GUI component
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
 *==============================================================================
 *
 *                  Loading all BPC data from a textual file
 *                  /loadtxt [string]
 *                  Export all BPC data to textual file
 *                  /exporttxt [string]
 *
 *                  Loading all BPC data from a binary OSC file
 *                  /loadosc [string]
 *                  Export all BPC data to binary OSC file
 *                  /exportosc [string]
 *
 *==============================================================================
 *
 *                  Loading one BPC data from a textual file
 *                  /bpc/[index]/loadtxt [string]
 *                  Export one BPC data to textual file
 *                  /bpc/[index]/exporttxt [string]
 *
 *                  Loading one BPC data from a binary OSC file
 *                  /bpc/[index]/loadosc [string]
 *                  Export one BPC data to binary OSC file
 *                  /bpc/[index]/exportosc [string]
 *
 *
 *==============================================================================
 *
 *                  /clear
 *
 *==============================================================================
 *
 *                  /bpc/[index]/clear
 *                  /bpc/[index]/visible [boolean]
 *                  /bpc/[index]/labelsvisible [boolean]
 *                  /bpc/[index]/color [number][number][number][number]
 *
 *                  Adding new points to the BPC :
 *                  (similar to spat.viewer coordinates syntax, with additional time in msec)
 *                  /bpc/[index]/xyzt [number][number][number][number]
 *                  /bpc/[index]/xyt [number][number][number]
 *                  /bpc/[index]/aedt [number][number][number][number]
 *                  /bpc/[index]/adet [number][number][number][number]
 *                  /bpc/[index]/aet [number][number][number]
 *                  /bpc/[index]/azt [number][number]
 *
 *                  Apply geometrical transforms to the BPC:
 *                  /bpc/[index]/translate [number][number][number]
 *                  /bpc/[index]/rotate [number][number][number]
 *                  /bpc/[index]/scale [number][number][number]
 *
 *                  Apply temporal transforms to the BPC
 *                  /bpc/[index]/shift [number]
 *                  /bpc/[index]/startat [number]
 *                  /bpc/[index]/endat [number]
 *
 *                  /bpc/[index]/loop [boolean]
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
 *
 *
 *==============================================================================
 *                  /timelines/visibles [boolean]
 *
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatBpcProcessOSCCommands(OmSpatBpc obj,
                                       const char *content,
                                       const unsigned long size);

#endif /* _OM_SPAT_BPC_H__ */

