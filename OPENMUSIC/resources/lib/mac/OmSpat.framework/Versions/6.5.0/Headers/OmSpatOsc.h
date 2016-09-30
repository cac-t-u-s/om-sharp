/************************************************************************************/
/*!
 *  @file       OmSpatOsc.h
 *  @brief      Opaque implementation for libomspat
 *  @author     Thibaut Carpentier
 *  @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_OSC_H__
#define _OM_SPAT_OSC_H__

#include "../src/OmSpatApi.h"



/************************************************************************************/
/*!
 *  @brief          Prints the OSC packet on OSX Console
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatDebugOSCPacket(const char *contents,
                                const unsigned long size);

/************************************************************************************/
/*!
 *  @brief          Prints the OSC packet on OSX Console
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatDebugOSCBundle(const OmSpatOscBundle * bundle);


#endif /* _OM_SPAT_OSC_H__ */

