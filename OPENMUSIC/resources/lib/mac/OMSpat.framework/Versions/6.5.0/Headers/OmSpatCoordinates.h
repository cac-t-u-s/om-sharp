/************************************************************************************/
/*  FILE DESCRIPTION																*/
/*----------------------------------------------------------------------------------*/
/*!
 *   @file       OmSpatCoordinates.h
 *   @brief      C-interface to libspat, for OM-Spat
 *   @author     Thibaut Carpentier
 *   @version    $(PRODUCT_VERSION)
 *   @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_COORDINATES_H__
#define _OM_SPAT_COORDINATES_H__

#include "../src/OmSpatApi.h"

/************************************************************************************/
/*!
 *  @struct         OmSpatCoordinatesFormat
 *  @brief          Different kinds of coordinate formats
 *  @ingroup        omspat
 *
 */
/************************************************************************************/
enum OM_SPAT_VISIBILITY_DEFAULT OmSpatCoordinatesFormat
{
    xyz = 0,                        ///< cartesian coordinates, in meters
    aed = 1,                        ///< azimuth, elevation, distance, with Spat~ navigational convention
};

typedef enum OmSpatCoordinatesFormat OmSpatCoordinatesFormat;   ///< C-style declaration

/************************************************************************************/
/*!
 *  @brief          Returns a string name corresponding to an OmSpatCoordinatesFormat
 *  @ingroup        omspat
 *
 */
/************************************************************************************/
OM_SPAT_API
const char * OmSpatCoordinatesFormatName(const OmSpatCoordinatesFormat format);

/************************************************************************************/
/*!
 *  @struct         OmSpatPoints
 *  @brief          Utility class representing a list of points in 3D space
 *  @ingroup        omspat
 *
 */
/************************************************************************************/
struct OM_SPAT_VISIBILITY_DEFAULT OmSpatPoints
{
    OmSpatCoordinatesFormat format;     ///< the coordinate format
    unsigned int numPoints;         ///< the number of points
    float * data;                  ///< the coordinates (total size is 3 x numPoints)
};

typedef struct OmSpatPoints OmSpatPoints;   ///< C-style declaration

/************************************************************************************/
/*!
 *  @brief          Converts an OmSpatPoints into a new coordinate system
 *  @param[in]      src : source
 *  @param[in]      newFormat : new coordinate format
 *  @param[out]     dest : destination (resized if needed)
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatConvertPoints(OmSpatPoints * dest,
                               const OmSpatCoordinatesFormat newFormat,
                               const OmSpatPoints * const src);

/************************************************************************************/
/*!
 *  @brief          Apply translation to an OmSpatPoints
 *  @param[in]      src : source
 *  @param[in]      offsetx : translation offset accros the x axis (in meters)
 *  @param[in]      offsety : translation offset accros the y axis (in meters)
 *  @param[in]      offsetz : translation offset accros the z axis (in meters)
 *  @param[out]     dest : destination (resized if needed)
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatTranslatePoints(OmSpatPoints * dest,
                                 const float offsetx,
                                 const float offsety,
                                 const float offsetz,
                                 const OmSpatPoints * const src);

/************************************************************************************/
/*!
 *  @brief          Apply rotation to an OmSpatPoints
 *  @param[in]      src : source
 *  @param[in]      yawInDegrees : rotation angle around the z axis (in degrees)
 *  @param[in]      pitchInDegrees : rotation angle around the y axis (in degrees)
 *  @param[in]      rollInDegrees : rotation angle around the x axis (in degrees)
 *  @param[out]     dest : destination (resized if needed)
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatRotatePoints(OmSpatPoints * dest,
                              const float yawInDegrees,
                              const float pitchInDegrees,
                              const float rollInDegrees,
                              const OmSpatPoints * const src );

/************************************************************************************/
/*!
 *  @brief          Apply scaling to an OmSpatPoints
 *  @param[in]      src : source
 *  @param[in]      scalex : scaling factor across the x axis
 *  @param[in]      scaley : scaling factor across the y axis
 *  @param[in]      scalez : scaling factor across the z axis
 *  @param[in]      distancescaling : scaling factor for distance
 *  @param[out]     dest : destination (resized if needed)
 *  @return         true on success; check OmSpatGetLastError() otherwise; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatScalePoints(OmSpatPoints * dest,
                             const float scalex,
                             const float scaley,
                             const float scalez,
                             const float distancescaling,
                             const OmSpatPoints * const src );

/************************************************************************************/
/*!
 *  @brief          Prints the content of an OmSpatPoints to the OSX Console
 *  @param[in]      src : source
 *  @param[in]      printData : also print the content of the src->data field
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatPrintPoints(const OmSpatPoints * const src,
                             const bool printData);


#endif /* _OM_SPAT_COORDINATES_H__ */


