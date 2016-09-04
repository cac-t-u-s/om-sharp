/************************************************************************************/
/*  FILE DESCRIPTION																*/
/*----------------------------------------------------------------------------------*/
/*!
 *   @file       OmSpatMatrixCtrl.h
 *   @brief      C-interface to libspat, for OM-Spat
 *   @author     Thibaut Carpentier
 *   @version    $(PRODUCT_VERSION)
 *   @date       05/11/2013
 *
 */
/************************************************************************************/
#ifndef _OM_SPAT_MATRIX_CTRL_H__
#define _OM_SPAT_MATRIX_CTRL_H__

#include "../src/OmSpatApi.h"

typedef void * OmSpatMatrixCtrl;

/************************************************************************************/
/*!
 *  @brief          Creates a spat.matrixctrl and install it into a NSView
 *  @param[in]      view : a NSView
 *  @param[in]      width : width of the component
 *  @param[in]      height : height of the component
 *  @param[in]      componentId : 
 *
 *  @return         a pointer on the spat.matrixctrl GUI object
 *                  or NULL if something went wrong
 *
 */
/************************************************************************************/
OM_SPAT_API
const OmSpatMatrixCtrl OmSpatCreateMatrixCtrlWithNSView(OmSpatNsViewHandler view,
                                                        const int width       ,
                                                        const int height      ,
                                                        const int componentId);

/************************************************************************************/
/*!
 *  @brief          Free a spat.matrixctrl object
 *  @param[in]      obj : a spat.matrixctrl GUI component
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatFreeMatrixCtrl(OmSpatMatrixCtrl obj);

/************************************************************************************/
/*!
 *  @brief          Changes the size of the GUI component
 *  @param[in]      obj : a spat.matrixctrl GUI component
 *  @param[in]      newWidth : width of the component
 *  @param[in]      newHeight : height of the component
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatMatrixCtrlSetComponentSize(OmSpatMatrixCtrl obj,
                                            const int newWidth       ,
                                            const int newHeight);

/************************************************************************************/
/*!
 *  @brief          Sets the number of rows and cols
 *  @param[in]      obj : a spat.matrixctrl GUI component
 *  @param[in]      newNumRows :
 *  @param[in]      newNumCols :
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatMatrixCtrlSetNumRowsAndCols(OmSpatMatrixCtrl obj,
                                             const unsigned int newNumRows,
                                             const unsigned int newNumCols);

/************************************************************************************/
/*!
 *  @brief          Sets the state of a given matrix element
 *  @param[in]      obj : a spat.matrixctrl GUI component
 *  @param[in]      rowIndex :
 *  @param[in]      colIndex :
 *  @param[in]      state :
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatMatrixCtrlSetState(OmSpatMatrixCtrl obj,
                                    const unsigned int rowIndex,
                                    const unsigned int colIndex,
                                    const bool state);

/// definition of a C-function callback
/// componentId : ID of the spat.viewer from which the callback emanates
/// rowIndex : index of the row the user clicked
/// colIndex : index of the column the user clicked
typedef void (*cCallbackForMatrixCtrl)( const int componentId,
                                        const unsigned int rowIndex,
                                        const unsigned int colIndex,
                                        const bool state );

/************************************************************************************/
/*!
 *  @brief          Register a C-callback function
 *  @param[in]      obj : a spat.matrixctrl GUI component
 *  @param[in]      theCallbackFunction : the C-callback function
 *
 *  @return         true on success; check OmSpatGetLastError() otherwise
 *
 */
/************************************************************************************/
OM_SPAT_API
const bool OmSpatMatrixCtrlRegisterCallback(OmSpatMatrixCtrl obj,
                                            cCallbackForMatrixCtrl theCallbackFunction);

#endif /* _OM_SPAT_MATRIX_CTRL_H__ */


