{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPlane
--
-- SCNPlane represents a rectangle with controllable width and height. The plane has one visible side.
--
-- Generated bindings for @SCNPlane@.
module ObjC.SceneKit.SCNPlane
  ( SCNPlane
  , IsSCNPlane(..)
  , planeWithWidth_height
  , width
  , setWidth
  , height
  , setHeight
  , widthSegmentCount
  , setWidthSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , cornerRadius
  , setCornerRadius
  , cornerSegmentCount
  , setCornerSegmentCount
  , planeWithWidth_heightSelector
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , widthSegmentCountSelector
  , setWidthSegmentCountSelector
  , heightSegmentCountSelector
  , setHeightSegmentCountSelector
  , cornerRadiusSelector
  , setCornerRadiusSelector
  , cornerSegmentCountSelector
  , setCornerSegmentCountSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | planeWithWidth:height:
--
-- Creates and returns a plane with given width and height.
--
-- @width@ — The width of the plane.
--
-- @height@ — The height of the plane.
--
-- ObjC selector: @+ planeWithWidth:height:@
planeWithWidth_height :: CDouble -> CDouble -> IO (Id SCNPlane)
planeWithWidth_height width height =
  do
    cls' <- getRequiredClass "SCNPlane"
    sendClassMsg cls' (mkSelector "planeWithWidth:height:") (retPtr retVoid) [argCDouble (fromIntegral width), argCDouble (fromIntegral height)] >>= retainedObject . castPtr

-- | width
--
-- The plane extent along the X axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- width@
width :: IsSCNPlane scnPlane => scnPlane -> IO CDouble
width scnPlane  =
  sendMsg scnPlane (mkSelector "width") retCDouble []

-- | width
--
-- The plane extent along the X axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNPlane scnPlane => scnPlane -> CDouble -> IO ()
setWidth scnPlane  value =
  sendMsg scnPlane (mkSelector "setWidth:") retVoid [argCDouble (fromIntegral value)]

-- | height
--
-- The plane extent along the Y axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNPlane scnPlane => scnPlane -> IO CDouble
height scnPlane  =
  sendMsg scnPlane (mkSelector "height") retCDouble []

-- | height
--
-- The plane extent along the Y axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNPlane scnPlane => scnPlane -> CDouble -> IO ()
setHeight scnPlane  value =
  sendMsg scnPlane (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- widthSegmentCount@
widthSegmentCount :: IsSCNPlane scnPlane => scnPlane -> IO CLong
widthSegmentCount scnPlane  =
  sendMsg scnPlane (mkSelector "widthSegmentCount") retCLong []

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setWidthSegmentCount:@
setWidthSegmentCount :: IsSCNPlane scnPlane => scnPlane -> CLong -> IO ()
setWidthSegmentCount scnPlane  value =
  sendMsg scnPlane (mkSelector "setWidthSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. The default value is 1. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNPlane scnPlane => scnPlane -> IO CLong
heightSegmentCount scnPlane  =
  sendMsg scnPlane (mkSelector "heightSegmentCount") retCLong []

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. The default value is 1. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNPlane scnPlane => scnPlane -> CLong -> IO ()
setHeightSegmentCount scnPlane  value =
  sendMsg scnPlane (mkSelector "setHeightSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | cornerRadius
--
-- The corner radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsSCNPlane scnPlane => scnPlane -> IO CDouble
cornerRadius scnPlane  =
  sendMsg scnPlane (mkSelector "cornerRadius") retCDouble []

-- | cornerRadius
--
-- The corner radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsSCNPlane scnPlane => scnPlane -> CDouble -> IO ()
setCornerRadius scnPlane  value =
  sendMsg scnPlane (mkSelector "setCornerRadius:") retVoid [argCDouble (fromIntegral value)]

-- | cornerSegmentCount
--
-- The number of subdivisions for the rounded corners. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- cornerSegmentCount@
cornerSegmentCount :: IsSCNPlane scnPlane => scnPlane -> IO CLong
cornerSegmentCount scnPlane  =
  sendMsg scnPlane (mkSelector "cornerSegmentCount") retCLong []

-- | cornerSegmentCount
--
-- The number of subdivisions for the rounded corners. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- setCornerSegmentCount:@
setCornerSegmentCount :: IsSCNPlane scnPlane => scnPlane -> CLong -> IO ()
setCornerSegmentCount scnPlane  value =
  sendMsg scnPlane (mkSelector "setCornerSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @planeWithWidth:height:@
planeWithWidth_heightSelector :: Selector
planeWithWidth_heightSelector = mkSelector "planeWithWidth:height:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @widthSegmentCount@
widthSegmentCountSelector :: Selector
widthSegmentCountSelector = mkSelector "widthSegmentCount"

-- | @Selector@ for @setWidthSegmentCount:@
setWidthSegmentCountSelector :: Selector
setWidthSegmentCountSelector = mkSelector "setWidthSegmentCount:"

-- | @Selector@ for @heightSegmentCount@
heightSegmentCountSelector :: Selector
heightSegmentCountSelector = mkSelector "heightSegmentCount"

-- | @Selector@ for @setHeightSegmentCount:@
setHeightSegmentCountSelector :: Selector
setHeightSegmentCountSelector = mkSelector "setHeightSegmentCount:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @cornerSegmentCount@
cornerSegmentCountSelector :: Selector
cornerSegmentCountSelector = mkSelector "cornerSegmentCount"

-- | @Selector@ for @setCornerSegmentCount:@
setCornerSegmentCountSelector :: Selector
setCornerSegmentCountSelector = mkSelector "setCornerSegmentCount:"

